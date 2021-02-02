package svcweather

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/PuerkitoBio/goquery"
	"golang.org/x/crypto/bcrypt"

	"github.com/powerman/structlog"
)

var log = structlog.New()

// MeasurementInterval is the time interval (in seconds) between fetching new temperature values from a remote service
const MeasurementInterval = 300

// Errors
var (
	ErrBadConfig        = errors.New("Bad or missing configuration parameter")
	ErrPropertyNotFound = errors.New("JSON property not found")
	ErrHTTPRequest      = errors.New("Failed HTTP request")
	ErrBadStatusCode    = errors.New("Bad HTTP response code")
	ErrNoPayload        = errors.New("No payload")
)

type ipV4addr string

type timeValPair struct {
	time  time.Time
	value float64
}

type service struct {
	lastMeasurement timeValPair
	providers       map[int]app.WeatherSvcProvider
}

type openWeatherProvider struct {
	app.APIKeyConfig
	ipV4addr ipV4addr
	lat      float64
	lng      float64
}

type meteoInfoProvider struct {
	app.APIKeyConfig
}

type coordinates interface {
	LatLng(clientIP ipV4addr) (float64, float64, error)
}

type ipapi struct { // throws RateLimited error all too often on a free plan
}

type ipify struct { // 1000 requests per month on free subscription. Requires environment variable IPIFY_API_KEY to be set
	app.APIKeyConfig
}

// Instance creates and returns an instance of a WeatherSvc.
func Instance() app.WeatherSvc {
	newInstance := &service{}
	newInstance.providers = make(map[int]app.WeatherSvcProvider)
	password := []byte("1234")
	passHash, _ := bcrypt.GenerateFromPassword(password, 20)
	log.Info("PASSWORD HASH IS ", passHash)
	return newInstance
}

// New returns a new weather service provider
func New(weatherProviderConfig *app.APIKeyConfig) (app.WeatherSvcProvider, error) {
	if weatherProviderConfig == nil {
		log.Info("Missing a weather provider config")
		return nil, ErrBadConfig
	}
	if weatherProviderConfig.BaseURL == "" {
		log.Info("Missing a weather provider base URL")
		return nil, ErrBadConfig
	}
	if weatherProviderConfig.ProviderName == "" {
		log.Info("Missing a weather provider name")
		return nil, ErrBadConfig
	}

	switch weatherProviderConfig.ProviderName {
	case app.OpenWeather:
		if weatherProviderConfig.APIKey == "" {
			log.Info("Missing OpenWeatherAPIKey")
			return nil, ErrBadConfig
		}
		provider := &openWeatherProvider{}
		provider.ProviderName = weatherProviderConfig.ProviderName
		provider.BaseURL = weatherProviderConfig.BaseURL
		provider.APIKey = weatherProviderConfig.APIKey
		return provider, nil
	case app.MeteoInfo:
		provider := &meteoInfoProvider{}
		provider.ProviderName = weatherProviderConfig.ProviderName
		provider.BaseURL = weatherProviderConfig.BaseURL
		return provider, nil
	default:
		log.Info("Missing a valid weather provider configuration")
		return nil, ErrBadConfig
	}
}

func (s *service) RegisterProvider(provider app.WeatherSvcProvider, priority int) error {
	if provider == nil {
		log.Info("Missing a weather service provider config")
		return ErrBadConfig
	}
	if priority < 0 {
		log.Info("Priority must be a positive integer")
		return ErrBadConfig
	}
	if _, hasProvider := s.providers[priority]; !hasProvider {
		log.Info(fmt.Sprintf("Registered %s weather provider", provider.Name()))
		s.providers[priority] = provider
	}
	return nil
}

// CurrentTemperature returns current temperature based on the client's IP address
func (s *service) CurrentTemperature() (float64, error) {
	var val float64
	var err error
	for index := 0; index < len(s.providers); index++ {
		provider := s.providers[index]
		if !provider.IsInitialized() {
			log.Info(fmt.Sprintf("Provider %s has not been initialized", provider.Name()))
			continue
		}

		currentTime := time.Now()
		if (currentTime.Unix() - s.lastMeasurement.time.Unix()) <= MeasurementInterval {
			return s.lastMeasurement.value, nil
		}
		log.Info(fmt.Sprintf("Using %s weather provider", provider.Name()))
		val, err = provider.CurrentTemperature()

		if err == nil {
			log.Printf("Temperature at %s is %f", currentTime.String(), val)
			s.lastMeasurement.value = val
			s.lastMeasurement.time = currentTime
			return val, err
		}
		log.Info(err.Error())
	}
	return val, err
}

func (p *meteoInfoProvider) CurrentTemperature() (float64, error) {
	var path = p.BaseURL
	weatherRequest, errHTTPNet := http.NewRequest("GET", path, nil)
	if errHTTPNet != nil {
		return 0, errHTTPNet
	}

	client := newClient()
	weatherResponse, errHTTPReq := client.Do(weatherRequest)
	if weatherResponse != nil {
		defer log.WarnIfFail(weatherResponse.Body.Close)
	}

	if errHTTPReq != nil {
		log.Info(fmt.Sprintf("Failed HTTP request to %s", path))
		return 0, ErrHTTPRequest
	}

	// Load the HTML document
	doc, err := goquery.NewDocumentFromReader(weatherResponse.Body)
	if err != nil {
		log.Info(fmt.Sprintf("Could not parse the response from %s", path))
		return 0, err
	}

	// Find the row with text Температура воздуха and the corresponding temperature value
	var temp float64
	var errParse = ErrPropertyNotFound
	doc.Find("tr").EachWithBreak(func(i int, s *goquery.Selection) bool {
		title := s.Children().First().Text()
		if strings.Contains(title, "Температура воздуха") {
			temp, errParse = strconv.ParseFloat(s.Children().Last().Text(), 64)
			return false
		}
		return true
	})
	if errParse != nil {
		log.Info(fmt.Sprintf("Property 'Температура воздуха' not found in the response from %s", path))
		return 0, errParse
	}

	return temp, nil
}

func (p *meteoInfoProvider) Name() string {
	return p.ProviderName
}

func (p *meteoInfoProvider) IsInitialized() bool {
	return true
}

func (p *meteoInfoProvider) Initialize(config *app.APIKeyConfig) error {
	return nil
}

func (p *openWeatherProvider) Name() string {
	return p.ProviderName
}

// CurrentTemperature returns current temperature based on the client's IP address
func (p *openWeatherProvider) CurrentTemperature() (float64, error) {
	path := fmt.Sprintf("%s?units=metric&lat=%f&lon=%f&appid=%s", p.BaseURL, p.lat, p.lng, p.APIKey)
	weatherRequest, errHTTPNet := http.NewRequest("GET", path, nil)
	if errHTTPNet != nil {
		return 0, errHTTPNet
	}

	client := newClient()
	weatherResponse, errHTTPReq := client.Do(weatherRequest)
	if weatherResponse != nil {
		defer log.WarnIfFail(weatherResponse.Body.Close)
	}

	if errHTTPReq != nil {
		log.Info(fmt.Sprintf("Failed HTTP request to %s", p.BaseURL))
		return 0, ErrHTTPRequest
	}

	if weatherResponse.StatusCode != http.StatusOK {
		log.Info(fmt.Sprintf("Bad status code %d returned by %s", weatherResponse.StatusCode, p.BaseURL))
		return 0, ErrBadStatusCode
	}

	weatherResponseBody, errIO := ioutil.ReadAll(weatherResponse.Body)
	if errIO != nil {
		return 0, errIO
	}
	var result map[string]interface{}

	json.Unmarshal(weatherResponseBody, &result)
	if result == nil {
		log.Info(fmt.Sprintf("No payload returned by %s", p.BaseURL))
		return 0, ErrNoPayload
	}

	main, hasMain := result["main"].(map[string]interface{})
	if !hasMain {
		log.Info(fmt.Sprintf("Property 'main' not found in the response from %s", p.BaseURL))
		return 0, ErrPropertyNotFound
	}

	temp, hasTemp := main["temp"].(float64)
	if !hasTemp {
		log.Info(fmt.Sprintf("Property 'temp' not found in the response from %s", p.BaseURL))
		return 0, ErrPropertyNotFound
	}

	return temp, nil
}

func (p *openWeatherProvider) IsInitialized() bool {
	return p.ipV4addr != ""
}

func (p *openWeatherProvider) Initialize(coordsConfig *app.APIKeyConfig) error {
	ip, ipErr := clientIP()
	if ipErr != nil {
		return ipErr
	}

	p.ipV4addr = ip

	var lat float64
	var lng float64
	var errCoord error

	if coordsConfig != nil {
		ipifyClient := &ipify{}
		ipifyClient.BaseURL = coordsConfig.BaseURL
		ipifyClient.APIKey = coordsConfig.APIKey
		lat, lng, errCoord = ipifyClient.LatLng(ip)
		if errCoord != nil {
			coords := &ipapi{}
			lat, lng, errCoord = coords.LatLng(ip)
		}
	}
	if errCoord != nil {
		return errCoord
	}
	p.lat = lat
	p.lng = lng

	return nil
}

// LatLng returns latitude and longitude of the client's IP address
func (coord ipapi) LatLng(clientIP ipV4addr) (float64, float64, error) {
	const baseURL = "https://ipapi.co/"

	path := fmt.Sprintf("%s%s/latlong", baseURL, string(clientIP)) // throws RateLimited error all too often on a free plan

	reqLatLng, errHTTPNet := http.NewRequest("GET", path, nil)
	if errHTTPNet != nil {
		return 0, 0, errHTTPNet
	}

	client := newClient()
	respLatLng, errHTTPReq := client.Do(reqLatLng)
	if respLatLng != nil {
		defer log.WarnIfFail(respLatLng.Body.Close)
	}

	if errHTTPReq != nil {
		log.Info(fmt.Sprintf("Failed HTTP request to %s", baseURL))
		return 0, 0, ErrHTTPRequest
	}

	if respLatLng.StatusCode != http.StatusOK {
		log.Info(fmt.Sprintf("Bad status code %d returned by %s", respLatLng.StatusCode, baseURL))
		return 0, 0, ErrBadStatusCode
	}

	bodyLatLong, errIO := ioutil.ReadAll(respLatLng.Body)
	if errIO != nil {
		return 0, 0, errIO
	}

	latlng := strings.Split(string(bodyLatLong), ",")
	lat, latConversionErr := strconv.ParseFloat(latlng[0], 64)
	if latConversionErr != nil {
		return 0, 0, latConversionErr
	}
	lng, lngConversionErr := strconv.ParseFloat(latlng[1], 64)
	if lngConversionErr != nil {
		return 0, 0, lngConversionErr
	}

	log.Info(fmt.Sprintf("IPAPI Lat, Lng: %f, %f", lat, lng))
	return lat, lng, nil
}

// LatLng returns latitude and longitude of the client's IP address
func (coord ipify) LatLng(clientIP ipV4addr) (float64, float64, error) {
	path := fmt.Sprintf("%s?apiKey=%s&ipAddress=%s", coord.BaseURL, coord.APIKey, string(clientIP))

	reqLatLng, errHTTPNet := http.NewRequest("GET", path, nil)
	if errHTTPNet != nil {
		return 0, 0, errHTTPNet
	}

	client := newClient()
	respLatLng, errHTTPReq := client.Do(reqLatLng)
	if respLatLng != nil {
		defer log.WarnIfFail(respLatLng.Body.Close)
	}

	if errHTTPReq != nil {
		log.Info(fmt.Sprintf("Failed HTTP request to %s", coord.BaseURL))
		return 0, 0, ErrHTTPRequest
	}

	if respLatLng.StatusCode != http.StatusOK {
		log.Info(fmt.Sprintf("Bad status code %d returned by %s", respLatLng.StatusCode, coord.BaseURL))
		return 0, 0, ErrBadStatusCode
	}

	bodyLatLng, errIO := ioutil.ReadAll(respLatLng.Body)
	if errIO != nil {
		return 0, 0, errIO
	}

	var result map[string]interface{}
	json.Unmarshal(bodyLatLng, &result)
	if result == nil {
		log.Info(fmt.Sprintf("No payload returned by %s", coord.BaseURL))
		return 0, 0, ErrNoPayload
	}

	location, hasLocation := result["location"].(map[string]interface{})
	if !hasLocation {
		log.Info(fmt.Sprintf("Property 'location' not found in the response from %s", coord.BaseURL))
		return 0, 0, ErrPropertyNotFound
	}

	lat, hasLat := location["lat"].(float64)
	if !hasLat {
		log.Info(fmt.Sprintf("Property 'lat' not found in the response from %s", coord.BaseURL))
		return 0, 0, ErrPropertyNotFound
	}

	lng, hasLng := location["lng"].(float64)
	if !hasLng {
		log.Info(fmt.Sprintf("Property 'lng' not found in the response from %s", coord.BaseURL))
		return 0, 0, ErrPropertyNotFound
	}

	log.Info(fmt.Sprintf("IPIFY Lat, Lng: %f, %f", lat, lng))
	return lat, lng, nil
}
