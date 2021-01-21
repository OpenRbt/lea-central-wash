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
	provider        weatherProviderInterface
}

// APIConfig values
type APIConfig struct {
	Name    string
	BaseURL string
}

// APIKeyConfig values
type APIKeyConfig struct {
	APIConfig
	APIKey string
}

type weatherProviderInterface interface {
	CurrentTemperature() (float64, error)
	IsInitialized() bool
	Initialize() error
}

type openWeatherProvider struct {
	APIKeyConfig
	ipV4addr ipV4addr
	lat      float64
	lng      float64
}

type meteoInfoProvider struct {
	APIConfig
}

type coordinates interface {
	LatLng(clientIP ipV4addr) (float64, float64, error)
}

type ipapi struct { // throws RateLimited error all too often on a free plan
}

type ipify struct { // 1000 requests per month on free subscription. Requires environment variable IPIFY_API_KEY to be set
	BaseURL string
	APIKey  string
}

var ipifyClient = &ipify{}

// Instance creates and returns an instance of a WeatherSvc.
func Instance(weatherProviderConfig *APIKeyConfig, coordsConfig *APIKeyConfig) (app.WeatherSvc, error) {
	newInstance := &service{}

	if weatherProviderConfig == nil {
		log.Info("Missing a weather provider config")
		return newInstance, ErrBadConfig
	}
	if weatherProviderConfig.BaseURL == "" {
		log.Info("Missing a weather provider base URL")
		return newInstance, ErrBadConfig
	}

	switch weatherProviderConfig.Name {
	case app.OpenWeather:
		if weatherProviderConfig.APIKey == "" {
			log.Info("Missing OpenWeatherAPIKey")
			return newInstance, ErrBadConfig
		}
		provider := &openWeatherProvider{}
		provider.Name = weatherProviderConfig.Name
		provider.BaseURL = weatherProviderConfig.BaseURL
		provider.APIKey = weatherProviderConfig.APIKey
		newInstance.provider = provider
		break
	case app.MeteoInfo:
		provider := &meteoInfoProvider{}
		provider.Name = weatherProviderConfig.Name
		provider.BaseURL = weatherProviderConfig.BaseURL
		newInstance.provider = provider
		break
	default:
		log.Info("Missing a valid weather provider configuration")
		return newInstance, ErrBadConfig
	}

	if coordsConfig != nil {
		ipifyClient.BaseURL = coordsConfig.BaseURL
		ipifyClient.APIKey = coordsConfig.APIKey
	}

	return newInstance, nil
}

// CurrentTemperature returns current temperature based on the client's IP address
func (s *service) CurrentTemperature() (float64, error) {
	if !s.provider.IsInitialized() {
		err := s.provider.Initialize()
		if err != nil {
			return 0, err
		}
	}

	currentTime := time.Now()
	if (currentTime.Unix() - s.lastMeasurement.time.Unix()) <= MeasurementInterval {
		return s.lastMeasurement.value, nil
	}

	val, err := s.provider.CurrentTemperature()

	if err != nil {
		return val, err
	}

	log.Printf("Temperature at %s is %f\n", currentTime.String(), val)

	s.lastMeasurement.value = val
	s.lastMeasurement.time = currentTime

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
		return 0, err
	}

	return temp, nil
}

func (p *meteoInfoProvider) IsInitialized() bool {
	return true
}

func (p *meteoInfoProvider) Initialize() error {
	return nil
}

// CurrentTemperature returns current temperature based on the client's IP address
func (s *openWeatherProvider) CurrentTemperature() (float64, error) {
	path := fmt.Sprintf("%s?units=metric&lat=%f&lon=%f&appid=%s", s.BaseURL, s.lat, s.lng, s.APIKey)
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
		log.Info(fmt.Sprintf("Failed HTTP request to %s", s.BaseURL))
		return 0, ErrHTTPRequest
	}

	if weatherResponse.StatusCode != http.StatusOK {
		log.Info(fmt.Sprintf("Bad status code %d returned by %s", weatherResponse.StatusCode, s.BaseURL))
		return 0, ErrBadStatusCode
	}

	weatherResponseBody, errIO := ioutil.ReadAll(weatherResponse.Body)
	if errIO != nil {
		return 0, errIO
	}
	var result map[string]interface{}

	json.Unmarshal(weatherResponseBody, &result)
	if result == nil {
		log.Info(fmt.Sprintf("No payload returned by %s", s.BaseURL))
		return 0, ErrNoPayload
	}

	main, hasMain := result["main"].(map[string]interface{})
	if !hasMain {
		log.Info(fmt.Sprintf("Property 'main' not found in the response from %s", s.BaseURL))
		return 0, ErrPropertyNotFound
	}

	temp, hasTemp := main["temp"].(float64)
	if !hasTemp {
		log.Info(fmt.Sprintf("Property 'temp' not found in the response from %s", s.BaseURL))
		return 0, ErrPropertyNotFound
	}

	return temp, nil
}

func (s *openWeatherProvider) IsInitialized() bool {
	return s.ipV4addr != ""
}

func (s *openWeatherProvider) Initialize() error {
	ip, ipErr := clientIP()
	if ipErr != nil {
		return ipErr
	}

	s.ipV4addr = ip

	coords := &ipapi{}
	var lat float64
	var lng float64
	var errCoord error

	lat, lng, errCoord = coords.LatLng(ip)
	if errCoord != nil {
		lat, lng, errCoord = ipifyClient.LatLng(ip)
		if errCoord != nil {
			return errCoord
		}
	}
	s.lat = lat
	s.lng = lng

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
