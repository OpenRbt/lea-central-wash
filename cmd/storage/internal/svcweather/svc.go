package svcweather

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"

	"github.com/powerman/structlog"
)

var log = structlog.New()

// MeasurementInterval is the time interval (in seconds) between fetching new temperature values from a remote service
const MeasurementInterval = 300

type ipV4addr string

type timeValPair struct {
	time  time.Time
	value float64
}

type service struct {
	ipV4addr           ipV4addr
	lat                string
	lng                string
	lastMeasurement    timeValPair
	openWeatherBaseURL string
	openWeatherAPIKey  string
}

// APIConfig values
type APIConfig struct {
	BaseURL string
	APIKey  string
}

type coordinates interface {
	LatLng(clientIP ipV4addr) (string, string, error)
}

type ipapi struct { // throws RateLimited error all too often on a free plan
}

type ipify struct { // 1000 requests per month on free subscription. Requires environment variable IPIFY_API_KEY to be set
	BaseURL string
	APIKey  string
}

var ipifyClient = &ipify{}

// Instance creates and returns an instance of a WeatherSvc.
func Instance(openWeatherConfig *APIConfig, coordsConfig *APIConfig) (app.WeatherSvc, error) {
	newInstance := &service{}

	if openWeatherConfig == nil {
		err := &ErrBadConfig{
			Param: "openWeatherConfig",
		}
		return newInstance, err.Error()
	}
	if openWeatherConfig.BaseURL == "" {
		err := &ErrBadConfig{
			Param: "OpenWeatherBaseURL",
		}
		return newInstance, err.Error()
	}
	if openWeatherConfig.APIKey == "" {
		err := &ErrBadConfig{
			Param: "OPENWEATHER_API_KEY",
		}
		return newInstance, err.Error()
	}
	newInstance.openWeatherBaseURL = openWeatherConfig.BaseURL
	newInstance.openWeatherAPIKey = openWeatherConfig.APIKey

	if coordsConfig != nil {
		ipifyClient.BaseURL = coordsConfig.BaseURL
		ipifyClient.APIKey = coordsConfig.APIKey
	}

	return newInstance, nil
}

// CurrentTemperature returns current temperature based on the client's IP address
func (s *service) CurrentTemperature() (float64, error) {
	if !s.isInitialized() {
		err := s.initialize()
		if err != nil {
			return 0, err
		}
	}

	currentTime := time.Now()
	if (currentTime.Unix() - s.lastMeasurement.time.Unix()) <= MeasurementInterval {
		return s.lastMeasurement.value, nil
	}

	val, err := s.currentTemperature()

	if err != nil {
		return val, err
	}

	log.Printf("Temperature at %s is %f\n", currentTime.String(), val)

	s.lastMeasurement.value = val
	s.lastMeasurement.time = currentTime

	return val, err
}

// CurrentTemperature returns current temperature based on the client's IP address
func (s *service) currentTemperature() (float64, error) {
	path := fmt.Sprintf("%s?units=metric&lat=%s&lon=%s&appid=%s", s.openWeatherBaseURL, s.lat, s.lng, s.openWeatherAPIKey)
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
		err := &ErrFailedHTTPRequest{
			URL: s.openWeatherBaseURL,
		}
		return 0, err.Error()
	}

	if weatherResponse.StatusCode != http.StatusOK {
		weatherResponseBody, errIO := ioutil.ReadAll(weatherResponse.Body)
		if errIO != nil {
			return 0, errIO
		}
		err := &ErrBadStatusCode{
			URL:        s.openWeatherBaseURL,
			StatusCode: weatherResponse.StatusCode,
			Reason:     string(weatherResponseBody),
		}
		return 0, err.Error()
	}

	weatherResponseBody, errIO := ioutil.ReadAll(weatherResponse.Body)
	if errIO != nil {
		return 0, errIO
	}
	var result map[string]interface{}

	json.Unmarshal(weatherResponseBody, &result)
	if result == nil {
		err := &ErrNoPayload{
			URL: s.openWeatherBaseURL,
		}
		return 0, err.Error()
	}

	main, hasMain := result["main"].(map[string]interface{})
	if !hasMain {
		err := &ErrNoPropertyFound{
			URL:      s.openWeatherBaseURL,
			Property: "main",
		}
		return 0, err.Error()
	}

	temp, hasTemp := main["temp"].(float64)
	if !hasTemp {
		err := &ErrNoPropertyFound{
			URL:      s.openWeatherBaseURL,
			Property: "temp",
		}
		return 0, err.Error()
	}

	return temp, nil
}

func (s *service) isInitialized() bool {
	return s.ipV4addr != ""
}

func (s *service) initialize() error {
	ip, ipErr := clientIP()
	if ipErr != nil {
		return ipErr
	}

	s.ipV4addr = ip

	coords := &ipapi{}
	var lat string
	var lng string
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
func (coord ipapi) LatLng(clientIP ipV4addr) (string, string, error) {
	const baseURL = "https://ipapi.co/"

	path := fmt.Sprintf("%s%s/latlong", baseURL, string(clientIP)) // throws RateLimited error all too often on a free plan

	reqLatLng, errHTTPNet := http.NewRequest("GET", path, nil)
	if errHTTPNet != nil {
		return "", "", errHTTPNet
	}

	client := newClient()
	respLatLng, errHTTPReq := client.Do(reqLatLng)
	if respLatLng != nil {
		defer log.WarnIfFail(respLatLng.Body.Close)
	}

	if errHTTPReq != nil {
		err := &ErrFailedHTTPRequest{
			URL: baseURL,
		}
		return "", "", err.Error()
	}

	if respLatLng.StatusCode != http.StatusOK {
		bodyLatLng, errIO := ioutil.ReadAll(respLatLng.Body)
		if errIO != nil {
			return "", "", errIO
		}
		err := &ErrBadStatusCode{
			URL:        baseURL,
			StatusCode: respLatLng.StatusCode,
			Reason:     string(bodyLatLng),
		}
		return "", "", err.Error()
	}

	bodyLatLong, errIO := ioutil.ReadAll(respLatLng.Body)
	if errIO != nil {
		return "", "", errIO
	}
	latlng := strings.Split(string(bodyLatLong), ",")

	log.Info("IPAPI Lat, Long: " + latlng[0] + ", " + latlng[1])

	return latlng[0], latlng[1], nil
}

// LatLng returns latitude and longitude of the client's IP address
func (coord ipify) LatLng(clientIP ipV4addr) (string, string, error) {
	path := fmt.Sprintf("%s?apiKey=%s&ipAddress=%s", coord.BaseURL, coord.APIKey, string(clientIP))

	reqLatLng, errHTTPNet := http.NewRequest("GET", path, nil)
	if errHTTPNet != nil {
		return "", "", errHTTPNet
	}

	client := newClient()
	respLatLng, errHTTPReq := client.Do(reqLatLng)
	if respLatLng != nil {
		defer log.WarnIfFail(respLatLng.Body.Close)
	}

	if errHTTPReq != nil {
		err := &ErrFailedHTTPRequest{
			URL: coord.BaseURL,
		}
		return "", "", err.Error()
	}

	if respLatLng.StatusCode != http.StatusOK {
		bodyLatLng, errIO := ioutil.ReadAll(respLatLng.Body)
		if errIO != nil {
			return "", "", errIO
		}
		err := &ErrBadStatusCode{
			URL:        coord.BaseURL,
			StatusCode: respLatLng.StatusCode,
			Reason:     string(bodyLatLng),
		}
		return "", "", err.Error()
	}

	bodyLatLng, errIO := ioutil.ReadAll(respLatLng.Body)
	if errIO != nil {
		return "", "", errIO
	}

	var result map[string]interface{}
	json.Unmarshal(bodyLatLng, &result)
	if result == nil {
		err := &ErrNoPayload{
			URL: coord.BaseURL,
		}
		return "", "", err.Error()
	}

	location, hasLocation := result["location"].(map[string]interface{})
	if !hasLocation {
		err := &ErrNoPropertyFound{
			URL:      coord.BaseURL,
			Property: "location",
		}
		return "", "", err.Error()
	}

	lat, hasLat := location["lat"].(float64)
	if !hasLat {
		err := &ErrNoPropertyFound{
			URL:      coord.BaseURL,
			Property: "lat",
		}
		return "", "", err.Error()
	}

	lng, hasLng := location["lng"].(float64)
	if !hasLng {
		err := &ErrNoPropertyFound{
			URL:      coord.BaseURL,
			Property: "lng",
		}
		return "", "", err.Error()
	}

	log.Info("IPIFY Lat, Long: " + fmt.Sprintf("%f", lat) + ", " + fmt.Sprintf("%f", lng))
	return fmt.Sprintf("%f", lat), fmt.Sprintf("%f", lng), nil
}
