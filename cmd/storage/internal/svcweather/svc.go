package svcweather

import (
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/def"
	"github.com/powerman/structlog"
)

// MeasurementInterval is the time interval (in seconds) between fetching new temperature values from a remote service
const MeasurementInterval = 300

type ipV4addr string

type timeValPair struct {
	time  time.Time
	value float64
}

type service struct {
	ipV4addr        ipV4addr
	lat             string
	lng             string
	lastMeasurement timeValPair
}

var log = structlog.New()

var instance = &service{
	ipV4addr:        "",
	lat:             "",
	lng:             "",
	lastMeasurement: timeValPair{},
}

// Instance creates and returns an instance of a WeatherSvc.
func Instance() app.WeatherSvc {
	if instance.ipV4addr == "" {
		ip, ipErr := clientIP()
		if ipErr != nil {
			panic(ipErr)
		}
		lat, lng, coordErr := clientCoordinates(ip)
		if coordErr != nil {
			panic(ipErr)
		}
		instance.ipV4addr = ip
		instance.lat = lat
		instance.lng = lng
	}
	return instance
}

// CurrentTemperature returns current temperature based on the client's IP address
func (s *service) CurrentTemperature() (float64, error) {
	return s.withLastMeasurementCheck(s.currentTemperature)
}

func (s *service) withLastMeasurementCheck(f func() (float64, error)) (float64, error) {

	currentTime := time.Now()
	if (currentTime.Unix() - s.lastMeasurement.time.Unix()) <= MeasurementInterval {
		return s.lastMeasurement.value, nil
	}

	val, err := f()

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

	if def.OpenWeatherAPIkey == "" {
		log.Err("OPENWEATHER_API_KEY is required, please set the environment variable, e.g. by running `export OPENWEATHER_API_KEY=<Your API key>` in the terminal")
		return 0, errors.New("OPENWEATHER_API_KEY is missing")
	}

	weatherRequest, err := http.NewRequest("GET", def.OpenWeatherURL+"?units=metric&lat="+s.lat+"&lon="+s.lng+"&appid="+def.OpenWeatherAPIkey, nil)

	client := newClient()
	weatherResponse, err := client.Do(weatherRequest)
	if err != nil {
		log.Err(def.OpenWeatherURL + " returned an error: " + err.Error())
		return 0, errors.New(def.OpenWeatherURL + " returned an error")
	}
	defer log.WarnIfFail(weatherResponse.Body.Close)

	if weatherResponse.StatusCode != http.StatusOK {
		weatherResponseBody, _ := ioutil.ReadAll(weatherResponse.Body)
		message := fmt.Sprintf("%s returned a status code %d %s", def.OpenWeatherURL, weatherResponse.StatusCode, string(weatherResponseBody))
		log.Err(message)
		return 0, errors.New(message)
	}

	weatherResponseBody, _ := ioutil.ReadAll(weatherResponse.Body)

	var result map[string]interface{}

	json.Unmarshal(weatherResponseBody, &result)
	if result == nil {
		message := def.OpenWeatherURL + " returned an empty response"
		log.Err(message)
		return 0, errors.New(message)
	}

	main, hasMain := result["main"].(map[string]interface{})
	if !hasMain {
		message := def.OpenWeatherURL + ": no property 'main' found in the response"
		log.Err(message)
		return 0, errors.New(message)
	}

	temp, hasTemp := main["temp"].(float64)
	if !hasTemp {
		message := def.OpenWeatherURL + ": no property 'temp' found in the response"
		log.Err(message)
		return 0, errors.New(message)
	}

	return temp, nil
}

type coordinates interface {
	latLng(clientIP ipV4addr) (string, string, error)
}

type ipapi struct { // throws RateLimited error all too often on a free plan
}

type ipify struct { // 1000 requests per month on free subscription. Requires environment variable IPIFY_API_KEY to be set
}

// clientCoordinates returns the latitude and longitude of the client's IP address
func clientCoordinates(clientIP ipV4addr) (string, string, error) {
	var coords coordinates

	coords = new(ipapi) // Try a free provider first, may throw a RateLimited error
	lat, lng, err := coords.latLng(clientIP)

	if err != nil && def.IpifyAPIkey != "" {
		coords = new(ipify) // Try a provider with monthly subscription. May exceed the subscription rate
		lat, lng, err = coords.latLng(clientIP)
	}

	return lat, lng, err
}

// latLng returns latitude and longitude of the client's IP address
func (coord ipapi) latLng(clientIP ipV4addr) (string, string, error) {

	URL := "https://ipapi.co/" + string(clientIP) + "/latlong/" // throws RateLimited error all too often on a free plan

	reqLatLng, err := http.NewRequest("GET", URL, nil)
	client := newClient()
	respLatLng, err := client.Do(reqLatLng)

	if err != nil {
		log.Err(URL + " returned an error " + err.Error())
		return "", "", errors.New(URL + " returned an error")
	}
	defer log.WarnIfFail(respLatLng.Body.Close)

	if respLatLng.StatusCode != http.StatusOK {
		bodyLatLng, _ := ioutil.ReadAll(respLatLng.Body)
		message := fmt.Sprintf("%s returned a status code %d %s", URL, respLatLng.StatusCode, string(bodyLatLng))
		log.Err(message)
		return "", "", errors.New(message)
	}

	bodyLatLong, _ := ioutil.ReadAll(respLatLng.Body)

	latlng := strings.Split(string(bodyLatLong), ",")

	log.Info("IPAPI Lat, Long: " + latlng[0] + ", " + latlng[1])

	return latlng[0], latlng[1], nil
}

// latLng returns latitude and longitude of the client's IP address
// Requires environment variable IPIFY_API_KEY to be set
func (coord ipify) latLng(clientIP ipV4addr) (string, string, error) {

	URL := "https://geo.ipify.org/api/v1?apiKey=" + def.IpifyAPIkey + "&ipAddress=" + string(clientIP)

	reqLatLng, err := http.NewRequest("GET", URL, nil)
	client := newClient()
	respLatLng, err := client.Do(reqLatLng)

	if err != nil {
		message := URL + " returned an error"
		log.Err(message + " " + err.Error())
		return "", "", errors.New(message)
	}
	defer log.WarnIfFail(respLatLng.Body.Close)

	if respLatLng.StatusCode != http.StatusOK {
		bodyLatLng, _ := ioutil.ReadAll(respLatLng.Body)
		message := fmt.Sprintf("%s returned a status code %d %s", URL, respLatLng.StatusCode, string(bodyLatLng))
		log.Err(message)
		return "", "", errors.New(message)
	}

	bodyLatLng, _ := ioutil.ReadAll(respLatLng.Body)

	var result map[string]interface{}
	json.Unmarshal(bodyLatLng, &result)
	if result == nil {
		message := URL + " returned an empty response"
		log.Err(message)
		return "", "", errors.New(message)
	}

	location, hasLocation := result["location"].(map[string]interface{})
	if !hasLocation {
		message := URL + ": no property 'location' found in the response"
		log.Err(message)
		return "", "", errors.New(message)
	}

	lat, hasLat := location["lat"].(float64)
	if !hasLat {
		message := URL + ": no property 'lat' found in the response"
		log.Err(message)
		return "", "", errors.New(message)
	}

	lng, hasLng := location["lng"].(float64)
	if !hasLng {
		message := URL + ": no property 'lng' found in the response"
		log.Err(message)
		return "", "", errors.New(message)
	}

	log.Info("IPIFY Lat, Long: " + fmt.Sprintf("%f", lat) + ", " + fmt.Sprintf("%f", lng))
	return fmt.Sprintf("%f", lat), fmt.Sprintf("%f", lng), nil
}

func clientIP() (ipV4addr, error) {
	const url = "https://api.ipify.org?format=text" // we are using a pulib IP API, we're using ipify here, below are some others
	// https://www.ipify.org
	// http://myexternalip.com
	// http://api.ident.me
	// http://whatismyipaddress.com/api

	respIP, respErr := http.Get(url)

	if respErr != nil {
		panic(respErr)
	}

	defer log.WarnIfFail(respIP.Body.Close)

	if respIP.StatusCode != http.StatusOK {
		log.Err("%s returned a status code %d", url, respIP.StatusCode)
		return "", fmt.Errorf("%s returned a status code %d", url, respIP.StatusCode)
	}

	ip, err := ioutil.ReadAll(respIP.Body)

	if err != nil {
		panic(err)
	}

	return ipV4addr(ip), nil
}

func newClient() *http.Client {
	return &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{InsecureSkipVerify: true}, //nolint:gosec
			Proxy:           http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
				DualStack: true,
			}).DialContext,
		},
	}
}
