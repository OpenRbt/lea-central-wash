package svcweather

import (
	"crypto/tls"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"strings"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"
	"github.com/powerman/structlog"
)

var log = structlog.New()

type client struct {
	ipV4addr string
}

// New creates and returns a new WeatherSvc.
func New(request *http.Request) app.WeatherSvc {
	return &client{
		ipV4addr: clientIP(request),
	}
}

const openWeatherURL = "http://api.openweathermap.org/data/2.5/weather"

var (
	openWeatherAPIkey = strGetenv("OPENWEATHER_API_KEY", "")
	ipifyAPIkey       = strGetenv("IPIFY_API_KEY", "")
)

// CurrentTemperature returns current temperature based on the client's IP address
func (c *client) CurrentTemperature() (float64, error) {
	lat, long, err := clientCoordinates(c.ipV4addr)

	if err != nil {
		return 0, err
	}

	if openWeatherAPIkey == "" {
		log.Err("OPENWEATHER_API_KEY is required, please set the environment variable, e.g. by running `export OPENWEATHER_API_KEY=<Your API key>` in the terminal")
		return 0, errors.New("OPENWEATHER_API_KEY is missing")
	}

	weatherRequest, err := http.NewRequest("GET", openWeatherURL+"?units=metric&lat="+lat+"&lon="+long+"&appid="+openWeatherAPIkey, nil)

	client := newClient()
	weatherResponse, err := client.Do(weatherRequest)
	if err != nil {
		log.Err(openWeatherURL + " returned an error: " + err.Error())
		return 0, errors.New(openWeatherURL + " returned an error")
	}
	defer log.WarnIfFail(weatherResponse.Body.Close)

	if weatherResponse.StatusCode != http.StatusOK {
		weatherResponseBody, _ := ioutil.ReadAll(weatherResponse.Body)
		message := fmt.Sprintf("%s returned a status code %d %s", openWeatherURL, weatherResponse.StatusCode, string(weatherResponseBody))
		log.Err(message)
		return 0, errors.New(message)
	}

	weatherResponseBody, _ := ioutil.ReadAll(weatherResponse.Body)

	var result map[string]interface{}

	json.Unmarshal(weatherResponseBody, &result)
	if result == nil {
		const message = openWeatherURL + " returned an empty response"
		log.Err(message)
		return 0, errors.New(message)
	}

	main, hasMain := result["main"].(map[string]interface{})
	if !hasMain {
		const message = openWeatherURL + ": no property 'main' found in the response"
		log.Err(message)
		return 0, errors.New(message)
	}

	temp, hasTemp := main["temp"].(float64)
	if !hasTemp {
		const message = openWeatherURL + ": no property 'temp' found in the response"
		log.Err(message)
		return 0, errors.New(message)
	}

	// log.Info("Temperature is %f\n", temp)

	return temp, nil
}

// clientCoordinates returns the latitude and longitude of the client's IP address
func clientCoordinates(clientIP string) (string, string, error) {
	var coords coordinates

	if ipifyAPIkey != "" {
		coords = new(ipify)
	} else {
		coords = new(ipapi)
	}

	return coords.latLng(clientIP)
}

type coordinates interface {
	latLng(clientIP string) (string, string, error)
}

type ipapi struct { // throws RateLimited error all too often on a free plan
}

type ipify struct { // 1000 requests per month on free subscription. Requires environment variable IPIFY_API_KEY to be set
}

// latLng returns latitude and longitude of the client's IP address
func (coord ipapi) latLng(clientIP string) (string, string, error) {

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

	// log.Info("Lat, Long: " + latlng[0] + ", " + latlng[1])

	return latlng[0], latlng[1], nil
}

// latLng returns latitude and longitude of the client's IP address
// Requires environment variable IPIFY_API_KEY to be set
func (coord ipify) latLng(clientIP string) (string, string, error) {

	URL := "https://geo.ipify.org/api/v1?apiKey=" + ipifyAPIkey + "&ipAddress=" + string(clientIP)

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

	log.Info("Lat, Long: " + fmt.Sprintf("%f", lat) + ", " + fmt.Sprintf("%f", lng))
	return fmt.Sprintf("%f", lat), fmt.Sprintf("%f", lng), nil
}

// clientIP gets a requests IP address by reading off the forwarded-for
// header (for proxies) and falls back to use the remote address.
func clientIP(r *http.Request) string {
	forwarded := r.Header.Get("X-FORWARDED-FOR")
	if forwarded != "" {
		return forwarded
	}
	return r.RemoteAddr
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

func strGetenv(name, def string) string {
	value := os.Getenv(name)
	if value == "" {
		return def
	}
	return value
}
