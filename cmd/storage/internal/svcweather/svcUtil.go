package svcweather

import (
	"crypto/tls"
	"io/ioutil"
	"net"
	"net/http"
	"time"
)

func clientIP() (ipV4addr, error) {
	const url = "https://api.ipify.org?format=text" // we are using a pulib IP API, we're using ipify here, below are some others
	// https://www.ipify.org
	// http://myexternalip.com
	// http://api.ident.me
	// http://whatismyipaddress.com/api

	reqIP, errHTTPNet := http.NewRequest("GET", url, nil)
	if errHTTPNet != nil {
		log.Fatal(errHTTPNet)
	}

	client := newClient()
	respIP, errHTTPReq := client.Do(reqIP)
	if respIP != nil {
		defer log.WarnIfFail(respIP.Body.Close)
	}

	if errHTTPReq != nil {
		err := &ErrFailedHTTPRequest{
			URL: url,
		}
		return "", err.Error()
	}

	if respIP.StatusCode != http.StatusOK {
		bodyIP, errIO := ioutil.ReadAll(respIP.Body)
		if errIO != nil {
			log.Fatal(errIO)
		}
		err := &ErrBadStatusCode{
			URL:        url,
			StatusCode: respIP.StatusCode,
			Reason:     string(bodyIP),
		}
		return "", err.Error()
	}

	ip, errIO := ioutil.ReadAll(respIP.Body)

	if errIO != nil {
		log.Fatal(errIO)
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
