package svcweather

import (
	"crypto/tls"
	"fmt"
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
		return "", errHTTPNet
	}

	client := newClient()
	respIP, errHTTPReq := client.Do(reqIP)
	if respIP != nil {
		defer log.WarnIfFail(respIP.Body.Close)
	}

	if errHTTPReq != nil {
		log.Info(fmt.Sprintf("Failed HTTP request to %s", url))
		return "", ErrHTTPRequest
	}

	if respIP.StatusCode != http.StatusOK {
		log.Info(fmt.Sprintf("Bad status code %d returned by %s", respIP.StatusCode, url))
		return "", ErrBadStatusCode
	}

	ip, errIO := ioutil.ReadAll(respIP.Body)

	if errIO != nil {
		return "", errIO
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
