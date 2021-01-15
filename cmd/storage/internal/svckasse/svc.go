package svckasse

import (
	"crypto/tls"
	"errors"
	"fmt"
	"io/ioutil"
	"net"
	"net/http"
	"time"

	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/app"

	"github.com/powerman/structlog"
)

var log = structlog.New()

type Config struct {
	Endpoint string
}

type client struct {
	cfg Config
}

// New creates and returns new KasseSvc.
func New(cfg Config) app.KasseSvc {
	return &client{
		cfg: cfg,
	}
}

func (c *client) Info() (string, error) {
	client := newClient()
	req, err := http.NewRequest("GET", fmt.Sprintf("%s/info", c.cfg.Endpoint), nil)
	if err != nil {
		return "", err
	}
	resp, err := client.Do(req)
	if err != nil {
		return "", err
	}
	defer log.WarnIfFail(resp.Body.Close)
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	if resp.StatusCode != http.StatusOK {
		return "", errors.New("bad code")
	}
	return string(body), nil
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
