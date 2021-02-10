//go:generate genny -in=$GOFILE -out=gen-$GOFILE gen "Kasse=SetKasse"

package extapi

import (
	"github.com/DiaElectronics/lea-central-wash/cmd/storage/internal/def"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/model"
	"github.com/DiaElectronics/lea-central-wash/storageapi/restapi/op"
	"github.com/go-openapi/swag"
)

//nolint:dupl,goconst
func errKasse(log Log, err error, code errCode) op.KasseResponder { //nolint:deadcode,unused
	if code.status < 500 {
		log.Info("client error", def.LogHTTPStatus, code.status, "code", code.extra, "err", err)
	} else {
		log.PrintErr("server error", def.LogHTTPStatus, code.status, "code", code.extra, "err", err)
	}

	msg := err.Error()
	if code.status == 500 { // Do no expose details about internal errors.
		msg = "internal error"
	}

	return op.NewKasseDefault(code.status).WithPayload(&model.Error{
		Code:    swag.Int32(code.extra),
		Message: swag.String(msg),
	})
}
