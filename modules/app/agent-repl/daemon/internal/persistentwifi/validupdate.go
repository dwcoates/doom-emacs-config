package persistentwifi

import (
	"errors"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// ErrNoAction refuses a mode change that names no action.
var ErrNoAction = errors.New("persistentwifi: a persistent-wifi action arm is required")

// ValidUpdate is a mode change that names an action: the only request
// Controller.Update takes, and ValidateUpdate its only constructor, so a
// request nobody validated cannot reach the controller. Its field is
// unexported for that reason.
type ValidUpdate struct {
	req *agentreplv1.UpdatePersistentWifiModeRequest
}

// Request is the validated request, for a caller that reads it (a test's
// fake) and cannot construct another.
func (v ValidUpdate) Request() *agentreplv1.UpdatePersistentWifiModeRequest { return v.req }

// ValidateUpdate is the one validator of a mode change, whoever receives it
// (the daemon's topbar handler, the service's own server). It refuses a
// request naming no action with ErrNoAction.
func ValidateUpdate(req *agentreplv1.UpdatePersistentWifiModeRequest) (ValidUpdate, error) {
	if req.GetAction() == nil {
		return ValidUpdate{}, ErrNoAction
	}
	return ValidUpdate{req: req}, nil
}
