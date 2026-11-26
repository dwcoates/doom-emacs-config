package persistentwifi

import (
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

func TestValidateUpdate(t *testing.T) {
	tests := []struct {
		name    string
		req     *agentreplv1.UpdatePersistentWifiModeRequest
		wantErr error
	}{
		{"on is valid", &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_On{On: &agentreplv1.UpdatePersistentWifiModeOn{}}}, nil},
		{"off is valid", &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_Off{Off: &agentreplv1.UpdatePersistentWifiModeOff{}}}, nil},
		{"toggle is valid", &agentreplv1.UpdatePersistentWifiModeRequest{Action: &agentreplv1.UpdatePersistentWifiModeRequest_Toggle{Toggle: &agentreplv1.UpdatePersistentWifiModeToggle{}}}, nil},
		{"no action is refused", &agentreplv1.UpdatePersistentWifiModeRequest{}, ErrNoAction},
		{"no request is refused", nil, ErrNoAction},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			valid, err := ValidateUpdate(tt.req)

			// Assert
			if !errors.Is(err, tt.wantErr) {
				t.Fatalf("ValidateUpdate() error = %v, want %v", err, tt.wantErr)
			}
			if tt.wantErr == nil && valid.req != tt.req {
				t.Fatalf("the valid update holds %v, want the request it was made from", valid.req)
			}
		})
	}
}
