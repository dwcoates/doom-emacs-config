package harness

import (
	"errors"
	"os"
	"strings"
	"testing"
)

func TestRequireBackgroundPriority(t *testing.T) {
	cases := []struct {
		name    string
		env     map[string]string
		wantErr error
	}{
		{name: "unset marker is refused", env: map[string]string{}, wantErr: errNotBackground},
		{name: "empty marker is refused", env: map[string]string{BackgroundPriorityEnv: ""}, wantErr: errNotBackground},
		{name: "the nice-19 marker is admitted", env: map[string]string{BackgroundPriorityEnv: "nice-19"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getenv := func(key string) string { return tc.env[key] }

			// Act
			err := requireBackgroundPriority(getenv)

			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("requireBackgroundPriority() = %v, want %v", err, tc.wantErr)
			}
		})
	}
}

func TestBackgroundRefusalNamesTheHelper(t *testing.T) {
	// Arrange / Act
	msg := errNotBackground.Error()

	// Assert
	if !strings.Contains(msg, "bin/background.sh") {
		t.Fatalf("refusal %q does not name bin/background.sh", msg)
	}
}

// This very run reached its tests through WithRunRoot, so it must carry the
// marker: the gate is live, not merely defined.
func TestThisRunCarriesTheBackgroundMarker(t *testing.T) {
	// Arrange / Act
	err := requireBackgroundPriority(os.Getenv)

	// Assert
	if err != nil {
		t.Fatalf("this run passed WithRunRoot without the marker: %v", err)
	}
}
