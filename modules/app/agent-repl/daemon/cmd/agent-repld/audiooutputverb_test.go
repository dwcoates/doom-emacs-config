package main

import (
	"bytes"
	"errors"
	"strings"
	"testing"

	"agent-repld/internal/audiooutput"
)

// failingWriter refuses every write.
type failingWriter struct{}

func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("broken pipe") }

func TestRunAudioOutputVerb(t *testing.T) {
	device := audiooutput.Device{Name: "AirPods", Transport: audiooutput.TransportBluetooth, UID: "F8-CB-15-F0-AE-6C:output"}
	cases := []struct {
		name       string
		readErr    error
		failWrite  bool
		wantCode   int
		wantStderr string
	}{
		{name: "prints the device", wantCode: exitSuccess},
		{name: "a failed read is said and fails", readErr: errors.New("CoreAudio has no default output device"), wantCode: exitFailure, wantStderr: "CoreAudio has no default output device"},
		{name: "a failed write is said and fails", failWrite: true, wantCode: exitFailure, wantStderr: "write the audio output: broken pipe"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var stdout bytes.Buffer
			var stderr strings.Builder
			read := func() (audiooutput.Device, error) { return device, tc.readErr }

			// Act
			var code int
			if tc.failWrite {
				code = runAudioOutputVerb(read, failingWriter{}, &stderr)
			} else {
				code = runAudioOutputVerb(read, &stdout, &stderr)
			}

			// Assert
			if code != tc.wantCode || !strings.Contains(stderr.String(), tc.wantStderr) {
				t.Fatalf("code %d stderr %q, want %d and %q", code, stderr.String(), tc.wantCode, tc.wantStderr)
			}
			if tc.wantCode == exitSuccess {
				got, err := audiooutput.Decode(stdout.String())
				if err != nil || got != device {
					t.Fatalf("printed %q (%+v, %v), want the device", stdout.String(), got, err)
				}
			}
		})
	}
}
