package audiooutput

import (
	"strings"
	"testing"
)

func TestEncodeDecodeRoundTrip(t *testing.T) {
	cases := []struct {
		name   string
		device Device
	}{
		{name: "a bluetooth device", device: Device{Name: "Dodge’s AirPods Pro", Transport: TransportBluetooth, UID: "F8-CB-15-F0-AE-6C:output"}},
		{name: "the built-in output with its data source", device: Device{Name: "MacBook Pro Speakers", Transport: TransportBuiltIn, UID: "BuiltInSpeakerDevice", DataSource: DataSourceInternalSpeakers}},
		{name: "a transport with a space", device: Device{Name: "DAC", Transport: TransportUSB, UID: "usb-1"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var b strings.Builder
			if err := Encode(&b, tc.device); err != nil {
				t.Fatalf("Encode: %v", err)
			}

			// Act
			got, err := Decode(b.String())

			// Assert
			if err != nil || got != tc.device {
				t.Fatalf("Decode = %+v, %v, want %+v", got, err, tc.device)
			}
		})
	}
}

func TestDecodeRefusesWhatIsNoDevice(t *testing.T) {
	cases := []struct {
		name    string
		stdout  string
		wantErr string
	}{
		{name: "not json", stdout: "garbage", wantErr: "decode the audio output"},
		{name: "empty", stdout: "", wantErr: "decode the audio output"},
		{name: "an unknown field", stdout: `{"name":"x","transport":"blue","uid":"u","class":"Headphones"}`, wantErr: "decode the audio output"},
		{name: "no transport", stdout: `{"name":"x","uid":"u"}`, wantErr: "names no transport"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := Decode(tc.stdout)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
				t.Fatalf("Decode err = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func TestBluetoothAddress(t *testing.T) {
	cases := []struct {
		name   string
		uid    string
		want   string
		wantOK bool
	}{
		{name: "an output uid", uid: "F8-CB-15-F0-AE-6C:output", want: "F8:CB:15:F0:AE:6C", wantOK: true},
		{name: "lower case", uid: "f8-cb-15-f0-ae-6c:output", want: "F8:CB:15:F0:AE:6C", wantOK: true},
		{name: "no suffix", uid: "F8-CB-15-F0-AE-6C", want: "F8:CB:15:F0:AE:6C", wantOK: true},
		{name: "the built-in output's uid", uid: "BuiltInSpeakerDevice", wantOK: false},
		{name: "five parts", uid: "F8-CB-15-F0-AE:output", wantOK: false},
		{name: "a part that is not hex", uid: "F8-CB-15-F0-AE-ZZ:output", wantOK: false},
		{name: "a part of three digits", uid: "F8-CB-15-F0-AE-6C0:output", wantOK: false},
		{name: "empty", uid: "", wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, ok := BluetoothAddress(tc.uid)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("BluetoothAddress(%q) = %q, %v, want %q, %v", tc.uid, got, ok, tc.want, tc.wantOK)
			}
		})
	}
}
