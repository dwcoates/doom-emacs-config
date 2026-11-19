// Package audiooutput reads the device the host plays its sound through now:
// CoreAudio's default output device, its transport, its UID and, where the
// device has one, its current data source. The daemon reads it in a child of
// its own binary (`agent-repld audio-output`), so a CoreAudio call that hangs
// costs that child's bound and never a daemon thread.
package audiooutput

import (
	"encoding/json"
	"fmt"
	"io"
	"strings"
)

// Verb is the daemon binary's verb that prints the current audio output: the
// one spelling its dispatch and its readers share.
const Verb = "audio-output"

// The transports CoreAudio reports (kAudioDevicePropertyTransportType), as
// their four-character codes.
const (
	TransportBuiltIn     = "bltn"
	TransportBluetooth   = "blue"
	TransportBluetoothLE = "blea"
	TransportUSB         = "usb "
	TransportHDMI        = "hdmi"
	TransportDisplayPort = "dprt"
	TransportAirPlay     = "airp"
)

// The data sources of the built-in output (kAudioDevicePropertyDataSource).
const (
	// DataSourceHeadphones is the built-in output's headphone jack.
	DataSourceHeadphones = "hdpn"
	// DataSourceInternalSpeakers is the built-in output's own speakers.
	DataSourceInternalSpeakers = "ispk"
)

// Device is CoreAudio's default output device. It is the one shape the helper
// prints and the daemon reads, so the two can never disagree about it.
type Device struct {
	// Name is the device's name (kAudioObjectPropertyName).
	Name string `json:"name"`
	// Transport is the device's transport's four-character code.
	Transport string `json:"transport"`
	// UID is the device's persistent UID (kAudioDevicePropertyDeviceUID). A
	// Bluetooth device's names its address: `F8-CB-15-F0-AE-6C:output`.
	UID string `json:"uid"`
	// DataSource is the output's current data source's four-character code,
	// empty for a device that has none (every Bluetooth device).
	DataSource string `json:"data_source,omitempty"`
}

// Encode writes d as the helper's one line of output.
func Encode(w io.Writer, d Device) error {
	return json.NewEncoder(w).Encode(d)
}

// Decode reads the helper's output. Output that is not one Device, or names
// no transport, is an error: the device is never guessed.
func Decode(stdout string) (Device, error) {
	var d Device
	dec := json.NewDecoder(strings.NewReader(stdout))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&d); err != nil {
		return Device{}, fmt.Errorf("decode the audio output: %w", err)
	}
	if d.Transport == "" {
		return Device{}, fmt.Errorf("the audio output names no transport: %q", strings.TrimSpace(stdout))
	}
	return d, nil
}

// BluetoothAddress answers the Bluetooth address a Bluetooth device's UID
// names, in the colon-separated upper case system_profiler reports
// (`F8:CB:15:F0:AE:6C`), and false for a UID that names none.
func BluetoothAddress(uid string) (string, bool) {
	addr, _, _ := strings.Cut(uid, ":")
	parts := strings.Split(addr, "-")
	if len(parts) != 6 {
		return "", false
	}
	for _, p := range parts {
		if len(p) != 2 || strings.Trim(strings.ToLower(p), "0123456789abcdef") != "" {
			return "", false
		}
	}
	return strings.ToUpper(strings.Join(parts, ":")), true
}
