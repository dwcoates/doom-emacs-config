//go:build darwin

package audiooutput

/*
#cgo LDFLAGS: -framework CoreAudio -framework CoreFoundation
#include <CoreAudio/CoreAudio.h>
#include <CoreFoundation/CoreFoundation.h>

// ao_device answers the default output device, or a CoreAudio status.
static OSStatus ao_device(AudioDeviceID *dev) {
	AudioObjectPropertyAddress a = {kAudioHardwarePropertyDefaultOutputDevice, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMain};
	UInt32 size = sizeof *dev;
	return AudioObjectGetPropertyData(kAudioObjectSystemObject, &a, 0, NULL, &size, dev);
}

// ao_u32 reads one UInt32 property of dev in scope.
static OSStatus ao_u32(AudioDeviceID dev, AudioObjectPropertySelector sel, AudioObjectPropertyScope scope, UInt32 *out) {
	AudioObjectPropertyAddress a = {sel, scope, kAudioObjectPropertyElementMain};
	UInt32 size = sizeof *out;
	return AudioObjectGetPropertyData(dev, &a, 0, NULL, &size, out);
}

// ao_string reads one CFString property of dev into buf as UTF-8.
static OSStatus ao_string(AudioDeviceID dev, AudioObjectPropertySelector sel, char *buf, CFIndex len) {
	AudioObjectPropertyAddress a = {sel, kAudioObjectPropertyScopeGlobal, kAudioObjectPropertyElementMain};
	CFStringRef s = NULL;
	UInt32 size = sizeof s;
	OSStatus st = AudioObjectGetPropertyData(dev, &a, 0, NULL, &size, &s);
	if (st != noErr) {
		return st;
	}
	if (s == NULL) {
		return kAudioHardwareUnspecifiedError;
	}
	Boolean ok = CFStringGetCString(s, buf, len, kCFStringEncodingUTF8);
	CFRelease(s);
	return ok ? noErr : kAudioHardwareUnspecifiedError;
}

// ao_has_data_source reports whether dev's output has a data source.
static Boolean ao_has_data_source(AudioDeviceID dev) {
	AudioObjectPropertyAddress a = {kAudioDevicePropertyDataSource, kAudioDevicePropertyScopeOutput, kAudioObjectPropertyElementMain};
	return AudioObjectHasProperty(dev, &a);
}
*/
import "C"

import "fmt"

// stringCap is the longest device name or UID read, in UTF-8 bytes.
const stringCap = 1024

// Read answers CoreAudio's default output device. A property CoreAudio will
// not answer is an error naming it; a device with no data source answers
// none, which is CoreAudio's own answer for it.
func Read() (Device, error) {
	var dev C.AudioDeviceID
	if st := C.ao_device(&dev); st != 0 {
		return Device{}, fmt.Errorf("read the default output device: CoreAudio status %d", int32(st))
	}
	if dev == C.kAudioObjectUnknown {
		return Device{}, fmt.Errorf("CoreAudio has no default output device")
	}
	var transport C.UInt32
	if st := C.ao_u32(dev, C.kAudioDevicePropertyTransportType, C.kAudioObjectPropertyScopeGlobal, &transport); st != 0 {
		return Device{}, fmt.Errorf("read the output's transport: CoreAudio status %d", int32(st))
	}
	name, err := readString(dev, C.kAudioObjectPropertyName, "name")
	if err != nil {
		return Device{}, err
	}
	uid, err := readString(dev, C.kAudioDevicePropertyDeviceUID, "UID")
	if err != nil {
		return Device{}, err
	}
	d := Device{Name: name, Transport: fourCC(uint32(transport)), UID: uid}
	if C.ao_has_data_source(dev) != 0 {
		var source C.UInt32
		if st := C.ao_u32(dev, C.kAudioDevicePropertyDataSource, C.kAudioDevicePropertyScopeOutput, &source); st != 0 {
			return Device{}, fmt.Errorf("read the output's data source: CoreAudio status %d", int32(st))
		}
		d.DataSource = fourCC(uint32(source))
	}
	return d, nil
}

// readString reads one string property of dev; what names it in the error.
func readString(dev C.AudioDeviceID, sel C.AudioObjectPropertySelector, what string) (string, error) {
	var buf [stringCap]C.char
	if st := C.ao_string(dev, sel, &buf[0], stringCap); st != 0 {
		return "", fmt.Errorf("read the output's %s: CoreAudio status %d", what, int32(st))
	}
	return C.GoString(&buf[0]), nil
}
