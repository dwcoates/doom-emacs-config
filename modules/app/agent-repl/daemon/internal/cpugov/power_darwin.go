//go:build darwin

package cpugov

/*
#cgo LDFLAGS: -framework IOKit -framework CoreFoundation
#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/ps/IOPowerSources.h>
#include <IOKit/ps/IOPSKeys.h>

// cpugov_power answers 1 on AC power, 2 on battery (a UPS's included), -1
// when the power sources could not be copied and -2 when none provides power.
static int cpugov_power(void) {
	CFTypeRef info = IOPSCopyPowerSourcesInfo();
	if (info == NULL) {
		return -1;
	}
	int answer = -2;
	CFStringRef type = IOPSGetProvidingPowerSourceType(info);
	if (type != NULL) {
		if (CFStringCompare(type, CFSTR(kIOPMACPowerKey), 0) == kCFCompareEqualTo) {
			answer = 1;
		} else if (CFStringCompare(type, CFSTR(kIOPMBatteryPowerKey), 0) == kCFCompareEqualTo ||
		           CFStringCompare(type, CFSTR(kIOPMUPSPowerKey), 0) == kCFCompareEqualTo) {
			answer = 2;
		}
	}
	CFRelease(info);
	return answer;
}
*/
import "C"

import "fmt"

// Power reads the source the host runs on: IOKit's providing power source,
// what the menu bar's battery item reports from.
func (*Host) Power() (Power, error) {
	switch answer := C.cpugov_power(); answer {
	case 1:
		return PowerAC, nil
	case 2:
		return PowerBattery, nil
	case -1:
		return PowerUnknown, fmt.Errorf("cpugov: IOPSCopyPowerSourcesInfo answered nothing")
	default:
		return PowerUnknown, fmt.Errorf("cpugov: IOPSGetProvidingPowerSourceType named no AC, battery or UPS source (%d)", int(answer))
	}
}
