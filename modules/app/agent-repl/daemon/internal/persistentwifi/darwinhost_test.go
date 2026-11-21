package persistentwifi

import (
	"context"
	"os"
	"strconv"
	"strings"
	"testing"
)

func TestParseBluetoothPower(t *testing.T) {
	cases := []struct {
		name    string
		out     string
		want    string
		wantErr string
	}{
		{name: "a powered controller is on", out: bluetoothOn, want: "on"},
		{name: "a controller powered down is off", out: bluetoothOff, want: "off"},
		{name: "an answer that is not JSON is an error", out: "nope", wantErr: "system_profiler's Bluetooth answer could not be read"},
		{name: "an answer naming no controller is an error", out: `{"SPBluetoothDataType":[]}`,
			wantErr: "system_profiler named no Bluetooth controller"},
		{name: "a state naming neither is an error carrying it",
			out:     `{"SPBluetoothDataType":[{"controller_properties":{"controller_state":""}}]}`,
			wantErr: `system_profiler named no Bluetooth power state: controller_state ""`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := parseBluetoothPower(tc.out)

			// Assert.
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("parseBluetoothPower error = %v, want one saying %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("parseBluetoothPower = (%q, %v), want %q", got, err, tc.want)
			}
		})
	}
}

// openWaitLostSaid is what `open -W` printed on 2026-11-20 19:06:03.
const openWaitLostSaid = "Unable to block on applications (initial call to kevent() failed: No such process)"

func TestAScanWhoseWaitWasLostAfterTheHelperAnsweredIsTheAnswer(t *testing.T) {
	// Arrange: the helper answered and exited before `open -W` waited.
	h := newHarness(t)
	h.r.onPrefix(argScan, func(argv []string) answer {
		writing(`{"authorization":"authorized","visible":[{"ssid":"Home","rssi":-40,"channel":6}]}`)(argv)
		return answer{out: openWaitLostSaid, code: 1}
	})

	// Act.
	scan, err := h.c.scanWifi(context.Background())

	// Assert.
	if err != nil || len(scan.Visible) != 1 || *scan.Visible[0].Channel != 6 {
		t.Fatalf("scanWifi = (%+v, %v), want the helper's answer", scan, err)
	}
	scans := h.failoverRecords("info", "scanned the visible Wi-Fi networks")
	if len(scans) != 1 || !strings.Contains(scans[0]["launch_wait_lost"].(string), openWaitLostSaid) {
		t.Fatalf("scan records = %+v, want the lost wait kept", scans)
	}
}

func TestAScanWhoseWaitWasLostWithNoAnswerIsAnError(t *testing.T) {
	// Arrange: the helper exited before `open -W` waited, writing nothing.
	h := newHarness(t)
	h.r.onPrefix(argScan, func([]string) answer { return answer{out: openWaitLostSaid, code: 1} })

	// Act.
	_, err := h.c.scanWifi(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "exited before `open -W` could wait on it and wrote no answer") ||
		!strings.Contains(err.Error(), openWaitLostSaid) {
		t.Fatalf("scanWifi error = %v, want the lost wait and the missing answer named", err)
	}
}

func TestAScanThatOpenCouldNotLaunchIgnoresAnyAnswerFile(t *testing.T) {
	// Arrange: open failed some other way, though a file is there.
	h := newHarness(t)
	h.r.onPrefix(argScan, func(argv []string) answer {
		writing(`{"authorization":"authorized","visible":[]}`)(argv)
		return answer{out: "LSOpenURLsWithRole() failed", code: 1}
	})

	// Act.
	_, err := h.c.scanWifi(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the Wi-Fi helper could not be run") {
		t.Fatalf("scanWifi error = %v, want the launch failure", err)
	}
}

func TestAScanNeverReadsAnAnswerLeftFromBefore(t *testing.T) {
	// Arrange: an answer is already at the path; the helper writes none.
	h := newHarness(t)
	h.r.onPrefix(argScan, func([]string) answer { return answer{} })
	host := h.c.host.(*darwinHost)
	stale := host.scanDir + "/agent-repl-wifi-scan-" + strconv.Itoa(os.Getpid()) + "-1.json"
	if err := os.WriteFile(stale, []byte(`{"authorization":"authorized","visible":[]}`), 0o600); err != nil {
		t.Fatal(err)
	}

	// Act.
	_, err := h.c.scanWifi(context.Background())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "wrote no answer") {
		t.Fatalf("scanWifi error = %v, want the stale answer ignored", err)
	}
}
