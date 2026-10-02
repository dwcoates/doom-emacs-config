package persistentwifi

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"
)

// answer is one scripted run result.
type answer struct {
	out  string
	code int
	err  error
}

// fakeRunner answers each argv from a script. An argv with several answers
// hands them out in order and repeats the last; an unscripted argv fails the
// test. Every call is recorded.
type fakeRunner struct {
	t       *testing.T
	mu      sync.Mutex
	answers map[string][]answer
	calls   []string
}

func newFakeRunner(t *testing.T) *fakeRunner {
	return &fakeRunner{t: t, answers: map[string][]answer{}}
}

// on scripts argv's answers.
func (f *fakeRunner) on(argv string, answers ...answer) *fakeRunner {
	f.answers[argv] = answers
	return f
}

func (f *fakeRunner) Run(_ context.Context, dir string, argv []string) (string, int, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if dir != runDir {
		f.t.Errorf("Run dir = %q, want %q", dir, runDir)
	}
	key := strings.Join(argv, " ")
	f.calls = append(f.calls, key)
	queue, ok := f.answers[key]
	if !ok || len(queue) == 0 {
		f.t.Errorf("unscripted run: %q", key)
		return "", 0, errors.New("unscripted")
	}
	a := queue[0]
	if len(queue) > 1 {
		f.answers[key] = queue[1:]
	}
	return a.out, a.code, a.err
}

// ran answers every recorded call, in order.
func (f *fakeRunner) ran() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.calls...)
}

// count answers how many recorded calls equal argv.
func (f *fakeRunner) count(argv string) int {
	n := 0
	for _, c := range f.ran() {
		if c == argv {
			n++
		}
	}
	return n
}

// fakeClock hands out an After channel the test fires.
type fakeClock struct {
	afters chan chan time.Time
}

func newFakeClock() *fakeClock { return &fakeClock{afters: make(chan chan time.Time, 8)} }

func (c *fakeClock) Now() time.Time { return time.Unix(0, 0) }

func (c *fakeClock) After(time.Duration) <-chan time.Time {
	ch := make(chan time.Time, 1)
	c.afters <- ch
	return ch
}

// tools are the fake layout every test runs against.
var tools = ToolsIn("/t")

// Scripted outputs.
const (
	pmsetOn  = "System-wide power settings:\n SleepDisabled\t\t1\nCurrently in use:\n standby 1\n"
	pmsetOff = "System-wide power settings:\n SleepDisabled\t\t0\nCurrently in use:\n standby 1\n"
	ports    = "Hardware Port: Ethernet\nDevice: en5\n\nHardware Port: Wi-Fi\nDevice: en0\nEthernet Address: aa\n"
	noWifi   = "Hardware Port: Ethernet\nDevice: en5\n"
	hotspot  = "Test Hotspot"
)

// summary composes an `ipconfig getsummary` dictionary.
func summary(active bool, ssid string) string {
	status := "FALSE"
	if active {
		status = "TRUE"
	}
	s := "<dictionary> {\n  BSSID : <redacted>\n  IPv4 : <array> {\n    0 : <dictionary> {\n      SSID : nested\n    }\n  }\n  InterfaceType : WiFi\n  LinkStatusActive : " + status + "\n"
	if ssid != "" {
		s += "  SSID : " + ssid + "\n"
	}
	return s + "}\n"
}

// Argv keys of every tool call.
const (
	argPmsetRead  = "/t/pmset -g"
	argPorts      = "/t/networksetup -listallhardwareports"
	argSummary    = "/t/ipconfig getsummary en0"
	argJoin       = "/t/networksetup -setairportnetwork en0 " + hotspot
	argPreferred  = "/t/networksetup -listpreferredwirelessnetworks en0"
	argDisconnect = "/t/wifi-util disconnect"
	argRadioOff   = "/t/networksetup -setairportpower en0 off"
	argRadioOn    = "/t/networksetup -setairportpower en0 on"
	argDim        = "/t/mac-brightness 0.0625"
	argRestore    = "/t/mac-brightness 1.0"
)

// preferred composes a `networksetup -listpreferredwirelessnetworks` answer.
func preferred(names ...string) string {
	s := "Preferred networks on en0:\n"
	for _, n := range names {
		s += "\t" + n + "\n"
	}
	return s
}

// argPower is one `sudo -n pmset -a <setting> <value>` call.
func argPower(setting, value string) string {
	return "/t/sudo -n /t/pmset -a " + setting + " " + value
}

// scriptPower answers every power setting call with success.
func scriptPower(r *fakeRunner, value string) {
	for _, s := range powerSettings {
		r.on(argPower(s, value), answer{})
	}
}
