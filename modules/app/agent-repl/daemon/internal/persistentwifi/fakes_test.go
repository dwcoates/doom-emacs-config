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

// fakeClock hands out an After channel the test fires, and stands at the
// instant the test last set (set), the epoch before.
type fakeClock struct {
	afters chan chan time.Time

	mu  sync.Mutex
	now time.Time
}

func newFakeClock() *fakeClock {
	return &fakeClock{afters: make(chan chan time.Time, 8), now: time.Unix(0, 0)}
}

func (c *fakeClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

// set moves Now to d past the epoch, firing nothing.
func (c *fakeClock) set(d time.Duration) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.now = time.Unix(0, 0).Add(d)
}

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
	// argBrightnessRead reads the display's level.
	argBrightnessRead = "/t/mac-brightness"
	// argLidDim sets the closed lid's level.
	argLidDim = "/t/mac-brightness 0"
)

// argBrightness is one `mac-brightness <level>` call.
func argBrightness(level string) string { return "/t/mac-brightness " + level }

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

// argEnergy is the energy mode step's `sudo -n pmset -a powermode <mode>`
// call for a mode turned on (Low Power Mode) or off (High Power Mode).
func argEnergy(on bool) string {
	if on {
		return argPower("powermode", energyModeLowPower)
	}
	return argPower("powermode", energyModeHighPower)
}

// scriptPower answers every privileged write of a change turning the mode to
// value -- each power setting and the energy mode -- with success.
func scriptPower(r *fakeRunner, value string) {
	for _, s := range powerSettings {
		r.on(argPower(s, value), answer{})
	}
	r.on(argEnergy(value == "1"), answer{})
}

// fakeLidDims is the durable pre-close brightness: a level, and an error for
// each operation. Every put and clear is recorded.
type fakeLidDims struct {
	mu                       sync.Mutex
	level                    *float64
	getErr, putErr, clearErr error
	puts                     []float64
	clears                   int
}

func (f *fakeLidDims) LidDimLevel(context.Context) (float64, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.getErr != nil {
		return 0, false, f.getErr
	}
	if f.level == nil {
		return 0, false, nil
	}
	return *f.level, true, nil
}

func (f *fakeLidDims) PutLidDimLevel(_ context.Context, level float64) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.putErr != nil {
		return f.putErr
	}
	f.puts = append(f.puts, level)
	f.level = &level
	return nil
}

func (f *fakeLidDims) ClearLidDimLevel(context.Context) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.clearErr != nil {
		return f.clearErr
	}
	f.clears++
	f.level = nil
	return nil
}

// kept answers the stored level, nil when none.
func (f *fakeLidDims) kept() *float64 {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.level
}

// fakeLidWatcher hands Watch's heard every lid sent on lids, then answers
// end once lids is closed, or nil when ctx ends first.
type fakeLidWatcher struct {
	lids chan Lid
	end  error
}

func newFakeLidWatcher() *fakeLidWatcher { return &fakeLidWatcher{lids: make(chan Lid)} }

func (f *fakeLidWatcher) Watch(ctx context.Context, heard func(Lid)) error {
	for {
		select {
		case <-ctx.Done():
			return nil
		case l, ok := <-f.lids:
			if !ok {
				return f.end
			}
			heard(l)
		}
	}
}
