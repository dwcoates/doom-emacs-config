package persistentwifi

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/appfreeze"
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
	// before runs, outside the fake's own lock, before argv's call is
	// answered: the controller acting while one of its calls is in flight.
	before map[string]func(ctx context.Context)
	// prefixed answers an argv no exact script names that starts with one of
	// its keys, handed the argv: a call whose last argument varies (a scan's
	// answer file).
	prefixed map[string]func(argv []string) answer
	// blocking are prefixes whose calls wait for their context to end, then
	// answer its error: a tool that never answers.
	blocking map[string]bool
}

// onPrefix answers every argv starting with PREFIX through FN.
func (f *fakeRunner) onPrefix(prefix string, fn func(argv []string) answer) *fakeRunner {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.prefixed == nil {
		f.prefixed = map[string]func(argv []string) answer{}
	}
	f.prefixed[prefix] = fn
	return f
}

// hangPrefix makes every argv starting with PREFIX wait until its context
// ends, then answer the context's error.
func (f *fakeRunner) hangPrefix(prefix string) *fakeRunner {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.blocking == nil {
		f.blocking = map[string]bool{}
	}
	f.blocking[prefix] = true
	return f
}

// whileRunning makes HOOK run as argv's call is in flight, handed the call's
// context.
func (f *fakeRunner) whileRunning(argv string, hook func(ctx context.Context)) *fakeRunner {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.before == nil {
		f.before = map[string]func(ctx context.Context){}
	}
	f.before[argv] = hook
	return f
}

func newFakeRunner(t *testing.T) *fakeRunner {
	return &fakeRunner{t: t, answers: map[string][]answer{}}
}

// on scripts argv's answers.
func (f *fakeRunner) on(argv string, answers ...answer) *fakeRunner {
	f.answers[argv] = answers
	return f
}

func (f *fakeRunner) Run(ctx context.Context, dir string, argv []string) (string, int, error) {
	f.mu.Lock()
	hook := f.before[strings.Join(argv, " ")]
	hang := false
	for prefix := range f.blocking {
		hang = hang || strings.HasPrefix(strings.Join(argv, " "), prefix)
	}
	if hang {
		f.calls = append(f.calls, strings.Join(argv, " "))
	}
	f.mu.Unlock()
	if hang {
		<-ctx.Done()
		return "", 0, ctx.Err()
	}
	if hook != nil {
		hook(ctx)
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	if dir != runDir {
		f.t.Errorf("Run dir = %q, want %q", dir, runDir)
	}
	key := strings.Join(argv, " ")
	f.calls = append(f.calls, key)
	queue, ok := f.answers[key]
	if !ok {
		for prefix, fn := range f.prefixed {
			if strings.HasPrefix(key, prefix) {
				a := fn(argv)
				return a.out, a.code, a.err
			}
		}
	}
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

// pmsetIn is `pmset -g` for a mode on or off and an energy mode in use (empty
// names none).
func pmsetIn(on bool, energy string) string {
	out := pmsetOff
	if on {
		out = pmsetOn
	}
	if energy != "" {
		out += " powermode            " + energy + "\n"
	}
	return out
}

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
	// argRadioPower reads whether the radio is powered.
	argRadioPower = "/t/networksetup -getairportpower en0"
	// radioPowered and radioUnpowered are its answers.
	radioPowered   = "Wi-Fi Power (en0): On\n"
	radioUnpowered = "Wi-Fi Power (en0): Off\n"
	// argBluetooth reads Bluetooth's power for a failover attempt's survey.
	argBluetooth = "/t/system_profiler SPBluetoothDataType -json"
	// bluetoothOn and bluetoothOff are its answers.
	bluetoothOn  = `{"SPBluetoothDataType":[{"controller_properties":{"controller_state":"attrib_on"}}]}`
	bluetoothOff = `{"SPBluetoothDataType":[{"controller_properties":{"controller_state":"attrib_off"}}]}`
	// argDisplaySleep puts the display to sleep for the quiet state.
	argDisplaySleep = "/t/pmset displaysleepnow"
	// argDisplayWake wakes it.
	argDisplayWake = "/t/caffeinate -u -t 1"
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

// argPowerMode is the energy step's `sudo -n pmset -a powermode <mode>` call.
func argPowerMode(mode string) string { return argPower("powermode", mode) }

// argLowPower is the quiet state's Low Power Mode write.
var argLowPower = argPowerMode(energyModeLowPower)

// scriptPower answers every privileged write of a change turning the mode to
// value -- each power setting, and the Low Power Mode a closed lid's quiet
// state sets -- with success.
func scriptPower(r *fakeRunner, value string) {
	for _, s := range powerSettings {
		r.on(argPower(s, value), answer{})
	}
	r.on(argLowPower, answer{})
}

// fakeEnergyStore is the recorded energy mode in memory, with an error for
// each operation. Each Save and Clear notes how many tool runs preceded it,
// so a test orders it against the runner's calls.
type fakeEnergyStore struct {
	mu                       sync.Mutex
	r                        *fakeRunner
	rec                      *EnergyRecord
	loadErr, saveErr, clrErr error
	saves, clears            int
	savedAfter, clearedAfter []int
}

func (f *fakeEnergyStore) Load() (EnergyRecord, bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.loadErr != nil {
		return EnergyRecord{}, false, f.loadErr
	}
	if f.rec == nil {
		return EnergyRecord{}, false, nil
	}
	return *f.rec, true, nil
}

func (f *fakeEnergyStore) Save(r EnergyRecord) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.savedAfter = append(f.savedAfter, len(f.r.ran()))
	if f.saveErr != nil {
		return f.saveErr
	}
	f.saves++
	f.rec = &r
	return nil
}

func (f *fakeEnergyStore) Clear() error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.clearedAfter = append(f.clearedAfter, len(f.r.ran()))
	if f.clrErr != nil {
		return f.clrErr
	}
	f.clears++
	f.rec = nil
	return nil
}

// kept answers the recorded mode, empty when none is kept.
func (f *fakeEnergyStore) kept() string {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.rec == nil {
		return ""
	}
	return f.rec.Mode
}

// fakeLidDims is an earlier build's kept lid dim: a level, and an error for
// each operation. Every clear is recorded.
type fakeLidDims struct {
	mu               sync.Mutex
	level            *float64
	getErr, clearErr error
	clears           int
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

// fakeFreezer records every freeze and thaw, in order, and answers each
// freeze with frozen.
type fakeFreezer struct {
	frozen []appfreeze.Frozen
	calls  []string
	// held is what Frozen answers: frozen from a freeze until a thaw.
	held     []appfreeze.Frozen
	onChange func()
}

func (f *fakeFreezer) Freeze(_ context.Context, trigger string) []appfreeze.Frozen {
	f.calls = append(f.calls, "freeze:"+trigger)
	f.held = f.frozen
	return f.frozen
}

func (f *fakeFreezer) Thaw(_ context.Context, trigger string) {
	f.calls = append(f.calls, "thaw:"+trigger)
	f.held = nil
}

func (f *fakeFreezer) Frozen() []appfreeze.Frozen { return f.held }

func (f *fakeFreezer) OnChange(fn func()) { f.onChange = fn }

// count answers how many recorded calls start with prefix ("freeze", "thaw").
func (f *fakeFreezer) count(prefix string) int {
	n := 0
	for _, c := range f.calls {
		if strings.HasPrefix(c, prefix+":") {
			n++
		}
	}
	return n
}

// fakeQuietTelemetry records every gauge setting.
type fakeQuietTelemetry struct{ set []bool }

func (f *fakeQuietTelemetry) SetLidQuiet(q bool) { f.set = append(f.set, q) }

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

// fakePowerSourceWatcher hands Watch's heard every power source sent on
// sources, then answers end once sources is closed, or nil when ctx ends
// first.
type fakePowerSourceWatcher struct {
	sources chan PowerSource
	end     error
}

func newFakePowerSourceWatcher() *fakePowerSourceWatcher {
	return &fakePowerSourceWatcher{sources: make(chan PowerSource)}
}

func (f *fakePowerSourceWatcher) Watch(ctx context.Context, heard func(PowerSource)) error {
	for {
		select {
		case <-ctx.Done():
			return nil
		case p, ok := <-f.sources:
			if !ok {
				return f.end
			}
			heard(p)
		}
	}
}

// fakeNetwork is a scripted network change signal (netwatch.Watcher.Changed).
// taken is told each time the signal is taken, so a test knows Run holds the
// channel a fire closes.
type fakeNetwork struct {
	mu    sync.Mutex
	ch    chan struct{}
	taken chan struct{}
}

func newFakeNetwork() *fakeNetwork {
	return &fakeNetwork{ch: make(chan struct{}), taken: make(chan struct{}, 16)}
}

func (n *fakeNetwork) changed() <-chan struct{} {
	n.mu.Lock()
	defer n.mu.Unlock()
	n.taken <- struct{}{}
	return n.ch
}

// fire reports one network change.
func (n *fakeNetwork) fire() {
	n.mu.Lock()
	defer n.mu.Unlock()
	close(n.ch)
	n.ch = make(chan struct{})
}

// fakePower is a scripted power-settings change source: a test sends on
// changes, and watching is closed once the source is taken.
type fakePower struct {
	changes  chan error
	watching chan struct{}
	once     sync.Once
}

func newFakePower() *fakePower {
	return &fakePower{changes: make(chan error), watching: make(chan struct{})}
}

func (p *fakePower) source(context.Context) (<-chan error, func()) {
	p.once.Do(func() { close(p.watching) })
	return p.changes, func() {}
}
