package lockwatch

import (
	"context"
	"errors"
	"strconv"
	"sync"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// t0 is the instant every test's first tick lands at.
var t0 = time.Date(2026, 9, 27, 14, 0, 44, 0, time.UTC)

// at is the instant d after t0.
func at(d time.Duration) time.Time { return t0.Add(d) }

// fixture is one watchdog with a counting dump and a capturing run log.
type fixture struct {
	w     *Watchdog
	run   *dlog.TestLogger
	dumps int
}

func newFixture(t *testing.T) *fixture {
	t.Helper()
	f := &fixture{run: dlog.NewTestLogger()}
	w, err := New(Deps{
		Log:       f.run,
		Threshold: 6 * time.Second,
		Every:     time.Second,
		Dump: func() (string, int) {
			f.dumps++
			return "goroutine 1 [running]:\nthe dump " + strconv.Itoa(f.dumps), 1
		},
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.w = w
	return f
}

// ticks checks once at each offset from t0.
func (f *fixture) ticks(offsets ...time.Duration) {
	for _, d := range offsets {
		f.w.check(at(d))
	}
}

// of is every record at one level and operation.
func of(log *dlog.TestLogger, level, operation string) []dlog.Record {
	var out []dlog.Record
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			out = append(out, r)
		}
	}
	return out
}

// seconds is n whole-second offsets from start.
func seconds(start, n int) []time.Duration {
	out := make([]time.Duration, 0, n)
	for i := range n {
		out = append(out, time.Duration(start+i)*time.Second)
	}
	return out
}

func TestNewRefuses(t *testing.T) {
	tests := []struct {
		name string
		deps Deps
	}{
		{name: "a missing run log", deps: Deps{}},
		{name: "a negative threshold", deps: Deps{Log: dlog.NewTestLogger(), Threshold: -time.Second}},
		{name: "a negative tick", deps: Deps{Log: dlog.NewTestLogger(), Every: -time.Second}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the table row.

			// Act.
			_, err := New(tt.deps)

			// Assert.
			if err == nil {
				t.Fatal("New accepted the deps")
			}
		})
	}
}

// TestAStallPastTheThresholdIsRecordedOnce pins ONE ERROR per episode however
// many ticks the stall outlives the threshold by.
func TestAStallPastTheThresholdIsRecordedOnce(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
	m.Lock()

	// Act.
	f.ticks(seconds(0, 20)...)

	// Assert.
	if got := of(log, dlog.LevelError, opStall); len(got) != 1 {
		t.Fatalf("stall records = %+v, want exactly one", got)
	}
	if f.dumps != 1 {
		t.Fatalf("dumps = %d, want 1", f.dumps)
	}
}

// TestAStallRecordCarriesItsContext pins the evidence on the one ERROR.
func TestAStallRecordCarriesItsContext(t *testing.T) {
	tests := []struct {
		key  string
		want any
	}{
		{key: "lock", want: "sessionwatcher.watcher"},
		{key: "workspace_id", want: "ws1"},
		{key: "held_for", want: "6s"},
		{key: "held_for_ms", want: int64(6000)},
		{key: "threshold", want: "6s"},
		{key: "resolution", want: "1s"},
		{key: "goroutines", want: 1},
		{key: "goroutine_dump", want: "goroutine 1 [running]:\nthe dump 1"},
		{key: "dump_id", want: uint64(1)},
		{key: "stalled_this_tick", want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.key, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			log := dlog.NewTestLogger()
			var m Mutex
			f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
			m.Lock()

			// Act.
			f.ticks(seconds(0, 7)...)

			// Assert.
			stalls := of(log, dlog.LevelError, opStall)
			if len(stalls) != 1 {
				t.Fatalf("stall records = %+v, want exactly one", stalls)
			}
			if got := stalls[0].Context[tt.key]; got != tt.want {
				t.Fatalf("context[%q] = %#v, want %#v", tt.key, got, tt.want)
			}
		})
	}
}

// TestADaemonWideLockStallNamesNoWorkspace pins that a lock no workspace owns
// is not stamped with an empty workspace id.
func TestADaemonWideLockStallNamesNoWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "promptqueue.queue", "", log)
	m.Lock()

	// Act.
	f.ticks(seconds(0, 7)...)

	// Assert.
	stalls := of(log, dlog.LevelError, opStall)
	if len(stalls) != 1 {
		t.Fatalf("stall records = %+v, want exactly one", stalls)
	}
	if _, ok := stalls[0].Context["workspace_id"]; ok {
		t.Fatalf("context = %+v, want no workspace_id", stalls[0].Context)
	}
}

// TestAHoldIsTimedFromTheTickThatFirstSawIt pins the detection bound: a hold
// first seen at a tick is a stall exactly one threshold later, not before.
func TestAHoldIsTimedFromTheTickThatFirstSawIt(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "promptqueue.drain", "ws1", log)
	f.ticks(0)
	m.Lock()

	// Act.
	f.ticks(seconds(1, 6)...) // first seen at 1s; 6s is only 5s later

	// Assert.
	if got := of(log, dlog.LevelError, opStall); len(got) != 0 {
		t.Fatalf("stall records = %+v, want none before the threshold has passed", got)
	}
}

// TestAReleaseAfterAStallIsRecordedAtInfo pins the episode's ending.
func TestAReleaseAfterAStallIsRecordedAtInfo(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
	m.Lock()
	f.ticks(seconds(0, 10)...)

	// Act.
	m.Unlock()
	f.ticks(10 * time.Second)

	// Assert.
	releases := of(log, dlog.LevelInfo, opRelease)
	if len(releases) != 1 {
		t.Fatalf("release records = %+v, want exactly one", releases)
	}
	if got := releases[0].Context["held_for_ms"]; got != int64(10000) {
		t.Fatalf("held_for_ms = %#v, want 10000", got)
	}
}

// TestAReleaseIsRecordedOnce pins that the ending is not repeated on the
// ticks after it.
func TestAReleaseIsRecordedOnce(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
	m.Lock()
	f.ticks(seconds(0, 10)...)
	m.Unlock()

	// Act.
	f.ticks(seconds(10, 5)...)

	// Assert.
	if got := of(log, dlog.LevelInfo, opRelease); len(got) != 1 {
		t.Fatalf("release records = %+v, want exactly one", got)
	}
}

// TestAReHeldLockIsANewEpisode pins that a lock released after its stall and
// held past the threshold again is reported again, with a dump of its own.
func TestAReHeldLockIsANewEpisode(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "promptqueue.drain", "ws1", log)
	m.Lock()
	f.ticks(seconds(0, 8)...)
	m.Unlock()
	m.Lock()

	// Act.
	f.ticks(seconds(8, 8)...)

	// Assert.
	if got := of(log, dlog.LevelError, opStall); len(got) != 2 {
		t.Fatalf("stall records = %+v, want two episodes", got)
	}
	if f.dumps != 2 {
		t.Fatalf("dumps = %d, want one per episode", f.dumps)
	}
}

// TestAHoldReTakenBetweenTicksIsNotOneHold pins that a busy lock taken and
// released many times is never mistaken for one long hold, even when every
// tick happens to land while it is held.
func TestAHoldReTakenBetweenTicksIsNotOneHold(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "feed.resolver", "", log)

	// Act.
	for _, d := range seconds(0, 30) {
		m.Lock()
		f.ticks(d)
		m.Unlock()
	}

	// Assert.
	if got := log.Records(); len(of(log, dlog.LevelError, opStall))+len(of(log, dlog.LevelInfo, opRelease)) != 0 {
		t.Fatalf("records = %+v, want no stall and no release", got)
	}
}

// TestShortHoldsRecordNothing pins silence for everything under the threshold.
func TestShortHoldsRecordNothing(t *testing.T) {
	tests := []struct {
		name string
		hold func(m *Mutex, f *fixture)
	}{
		{name: "a lock never taken", hold: func(_ *Mutex, f *fixture) { f.ticks(seconds(0, 30)...) }},
		{name: "a hold one tick under the threshold", hold: func(m *Mutex, f *fixture) {
			m.Lock()
			f.ticks(seconds(0, 6)...)
			m.Unlock()
			f.ticks(seconds(6, 10)...)
		}},
		{name: "a hold released between ticks", hold: func(m *Mutex, f *fixture) {
			f.ticks(0)
			m.Lock()
			m.Unlock()
			f.ticks(seconds(1, 10)...)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			log := dlog.NewTestLogger()
			var m Mutex
			f.w.Watch(&m, "promptqueue.drain", "ws1", log)

			// Act.
			tt.hold(&m, f)

			// Assert.
			for _, r := range log.Records() {
				if r.Operation != opWatch {
					t.Fatalf("record %+v, want nothing but the registration", r)
				}
			}
			if f.dumps != 0 {
				t.Fatalf("dumps = %d, want none", f.dumps)
			}
		})
	}
}

// TestSimultaneousStallsShareOneDump pins the global cap: fifty workspaces
// wedged in the same tick are fifty ERRORs and ONE goroutine dump.
func TestSimultaneousStallsShareOneDump(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	locks := make([]Mutex, 50)
	for i := range locks {
		f.w.Watch(&locks[i], "sessionwatcher.watcher", ids.WorkspaceID("ws"+strconv.Itoa(i)), log)
		locks[i].Lock()
	}

	// Act.
	f.ticks(seconds(0, 7)...)

	// Assert.
	stalls := of(log, dlog.LevelError, opStall)
	if len(stalls) != 50 {
		t.Fatalf("stall records = %d, want 50", len(stalls))
	}
	if f.dumps != 1 {
		t.Fatalf("dumps = %d, want 1 shared by the tick", f.dumps)
	}
}

// TestAStallSharingADumpNamesItsCarrier pins that a stall without the dump
// names the one record that has it.
func TestAStallSharingADumpNamesItsCarrier(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var a, b Mutex
	f.w.Watch(&a, "sessionwatcher.watcher", "wsA", log)
	f.w.Watch(&b, "sessionwatcher.watcher", "wsB", log)
	a.Lock()
	b.Lock()

	// Act.
	f.ticks(seconds(0, 7)...)

	// Assert.
	stalls := of(log, dlog.LevelError, opStall)
	if len(stalls) != 2 {
		t.Fatalf("stall records = %+v, want two", stalls)
	}
	second := stalls[1].Context
	if _, ok := second["goroutine_dump"]; ok {
		t.Fatalf("the second stall carries a dump of its own: %+v", second)
	}
	if second["goroutine_dump_carried_by"] != "sessionwatcher.watcher wsA" || second["dump_id"] != stalls[0].Context["dump_id"] {
		t.Fatalf("second stall = %+v, want it to name wsA's record and share its dump id", second)
	}
}

// TestStallsInDifferentTicksTakeTheirOwnDumps pins that the cap is per tick:
// a stall that begins later is dumped when it is detected.
func TestStallsInDifferentTicksTakeTheirOwnDumps(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var a, b Mutex
	f.w.Watch(&a, "sessionwatcher.watcher", "wsA", log)
	f.w.Watch(&b, "sessionwatcher.watcher", "wsB", log)
	a.Lock()
	f.ticks(seconds(0, 3)...)
	b.Lock()

	// Act.
	f.ticks(seconds(3, 10)...)

	// Assert.
	if f.dumps != 2 {
		t.Fatalf("dumps = %d, want one per tick that found a stall", f.dumps)
	}
}

// TestAnUnwatchedLockIsNoLongerChecked pins unregistration on close.
func TestAnUnwatchedLockIsNoLongerChecked(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	unwatch := f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
	unwatch()
	m.Lock()

	// Act.
	f.ticks(seconds(0, 20)...)

	// Assert.
	if got := of(log, dlog.LevelError, opStall); len(got) != 0 {
		t.Fatalf("stall records = %+v, want none for an unwatched lock", got)
	}
	if n := len(f.w.entries); n != 0 {
		t.Fatalf("entries = %d, want the unwatched one dropped", n)
	}
}

// TestUnwatchingAStalledLockRecordsTheEnd pins that a stall whose owner is
// retired still gets its INFO ending rather than trailing off.
func TestUnwatchingAStalledLockRecordsTheEnd(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	unwatch := f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
	m.Lock()
	f.ticks(seconds(0, 8)...)
	m.Unlock()
	unwatch()

	// Act.
	f.ticks(8 * time.Second)

	// Assert.
	releases := of(log, dlog.LevelInfo, opRelease)
	if len(releases) != 1 || releases[0].Context["held_for_ms"] != int64(8000) {
		t.Fatalf("release records = %+v, want one at 8000ms", releases)
	}
}

// TestUnwatchIsIdempotent pins that a second unwatch changes nothing.
func TestUnwatchIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	var m Mutex
	unwatch := f.w.Watch(&m, "sessionwatcher.watcher", "ws1", dlog.NewTestLogger())
	unwatch()

	// Act.
	unwatch()
	f.ticks(0)

	// Assert.
	if n := len(f.w.entries); n != 0 {
		t.Fatalf("entries = %d, want 0", n)
	}
}

// TestRegistrationRacesTheTick pins, under -race, that workspaces opening and
// closing while the watchdog walks is safe.
func TestRegistrationRacesTheTick(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	var wg sync.WaitGroup
	start := make(chan struct{})

	// Act.
	for i := range 8 {
		wg.Add(1)
		go func() {
			defer wg.Done()
			<-start
			for range 100 {
				var m Mutex
				unwatch := f.w.Watch(&m, "sessionwatcher.watcher", ids.WorkspaceID("ws"+strconv.Itoa(i)), dlog.NewTestLogger())
				m.Lock()
				m.Unlock()
				unwatch()
			}
		}()
	}
	close(start)
	for _, d := range seconds(0, 100) {
		f.w.check(at(d))
	}
	wg.Wait()
	f.w.check(at(100 * time.Second))

	// Assert.
	if n := len(f.w.entries); n != 0 {
		t.Fatalf("entries = %d, want every unwatched lock dropped", n)
	}
}

// runBound is how long a test waits for Run to return. Run returns within one
// select of its context ending; this is a failure bound, never a delay.
const runBound = time.Second

// runWatchdog starts Run over a tick channel and returns the channel, the
// cancel and Run's result.
func runWatchdog(t *testing.T, f *fixture) (chan time.Time, context.CancelFunc, <-chan error) {
	t.Helper()
	ticks := make(chan time.Time)
	f.w.ticks = ticks
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- f.w.Run(ctx) }()
	return ticks, cancel, done
}

// awaitRun waits, bounded, for Run to return.
func awaitRun(t *testing.T, done <-chan error) error {
	t.Helper()
	select {
	case err := <-done:
		return err
	case <-time.After(runBound):
		t.Fatalf("Run did not return within %v", runBound)
		return nil
	}
}

// TestRunStopsWithTheServingLifetime pins that the watchdog's one goroutine
// leaves when the daemon's context ends.
func TestRunStopsWithTheServingLifetime(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	_, cancel, done := runWatchdog(t, f)

	// Act.
	cancel()

	// Assert.
	if err := awaitRun(t, done); err != nil {
		t.Fatalf("Run = %v, want nil on the serving lifetime's end", err)
	}
}

// TestRunChecksOnEveryTick pins that the ticks Run receives drive the checks.
func TestRunChecksOnEveryTick(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	log := dlog.NewTestLogger()
	var m Mutex
	f.w.Watch(&m, "sessionwatcher.watcher", "ws1", log)
	m.Lock()
	ticks, cancel, done := runWatchdog(t, f)

	// Act.
	for _, d := range seconds(0, 7) {
		ticks <- at(d)
	}
	cancel()
	if err := awaitRun(t, done); err != nil {
		t.Fatalf("Run = %v", err)
	}

	// Assert.
	if got := of(log, dlog.LevelError, opStall); len(got) != 1 {
		t.Fatalf("stall records = %+v, want exactly one", got)
	}
}

// TestRunReportsAClosedTickSource pins that losing the ticks is loud: the
// daemon would otherwise go on with no stall detection at all.
func TestRunReportsAClosedTickSource(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ticks, cancel, done := runWatchdog(t, f)
	defer cancel()

	// Act.
	close(ticks)
	err := awaitRun(t, done)

	// Assert.
	if !errors.Is(err, errTicksClosed) {
		t.Fatalf("Run = %v, want errTicksClosed", err)
	}
	if got := of(f.run, dlog.LevelError, opRun); len(got) != 1 {
		t.Fatalf("run records = %+v, want one ERROR", got)
	}
}

// TestRunUsesItsOwnTickerWhenNoneIsInjected pins the production default: Run
// builds its own ticker and still stops with the serving lifetime.
func TestRunUsesItsOwnTickerWhenNoneIsInjected(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	ctx, cancel := context.WithCancel(context.Background())
	done := make(chan error, 1)
	go func() { done <- f.w.Run(ctx) }()

	// Act.
	cancel()

	// Assert.
	if err := awaitRun(t, done); err != nil {
		t.Fatalf("Run = %v, want nil", err)
	}
}

// BenchmarkCheck measures one tick's cost over fifty watched locks: the steady
// state the daemon pays once a second.
func BenchmarkCheck(b *testing.B) {
	for _, held := range []bool{false, true} {
		b.Run("50 locks held="+strconv.FormatBool(held), func(b *testing.B) {
			w, err := New(Deps{Log: dlog.NewTestLogger(), Threshold: time.Duration(1 << 62)})
			if err != nil {
				b.Fatal(err)
			}
			log := dlog.NewTestLogger()
			locks := make([]Mutex, 50)
			for i := range locks {
				w.Watch(&locks[i], "sessionwatcher.watcher", ids.WorkspaceID("ws"+strconv.Itoa(i)), log)
				if held {
					locks[i].Lock()
				}
			}
			now := t0
			b.ReportAllocs()
			b.ResetTimer()
			for range b.N {
				now = now.Add(time.Second)
				w.check(now)
			}
		})
	}
}
