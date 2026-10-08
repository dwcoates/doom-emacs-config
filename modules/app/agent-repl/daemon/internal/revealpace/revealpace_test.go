package revealpace

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// fakeStore is the durable record, in memory, with injectable failures.
type fakeStore struct {
	windows []wsm.RevealGapWindow
	puts    []wsm.RevealGapWindow
	readErr error
	putErr  error
}

func (f *fakeStore) PutRevealGapWindow(_ context.Context, w wsm.RevealGapWindow) error {
	if f.putErr != nil {
		return f.putErr
	}
	f.puts = append(f.puts, w)
	return nil
}

func (f *fakeStore) RevealGapWindows(context.Context) ([]wsm.RevealGapWindow, error) {
	return f.windows, f.readErr
}

var prose = Key{Model: "claude-opus-5", Kind: wsm.RevealKindProse}

// constant answers n gaps of ms each.
func constant(n int, ms int64) []int64 {
	out := make([]int64, n)
	for i := range out {
		out[i] = ms
	}
	return out
}

func load(t *testing.T, store *fakeStore) (*Pacer, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	p, err := Load(context.Background(), store, log)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	return p, log
}

func hasRecord(log *dlog.TestLogger, level, operation string) (dlog.Record, bool) {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			return r, true
		}
	}
	return dlog.Record{}, false
}

func TestExpectedGapIsAbsentUntilTheWindowIsFull(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	for range WindowSize - 1 {
		p.Observe(prose, 40*time.Millisecond)
	}

	// Act
	_, ok := p.ExpectedGap(prose)

	// Assert
	if ok {
		t.Fatalf("ExpectedGap answered with %d gaps, want none until %d", WindowSize-1, WindowSize)
	}
}

func TestExpectedGapOfAConstantWindowIsThatGap(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	for range WindowSize {
		p.Observe(prose, 40*time.Millisecond)
	}

	// Act
	got, ok := p.ExpectedGap(prose)

	// Assert
	if !ok || got != 40 {
		t.Fatalf("ExpectedGap() = %d, %v, want 40, true", got, ok)
	}
}

func TestExpectedGapWeighsTheNewestGapMost(t *testing.T) {
	// Arrange: a long run of 10ms gaps, then one 1000ms gap.
	p, _ := load(t, &fakeStore{})
	for range WindowSize - 1 {
		p.Observe(prose, 10*time.Millisecond)
	}
	p.Observe(prose, time.Second)
	unweighted := (int64(WindowSize-1)*10 + 1000) / WindowSize

	// Act
	got, _ := p.ExpectedGap(prose)

	// Assert
	if int64(got) <= unweighted {
		t.Fatalf("ExpectedGap() = %d, want above the plain average %d", got, unweighted)
	}
}

func TestExpectedGapIsNeverBelowOneMillisecond(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	for range WindowSize {
		p.Observe(prose, 0)
	}

	// Act
	got, _ := p.ExpectedGap(prose)

	// Assert
	if got != 1 {
		t.Fatalf("ExpectedGap() = %d, want 1", got)
	}
}

func TestObserveDropsTheOldestGapOnceFull(t *testing.T) {
	// Arrange: a full window of 1000ms gaps, then a full window of 10ms ones.
	p, _ := load(t, &fakeStore{})
	for range WindowSize {
		p.Observe(prose, time.Second)
	}

	// Act
	for range WindowSize {
		p.Observe(prose, 10*time.Millisecond)
	}

	// Assert
	if got, _ := p.ExpectedGap(prose); got != 10 {
		t.Fatalf("ExpectedGap() = %d, want 10 once every old gap rolled out", got)
	}
}

func TestWindowsAreKeptPerModelAndKind(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	thinking := Key{Model: prose.Model, Kind: wsm.RevealKindThinking}
	other := Key{Model: "claude-sonnet-5", Kind: wsm.RevealKindProse}
	for range WindowSize {
		p.Observe(prose, 40*time.Millisecond)
	}

	// Act
	_, thinkingOK := p.ExpectedGap(thinking)
	_, otherOK := p.ExpectedGap(other)

	// Assert
	if thinkingOK || otherOK {
		t.Fatalf("another key answered from this key's window (thinking %v, other model %v)", thinkingOK, otherOK)
	}
}

func TestLoadResumesTheStoredWindows(t *testing.T) {
	// Arrange
	store := &fakeStore{windows: []wsm.RevealGapWindow{{Model: prose.Model, Kind: prose.Kind, GapsMs: constant(WindowSize, 55)}}}

	// Act
	p, _ := load(t, store)

	// Assert
	if got, ok := p.ExpectedGap(prose); !ok || got != 55 {
		t.Fatalf("ExpectedGap() = %d, %v, want 55, true", got, ok)
	}
}

func TestLoadKeepsOnlyTheNewestGapsOfAnOversizedWindow(t *testing.T) {
	// Arrange: a stored window longer than WindowSize, its oldest gaps 1000ms.
	gaps := append(constant(5, 1000), constant(WindowSize, 20)...)
	store := &fakeStore{windows: []wsm.RevealGapWindow{{Model: prose.Model, Kind: prose.Kind, GapsMs: gaps}}}
	p, _ := load(t, store)

	// Act
	err := p.Persist(context.Background(), prose)

	// Assert
	if err != nil {
		t.Fatalf("Persist: %v", err)
	}
	if !reflect.DeepEqual(store.puts[0].GapsMs, constant(WindowSize, 20)) {
		t.Fatalf("persisted %v, want the newest %d gaps", store.puts[0].GapsMs, WindowSize)
	}
}

func TestLoadFailsWhenTheStoreCannotBeRead(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	cause := errors.New("disk gone")

	// Act
	p, err := Load(context.Background(), &fakeStore{readErr: cause}, log)

	// Assert
	if !errors.Is(err, cause) || p != nil {
		t.Fatalf("Load() = %v, %v, want nil and the store's error", p, err)
	}
	record, ok := hasRecord(log, "error", "daemon.revealpace.load")
	if !ok || record.Context["error"] != cause.Error() {
		t.Fatalf("no ERROR daemon.revealpace.load naming the cause: %v", log.Records())
	}
}

func TestPersistWritesTheKeysWindowOldestFirst(t *testing.T) {
	// Arrange
	store := &fakeStore{}
	p, _ := load(t, store)
	p.Observe(prose, 10*time.Millisecond)
	p.Observe(prose, 20*time.Millisecond)

	// Act
	err := p.Persist(context.Background(), prose)

	// Assert
	if err != nil {
		t.Fatalf("Persist: %v", err)
	}
	want := []wsm.RevealGapWindow{{Model: prose.Model, Kind: prose.Kind, GapsMs: []int64{10, 20}}}
	if !reflect.DeepEqual(store.puts, want) {
		t.Fatalf("puts = %+v, want %+v", store.puts, want)
	}
}

func TestPersistSurfacesAndLogsAStoreFailure(t *testing.T) {
	// Arrange
	cause := errors.New("disk full")
	store := &fakeStore{}
	p, log := load(t, store)
	p.Observe(prose, 10*time.Millisecond)
	store.putErr = cause

	// Act
	err := p.Persist(context.Background(), prose)

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("Persist() error = %v, want the store's error", err)
	}
	record, ok := hasRecord(log, "error", "daemon.revealpace.persist")
	if !ok || record.Context["model"] != prose.Model || record.Context["kind"] != string(prose.Kind) || record.Context["error"] != cause.Error() {
		t.Fatalf("no ERROR daemon.revealpace.persist naming the key and cause: %v", log.Records())
	}
}

// at answers the instant ms milliseconds after an arbitrary origin.
func at(ms int64) time.Time {
	return time.Date(2026, 10, 8, 15, 31, 25, 0, time.UTC).Add(time.Duration(ms) * time.Millisecond)
}

func TestBurstArrive(t *testing.T) {
	cases := []struct {
		name      string
		earlier   []int64
		arriveMs  int64
		wantGapMs int64
		wantOK    bool
	}{
		{name: "a block's first fragment only starts the clock", earlier: nil, arriveMs: 5_000},
		{name: "a fragment within the threshold joins the burst", earlier: []int64{1_000}, arriveMs: 1_003},
		{name: "a fragment exactly at the threshold joins the burst", earlier: []int64{1_000}, arriveMs: 1_020},
		{name: "a fragment past the threshold starts a burst and samples from the last burst's start", earlier: []int64{1_000, 1_003, 1_010}, arriveMs: 1_530, wantGapMs: 530, wantOK: true},
		{name: "a burst longer than the threshold stays one burst while each step is within it", earlier: []int64{1_000, 1_015, 1_030, 1_045}, arriveMs: 1_060},
		{name: "a long chained burst's gap is measured from its start", earlier: []int64{1_000, 1_015, 1_030, 1_045}, arriveMs: 1_500, wantGapMs: 500, wantOK: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var b Burst
			for _, ms := range tc.earlier {
				b.Arrive(at(ms))
			}

			// Act
			gap, ok := b.Arrive(at(tc.arriveMs))

			// Assert
			if ok != tc.wantOK || gap.Milliseconds() != tc.wantGapMs {
				t.Fatalf("Arrive() = %v, %v, want %dms, %v", gap, ok, tc.wantGapMs, tc.wantOK)
			}
		})
	}
}

// every answers the instants from through to, step milliseconds apart.
func every(from, to, step int64) []int64 {
	var out []int64
	for ms := from; ms <= to; ms += step {
		out = append(out, ms)
	}
	return out
}

func TestBurstWindow(t *testing.T) {
	const expected = 500
	cases := []struct {
		name    string
		arrived []int64
		drawMs  int64
		want    uint32
	}{
		{name: "a block no fragment reached gets the full window", arrived: nil, drawMs: 1_000, want: expected},
		{name: "a burst's first fragment gets the full window", arrived: []int64{1_000}, drawMs: 1_000, want: expected},
		{name: "a later fragment of the burst gets what is left of the burst's window", arrived: []int64{1_000, 1_003, 1_010}, drawMs: 1_010, want: expected - 10},
		{name: "a draw soon after the burst keeps the burst's deadline", arrived: []int64{1_000, 1_010}, drawMs: 1_030, want: expected - 30},
		{name: "a draw after a pause past the threshold gets the full window", arrived: []int64{1_000, 1_010}, drawMs: 1_031, want: expected},
		{name: "a burst outlasting its window gets the floor", arrived: every(1_000, 1_600, 20), drawMs: 1_600, want: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var b Burst
			for _, ms := range tc.arrived {
				b.Arrive(at(ms))
			}

			// Act
			got := b.Window(expected, at(tc.drawMs))

			// Assert
			if got != tc.want {
				t.Fatalf("Window() = %d, want %d", got, tc.want)
			}
		})
	}
}
