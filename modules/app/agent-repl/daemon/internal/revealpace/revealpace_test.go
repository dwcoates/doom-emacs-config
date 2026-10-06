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
