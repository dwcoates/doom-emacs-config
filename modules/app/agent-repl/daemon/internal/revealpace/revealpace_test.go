package revealpace

import (
	"context"
	"errors"
	"math"
	"reflect"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// fakeStore is the durable record, in memory, with injectable failures.
type fakeStore struct {
	windows []wsm.RevealSpeedWindow
	puts    []wsm.RevealSpeedWindow
	readErr error
	putErr  error
}

func (f *fakeStore) PutRevealSpeedWindow(_ context.Context, w wsm.RevealSpeedWindow) error {
	if f.putErr != nil {
		return f.putErr
	}
	f.puts = append(f.puts, w)
	return nil
}

func (f *fakeStore) RevealSpeedWindows(context.Context) ([]wsm.RevealSpeedWindow, error) {
	return f.windows, f.readErr
}

var (
	prose    = Key{Model: "claude-opus-5", Kind: wsm.RevealKindProse}
	thinking = Key{Model: "claude-opus-5", Kind: wsm.RevealKindThinking}
)

// ramp answers n speeds 1, 2, ..., n, so the p-th percentile of a window of
// them is the value at index n*p/100, plus one.
func ramp(n int) []float64 {
	out := make([]float64, n)
	for i := range out {
		out[i] = float64(i + 1)
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

func observeAll(p *Pacer, key Key, speeds []float64) {
	for _, s := range speeds {
		p.Observe(key, s)
	}
}

func hasRecord(log *dlog.TestLogger, level, operation string) (dlog.Record, bool) {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			return r, true
		}
	}
	return dlog.Record{}, false
}

func TestDefaultBoundsPerKind(t *testing.T) {
	cases := []struct {
		name string
		kind wsm.RevealKind
		want Bounds
	}{
		{name: "thinking", kind: wsm.RevealKindThinking, want: Bounds{MinCharsPerSecond: 45, MaxCharsPerSecond: 55}},
		{name: "prose", kind: wsm.RevealKindProse, want: Bounds{MinCharsPerSecond: 49, MaxCharsPerSecond: 90}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := DefaultBounds(tc.kind)

			// Assert
			if got != tc.want {
				t.Fatalf("DefaultBounds(%s) = %+v, want %+v", tc.kind, got, tc.want)
			}
		})
	}
}

func TestDefaultBoundsPanicsOnAnUnknownKind(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("DefaultBounds accepted an unknown kind")
		}
	}()

	// Act
	DefaultBounds("tool")
}

func TestBoundsIsTheKindsDefaultUntilEnoughBlocks(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	observeAll(p, prose, ramp(MinBlocks-1))

	// Act
	got := p.Bounds(prose)

	// Assert
	if want := DefaultBounds(wsm.RevealKindProse); got != want {
		t.Fatalf("Bounds with %d blocks = %+v, want the default %+v", MinBlocks-1, got, want)
	}
}

func TestBoundsAreTheWindowsPercentilesOnceEnoughBlocks(t *testing.T) {
	// Arrange: 1..40 shuffled into arrival order, so the percentiles come
	// from the sorted speeds and not from the order they arrived in.
	p, _ := load(t, &fakeStore{})
	speeds := ramp(40)
	for i, j := 0, len(speeds)-1; i < j; i, j = i+1, j-1 {
		speeds[i], speeds[j] = speeds[j], speeds[i]
	}
	observeAll(p, prose, speeds)

	// Act
	got := p.Bounds(prose)

	// Assert: index 40*25/100 = 10 holds 11, index 40*38/100 = 15 holds 16.
	if want := (Bounds{MinCharsPerSecond: 11, MaxCharsPerSecond: 16}); got != want {
		t.Fatalf("Bounds = %+v, want %+v", got, want)
	}
}

func TestBoundsRoundToWholeCharsPerSecond(t *testing.T) {
	// Arrange: every block arrived at 44.6 chars/sec.
	p, _ := load(t, &fakeStore{})
	for range MinBlocks {
		p.Observe(prose, 44.6)
	}

	// Act
	got := p.Bounds(prose)

	// Assert
	if want := (Bounds{MinCharsPerSecond: 45, MaxCharsPerSecond: 45}); got != want {
		t.Fatalf("Bounds = %+v, want %+v", got, want)
	}
}

func TestBoundsAreNeverBelowOneCharPerSecond(t *testing.T) {
	// Arrange: every block arrived at a crawl.
	p, _ := load(t, &fakeStore{})
	for range MinBlocks {
		p.Observe(prose, 0.2)
	}

	// Act
	got := p.Bounds(prose)

	// Assert
	if want := (Bounds{MinCharsPerSecond: 1, MaxCharsPerSecond: 1}); got != want {
		t.Fatalf("Bounds = %+v, want %+v", got, want)
	}
}

func TestObserveDropsTheOldestSpeedOnceFull(t *testing.T) {
	// Arrange: a full window of slow blocks, then a full window of fast ones.
	p, _ := load(t, &fakeStore{})
	for range WindowSize {
		p.Observe(prose, 10)
	}
	for range WindowSize {
		p.Observe(prose, 100)
	}

	// Act
	got := p.Bounds(prose)

	// Assert
	if want := (Bounds{MinCharsPerSecond: 100, MaxCharsPerSecond: 100}); got != want {
		t.Fatalf("Bounds = %+v, want only the newest window %+v", got, want)
	}
}

func TestObservePanicsOnASpeedThatIsNotPositiveAndFinite(t *testing.T) {
	cases := []struct {
		name  string
		speed float64
	}{
		{name: "zero", speed: 0},
		{name: "negative", speed: -3},
		{name: "infinite", speed: math.Inf(1)},
		{name: "NaN", speed: math.NaN()},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			p, _ := load(t, &fakeStore{})

			// Assert
			defer func() {
				if recover() == nil {
					t.Fatalf("Observe accepted %v", tc.speed)
				}
				if got := p.Bounds(prose); got != DefaultBounds(wsm.RevealKindProse) {
					t.Fatalf("a refused speed changed the window: Bounds = %+v", got)
				}
			}()

			// Act
			p.Observe(prose, tc.speed)
		})
	}
}

func TestWindowsAreKeptPerModelAndKind(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	for range MinBlocks {
		p.Observe(prose, 130)
	}

	// Act
	gotThinking := p.Bounds(thinking)
	gotOtherModel := p.Bounds(Key{Model: "claude-haiku-4-5", Kind: wsm.RevealKindProse})

	// Assert
	if want := DefaultBounds(wsm.RevealKindThinking); gotThinking != want {
		t.Fatalf("thinking Bounds = %+v, want its own default %+v", gotThinking, want)
	}
	if want := DefaultBounds(wsm.RevealKindProse); gotOtherModel != want {
		t.Fatalf("another model's Bounds = %+v, want its own default %+v", gotOtherModel, want)
	}
}

func TestKindBoundsPoolsEveryModelOfTheKind(t *testing.T) {
	// Arrange: two models, neither with enough blocks alone, enough together,
	// and a thinking window that must stay out of the prose pool.
	p, _ := load(t, &fakeStore{})
	for range MinBlocks / 2 {
		p.Observe(prose, 60)
		p.Observe(Key{Model: "claude-haiku-4-5", Kind: wsm.RevealKindProse}, 60)
	}
	for range MinBlocks {
		p.Observe(thinking, 7)
	}

	// Act
	got := p.KindBounds(wsm.RevealKindProse)

	// Assert
	if want := (Bounds{MinCharsPerSecond: 60, MaxCharsPerSecond: 60}); got != want {
		t.Fatalf("KindBounds = %+v, want the pooled prose %+v", got, want)
	}
}

func TestKindBoundsIsTheKindsDefaultUntilEnoughBlocks(t *testing.T) {
	// Arrange
	p, _ := load(t, &fakeStore{})
	observeAll(p, prose, ramp(MinBlocks-1))

	// Act
	got := p.KindBounds(wsm.RevealKindProse)

	// Assert
	if want := DefaultBounds(wsm.RevealKindProse); got != want {
		t.Fatalf("KindBounds = %+v, want the default %+v", got, want)
	}
}

func TestLoadResumesTheStoredWindows(t *testing.T) {
	// Arrange
	store := &fakeStore{windows: []wsm.RevealSpeedWindow{{Model: prose.Model, Kind: prose.Kind, CharsPerSecond: ramp(40)}}}

	// Act
	p, log := load(t, store)

	// Assert
	if want := (Bounds{MinCharsPerSecond: 11, MaxCharsPerSecond: 16}); p.Bounds(prose) != want {
		t.Fatalf("Bounds after load = %+v, want %+v", p.Bounds(prose), want)
	}
	if _, ok := hasRecord(log, "info", "daemon.revealpace.loaded"); !ok {
		t.Fatalf("no INFO load record: %v", log.Records())
	}
}

func TestLoadKeepsOnlyTheNewestSpeedsOfAnOversizedWindow(t *testing.T) {
	// Arrange: an older build's longer window, slow first and fast last.
	speeds := append(make([]float64, 0, 2*WindowSize), make([]float64, WindowSize)...)
	for i := range speeds {
		speeds[i] = 10
	}
	for range WindowSize {
		speeds = append(speeds, 100)
	}
	store := &fakeStore{windows: []wsm.RevealSpeedWindow{{Model: prose.Model, Kind: prose.Kind, CharsPerSecond: speeds}}}

	// Act
	p, _ := load(t, store)

	// Assert
	if want := (Bounds{MinCharsPerSecond: 100, MaxCharsPerSecond: 100}); p.Bounds(prose) != want {
		t.Fatalf("Bounds = %+v, want only the newest %d speeds %+v", p.Bounds(prose), WindowSize, want)
	}
}

func TestLoadFailsWhenTheStoreCannotBeRead(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	store := &fakeStore{readErr: errors.New("disk gone")}

	// Act
	p, err := Load(context.Background(), store, log)

	// Assert
	if err == nil || p != nil {
		t.Fatalf("Load = (%v, %v), want a failure and no pacer", p, err)
	}
	r, ok := hasRecord(log, "error", "daemon.revealpace.load")
	if !ok || r.Context["error"] != "disk gone" {
		t.Fatalf("no ERROR load record carrying the cause: %v", log.Records())
	}
}

func TestPersistWritesTheKeysWindowOldestFirst(t *testing.T) {
	// Arrange
	store := &fakeStore{}
	p, _ := load(t, store)
	p.Observe(prose, 10)
	p.Observe(prose, 20)

	// Act
	err := p.Persist(context.Background(), prose)

	// Assert
	if err != nil {
		t.Fatalf("Persist: %v", err)
	}
	want := []wsm.RevealSpeedWindow{{Model: prose.Model, Kind: prose.Kind, CharsPerSecond: []float64{10, 20}}}
	if !reflect.DeepEqual(store.puts, want) {
		t.Fatalf("puts = %+v, want %+v", store.puts, want)
	}
}

func TestPersistSurfacesAndLogsAStoreFailure(t *testing.T) {
	// Arrange
	store := &fakeStore{putErr: errors.New("disk full")}
	p, log := load(t, store)
	p.Observe(prose, 10)

	// Act
	err := p.Persist(context.Background(), prose)

	// Assert
	if err == nil {
		t.Fatal("Persist swallowed the store's failure")
	}
	r, ok := hasRecord(log, "error", "daemon.revealpace.persist")
	if !ok || r.Context["error"] != "disk full" || r.Context["model"] != prose.Model || r.Context["kind"] != string(prose.Kind) {
		t.Fatalf("no ERROR persist record carrying the key and cause: %v", log.Records())
	}
}
