package composerselections

import (
	"context"
	"errors"
	"slices"
	"sync"
	"testing"
	"time"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// fakeStore is the durable record in memory, with its two failures.
type fakeStore struct {
	mu      sync.Mutex
	stored  []wsm.ComposerSelection
	puts    []wsm.ComposerSelection
	loadErr error
	putErr  error
}

func (f *fakeStore) PutComposerSelection(_ context.Context, sel wsm.ComposerSelection) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.puts = append(f.puts, sel)
	return f.putErr
}

func (f *fakeStore) ComposerSelections(context.Context) ([]wsm.ComposerSelection, error) {
	return f.stored, f.loadErr
}

// clock answers the instants in AT one by one, holding the last.
func clock(at ...int64) func() time.Time {
	var mu sync.Mutex
	return func() time.Time {
		mu.Lock()
		defer mu.Unlock()
		now := at[0]
		if len(at) > 1 {
			at = at[1:]
		}
		return time.UnixMilli(now)
	}
}

func newKeeper(t *testing.T, store *fakeStore, now func() time.Time) (*Keeper, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	k, err := New(context.Background(), Deps{Store: store, Now: now, Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return k, log
}

// latest answers the record in force, nil when the topic holds none.
func latest(k *Keeper) *agentreplv1.HostComposerSelections {
	v, _ := k.Topic().Latest()
	return v
}

func selections(commands, workspaces, channels []string) *agentreplv1.HostComposerSelections {
	return &agentreplv1.HostComposerSelections{Commands: commands, Workspaces: workspaces, Channels: channels}
}

type taking struct {
	kind wsm.ComposerSelectionKind
	name string
}

func TestSelectedOrdersTheRecord(t *testing.T) {
	tests := []struct {
		name    string
		stored  []wsm.ComposerSelection
		takings []taking
		want    *agentreplv1.HostComposerSelections
	}{
		{name: "nothing taken holds no record"},
		{
			name:    "the most recently taken command comes first",
			takings: []taking{{wsm.ComposerSelectionCommand, "autocompact"}, {wsm.ComposerSelectionCommand, "compact"}},
			want:    selections([]string{"compact", "autocompact"}, []string{}, []string{}),
		},
		{
			name:    "taking a name again moves it to the front",
			takings: []taking{{wsm.ComposerSelectionCommand, "a"}, {wsm.ComposerSelectionCommand, "b"}, {wsm.ComposerSelectionCommand, "a"}},
			want:    selections([]string{"a", "b"}, []string{}, []string{}),
		},
		{
			name: "each kind is ordered apart",
			takings: []taking{
				{wsm.ComposerSelectionWorkspace, "alpha"},
				{wsm.ComposerSelectionChannel, "eng"},
				{wsm.ComposerSelectionCommand, "alpha"},
			},
			want: selections([]string{"alpha"}, []string{"alpha"}, []string{"eng"}),
		},
		{
			name:    "a taking after a hydrated record from the future is still the newest",
			stored:  []wsm.ComposerSelection{{Kind: wsm.ComposerSelectionCommand, Name: "old", SelectedAtMs: 1_000_000}},
			takings: []taking{{wsm.ComposerSelectionCommand, "new"}},
			want:    selections([]string{"new", "old"}, []string{}, []string{}),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a clock frozen at one instant, so only the stamp's
			// strict increase can order the takings.
			k, _ := newKeeper(t, &fakeStore{stored: tt.stored}, clock(10))

			// Act
			for _, tk := range tt.takings {
				if err := k.Selected(context.Background(), tk.kind, tk.name); err != nil {
					t.Fatalf("Selected: %v", err)
				}
			}

			// Assert
			if got := latest(k); !proto.Equal(got, tt.want) {
				t.Fatalf("record = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestSelectedStampsStrictlyLaterThanEveryEarlierTaking(t *testing.T) {
	tests := []struct {
		name string
		now  []int64
		want []int64
	}{
		{name: "a moving clock is taken as it reads", now: []int64{10, 20}, want: []int64{10, 20}},
		{name: "a frozen clock is stepped past the last stamp", now: []int64{10, 10}, want: []int64{10, 11}},
		{name: "a clock stepped backwards is stepped past the last stamp", now: []int64{50, 5}, want: []int64{50, 51}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := &fakeStore{}
			k, _ := newKeeper(t, store, clock(tt.now...))

			// Act
			for range tt.now {
				if err := k.Selected(context.Background(), wsm.ComposerSelectionCommand, "compact"); err != nil {
					t.Fatalf("Selected: %v", err)
				}
			}

			// Assert
			var got []int64
			for _, put := range store.puts {
				got = append(got, put.SelectedAtMs)
			}
			if !slices.Equal(got, tt.want) {
				t.Fatalf("stamps written = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNewHydratesTheRecord(t *testing.T) {
	// Arrange
	store := &fakeStore{stored: []wsm.ComposerSelection{
		{Kind: wsm.ComposerSelectionCommand, Name: "old", SelectedAtMs: 1},
		{Kind: wsm.ComposerSelectionCommand, Name: "new", SelectedAtMs: 2},
		{Kind: wsm.ComposerSelectionChannel, Name: "eng", SelectedAtMs: 3},
	}}

	// Act
	k, _ := newKeeper(t, store, clock(10))

	// Assert
	if got, want := latest(k), selections([]string{"new", "old"}, []string{}, []string{"eng"}); !proto.Equal(got, want) {
		t.Fatalf("record = %v, want %v", got, want)
	}
}

func TestSelectedPublishesEachChange(t *testing.T) {
	// Arrange
	k, _ := newKeeper(t, &fakeStore{}, clock(10, 20))
	sub := k.Topic().Subscribe(t.Context())

	// Act
	_ = k.Selected(context.Background(), wsm.ComposerSelectionCommand, "a")
	_ = k.Selected(context.Background(), wsm.ComposerSelectionCommand, "b")

	// Assert
	if got := <-sub; !proto.Equal(got, selections([]string{"a"}, []string{}, []string{})) {
		t.Fatalf("first push = %v, want the first taking", got)
	}
	if got := <-sub; !proto.Equal(got, selections([]string{"b", "a"}, []string{}, []string{})) {
		t.Fatalf("second push = %v, want the second taking", got)
	}
}

func TestAFailedWriteChangesNothingAndIsLogged(t *testing.T) {
	// Arrange
	store := &fakeStore{
		stored: []wsm.ComposerSelection{{Kind: wsm.ComposerSelectionCommand, Name: "a", SelectedAtMs: 1}},
		putErr: errors.New("disk full"),
	}
	k, log := newKeeper(t, store, clock(10))
	before := latest(k)

	// Act
	err := k.Selected(context.Background(), wsm.ComposerSelectionCommand, "b")

	// Assert
	if err == nil {
		t.Fatal("Selected = nil, want the write's failure")
	}
	if got := latest(k); got != before {
		t.Fatalf("record = %v, want the one in force before (%v), unpublished", got, before)
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opSelected})
	if len(records) != 1 || records[0].Context["name"] != "b" || records[0].Context["kind"] != "command" || records[0].Context["cause"] == nil {
		t.Fatalf("records = %+v, want one ERROR naming the selection and its cause", log.Records())
	}
	// The failed stamp is not held: the next taking may reuse it.
	store.putErr = nil
	if err := k.Selected(context.Background(), wsm.ComposerSelectionCommand, "b"); err != nil {
		t.Fatalf("Selected after recovery: %v", err)
	}
	if got := store.puts[len(store.puts)-1].SelectedAtMs; got != 10 {
		t.Fatalf("stamp after a failed write = %d, want 10", got)
	}
}

func TestAFailedLoadIsLoggedAndReturned(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	store := &fakeStore{loadErr: errors.New("corrupt")}

	// Act
	k, err := New(context.Background(), Deps{Store: store, Now: clock(10), Log: log})

	// Assert
	if err == nil || k != nil {
		t.Fatalf("New = %v, %v; want the failure", k, err)
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opLoad})
	if len(records) != 1 || records[0].Context["cause"] == nil {
		t.Fatalf("records = %+v, want one ERROR carrying the cause", log.Records())
	}
}
