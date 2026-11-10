package composermentions

import (
	"context"
	"errors"
	"slices"
	"sync"
	"testing"

	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// fakeStore is the durable record in memory, with its two failures.
type fakeStore struct {
	mu      sync.Mutex
	stored  []wsm.ComposerMention
	puts    [][]wsm.ComposerMention
	loadErr error
	putErr  error
}

func (f *fakeStore) PutComposerMentions(_ context.Context, ms []wsm.ComposerMention) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.puts = append(f.puts, ms)
	return f.putErr
}

func (f *fakeStore) ComposerMentions(context.Context) ([]wsm.ComposerMention, error) {
	return f.stored, f.loadErr
}

const ws = ids.WorkspaceID("ws-1")

func newKeeper(t *testing.T, store *fakeStore, open ...string) (*Keeper, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	k, err := New(context.Background(), Deps{Store: store, WorkspaceNames: func() []string { return open }, Log: log})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return k, log
}

// latest answers W's record in force, nil when the topic holds none.
func latest(k *Keeper, w ids.WorkspaceID) *agentreplv1.HostWorkspaceMentions {
	v, _ := k.Topic(w).Latest()
	return v
}

func mentions(commands, workspaces []string) *agentreplv1.HostWorkspaceMentions {
	return &agentreplv1.HostWorkspaceMentions{Commands: commands, Workspaces: workspaces}
}

func TestAnsweredOrdersTheRecord(t *testing.T) {
	type answer struct {
		markdown string
		at       int64
	}
	tests := []struct {
		name    string
		answers []answer
		want    *agentreplv1.HostWorkspaceMentions
	}{
		{name: "a workspace that mentioned nothing holds no record"},
		{name: "a response mentioning nothing holds no record", answers: []answer{{"plain prose", 10}}},
		{
			name:    "a response's commands and open workspaces are recorded",
			answers: []answer{{"run /merge-queue, then ask `alpha` and closed-one", 10}},
			want:    mentions([]string{"merge-queue"}, []string{"alpha"}),
		},
		{
			name:    "the most recently mentioned comes first",
			answers: []answer{{"/a and alpha", 10}, {"/b and beta", 20}},
			want:    mentions([]string{"b", "a"}, []string{"beta", "alpha"}),
		},
		{
			name:    "an older response read later never makes its names newer",
			answers: []answer{{"/b and beta", 20}, {"/a and alpha", 10}, {"/b and beta", 5}},
			want:    mentions([]string{"b", "a"}, []string{"beta", "alpha"}),
		},
		{
			name:    "a newer mention moves a name to the front",
			answers: []answer{{"/a /b", 10}, {"/a", 20}},
			want:    mentions([]string{"a", "b"}, nil),
		},
		{
			name:    "names mentioned at one instant are ordered by name",
			answers: []answer{{"/z /m beta alpha", 10}},
			want:    mentions([]string{"m", "z"}, []string{"alpha", "beta"}),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			k, _ := newKeeper(t, &fakeStore{}, "alpha", "beta")

			// Act
			for _, a := range tt.answers {
				k.Answered(ws, a.markdown, a.at)
			}

			// Assert
			if got := latest(k, ws); !proto.Equal(got, tt.want) {
				t.Fatalf("record = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestAnsweredWritesOnlyWhatMovedForward(t *testing.T) {
	// Arrange
	store := &fakeStore{}
	k, _ := newKeeper(t, store, "alpha")
	k.Answered(ws, "/a alpha", 20)

	// Act
	k.Answered(ws, "/a /b alpha", 10)

	// Assert
	want := [][]wsm.ComposerMention{
		{{Workspace: ws, Kind: wsm.ComposerMentionCommand, Name: "a", MentionedAtMs: 20}, {Workspace: ws, Kind: wsm.ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 20}},
		{{Workspace: ws, Kind: wsm.ComposerMentionCommand, Name: "b", MentionedAtMs: 10}},
	}
	if len(store.puts) != len(want) {
		t.Fatalf("puts = %+v, want %+v", store.puts, want)
	}
	for i := range want {
		got := slices.Clone(store.puts[i])
		slices.SortFunc(got, func(a, b wsm.ComposerMention) int { return len(a.Kind) - len(b.Kind) })
		if !slices.Equal(got, want[i]) {
			t.Fatalf("put %d = %+v, want %+v", i, got, want[i])
		}
	}
}

func TestAnsweredWithNothingNewWritesNothing(t *testing.T) {
	// Arrange
	store := &fakeStore{}
	k, _ := newKeeper(t, store)

	// Act
	k.Answered(ws, "no names at all", 10)

	// Assert
	if len(store.puts) != 0 {
		t.Fatalf("puts = %+v, want none", store.puts)
	}
}

func TestNewHydratesEveryWorkspacesRecord(t *testing.T) {
	// Arrange
	other := ids.WorkspaceID("ws-2")
	store := &fakeStore{stored: []wsm.ComposerMention{
		{Workspace: ws, Kind: wsm.ComposerMentionCommand, Name: "old", MentionedAtMs: 1},
		{Workspace: ws, Kind: wsm.ComposerMentionCommand, Name: "new", MentionedAtMs: 2},
		{Workspace: other, Kind: wsm.ComposerMentionWorkspace, Name: "alpha", MentionedAtMs: 3},
	}}

	// Act
	k, _ := newKeeper(t, store)

	// Assert
	if got, want := latest(k, ws), mentions([]string{"new", "old"}, nil); !proto.Equal(got, want) {
		t.Fatalf("ws record = %v, want %v", got, want)
	}
	if got, want := latest(k, other), mentions(nil, []string{"alpha"}); !proto.Equal(got, want) {
		t.Fatalf("other record = %v, want %v", got, want)
	}
}

func TestAHydratedInstantHoldsAgainstAnOlderReplay(t *testing.T) {
	// Arrange
	store := &fakeStore{stored: []wsm.ComposerMention{{Workspace: ws, Kind: wsm.ComposerMentionCommand, Name: "a", MentionedAtMs: 20}}}
	k, _ := newKeeper(t, store)

	// Act
	k.Answered(ws, "/b /a", 10)

	// Assert
	if got, want := latest(k, ws), mentions([]string{"a", "b"}, nil); !proto.Equal(got, want) {
		t.Fatalf("record = %v, want %v", got, want)
	}
}

func TestAnsweredPublishesEachChange(t *testing.T) {
	// Arrange
	k, _ := newKeeper(t, &fakeStore{})
	sub := k.Topic(ws).Subscribe(t.Context())

	// Act
	k.Answered(ws, "/a", 10)
	k.Answered(ws, "/b", 20)

	// Assert
	if got := <-sub; !proto.Equal(got, mentions([]string{"a"}, nil)) {
		t.Fatalf("first push = %v, want the first change", got)
	}
	if got := <-sub; !proto.Equal(got, mentions([]string{"b", "a"}, nil)) {
		t.Fatalf("second push = %v, want the second change", got)
	}
}

func TestAResponseWithNoSettleInstantIsRefused(t *testing.T) {
	// Arrange
	store := &fakeStore{}
	k, log := newKeeper(t, store)

	// Act
	k.Answered(ws, "/a", 0)

	// Assert
	if len(store.puts) != 0 || latest(k, ws) != nil {
		t.Fatalf("puts = %+v, record = %v; want nothing recorded", store.puts, latest(k, ws))
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opAnswered})
	if len(records) != 1 || records[0].Context["workspace"] != string(ws) {
		t.Fatalf("records = %+v, want one ERROR naming the workspace", log.Records())
	}
}

func TestAFailedWriteIsLoggedAndKeptInHand(t *testing.T) {
	// Arrange
	store := &fakeStore{putErr: errors.New("disk full")}
	k, log := newKeeper(t, store)

	// Act
	k.Answered(ws, "/a", 10)

	// Assert
	if !proto.Equal(latest(k, ws), mentions([]string{"a"}, nil)) {
		t.Fatalf("record = %v, want the change kept in hand", latest(k, ws))
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opAnswered})
	if len(records) != 1 || records[0].Context["changed"] != 1 || records[0].Context["cause"] == nil {
		t.Fatalf("records = %+v, want one ERROR naming the change and its cause", log.Records())
	}
}

func TestAFailedLoadIsLoggedAndReturned(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	store := &fakeStore{loadErr: errors.New("corrupt")}

	// Act
	k, err := New(context.Background(), Deps{Store: store, WorkspaceNames: func() []string { return nil }, Log: log})

	// Assert
	if err == nil || k != nil {
		t.Fatalf("New = %v, %v; want the failure", k, err)
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opLoad})
	if len(records) != 1 || records[0].Context["cause"] == nil {
		t.Fatalf("records = %+v, want one ERROR carrying the cause", log.Records())
	}
}
