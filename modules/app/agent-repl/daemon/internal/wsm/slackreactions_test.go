package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// p1 is the p1 priority, addressable.
var p1 = PriorityP1

// ackBinding binds :agent-ack: to an announced workspace.
var ackBinding = SlackReactionBinding{Emoji: "agent-ack", Thread: SlackReactionAnnounced}

func TestFreshReactionSettingsMissEverything(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	got, err := s.SlackReactionSettings(context.Background())

	// Assert
	if err != nil || got.BaselinedAt != nil || len(got.Bindings) != 0 {
		t.Fatalf("SlackReactionSettings = %+v, %v; want empty settings", got, err)
	}
	if want := []string{"a watch user", "a repository", "a binding"}; !reflect.DeepEqual(got.Missing(), want) {
		t.Fatalf("Missing = %v, want %v", got.Missing(), want)
	}
}

func TestBindingsRoundTripAndReplace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	sub := SlackReactionBinding{Emoji: "p1-one-shot", Thread: SlackReactionSubscribed, Mode: SlackThreadModeRespond, OneShot: true, Priority: &p1}
	if _, err := s.BindSlackReaction(context.Background(), ackBinding); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := s.BindSlackReaction(context.Background(), sub)
	replaced, err2 := s.BindSlackReaction(context.Background(), SlackReactionBinding{Emoji: "agent-ack", Thread: SlackReactionAnnounced, OneShot: true})

	// Assert
	if err != nil || err2 != nil {
		t.Fatalf("BindSlackReaction: %v, %v", err, err2)
	}
	if !reflect.DeepEqual(got.Bindings, []SlackReactionBinding{ackBinding, sub}) {
		t.Fatalf("bindings = %+v", got.Bindings)
	}
	if b, ok := replaced.Binding("agent-ack"); !ok || !b.OneShot || len(replaced.Bindings) != 2 {
		t.Fatalf("after the replace: %+v", replaced.Bindings)
	}
}

func TestBindingRefusesAContradictoryBinding(t *testing.T) {
	tests := []struct {
		name string
		b    SlackReactionBinding
	}{
		{name: "announced with a mode", b: SlackReactionBinding{Emoji: "x", Thread: SlackReactionAnnounced, Mode: SlackThreadModeRespond}},
		{name: "subscribed without one", b: SlackReactionBinding{Emoji: "x", Thread: SlackReactionSubscribed}},
		{name: "no emoji", b: SlackReactionBinding{Thread: SlackReactionAnnounced}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			_, err := s.BindSlackReaction(context.Background(), tt.b)

			// Assert
			if err == nil || !hasRecord(log, "error", "daemon.wsm.bind_slack_reaction", "refused a binding its caller should never have made") {
				t.Fatalf("BindSlackReaction = %v; want a refusal recorded: %+v", err, log.Records())
			}
			if n := scalar[int](t, s, `SELECT COUNT(*) FROM slack_reaction_bindings`); n != 0 {
				t.Fatalf("%d bindings written", n)
			}
		})
	}
}

func TestUnbinding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.BindSlackReaction(context.Background(), ackBinding); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := s.UnbindSlackReaction(context.Background(), "agent-ack")
	_, again := s.UnbindSlackReaction(context.Background(), "agent-ack")

	// Assert
	if err != nil || len(got.Bindings) != 0 {
		t.Fatalf("UnbindSlackReaction = %+v, %v", got, err)
	}
	if !errors.Is(again, ErrNotFound) {
		t.Fatalf("a second unbind = %v, want ErrNotFound", again)
	}
}

func TestChangingTheWatchUserOwesANewBaselineAndFirstSearch(t *testing.T) {
	tests := []struct {
		name string
		user string
		kept bool
	}{
		{name: "the same user keeps the baseline and where the search read to", user: "U7HH4U1J4", kept: true},
		{name: "another user owes a new baseline and first search", user: "U0000000", kept: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			if _, err := s.SetSlackReactionWatchUser(context.Background(), "U7HH4U1J4"); err != nil {
				t.Fatal(err)
			}
			if err := s.RecordSlackReactionBaseline(context.Background(), nil, instant); err != nil {
				t.Fatal(err)
			}
			if err := s.RecordSlackReactionSearch(context.Background(), instant); err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := s.SetSlackReactionWatchUser(context.Background(), tt.user)

			// Assert
			if err != nil || got.WatchUser != tt.user || (got.BaselinedAt != nil) != tt.kept || (got.SearchedThrough != nil) != tt.kept {
				t.Fatalf("SetSlackReactionWatchUser = %+v, %v; want kept=%v", got, err, tt.kept)
			}
		})
	}
}

func TestSettingTheRepository(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	got, err := s.SetSlackReactionRepository(context.Background(), string(ws.Repo))
	_, unknown := s.SetSlackReactionRepository(context.Background(), "repo-nobody")

	// Assert
	if err != nil || got.RepoID != string(ws.Repo) {
		t.Fatalf("SetSlackReactionRepository = %+v, %v", got, err)
	}
	if !errors.Is(unknown, ErrNotFound) {
		t.Fatalf("an unknown repository = %v, want ErrNotFound", unknown)
	}
}

func TestATriggerFiresOnceAndTheBaselineCountsAsFired(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	standing := SlackReactionTrigger{DispatchID: "aaaa", Channel: "C1", TS: "1.1", Emoji: "agent-ack"}
	fresh := SlackReactionTrigger{DispatchID: "bbbb", Channel: "C1", TS: "1.2", Emoji: "agent-ack"}
	if err := s.RecordSlackReactionBaseline(context.Background(), []SlackReactionTrigger{standing}, instant); err != nil {
		t.Fatal(err)
	}

	// Act
	standingFired, err1 := s.MarkSlackReactionFired(context.Background(), standing, instant.Add(time.Minute))
	first, err2 := s.MarkSlackReactionFired(context.Background(), fresh, instant.Add(time.Minute))
	second, err3 := s.MarkSlackReactionFired(context.Background(), fresh, instant.Add(2*time.Minute))

	// Assert
	if err1 != nil || err2 != nil || err3 != nil {
		t.Fatalf("MarkSlackReactionFired: %v, %v, %v", err1, err2, err3)
	}
	if standingFired || !first || second {
		t.Fatalf("standing %v, first %v, second %v; want only the first fresh mark new", standingFired, first, second)
	}
	got, _ := s.SlackReactionSettings(context.Background())
	if got.BaselinedAt == nil || !got.BaselinedAt.Equal(instant) {
		t.Fatalf("baselined at %v, want %v", got.BaselinedAt, instant)
	}
}

func TestRecordingASearchSetsWhereTheNextReadsOn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	later := instant.Add(time.Minute)
	if err := s.RecordSlackReactionSearch(context.Background(), instant); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.RecordSlackReactionSearch(context.Background(), later)

	// Assert
	got, readErr := s.SlackReactionSettings(context.Background())
	if err != nil || readErr != nil || got.SearchedThrough == nil || !got.SearchedThrough.Equal(later) {
		t.Fatalf("SearchedThrough = %v (%v, %v), want %v", got.SearchedThrough, err, readErr, later)
	}
}

func TestRecordingASearchOnAReadOnlyHandleIsRefused(t *testing.T) {
	// Arrange
	ro, log := readOnlyUsageStore(t)

	// Act
	err := ro.RecordSlackReactionSearch(context.Background(), instant)

	// Assert
	if !errors.Is(err, ErrReadOnly) {
		t.Fatalf("RecordSlackReactionSearch = %v, want ErrReadOnly", err)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.record_slack_reaction_search", Message: "refused a write on a read-only handle"})
	if !ok || rec.Context["searched_through"] != instant.UTC().Format(time.RFC3339Nano) {
		t.Fatalf("records %+v", log.Records())
	}
}

func TestTheMigrationLeavesTheFirstSearchOwed(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 212)
	execRaw(t, path, `INSERT INTO slack_reaction_settings (id, watch_user, baselined_at) VALUES (1, 'U7HH4U1J4', 5)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-212 database: %v", err)
	}
	defer handle.Close()

	// Assert
	got, err := handle.SlackReactionSettings(context.Background())
	if err != nil || got.WatchUser != "U7HH4U1J4" || got.BaselinedAt == nil || got.SearchedThrough != nil {
		t.Fatalf("SlackReactionSettings = %+v, %v; want the baseline kept and the first search owed", got, err)
	}
}
