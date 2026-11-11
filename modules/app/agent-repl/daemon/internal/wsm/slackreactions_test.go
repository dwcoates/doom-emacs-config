package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
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

func TestChangingTheWatchUserOwesANewBaseline(t *testing.T) {
	tests := []struct {
		name      string
		user      string
		baselined bool
	}{
		{name: "the same user keeps the baseline", user: "U7HH4U1J4", baselined: true},
		{name: "another user owes a new one", user: "U0000000", baselined: false},
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

			// Act
			got, err := s.SetSlackReactionWatchUser(context.Background(), tt.user)

			// Assert
			if err != nil || got.WatchUser != tt.user || (got.BaselinedAt != nil) != tt.baselined {
				t.Fatalf("SetSlackReactionWatchUser = %+v, %v; want baselined=%v", got, err, tt.baselined)
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
