package wsm

import (
	"context"
	"errors"
	"testing"
	"time"

	"agent-repld/internal/semver"
)

// beginFinish records workspace "remedy"'s finish of REPO at AT.
func beginFinish(t *testing.T, s *store, repo RepoKey, at time.Time, v *VersionIncrement) RemediationFinish {
	t.Helper()
	f, err := s.BeginRemediationFinish(context.Background(), RemediationFinish{Workspace: "remedy", Repo: repo, StartedAt: at, Version: v})
	if err != nil {
		t.Fatalf("BeginRemediationFinish: %v", err)
	}
	return f
}

func TestARemediationFinishBeginsAtItsGate(t *testing.T) {
	tests := []struct {
		name    string
		version *VersionIncrement
	}{
		{name: "a versioned finish keeps its increment", version: &VersionIncrement{Part: semver.Patch, ChangelogLine: "fix the reaper"}},
		{name: "an unversioned finish keeps none"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			beginFinish(t, s, repo, instant, tt.version)
			got, err := s.RemediationFinishOf(context.Background(), "remedy")

			// Assert
			if err != nil || got == nil || got.Step != RemediationGating || !got.StepAt.Equal(instant) || got.GateRounds != 0 || !got.StartedAt.Equal(instant) {
				t.Fatalf("RemediationFinishOf = (%+v, %v), want it gating since %v", got, err, instant)
			}
			if (got.Version == nil) != (tt.version == nil) || (got.Version != nil && *got.Version != *tt.version) {
				t.Fatalf("version = %+v, want %+v", got.Version, tt.version)
			}
		})
	}
}

func TestARemediationFinishIsRefusedWhileItRuns(t *testing.T) {
	for _, step := range []RemediationStep{RemediationGating, RemediationLanding, RemediationDeploying} {
		t.Run(string(step), func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			beginFinish(t, s, repo, instant, nil)
			if step != RemediationGating {
				if ok, err := s.AdvanceRemediationFinish(context.Background(), "remedy", RemediationGating, step, instant); err != nil || !ok {
					t.Fatalf("AdvanceRemediationFinish = (%v, %v)", ok, err)
				}
			}

			// Act
			_, err := s.BeginRemediationFinish(context.Background(), RemediationFinish{Workspace: "remedy", Repo: repo, StartedAt: instant.Add(time.Minute)})

			// Assert
			if !errors.Is(err, ErrRemediationFinishing) {
				t.Fatalf("BeginRemediationFinish = %v, want ErrRemediationFinishing", err)
			}
		})
	}
}

func TestAFinishAfterAFailedGateGatesAgainKeepingItsRounds(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	beginFinish(t, s, repo, instant, nil)
	if ok, err := s.AdvanceRemediationFinish(ctx, "remedy", RemediationGating, RemediationFixing, instant.Add(time.Minute)); err != nil || !ok {
		t.Fatalf("AdvanceRemediationFinish = (%v, %v)", ok, err)
	}
	again := instant.Add(time.Hour)
	v := &VersionIncrement{Part: semver.Minor, ChangelogLine: "again"}

	// Act
	got, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: "remedy", Repo: repo, StartedAt: again, Version: v})

	// Assert
	if err != nil || got.Step != RemediationGating || got.GateRounds != 1 || !got.StepAt.Equal(again) || !got.StartedAt.Equal(instant) || *got.Version != *v {
		t.Fatalf("BeginRemediationFinish = (%+v, %v), want gating again with one round", got, err)
	}
	stored, err := s.RemediationFinishOf(ctx, "remedy")
	if err != nil || stored == nil || stored.GateRounds != 1 || stored.Step != RemediationGating || *stored.Version != *v {
		t.Fatalf("RemediationFinishOf = (%+v, %v), want what the finish answered", stored, err)
	}
}

func TestARemediationFinishAdvancesOnlyFromWhereItStands(t *testing.T) {
	tests := []struct {
		name       string
		from, to   RemediationStep
		want       bool
		wantStep   RemediationStep
		wantRounds int
	}{
		{name: "the gate passes into the landing", from: RemediationGating, to: RemediationLanding, want: true, wantStep: RemediationLanding},
		{name: "a failed gate counts a round", from: RemediationGating, to: RemediationFixing, want: true, wantStep: RemediationFixing, wantRounds: 1},
		{name: "a move from a step it is not at changes nothing", from: RemediationLanding, to: RemediationDeploying, wantStep: RemediationGating},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)

			// Act
			moved, err := s.AdvanceRemediationFinish(ctx, "remedy", tt.from, tt.to, instant.Add(time.Minute))

			// Assert
			if err != nil || moved != tt.want {
				t.Fatalf("AdvanceRemediationFinish = (%v, %v), want %v", moved, err, tt.want)
			}
			got, _ := s.RemediationFinishOf(ctx, "remedy")
			if got.Step != tt.wantStep || got.GateRounds != tt.wantRounds {
				t.Fatalf("finish = %+v, want step %s with %d rounds", got, tt.wantStep, tt.wantRounds)
			}
		})
	}
}

func TestARemediationFinishRefusesAStepThatIsNoStep(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)

	// Act
	_, err := s.AdvanceRemediationFinish(context.Background(), "remedy", RemediationGating, "merged", instant)

	// Assert
	if err == nil {
		t.Fatal("AdvanceRemediationFinish moved to a step that is no step")
	}
	if !loggedOperation(log, "daemon.wsm.advance_remediation_finish", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestARemediationFinishRefusesOneNamingNoWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.BeginRemediationFinish(context.Background(), RemediationFinish{Repo: RepoKey(t.TempDir()), StartedAt: instant})

	// Assert
	if err == nil {
		t.Fatal("BeginRemediationFinish recorded a finish of no workspace")
	}
	if !loggedOperation(log, "daemon.wsm.begin_remediation_finish", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
	if all, _ := s.RemediationFinishes(context.Background()); len(all) != 0 {
		t.Fatalf("RemediationFinishes = %+v, want none recorded", all)
	}
}

func TestRemediationFinishesListsEveryFinishOldestFirstUntilItEnds(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	for i, ws := range []WorkspaceID{"second", "first"} {
		at := instant.Add(time.Duration(1-i) * time.Minute)
		if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: ws, Repo: RepoKey(t.TempDir()), StartedAt: at}); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	ended, err := s.EndRemediationFinish(ctx, "first")
	all, readErr := s.RemediationFinishes(ctx)

	// Assert
	if err != nil || !ended {
		t.Fatalf("EndRemediationFinish = (%v, %v), want it ended", ended, err)
	}
	if readErr != nil || len(all) != 1 || all[0].Workspace != "second" {
		t.Fatalf("RemediationFinishes = (%+v, %v), want only second", all, readErr)
	}
	if again, err := s.EndRemediationFinish(ctx, "first"); err != nil || again {
		t.Fatalf("EndRemediationFinish again = (%v, %v), want nothing ended", again, err)
	}
}
