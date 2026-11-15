package wsm

import (
	"context"
	"errors"
	"strings"
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

func TestARemediationLandingRecordsItsHeadAndAddsItsPaths(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)
	land := func(head string, paths ...string) {
		t.Helper()
		if ok, err := s.AdvanceRemediationFinish(ctx, "remedy", RemediationGating, RemediationLanding, instant); err != nil || !ok {
			t.Fatalf("AdvanceRemediationFinish = (%v, %v)", ok, err)
		}
		if ok, err := s.RecordRemediationLanded(ctx, "remedy", head, paths, instant.Add(time.Minute)); err != nil || !ok {
			t.Fatalf("RecordRemediationLanded = (%v, %v)", ok, err)
		}
	}
	land("abc", "daemon/b.go", "daemon/a.go")
	if ok, err := s.AdvanceRemediationFinish(ctx, "remedy", RemediationDeploying, RemediationDeployed, instant); err != nil || !ok {
		t.Fatalf("AdvanceRemediationFinish = (%v, %v)", ok, err)
	}
	if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: "remedy", Repo: RepoKey(t.TempDir()), StartedAt: instant.Add(time.Hour)}); err != nil {
		t.Fatalf("a restored workspace's finish after its deploy: %v", err)
	}

	// Act
	land("def", "daemon/a.go", "webapp/c.ts")
	got, err := s.RemediationFinishOf(ctx, "remedy")

	// Assert
	want := []string{"daemon/a.go", "daemon/b.go", "webapp/c.ts"}
	if err != nil || got.Step != RemediationDeploying || got.LandedHead != "def" || strings.Join(got.LandedPaths, ",") != strings.Join(want, ",") {
		t.Fatalf("RemediationFinishOf = (%+v, %v), want deploying at def having changed %v", got, err, want)
	}
}

func TestARemediationLandingIsRecordedOnlyWhileLanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)

	// Act
	ok, err := s.RecordRemediationLanded(context.Background(), "remedy", "abc", nil, instant)

	// Assert
	if err != nil || ok {
		t.Fatalf("RecordRemediationLanded while gating = (%v, %v), want nothing recorded", ok, err)
	}
}

func TestARemediationLandingRefusesOneNamingNoHead(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.RecordRemediationLanded(context.Background(), "remedy", "", nil, instant)

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.record_remediation_landed", "error") {
		t.Fatalf("RecordRemediationLanded = %v, want a refusal recorded at ERROR: %v", err, log.Records())
	}
}

func TestEndRemediationFinishesOfEndsOnlyThatQueuesFinishes(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	ended, kept := RepoKey(t.TempDir()), RepoKey(t.TempDir())
	for ws, repo := range map[WorkspaceID]RepoKey{"a": ended, "b": ended, "c": kept} {
		if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: ws, Repo: repo, StartedAt: instant}); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	n, err := s.EndRemediationFinishesOf(ctx, ended)
	all, _ := s.RemediationFinishes(ctx)

	// Assert
	if err != nil || n != 2 || len(all) != 1 || all[0].Workspace != "c" {
		t.Fatalf("EndRemediationFinishesOf = (%d, %v) leaving %+v, want 2 ended and c kept", n, err, all)
	}
}

func TestAChainsEndKeepsADeployEscalationsFinish(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	for _, ws := range []WorkspaceID{"remedy", "escalation"} {
		if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: ws, Repo: repo, StartedAt: instant}); err != nil {
			t.Fatal(err)
		}
	}
	if err := s.RecordDeployEscalation(ctx, "escalation", repo, instant); err != nil {
		t.Fatal(err)
	}

	// Act
	n, err := s.EndRemediationFinishesOf(ctx, repo)
	all, _ := s.RemediationFinishes(ctx)

	// Assert
	if err != nil || n != 1 || len(all) != 1 || all[0].Workspace != "escalation" {
		t.Fatalf("EndRemediationFinishesOf = (%d, %v) leaving %+v, want the chain's remediation ended and the escalation kept", n, err, all)
	}
}

func TestADeployEscalationIsRecordedAndRead(t *testing.T) {
	tests := []struct {
		name      string
		record    bool
		wantFound bool
	}{
		{name: "a recorded escalation", record: true, wantFound: true},
		{name: "any other workspace"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			if tt.record {
				if err := s.RecordDeployEscalation(context.Background(), "escalation", repo, instant); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, found, err := s.DeployEscalationOf(context.Background(), "escalation")

			// Assert
			if err != nil || found != tt.wantFound || (found && got != repo) {
				t.Fatalf("DeployEscalationOf = (%q, %v, %v), want found %v", got, found, err, tt.wantFound)
			}
		})
	}
}

func TestARecordOfADeployEscalationNamingNothingIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	err := s.RecordDeployEscalation(context.Background(), "", "", instant)

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.record_deploy_escalation", "error") {
		t.Fatalf("RecordDeployEscalation = %v, want a refusal recorded at ERROR: %v", err, log.Records())
	}
}
