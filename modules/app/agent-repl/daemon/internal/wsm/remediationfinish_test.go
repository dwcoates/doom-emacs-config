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
			s, log := testStore(t)
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
			if loggedOperation(log, "daemon.wsm.begin_remediation_finish", "error") || !loggedOperation(log, "daemon.wsm.begin_remediation_finish", "debug") {
				t.Fatalf("records = %v, want the refusal at DEBUG: it is the store answering what was asked", log.Records())
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
		if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: ws, Repo: repo, StartedAt: instant, ServesEscalation: ws == "escalation"}); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	n, err := s.EndRemediationFinishesOf(ctx, repo)
	all, _ := s.RemediationFinishes(ctx)

	// Assert
	if err != nil || n != 1 || len(all) != 1 || all[0].Workspace != "escalation" {
		t.Fatalf("EndRemediationFinishesOf = (%d, %v) leaving %+v, want the chain's remediation ended and the escalation kept", n, err, all)
	}
}

func TestAPassedRemediationGateRecordsTheCommitItPassedOn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)

	// Act
	moved, err := s.PassRemediationGate(context.Background(), "remedy", "abc", instant)
	f, _ := s.RemediationFinishOf(context.Background(), "remedy")

	// Assert
	if err != nil || !moved || f.Step != RemediationLanding || f.GatedHead != "abc" {
		t.Fatalf("PassRemediationGate = (%v, %v), finish %+v; want it landing at abc", moved, err, f)
	}
}

func TestAPassedRemediationGateNamingNoCommitIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)

	// Act
	_, err := s.PassRemediationGate(context.Background(), "remedy", "", instant)

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.pass_remediation_gate", "error") {
		t.Fatalf("PassRemediationGate = %v, want a refusal recorded at ERROR", err)
	}
}

func TestALandingFinishWithNoGatedCommitGoesBackToItsGate(t *testing.T) {
	tests := []struct {
		name      string
		step      RemediationStep
		gatedHead string
		want      RemediationStep
	}{
		{name: "a landing recorded before the gated commit was goes back to its gate", step: RemediationLanding, want: RemediationGating},
		{name: "a landing naming its gated commit stays", step: RemediationLanding, gatedHead: "abc", want: RemediationLanding},
		{name: "a finish past its landing stays", step: RemediationDeploying, want: RemediationDeploying},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			beginFinish(t, s, RepoKey(t.TempDir()), instant, nil)
			if _, err := s.db().Exec(`UPDATE deploy_remediation_finishes SET step = ?, gated_head = ?`, string(tt.step), tt.gatedHead); err != nil {
				t.Fatal(err)
			}

			// Act
			if _, err := s.db().Exec(remediationUngatedLandingsDDL); err != nil {
				t.Fatalf("the layout-149 step: %v", err)
			}

			// Assert
			f, err := s.RemediationFinishOf(context.Background(), "remedy")
			if err != nil || f.Step != tt.want {
				t.Fatalf("finish = %+v (%v), want step %s", f, err, tt.want)
			}
		})
	}
}

func TestAFinishKeepsWhetherItServesAnEscalation(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{name: "a dispatched fix's finish is its chain's"},
		{name: "an escalation's finish serves it", want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())

			// Act
			begun, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: "mediator", Repo: repo, StartedAt: instant, ServesEscalation: tt.want})

			// Assert
			read, readErr := s.RemediationFinishOf(ctx, "mediator")
			if err != nil || readErr != nil || begun.ServesEscalation != tt.want || read == nil || read.ServesEscalation != tt.want {
				t.Fatalf("Begin = (%+v, %v), read (%+v, %v); want serves escalation %v", begun, err, read, readErr, tt.want)
			}
		})
	}
}

func TestARebegunFinishTakesTheKindOfItsNewWork(t *testing.T) {
	// Arrange: the mediator's escalation fix deployed; it now fixes a dispatch.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: "mediator", Repo: repo, StartedAt: instant, ServesEscalation: true}); err != nil {
		t.Fatal(err)
	}
	if _, err := s.AdvanceRemediationFinish(ctx, "mediator", RemediationGating, RemediationDeployed, instant); err != nil {
		t.Fatal(err)
	}

	// Act
	begun, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: "mediator", Repo: repo, StartedAt: instant.Add(time.Hour)})

	// Assert
	read, _ := s.RemediationFinishOf(ctx, "mediator")
	if err != nil || begun.ServesEscalation || read.ServesEscalation {
		t.Fatalf("Begin = (%+v, %v), read %+v; want the dispatch's kind", begun, err, read)
	}
}

func TestTheBackfillMarksAnEarlierEscalationsFinish(t *testing.T) {
	// Arrange: a file at layout 166 with two finishes, one of an escalation.
	path := fixtureAt(t, 166)
	for _, ws := range []string{"remedy", "escalation"} {
		execRaw(t, path, `INSERT INTO deploy_remediation_finishes (workspace_id, repo_key, step, step_at, started_at) VALUES (?, '/tmp/repo', 'gating', 1, 1)`, ws)
	}
	execRaw(t, path, `INSERT INTO deploy_escalations (workspace_id, repo_key, made_at) VALUES ('escalation', '/tmp/repo', 1)`)

	// Act
	execRaw(t, path, remediationEscalationDDL)

	// Assert
	remedy := rawScalar[int](t, path, `SELECT serves_escalation FROM deploy_remediation_finishes WHERE workspace_id = 'remedy'`)
	escalation := rawScalar[int](t, path, `SELECT serves_escalation FROM deploy_remediation_finishes WHERE workspace_id = 'escalation'`)
	if remedy != 0 || escalation != 1 {
		t.Fatalf("serves_escalation remedy %d, escalation %d; want only the escalation's finish marked", remedy, escalation)
	}
}
