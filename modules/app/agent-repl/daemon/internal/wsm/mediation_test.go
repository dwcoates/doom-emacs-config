package wsm

import (
	"context"
	"testing"
	"time"
)

func TestARepositoryWithNoDeployOwesNoMediation(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	m, err := s.MediationOf(context.Background(), RepoKey(t.TempDir()))

	// Assert
	if err != nil || m != nil {
		t.Fatalf("MediationOf = (%+v, %v), want none", m, err)
	}
}

func TestADeployOwesAMediationUnprompted(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())

	// Act
	if _, err := s.OweMediation(ctx, repo, instant, "0.431.0"); err != nil {
		t.Fatalf("OweMediation: %v", err)
	}
	m, err := s.MediationOf(ctx, repo)

	// Assert
	if err != nil || m == nil || !m.OwedAt.Equal(instant) || m.Version != "0.431.0" || m.Prompted() {
		t.Fatalf("MediationOf = (%+v, %v), want the deploy's mediation, unprompted", m, err)
	}
}

func TestAMediationIsPromptedOnlyWhileItIsTheOneOwed(t *testing.T) {
	tests := []struct {
		name string
		owed time.Time
		want bool
	}{
		{name: "the mediation owed", owed: instant, want: true},
		{name: "a mediation a newer deploy replaced", owed: instant.Add(-time.Minute)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			if _, err := s.OweMediation(ctx, repo, instant, ""); err != nil {
				t.Fatal(err)
			}

			// Act
			marked, err := s.MarkMediationPrompted(ctx, repo, tt.owed, instant.Add(time.Second), "turn-1")

			// Assert
			m, rerr := s.MediationOf(ctx, repo)
			if err != nil || rerr != nil || marked != tt.want || m.Prompted() != tt.want || (m.Turn == "turn-1") != tt.want {
				t.Fatalf("marked %v, prompted %v in %q (%v, %v); want %v", marked, m.Prompted(), m.Turn, err, rerr, tt.want)
			}
		})
	}
}

func TestReleaseMediationClearsOnlyTheMediationItNames(t *testing.T) {
	tests := []struct {
		name     string
		release  time.Time
		wantOwed bool
	}{
		{name: "a decision on the deploy owed releases it", release: instant},
		{name: "a decision on an older deploy releases nothing", release: instant.Add(-time.Minute), wantOwed: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			if _, err := s.OweMediation(ctx, repo, instant, ""); err != nil {
				t.Fatal(err)
			}

			// Act
			released, err := s.ReleaseMediation(ctx, repo, tt.release)

			// Assert
			m, rerr := s.MediationOf(ctx, repo)
			if err != nil || rerr != nil || released == tt.wantOwed || (m != nil) != tt.wantOwed {
				t.Fatalf("released %v, owed %+v (%v, %v); want owed %v", released, m, err, rerr, tt.wantOwed)
			}
		})
	}
}

func TestANewerDeployReplacesTheMediationOwedAndOwesItsPrompt(t *testing.T) {
	// Arrange: the first deploy's mediation was prompted.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	if _, err := s.OweMediation(ctx, repo, instant, "0.1.0"); err != nil {
		t.Fatal(err)
	}
	if _, err := s.MarkMediationPrompted(ctx, repo, instant, instant.Add(time.Second), "turn-1"); err != nil {
		t.Fatal(err)
	}

	// Act
	replaced, err := s.OweMediation(ctx, repo, instant.Add(time.Minute), "0.2.0")

	// Assert: the newer deploy's owed, and the prompted one it replaced
	// answered.
	if err != nil || replaced == nil || !replaced.OwedAt.Equal(instant) || replaced.PromptedAt == nil || !replaced.PromptedAt.Equal(instant.Add(time.Second)) {
		t.Fatalf("OweMediation = (%+v, %v), want the first deploy's prompted mediation replaced", replaced, err)
	}
	m, err := s.MediationOf(ctx, repo)
	if err != nil || !m.OwedAt.Equal(instant.Add(time.Minute)) || m.Version != "0.2.0" || m.Prompted() || m.Turn != "" {
		t.Fatalf("MediationOf = (%+v, %v), want the newer deploy's, unprompted", m, err)
	}
}

func TestOweMediationWithNothingOwedReplacesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())

	// Act
	replaced, err := s.OweMediation(context.Background(), repo, instant, "0.1.0")

	// Assert
	if err != nil || replaced != nil {
		t.Fatalf("OweMediation = (%+v, %v), want nothing replaced", replaced, err)
	}
}

func TestOweMediationRefusesADeployWithNoInstantAndWritesNothing(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())

	// Act
	_, err := s.OweMediation(ctx, repo, time.Time{}, "")

	// Assert
	m, rerr := s.MediationOf(ctx, repo)
	if err == nil || rerr != nil || m != nil {
		t.Fatalf("OweMediation = %v, owed %+v; want a refusal and nothing owed", err, m)
	}
	if !hasRecord(log, "error", "daemon.wsm.owe_mediation", "refused a mediation that names no deploy instant") {
		t.Fatalf("no error record of the refusal")
	}
}

// TestTheMediationChainCountsRemediationsInARow covers the chain: it begins
// where nothing was owed, passes to every newer deploy, counts one
// remediation for each replaced mediation its mediator was told of, and ends
// with the release.
func TestTheMediationChainCountsRemediationsInARow(t *testing.T) {
	second, third := instant.Add(time.Minute), instant.Add(2*time.Minute)
	tests := []struct {
		name          string
		arrange       func(t *testing.T, s *store, repo RepoKey)
		wantStart     time.Time
		wantRedeploys int
	}{
		{name: "a first deploy begins its chain", wantStart: instant,
			arrange: func(t *testing.T, s *store, repo RepoKey) { oweAt(t, s, repo, instant) }},
		{name: "a deploy replacing a dispatched mediation counts a remediation", wantStart: instant, wantRedeploys: 2,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				dispatchAt(t, s, repo, instant)
				fixDeploysAt(t, s, repo, second)
				dispatchAt(t, s, repo, second)
				fixDeploysAt(t, s, repo, third)
			}},
		{name: "a deploy that is not the dispatched fix's counts none", wantStart: instant,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				dispatchAt(t, s, repo, instant)
				oweAt(t, s, repo, second)
			}},
		{name: "a deploy replacing a told mediation nothing was dispatched for counts none", wantStart: instant,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				tellAt(t, s, repo, instant)
				oweAt(t, s, repo, second)
			}},
		{name: "a release ends the chain, so the next deploy begins another", wantStart: third,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				tellAt(t, s, repo, instant)
				if _, err := s.ReleaseMediation(context.Background(), repo, instant); err != nil {
					t.Fatal(err)
				}
				oweAt(t, s, repo, third)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			tt.arrange(t, s, repo)

			// Assert
			m, err := s.MediationOf(context.Background(), repo)
			if err != nil || m == nil || !m.ChainStartedAt.Equal(tt.wantStart) || m.ChainRedeploys != tt.wantRedeploys {
				t.Fatalf("MediationOf = (%+v, %v), want a chain from %v with %d redeploys", m, err, tt.wantStart, tt.wantRedeploys)
			}
		})
	}
}

// TestAMediationOwedWithoutAChainIsChainedByTheStore covers layout 135's
// triggers: a build from before the chain owes a mediation without one, and
// the store begins its chain at its own deploy.
func TestAMediationOwedWithoutAChainIsChainedByTheStore(t *testing.T) {
	tests := []struct {
		name  string
		write string
	}{
		{name: "an insert", write: `INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at, mediation_prompted_at, mediation_version, mediation_turn) VALUES (?, 0, ?, NULL, '', '')`},
		{name: "an update", write: `INSERT INTO merge_queue_repos (repo_key, paused) VALUES (?, 0) ON CONFLICT(repo_key) DO NOTHING; UPDATE merge_queue_repos SET mediation_owed_at = ?2 WHERE repo_key = ?1`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			key, err := normalizeRepoKey(repo)
			if err != nil {
				t.Fatal(err)
			}

			// Act: the build before the chain's write.
			if _, err := s.db().Exec(tt.write, string(key), nanos(instant)); err != nil {
				t.Fatalf("the earlier build's write: %v", err)
			}

			// Assert
			m, err := s.MediationOf(context.Background(), repo)
			if err != nil || m == nil || !m.ChainStartedAt.Equal(instant) || m.ChainRedeploys != 0 {
				t.Fatalf("MediationOf = (%+v, %v), want a chain begun at its own deploy", m, err)
			}
		})
	}
}

// TestTheMigrationChainsAMediationAlreadyOwed covers layout 135's backfill.
func TestTheMigrationChainsAMediationAlreadyOwed(t *testing.T) {
	// Arrange: a layout-134 file owing a mediation.
	path := fixtureAt(t, 134)
	execRaw(t, path, `INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at) VALUES ('/tmp/repo-1', 0, 1000)`)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	defer handle.Close()

	// Assert
	if got := rawScalar[int](t, path, `SELECT mediation_chain_started_at FROM merge_queue_repos WHERE repo_key = '/tmp/repo-1'`); got != 1000 {
		t.Fatalf("chain started at %d, want the owed deploy's 1000", got)
	}
}

// oweAt makes REPO owe the mediation of the deploy at AT.
func oweAt(t *testing.T, s *store, repo RepoKey, at time.Time) {
	t.Helper()
	if _, err := s.OweMediation(context.Background(), repo, at, ""); err != nil {
		t.Fatalf("OweMediation: %v", err)
	}
}

// dispatchAt records a fix for REPO's mediation owed at OWED dispatched to
// workspace "remedy".
func dispatchAt(t *testing.T, s *store, repo RepoKey, owed time.Time) {
	t.Helper()
	if ok, err := s.DispatchMediation(context.Background(), repo, owed, owed.Add(time.Second), "remedy"); err != nil || !ok {
		t.Fatalf("DispatchMediation = (%v, %v), want it recorded", ok, err)
	}
}

// fixDeploysAt makes the deploy at AT the fix's: remedy's finish lands just
// before it, so its deploy replaces the dispatched mediation.
func fixDeploysAt(t *testing.T, s *store, repo RepoKey, at time.Time) {
	t.Helper()
	ctx := context.Background()
	landed := at.Add(-time.Second)
	if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: "remedy", Repo: repo, StartedAt: landed}); err != nil {
		t.Fatalf("BeginRemediationFinish: %v", err)
	}
	if _, err := s.AdvanceRemediationFinish(ctx, "remedy", RemediationGating, RemediationLanding, landed); err != nil {
		t.Fatalf("AdvanceRemediationFinish: %v", err)
	}
	if _, err := s.RecordRemediationLanded(ctx, "remedy", "abc123", nil, landed); err != nil {
		t.Fatalf("RecordRemediationLanded: %v", err)
	}
	oweAt(t, s, repo, at)
	if _, err := s.AdvanceRemediationFinish(ctx, "remedy", RemediationDeploying, RemediationDeployed, at); err != nil {
		t.Fatalf("AdvanceRemediationFinish: %v", err)
	}
}

// TestTheChainsRolesPassAlongItAndEndWithIt covers the chain's intake and
// remediation workspaces: recorded once, carried to each newer deploy of the
// chain, the dispatch owed afresh for each deploy, and all cleared by the
// release.
func TestTheChainsRolesPassAlongItAndEndWithIt(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, repo RepoKey)
		want    *Mediation
	}{
		{name: "the intake and the dispatch are recorded on the owed mediation",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				if ok, err := s.RecordMediationIntake(context.Background(), repo, instant, "intake"); err != nil || !ok {
					t.Fatalf("RecordMediationIntake = (%v, %v)", ok, err)
				}
				dispatchAt(t, s, repo, instant)
			},
			want: &Mediation{Intake: "intake", Remediation: "remedy", Dispatched: true}},
		{name: "the fix's deploy carries the roles and owes its own dispatch",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				if _, err := s.RecordMediationIntake(context.Background(), repo, instant, "intake"); err != nil {
					t.Fatal(err)
				}
				dispatchAt(t, s, repo, instant)
				fixDeploysAt(t, s, repo, instant.Add(time.Minute))
			},
			want: &Mediation{Intake: "intake", Remediation: "remedy", Dispatched: false}},
		{name: "a deploy that is not the fix's carries the roles and the dispatch",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				if _, err := s.RecordMediationIntake(context.Background(), repo, instant, "intake"); err != nil {
					t.Fatal(err)
				}
				dispatchAt(t, s, repo, instant)
				oweAt(t, s, repo, instant.Add(time.Minute))
			},
			want: &Mediation{Intake: "intake", Remediation: "remedy", Dispatched: true}},
		{name: "the release clears the roles", want: nil,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				if _, err := s.RecordMediationIntake(context.Background(), repo, instant, "intake"); err != nil {
					t.Fatal(err)
				}
				if _, err := s.ReleaseMediation(context.Background(), repo, instant); err != nil {
					t.Fatal(err)
				}
				oweAt(t, s, repo, instant.Add(time.Minute))
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())

			// Act
			tt.arrange(t, s, repo)

			// Assert: the roles, or none after a release (the next deploy
			// begins a new chain with none).
			m, err := s.MediationOf(context.Background(), repo)
			want := tt.want
			if want == nil {
				want = &Mediation{}
			}
			if err != nil || m == nil || m.Intake != want.Intake || m.Remediation != want.Remediation || m.Dispatched != want.Dispatched {
				t.Fatalf("MediationOf = (%+v, %v), want roles %+v", m, err, want)
			}
		})
	}
}

// TestTheChainsRoleWritesRefuseWhatTheyCannotRecord covers the writes'
// refusals: a stale mediation, a second intake, a second dispatch, and no
// workspace.
func TestTheChainsRoleWritesRefuseWhatTheyCannotRecord(t *testing.T) {
	tests := []struct {
		name      string
		write     func(s *store, repo RepoKey) (bool, error)
		wantOK    bool
		wantError bool
	}{
		{name: "an intake for a mediation no longer owed",
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.RecordMediationIntake(context.Background(), repo, instant.Add(-time.Minute), "intake")
			}},
		{name: "a second intake for the chain",
			write: func(s *store, repo RepoKey) (bool, error) {
				if _, err := s.RecordMediationIntake(context.Background(), repo, instant, "first"); err != nil {
					return false, err
				}
				return s.RecordMediationIntake(context.Background(), repo, instant, "second")
			}},
		{name: "a second dispatch for one deploy",
			write: func(s *store, repo RepoKey) (bool, error) {
				if _, err := s.DispatchMediation(context.Background(), repo, instant, instant, "first"); err != nil {
					return false, err
				}
				return s.DispatchMediation(context.Background(), repo, instant, instant, "second")
			}},
		{name: "an intake that names no workspace", wantError: true,
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.RecordMediationIntake(context.Background(), repo, instant, "")
			}},
		{name: "a dispatch that names no workspace", wantError: true,
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.DispatchMediation(context.Background(), repo, instant, instant, "")
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)

			// Act
			ok, err := tt.write(s, repo)

			// Assert
			if ok != tt.wantOK || (err != nil) != tt.wantError {
				t.Fatalf("write = (%v, %v), want ok %v, error %v", ok, err, tt.wantOK, tt.wantError)
			}
		})
	}
}

// tellAt marks REPO's mediation owed at OWED told.
func tellAt(t *testing.T, s *store, repo RepoKey, owed time.Time) {
	t.Helper()
	if _, err := s.MarkMediationPrompted(context.Background(), repo, owed, owed.Add(time.Second), "turn"); err != nil {
		t.Fatalf("MarkMediationPrompted: %v", err)
	}
}

func TestMediationRoleOfAnswersTheChainAWorkspaceWorksOn(t *testing.T) {
	tests := []struct {
		name       string
		ws         WorkspaceID
		wantRole   bool
		wantIntake bool
	}{
		{name: "the chain's intake", ws: "intake", wantRole: true, wantIntake: true},
		{name: "the chain's last remediation", ws: "remedy", wantRole: true},
		{name: "a workspace with no role", ws: "bystander"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			if _, err := s.RecordMediationIntake(ctx, repo, instant, "intake"); err != nil {
				t.Fatal(err)
			}
			dispatchAt(t, s, repo, instant)

			// Act
			role, err := s.MediationRoleOf(ctx, tt.ws)

			// Assert
			if err != nil || (role != nil) != tt.wantRole {
				t.Fatalf("MediationRoleOf = (%+v, %v), want a role: %v", role, err, tt.wantRole)
			}
			if role != nil && (role.Intake != tt.wantIntake || !role.Mediation.OwedAt.Equal(instant) || !role.Mediation.Dispatched) {
				t.Fatalf("role = %+v, want intake %v on the dispatched mediation", role, tt.wantIntake)
			}
		})
	}
}

func TestMediationRoleOfAnswersNoneOnceTheChainIsReleased(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)
	if _, err := s.RecordMediationIntake(ctx, repo, instant, "intake"); err != nil {
		t.Fatal(err)
	}
	if _, err := s.ReleaseMediation(ctx, repo, instant); err != nil {
		t.Fatal(err)
	}

	// Act
	role, err := s.MediationRoleOf(ctx, "intake")

	// Assert
	if err != nil || role != nil {
		t.Fatalf("MediationRoleOf = (%+v, %v), want none", role, err)
	}
}

func TestMediationRoleOfRefusesNoWorkspace(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.MediationRoleOf(context.Background(), "")

	// Assert
	if err == nil || !loggedOperation(log, "daemon.wsm.mediation_role_of", "error") {
		t.Fatalf("MediationRoleOf(\"\") = %v, want a refusal recorded at ERROR: %v", err, log.Records())
	}
}

func TestTheChainsLastDispatchInstantPassesAlongItAndEndsWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)
	dispatched := instant.Add(time.Second)
	if ok, err := s.DispatchMediation(ctx, repo, instant, dispatched, "remedy"); err != nil || !ok {
		t.Fatalf("DispatchMediation = (%v, %v)", ok, err)
	}

	// Act
	oweAt(t, s, repo, instant.Add(time.Minute))
	carried, err := s.MediationOf(ctx, repo)
	if _, relErr := s.ReleaseMediation(ctx, repo, instant.Add(time.Minute)); relErr != nil {
		t.Fatal(relErr)
	}
	oweAt(t, s, repo, instant.Add(time.Hour))
	fresh, freshErr := s.MediationOf(ctx, repo)

	// Assert
	if err != nil || carried.LastDispatchedAt == nil || !carried.LastDispatchedAt.Equal(dispatched) {
		t.Fatalf("the newer deploy's check = (%+v, %v), want the dispatch instant carried", carried, err)
	}
	if freshErr != nil || fresh.LastDispatchedAt != nil {
		t.Fatalf("the next chain's check = (%+v, %v), want no dispatch instant", fresh, freshErr)
	}
}

func TestAReplacedDispatchedMediationSaysWhetherItsDispatchCarriesOn(t *testing.T) {
	tests := []struct {
		name        string
		fixDeploys  bool
		wantCarried bool
	}{
		{name: "the fix's deploy ends the dispatch", fixDeploys: true},
		{name: "another deploy carries the dispatch on", wantCarried: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			dispatchAt(t, s, repo, instant)
			if tt.fixDeploys {
				landed := instant.Add(30 * time.Second)
				beginFinish(t, s, repo, landed, nil)
				if _, err := s.AdvanceRemediationFinish(ctx, "remedy", RemediationGating, RemediationLanding, landed); err != nil {
					t.Fatal(err)
				}
				if _, err := s.RecordRemediationLanded(ctx, "remedy", "abc123", nil, landed); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			replaced, err := s.OweMediation(ctx, repo, instant.Add(time.Minute), "")

			// Assert
			if err != nil || replaced == nil || !replaced.Dispatched || replaced.DispatchCarried != tt.wantCarried {
				t.Fatalf("OweMediation = (%+v, %v), want the dispatched mediation replaced, carried %v", replaced, err, tt.wantCarried)
			}
		})
	}
}
