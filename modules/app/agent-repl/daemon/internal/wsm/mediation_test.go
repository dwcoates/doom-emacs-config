package wsm

import (
	"context"
	"fmt"
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
			if err != nil || rerr != nil || released.Released == tt.wantOwed || (m != nil) != tt.wantOwed {
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

	// Act: layout 135 alone, since layout 168 releases every owed check.
	execRaw(t, path, mediationChainDDL)

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

// dispatchAt records a fix the mediator dispatched to itself for REPO's
// mediation owed at OWED.
func dispatchAt(t *testing.T, s *store, repo RepoKey, owed time.Time) {
	t.Helper()
	if ok, err := s.DispatchMediation(context.Background(), repo, owed, owed.Add(time.Second), aFixerTurn); err != nil || !ok {
		t.Fatalf("DispatchMediation = (%v, %v), want it recorded", ok, err)
	}
}

// finishDeploying lands WS's direct finish at LANDED, leaving it deploying.
func finishDeploying(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, landed time.Time) {
	t.Helper()
	ctx := context.Background()
	if _, err := s.BeginRemediationFinish(ctx, RemediationFinish{Workspace: ws, Repo: repo, StartedAt: landed}); err != nil {
		t.Fatalf("BeginRemediationFinish: %v", err)
	}
	if _, err := s.AdvanceRemediationFinish(ctx, ws, RemediationGating, RemediationLanding, landed); err != nil {
		t.Fatalf("AdvanceRemediationFinish: %v", err)
	}
	if _, err := s.RecordRemediationLanded(ctx, ws, "abc123", nil, landed); err != nil {
		t.Fatalf("RecordRemediationLanded: %v", err)
	}
}

// fixDeploysAt makes the deploy at AT the fix's: remedy's finish lands just
// before it and that deploy covers the landing, so its deploy replaces the
// dispatched mediation and moves the finish to deployed in the same write.
func fixDeploysAt(t *testing.T, s *store, repo RepoKey, at time.Time) {
	t.Helper()
	finishDeploying(t, s, repo, "remedy", at.Add(-time.Second))
	coverLandings(t, s)
	oweAt(t, s, repo, at)
}

// coverLandings covers every landing still deploying, as a deploy's commit
// does.
func coverLandings(t *testing.T, s *store) {
	t.Helper()
	if _, err := s.CoverDeployingLandings(context.Background()); err != nil {
		t.Fatalf("CoverDeployingLandings: %v", err)
	}
}

// TestTheChainsDispatchPassesAlongItAndEndsWithIt covers the mediator's
// dispatch: recorded on the owed check, carried to each newer deploy of the
// chain but the fix's own, and cleared by the release.
func TestTheChainsDispatchPassesAlongItAndEndsWithIt(t *testing.T) {
	tests := []struct {
		name           string
		arrange        func(t *testing.T, s *store, repo RepoKey)
		wantDispatched bool
	}{
		{name: "the dispatch is recorded on the owed check", wantDispatched: true,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				dispatchAt(t, s, repo, instant)
			}},
		{name: "the fix's deploy owes its own dispatch",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				dispatchAt(t, s, repo, instant)
				fixDeploysAt(t, s, repo, instant.Add(time.Minute))
			}},
		{name: "a deploy that is not the fix's carries the dispatch", wantDispatched: true,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				dispatchAt(t, s, repo, instant)
				oweAt(t, s, repo, instant.Add(time.Minute))
			}},
		{name: "the release clears the dispatch",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				oweAt(t, s, repo, instant)
				dispatchAt(t, s, repo, instant)
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

			// Assert
			m, err := s.MediationOf(context.Background(), repo)
			if err != nil || m == nil || m.Dispatched != tt.wantDispatched {
				t.Fatalf("MediationOf = (%+v, %v), want dispatched %v", m, err, tt.wantDispatched)
			}
		})
	}
}

func TestASecondDispatchOrOneForACheckNoLongerOwedIsNotRecorded(t *testing.T) {
	tests := []struct {
		name  string
		write func(s *store, repo RepoKey) (bool, error)
	}{
		{name: "a dispatch for a check no longer owed",
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.DispatchMediation(context.Background(), repo, instant.Add(-time.Minute), instant, aFixerTurn)
			}},
		{name: "a second dispatch for one deploy",
			write: func(s *store, repo RepoKey) (bool, error) {
				if _, err := s.DispatchMediation(context.Background(), repo, instant, instant, aFixerTurn); err != nil {
					return false, err
				}
				return s.DispatchMediation(context.Background(), repo, instant, instant, aFixerTurn)
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
			if ok || err != nil {
				t.Fatalf("write = (%v, %v), want nothing recorded and no error", ok, err)
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

func TestTheChainsLastDispatchInstantPassesAlongItAndEndsWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)
	dispatched := instant.Add(time.Second)
	if ok, err := s.DispatchMediation(ctx, repo, instant, dispatched, aFixerTurn); err != nil || !ok {
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
				coverLandings(t, s)
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

func TestAnIntakeIsResumedOnceOnTheMediationOwed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, repo RepoKey)
		owed    time.Time
		want    bool
	}{
		{name: "the mediation owed, never resumed", owed: instant, want: true},
		{name: "a mediation a newer deploy replaced", owed: instant.Add(-time.Minute)},
		{name: "a mediation resumed already", owed: instant,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				if ok, err := s.MarkMediationResumed(context.Background(), repo, instant, "turn-0"); err != nil || !ok {
					t.Fatalf("MarkMediationResumed = (%v, %v)", ok, err)
				}
			}},
		{name: "a mediation a fix was dispatched for", owed: instant,
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				if ok, err := s.DispatchMediation(context.Background(), repo, instant, instant, aFixerTurn); err != nil || !ok {
					t.Fatalf("DispatchMediation = (%v, %v)", ok, err)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			if tt.arrange != nil {
				tt.arrange(t, s, repo)
			}
			before, err := s.MediationOf(ctx, repo)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			marked, err := s.MarkMediationResumed(ctx, repo, tt.owed, "turn-2")

			// Assert
			m, rerr := s.MediationOf(ctx, repo)
			if err != nil || rerr != nil || marked != tt.want {
				t.Fatalf("MarkMediationResumed = (%v, %v), read %v; want %v", marked, err, rerr, tt.want)
			}
			if tt.want && (!m.Resumed || m.Turn != "turn-2") {
				t.Fatalf("MediationOf = %+v, want it resumed in turn-2", m)
			}
			if !tt.want && (m.Resumed != before.Resumed || m.Turn != before.Turn) {
				t.Fatalf("MediationOf = %+v, want it unchanged from %+v", m, before)
			}
		})
	}
}

func TestAResumedMarkNeverReadsAsALaterMediations(t *testing.T) {
	tests := []struct {
		name  string
		later func(t *testing.T, s *store, repo RepoKey)
	}{
		{name: "a newer deploy replaces the resumed mediation",
			later: func(t *testing.T, s *store, repo RepoKey) { oweAt(t, s, repo, instant.Add(time.Minute)) }},
		{name: "the resumed mediation is released and another owed",
			later: func(t *testing.T, s *store, repo RepoKey) {
				if _, err := s.ReleaseMediation(context.Background(), repo, instant); err != nil {
					t.Fatal(err)
				}
				oweAt(t, s, repo, instant.Add(time.Minute))
			}},
		{name: "a build that knows no mark owes a newer deploy over it",
			later: func(t *testing.T, s *store, repo RepoKey) {
				key, err := normalizeRepoKey(repo)
				if err != nil {
					t.Fatal(err)
				}
				if _, err := s.db().Exec(`UPDATE merge_queue_repos SET mediation_owed_at = ? WHERE repo_key = ?`, nanos(instant.Add(time.Minute)), key); err != nil {
					t.Fatal(err)
				}
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)
			if ok, err := s.MarkMediationResumed(ctx, repo, instant, "turn-2"); err != nil || !ok {
				t.Fatalf("MarkMediationResumed = (%v, %v)", ok, err)
			}

			// Act
			tt.later(t, s, repo)
			m, err := s.MediationOf(ctx, repo)

			// Assert
			if err != nil || m == nil || m.Resumed {
				t.Fatalf("MediationOf = (%+v, %v), want the later mediation not resumed", m, err)
			}
		})
	}
}

func TestResumingAnIntakeInNoTurnIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	oweAt(t, s, repo, instant)

	// Act
	marked, err := s.MarkMediationResumed(ctx, repo, instant, "")

	// Assert
	m, rerr := s.MediationOf(ctx, repo)
	if err == nil || marked || !loggedOperation(log, "daemon.wsm.mark_mediation_resumed", "error") {
		t.Fatalf("MarkMediationResumed = (%v, %v), want a refusal recorded at ERROR: %v", marked, err, log.Records())
	}
	if rerr != nil || m.Resumed {
		t.Fatalf("MediationOf = (%+v, %v), want it not resumed", m, rerr)
	}
}

// escalateAt escalates the mediation REPO owes at OWED, marking the mediator
// and the user told when TOLD.
func escalateAt(t *testing.T, s *store, repo RepoKey, owed time.Time, told bool) {
	t.Helper()
	if ok, err := s.EscalateMediation(context.Background(), repo, owed, `{"summary":"s"}`); err != nil || !ok {
		t.Fatalf("EscalateMediation = (%v, %v)", ok, err)
	}
	if !told {
		return
	}
	if ok, err := s.MarkEscalationTold(context.Background(), repo, owed); err != nil || !ok {
		t.Fatalf("MarkEscalationTold = (%v, %v)", ok, err)
	}
}

func TestAnEscalatedMediationHoldsThroughEveryDeployButItsFixs(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, repo RepoKey)
		want    Mediation
	}{
		{name: "escalated and told",
			arrange: func(t *testing.T, s *store, repo RepoKey) { escalateAt(t, s, repo, instant, true) },
			want:    Mediation{EscalationBrief: `{"summary":"s"}`, EscalationTold: true}},
		{name: "an escalation ends a dispatch it carried",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				dispatchAt(t, s, repo, instant)
				escalateAt(t, s, repo, instant, true)
			},
			want: Mediation{EscalationBrief: `{"summary":"s"}`, EscalationTold: true}},
		{name: "another deploy carries the told escalation",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				escalateAt(t, s, repo, instant, true)
				oweAt(t, s, repo, instant.Add(time.Minute))
			},
			want: Mediation{EscalationBrief: `{"summary":"s"}`, EscalationTold: true}},
		{name: "an escalation not yet told is carried untold",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				escalateAt(t, s, repo, instant, false)
				oweAt(t, s, repo, instant.Add(time.Minute))
			},
			want: Mediation{EscalationBrief: `{"summary":"s"}`}},
		{name: "the mediator's own fix deploying ends the hold",
			arrange: func(t *testing.T, s *store, repo RepoKey) {
				escalateAt(t, s, repo, instant, true)
				finishDeploying(t, s, repo, "mediator", instant.Add(30*time.Second))
				coverLandings(t, s)
				oweAt(t, s, repo, instant.Add(time.Minute))
			},
			want: Mediation{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)

			// Act
			tt.arrange(t, s, repo)

			// Assert
			m, err := s.MediationOf(context.Background(), repo)
			if err != nil || m == nil || m.EscalationBrief != tt.want.EscalationBrief || m.EscalationTold != tt.want.EscalationTold ||
				m.Escalated() != (tt.want.EscalationBrief != "") || m.Dispatched {
				t.Fatalf("MediationOf = (%+v, %v), want %+v", m, err, tt.want)
			}
		})
	}
}

func TestTheEscalationWritesRefuseWhatTheyCannotRecord(t *testing.T) {
	tests := []struct {
		name     string
		write    func(s *store, repo RepoKey) (bool, error)
		wantErr  bool
		wantRead Mediation
	}{
		{name: "a second escalation",
			write: func(s *store, repo RepoKey) (bool, error) {
				if _, err := s.EscalateMediation(context.Background(), repo, instant, `{"summary":"s"}`); err != nil {
					return false, err
				}
				return s.EscalateMediation(context.Background(), repo, instant, `{"summary":"other"}`)
			},
			wantRead: Mediation{EscalationBrief: `{"summary":"s"}`}},
		{name: "an escalation of a mediation no longer owed",
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.EscalateMediation(context.Background(), repo, instant.Add(-time.Minute), `{"summary":"s"}`)
			}},
		{name: "an escalation with no brief", wantErr: true,
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.EscalateMediation(context.Background(), repo, instant, "")
			}},
		{name: "told on a check not escalated",
			write: func(s *store, repo RepoKey) (bool, error) {
				return s.MarkEscalationTold(context.Background(), repo, instant)
			}},
		{name: "told on an escalation no longer owed",
			write: func(s *store, repo RepoKey) (bool, error) {
				if _, err := s.EscalateMediation(context.Background(), repo, instant, `{"summary":"s"}`); err != nil {
					return false, err
				}
				return s.MarkEscalationTold(context.Background(), repo, instant.Add(-time.Minute))
			},
			wantRead: Mediation{EscalationBrief: `{"summary":"s"}`}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			repo := RepoKey(t.TempDir())
			oweAt(t, s, repo, instant)

			// Act
			wrote, err := tt.write(s, repo)

			// Assert
			m, rerr := s.MediationOf(context.Background(), repo)
			if wrote || (err != nil) != tt.wantErr || rerr != nil {
				t.Fatalf("write = (%v, %v), read %v; want nothing written, an error %v", wrote, err, rerr, tt.wantErr)
			}
			if m.EscalationTold || m.EscalationBrief != tt.wantRead.EscalationBrief {
				t.Fatalf("MediationOf = %+v, want %+v", m, tt.wantRead)
			}
		})
	}
}

func TestAnOwedCheckRecordsWhenItsLandingBegan(t *testing.T) {
	cases := []struct {
		name string
		// merges are the queue's merges: whether each is running, and how
		// long before the deploy its ledger opened.
		merges []runningMerge
		// want is how long before the deploy the landing began.
		want time.Duration
	}{
		{name: "a deploy no merge is running for began its own landing"},
		{name: "a landing began when its running merge began", merges: []runningMerge{{state: MergeAdmitted, opened: 20 * time.Minute}}, want: 20 * time.Minute},
		{name: "a merge being repaired is running", merges: []runningMerge{{state: MergeRemediating, opened: 30 * time.Minute}}, want: 30 * time.Minute},
		{
			name:   "two running merges began the landing at the earlier",
			merges: []runningMerge{{state: MergeAdmitted, opened: 10 * time.Minute}, {state: MergeAdmitted, opened: 40 * time.Minute}},
			want:   40 * time.Minute,
		},
		{name: "a merge only waiting in line began nothing", merges: []runningMerge{{state: MergeQueued, opened: 50 * time.Minute}}},
		{name: "a merge whose ledger opened after the deploy began nothing", merges: []runningMerge{{state: MergeAdmitted, opened: -time.Minute}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			repo := RepoKey(t.TempDir())
			key, err := normalizeRepoKey(repo)
			if err != nil {
				t.Fatal(err)
			}
			for i, merge := range tc.merges {
				ws := testWorkspace(t, s)
				lease := fmt.Sprintf("lease-%d", i)
				if _, err := s.db().ExecContext(ctx, `INSERT INTO merge_ledger (lease_id, workspace_id, opened_at) VALUES (?, ?, ?)`,
					lease, string(ws.ID), nanos(instant.Add(-merge.opened))); err != nil {
					t.Fatal(err)
				}
				if _, err := s.db().ExecContext(ctx, `INSERT INTO merge_queue (repo_key, workspace_id, seq, state, enqueued_at, ledger_id, source_pre_deploy_hooks, source_post_deploy_hooks)
					VALUES (?, ?, ?, ?, 1, ?, '[]', '[]')`, string(key), string(ws.ID), i+1, int(merge.state), lease); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			if _, err := s.OweMediation(ctx, repo, instant, ""); err != nil {
				t.Fatalf("OweMediation: %v", err)
			}
			m, err := s.MediationOf(ctx, repo)

			// Assert
			if err != nil || m == nil || !m.LandingStartedAt.Equal(instant.Add(-tc.want)) {
				t.Fatalf("MediationOf = (%+v, %v), want the landing begun %v before the deploy", m, err, tc.want)
			}
		})
	}
}

// runningMerge is one merge of a queue a landing's start is read from.
type runningMerge struct {
	state  MergeQueueState
	opened time.Duration
}
