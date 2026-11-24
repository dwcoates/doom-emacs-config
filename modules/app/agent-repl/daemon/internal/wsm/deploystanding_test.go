package wsm

import (
	"context"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// owingQueue answers a store with REPO's queue owing a check, so the queue
// has its row.
func owingQueue(t *testing.T) (*store, RepoKey) {
	t.Helper()
	s, _ := testStore(t)
	repo := RepoKey(t.TempDir())
	if _, err := s.OweMediation(context.Background(), repo, instant, ""); err != nil {
		t.Fatalf("OweMediation: %v", err)
	}
	return s, repo
}

func TestADeployMediatorRoundTripsUnbriefed(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()

	// Act
	if err := s.RecordDeployMediator(ctx, repo, "ws-mediator"); err != nil {
		t.Fatalf("RecordDeployMediator: %v", err)
	}
	got, found, err := s.DeployMediatorOf(ctx, repo)

	// Assert
	if err != nil || !found || got != (DeployMediator{Workspace: "ws-mediator"}) {
		t.Fatalf("DeployMediatorOf = (%+v, %v, %v), want ws-mediator unbriefed", got, found, err)
	}
}

func TestAQueueWithNoDeployMediatorAnswersNone(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)

	// Act
	_, found, err := s.DeployMediatorOf(context.Background(), repo)

	// Assert
	if err != nil || found {
		t.Fatalf("DeployMediatorOf = (%v, %v), want none", found, err)
	}
}

func TestMarkMediatorBriefedMarksTheQueuesMediator(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	if err := s.RecordDeployMediator(ctx, repo, "ws-r"); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.MarkMediatorBriefed(ctx, repo)

	// Assert
	got, _, readErr := s.DeployMediatorOf(ctx, repo)
	if err != nil || readErr != nil || !got.Briefed {
		t.Fatalf("MarkMediatorBriefed = %v, then %+v (%v), want it briefed", err, got, readErr)
	}
}

func TestMarkMediatorBriefedRefusesAQueueWithNoMediator(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)

	// Act
	err := s.MarkMediatorBriefed(context.Background(), repo)

	// Assert
	if err == nil {
		t.Fatal("MarkMediatorBriefed = nil, want the missing mediator refused")
	}
}

func TestReplacingADeployMediatorLeavesTheNewOneUnbriefed(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	if err := s.RecordDeployMediator(ctx, repo, "ws-old"); err != nil {
		t.Fatal(err)
	}
	if err := s.MarkMediatorBriefed(ctx, repo); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.RecordDeployMediator(ctx, repo, "ws-new")

	// Assert
	got, _, readErr := s.DeployMediatorOf(ctx, repo)
	if err != nil || readErr != nil || got != (DeployMediator{Workspace: "ws-new"}) {
		t.Fatalf("after the replace %+v (%v, %v), want ws-new unbriefed", got, err, readErr)
	}
}

func TestADeployMediatorWithNoWorkspaceIsRefused(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	repo := RepoKey(t.TempDir())
	ctx := context.Background()

	// Act
	err := s.RecordDeployMediator(ctx, repo, "")

	// Assert
	_, found, _ := s.DeployMediatorOf(ctx, repo)
	if err == nil || found {
		t.Fatalf("RecordDeployMediator = %v, recorded %v; want it refused and nothing written", err, found)
	}
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.record_deploy_mediator"}); !ok {
		t.Fatalf("no ERROR record of the refusal: %+v", logs.Records())
	}
}

func TestMediatorRepoOf(t *testing.T) {
	tests := []struct {
		name      string
		ws        WorkspaceID
		wantFound bool
	}{
		{name: "the mediator answers its queue", ws: "ws-mediator", wantFound: true},
		{name: "another workspace answers none", ws: "ws-other"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, repo := owingQueue(t)
			ctx := context.Background()
			if err := s.RecordDeployMediator(ctx, repo, "ws-mediator"); err != nil {
				t.Fatal(err)
			}

			// Act
			got, found, err := s.MediatorRepoOf(ctx, tt.ws)

			// Assert
			want := RepoKey("")
			if tt.wantFound {
				want, _ = normalizeRepoKey(repo)
			}
			if err != nil || found != tt.wantFound || got != want {
				t.Fatalf("MediatorRepoOf = (%q, %v, %v), want (%q, %v)", got, found, err, want, tt.wantFound)
			}
		})
	}
}

func TestOweCompaction(t *testing.T) {
	tests := []struct {
		name      string
		vendor    string
		owedFirst *OwedStartWork
		want      bool
		wantOwed  OwedStartWork
	}{
		{name: "a workspace with a conversation owes it", vendor: "v-1", want: true, wantOwed: OwedStartWork{Compact: true}},
		{name: "a workspace with no conversation owes nothing", vendor: ""},
		{name: "a workspace owing a summary keeps it", vendor: "v-1", owedFirst: &OwedStartWork{SummaryOf: "v-0"}, wantOwed: OwedStartWork{SummaryOf: "v-0"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			ws := testWorkspace(t, s)
			sess := Session{Workspace: ws.ID, HostSessionID: "h", VendorSessionID: tt.vendor, StartedAt: instant}
			if err := s.BindSession(ctx, sess, tt.owedFirst); err != nil {
				t.Fatal(err)
			}

			// Act
			owed, err := s.OweCompaction(ctx, ws.ID)

			// Assert
			got, found, readErr := s.OwedStartWork(ctx, ws.ID)
			if err != nil || readErr != nil || owed != tt.want || got != tt.wantOwed || found != (tt.wantOwed != OwedStartWork{}) {
				t.Fatalf("OweCompaction = (%v, %v), owed (%+v, %v, %v); want (%v), owed %+v", owed, err, got, found, readErr, tt.want, tt.wantOwed)
			}
		})
	}
}

func TestAWorkspaceReadsWhetherItIsADeployMediator(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	standing, other := testWorkspace(t, s), testWorkspace(t, s)
	if err := s.RecordDeployMediator(ctx, repo, standing.ID); err != nil {
		t.Fatal(err)
	}

	// Act
	gotStanding, err1 := s.Workspace(ctx, standing.ID)
	gotOther, err2 := s.Workspace(ctx, other.ID)

	// Assert
	if err1 != nil || err2 != nil || !gotStanding.DeployMediator || gotOther.DeployMediator {
		t.Fatalf("DeployMediator = (%v, %v), errors (%v, %v); want only the recorded one", gotStanding.DeployMediator, gotOther.DeployMediator, err1, err2)
	}
}

func TestRecordMediationEnding(t *testing.T) {
	tests := []struct {
		name    string
		owed    time.Time
		ending  DeployEnding
		want    bool
		wantErr bool
		stored  DeployEnding
	}{
		{name: "the owed check takes its deploy's ending", owed: instant, ending: DeployStoppedShort, want: true, stored: DeployStoppedShort},
		{name: "an older deploy's ending is not recorded on a newer check", owed: instant.Add(-time.Minute), ending: DeployFinished},
		{name: "an ending that is none of the three is refused", owed: instant, ending: "exploded", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, repo := owingQueue(t)
			ctx := context.Background()

			// Act
			got, err := s.RecordMediationEnding(ctx, repo, tt.owed, tt.ending)

			// Assert
			m, readErr := s.MediationOf(ctx, repo)
			if (err != nil) != tt.wantErr || got != tt.want || readErr != nil || m == nil || m.DeployEnding != tt.stored {
				t.Fatalf("RecordMediationEnding = (%v, %v), stored %+v (%v); want (%v), %q stored", got, err, m, readErr, tt.want, tt.stored)
			}
		})
	}
}

func TestANewerDeployForgetsTheOlderOnesEnding(t *testing.T) {
	// Arrange
	s, repo := owingQueue(t)
	ctx := context.Background()
	if _, err := s.RecordMediationEnding(ctx, repo, instant, DeployStoppedShort); err != nil {
		t.Fatal(err)
	}

	// Act
	if _, err := s.OweMediation(ctx, repo, instant.Add(time.Minute), ""); err != nil {
		t.Fatal(err)
	}

	// Assert
	if m, err := s.MediationOf(ctx, repo); err != nil || m == nil || m.DeployEnding != "" {
		t.Fatalf("MediationOf = (%+v, %v), want the newer check with no ending yet", m, err)
	}
}

// checkedFixture answers a file at layout 167 whose queue owes a dispatched,
// escalated check with its intake, fixer and escalation workspaces, a quiet
// run, a remediation finish and a problem, as the deploy checks before the
// mediator left it.
func checkedFixture(t *testing.T) string {
	t.Helper()
	path := fixtureAt(t, 167)
	execRaw(t, path, `INSERT INTO merge_queue_repos (repo_key, paused, mediation_owed_at, mediation_prompted_at, mediation_turn, mediation_chain_started_at,
		mediation_chain_redeploys, mediation_intake, mediation_remediation, mediation_dispatched, mediation_dispatched_at, mediation_escalation,
		mediation_escalation_brief, mediation_fixer_turn, mediation_fixer_brief, mediation_fixer_origin, checked_through, deploy_quiet_checks, deploy_compaction_armed)
		VALUES ('/tmp/repo', 0, 5, 6, 'turn', 5, 2, 'intake', 'fixer', 1, 7, 'fixer', '{"summary":"s"}', 'turn-fix', 'fix it', 1, 8, 2, 1)`)
	execRaw(t, path, `INSERT INTO deploy_check_standing (repo_key, role, workspace_id, briefed) VALUES ('/tmp/repo', 'fixer', 'fixer', 1)`)
	execRaw(t, path, `INSERT INTO deploy_remediation_finishes (workspace_id, repo_key, step, step_at, started_at) VALUES ('fixer', '/tmp/repo', 'deploying', 1, 1)`)
	execRaw(t, path, `INSERT INTO deploy_problems (repo_key, operation, message, owner, opened_at) VALUES ('/tmp/repo', 'op', 'm', 'fixer', 1)`)
	return path
}

func TestTheMediatorLayoutRestartsEveryDeployCheck(t *testing.T) {
	tests := []struct {
		name  string
		query string
		want  int
	}{
		{name: "the owed check is released", query: `SELECT count(*) FROM merge_queue_repos WHERE mediation_owed_at IS NOT NULL OR mediation_chain_started_at IS NOT NULL`},
		{name: "the dispatch is cleared", query: `SELECT count(*) FROM merge_queue_repos WHERE mediation_dispatched <> 0 OR mediation_dispatched_at IS NOT NULL OR mediation_fixer_turn <> ''`},
		{name: "the escalation is cleared", query: `SELECT count(*) FROM merge_queue_repos WHERE mediation_escalation_brief <> '' OR mediation_escalation_told <> 0`},
		{name: "the logs read so far are forgotten", query: `SELECT count(*) FROM merge_queue_repos WHERE checked_through IS NOT NULL`},
		{name: "the quiet run and the compaction arming start over", query: `SELECT count(*) FROM merge_queue_repos WHERE deploy_quiet_checks <> 0 OR deploy_compaction_armed <> 0`},
		{name: "every remediation finish ends", query: `SELECT count(*) FROM deploy_remediation_finishes`},
		{name: "no queue has a mediator", query: `SELECT count(*) FROM deploy_mediators`},
		{name: "the mediator has taken on no problem", query: `SELECT count(*) FROM deploy_mediator_problems`},
		{name: "the queue itself is kept", query: `SELECT count(*) FROM merge_queue_repos WHERE repo_key = '/tmp/repo'`, want: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := checkedFixture(t)

			// Act
			execRaw(t, path, deployMediatorsDDL)

			// Assert
			if got := rawScalar[int](t, path, tt.query); got != tt.want {
				t.Fatalf("%s = %d, want %d", tt.query, got, tt.want)
			}
		})
	}
}

func TestTheRetiredDeployChecksContractDropsTheirShapes(t *testing.T) {
	tests := []struct {
		name  string
		query string
	}{
		{name: "the standing roles' table", query: `SELECT count(*) FROM sqlite_master WHERE name = 'deploy_check_standing'`},
		{name: "the escalation workspaces' table", query: `SELECT count(*) FROM sqlite_master WHERE name = 'deploy_escalations'`},
		{name: "the owned problems' table", query: `SELECT count(*) FROM sqlite_master WHERE name = 'deploy_problems'`},
		{name: "the chain's workspace columns", query: `SELECT count(*) FROM pragma_table_info('merge_queue_repos') WHERE name IN ('mediation_intake', 'mediation_remediation', 'mediation_escalation')`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := checkedFixture(t)
			execRaw(t, path, deployMediatorsDDL)

			// Act
			execRaw(t, path, retiredDeployChecksDDL)

			// Assert
			if got := rawScalar[int](t, path, tt.query); got != 0 {
				t.Fatalf("%s = %d, want it dropped", tt.query, got)
			}
		})
	}
}

func TestTheQuietCompactionContractDropsItsColumns(t *testing.T) {
	tests := []struct {
		name   string
		column string
	}{
		{name: "the quiet run's counter", column: "deploy_quiet_checks"},
		{name: "the compaction's arming", column: "deploy_compaction_armed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			path := fixtureAt(t, 177)
			query := `SELECT count(*) FROM pragma_table_info('merge_queue_repos') WHERE name = '` + tt.column + `'`
			if got := rawScalar[int](t, path, query); got != 1 {
				t.Fatalf("%s before the contract = %d, want 1", tt.column, got)
			}

			// Act
			execRaw(t, path, retiredQuietCompactionDDL)

			// Assert
			if got := rawScalar[int](t, path, query); got != 0 {
				t.Fatalf("%s after the contract = %d, want 0", tt.column, got)
			}
		})
	}
}
