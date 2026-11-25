package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"
	"time"

	"agent-repld/internal/dlog"
)

// THE QUEUE'S RACE FINDINGS (owner ruling, 2026-11-25). A test that failed in a
// merge gate and then passed, in the repair's rerun or a later gate, on the
// SAME inputs digest (testrun.v1.TestRunUnitEnded.inputs_digest) failed for no
// change of the code it ran: its failure is a race, not the branch's doing.
// The daemon decides it, never an agent.
//
// A gate's failure is kept as a CANDIDATE (queue_gate_failures), keyed by its
// test and digest, until a pass under the same digest promotes it to the
// test's one FINDING (queue_race_findings), or it ages out. A finding is put
// to the deploy mediator at every check until a dispatch takes it on; the
// dispatch's deploy marks it fixed, and a later promotion opens it again.
// Every promotion is a recurrence, and names the merge it failed in
// (queue_race_finding_merges).

// raceFindingsDDL is the layout-214 addition: the gate failures awaiting a
// pass, the race findings, and the merges each failed in, seeded with the
// races the merge queue showed from 2026-11-22 to 2026-11-25 in the
// repository at ~/.config/doom, keyed by its common dir, the one whose
// deploys are checked. IT IS ADDITIVE: three new tables the previous build
// never names.
const raceFindingsDDL = `
CREATE TABLE queue_gate_failures (
  repo_key      TEXT NOT NULL CHECK (repo_key <> ''),
  suite         TEXT NOT NULL CHECK (suite <> ''),
  test          TEXT NOT NULL CHECK (test <> ''),
  inputs_digest TEXT NOT NULL CHECK (inputs_digest <> ''),
  merge_name    TEXT NOT NULL CHECK (merge_name <> ''),
  failed_at     INTEGER NOT NULL CHECK (failed_at > 0),
  failure       TEXT NOT NULL DEFAULT '',
  artifacts     TEXT NOT NULL DEFAULT '',
  PRIMARY KEY (repo_key, suite, test, inputs_digest, merge_name)
);
CREATE TABLE queue_race_findings (
  repo_key     TEXT NOT NULL CHECK (repo_key <> ''),
  suite        TEXT NOT NULL CHECK (suite <> ''),
  test         TEXT NOT NULL CHECK (test <> ''),
  first_seen   INTEGER NOT NULL CHECK (first_seen > 0),
  last_seen    INTEGER NOT NULL CHECK (last_seen > 0),
  recurrences  INTEGER NOT NULL DEFAULT 1 CHECK (recurrences > 0),
  failure      TEXT NOT NULL DEFAULT '',
  artifacts    TEXT NOT NULL DEFAULT '',
  taken_on_at  INTEGER,
  fixed_at     INTEGER,
  fixes        INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (repo_key, suite, test)
);
CREATE TABLE queue_race_finding_merges (
  repo_key   TEXT NOT NULL,
  suite      TEXT NOT NULL,
  test       TEXT NOT NULL,
  merge_name TEXT NOT NULL CHECK (merge_name <> ''),
  failed_at  INTEGER NOT NULL CHECK (failed_at > 0),
  PRIMARY KEY (repo_key, suite, test, merge_name)
);
` + raceFindingSeedsDDL

// raceFindingSeedsDDL seeds the races seen before the daemon detected them,
// each in every merge it failed, into the repository whose key ends in
// /.config/doom/.git. A store holding no such repository is seeded with nothing.
const raceFindingSeedsDDL = `
INSERT INTO queue_race_findings (repo_key, suite, test, first_seen, last_seen, recurrences, failure, artifacts)
SELECT r.repo_key, s.suite, s.test, s.first_seen, s.last_seen, s.recurrences, s.failure, s.artifacts
FROM merge_queue_repos r, (
  SELECT 'daemon' AS suite, 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement' AS test,
    1795375140000000000 AS first_seen, 1795459800000000000 AS last_seen, 6 AS recurrences,
    'drain_rollout_test.go:1342: AdoptWebWorkspace for a client not open at announcement = error:{transferring_away:{address:"127.0.0.1:52348"}}, want error.participant_not_expected' AS failure,
    '~/.claude-emacs/merge-logs/a962667c42db45af-tests-1.log' AS artifacts
  UNION ALL SELECT 'e2e', 'TestLanding', 1795375140000000000, 1795625400000000000, 7,
    'mergequeue_e2e_test.go:427: waiting for the merge-queued park of the finished workspace (TestLanding/DisplacedTurnCapturedEndedThenResubmittedExactlyOnce) ran out',
    '~/.claude-emacs/merge-logs/dd0fb85a23e143b5-tests-1.log'
  UNION ALL SELECT 'daemon', 'TestALateFactsRefusalIsOfferedAgainMidWork', 1795375140000000000, 1795558140000000000, 5,
    'fault.go:198: harness: fault latch closed: the daemon wrote ERROR daemon.rollout.adopt: an unexpected daemon owns the workspace at the adoption boundary {cause=rollout: adopt: workspace is served by unexpected daemon}',
    '~/.claude-emacs/merge-logs/6be282b3db2a4851-tests-1.log'
  UNION ALL SELECT 'daemon', 'TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands', 1795375140000000000, 1795452180000000000, 5,
    'merge_shutdown_test.go:166: the resumed merge recorded an error: daemon.merge.progress: could not record the merge''s progress; the merge stops rather than take a step a restart could not resume {error=wsm: begin transaction: context canceled}',
    '~/.claude-emacs/merge-logs/a3c6b7deba3a4121-tests-1.log'
  UNION ALL SELECT 'daemon', 'TestQueryDiedDrawsTheTurnsTerminal', 1795380780000000000, 1795380780000000000, 1,
    'harness: fault latch closed: the daemon wrote ERROR daemon.cachelapse.drive: could not record whether the workspace owes keep-alives {cause=wsm: workspace: wsm: not found edge=the workspace was closed or reopened} (wsm not found after a close)',
    '~/.claude-emacs/merge-logs/62c406a30ad6479b-tests-3.log'
  UNION ALL SELECT 'daemon', 'TestRosterRowIsTurnDiedWhenTheQueryDiesUnderATurn', 1795488180000000000, 1795488180000000000, 1,
    'harness: fault latch closed: the daemon wrote ERROR daemon.rollout.relaunch: could not release the restart-pending hold {cause=wsm: lease: wsm: not found reason=query_died} (wsm not found after a close)',
    '~/.claude-emacs/merge-logs/dd0fb85a23e143b5-tests-2.log'
  UNION ALL SELECT 'daemon', 'TestAParentWithAnOpenChildIsNotKilled', 1795488300000000000, 1795488300000000000, 1,
    'harness: fault latch closed: the daemon wrote ERROR daemon.promptqueue.submit: the revival failed; the submission cannot be delivered {cause=workspace: claim serving: wsm: workspace: wsm: not found} (wsm not found after a close)',
    '~/.claude-emacs/merge-logs/7c67f6f4010b4b41-tests-2.log'
  UNION ALL SELECT 'daemon', 'TestPromptsHeldBeforeAMergeLeaseStayHeld', 1795462560000000000, 1795462560000000000, 1,
    'harness: fault latch closed: the daemon wrote ERROR daemon.merge.live_bubbles: could not read the merge queues; no live bubble is ended {cause=context canceled why=queue} (context canceled at shutdown logged at ERROR)',
    '~/.claude-emacs/merge-logs/bda221fe33a8492b-tests-1.log'
  UNION ALL SELECT 'daemon', 'TestARequiredWarningCountsAsStaleUntilItIsProduced', 1795437720000000000, 1795437720000000000, 1,
    'harness: fault latch closed: the daemon wrote ERROR daemon.workspace.bring_up: the StartSession call failed {cause=canceled: context canceled} (context canceled at shutdown logged at ERROR)',
    '~/.claude-emacs/merge-logs/0ef0af160162485b-tests-1.log'
) s
WHERE r.repo_key LIKE '%/.config/doom/.git';
INSERT INTO queue_race_finding_merges (repo_key, suite, test, merge_name, failed_at)
SELECT f.repo_key, f.suite, f.test, m.merge_name, f.last_seen
FROM queue_race_findings f, (
  SELECT 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement' AS test, 'withheld-standdown-remote-control' AS merge_name
  UNION ALL SELECT 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement', 'land-coverage-impact-selection-3'
  UNION ALL SELECT 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement', 'power-mode-lid-closed'
  UNION ALL SELECT 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement', 'sidebar-duration-context-color'
  UNION ALL SELECT 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement', 'deploy-build-reuse-unchanged'
  UNION ALL SELECT 'TestAdoptWebWorkspaceRefusesParticipantNotExpectedForAClientNotOpenAtAnnouncement', 'oneshot-ack-status'
  UNION ALL SELECT 'TestLanding', 'withheld-standdown-remote-control'
  UNION ALL SELECT 'TestLanding', 'merge-compact-model-downgrade'
  UNION ALL SELECT 'TestLanding', 'deploy-build-reuse-unchanged'
  UNION ALL SELECT 'TestLanding', 'oneshot-ack-status'
  UNION ALL SELECT 'TestLanding', 'deploy-exact-source-sets'
  UNION ALL SELECT 'TestLanding', 'handover-page-single-attach'
  UNION ALL SELECT 'TestLanding', 'visibility-allocation-rebased'
  UNION ALL SELECT 'TestALateFactsRefusalIsOfferedAgainMidWork', 'withheld-standdown-remote-control'
  UNION ALL SELECT 'TestALateFactsRefusalIsOfferedAgainMidWork', 'deploy-build-reuse-unchanged'
  UNION ALL SELECT 'TestALateFactsRefusalIsOfferedAgainMidWork', 'oneshot-ack-status'
  UNION ALL SELECT 'TestALateFactsRefusalIsOfferedAgainMidWork', 'visibility-page-load-metrics'
  UNION ALL SELECT 'TestALateFactsRefusalIsOfferedAgainMidWork', 'land-deploy-check-admission-race-2'
  UNION ALL SELECT 'TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands', 'withheld-standdown-remote-control'
  UNION ALL SELECT 'TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands', 'merge-workspace-close-on-every-end'
  UNION ALL SELECT 'TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands', 'deploy-build-reuse-unchanged'
  UNION ALL SELECT 'TestAMergeStoppedInItsTestGateResumesAfterARestartAndLands', 'oneshot-ack-status'
  UNION ALL SELECT 'TestQueryDiedDrawsTheTurnsTerminal', 'land-handover-keeps-pages'
  UNION ALL SELECT 'TestRosterRowIsTurnDiedWhenTheQueryDiesUnderATurn', 'handover-page-single-attach'
  UNION ALL SELECT 'TestAParentWithAnOpenChildIsNotKilled', 'visibility-page-load-metrics'
  UNION ALL SELECT 'TestPromptsHeldBeforeAMergeLeaseStayHeld', 'agent-repl-slack-threads'
  UNION ALL SELECT 'TestARequiredWarningCountsAsStaleUntilItIsProduced', 'production-mediator-land-3'
) m
WHERE f.test = m.test;
`

// GateFailureKeep is how long a gate's failure waits for a pass under its
// digest before it is forgotten: a digest no run reaches again in a month
// belongs to a tree nobody tests any more.
const GateFailureKeep = 30 * 24 * time.Hour

// RaceFindingRef names one race finding of a repository: its test, in the
// suite that ran it (agentrepl.v1.QueueRaceFindingRef).
type RaceFindingRef struct {
	Suite string
	Test  string
}

// String names the finding as briefs and records do: "suite/test".
func (r RaceFindingRef) String() string { return r.Suite + "/" + r.Test }

// GateTestFailure is one test that failed in a merge gate, under the inputs
// digest its unit ran on.
type GateTestFailure struct {
	Ref          RaceFindingRef
	InputsDigest string
	// Merge is the merge's name, as the queue lists it.
	Merge    string
	FailedAt time.Time
	// Failure is the failure's text: the end of its unit's output.
	Failure string
	// Artifacts is where the run's archive and preserved artifacts are.
	Artifacts string
}

// TestPass is one test that passed, under the inputs digest its unit ran on.
type TestPass struct {
	Ref          RaceFindingRef
	InputsDigest string
}

// RaceFinding is one test's race, as the repository keeps it.
type RaceFinding struct {
	Ref RaceFindingRef
	// FirstSeen and LastSeen are when its first and latest failing gate
	// failed.
	FirstSeen, LastSeen time.Time
	// Recurrences is how many gate failures were promoted to it.
	Recurrences int
	// Merges is every merge it failed in, oldest first.
	Merges []string
	// Failure and Artifacts are its latest recurrence's.
	Failure, Artifacts string
	// TakenOnAt is when a dispatch took it on, nil while it is open.
	TakenOnAt *time.Time
	// FixedAt is when the fix's deploy marked it fixed, nil until then.
	FixedAt *time.Time
	// Fixes is how many fixes of it deployed.
	Fixes int
}

// Open reports whether the finding waits for a dispatch: never taken on, or
// promoted again after its fix.
func (f RaceFinding) Open() bool { return f.TakenOnAt == nil }

// Promotion is a race finding a pass just made or updated.
type Promotion struct {
	Finding RaceFinding
	// New is a finding the repository did not hold before.
	New bool
	// Reopened is a finding whose fix had deployed: the fix did not end it.
	Reopened bool
}

// validRef refuses a reference naming no suite or no test.
func validRef(r RaceFindingRef) error {
	if strings.TrimSpace(r.Suite) == "" || strings.TrimSpace(r.Test) == "" {
		return fmt.Errorf("wsm: a race finding names its suite and its test (%q)", r.String())
	}
	return nil
}

// RecordGateFailures keeps every one of FAILURES as a candidate of REPO's
// race findings, and forgets each candidate older than GateFailureKeep
// before the latest of them. A failure naming no digest, no test, no merge or
// no instant is refused and nothing is written.
func (s *store) RecordGateFailures(ctx context.Context, repo RepoKey, failures []GateTestFailure) error {
	const op = "daemon.wsm.record_gate_failures"
	fields := dlog.Context{"repo": string(repo), "failures": len(failures)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	var latest time.Time
	for _, f := range failures {
		if err := validRef(f.Ref); err != nil || f.InputsDigest == "" || f.Merge == "" || f.FailedAt.IsZero() {
			if err == nil {
				err = fmt.Errorf("wsm: the gate failure of %s names its digest, its merge and its instant", f.Ref)
			}
			s.log.Error(op, "refused a gate failure that does not name its test, digest, merge and instant", withError(fields, err))
			return err
		}
		if f.FailedAt.After(latest) {
			latest = f.FailedAt
		}
	}
	if len(failures) == 0 {
		return nil
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, f := range failures {
			if _, err := tx.ExecContext(ctx, `INSERT INTO queue_gate_failures (repo_key, suite, test, inputs_digest, merge_name, failed_at, failure, artifacts) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
				ON CONFLICT(repo_key, suite, test, inputs_digest, merge_name) DO UPDATE SET failed_at = excluded.failed_at, failure = excluded.failure, artifacts = excluded.artifacts`,
				string(key), f.Ref.Suite, f.Ref.Test, f.InputsDigest, f.Merge, nanos(f.FailedAt), f.Failure, f.Artifacts); err != nil {
				return err
			}
		}
		_, err := tx.ExecContext(ctx, `DELETE FROM queue_gate_failures WHERE repo_key = ? AND failed_at < ?`, string(key), nanos(latest.Add(-GateFailureKeep)))
		return err
	})
}

// PromoteRaces promotes every candidate of REPO that one of PASSES passed
// under the digest it failed under: each such candidate is a recurrence of
// its test's race finding, made when the repository holds none and opened
// again when its fix had deployed, and the candidate is forgotten. It
// answers every finding a pass promoted, in the order of PASSES.
func (s *store) PromoteRaces(ctx context.Context, repo RepoKey, passes []TestPass) ([]Promotion, error) {
	const op = "daemon.wsm.promote_races"
	fields := dlog.Context{"repo": string(repo), "passes": len(passes)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	for _, p := range passes {
		if err := validRef(p.Ref); err != nil || p.InputsDigest == "" {
			if err == nil {
				err = fmt.Errorf("wsm: the pass of %s names its digest", p.Ref)
			}
			s.log.Error(op, "refused a pass that does not name its test and digest", withError(fields, err))
			return nil, err
		}
	}
	var out []Promotion
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		out = nil
		for _, p := range passes {
			promoted, err := promoteRace(ctx, tx, string(key), p)
			if err != nil {
				return err
			}
			if promoted == nil {
				continue
			}
			finding, found, err := raceFindingIn(ctx, tx, string(key), p.Ref)
			if err != nil {
				return err
			}
			if !found {
				return fmt.Errorf("wsm: the promoted race finding %s is gone", p.Ref)
			}
			promoted.Finding = finding
			out = append(out, *promoted)
		}
		return nil
	})
	return out, err
}

// promoteRace promotes P's candidates in REPO, answering nil when P passed
// under no digest a candidate failed under.
func promoteRace(ctx context.Context, tx *sql.Tx, repo string, p TestPass) (*Promotion, error) {
	type candidate struct {
		merge              string
		failedAt           int64
		failure, artifacts string
	}
	candidates, err := listRows(ctx, tx, `SELECT merge_name, failed_at, failure, artifacts FROM queue_gate_failures WHERE repo_key = ? AND suite = ? AND test = ? AND inputs_digest = ? ORDER BY failed_at`,
		func(r interface{ Scan(...any) error }) (candidate, error) {
			var c candidate
			return c, r.Scan(&c.merge, &c.failedAt, &c.failure, &c.artifacts)
		}, repo, p.Ref.Suite, p.Ref.Test, p.InputsDigest)
	if err != nil {
		return nil, err
	}
	if len(candidates) == 0 {
		return nil, nil
	}
	var fixed sql.NullInt64
	err = tx.QueryRowContext(ctx, `SELECT fixed_at FROM queue_race_findings WHERE repo_key = ? AND suite = ? AND test = ?`, repo, p.Ref.Suite, p.Ref.Test).Scan(&fixed)
	promoted := &Promotion{}
	switch {
	case errors.Is(err, sql.ErrNoRows):
		promoted.New = true
	case err != nil:
		return nil, err
	default:
		promoted.Reopened = fixed.Valid
	}
	for _, c := range candidates {
		// A FIXED FINDING PROMOTED AGAIN IS OPEN AGAIN: its fix did not end
		// the race, so the next check puts it to the mediator again.
		if _, err := tx.ExecContext(ctx, `INSERT INTO queue_race_findings (repo_key, suite, test, first_seen, last_seen, recurrences, failure, artifacts) VALUES (?, ?, ?, ?, ?, 1, ?, ?)
			ON CONFLICT(repo_key, suite, test) DO UPDATE SET
			  last_seen = MAX(queue_race_findings.last_seen, excluded.last_seen),
			  recurrences = queue_race_findings.recurrences + 1,
			  failure = excluded.failure, artifacts = excluded.artifacts,
			  taken_on_at = CASE WHEN queue_race_findings.fixed_at IS NULL THEN queue_race_findings.taken_on_at END,
			  fixed_at = NULL`,
			repo, p.Ref.Suite, p.Ref.Test, c.failedAt, c.failedAt, c.failure, c.artifacts); err != nil {
			return nil, err
		}
		if _, err := tx.ExecContext(ctx, `INSERT INTO queue_race_finding_merges (repo_key, suite, test, merge_name, failed_at) VALUES (?, ?, ?, ?, ?)
			ON CONFLICT(repo_key, suite, test, merge_name) DO UPDATE SET failed_at = excluded.failed_at`,
			repo, p.Ref.Suite, p.Ref.Test, c.merge, c.failedAt); err != nil {
			return nil, err
		}
	}
	if _, err := tx.ExecContext(ctx, `DELETE FROM queue_gate_failures WHERE repo_key = ? AND suite = ? AND test = ? AND inputs_digest = ?`,
		repo, p.Ref.Suite, p.Ref.Test, p.InputsDigest); err != nil {
		return nil, err
	}
	return promoted, nil
}

// UnpromotedGateFailures answers each of FAILED, MERGE's failures in REPO,
// that no pass has promoted to a race finding: still a candidate under its
// digest, or never recorded at all.
func (s *store) UnpromotedGateFailures(ctx context.Context, repo RepoKey, merge string, failed []TestPass) ([]RaceFindingRef, error) {
	const op = "daemon.wsm.unpromoted_gate_failures"
	fields := dlog.Context{"repo": string(repo), "merge": merge, "failed": len(failed)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return nil, err
	}
	var out []RaceFindingRef
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		out = nil
		for _, f := range failed {
			var pending, promoted int
			if err := s.q(ctx).QueryRowContext(ctx, `SELECT
				(SELECT count(*) FROM queue_gate_failures WHERE repo_key = ? AND suite = ? AND test = ? AND inputs_digest = ? AND merge_name = ?),
				(SELECT count(*) FROM queue_race_finding_merges WHERE repo_key = ? AND suite = ? AND test = ? AND merge_name = ?)`,
				string(key), f.Ref.Suite, f.Ref.Test, f.InputsDigest, merge,
				string(key), f.Ref.Suite, f.Ref.Test, merge).Scan(&pending, &promoted); err != nil {
				return err
			}
			if pending > 0 || promoted == 0 {
				out = append(out, f.Ref)
			}
		}
		return nil
	})
	return out, err
}

// RaceFindings answers every race finding REPO holds, the oldest first.
func (s *store) RaceFindings(ctx context.Context, repo RepoKey) ([]RaceFinding, error) {
	const op = "daemon.wsm.race_findings"
	key, err := s.normalizedRepoKey(op, repo, dlog.Context{"repo": string(repo)})
	if err != nil {
		return nil, err
	}
	var out []RaceFinding
	err = s.read(ctx, op, dlog.Context{"repo": string(key)}, func(ctx context.Context) error {
		out = nil
		refs, err := listRows(ctx, s.q(ctx), `SELECT suite, test FROM queue_race_findings WHERE repo_key = ? ORDER BY first_seen, suite, test`,
			func(r interface{ Scan(...any) error }) (RaceFindingRef, error) {
				var ref RaceFindingRef
				return ref, r.Scan(&ref.Suite, &ref.Test)
			}, string(key))
		if err != nil {
			return err
		}
		for _, r := range refs {
			f, _, err := raceFindingIn(ctx, s.q(ctx), string(key), r)
			if err != nil {
				return err
			}
			out = append(out, f)
		}
		return nil
	})
	return out, err
}

// findingQuerier is what raceFindingIn reads through: a transaction or the
// store's own connection.
type findingQuerier interface {
	querier
	rowQuerier
}

// raceFindingIn reads REF's finding in REPO through Q, false when none.
func raceFindingIn(ctx context.Context, q findingQuerier, repo string, ref RaceFindingRef) (RaceFinding, bool, error) {
	f := RaceFinding{Ref: ref}
	var first, last int64
	var taken, fixed sql.NullInt64
	err := q.QueryRowContext(ctx, `SELECT first_seen, last_seen, recurrences, failure, artifacts, taken_on_at, fixed_at, fixes FROM queue_race_findings WHERE repo_key = ? AND suite = ? AND test = ?`,
		repo, ref.Suite, ref.Test).Scan(&first, &last, &f.Recurrences, &f.Failure, &f.Artifacts, &taken, &fixed, &f.Fixes)
	if errors.Is(err, sql.ErrNoRows) {
		return RaceFinding{}, false, nil
	}
	if err != nil {
		return RaceFinding{}, false, err
	}
	f.FirstSeen, f.LastSeen, f.TakenOnAt, f.FixedAt = fromNanos(first), fromNanos(last), optTime(taken), optTime(fixed)
	f.Merges, err = listRows(ctx, q, `SELECT merge_name FROM queue_race_finding_merges WHERE repo_key = ? AND suite = ? AND test = ? ORDER BY failed_at, merge_name`,
		func(r interface{ Scan(...any) error }) (string, error) {
			var m string
			return m, r.Scan(&m)
		}, repo, ref.Suite, ref.Test)
	if err != nil {
		return RaceFinding{}, false, err
	}
	return f, true, nil
}

// UnknownRaceFindingError is a reference to a race finding the repository
// does not hold.
type UnknownRaceFindingError struct{ Ref RaceFindingRef }

func (e *UnknownRaceFindingError) Error() string {
	return fmt.Sprintf("wsm: the repository holds no race finding %s", e.Ref)
}

// CheckRaceFindings answers an *UnknownRaceFindingError naming the first of
// REFS that REPO does not hold, nil when it holds them all.
func (s *store) CheckRaceFindings(ctx context.Context, repo RepoKey, refs []RaceFindingRef) error {
	const op = "daemon.wsm.check_race_findings"
	fields := dlog.Context{"repo": string(repo), "findings": len(refs)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	var unknown *UnknownRaceFindingError
	err = s.read(ctx, op, fields, func(ctx context.Context) error {
		unknown = nil
		for _, r := range refs {
			if _, found, err := raceFindingIn(ctx, s.q(ctx), string(key), r); err != nil {
				return err
			} else if !found {
				unknown = &UnknownRaceFindingError{Ref: r}
				return nil
			}
		}
		return nil
	})
	if err != nil {
		return err
	}
	if unknown != nil {
		return unknown
	}
	return nil
}

// TakeOnRaceFindings records that a dispatch at AT took on every finding
// REFS names in REPO. A finding the repository does not hold refuses the
// whole call with an *UnknownRaceFindingError, writing nothing.
func (s *store) TakeOnRaceFindings(ctx context.Context, repo RepoKey, refs []RaceFindingRef, at time.Time) error {
	const op = "daemon.wsm.take_on_race_findings"
	fields := dlog.Context{"repo": string(repo), "findings": len(refs)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return err
	}
	if len(refs) == 0 || at.IsZero() {
		err := errors.New("wsm: race findings are taken on by name, at an instant")
		s.log.Error(op, "refused to take on race findings that name none or no instant", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		for _, r := range refs {
			res, err := tx.ExecContext(ctx, `UPDATE queue_race_findings SET taken_on_at = ?, fixed_at = NULL WHERE repo_key = ? AND suite = ? AND test = ?`,
				nanos(at), string(key), r.Suite, r.Test)
			if err != nil {
				return err
			}
			if touched, err := touchedRow(res); err != nil {
				return err
			} else if !touched {
				return &UnknownRaceFindingError{Ref: r}
			}
		}
		return nil
	})
}

// FixRaceFindings marks every race finding of REPO a dispatch took on fixed
// at AT, the instant the fix's deploy began, answering how many it marked.
func (s *store) FixRaceFindings(ctx context.Context, repo RepoKey, at time.Time) (int, error) {
	const op = "daemon.wsm.fix_race_findings"
	fields := dlog.Context{"repo": string(repo)}
	key, err := s.normalizedRepoKey(op, repo, fields)
	if err != nil {
		return 0, err
	}
	if at.IsZero() {
		err := errors.New("wsm: a fix names its instant")
		s.log.Error(op, "refused to mark race findings fixed with no instant", withError(fields, err))
		return 0, err
	}
	n := 0
	err = s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE queue_race_findings SET fixed_at = ?, fixes = fixes + 1 WHERE repo_key = ? AND taken_on_at IS NOT NULL AND fixed_at IS NULL`, nanos(at), string(key))
		if err != nil {
			return err
		}
		rows, err := res.RowsAffected()
		n = int(rows)
		return err
	})
	return n, err
}
