package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// fakeFaults is an in-memory fault store: every fault opened, with its
// resolved-at once closed.
type fakeFaults struct {
	mu     sync.Mutex
	faults []wsm.Fault
	closed map[ids.FaultID]time.Time
	// openErr fails OpenFault.
	openErr error
}

func (f *fakeFaults) OpenFault(_ context.Context, fault wsm.Fault) (ids.FaultID, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.openErr != nil {
		return "", f.openErr
	}
	fault.ID = ids.FaultID(fmt.Sprintf("fault-%d", len(f.faults)+1))
	f.faults = append(f.faults, fault)
	return fault.ID, nil
}

func (f *fakeFaults) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []wsm.Fault
	for _, fault := range f.faults {
		if _, done := f.closed[fault.ID]; done {
			continue
		}
		if scope.Workspace != nil && (fault.Workspace == nil || *fault.Workspace != *scope.Workspace) {
			continue
		}
		out = append(out, fault)
	}
	return out, nil
}

func (f *fakeFaults) CloseFault(_ context.Context, id ids.FaultID, at time.Time) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.closed == nil {
		f.closed = map[ids.FaultID]time.Time{}
	}
	f.closed[id] = at
	return nil
}

// standing answers the faults of KIND that stand.
func (f *fakeFaults) standing(t *testing.T, kind string) []wsm.Fault {
	t.Helper()
	open, _ := f.OpenFaults(context.Background(), wsm.FaultScope{})
	var out []wsm.Fault
	for _, fault := range open {
		if fault.Kind == kind {
			out = append(out, fault)
		}
	}
	return out
}

// oneUndelivered answers the one standing undelivered fault, failing the
// test unless exactly one stands, for the workspace.
func oneUndelivered(t *testing.T, h *harness) wsm.Fault {
	t.Helper()
	standing := h.faults.standing(t, health.KindKeepaliveUndelivered)
	if len(standing) != 1 || standing[0].Workspace == nil || *standing[0].Workspace != theWorkspace {
		t.Fatalf("undelivered faults standing = %+v, want one for %s", standing, theWorkspace)
	}
	return standing[0]
}

// errorRecord answers the ERROR record MESSAGE, failing the test unless it
// names the workspace and CAUSE.
func errorRecord(t *testing.T, h *harness, op, message, cause string) dlog.Record {
	t.Helper()
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: op, Message: message})
	if !ok || record.Context["workspace"] != string(theWorkspace) || !strings.Contains(fmt.Sprint(record.Context["cause"]), cause) {
		t.Fatalf("records = %+v, want %q at ERROR naming the workspace and %q", h.log.Records(), message, cause)
	}
	return record
}

// lapsedAt is the shim's judgment that the cache lapsed at AT.
func lapsedAt(at time.Time) *shimv1.SendKeepaliveSuccess {
	return skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{
		CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: at.UnixMilli()},
	}})
}

func TestAKeepaliveThatCannotBeArmedIsUndelivered(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.sessionErr = errors.New("database is locked")

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	record := errorRecord(t, h, opDrive, "could not read the workspace's session; its keep-alive is not armed", "database is locked")
	fault := oneUndelivered(t, h)
	if record.Context["due"] != "untimed" || fault.Evidence[health.EvidenceDueAtMs] != "0" || !strings.Contains(fault.Evidence[health.EvidenceCause], "database is locked") {
		t.Fatalf("record %+v, fault %+v; want an untimed keep-alive and its cause", record.Context, fault.Evidence)
	}
}

func TestAKeepaliveWhoseSubmissionFailedIsUndelivered(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = &BeatSubmitFailed{Detail: "the vendor query ended"}
	due := now.Add(-time.Minute)

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	errorRecord(t, h, opDrive, "the keep-alive's submission failed; it is asked again shortly", "the vendor query ended")
	if fault := oneUndelivered(t, h); fault.Evidence[health.EvidenceDueAtMs] != fmt.Sprint(due.UnixMilli()) {
		t.Fatalf("fault evidence = %+v, want the due %v", fault.Evidence, due)
	}
}

func TestAKeepaliveTheShimCouldNotBeAskedForIsUndelivered(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = errors.New("connection refused")

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	errorRecord(t, h, opDrive, "could not ask the shim for its keep-alive; it is asked again shortly", "connection refused")
	if fault := oneUndelivered(t, h); !strings.Contains(fault.Evidence[health.EvidenceCause], "connection refused") {
		t.Fatalf("fault evidence = %+v, want the call's cause", fault.Evidence)
	}
}

func TestAKeepaliveUnderAShimThatDiedIsNotTheKeepersFailure(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = fmt.Errorf("%w: unexpected EOF", shimclient.ErrShimDied)

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 {
		t.Fatalf("undelivered faults = %+v, want none: the death is recorded where it happened", standing)
	}
}

func TestAFailureTheDaemonsExitMadeIsNoUndeliveredKeepalive(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.sessionErr = context.Canceled
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	h.keeper.schedule(ctx, theWorkspace, "test")

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 || h.found(dlog.RecordQuery{Level: "error"}) {
		t.Fatalf("faults %+v, records %+v; want neither a fault nor an ERROR on the daemon's exit", standing, h.log.Records())
	}
}

func TestALaterFailureSupersedesTheUndeliveredFault(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = &BeatSubmitFailed{Detail: "first"}
	h.keeper.sendBeat(context.Background(), theWorkspace, now)
	h.shim.beatErr = &BeatSubmitFailed{Detail: "second"}

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now.Add(BeatRetry))

	// Assert
	if fault := oneUndelivered(t, h); !strings.Contains(fault.Evidence[health.EvidenceCause], "second") {
		t.Fatalf("the standing fault is %+v, want the latest failure's", fault.Evidence)
	}
}

func TestASentKeepaliveClosesTheUndeliveredFault(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = &BeatSubmitFailed{Detail: "the vendor query ended"}
	h.keeper.sendBeat(context.Background(), theWorkspace, now)
	h.shim.beatErr, h.shim.answer = nil, sentAnswer()

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now.Add(BeatRetry))

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 {
		t.Fatalf("undelivered faults = %+v, want none once a keep-alive went out", standing)
	}
}

func TestAnUndeliveredFaultThatCannotBeRecordedIsAnError(t *testing.T) {
	// Arrange
	h := dueHarness(t, nil)
	h.shim.beatErr = &BeatSubmitFailed{Detail: "the vendor query ended"}
	h.faults.openErr = errors.New("disk full")

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	errorRecord(t, h, opDelivery, "could not record the undelivered keep-alive's fault; the failure stands on the record above alone", "disk full")
}

func TestAKeepaliveTheCacheLapsedAfterWasMissed(t *testing.T) {
	// Arrange: due a minute ago, the cache lapsed thirty seconds ago.
	due := now.Add(-time.Minute)
	h := dueHarness(t, lapsedAt(due.Add(30*time.Second)))

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive,
		Message: "a keep-alive that came due was not sent before the cache lapsed; the keeper missed it"})
	if !ok || record.Context["workspace"] != string(theWorkspace) || record.Context["late_by"] != (30*time.Second).String() {
		t.Fatalf("records = %+v, want the miss at ERROR with how late it was", h.log.Records())
	}
	if fault := oneUndelivered(t, h); !strings.Contains(fault.Evidence[health.EvidenceCause], "before the keep-alive went out") {
		t.Fatalf("fault evidence = %+v, want the miss as its cause", fault.Evidence)
	}
}

func TestACacheThatLapsedBeforeItsKeepaliveCameDueWasNotMissed(t *testing.T) {
	// Arrange: the last request left the context uncached an interval ago.
	due := now.Add(-time.Minute)
	h := dueHarness(t, lapsedAt(due.Add(-KeepaliveInterval)))

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 || h.found(dlog.RecordQuery{Level: "error"}) {
		t.Fatalf("faults %+v, records %+v; want no miss", standing, h.log.Records())
	}
}

func TestACacheThatLapsedWhileTheWorkspaceOwedNoKeepaliveWasNotMissed(t *testing.T) {
	// Arrange: due a minute ago, the cache lapsed thirty seconds ago, and the
	// row says the workspace began owing keep-alives again only after the due
	// (it was withheld until then, under whichever daemon).
	due := now.Add(-time.Minute)
	h := dueHarness(t, lapsedAt(due.Add(30*time.Second)))
	rec := h.db.records[theWorkspace]
	from := due.Add(time.Second)
	rec.KeepaliveOwedFrom = &from
	h.db.records[theWorkspace] = rec

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 || h.found(dlog.RecordQuery{Level: "error"}) {
		t.Fatalf("faults %+v, records %+v; want no miss", standing, h.log.Records())
	}
	if !h.found(dlog.RecordQuery{Level: "info", Operation: opDrive, Message: "the cache lapsed while the workspace owed no keep-alive; the keeper missed nothing"}) {
		t.Fatalf("records = %+v, want the lapse recorded at INFO", h.log.Records())
	}
}

// TestASuccessorKeeperJudgesADueItsPredecessorOwedNoneForAsNoMiss pins the
// 2026-11-17 miss: one daemon decided the workspace owed none, a successor
// that never saw that decision found it owed again, and the beat due in
// between lapsed. The decision on the row carries across.
func TestASuccessorKeeperJudgesADueItsPredecessorOwedNoneForAsNoMiss(t *testing.T) {
	// Arrange
	due := now.Add(-time.Minute)
	h := dueHarness(t, lapsedAt(due.Add(30*time.Second)))
	predecessor := newHarness(t, h.db.records[theWorkspace])
	predecessor.db = h.db
	predecessor.keeper.deps.DB = h.db
	predecessor.keeper.owesNone(context.Background(), dlog.NewTestLogger(), h.db.records[theWorkspace], "it is withheld, so it runs no session")
	h.keeper.owesAgain(context.Background(), dlog.NewTestLogger(), h.db.records[theWorkspace])

	// Act
	h.keeper.sendBeat(context.Background(), theWorkspace, now)

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 || h.found(dlog.RecordQuery{Level: "error"}) {
		t.Fatalf("faults %+v, records %+v; want no miss", standing, h.log.Records())
	}
}

func TestAMissThatCannotReadTheRowIsNotJudged(t *testing.T) {
	// Arrange
	due := now.Add(-time.Minute)
	h := dueHarness(t, lapsedAt(due.Add(30*time.Second)))
	h.db.readErr = errors.New("registry down")
	log := dlog.NewTestLogger()

	// Act
	missed := h.keeper.missed(context.Background(), log, theWorkspace, due, due.Add(30*time.Second))

	// Assert
	if missed || len(h.faults.standing(t, health.KindKeepaliveUndelivered)) != 0 {
		t.Fatalf("missed = %v, faults %+v; want no judgment", missed, h.faults.standing(t, health.KindKeepaliveUndelivered))
	}
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive,
		Message: "could not read when the workspace began owing keep-alives; whether the keeper missed the lapsed beat is not judged"})
	if !ok || !strings.Contains(fmt.Sprint(record.Context["cause"]), "registry down") {
		t.Fatalf("records = %+v, want the read's failure at ERROR with its cause", log.Records())
	}
}

func TestOwingRecordsTheDecisionOnTheRow(t *testing.T) {
	stamped := now.Add(-time.Hour)
	tests := []struct {
		name       string
		arrange    func(rec *wsm.Workspace)
		act        func(h *harness, rec wsm.Workspace)
		wantNone   *time.Time
		wantFrom   *time.Time
		wantWrites int
	}{
		{name: "owing none stamps when it began", arrange: func(*wsm.Workspace) {},
			act: func(h *harness, rec wsm.Workspace) {
				h.keeper.owesNone(context.Background(), dlog.NewTestLogger(), rec, "why")
			},
			wantNone: &now, wantWrites: 1},
		{name: "a standing decision is not written again", arrange: func(rec *wsm.Workspace) { rec.KeepaliveOwedNoneAt = &stamped },
			act: func(h *harness, rec wsm.Workspace) {
				h.keeper.owesNone(context.Background(), dlog.NewTestLogger(), rec, "why")
			},
			wantNone: &stamped},
		{name: "owing again stamps when, from the row alone", arrange: func(rec *wsm.Workspace) { rec.KeepaliveOwedNoneAt = &stamped },
			act: func(h *harness, rec wsm.Workspace) {
				h.keeper.owesAgain(context.Background(), dlog.NewTestLogger(), rec)
			},
			wantFrom: &now, wantWrites: 1},
		{name: "a workspace that owed them all along writes nothing", arrange: func(*wsm.Workspace) {},
			act: func(h *harness, rec wsm.Workspace) {
				h.keeper.owesAgain(context.Background(), dlog.NewTestLogger(), rec)
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			rec := h.db.records[theWorkspace]
			tt.arrange(&rec)
			h.db.records[theWorkspace] = rec

			// Act
			tt.act(h, rec)

			// Assert
			got := h.db.records[theWorkspace]
			if !sameInstant(got.KeepaliveOwedNoneAt, tt.wantNone) || !sameInstant(got.KeepaliveOwedFrom, tt.wantFrom) || h.db.owingWrites != tt.wantWrites {
				t.Fatalf("row = (none %v, from %v), writes %d; want (%v, %v), %d",
					got.KeepaliveOwedNoneAt, got.KeepaliveOwedFrom, h.db.owingWrites, tt.wantNone, tt.wantFrom, tt.wantWrites)
			}
		})
	}
}

func TestAnOwingDecisionThatCannotBeWrittenIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.owingErr = errors.New("database is locked")
	log := dlog.NewTestLogger()

	// Act
	h.keeper.owesNone(context.Background(), log, h.db.records[theWorkspace], "it is withheld, so it runs no session")

	// Assert
	record, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: opDrive,
		Message: "could not record whether the workspace owes keep-alives; a lapse judged against the row may misjudge a miss"})
	if !ok || record.Context["owing"] != false || !strings.Contains(fmt.Sprint(record.Context["cause"]), "database is locked") {
		t.Fatalf("records = %+v, want the failed write at ERROR with its cause", log.Records())
	}
	if h.db.records[theWorkspace].KeepaliveOwedNoneAt != nil {
		t.Fatalf("row = %+v, want it unchanged", h.db.records[theWorkspace])
	}
}

// sameInstant reports whether two optional instants are both nil or equal.
func sameInstant(a, b *time.Time) bool {
	if a == nil || b == nil {
		return a == nil && b == nil
	}
	return a.Equal(*b)
}

func TestAParkedSessionThatCouldNotBeRestoredIsUndelivered(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)
	h.keeper.schedule(context.Background(), theWorkspace, "test")
	due := h.keeper.beats[theWorkspace]

	// Act
	h.keeper.restored(context.Background(), theWorkspace, errors.New("the shim would not start"))

	// Assert
	errorRecord(t, h, opRestore, "the parked session could not be restored for its keep-alive; it is asked again shortly", "the shim would not start")
	if fault := oneUndelivered(t, h); fault.Evidence[health.EvidenceDueAtMs] != fmt.Sprint(due.UnixMilli()) {
		t.Fatalf("fault evidence = %+v, want the armed beat's due %v", fault.Evidence, due)
	}
}

func TestAParkedKeepaliveThatFailedIsUndelivered(t *testing.T) {
	tests := []struct {
		name    string
		err     error
		message string
		cause   string
	}{
		{name: "its submission failed", err: &BeatSubmitFailed{Detail: "overloaded"},
			message: "the parked conversation's keep-alive failed; it is asked again shortly", cause: "overloaded"},
		{name: "the shim could not be asked", err: errors.New("connection refused"),
			message: "could not ask the shim for the parked keep-alive; it is asked again shortly", cause: "connection refused"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))

			// Act
			h.keeper.parkedAnswered(context.Background(), theWorkspace, nil, tt.err)

			// Assert
			errorRecord(t, h, opDrive, tt.message, tt.cause)
			oneUndelivered(t, h)
		})
	}
}

func TestAnAnsweredParkedKeepaliveClosesTheUndeliveredFault(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.bypass[theWorkspace] = wsm.QueueBypass{ParkedVendorSessionID: "parked-1"}
	h.keeper.parkedAnswered(context.Background(), theWorkspace, nil, errors.New("connection refused"))

	// Act
	h.keeper.parkedAnswered(context.Background(), theWorkspace, sentAnswer(), nil)

	// Assert
	if standing := h.faults.standing(t, health.KindKeepaliveUndelivered); len(standing) != 0 {
		t.Fatalf("undelivered faults = %+v, want none once the parked keep-alive was answered", standing)
	}
}

func TestAKeeperThatCannotStartStandsStopped(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.listErr = errors.New("disk I/O error")

	// Act
	err := h.keeper.Run(context.Background())

	// Assert
	if err == nil {
		t.Fatal("Run started with no workspaces to arm")
	}
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opLapse,
		Message: "could not list the workspaces to re-arm their cache lapses; the cache keeper stops, and no keep-alive goes out for any workspace until the next daemon runs its own"})
	if !ok || !strings.Contains(fmt.Sprint(record.Context["cause"]), "disk I/O error") {
		t.Fatalf("records = %+v, want the stop at ERROR with its cause", h.log.Records())
	}
	stopped := h.faults.standing(t, health.KindKeepaliveDriveStopped)
	if len(stopped) != 1 || stopped[0].Workspace != nil || !strings.Contains(stopped[0].Evidence[health.EvidenceCause], "disk I/O error") {
		t.Fatalf("stopped faults = %+v, want one daemon-wide fault naming the cause", stopped)
	}
}

func TestAStoppedKeeperWhoseFaultCannotBeRecordedIsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.db.listErr = errors.New("disk I/O error")
	h.faults.openErr = errors.New("disk full")

	// Act
	_ = h.keeper.Run(context.Background())

	// Assert
	record, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opDelivery,
		Message: "could not record the stopped keeper's fault; the stop stands on the record above alone"})
	if !ok || record.Context["cause"] != "disk full" {
		t.Fatalf("records = %+v, want the failed fault write at ERROR", h.log.Records())
	}
}

// TestTheUndeliveredFaultHasOneOpen fails when a source of this package opens
// the undelivered fault other than through Keeper.raiseUndelivered.
func TestTheUndeliveredFaultHasOneOpen(t *testing.T) {
	// Act
	sites := sourcescan.Sites(t, "Kind:      health.KindKeepaliveUndelivered")

	// Assert
	if len(sites) != 1 || sites[0] != "delivery.go" {
		t.Fatalf("the undelivered fault is opened at %v, want only Keeper.raiseUndelivered in delivery.go", sites)
	}
}
