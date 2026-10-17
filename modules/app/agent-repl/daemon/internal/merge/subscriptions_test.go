package merge

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// lastChange answers the last change the harness's orchestrator reported,
// failing the test when none was.
func lastChange(t *testing.T, h *harness) QueueChange {
	t.Helper()
	changes := h.events.reported()
	if len(changes) == 0 {
		t.Fatal("no queue change was reported")
	}
	return changes[len(changes)-1]
}

// memberNames names the members of QUEUE, front first.
func memberNames(queue QueueSnapshot) []wsm.WorkspaceID {
	var out []wsm.WorkspaceID
	for _, member := range queue.Members {
		out = append(out, member.Workspace)
	}
	return out
}

// rawInLine puts WS in line by writing the fake store directly, as another
// daemon or an earlier build would: no change is reported for it.
func rawInLine(t *testing.T, h *harness, ws wsm.WorkspaceID) {
	t.Helper()
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), ws, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if _, err := h.db.QueueMerge(context.Background(), h.repoKey(), ws, "lease-raw"); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}
}

// merging is the facts of a merge standing on STEP.
func merging(h *harness, step footer.MergeStep) MergeFacts {
	return MergeFacts{State: StateMerging, Step: step, QueuedAt: h.clock()}
}

func TestTheOrchestratorWritesThroughTheQueueEventsDB(t *testing.T) {
	// Arrange, Act
	h := newHarness(t)

	// Assert: every queue write the orchestrator makes reports its change,
	// because the roster's wrapper writes through the queue events' wrapper.
	roster, ok := h.o.deps.DB.(rosterQueueDB)
	if !ok {
		t.Fatalf("the orchestrator's DB is %T, want rosterQueueDB", h.o.deps.DB)
	}
	if _, ok := roster.DB.(queueEventsDB); !ok {
		t.Fatalf("the roster queue's DB is %T, want queueEventsDB", roster.DB)
	}
}

func TestPuttingAMergeInLineReportsItEnqueuedWithItsSnapshot(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	enqueue(t, h)

	// Assert.
	change := lastChange(t, h)
	want := QueueMember{Workspace: theWorkspace, Name: "ws-one", Branch: "feature", Mode: modeOf(ownBranch, wsm.Workspace{})}
	if change.Kind != QueueChangeEnqueued || change.Subject != want {
		t.Fatalf("change = %v about %+v, want enqueued about %+v", change.Kind, change.Subject, want)
	}
	if !reflect.DeepEqual(change.Queue, QueueSnapshot{Repo: h.repoKey(), Members: []QueueMember{want}}) {
		t.Fatalf("queue = %+v, want the one merge in line", change.Queue)
	}
}

func TestTheSnapshotListsEveryMergeInLineFrontFirst(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)

	// Act.
	queueAnother(t, h, "ws-2", "two")

	// Assert.
	change := lastChange(t, h)
	if got := memberNames(change.Queue); !reflect.DeepEqual(got, []wsm.WorkspaceID{theWorkspace, "ws-2"}) {
		t.Fatalf("members = %v, want both, front first", got)
	}
	if got := change.Queue.Members[1]; got.Name != "two" || got.Branch != "b-two" || got.Started {
		t.Fatalf("second member = %+v, want two on b-two, waiting", got)
	}
}

func TestAdmittingAMergeReportsItStarted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	kinds := h.events.kinds()
	if len(kinds) < 2 || kinds[0] != QueueChangeEnqueued || kinds[1] != QueueChangeStarted {
		t.Fatalf("changes = %v, want enqueued then started", kinds)
	}
	if started := h.events.reported()[1]; !started.Subject.Started || !started.Queue.Members[0].Started {
		t.Fatalf("started change = %+v, want its subject and member started", started)
	}
}

func TestEachNewStepOfARunningMergeIsReportedOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	var steps []footer.MergeStep
	for _, change := range h.events.reported() {
		if change.Kind == QueueChangeStepped {
			steps = append(steps, change.Subject.Step)
		}
	}
	if len(steps) == 0 {
		t.Fatalf("no step was reported: %v", h.events.kinds())
	}
	for i := 1; i < len(steps); i++ {
		if steps[i] == steps[i-1] {
			t.Fatalf("steps reported = %v, want no step reported twice in a row", steps)
		}
	}
}

func TestAStepReportCarriesTheStepOnTheSubjectAndItsMember(t *testing.T) {
	// Arrange: a merge in line whose facts this daemon already published.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)
	h.o.publish(h.repoKey(), theWorkspace, MergeFacts{State: StateQueued, Step: footer.StepEnqueued, QueuedAt: h.clock()})
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}

	// Act.
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepTesting))

	// Assert.
	change := lastChange(t, h)
	if change.Kind != QueueChangeStepped || change.Subject.Step != footer.StepTesting || change.Queue.Members[0].Step != footer.StepTesting {
		t.Fatalf("change = %+v, want stepped to testing on the subject and its member", change)
	}
}

func TestRepublishingTheSameStepReportsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)
	h.o.publish(h.repoKey(), theWorkspace, MergeFacts{State: StateQueued, Step: footer.StepEnqueued, QueuedAt: h.clock()})
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepTesting))
	before := len(h.events.reported())

	// Act.
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepTesting))

	// Assert.
	if got := len(h.events.reported()); got != before {
		t.Fatalf("changes = %v, want nothing more reported for the same step", h.events.kinds())
	}
}

func TestATakenOverMergesFirstStepIsNotReportedAgain(t *testing.T) {
	// Arrange: a merge in line this daemon never published facts for, as a
	// restart's or a handover's successor finds one.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)

	// Act.
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepFixing))

	// Assert.
	if kinds := h.events.kinds(); len(kinds) != 0 {
		t.Fatalf("changes = %v, want the taken-over step not reported", kinds)
	}
}

func TestATakenOverMergesNextStepIsReported(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepFixing))

	// Act.
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepTesting))

	// Assert.
	if kinds := h.events.kinds(); !reflect.DeepEqual(kinds, []QueueChangeKind{QueueChangeStepped}) {
		t.Fatalf("changes = %v, want the next step reported", kinds)
	}
}

func TestAStepOfAMergeNotInLineIsRecordedAndNotReported(t *testing.T) {
	// Arrange: facts published before, and no line holding the merge.
	h := newHarness(t)
	h.o.publish(h.repoKey(), theWorkspace, MergeFacts{State: StateQueued, Step: footer.StepEnqueued, QueuedAt: h.clock()})

	// Act.
	h.o.publish(h.repoKey(), theWorkspace, merging(h, footer.StepTesting))

	// Assert.
	if kinds := h.events.kinds(); len(kinds) != 0 {
		t.Fatalf("changes = %v, want none", kinds)
	}
	if !loggedLike(h, logLike{level: "error", op: opQueueEvents, text: "a merge published a new step while not in its queue's line"}) {
		t.Fatalf("no error record: %+v", h.logs.Records())
	}
}

func TestALandingThatEmptiesTheLineIsReportedLandedAndDepleted(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	landing(h, 1)

	// Act.
	admitted(t, h)

	// Assert.
	change := lastChange(t, h)
	if change.Kind != QueueChangeLanded || !change.Depleted || len(change.Queue.Members) != 0 || change.Subject.Workspace != theWorkspace {
		t.Fatalf("last change = %+v, want the merge landed and the line empty", change)
	}
	if change.Cause != "" {
		t.Fatalf("cause = %q, want none on a landing", change.Cause)
	}
}

func TestAFailedMergeIsReportedFailed(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.emacsRepo()
	h.git.mergeErr = errors.New("refusing to merge unrelated histories")

	// Act.
	admitted(t, h)

	// Assert.
	if change := lastChange(t, h); change.Kind != QueueChangeFailed || change.Subject.Workspace != theWorkspace {
		t.Fatalf("last change = %+v, want the merge failed", change)
	}
}

func TestAnEvictionIsReportedDequeuedWithItsCause(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Act.
	if err := h.o.Evict(context.Background(), theWorkspace); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert.
	change := lastChange(t, h)
	if change.Kind != QueueChangeDequeued || change.Cause != string(CauseUserDrop) || change.Subject.Name != "ws-one" {
		t.Fatalf("change = %+v, want ws-one dequeued for %q", change, CauseUserDrop)
	}
	if change.Depleted || !reflect.DeepEqual(memberNames(change.Queue), []wsm.WorkspaceID{"ws-2"}) {
		t.Fatalf("change = %+v, want the queue left holding ws-2, not depleted", change)
	}
}

func TestTheLastMergeLeavingDepletesTheQueueOnce(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Act.
	for _, ws := range []wsm.WorkspaceID{theWorkspace, "ws-2"} {
		if err := h.o.Evict(context.Background(), ws); err != nil {
			t.Fatalf("Evict %s: %v", ws, err)
		}
	}

	// Assert.
	var depletions int
	for _, change := range h.events.reported() {
		if change.Depleted {
			depletions++
		}
	}
	if depletions != 1 || !lastChange(t, h).Depleted {
		t.Fatalf("depletions = %d, last = %+v; want exactly the last leave to deplete", depletions, lastChange(t, h))
	}
}

func TestRemovingAMergeOnlyRequestedReportsNothing(t *testing.T) {
	// Arrange: a request whose asking turn has not ended, so it is in no line.
	h := newHarness(t)
	if err := h.db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}

	// Act.
	err := h.o.deps.DB.RemoveMergeQueueEntry(context.Background(), h.repoKey(), theWorkspace, string(CauseUserDrop))

	// Assert.
	if err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if kinds := h.events.kinds(); len(kinds) != 0 {
		t.Fatalf("changes = %v, want none for a merge never in line", kinds)
	}
}

func TestAMergeARestartPutsBackInLineIsReportedRequeuedAndNeverLeft(t *testing.T) {
	// Arrange: an admitted merge a restart takes out of line to run again.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)
	if err := h.db.AdmitMerge(context.Background(), h.repoKey(), theWorkspace); err != nil {
		t.Fatalf("AdmitMerge: %v", err)
	}
	db := h.o.deps.DB

	// Act.
	if err := db.RemoveMergeQueueEntry(context.Background(), h.repoKey(), theWorkspace, causeRestartResume); err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if err := db.RequestMerge(context.Background(), h.repoKey(), theWorkspace, ownBranch, h.clock()); err != nil {
		t.Fatalf("RequestMerge: %v", err)
	}
	if _, err := db.QueueMerge(context.Background(), h.repoKey(), theWorkspace, "lease-again"); err != nil {
		t.Fatalf("QueueMerge: %v", err)
	}

	// Assert.
	if kinds := h.events.kinds(); !reflect.DeepEqual(kinds, []QueueChangeKind{QueueChangeRequeued}) {
		t.Fatalf("changes = %v, want only the requeue", kinds)
	}
}

func TestAMoveIsReportedReorderedInTheNewOrder(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	queueAnother(t, h, "ws-2", "two")

	// Act.
	if _, err := h.o.Move(context.Background(), "ws-2", wsm.MergeMoveUp); err != nil {
		t.Fatalf("Move: %v", err)
	}

	// Assert.
	change := lastChange(t, h)
	if change.Kind != QueueChangeReordered || change.Subject.Workspace != "ws-2" {
		t.Fatalf("change = %+v, want ws-2 reordered", change)
	}
	if got := memberNames(change.Queue); !reflect.DeepEqual(got, []wsm.WorkspaceID{"ws-2", theWorkspace}) {
		t.Fatalf("members = %v, want ws-2 first", got)
	}
}

func TestAMoveThatMovesNothingReportsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	enqueue(t, h)
	before := len(h.events.reported())

	// Act.
	if _, err := h.o.Move(context.Background(), theWorkspace, wsm.MergeMoveUp); err != nil {
		t.Fatalf("Move: %v", err)
	}

	// Assert.
	if got := len(h.events.reported()); got != before {
		t.Fatalf("changes = %v, want nothing reported for an unmoved merge", h.events.kinds())
	}
}

func TestAPauseAndAResumeAreReportedWithTheQueuesPausedState(t *testing.T) {
	tests := []struct {
		name       string
		act        func(h *harness) error
		wantKind   QueueChangeKind
		wantPaused bool
	}{
		{name: "pause", act: func(h *harness) error { return h.o.Pause(context.Background(), nil) }, wantKind: QueueChangePaused, wantPaused: true},
		{name: "resume", act: func(h *harness) error {
			if err := h.o.Pause(context.Background(), nil); err != nil {
				return err
			}
			return h.o.Unpause(context.Background(), nil)
		}, wantKind: QueueChangeResumed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)
			enqueue(t, h)

			// Act.
			if err := tt.act(h); err != nil {
				t.Fatalf("act: %v", err)
			}

			// Assert.
			change := lastChange(t, h)
			if change.Kind != tt.wantKind || change.Queue.Paused != tt.wantPaused || change.Subject != (QueueMember{}) {
				t.Fatalf("change = %+v, want %v with paused %v and no subject", change, tt.wantKind, tt.wantPaused)
			}
		})
	}
}

func TestAChangeWhoseQueueCannotBeReadIsRecordedAndTheWriteStands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.pausedReadErr = errors.New("the store is gone")

	// Act.
	enqueue(t, h)

	// Assert.
	if kinds := h.events.kinds(); len(kinds) != 0 {
		t.Fatalf("changes = %v, want none reported", kinds)
	}
	if !loggedLike(h, logLike{level: "error", op: opQueueEvents, text: "could not read the merge queue after its change",
		context: map[string]string{"change": "enqueued", "error": "the store is gone"}}) {
		t.Fatalf("no error record of the unread change: %+v", h.logs.Records())
	}
	queue, err := h.db.MergeQueue(context.Background(), h.repoKey())
	if err != nil || len(queue) != 1 || queue[0].State != wsm.MergeQueued {
		t.Fatalf("queue = %+v, %v; want the merge in line regardless", queue, err)
	}
}

func TestALeaveWhoseQueueCannotBeReadBeforehandIsRecordedAndTheWriteStands(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	rawInLine(t, h, theWorkspace)
	h.db.queueReadErr = errors.New("the store is gone")

	// Act.
	err := h.o.deps.DB.RemoveMergeQueueEntry(context.Background(), h.repoKey(), theWorkspace, string(CauseUserDrop))

	// Assert.
	if err != nil {
		t.Fatalf("RemoveMergeQueueEntry: %v", err)
	}
	if kinds := h.events.kinds(); len(kinds) != 0 {
		t.Fatalf("changes = %v, want none reported", kinds)
	}
	if !loggedLike(h, logLike{level: "error", op: opQueueEvents, text: "could not read the merge queue before a merge left it",
		context: map[string]string{"change": "dequeued", "error": "the store is gone"}}) {
		t.Fatalf("no error record of the unread change: %+v", h.logs.Records())
	}
}

func TestAFailedWriteReportsNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.queueErr = errors.New("the queue write failed")

	// Act.
	_, err := h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, By: RequestedByUser})

	// Assert.
	if err == nil {
		t.Fatal("Enqueue succeeded over a failing write")
	}
	if kinds := h.events.kinds(); len(kinds) != 0 {
		t.Fatalf("changes = %v, want none for a write that failed", kinds)
	}
}

func TestLeaveKindClassifiesEachRemovalCause(t *testing.T) {
	tests := []struct {
		cause string
		want  QueueChangeKind
	}{
		{cause: causeMerged, want: QueueChangeLanded},
		{cause: causeFailed, want: QueueChangeFailed},
		{cause: causeRestartFailed, want: QueueChangeFailed},
		{cause: causePickFailed, want: QueueChangeFailed},
		{cause: causeDetachedForkFailed, want: QueueChangeFailed},
		{cause: string(CauseUserDrop), want: QueueChangeDequeued},
		{cause: string(CauseUserDequeue), want: QueueChangeDequeued},
		{cause: string(CauseWorkspaceClosed), want: QueueChangeDequeued},
		{cause: string(CauseDaemonShutdown), want: QueueChangeDequeued},
	}
	for _, tt := range tests {
		t.Run(tt.cause, func(t *testing.T) {
			// Act.
			got := leaveKind(tt.cause)

			// Assert.
			if got != tt.want {
				t.Fatalf("leaveKind(%q) = %v, want %v", tt.cause, got, tt.want)
			}
		})
	}
}

func TestModeOfNamesHowAMergeWasAskedFor(t *testing.T) {
	tests := []struct {
		name   string
		source wsm.MergeSource
		record wsm.Workspace
		want   MergeMode
	}{
		{name: "a merge workspace's merge is async", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch},
			record: wsm.Workspace{MergeOf: &wsm.MergeOrigin{}}, want: MergeModeAsync},
		{name: "a detached request is async", source: wsm.MergeSource{Kind: wsm.MergeSourceBranch, Detach: "request-1"}, want: MergeModeAsync},
		{name: "an own branch closing its workspace is a one-shot", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}, want: MergeModeOneShot},
		{name: "a branch merged upstream is a one-shot", source: wsm.MergeSource{Kind: wsm.MergeSourceMergedUpstream}, want: MergeModeOneShot},
		{name: "an own branch kept open is in place", source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true}, want: MergeModeInPlace},
		{name: "another workspace's branch is in place", source: wsm.MergeSource{Kind: wsm.MergeSourceWorkspace}, want: MergeModeInPlace},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := modeOf(tt.source, tt.record)

			// Assert.
			if got != tt.want {
				t.Fatalf("modeOf = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestMemberOfTakesTheWorkspacesBranchForARequestRecordedWithNone(t *testing.T) {
	// Arrange.
	entry := wsm.MergeQueueEntry{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch}}

	// Act.
	got := memberOf(entry, wsm.Workspace{Name: "ws-one", Branch: "feature"})

	// Assert.
	if got.Branch != "feature" {
		t.Fatalf("branch = %q, want the workspace's own", got.Branch)
	}
}

func TestQueueChangeKindNamesAndLeaves(t *testing.T) {
	tests := []struct {
		kind       QueueChangeKind
		wantName   string
		wantLeaves bool
	}{
		{QueueChangeEnqueued, "enqueued", false},
		{QueueChangeRequeued, "requeued", false},
		{QueueChangeStarted, "started", false},
		{QueueChangeStepped, "stepped", false},
		{QueueChangeLanded, "landed", true},
		{QueueChangeFailed, "failed", true},
		{QueueChangeDequeued, "dequeued", true},
		{QueueChangeReordered, "reordered", false},
		{QueueChangePaused, "paused", false},
		{QueueChangeResumed, "resumed", false},
		{QueueChangeKind(0), "QueueChangeKind(0)", false},
	}
	for _, tt := range tests {
		t.Run(tt.wantName, func(t *testing.T) {
			// Act.
			name, leaves := tt.kind.String(), tt.kind.Leaves()

			// Assert.
			if name != tt.wantName || leaves != tt.wantLeaves {
				t.Fatalf("%d = %q leaves %v, want %q leaves %v", int(tt.kind), name, leaves, tt.wantName, tt.wantLeaves)
			}
		})
	}
}

func TestMergeModeNamesEachMode(t *testing.T) {
	tests := []struct {
		mode MergeMode
		want string
	}{
		{MergeModeOneShot, "one-shot"},
		{MergeModeAsync, "async"},
		{MergeModeInPlace, "in place"},
		{MergeMode(0), "MergeMode(0)"},
	}
	for _, tt := range tests {
		t.Run(tt.want, func(t *testing.T) {
			// Act.
			got := tt.mode.String()

			// Assert.
			if got != tt.want {
				t.Fatalf("String() = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestSubscribeRecordsTheSubscriptionInItsScope(t *testing.T) {
	tests := []struct {
		name     string
		scope    *RepositoryScope
		wantRepo func(h *harness) wsm.RepoKey
	}{
		{name: "every repository", wantRepo: func(*harness) wsm.RepoKey { return wsm.EveryRepository }},
		{name: "one repository", scope: &RepositoryScope{ID: theRepository}, wantRepo: func(h *harness) wsm.RepoKey { return wsm.RepoKey(h.repoRoot + "/.git") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			replaced, err := h.o.Subscribe(context.Background(), theWorkspace, tt.scope, wsm.MergeQueueDetailAdvances)

			// Assert.
			if err != nil || replaced {
				t.Fatalf("Subscribe = %v, %v; want a fresh subscription", replaced, err)
			}
			if len(h.db.subscriptions) != 1 || h.db.subscriptions[0].SubscribedAt.IsZero() {
				t.Fatalf("subscriptions = %+v, want one stamped with when it was taken", h.db.subscriptions)
			}
			got := h.db.subscriptions[0]
			got.SubscribedAt = time.Time{}
			if want := (wsm.MergeQueueSubscription{Subscriber: theWorkspace, Repo: tt.wantRepo(h), Detail: wsm.MergeQueueDetailAdvances}); got != want {
				t.Fatalf("subscription = %+v, want %+v", got, want)
			}
			if !loggedLike(h, logLike{level: "info", op: "daemon.merge.subscribe", text: "subscribed the workspace's agent",
				context: map[string]string{"detail": "advances", "replaced": "false"}}) {
				t.Fatalf("no record of the subscription: %+v", h.logs.Records())
			}
		})
	}
}

func TestSubscribingAgainAnswersReplaced(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.o.Subscribe(context.Background(), theWorkspace, nil, wsm.MergeQueueDetailAdvances); err != nil {
		t.Fatalf("Subscribe: %v", err)
	}

	// Act.
	replaced, err := h.o.Subscribe(context.Background(), theWorkspace, nil, wsm.MergeQueueDetailDepletes)

	// Assert.
	if err != nil || !replaced {
		t.Fatalf("Subscribe = %v, %v; want the standing subscription replaced", replaced, err)
	}
	if len(h.db.subscriptions) != 1 || h.db.subscriptions[0].Detail != wsm.MergeQueueDetailDepletes {
		t.Fatalf("subscriptions = %+v, want one at depletes", h.db.subscriptions)
	}
}

func TestSubscribeRefusesAnUnknownRepository(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.o.Subscribe(context.Background(), theWorkspace, &RepositoryScope{Dir: "/no/such/repository"}, wsm.MergeQueueDetailAdvances)

	// Assert.
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("Subscribe = %v, want the unknown_repository refusal", err)
	}
	if len(h.db.subscriptions) != 0 {
		t.Fatalf("subscriptions = %+v, want none", h.db.subscriptions)
	}
	if !loggedLike(h, logLike{op: "daemon.merge.subscribe", text: "refused a merge queue subscription for a repository the registry does not hold"}) {
		t.Fatalf("the refusal was not recorded through the shared scope resolution: %+v", h.logs.Records())
	}
}

func TestSubscribeRefusesAClosedSubscriber(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.subscribeErr = wsm.ErrSubscriberClosed

	// Act.
	_, err := h.o.Subscribe(context.Background(), theWorkspace, nil, wsm.MergeQueueDetailAdvances)

	// Assert.
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmSubscriberClosed || refusal.Workspace != theWorkspace {
		t.Fatalf("Subscribe = %v, want the subscriber_closed refusal", err)
	}
	if !loggedLike(h, logLike{level: "info", op: "daemon.merge.subscribe", text: "refused a merge queue subscription of a closed workspace",
		context: map[string]string{"arm": ArmSubscriberClosed}}) {
		t.Fatalf("no record of the refusal: %+v", h.logs.Records())
	}
}

func TestSubscribeRecordsAStoreFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.subscribeErr = errors.New("the store is gone")

	// Act.
	_, err := h.o.Subscribe(context.Background(), theWorkspace, nil, wsm.MergeQueueDetailAdvances)

	// Assert.
	if !errors.Is(err, h.db.subscribeErr) {
		t.Fatalf("Subscribe = %v, want the store's failure", err)
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.subscribe", text: "could not record a merge queue subscription",
		context: map[string]string{"error": "the store is gone", "subscriber": string(theWorkspace)}}) {
		t.Fatalf("no error record: %+v", h.logs.Records())
	}
}

func TestUnsubscribeEndsTheSubscriptionInItsScope(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	if _, err := h.o.Subscribe(context.Background(), theWorkspace, nil, wsm.MergeQueueDetailAdvances); err != nil {
		t.Fatalf("Subscribe: %v", err)
	}

	// Act.
	err := h.o.Unsubscribe(context.Background(), theWorkspace, nil)

	// Assert.
	if err != nil || len(h.db.subscriptions) != 0 {
		t.Fatalf("Unsubscribe = %v, subscriptions %+v; want it ended", err, h.db.subscriptions)
	}
	if !loggedLike(h, logLike{level: "info", op: "daemon.merge.unsubscribe", text: "ended the workspace's merge queue subscription"}) {
		t.Fatalf("no record of the end: %+v", h.logs.Records())
	}
}

func TestUnsubscribeWithNoSubscriptionIsNotSubscribed(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	err := h.o.Unsubscribe(context.Background(), theWorkspace, nil)

	// Assert.
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmNotSubscribed {
		t.Fatalf("Unsubscribe = %v, want the not_subscribed refusal", err)
	}
	if !loggedLike(h, logLike{level: "info", op: "daemon.merge.unsubscribe", text: "holds no merge queue subscription in that scope"}) {
		t.Fatalf("no record of the answer: %+v", h.logs.Records())
	}
}

func TestUnsubscribeRecordsAStoreFailure(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.db.unsubscribeErr = errors.New("the store is gone")

	// Act.
	err := h.o.Unsubscribe(context.Background(), theWorkspace, nil)

	// Assert.
	if !errors.Is(err, h.db.unsubscribeErr) {
		t.Fatalf("Unsubscribe = %v, want the store's failure", err)
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.unsubscribe", text: "could not end a merge queue subscription",
		context: map[string]string{"error": "the store is gone"}}) {
		t.Fatalf("no error record: %+v", h.logs.Records())
	}
}

// recordingObserver records the kinds it is told.
type recordingObserver struct{ kinds []QueueChangeKind }

func (r *recordingObserver) QueueChanged(change QueueChange) { r.kinds = append(r.kinds, change.Kind) }

func TestQueueObserversTellEveryObserverEachChange(t *testing.T) {
	// Arrange
	a, b := &recordingObserver{}, &recordingObserver{}
	observers := QueueObservers{a, b}

	// Act
	observers.QueueChanged(QueueChange{Kind: QueueChangeLanded})
	observers.QueueChanged(QueueChange{Kind: QueueChangePaused})

	// Assert
	want := []QueueChangeKind{QueueChangeLanded, QueueChangePaused}
	if !reflect.DeepEqual(a.kinds, want) || !reflect.DeepEqual(b.kinds, want) {
		t.Fatalf("told %v and %v, want both %v", a.kinds, b.kinds, want)
	}
}
