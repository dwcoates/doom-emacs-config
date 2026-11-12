package merge

import (
	"context"
	"errors"
	"slices"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// requestAsyncNamed asks, as the harness workspace's agent, for an async merge
// of the harness's branch under NAME, placed behind BEHIND when it is set.
func requestAsyncNamed(h *harness, name, behind string) error {
	return enqueueErr(h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: asyncSource, By: RequestedByAgent,
		Asker: theWorkspace, Detached: &DetachedRun{Maker: theWorkspace, Name: name}, PlaceBehind: behind}))
}

// recordsFor answers every record with one level, operation and message.
func (h *harness) recordsFor(level, operation, message string) []dlog.Record {
	var out []dlog.Record
	for _, rec := range h.logs.Records() {
		if string(rec.Level) == level && rec.Operation == operation && rec.Message == message {
			out = append(out, rec)
		}
	}
	return out
}

// lineOrder answers the workspace of every entry of the harness's queue, in
// queue order.
func (h *harness) lineOrder() []ids.WorkspaceID {
	entries, _ := h.db.MergeQueue(context.Background(), h.repoKey())
	var out []ids.WorkspaceID
	for _, entry := range entries {
		out = append(out, entry.Workspace)
	}
	return out
}

// asyncHarness is a harness whose forks each get a merge workspace of their
// own, with the harness workspace's agent mid-turn.
func asyncHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.detached.byName = true
	h.mu.Lock()
	h.inFlight = "turn-asking"
	h.mu.Unlock()
	return h
}

func TestTwoAsyncRequestsOfOneTurnAreBothAcceptedAndJoinTheLineInOrder(t *testing.T) {
	// Arrange
	h := asyncHarness(t)

	// Act: two requests of one turn, then that turn's end.
	errFirst := requestAsyncNamed(h, "first", "")
	errSecond := requestAsyncNamed(h, "second", "")
	h.runWait(t)

	// Assert: both accepted and recorded, then forked and put in line in the
	// order they were asked for.
	if errFirst != nil || errSecond != nil {
		t.Fatalf("Enqueue = %v, %v; want both accepted", errFirst, errSecond)
	}
	if recorded := h.recordsFor("info", "daemon.merge.enqueue", "recorded a merge request"); len(recorded) != 2 ||
		recorded[0].Context["name"] != "first" || recorded[1].Context["name"] != "second" || recorded[0].Context["turn"] != "turn-asking" {
		t.Fatalf("records = %+v, want both requests recorded with the turn that asked", recorded)
	}
	wantForks := []string{string(theWorkspace) + ":first:" + asyncBranch, string(theWorkspace) + ":second:" + asyncBranch}
	if forks := h.detached.forks; !slices.Equal(forks, wantForks) {
		t.Fatalf("forks = %v, want %v", forks, wantForks)
	}
	if line := h.lineOrder(); !slices.Equal(line, []ids.WorkspaceID{mergeWorkspaceNamed("first"), mergeWorkspaceNamed("second")}) {
		t.Fatalf("line = %v, want first then second", line)
	}
	if handed := h.recordsFor("info", "daemon.merge.detached", "handed a detached merge to its merge workspace"); len(handed) != 2 {
		t.Fatalf("hand-off records = %+v, want two", handed)
	}
}

func TestAnAsyncRequestMayBePlacedBehindAnEarlierRequestOfTheSameTurn(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	if err := requestAsyncNamed(h, "first", ""); err != nil {
		t.Fatalf("Enqueue(first): %v", err)
	}

	// Act
	err := requestAsyncNamed(h, "second", "first")
	h.runWait(t)

	// Assert: accepted, and its merge workspace waits on the first's.
	if err != nil {
		t.Fatalf("Enqueue(second) = %v, want it placed behind the first", err)
	}
	if recorded := h.recordsFor("info", "daemon.merge.enqueue", "recorded a merge request"); len(recorded) != 2 || recorded[1].Context["place_behind"] != "first" {
		t.Fatalf("records = %+v, want the second recorded placed behind the first", recorded)
	}
	second, ok := h.entryOf(mergeWorkspaceNamed("second"))
	if !ok || second.WaitsOn != mergeWorkspaceNamed("first") || second.WaitsOnName != "first" || second.Source.Behind != mergeWorkspaceNamed("first") {
		t.Fatalf("second's entry = %+v, %v; want it placed behind, and waiting on, the first's merge workspace", second, ok)
	}
	if line := h.lineOrder(); !slices.Equal(line, []ids.WorkspaceID{mergeWorkspaceNamed("first"), mergeWorkspaceNamed("second")}) {
		t.Fatalf("line = %v, want first then second", line)
	}
}

func TestAnAsyncRequestPlacedBehindAnUnknownMergeIsRefused(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	if err := requestAsyncNamed(h, "first", ""); err != nil {
		t.Fatalf("Enqueue(first): %v", err)
	}

	// Act
	err := requestAsyncNamed(h, "second", "nobody")

	// Assert: refused naming the anchor, recorded, and nothing of it kept.
	refusal, refused := Refused(err)
	if !refused || refusal.Arm != ArmPlacementBehindNotInLine || refusal.Fields["behind"] != "nobody" {
		t.Fatalf("Enqueue = %v, want placement_behind_not_in_line naming nobody", err)
	}
	if refused := h.recordsFor("info", "daemon.merge.enqueue", "refused a merge placed behind a merge not in its queue"); len(refused) != 1 || refused[0].Context["place_behind"] != "nobody" {
		t.Fatalf("records = %+v, want the refusal recorded with its anchor", refused)
	}
	if requests, _ := h.db.AsyncMergeRequestsOf(context.Background(), theWorkspace); len(requests) != 1 || requests[0].Source.DetachName != "first" {
		t.Fatalf("requests = %+v, want the first alone", requests)
	}
}

func TestTheOwnMergeOfATurnJoinsTheLineAfterItsAsyncRequests(t *testing.T) {
	// Arrange: an async request and the requester's own in-place merge, both
	// asked for in one turn.
	h := asyncHarness(t)
	if err := requestAsyncNamed(h, "first", ""); err != nil {
		t.Fatalf("Enqueue(async): %v", err)
	}
	if err := enqueueErr(h.o.Enqueue(context.Background(), Request{Workspace: theWorkspace, Source: wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch, KeepOpen: true},
		By: RequestedByAgent, Asker: theWorkspace})); err != nil {
		t.Fatalf("Enqueue(own): %v", err)
	}
	h.o.mu.Lock()
	own := h.o.requested[theWorkspace]
	h.o.mu.Unlock()

	// Act: the own merge's wait ends first.
	h.o.waitThenQueue(own)

	// Assert: the async request was handed off ahead of it.
	if line := h.lineOrder(); !slices.Equal(line, []ids.WorkspaceID{mergeWorkspaceNamed("first"), theWorkspace}) {
		t.Fatalf("line = %v, want the async merge then the own merge", line)
	}
}

func TestRecoverReArmsEveryPendingAsyncRequestAndHandsThemOffInOrder(t *testing.T) {
	// Arrange: two requests a restart caught before their hand-off.
	h := newHarness(t)
	h.detached.byName = true
	for _, name := range []string{"first", "second"} {
		source := asyncSource
		source.Detach, source.DetachName, source.DetachMaker = wsm.NewDetachedMergeID(), name, theWorkspace
		if err := h.db.RequestAsyncMerge(context.Background(), h.repoKey(), theWorkspace, "turn-gone", source, h.clock()); err != nil {
			t.Fatalf("RequestAsyncMerge(%s): %v", name, err)
		}
	}

	// Act
	err := h.o.Recover(context.Background())

	// Assert: re-armed once for the requester, and both handed off in order
	// once the wait runs.
	if err != nil {
		t.Fatalf("Recover = %v, want the requests re-armed", err)
	}
	if rearmed := h.recordsFor("info", "daemon.merge.recover", "re-armed the async merge requests still waiting for their turn to end"); len(rearmed) != 1 || rearmed[0].Context["workspace"] != string(theWorkspace) {
		t.Fatalf("records = %+v, want one re-arm of the requester", rearmed)
	}
	if forks := h.detached.forks; len(forks) != 0 {
		t.Fatalf("forks before the wait = %v, want none", forks)
	}
	h.runWait(t)
	if line := h.lineOrder(); !slices.Equal(line, []ids.WorkspaceID{mergeWorkspaceNamed("first"), mergeWorkspaceNamed("second")}) {
		t.Fatalf("line = %v, want first then second", line)
	}
}

func TestRecoverThatCannotReadTheAsyncRequestsFails(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.db.mu.Lock()
	h.db.asyncReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := h.o.Recover(context.Background())

	// Assert
	if err == nil {
		t.Fatal("Recover succeeded with the async requests unread")
	}
	if rec, ok := h.recordFor("error", "daemon.merge.recover"); !ok || rec.Context["error"] != "database is locked" {
		t.Fatalf("record = %+v, %v; want the failed read at ERROR with its cause", rec, ok)
	}
}

func TestAnAsyncRequestThatCannotBeRecordedIsAnErrorWithNoWait(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	h.db.mu.Lock()
	h.db.asyncRequestErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	err := requestAsyncNamed(h, "first", "")

	// Assert: the error, recorded at ERROR, the name let go, and no wait.
	if err == nil {
		t.Fatal("Enqueue succeeded with the request unrecorded")
	}
	if rec, ok := h.recordFor("error", "daemon.merge.enqueue"); !ok || rec.Context["error"] != "database is locked" || rec.Context["name"] != "first" {
		t.Fatalf("record = %+v, %v; want the failed record at ERROR with the request", rec, ok)
	}
	h.names.mu.Lock()
	claims, releases := len(h.names.claims), h.names.releases
	h.names.mu.Unlock()
	if claims != 1 || releases != 1 {
		t.Fatalf("claims = %d, releases = %d; want the one claim let go", claims, releases)
	}
	h.o.mu.Lock()
	_, waits := h.o.asyncWaits[theWorkspace]
	h.o.mu.Unlock()
	if waits {
		t.Fatal("a wait stands for a request never recorded")
	}
}

func TestAnAsyncWaitThatCannotReadItsRequestsRetiresAndRecordsWhy(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	if err := requestAsyncNamed(h, "first", ""); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.db.mu.Lock()
	h.db.asyncReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.runWait(t)

	// Assert: recorded at ERROR, nothing forked, and the wait retired.
	if rec, ok := h.recordFor("error", "daemon.merge.async_request"); !ok || rec.Context["error"] != "database is locked" || rec.Context["workspace"] != string(theWorkspace) {
		t.Fatalf("record = %+v, %v; want the failed read at ERROR", rec, ok)
	}
	if forks := h.detached.forks; len(forks) != 0 {
		t.Fatalf("forks = %v, want none", forks)
	}
	h.o.mu.Lock()
	_, waits := h.o.asyncWaits[theWorkspace]
	h.o.mu.Unlock()
	if waits {
		t.Fatal("the wait still stands after its read failed")
	}
}

func TestAClosedRequestersAsyncRequestsAreDropped(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	if err := requestAsyncNamed(h, "first", ""); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert: the request is gone, recorded, and its wait withdrawn.
	if requests, _ := h.db.AsyncMergeRequestsOf(context.Background(), theWorkspace); len(requests) != 0 {
		t.Fatalf("requests = %+v, want none", requests)
	}
	if dropped := h.recordsFor("info", "daemon.merge.workspace_closed", "dropped an async merge request its closed workspace never handed off"); len(dropped) != 1 || dropped[0].Context["name"] != "first" {
		t.Fatalf("records = %+v, want the drop recorded", dropped)
	}
	h.o.mu.Lock()
	_, waits := h.o.asyncWaits[theWorkspace]
	h.o.mu.Unlock()
	if waits {
		t.Fatal("the wait still stands after its requester closed")
	}
}

func TestAClosedRequestersAsyncRequestThatCannotBeDroppedIsRecorded(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	if err := requestAsyncNamed(h, "first", ""); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.db.mu.Lock()
	h.db.asyncDropErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.OnWorkspaceClosed(context.Background(), theWorkspace)

	// Assert
	if rec, ok := h.recordFor("error", "daemon.merge.workspace_closed"); !ok || rec.Context["error"] != "database is locked" || rec.Context["name"] != "first" {
		t.Fatalf("record = %+v, %v; want the failed drop at ERROR with the request", rec, ok)
	}
}

func TestAClosedRequestersAsyncRequestsThatCannotBeReadAreRecorded(t *testing.T) {
	// Arrange
	h := asyncHarness(t)
	h.db.mu.Lock()
	h.db.asyncReadErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.dropAsyncRequestsOf(context.Background(), theWorkspace)

	// Assert
	if rec, ok := h.recordFor("error", "daemon.merge.workspace_closed"); !ok || rec.Context["error"] != "database is locked" {
		t.Fatalf("record = %+v, %v; want the failed read at ERROR", rec, ok)
	}
}

func TestReadyRequestsAreTheLeadingRunWhoseTurnEnded(t *testing.T) {
	request := func(turn ids.TurnID) wsm.AsyncMergeRequest { return wsm.AsyncMergeRequest{Turn: turn} }
	tests := []struct {
		name     string
		requests []wsm.AsyncMergeRequest
		turn     ids.TurnID
		inFlight bool
		want     int
	}{
		{name: "no turn in flight readies every request", requests: []wsm.AsyncMergeRequest{request("t-1"), request("")}, want: 2},
		{name: "an ended turn's requests ahead of the turn in flight", requests: []wsm.AsyncMergeRequest{request("t-1"), request("t-1"), request("t-2")}, turn: "t-2", inFlight: true, want: 2},
		{name: "a request with no turn waits for the turn in flight", requests: []wsm.AsyncMergeRequest{request(""), request("t-1")}, turn: "t-2", inFlight: true, want: 0},
		{name: "the turn in flight's requests wait", requests: []wsm.AsyncMergeRequest{request("t-2")}, turn: "t-2", inFlight: true, want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := readyRequests(tt.requests, tt.turn, tt.inFlight)

			// Assert
			if len(got) != tt.want {
				t.Fatalf("readyRequests = %d requests, want %d", len(got), tt.want)
			}
		})
	}
}
