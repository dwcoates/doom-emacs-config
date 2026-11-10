package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// fakeDeliveries is the fleet's delivery exclusion: it records each hold and
// whether the work ran inside it, and runs the work.
type fakeDeliveries struct {
	held   []ids.WorkspaceID
	calls  []string
	inside bool
	err    error
}

func (d *fakeDeliveries) Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error {
	d.held = append(d.held, ws)
	d.calls = append(d.calls, what)
	if d.err != nil {
		return d.err
	}
	d.inside = true
	defer func() { d.inside = false }()
	return perform(ctx)
}

// parentAccountRoot is the account root pendingFork's parent session was
// spawned under.
const parentAccountRoot = "/accounts/parent"

// pendingFork arranges a parent with a conversation and a child owing a fork
// of it, with a session row and no vendor id yet.
func pendingFork(f *fleetFixture) (parent, child wsm.Workspace) {
	parent, child = f.workspace("parent"), f.workspace("child")
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-parent", ConfigDir: parentAccountRoot}
	f.db.sessions[child.ID] = wsm.Session{Workspace: child.ID, HostSessionID: "host-child"}
	if f.db.pendingForks == nil {
		f.db.pendingForks = map[ids.WorkspaceID]wsm.PendingFork{}
	}
	f.db.pendingForks[child.ID] = wsm.PendingFork{Workspace: child.ID, Parent: parent.ID}
	return parent, child
}

// forkPointResponse is a parent shim's fork point through RECORD at THROUGHMS
// with UNINHERITED open.
func forkPointResponse(record string, throughMs int64, uninherited ...string) *shimv1.GetForkPointResponse {
	turns := make([]*conversationv1.TurnId, 0, len(uninherited))
	for _, turn := range uninherited {
		turns = append(turns, &conversationv1.TurnId{Value: turn})
	}
	return &shimv1.GetForkPointResponse{Result: &shimv1.GetForkPointResponse_Success{Success: &shimv1.GetForkPointSuccess{
		Point: &shimv1.ForkPoint{Extent: &shimv1.ForkPoint_ThroughRecord{ThroughRecord: &shimv1.ForkPointThroughRecord{
			Record: &shimv1.ForkRecordToken{Value: record},
		}}},
		Through:          &conversationv1.ConversationThrough{AtMs: throughMs},
		UninheritedTurns: turns,
	}}}
}

func TestAForksFirstBringUpMakesTheForkThenResumesIt(t *testing.T) {
	// Arrange: no shim runs the parent.
	f := newFleetFixture(t)
	parent, child := pendingFork(f)
	f.client.response = startedResponse("forked-1")

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if len(f.client.forkSessionAsked) != 1 {
		t.Fatalf("ForkSession asked %d times, want once", len(f.client.forkSessionAsked))
	}
	asked := f.client.forkSessionAsked[0]
	if asked.GetParentVendorSessionId() != "vendor-parent" || asked.GetParentWorkspaceDir() != parent.Dir || asked.GetPoint().GetWhole() == nil {
		t.Fatalf("ForkSession asked %v, want the parent's whole transcript", asked)
	}
	if asked.GetParentAccountRoot() != parentAccountRoot {
		t.Fatalf("ForkSession parent_account_root = %q, want the root the parent was spawned under %q", asked.GetParentAccountRoot(), parentAccountRoot)
	}
	if !slices.Equal(f.deliveries.held, []ids.WorkspaceID{parent.ID}) {
		t.Fatalf("held = %v, want the parent's deliveries held once", f.deliveries.held)
	}
	resume := f.client.requests[len(f.client.requests)-1].GetResume()
	if resume.GetVendorSessionId() != "forked-1" {
		t.Fatalf("StartSession = %v, want a resume of the fork", f.client.requests[len(f.client.requests)-1])
	}
	if len(f.db.resolvedForks) != 1 {
		t.Fatalf("resolved forks = %+v, want the fork recorded once", f.db.resolvedForks)
	}
	lineage := f.db.resolvedForks[0].lineage
	if lineage.Parent != parent.ID || lineage.ParentBook != "book-parent" || lineage.ThroughMs != fixedNow.UnixMilli() || len(lineage.Uninherited) != 0 {
		t.Fatalf("lineage = %+v, want the parent's book bounded at now", lineage)
	}
	if got := f.feed.lineages[child.ID]; len(got) == 0 || len(got[len(got)-1]) != 1 || got[len(got)-1][0].Book.GetValue() != "book-parent" {
		t.Fatalf("lineages stated = %+v, want the parent's book", got)
	}
	if !slices.Contains(f.feed.freshBooks, child.ID) {
		t.Fatalf("fresh books = %v, want the fork's own book known empty", f.feed.freshBooks)
	}
}

func TestAForkNamesTheRootTheParentWasSpawnedUnderNotItsSelection(t *testing.T) {
	// Arrange: the reader selected another account for the parent, which the
	// parent has not been restarted into, so its transcript is still filed
	// under the root it was spawned under.
	f := newFleetFixture(t)
	parent, child := pendingFork(f)
	session := f.db.sessions[parent.ID]
	session.SelectedConfigDir = "/accounts/selected"
	f.db.sessions[parent.ID] = session
	f.client.response = startedResponse("forked-1")

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if got := f.client.forkSessionAsked[0].GetParentAccountRoot(); got != parentAccountRoot {
		t.Fatalf("ForkSession parent_account_root = %q, want the spawned-under root %q", got, parentAccountRoot)
	}
}

func TestAForkOfARunningParentCutsWhereTheParentsShimSays(t *testing.T) {
	// Arrange: the parent's shim runs and names a cut with a turn in flight.
	f := newFleetFixture(t)
	parent, child := pendingFork(f)
	if err := f.fleet.Start(context.Background(), parent.ID); err != nil {
		t.Fatalf("Start parent: %v", err)
	}
	f.client.forkPoint = forkPointResponse("rec-9", 777, "turn-open")
	f.client.response = startedResponse("forked-1")
	var inside []bool
	f.client.onFork = func(string) { inside = append(inside, f.deliveries.inside) }

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if err != nil {
		t.Fatalf("Start child: %v", err)
	}
	asked := f.client.forkSessionAsked[0]
	if asked.GetPoint().GetThroughRecord().GetRecord().GetValue() != "rec-9" {
		t.Fatalf("ForkSession point = %v, want the parent shim's cut", asked.GetPoint())
	}
	lineage := f.db.resolvedForks[0].lineage
	if lineage.ThroughMs != 777 || !slices.Equal(lineage.Uninherited, []ids.TurnID{"turn-open"}) {
		t.Fatalf("lineage = %+v, want the parent shim's bound and open turn", lineage)
	}
	if !slices.Equal(inside, []bool{true, true}) {
		t.Fatalf("calls inside the parent's hold = %v, want both shim calls held", inside)
	}
}

func TestAForkThatCannotBeMadeLeavesItOwedAndSaysWhy(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fleetFixture, parent wsm.Workspace)
		message string
	}{
		{
			name: "the parent's shim names no point",
			arrange: func(f *fleetFixture, parent wsm.Workspace) {
				if err := f.fleet.Start(context.Background(), parent.ID); err != nil {
					t.Fatalf("Start parent: %v", err)
				}
				f.client.forkPoint = &shimv1.GetForkPointResponse{Result: &shimv1.GetForkPointResponse_Failure{Failure: &shimv1.GetForkPointFailure{
					Cause:  &shimv1.GetForkPointFailure_Unresolved{Unresolved: &shimv1.GetForkPointUnresolved{}},
					Detail: "a turn is in flight",
				}}}
			},
			message: "the fork's parent shim could name no fork point",
		},
		{
			name: "the parent's shim cannot be asked",
			arrange: func(f *fleetFixture, parent wsm.Workspace) {
				if err := f.fleet.Start(context.Background(), parent.ID); err != nil {
					t.Fatalf("Start parent: %v", err)
				}
				f.client.forkPointErr = errors.New("socket closed")
			},
			message: "the fork's parent shim could not be asked for the fork point",
		},
		{
			name: "the child's shim refuses the fork",
			arrange: func(f *fleetFixture, _ wsm.Workspace) {
				f.client.forkSession = &shimv1.ForkSessionResponse{Result: &shimv1.ForkSessionResponse_Failure{Failure: &shimv1.ForkSessionFailure{
					Cause:  &shimv1.ForkSessionFailure_UnknownSession{UnknownSession: &shimv1.ForkSessionUnknownSession{}},
					Detail: "no transcript",
				}}}
			},
			message: "the child's shim refused the fork",
		},
		{
			name: "the child's shim cannot be asked",
			arrange: func(f *fleetFixture, _ wsm.Workspace) {
				f.client.forkSessionErr = errors.New("socket closed")
			},
			message: "the child's shim could not be asked for the fork",
		},
		{
			name: "the parent's conversation records no account root",
			arrange: func(f *fleetFixture, parent wsm.Workspace) {
				f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-parent"}
			},
			message: "the fork's parent conversation records no account root",
		},
		{
			name: "the parent has no conversation",
			arrange: func(f *fleetFixture, parent wsm.Workspace) {
				f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID}
			},
			message: "the fork's parent has no conversation to fork",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			parent, child := pendingFork(f)
			tc.arrange(f, parent)

			// Act
			err := f.fleet.Start(context.Background(), child.ID)

			// Assert
			if !errors.Is(err, ErrForkRefused) {
				t.Fatalf("Start = %v, want ErrForkRefused", err)
			}
			if _, owed := f.db.pendingForks[child.ID]; !owed || len(f.db.resolvedForks) != 0 {
				t.Fatalf("pending %v resolved %+v, want the fork still owed and nothing recorded", owed, f.db.resolvedForks)
			}
			if !fleetLogged(f, "error", opBringUp, tc.message) {
				t.Fatalf("records = %+v, want %q at error", f.log.logger.Records(), tc.message)
			}
		})
	}
}

func TestAForkMadeButNotRecordedFailsTheBringUp(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	_, child := pendingFork(f)
	f.db.resolveForkErr = errors.New("disk full")

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if err == nil {
		t.Fatal("Start succeeded with the fork unrecorded")
	}
	if !fleetLogged(f, "error", opBringUp, "the fork was made but could not be recorded; the next bring-up forks again") {
		t.Fatalf("records = %+v, want the unrecorded fork logged", f.log.logger.Records())
	}
	if len(f.client.requests) != 0 {
		t.Fatalf("StartSession asked %d times, want none for an unrecorded fork", len(f.client.requests))
	}
}

func TestAForkIsNotMadeWhenTheParentsDeliveriesCannotBeHeld(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	_, child := pendingFork(f)
	f.deliveries.err = errors.New("queue down")

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if err == nil || len(f.client.forkSessionAsked) != 0 {
		t.Fatalf("Start = %v with %d forks asked, want a failure and no fork", err, len(f.client.forkSessionAsked))
	}
}

func TestAForksLaterBringUpStatesItsRecordedLineage(t *testing.T) {
	// Arrange: a fork already made.
	f := newFleetFixture(t)
	parent, child := f.workspace("parent"), f.workspace("child")
	f.db.sessions[child.ID] = wsm.Session{Workspace: child.ID, VendorSessionID: "vendor-1"}
	f.db.lineage = map[ids.WorkspaceID][]wsm.ForkLineage{child.ID: {{
		Workspace: child.ID, Parent: parent.ID, ParentBook: "book-parent", ThroughMs: 5, Uninherited: []ids.TurnID{"t"},
	}}}

	// Act
	if err := f.fleet.Start(context.Background(), child.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert
	got := f.feed.lineages[child.ID]
	if len(got) != 1 || len(got[0]) != 1 {
		t.Fatalf("lineages stated = %+v, want the recorded generation once", got)
	}
	segment := got[0][0]
	if segment.Workspace != parent.ID || segment.Book.GetValue() != "book-parent" || segment.ThroughMs != 5 || !segment.Uninherited["t"] {
		t.Fatalf("segment = %+v, want the recorded lineage", segment)
	}
	if len(f.client.forkSessionAsked) != 0 {
		t.Fatal("a made fork was forked again")
	}
}

func TestABringUpFailsWhenTheLineageCannotBeRead(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := resumable(f)
	f.db.lineageErr = errors.New("db locked")

	// Act
	err := f.fleet.Start(context.Background(), ws)

	// Assert
	if err == nil || !fleetLogged(f, "error", opBringUp, "the workspace's fork lineage could not be read") {
		t.Fatalf("Start = %v, records %+v; want the failure logged", err, f.log.logger.Records())
	}
}

func TestABringUpFailsWhenThePendingForkCannotBeRead(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	_, child := pendingFork(f)
	f.db.pendingForkErr = errors.New("db locked")

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if err == nil || !fleetLogged(f, "error", opBringUp, "whether the workspace owes a fork could not be read") {
		t.Fatalf("Start = %v, records %+v; want the failure logged", err, f.log.logger.Records())
	}
}

func TestFleetReadInheritedReadsTheBookFromTheStoreAndTellsNoView(t *testing.T) {
	cases := []struct {
		name    string
		after   *conversationv1.HistoryPointer
		through int64
		want    string
	}{
		{name: "the first page of a generation reads through its bound", through: 42, want: "through book-parent 42"},
		{name: "a later page reads after the served pointer", after: &conversationv1.HistoryPointer{Value: "p-3"}, want: "before book-parent p-3"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			ws := parkAtColdGate(t, f)
			f.store.page = &conversationv1.HistoryPage{Boundary: &conversationv1.HistoryPage_Floor{Floor: &conversationv1.HistoryFloor{}}}

			// Act
			_, err := f.fleet.ReadInherited(context.Background(), ws, &conversationv1.AgentId{Value: "book-parent"}, tc.after, tc.through)

			// Assert
			if err != nil {
				t.Fatalf("ReadInherited: %v", err)
			}
			if len(f.store.asked) != 1 || f.store.asked[0] != tc.want {
				t.Fatalf("store asked %v, want %q", f.store.asked, tc.want)
			}
			if len(f.feed.mainAgents) != 0 {
				t.Fatalf("feed main agents = %v, want an inherited page to name none", f.feed.mainAgents)
			}
		})
	}
}

// fleetLogged reports whether the fleet fixture's log holds MESSAGE at LEVEL
// under OPERATION.
func fleetLogged(f *fleetFixture, level, operation, message string) bool {
	for _, r := range f.log.logger.Records() {
		if r.Level == level && r.Operation == operation && r.Message == message {
			return true
		}
	}
	return false
}

// nothingFinishedResponse is a parent shim answering that its turn in flight
// is its conversation's first.
func nothingFinishedResponse() *shimv1.GetForkPointResponse {
	return &shimv1.GetForkPointResponse{Result: &shimv1.GetForkPointResponse_Failure{Failure: &shimv1.GetForkPointFailure{
		Cause:  &shimv1.GetForkPointFailure_NothingFinished{NothingFinished: &shimv1.GetForkPointNothingFinished{}},
		Detail: "the turn-opening record r1 is the parent's first; no finished turn precedes it",
	}}}
}

func TestAForkOfAParentInItsFirstTurnIsRefusedAsNothingFinished(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	parent, child := pendingFork(f)
	if err := f.fleet.Start(context.Background(), parent.ID); err != nil {
		t.Fatalf("Start parent: %v", err)
	}
	f.client.forkPoint = nothingFinishedResponse()

	// Act
	err := f.fleet.Start(context.Background(), child.ID)

	// Assert
	if !errors.Is(err, ErrForkRefused) || !errors.Is(err, ErrForkNothingFinished) {
		t.Fatalf("Start = %v, want ErrForkRefused wrapping ErrForkNothingFinished", err)
	}
	if len(f.client.forkSessionAsked) != 0 {
		t.Fatalf("ForkSession asked %d times, want the child's shim never asked", len(f.client.forkSessionAsked))
	}
	if !fleetLogged(f, "info", opBringUp, "the fork's parent is in its conversation's first turn; there is no finished turn to fork") {
		t.Fatalf("records = %+v, want the first turn recorded at info", f.log.logger.Records())
	}
}

func TestProbeForkPointAnswersWhetherAForkCouldBeMade(t *testing.T) {
	cases := []struct {
		name      string
		running   bool
		forkPoint *shimv1.GetForkPointResponse
		pointErr  error
		want      error
	}{
		{name: "a parent with no running shim forks whole", running: false, want: nil},
		{name: "a parent that concluded a turn has a point", running: true, forkPoint: forkPointResponse("rec-1", 1), want: nil},
		{name: "a parent in its first turn has nothing finished", running: true, forkPoint: nothingFinishedResponse(), want: ErrForkNothingFinished},
		{name: "a parent whose shim cannot be asked fails", running: true, pointErr: errors.New("socket closed"), want: ErrForkRefused},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			parent, _ := pendingFork(f)
			if tc.running {
				if err := f.fleet.Start(context.Background(), parent.ID); err != nil {
					t.Fatalf("Start parent: %v", err)
				}
			}
			f.client.forkPoint, f.client.forkPointErr = tc.forkPoint, tc.pointErr

			// Act
			err := f.fleet.ProbeForkPoint(context.Background(), f.log.logger, parent.ID)

			// Assert: the answer, and no delivery held for a read.
			if tc.want == nil && err != nil || tc.want != nil && !errors.Is(err, tc.want) {
				t.Fatalf("ProbeForkPoint = %v, want %v", err, tc.want)
			}
			if len(f.deliveries.held) != 0 {
				t.Fatalf("deliveries held = %v, want none for a probe", f.deliveries.held)
			}
		})
	}
}

// ownAccountRoot is the account root ownPendingFork's session records.
const ownAccountRoot = "/accounts/own"

// ownPendingFork arranges a workspace bound to a fork of its own queue
// conversation, as the first switch into the queue bypass mode leaves it.
func ownPendingFork(f *fleetFixture) wsm.Workspace {
	ws := f.workspace("own")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, HostSessionID: "host-own", ConfigDir: ownAccountRoot}
	if f.db.pendingForks == nil {
		f.db.pendingForks = map[ids.WorkspaceID]wsm.PendingFork{}
	}
	f.db.pendingForks[ws.ID] = wsm.PendingFork{Workspace: ws.ID, Parent: ws.ID, ParentVendorSessionID: "vendor-queue"}
	return ws
}

func TestAnOwnForksBringUpForksTheWholeConversationUnderNoHold(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := ownPendingFork(f)
	f.client.response = startedResponse("forked-own")

	// Act
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if len(f.client.forkSessionAsked) != 1 {
		t.Fatalf("ForkSession asked %d times, want once", len(f.client.forkSessionAsked))
	}
	asked := f.client.forkSessionAsked[0]
	if asked.GetParentVendorSessionId() != "vendor-queue" || asked.GetParentWorkspaceDir() != ws.Dir || asked.GetParentAccountRoot() != ownAccountRoot || asked.GetPoint().GetWhole() == nil {
		t.Fatalf("ForkSession asked %v, want the whole queue conversation under the workspace's own root", asked)
	}
	if len(f.deliveries.held) != 0 {
		t.Fatalf("held = %v, want no deliveries held for a conversation nothing drives", f.deliveries.held)
	}
	if got := f.client.requests[len(f.client.requests)-1].GetResume().GetVendorSessionId(); got != "forked-own" {
		t.Fatalf("StartSession resumed %q, want the fork", got)
	}
}

func TestAnOwnForkRecordsTheWorkspaceAsItsOwnParent(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := ownPendingFork(f)
	f.client.response = startedResponse("forked-own")

	// Act
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if len(f.db.resolvedForks) != 1 {
		t.Fatalf("resolved forks = %+v, want the fork recorded once", f.db.resolvedForks)
	}
	got := f.db.resolvedForks[0]
	if got.vendorSessionID != "forked-own" || got.lineage.Workspace != ws.ID || got.lineage.Parent != ws.ID || got.lineage.ThroughMs != fixedNow.UnixMilli() {
		t.Fatalf("resolved = %+v, want the fork of the workspace's own conversation bounded at now", got)
	}
	if _, owed := f.db.pendingForks[ws.ID]; owed {
		t.Fatal("the own fork is still owed after it was made")
	}
}

func TestAnOwnForkOfASessionRecordingNoAccountRootIsRefused(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := ownPendingFork(f)
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, HostSessionID: "host-own"}

	// Act
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert
	if !errors.Is(err, ErrForkRefused) {
		t.Fatalf("Start = %v, want ErrForkRefused", err)
	}
	if _, owed := f.db.pendingForks[ws.ID]; !owed || len(f.db.resolvedForks) != 0 || len(f.client.forkSessionAsked) != 0 {
		t.Fatalf("pending %v resolved %+v asked %d, want the fork still owed and nothing made", owed, f.db.resolvedForks, len(f.client.forkSessionAsked))
	}
	if !fleetLogged(f, "error", opBringUp, "the workspace whose own conversation is forked records no account root") {
		t.Fatalf("records = %+v, want the missing root at error", f.log.logger.Records())
	}
}
