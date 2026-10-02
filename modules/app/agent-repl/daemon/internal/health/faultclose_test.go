package health

import (
	"context"
	"errors"
	"sort"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// closedAt is the instant every edge in these tests fires at.
var closedAt = fixedNow.Add(90 * time.Second)

// wantStandingEdges is the plan's lifetime for every STANDING kind, stated a
// second time on purpose: the table in lifetime.go is what the daemon runs,
// and this is what the owner approved it to say.
var wantStandingEdges = map[string][]Edge{
	KindAdoptionWindowExpired: {EdgeHealthyAttach, EdgeSessionStarted, EdgeSuccessorServing},
	KindLogSinkPoisoned:       {EdgeDaemonBoot},
	KindWsmReadOnly:           {EdgeDaemonBoot},
	KindSuccessorSpawnFailed:  {EdgeSuccessorServing, EdgeSuperseded, EdgeDaemonBoot},
	KindPromptsDirMissing:     {EdgePromptsDirServed},
	KindDeployFailed:          {EdgeDeployStepSucceeded, EdgeSuperseded, EdgeDaemonBoot},
	KindShimStartFailed:       {EdgeHealthyAttach},
	KindShimDied:              {EdgeHealthyAttach},
	KindLinkSevered:           {EdgeHealthyAttach, EdgeShimDeathRecorded},
	KindWatchOpenRefused:      {EdgeHealthyAttach},
	KindResumeFailed:          {EdgeSessionStarted},
	KindVendorStartRetrying:   {EdgeSessionStarted},
	KindVendorStartRejected:   {EdgeSessionStarted},
	KindVendorStartFailed:     {EdgeSessionStarted},
	KindRelaunchResumeFailed:  {EdgeSessionStarted},
	KindColdGateReopenFailed:  {EdgeSessionStarted},
	KindBounceUnknown:         {EdgeHealthyAttach, EdgeSessionStarted},
	KindShimReported:          {EdgeShimDiagnostics},
	KindClassifierFailed:      {EdgeTurnStarted},
	KindConversationAbandoned: {EdgeTurnStarted},
	KindFinalAnswerUnresolved: {EdgeAnswerArrived, EdgeTurnStarted, EdgeSuperseded, EdgeFeedReset},
}

// wantMomentary is every kind the plan declares momentary.
var wantMomentary = []string{KindBounceDied, KindBounceDisposition, KindSessionAbsent, KindStateUnreadable}

// allEdges answers every recovery edge, sorted.
func allEdges(t *testing.T) []Edge {
	t.Helper()
	var out []Edge
	for _, value := range edgeConsts(t) {
		out = append(out, Edge(value))
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

func TestEveryStandingKindClosesOnItsEdgesAndNoOther(t *testing.T) {
	type row struct {
		name  string
		kind  string
		edge  Edge
		close bool
	}
	var tests []row
	for kind, edges := range wantStandingEdges {
		declared := map[Edge]bool{}
		for _, e := range edges {
			declared[e] = true
		}
		for _, edge := range allEdges(t) {
			tests = append(tests, row{kind + " on " + string(edge), kind, edge, declared[edge]})
		}
	}
	sort.Slice(tests, func(i, j int) bool { return tests[i].name < tests[j].name })

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := newMemFaults()
			id := store.seed(wsm.Fault{Kind: tt.kind, OpenedAt: fixedNow})

			// Act
			closed, err := CloseOnEdge(context.Background(), store, dlog.NewTestLogger(), tt.edge, EdgeScope{}, closedAt)

			// Assert
			if err != nil {
				t.Fatalf("CloseOnEdge: %v", err)
			}
			if got := !store.standing(id); got != tt.close {
				t.Fatalf("%q closed on %q = %v, want %v", tt.kind, tt.edge, got, tt.close)
			}
			if want := map[bool]int{true: 1, false: 0}[tt.close]; closed != want {
				t.Fatalf("CloseOnEdge answered %d closed, want %d", closed, want)
			}
		})
	}
}

func TestEveryKindIsDeclaredEitherStandingOrMomentaryAsThePlanSays(t *testing.T) {
	for kind := range faultLifetimes {
		t.Run(kind, func(t *testing.T) {
			// Arrange
			_, standingKind := wantStandingEdges[kind]
			momentaryKind := false
			for _, m := range wantMomentary {
				momentaryKind = momentaryKind || m == kind
			}

			// Act
			got := Momentary(kind)

			// Assert
			if standingKind == momentaryKind {
				t.Fatalf("%q is in neither or both of the plan's lists", kind)
			}
			if got != momentaryKind {
				t.Fatalf("Momentary(%q) = %v, want %v", kind, got, momentaryKind)
			}
		})
	}
}

func TestAMomentaryKindNeverStands(t *testing.T) {
	for _, kind := range wantMomentary {
		t.Run(kind, func(t *testing.T) {
			// Arrange
			store := newMemFaults()
			ws := ids.WorkspaceID("ws-1")

			// Act
			id, err := RecordMomentary(context.Background(), store, dlog.NewTestLogger(),
				wsm.Fault{Workspace: &ws, Kind: kind}, closedAt)

			// Assert
			if err != nil {
				t.Fatalf("RecordMomentary: %v", err)
			}
			if store.standing(id) {
				t.Fatalf("the momentary %q still stands after it was recorded", kind)
			}
		})
	}
}

func TestRecordMomentaryRefusesAStandingKind(t *testing.T) {
	// Arrange
	store := newMemFaults()
	log := dlog.NewTestLogger()

	// Act
	_, err := RecordMomentary(context.Background(), store, log, wsm.Fault{Kind: KindBounceUnknown}, closedAt)

	// Assert
	if err == nil {
		t.Fatalf("RecordMomentary recorded a standing kind as momentary")
	}
	if len(store.faults) != 0 {
		t.Fatalf("a refused momentary record was written: %+v", store.faults)
	}
}

func TestAMomentaryRecordWhoseCloseFailsSaysSo(t *testing.T) {
	// Arrange
	store := newMemFaults()
	store.closeErr = errors.New("state client closed")

	// Act
	_, err := RecordMomentary(context.Background(), store, dlog.NewTestLogger(),
		wsm.Fault{Kind: KindBounceDisposition}, closedAt)

	// Assert
	if err == nil {
		t.Fatalf("RecordMomentary answered no error for a record it could not close")
	}
}

func TestACloseIsRecordedAtInfoWithKindFaultEdgeAndHowLongItStood(t *testing.T) {
	// Arrange
	store := newMemFaults()
	ws := ids.WorkspaceID("ws-1")
	id := store.seed(wsm.Fault{Workspace: &ws, Kind: KindBounceUnknown, OpenedAt: fixedNow})
	log := dlog.NewTestLogger()

	// Act
	CloseOnEdge(context.Background(), store, log, EdgeHealthyAttach, EdgeScope{Workspace: &ws}, closedAt)

	// Assert
	want := dlog.Context{
		"kind": KindBounceUnknown, "fault": string(id), "edge": string(EdgeHealthyAttach),
		"workspace": string(ws), "stood": "1m30s",
	}
	for _, r := range log.Records() {
		if r.Level != "info" || r.Operation != opCloseOnEdge {
			continue
		}
		for key, value := range want {
			if r.Context[key] != value {
				t.Fatalf("record context %s = %v, want %v (record %+v)", key, r.Context[key], value, r)
			}
		}
		return
	}
	t.Fatalf("records = %+v, want the close at INFO", log.Records())
}

func TestAnEdgeClosesOnlyTheWorkspaceItFiredFor(t *testing.T) {
	// Arrange
	store := newMemFaults()
	mine, other := ids.WorkspaceID("ws-1"), ids.WorkspaceID("ws-2")
	theirs := store.seed(wsm.Fault{Workspace: &other, Kind: KindShimDied, OpenedAt: fixedNow})

	// Act
	CloseOnEdge(context.Background(), store, dlog.NewTestLogger(), EdgeHealthyAttach, EdgeScope{Workspace: &mine}, closedAt)

	// Assert
	if !store.standing(theirs) {
		t.Fatalf("an edge fired for %q closed a fault of %q", mine, other)
	}
}

func TestADaemonOnlyEdgeLeavesAWorkspaceScopedFaultStanding(t *testing.T) {
	// Arrange
	store := newMemFaults()
	ws := ids.WorkspaceID("ws-1")
	id := store.seed(wsm.Fault{Workspace: &ws, Kind: KindAdoptionWindowExpired, OpenedAt: fixedNow})

	// Act
	CloseOnEdge(context.Background(), store, dlog.NewTestLogger(), EdgeSuccessorServing, EdgeScope{DaemonOnly: true}, closedAt)

	// Assert
	if !store.standing(id) {
		t.Fatalf("a daemon-only edge closed a workspace-scoped fault")
	}
}

func TestAnEdgeScopeMatchLeavesTheFaultsItIsNotAbout(t *testing.T) {
	// Arrange
	store := newMemFaults()
	build := store.seed(wsm.Fault{Kind: KindDeployFailed, Evidence: map[string]string{"step": DeployStepBuild}, OpenedAt: fixedNow})
	match := func(f wsm.Fault) bool { return f.Evidence["step"] == DeployStepInstall }

	// Act
	CloseOnEdge(context.Background(), store, dlog.NewTestLogger(), EdgeDeployStepSucceeded, EdgeScope{Match: match}, closedAt)

	// Assert
	if !store.standing(build) {
		t.Fatalf("an edge scoped to the install step closed the build step's fault")
	}
}

func TestAnUnreadableFaultSetIsAnErrorAndClosesNothing(t *testing.T) {
	// Arrange
	store := newMemFaults()
	store.readErr = errors.New("state client closed")
	log := dlog.NewTestLogger()

	// Act
	closed, err := CloseOnEdge(context.Background(), store, log, EdgeHealthyAttach, EdgeScope{}, closedAt)

	// Assert
	if err == nil || closed != 0 {
		t.Fatalf("CloseOnEdge = (%d, %v), want (0, an error)", closed, err)
	}
	if !loggedAt(log, "error", "could not read the standing faults a recovery edge closes; they stand") {
		t.Fatalf("records = %+v, want the unreadable faults at ERROR", log.Records())
	}
}

func TestAFaultThatCannotBeClosedStandsAndSaysSoAtError(t *testing.T) {
	// Arrange
	store := newMemFaults()
	id := store.seed(wsm.Fault{Kind: KindSuccessorSpawnFailed, OpenedAt: fixedNow})
	store.closeErr = errors.New("state client closed")
	log := dlog.NewTestLogger()

	// Act
	closed, _ := CloseOnEdge(context.Background(), store, log, EdgeSuccessorServing, EdgeScope{}, closedAt)

	// Assert
	if closed != 0 || !store.standing(id) {
		t.Fatalf("a close that failed was counted or took effect")
	}
	if !loggedAt(log, "error", "could not close a standing fault on its recovery edge; it stands") {
		t.Fatalf("records = %+v, want the failed close at ERROR", log.Records())
	}
}

func TestCloseFaultOnRefusesAnEdgeTheKindDoesNotDeclare(t *testing.T) {
	// Arrange
	store := newMemFaults()
	id := store.seed(wsm.Fault{Kind: KindShimDied, OpenedAt: fixedNow})
	log := dlog.NewTestLogger()

	// Act
	ok := CloseFaultOn(context.Background(), store, log, EdgeTurnStarted,
		wsm.Fault{ID: id, Kind: KindShimDied, OpenedAt: fixedNow}, closedAt)

	// Assert
	if ok || !store.standing(id) {
		t.Fatalf("CloseFaultOn closed a fault on an edge its kind does not declare")
	}
	if !loggedAt(log, "error", "refused to close a fault on an edge its kind does not declare; it stands") {
		t.Fatalf("records = %+v, want the refusal at ERROR", log.Records())
	}
}

func TestTheFooterLineClearsWhenItsFaultClosesOnItsEdge(t *testing.T) {
	// Arrange
	store := newMemFaults()
	sink := &recordingSink{}
	observed := ObserveFaults(store, sink, newStubSurfaces())
	ws := ids.WorkspaceID("ws-1")
	id, err := observed.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: KindBounceUnknown, OpenedAt: fixedNow})
	if err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act
	CloseOnEdge(context.Background(), observed, dlog.NewTestLogger(), EdgeHealthyAttach, EdgeScope{Workspace: &ws}, closedAt)

	// Assert
	if len(sink.closed) != 1 || sink.closed[0].id != id || sink.closed[0].ws != ws {
		t.Fatalf("sink retractions = %+v, want the line of %s on %q cleared", sink.closed, id, ws)
	}
}

func TestTheFooterLineStandsOnAnEdgeThatIsNotItsOwn(t *testing.T) {
	// Arrange
	store := newMemFaults()
	sink := &recordingSink{}
	observed := ObserveFaults(store, sink, newStubSurfaces())
	ws := ids.WorkspaceID("ws-1")
	if _, err := observed.OpenFault(context.Background(), wsm.Fault{Workspace: &ws, Kind: KindBounceUnknown, OpenedAt: fixedNow}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act
	CloseOnEdge(context.Background(), observed, dlog.NewTestLogger(), EdgeTurnStarted, EdgeScope{Workspace: &ws}, closedAt)

	// Assert
	if len(sink.closed) != 0 {
		t.Fatalf("sink retractions = %+v, want the line still drawn", sink.closed)
	}
}

func TestReporterFaultsClosesThroughTheReporter(t *testing.T) {
	// Arrange
	db := &stubDB{faults: []wsm.Fault{{ID: "fault-9", Kind: KindPromptsDirMissing, OpenedAt: fixedNow}}}
	reporter := newReporter(t, db, alwaysLive, newStubSurfaces())

	// Act
	closed, err := CloseOnEdge(context.Background(), ReporterFaults(reporter), dlog.NewTestLogger(),
		EdgePromptsDirServed, EdgeScope{DaemonOnly: true}, closedAt)

	// Assert
	if err != nil || closed != 1 || db.closedID != "fault-9" {
		t.Fatalf("CloseOnEdge = (%d, %v) closing %q, want (1, nil) closing fault-9", closed, err, db.closedID)
	}
}

// loggedAt reports whether the logger captured a record at level with msg
// under the edge-close operation.
func loggedAt(log *dlog.TestLogger, level, msg string) bool {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == opCloseOnEdge && r.Message == msg {
			return true
		}
	}
	return false
}
