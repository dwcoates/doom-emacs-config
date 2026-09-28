package main

import (
	"context"
	"errors"
	"fmt"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/rollout"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/wsm"
)

// fakeReporter records the fault opens and closes the lifecycle sink makes.
type fakeReporter struct {
	standing []wsm.Fault
	opened   []wsm.Fault
	closed   []ids.FaultID
	next     int
	// openErr scripts the state client's refusal of an OpenFault.
	openErr error
}

func (r *fakeReporter) Daemon(context.Context) (*agentreplv1.DaemonHealthResponse, error) {
	return nil, fmt.Errorf("not used")
}

func (r *fakeReporter) Session(context.Context, ids.WorkspaceID) (*agentreplv1.SessionHealthResponse, error) {
	return nil, fmt.Errorf("not used")
}

func (r *fakeReporter) OpenFault(_ context.Context, f wsm.Fault) (ids.FaultID, error) {
	if r.openErr != nil {
		return "", r.openErr
	}
	r.next++
	id := ids.FaultID(fmt.Sprintf("f%d", r.next))
	f.ID = id
	r.opened = append(r.opened, f)
	return id, nil
}

func (r *fakeReporter) CloseFault(_ context.Context, id ids.FaultID) error {
	r.closed = append(r.closed, id)
	return nil
}

func (r *fakeReporter) OpenFaults(context.Context, wsm.FaultScope) ([]wsm.Fault, error) {
	return r.standing, nil
}

func newDiagnosticsSink(t *testing.T, reporter health.Reporter) *lifecycleSink {
	t.Helper()
	ref := &healthForwarder{}
	ref.bind(reporter)
	builds := &rolloutForwarder{}
	builds.bind(&fakeBuildJudge{})
	return &lifecycleSink{health: ref, builds: builds, log: dlog.NewTestLogger()}
}

// fakeBuildJudge records the shim builds the lifecycle sink reports.
type fakeBuildJudge struct {
	rollout.Controller
	reported []string
}

func (f *fakeBuildJudge) ShimReported(ws ids.WorkspaceID, build string) {
	f.reported = append(f.reported, string(ws)+"="+build)
}

// fakeFreeQueue records the freeness edges the lifecycle sink hands on.
type fakeFreeQueue struct {
	promptqueue.Queue
	frees      []ids.WorkspaceID
	departures []departedAt
	unobserved map[ids.WorkspaceID][]ids.TurnID
	adopted    []string
}

func (f *fakeFreeQueue) OnTurnAdopted(ws ids.WorkspaceID, turn ids.TurnID) {
	f.adopted = append(f.adopted, string(ws)+"/"+string(turn))
}

func (f *fakeFreeQueue) OnTurnsEndedUnobserved(ws ids.WorkspaceID, turns []ids.TurnID) {
	if f.unobserved == nil {
		f.unobserved = map[ids.WorkspaceID][]ids.TurnID{}
	}
	f.unobserved[ws] = append(f.unobserved[ws], turns...)
}

func (f *fakeFreeQueue) OnFree(ws ids.WorkspaceID) { f.frees = append(f.frees, ws) }

func (f *fakeFreeQueue) OnDeparted(ws ids.WorkspaceID, departed promptqueue.Watcher, departure sessionwatcher.Departure) {
	f.departures = append(f.departures, departedAt{ws: ws, departed: departed, departure: departure})
}

// departedAt is one departure edge the queue was handed.
type departedAt struct {
	ws        ids.WorkspaceID
	departed  promptqueue.Watcher
	departure sessionwatcher.Departure
}

// departingWatcher is a stand-in for the watcher a departure names.
type departingWatcher struct{ sessionwatcher.Watcher }

func unhealthyDiagnostics(faults ...*conversationv1.SessionFault) *conversationv1.SessionDiagnostics {
	return &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Unhealthy{
			Unhealthy: &conversationv1.SessionUnhealthy{Faults: faults},
		},
	}
}

func TestAnUnhealthyDiagnosticsPushOpensAShimReportedFault(t *testing.T) {
	// Arrange
	reporter := &fakeReporter{}
	sink := newDiagnosticsSink(t, reporter)

	// Act
	sink.OnSessionDiagnostics("ws-1", unhealthyDiagnostics(&conversationv1.SessionFault{
		Component: "store client",
		Detail:    "the store socket went away",
		Kind: &conversationv1.SessionFault_StoreUnreachable{
			StoreUnreachable: &conversationv1.SessionFaultStoreUnreachable{},
		},
	}))

	// Assert
	if len(reporter.opened) != 1 {
		t.Fatalf("opened %d faults, want exactly 1", len(reporter.opened))
	}
	got := reporter.opened[0]
	if got.Kind != health.KindShimReported {
		t.Fatalf("the opened fault's kind = %q, want %q", got.Kind, health.KindShimReported)
	}
	if got.Evidence["component"] != "store client" || got.Evidence["kind"] != "store_unreachable" {
		t.Fatalf("the opened fault's evidence = %v, want the shim's component and kind", got.Evidence)
	}
}

func TestAHealthyDiagnosticsPushRetractsTheStandingShimReportedFault(t *testing.T) {
	// Arrange
	ws := wsm.WorkspaceID("ws-1")
	reporter := &fakeReporter{standing: []wsm.Fault{
		{ID: "f-old", Workspace: &ws, Kind: health.KindShimReported},
	}}
	sink := newDiagnosticsSink(t, reporter)

	// Act
	sink.OnSessionDiagnostics("ws-1", &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
	})

	// Assert
	if len(reporter.closed) != 1 || reporter.closed[0] != "f-old" {
		t.Fatalf("closed %v, want exactly the standing shim-reported fault", reporter.closed)
	}
	if len(reporter.opened) != 0 {
		t.Fatalf("opened %v, want nothing on a healthy verdict", reporter.opened)
	}
}

func TestADiagnosticsPushLeavesAFaultOfAnotherKindStanding(t *testing.T) {
	// Arrange
	ws := wsm.WorkspaceID("ws-1")
	reporter := &fakeReporter{standing: []wsm.Fault{
		{ID: "f-link", Workspace: &ws, Kind: health.KindLinkSevered},
	}}
	sink := newDiagnosticsSink(t, reporter)

	// Act
	sink.OnSessionDiagnostics("ws-1", &conversationv1.SessionDiagnostics{
		Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}},
	})

	// Assert
	if len(reporter.closed) != 0 {
		t.Fatalf("closed %v, want a fault the shim never reported left standing", reporter.closed)
	}
}

func TestADiagnosticsPushWithNoBoundReporterIsRecordedNotDropped(t *testing.T) {
	// Arrange
	builds := &rolloutForwarder{}
	builds.bind(&fakeBuildJudge{})
	sink := &lifecycleSink{health: &healthForwarder{}, builds: builds, log: dlog.NewTestLogger()}

	// Act / Assert: an unbound forwarder must not panic; the boot-order defect
	// is surfaced through the error record instead.
	sink.OnSessionDiagnostics("ws-1", unhealthyDiagnostics())
}

// TestOnLinkFaultLevelsAForgottenWorkspaceAtDebug is the outermost layer of the
// shim-death cascade. This sink outlives the registry row, so a shim dying
// after its workspace was forgotten reaches it about a workspace nothing can
// carry a fault for -- three ERRORs deep, once per death, for a condition with
// nothing to remediate.
func TestOnLinkFaultLevelsAForgottenWorkspaceAtDebug(t *testing.T) {
	tests := []struct {
		name      string
		openErr   error
		wantLevel string
	}{
		{
			name:      "the workspace was forgotten",
			openErr:   fmt.Errorf("health: open fault %q: %w", "shim_died", wsm.ErrNotFound),
			wantLevel: "debug",
		},
		{
			name:      "the state client failed",
			openErr:   errors.New("disk is gone"),
			wantLevel: "error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			sink := newDiagnosticsSink(t, &fakeReporter{openErr: tt.openErr})
			log, ok := sink.log.(*dlog.TestLogger)
			if !ok {
				t.Fatalf("sink logger is %T, want *dlog.TestLogger", sink.log)
			}

			// Act.
			sink.OnLinkFault("w1", sessionwatcher.LinkFault{
				Kind: sessionwatcher.LinkFaultDead, Detail: "the shim process is gone",
			})

			// Assert.
			var level string
			for _, record := range log.Records() {
				if record.Operation == "daemon.cmd.lifecycle" {
					level = record.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("lifecycle record level = %q, want %q", level, tt.wantLevel)
			}
		})
	}
}

func TestEveryDiagnosticsFrameReportsItsShimBuild(t *testing.T) {
	tests := []struct {
		name        string
		diagnostics *conversationv1.SessionDiagnostics
		want        string
	}{
		{name: "a healthy frame", diagnostics: &conversationv1.SessionDiagnostics{ShimBuild: "b1",
			Health: &conversationv1.SessionDiagnostics_Healthy{Healthy: &conversationv1.SessionHealthy{}}}, want: "ws-1=b1"},
		{name: "an unhealthy frame", diagnostics: func() *conversationv1.SessionDiagnostics {
			d := unhealthyDiagnostics(&conversationv1.SessionFault{Component: "store", Detail: "down"})
			d.ShimBuild = "b2"
			return d
		}(), want: "ws-1=b2"},
		{name: "a frame naming no build", diagnostics: &conversationv1.SessionDiagnostics{}, want: "ws-1="},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			judge := &fakeBuildJudge{}
			builds := &rolloutForwarder{}
			builds.bind(judge)
			sink := &lifecycleSink{health: &healthForwarder{}, builds: builds, log: dlog.NewTestLogger()}

			// Act
			sink.OnSessionDiagnostics("ws-1", tc.diagnostics)

			// Assert
			if len(judge.reported) != 1 || judge.reported[0] != tc.want {
				t.Fatalf("reported = %v, want [%s]", judge.reported, tc.want)
			}
		})
	}
}

func TestABuildReportedBeforeTheRolloutExistsIsRecordedNotDropped(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	sink := &lifecycleSink{health: &healthForwarder{}, builds: &rolloutForwarder{}, log: log}

	// Act
	sink.OnSessionDiagnostics("ws-1", &conversationv1.SessionDiagnostics{ShimBuild: "b1"})

	// Assert
	for _, r := range log.Records() {
		if r.Level == "error" && r.Message == "a shim reported its build before the rollout controller existed" && r.Context["build"] == "b1" {
			return
		}
	}
	t.Fatalf("records = %+v, want the unbound report at ERROR naming the build", log.Records())
}

func TestTheFreenessEdgeReachesTheQueue(t *testing.T) {
	// Arrange
	queue := &fakeFreeQueue{}
	sink := &lifecycleSink{queue: queue, log: dlog.NewTestLogger()}

	// Act
	sink.OnFree("ws-1")

	// Assert
	if len(queue.frees) != 1 || queue.frees[0] != "ws-1" {
		t.Fatalf("frees = %v, want [ws-1]", queue.frees)
	}
}

func TestTheDepartureEdgeReachesTheQueueNamingItsWatcher(t *testing.T) {
	// Arrange
	queue := &fakeFreeQueue{}
	sink := &lifecycleSink{queue: queue, log: dlog.NewTestLogger()}
	watcher := &departingWatcher{}
	departure := sessionwatcher.Departure{Ordered: false, Cause: sessionwatcher.DepartureLinkDead}

	// Act
	sink.OnDeparted("ws-1", watcher, departure)

	// Assert
	if len(queue.departures) != 1 {
		t.Fatalf("departures = %d, want exactly one", len(queue.departures))
	}
	got := queue.departures[0]
	if got.ws != "ws-1" || got.departure != departure || got.departed != promptqueue.Watcher(watcher) {
		t.Fatalf("departure = %+v, want ws-1's, naming the watcher that departed", got)
	}
}

func TestTheTurnsAnAdoptionFoundEndedUnobservedReachTheQueue(t *testing.T) {
	// Arrange
	queue := &fakeFreeQueue{}
	sink := &lifecycleSink{queue: queue, log: dlog.NewTestLogger()}

	// Act
	sink.OnTurnsEndedUnobserved("ws-1", []ids.TurnID{"turn-1", "turn-2"})

	// Assert
	if got := queue.unobserved["ws-1"]; len(got) != 2 || got[0] != "turn-1" || got[1] != "turn-2" {
		t.Fatalf("unobserved turns handed to the queue = %v, want ws-1's [turn-1 turn-2]", queue.unobserved)
	}
}

func TestAVendorStartedTurnTheWatcherAdoptedReachesTheQueue(t *testing.T) {
	// Arrange
	queue := &fakeFreeQueue{}
	sink := &lifecycleSink{queue: queue, log: dlog.NewTestLogger()}

	// Act
	sink.OnTurnAdopted("ws-1", "turn-v")

	// Assert
	if len(queue.adopted) != 1 || queue.adopted[0] != "ws-1/turn-v" {
		t.Fatalf("adopted turns handed to the queue = %v, want [ws-1/turn-v]", queue.adopted)
	}
}
