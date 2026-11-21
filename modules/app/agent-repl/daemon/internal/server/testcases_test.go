package server

import (
	"context"
	"errors"
	"slices"
	"testing"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	testrunv1 "agentrepl/proto/testrun/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/testcases"
)

const casesPkg = "modules/app/agent-repl/daemon/internal/merge"

// casesHarness is a harness whose logs are captured, with the tests
// workspace registered.
func casesHarness(t *testing.T) (*harness, *dlog.TestLogger, *agentreplv1.RunTestsRequest) {
	t.Helper()
	log := dlog.NewTestLogger()
	h := newHarness(t, func(deps *Deps) { deps.Log = &fakeSurfaces{global: log, workspace: log} })
	ref := testsWorkspace(t, h, testsDeploy)
	h.Tests.events = []*testrunv1.TestRunEvent{endedPassed()}
	return h, log, &agentreplv1.RunTestsRequest{Workspace: ref, Requester: agent()}
}

// runTestsEnded calls RunTests and answers its frames and how its stream
// ended.
func runTestsEnded(t *testing.T, h *harness, req *agentreplv1.RunTestsRequest) ([]*agentreplv1.RunTestsResponse, error) {
	t.Helper()
	stream, err := h.Client.RunTests(context.Background(), connect.NewRequest(req))
	if err != nil {
		t.Fatal(err)
	}
	defer stream.Close()
	var frames []*agentreplv1.RunTestsResponse
	for stream.Receive() {
		frames = append(frames, stream.Msg())
	}
	return frames, stream.Err()
}

func sameSelections(a, b []*testrunv1.TestSelection) bool {
	return slices.EqualFunc(a, b, func(x, y *testrunv1.TestSelection) bool { return proto.Equal(x, y) })
}

var testsWS = ids.WorkspaceID("ws-tests")

func TestARunRecordsTheCasesItNamesOnce(t *testing.T) {
	// Arrange
	h, _, req := casesHarness(t)
	req.Run = &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{testcases.GoPackage(casesPkg, []string{"TestA", "TestB/a_case"})}}

	// Act
	runTests(t, h, req)
	runTests(t, h, req)

	// Assert
	want := []*testrunv1.TestSelection{testcases.GoPackage(casesPkg, []string{"TestA"}), testcases.GoPackage(casesPkg, []string{"TestB"})}
	if got := h.DB.testCases[testsWS]; !sameSelections(got, want) {
		t.Fatalf("recorded %v, want %v", got, want)
	}
}

func TestARunThatRunsNoCaseRecordsNothing(t *testing.T) {
	tests := []struct {
		name string
		run  *testrunv1.TestRunRequest
	}{
		{name: "a dry run", run: &testrunv1.TestRunRequest{Selections: oneHarness().GetSelections(), PlanOnly: true}},
		{name: "the whole roster", run: &testrunv1.TestRunRequest{Selections: []*testrunv1.TestSelection{{Target: &testrunv1.TestSelection_EverySuite{EverySuite: &testrunv1.TestSelectionEverySuite{}}}}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h, _, req := casesHarness(t)
			req.Run = tt.run

			// Act
			runTests(t, h, req)

			// Assert
			if got := h.DB.testCases[testsWS]; len(got) != 0 || h.Tests.calls != 1 {
				t.Fatalf("recorded %v after %d runs, want nothing recorded and the run run", got, h.Tests.calls)
			}
		})
	}
}

func TestARunWhoseCasesCannotBeRecordedDoesNotStart(t *testing.T) {
	// Arrange
	h, log, req := casesHarness(t)
	h.DB.recordCasesErr = errors.New("disk I/O error")
	req.Run = oneHarness()

	// Act
	_, err := runTestsEnded(t, h, req)

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal || h.Tests.calls != 0 {
		t.Fatalf("stream ended %v after %d runs, want Internal and no run", err, h.Tests.calls)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opTestCases})
	if !ok || rec.Context["workspace"] != string(testsWS) || rec.Context["cause"] == nil {
		t.Fatalf("records = %+v, want the failed record logged with the workspace and its cause", log.Records())
	}
}

func TestARerunRunsTheRecordedCasesDroppingWhatTheTreeNoLongerHolds(t *testing.T) {
	// Arrange
	h, _, req := casesHarness(t)
	recorded := []*testrunv1.TestSelection{testcases.GoPackage(casesPkg, []string{"TestA"}), testcases.Suite("daemon")}
	h.DB.testCases = map[ids.WorkspaceID][]*testrunv1.TestSelection{testsWS: recorded}
	req.Rerun, req.Run = &agentreplv1.RunTestsRerun{}, &testrunv1.TestRunRequest{Remeasure: true}

	// Act
	frames := runTests(t, h, req)

	// Assert
	want := &testrunv1.TestRunRequest{Selections: recorded, DropMissing: true, Remeasure: true}
	if !proto.Equal(h.Tests.selected, want) || len(frames) != 1 || frames[0].GetEvent().GetEnded().GetPassed() == nil {
		t.Fatalf("ran %v, frames %v; want %v run to its end", h.Tests.selected, frames, want)
	}
	if got := h.DB.testCases[testsWS]; !sameSelections(got, recorded) {
		t.Fatalf("recorded %v after the rerun, want the record as it stood", got)
	}
}

func TestARerunForgetsTheCasesItDropped(t *testing.T) {
	// Arrange
	h, log, req := casesHarness(t)
	kept, gone := testcases.GoPackage(casesPkg, []string{"TestA"}), testcases.GoPackage(casesPkg, []string{"TestRenamed"})
	h.DB.testCases = map[ids.WorkspaceID][]*testrunv1.TestSelection{testsWS: {kept, gone}}
	dropped := &testrunv1.TestRunEvent{Event: &testrunv1.TestRunEvent_Dropped{Dropped: &testrunv1.TestRunDropped{Cases: []*testrunv1.TestRunDroppedCase{
		{Selection: gone, Detail: "it no longer defines TestRenamed"}}}}}
	h.Tests.events = []*testrunv1.TestRunEvent{dropped, endedPassed()}
	req.Rerun, req.Run = &agentreplv1.RunTestsRerun{}, &testrunv1.TestRunRequest{}

	// Act
	frames := runTests(t, h, req)

	// Assert
	if len(frames) != 2 || !proto.Equal(frames[0].GetEvent(), dropped) {
		t.Fatalf("frames = %v, want the dropped cases relayed first", frames)
	}
	if got := h.DB.testCases[testsWS]; !sameSelections(got, []*testrunv1.TestSelection{kept}) {
		t.Fatalf("recorded %v, want the dropped case forgotten", got)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opTestCases, Message: "forgot the cases a rerun dropped because the tree no longer holds them"}); !ok {
		t.Fatalf("records = %+v, want the forgetting logged", log.Records())
	}
}

func TestARerunWhoseDroppedCasesCannotBeForgottenFailsLoudly(t *testing.T) {
	// Arrange
	h, log, req := casesHarness(t)
	gone := testcases.Suite("retired")
	h.DB.testCases = map[ids.WorkspaceID][]*testrunv1.TestSelection{testsWS: {gone, testcases.Suite("daemon")}}
	h.DB.forgetCasesErr = errors.New("database is locked")
	h.Tests.events = []*testrunv1.TestRunEvent{{Event: &testrunv1.TestRunEvent_Dropped{Dropped: &testrunv1.TestRunDropped{Cases: []*testrunv1.TestRunDroppedCase{{Selection: gone, Detail: "gone"}}}}}, endedPassed()}
	req.Rerun, req.Run = &agentreplv1.RunTestsRerun{}, &testrunv1.TestRunRequest{}

	// Act
	frames, err := runTestsEnded(t, h, req)

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal || len(frames) != 2 {
		t.Fatalf("stream ended %v after %d frames, want Internal after the run's two", err, len(frames))
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opTestCases})
	if !ok || rec.Context["workspace"] != string(testsWS) || rec.Context["dropped"] != 1 {
		t.Fatalf("records = %+v, want the failed forgetting logged with the workspace and the cases", log.Records())
	}
}

func TestARerunOfAnEmptyRecordIsRefusedAsNothingToRerun(t *testing.T) {
	// Arrange
	h, log, req := casesHarness(t)
	req.Rerun, req.Run = &agentreplv1.RunTestsRerun{}, &testrunv1.TestRunRequest{}

	// Act
	frames := runTests(t, h, req)

	// Assert
	if len(frames) != 1 || frames[0].GetError().GetNothingToRerun() == nil || h.Tests.calls != 0 {
		t.Fatalf("frames = %v after %d runs, want nothing_to_rerun and no run", frames, h.Tests.calls)
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opTestCases, Message: "refused a rerun: the workspace's record names no case"}); !ok {
		t.Fatalf("records = %+v, want the refusal logged", log.Records())
	}
}

func TestARerunWhoseRecordCannotBeReadDoesNotStart(t *testing.T) {
	// Arrange
	h, log, req := casesHarness(t)
	h.DB.testCasesErr = errors.New("disk I/O error")
	req.Rerun, req.Run = &agentreplv1.RunTestsRerun{}, &testrunv1.TestRunRequest{}

	// Act
	_, err := runTestsEnded(t, h, req)

	// Assert
	if connect.CodeOf(err) != connect.CodeInternal || h.Tests.calls != 0 {
		t.Fatalf("stream ended %v after %d runs, want Internal and no run", err, h.Tests.calls)
	}
	rec, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opTestCases})
	if !ok || rec.Context["workspace"] != string(testsWS) || rec.Context["cause"] == nil {
		t.Fatalf("records = %+v, want the failed read logged with the workspace and its cause", log.Records())
	}
}

func TestARerunNamingItsOwnSelectionIsRefused(t *testing.T) {
	// Arrange
	h, _, req := casesHarness(t)
	req.Rerun, req.Run = &agentreplv1.RunTestsRerun{}, oneHarness()

	// Act
	frames := runTests(t, h, req)

	// Assert
	if len(frames) != 1 || frames[0].GetError().GetInvalid() == nil || h.Tests.calls != 0 {
		t.Fatalf("frames = %v after %d runs, want one invalid refusal and no run", frames, h.Tests.calls)
	}
}
