package workspace

import (
	"context"
	"errors"
	"testing"

	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/shimclient"
)

// TestRollBackNoSessionRefuses covers the gate that stops a rollback cold
// when no session is running: the shim is never asked to rewind anything it
// is not there to rewind, and the feed keeps every turn.
func TestRollBackNoSessionRefuses(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hasSession = false
	req := RollbackRequest{Turns: []ids.TurnID{"t1"}}

	// Act.
	_, err := f.verbs.RollBack(context.Background(), "w1", req)

	// Assert.
	refusal, ok := AsShimRefusal(err)
	if !ok {
		t.Fatalf("RollBack error = %v, want a *ShimRefusal", err)
	}
	if refusal.Arm != ArmShimNoSession {
		t.Fatalf("refusal arm = %q, want %q", refusal.Arm, ArmShimNoSession)
	}
	if !hasRecord(f, "info", opRollBack) {
		t.Fatalf("records = %+v, want an info record under %s", f.log.logger.Records(), opRollBack)
	}
	if len(f.feed.rolledBackTurns) != 0 {
		t.Fatalf("rolled back turns = %+v, want none: the feed never moves without a session", f.feed.rolledBackTurns)
	}
}

// TestRollBackEmptyTurnsErrors covers the request shape no caller should ever
// send: a rollback naming no turn has nothing to roll back TO, so the verb
// refuses it before touching the shim or the queue.
func TestRollBackEmptyTurnsErrors(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	req := RollbackRequest{Turns: nil}

	// Act.
	_, err := f.verbs.RollBack(context.Background(), "w1", req)

	// Assert.
	if err == nil {
		t.Fatalf("RollBack error = nil, want an error naming the empty turn list")
	}
	if _, ok := AsShimRefusal(err); ok {
		t.Fatalf("RollBack error = %v, want a plain error, not a *ShimRefusal", err)
	}
	if !hasRecord(f, "error", opRollBack) {
		t.Fatalf("records = %+v, want an error record under %s", f.log.logger.Records(), opRollBack)
	}
}

// TestRollBackShimRefusalPropagatesWithoutFeedCall covers every arm the
// shim's RollBackSession can refuse with, reached through the queue's own
// perform: the arm comes back unchanged and the feed is never told to drop
// the turns, because the vendor conversation was never actually rewound.
func TestRollBackShimRefusalPropagatesWithoutFeedCall(t *testing.T) {
	tests := []struct {
		name string
		arm  string
	}{
		{name: "no session", arm: ArmShimNoSession},
		{name: "prompt not recorded", arm: ArmShimPromptNotRecorded},
		{name: "first prompt", arm: ArmShimFirstPrompt},
		{name: "unseen prompt", arm: ArmShimUnseenPrompt},
		{name: "vendor refused", arm: ArmShimVendorRefused},
		{name: "files not restorable", arm: ArmShimFilesNotRestorable},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			f.shim.rollBackErr = &ShimRefusal{Verb: "RollBackSession", Arm: tt.arm, Detail: "the shim said no"}
			req := RollbackRequest{Turns: []ids.TurnID{"t1"}}

			// Act.
			result, err := f.verbs.RollBack(context.Background(), "w1", req)

			// Assert.
			refusal, ok := AsShimRefusal(err)
			if !ok {
				t.Fatalf("RollBack error = %v, want a *ShimRefusal", err)
			}
			if refusal.Arm != tt.arm {
				t.Fatalf("refusal arm = %q, want %q", refusal.Arm, tt.arm)
			}
			if result != (RollbackResult{}) {
				t.Fatalf("result = %+v, want the zero result on refusal", result)
			}
			if len(f.feed.rolledBackTurns) != 0 {
				t.Fatalf("rolled back turns = %+v, want none: the shim refused the rewind", f.feed.rolledBackTurns)
			}
		})
	}
}

// TestRollBackErrHoldsChangedPropagatesWithoutFeedCall covers the queue's own
// refusal: the held prompts changed since the rollback was planned, so the
// rewind never ran and the feed keeps every turn.
func TestRollBackErrHoldsChangedPropagatesWithoutFeedCall(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.queue.rollBackErr = promptqueue.ErrHoldsChanged
	req := RollbackRequest{Turns: []ids.TurnID{"t1"}}

	// Act.
	_, err := f.verbs.RollBack(context.Background(), "w1", req)

	// Assert.
	if !errors.Is(err, promptqueue.ErrHoldsChanged) {
		t.Fatalf("RollBack error = %v, want promptqueue.ErrHoldsChanged", err)
	}
	if len(f.feed.rolledBackTurns) != 0 {
		t.Fatalf("rolled back turns = %+v, want none: the queue never ran the rewind", f.feed.rolledBackTurns)
	}
	if len(f.shim.rollBacks) != 0 {
		t.Fatalf("shim rollbacks = %+v, want none: the queue's own refusal never calls perform", f.shim.rollBacks)
	}
}

// TestRollBackSuccessCallsFeedAndCountsFilesRestored covers the happy path:
// the shim rewinds the conversation, the feed drops exactly the turns named,
// and the restored paths are counted into the result.
func TestRollBackSuccessCallsFeedAndCountsFilesRestored(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.shim.rollBackPaths = []string{"a.go", "b.go", "c.go"}
	turns := []ids.TurnID{"t1", "t2", "t3"}
	req := RollbackRequest{Turns: turns, RestoreFiles: true}

	// Act.
	result, err := f.verbs.RollBack(context.Background(), "w1", req)

	// Assert.
	if err != nil {
		t.Fatalf("RollBack: %v", err)
	}
	if result != (RollbackResult{FilesRestored: 3}) {
		t.Fatalf("result = %+v, want FilesRestored: 3", result)
	}
	if len(f.feed.rolledBackTurns) != 1 {
		t.Fatalf("rolled back turns = %+v, want exactly one call", f.feed.rolledBackTurns)
	}
	if got := f.feed.rolledBackTurns[0]; len(got) != 3 || got[0] != "t1" || got[1] != "t2" || got[2] != "t3" {
		t.Fatalf("rolled back turns = %+v, want %+v", got, turns)
	}
	if !hasRecord(f, "info", opRollBack) {
		t.Fatalf("records = %+v, want an info record under %s", f.log.logger.Records(), opRollBack)
	}
}

// hasRecord reports whether the fixture's captured log carries a record at
// level under operation, which is how a synchronous verb's logging is
// checked (RollBack runs on the caller's own goroutine, so there is nothing
// to await).
func hasRecord(f *fixture, level, operation string) bool {
	for _, r := range f.log.logger.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

// fakeRollBackTransport is the narrow shimclient.Client surface a shimAdapter
// test drives directly: only RollBackSession is scripted, and every other
// call panics through the embedded nil interface, which no test here reaches.
type fakeRollBackTransport struct {
	shimclient.Client

	req  *shimv1.RollBackSessionRequest
	resp *shimv1.RollBackSessionResponse
	err  error
}

func (c *fakeRollBackTransport) RollBackSession(_ context.Context, req *shimv1.RollBackSessionRequest) (*shimv1.RollBackSessionResponse, error) {
	c.req = req
	return c.resp, c.err
}

// TestShimAdapterRollBackSessionRequestShape covers the request the adapter
// builds: ToBefore and DroppedTurns name the same turns the verb was given,
// in order, and the Files oneof arm matches whether files are restored.
func TestShimAdapterRollBackSessionRequestShape(t *testing.T) {
	tests := []struct {
		name         string
		restoreFiles bool
	}{
		{name: "restore files", restoreFiles: true},
		{name: "keep files", restoreFiles: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			transport := &fakeRollBackTransport{resp: &shimv1.RollBackSessionResponse{
				Result: &shimv1.RollBackSessionResponse_Success{Success: &shimv1.RollBackSessionSuccess{}},
			}}
			adapter := &shimAdapter{client: transport}
			turns := []ids.TurnID{"t1", "t2"}

			// Act.
			if _, err := adapter.RollBackSession(context.Background(), turns, tt.restoreFiles); err != nil {
				t.Fatalf("RollBackSession: %v", err)
			}

			// Assert.
			if got := transport.req.GetToBefore().GetValue(); got != "t1" {
				t.Fatalf("to_before = %q, want %q", got, "t1")
			}
			dropped := transport.req.GetDroppedTurns()
			if len(dropped) != 2 || dropped[0].GetValue() != "t1" || dropped[1].GetValue() != "t2" {
				t.Fatalf("dropped_turns = %+v, want [t1 t2] in order", dropped)
			}
			if tt.restoreFiles {
				if transport.req.GetRestoreFiles() == nil {
					t.Fatalf("files = %+v, want the restore_files arm", transport.req.GetFiles())
				}
			} else {
				if transport.req.GetKeepFiles() == nil {
					t.Fatalf("files = %+v, want the keep_files arm", transport.req.GetFiles())
				}
			}
		})
	}
}

// TestShimAdapterRollBackSessionFailureArms covers every RollBackSession
// failure arm the shim can name, including the two that carry the vendor's
// own words as the refusal's detail.
func TestShimAdapterRollBackSessionFailureArms(t *testing.T) {
	tests := []struct {
		name       string
		failure    *shimv1.RollBackSessionFailure
		wantArm    string
		wantDetail string
	}{
		{
			name:    "no session",
			failure: &shimv1.RollBackSessionFailure{Cause: &shimv1.RollBackSessionFailure_NoSession{NoSession: &shimv1.RollBackSessionNoSession{}}},
			wantArm: ArmShimNoSession,
		},
		{
			name:    "prompt not recorded",
			failure: &shimv1.RollBackSessionFailure{Cause: &shimv1.RollBackSessionFailure_PromptNotRecorded{PromptNotRecorded: &shimv1.RollBackSessionPromptNotRecorded{}}},
			wantArm: ArmShimPromptNotRecorded,
		},
		{
			name:    "first prompt",
			failure: &shimv1.RollBackSessionFailure{Cause: &shimv1.RollBackSessionFailure_FirstPrompt{FirstPrompt: &shimv1.RollBackSessionFirstPrompt{}}},
			wantArm: ArmShimFirstPrompt,
		},
		{
			name: "unseen prompt carries the vendor prompt uuid as detail",
			failure: &shimv1.RollBackSessionFailure{Cause: &shimv1.RollBackSessionFailure_UnseenPrompt{UnseenPrompt: &shimv1.RollBackSessionUnseenPrompt{
				VendorPromptUuid: "uuid-123",
			}}},
			wantArm:    ArmShimUnseenPrompt,
			wantDetail: "uuid-123",
		},
		{
			name: "vendor refused carries the vendor message as detail",
			failure: &shimv1.RollBackSessionFailure{Cause: &shimv1.RollBackSessionFailure_VendorRefused{VendorRefused: &shimv1.RollBackSessionVendorRefused{
				VendorMessage: "the vendor said no",
			}}},
			wantArm:    ArmShimVendorRefused,
			wantDetail: "the vendor said no",
		},
		{
			name: "files not restorable carries the vendor message as detail",
			failure: &shimv1.RollBackSessionFailure{Cause: &shimv1.RollBackSessionFailure_FilesNotRestorable{FilesNotRestorable: &shimv1.RollBackSessionFilesNotRestorable{
				VendorMessage: "the files are gone",
			}}},
			wantArm:    ArmShimFilesNotRestorable,
			wantDetail: "the files are gone",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			transport := &fakeRollBackTransport{resp: &shimv1.RollBackSessionResponse{
				Result: &shimv1.RollBackSessionResponse_Failure{Failure: tt.failure},
			}}
			adapter := &shimAdapter{client: transport}

			// Act.
			_, err := adapter.RollBackSession(context.Background(), []ids.TurnID{"t1"}, false)

			// Assert.
			refusal, ok := AsShimRefusal(err)
			if !ok {
				t.Fatalf("RollBackSession error = %v, want a *ShimRefusal", err)
			}
			if refusal.Arm != tt.wantArm {
				t.Fatalf("arm = %q, want %q", refusal.Arm, tt.wantArm)
			}
			if refusal.Detail != tt.wantDetail {
				t.Fatalf("detail = %q, want %q", refusal.Detail, tt.wantDetail)
			}
		})
	}
}
