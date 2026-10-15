package cachelapse

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"google.golang.org/protobuf/reflect/protoreflect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

func sentAnswer() *shimv1.SendKeepaliveSuccess {
	return &shimv1.SendKeepaliveSuccess{Outcome: &shimv1.SendKeepaliveSuccess_Sent{Sent: &shimv1.SendKeepaliveSent{}}}
}

func skippedAnswer(reason *shimv1.SendKeepaliveSkipped) *shimv1.SendKeepaliveSuccess {
	return &shimv1.SendKeepaliveSuccess{Outcome: &shimv1.SendKeepaliveSuccess_Skipped{Skipped: reason}}
}

// workspaceNamed is a hidden workspace with its own id and dir.
func workspaceNamed(id string) wsm.Workspace {
	ws := hidden()
	ws.ID, ws.Dir = ids.WorkspaceID(id), "/work/"+id
	return ws
}

// beatHarness is a keeper over RECORDS, each workspace served by the shim in
// SHIMS (none when absent).
func beatHarness(t *testing.T, records []wsm.Workspace, shims map[ids.WorkspaceID]*fakeShim) *harness {
	t.Helper()
	h := newHarness(t, records[0])
	for _, rec := range records[1:] {
		h.db.records[rec.ID] = rec
	}
	h.shims = shims
	return h
}

func outcomesOf(t *testing.T, resp *agentreplv1.SendHiddenKeepalivesResponse) []string {
	t.Helper()
	success := resp.GetSuccess()
	if success == nil {
		t.Fatalf("response = %v, want a success", resp)
	}
	var out []string
	for _, ws := range success.GetWorkspaces() {
		out = append(out, ws.GetWorkspace().GetId()+":"+outcomeName(ws))
	}
	return out
}

func TestSendHiddenKeepalivesAsksEveryHiddenWorkspaceAndNoOther(t *testing.T) {
	// Arrange
	merged := workspaceNamed("c-merged")
	merged.MergedAt = merged.HiddenAt
	shown := workspaceNamed("b-shown")
	shown.Hidden, shown.HiddenAt = false, nil
	shims := map[ids.WorkspaceID]*fakeShim{}
	records := []wsm.Workspace{workspaceNamed("a-hidden"), shown, merged, workspaceNamed("d-hidden")}
	for _, rec := range records {
		shims[rec.ID] = &fakeShim{answer: sentAnswer()}
	}
	h := beatHarness(t, records, shims)

	// Act
	resp := h.keeper.SendHiddenKeepalives(context.Background())

	// Assert
	got := fmt.Sprint(outcomesOf(t, resp), shims["b-shown"].beats, shims["c-merged"].beats)
	if want := fmt.Sprint([]string{"a-hidden:sent", "d-hidden:sent"}, 0, 0); got != want {
		t.Fatalf("outcomes = %s, want %s", got, want)
	}
}

func TestSendHiddenKeepalivesNamesEachWorkspaceByItsRef(t *testing.T) {
	// Arrange
	ws := workspaceNamed("a-hidden")
	h := beatHarness(t, []wsm.Workspace{ws}, map[ids.WorkspaceID]*fakeShim{ws.ID: {answer: sentAnswer()}})

	// Act
	resp := h.keeper.SendHiddenKeepalives(context.Background())

	// Assert
	ref := resp.GetSuccess().GetWorkspaces()[0].GetWorkspace()
	if ref.GetId() != "a-hidden" || ref.GetDir() != "/work/a-hidden" {
		t.Fatalf("ref = %v, want the workspace's id and dir", ref)
	}
}

func TestSendHiddenKeepalivesAnswersAnEmptyHiddenSection(t *testing.T) {
	// Arrange
	h := newHarness(t, visible())

	// Act
	resp := h.keeper.SendHiddenKeepalives(context.Background())

	// Assert
	if resp.GetSuccess() == nil || len(resp.GetSuccess().GetWorkspaces()) != 0 || h.shim.beats != 0 {
		t.Fatalf("response = %v with %d beats, want an empty success", resp, h.shim.beats)
	}
}

func TestSendHiddenKeepalivesReportsEachOutcome(t *testing.T) {
	tests := []struct {
		name    string
		served  bool
		answer  *shimv1.SendKeepaliveSuccess
		err     error
		outcome string
		level   string
		message string
	}{
		{name: "sent", served: true, answer: sentAnswer(), outcome: "sent",
			level: "info", message: "the hidden workspace's shim answered its keep-alive"},
		{name: "skipped, busy", served: true, outcome: "skipped_busy",
			answer: skippedAnswer(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{Detail: "a turn is in flight"}}}),
			level:  "info", message: "the hidden workspace's shim answered its keep-alive"},
		{name: "no session served", served: false, outcome: "not_running",
			level: "info", message: "no session is served for the hidden workspace; it has no cache to keep"},
		{name: "the shim holds no session", served: true, err: ErrNoSession, outcome: "not_running",
			level: "info", message: "the hidden workspace's shim holds no session; it has no cache to keep"},
		{name: "the submission failed", served: true, err: &BeatSubmitFailed{Detail: "stream closed"}, outcome: "failed",
			level: "error", message: "could not send the hidden workspace its keep-alive"},
		{name: "the shim died under the call", served: true, err: fmt.Errorf("%w: unexpected EOF", shimclient.ErrShimDied), outcome: "failed",
			level: "info", message: "the shim died under the keep-alive call; the death is recorded by daemon.shimclient.exit"},
		{name: "the shim cannot be reached", served: true, err: errors.New("connection refused"), outcome: "failed",
			level: "error", message: "could not send the hidden workspace its keep-alive"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ws := workspaceNamed("a-hidden")
			shims := map[ids.WorkspaceID]*fakeShim{}
			if tt.served {
				shims[ws.ID] = &fakeShim{answer: tt.answer, beatErr: tt.err}
			}
			h := beatHarness(t, []wsm.Workspace{ws}, shims)

			// Act
			resp := h.keeper.SendHiddenKeepalives(context.Background())

			// Assert
			if got := outcomesOf(t, resp); len(got) != 1 || got[0] != "a-hidden:"+tt.outcome {
				t.Fatalf("outcomes = %v, want a-hidden:%s", got, tt.outcome)
			}
			rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: tt.level, Operation: opBeat, Message: tt.message})
			if !ok || rec.Context["workspace"] != "a-hidden" {
				t.Fatalf("%s record %q = %+v, %v; want one naming the workspace", tt.level, tt.message, rec, ok)
			}
		})
	}
}

func TestSendHiddenKeepalivesCarriesASubmitFailuresOwnDetail(t *testing.T) {
	// Arrange
	ws := workspaceNamed("a-hidden")
	h := beatHarness(t, []wsm.Workspace{ws}, map[ids.WorkspaceID]*fakeShim{ws.ID: {beatErr: &BeatSubmitFailed{Detail: "stream closed"}}})

	// Act
	resp := h.keeper.SendHiddenKeepalives(context.Background())

	// Assert
	if got := resp.GetSuccess().GetWorkspaces()[0].GetFailed().GetDetail(); got != "stream closed" {
		t.Fatalf("detail = %q, want the shim's own", got)
	}
}

func TestSendHiddenKeepalivesRecordsACallTheDaemonsExitCancels(t *testing.T) {
	// Arrange
	ws := workspaceNamed("a-hidden")
	h := beatHarness(t, []wsm.Workspace{ws}, map[ids.WorkspaceID]*fakeShim{ws.ID: {beatErr: context.Canceled}})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	h.keeper.SendHiddenKeepalives(ctx)

	// Assert
	if _, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opBeat}); ok {
		t.Fatalf("records %v, want no error for the daemon leaving", h.log.Records())
	}
}

func TestSendHiddenKeepalivesSummarizesTheOutcomes(t *testing.T) {
	// Arrange
	a, b := workspaceNamed("a-hidden"), workspaceNamed("b-hidden")
	h := beatHarness(t, []wsm.Workspace{a, b}, map[ids.WorkspaceID]*fakeShim{a.ID: {answer: sentAnswer()}})

	// Act
	h.keeper.SendHiddenKeepalives(context.Background())

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opBeat, Message: "sent the hidden section its keep-alives"})
	if !ok || rec.Context["hidden"] != 2 || rec.Context["sent"] != 1 || rec.Context["not_running"] != 1 {
		t.Fatalf("summary = %+v, %v; want 2 hidden, 1 sent, 1 not running", rec, ok)
	}
}

func TestSendHiddenKeepalivesRefusesARegistryThatCannotBeRead(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	h.db.listErr = errors.New("database is locked")

	// Act
	resp := h.keeper.SendHiddenKeepalives(context.Background())

	// Assert
	if resp.GetError().GetRegistryUnreadable().GetDetail() != "database is locked" || h.shim.beats != 0 {
		t.Fatalf("response = %v with %d beats, want registry_unreadable and none sent", resp, h.shim.beats)
	}
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "error", Operation: opBeat})
	if !ok || rec.Context["cause"] != "database is locked" {
		t.Fatalf("error record = %+v, %v; want the cause", rec, ok)
	}
}

// TestSetBeatOutcomeMapsEverySkipReasonTheShimNames walks every arm of
// shim.v1.SendKeepaliveSkipped.reason by reflection, so a reason added to the
// shim's contract and not mapped here fails rather than panicking live.
func TestSetBeatOutcomeMapsEverySkipReasonTheShimNames(t *testing.T) {
	reasons := (&shimv1.SendKeepaliveSkipped{}).ProtoReflect().Descriptor().Oneofs().ByName("reason").Fields()
	for i := range reasons.Len() {
		field := reasons.Get(i)
		t.Run(string(field.Name()), func(t *testing.T) {
			// Arrange
			skipped := &shimv1.SendKeepaliveSkipped{}
			m := skipped.ProtoReflect()
			m.Set(field, protoreflect.ValueOfMessage(m.NewField(field).Message()))
			out := &agentreplv1.SendHiddenKeepalivesWorkspace{}

			// Act
			SetBeatOutcome(out, skippedAnswer(skipped))

			// Assert
			if out.GetSkipped().GetReason() == nil {
				t.Fatalf("outcome = %v, want a skip with its reason", out)
			}
		})
	}
}

func TestSetBeatOutcomeCarriesEachSkipReasonsFacts(t *testing.T) {
	tests := []struct {
		name   string
		reason *shimv1.SendKeepaliveSkipped
		check  func(*agentreplv1.SendHiddenKeepalivesSkipped) bool
	}{
		{name: "busy",
			reason: &shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{Detail: "a rollback is in progress"}}},
			check: func(s *agentreplv1.SendHiddenKeepalivesSkipped) bool {
				return s.GetBusy().GetDetail() == "a rollback is in progress"
			}},
		{name: "detached work live",
			reason: &shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{LiveItems: 3}}},
			check: func(s *agentreplv1.SendHiddenKeepalivesSkipped) bool {
				return s.GetDetachedWorkLive().GetLiveItems() == 3
			}},
		{name: "cache lapsed",
			reason: &shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: 42}}},
			check:  func(s *agentreplv1.SendHiddenKeepalivesSkipped) bool { return s.GetCacheLapsed().GetLapsedAtMs() == 42 }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			out := &agentreplv1.SendHiddenKeepalivesWorkspace{}

			// Act
			SetBeatOutcome(out, skippedAnswer(tt.reason))

			// Assert
			if !tt.check(out.GetSkipped()) {
				t.Fatalf("skip = %v, want the shim's facts carried", out.GetSkipped())
			}
		})
	}
}

func TestSetBeatOutcomeFailsHardOnAJudgmentItDoesNotName(t *testing.T) {
	tests := []struct {
		name   string
		answer *shimv1.SendKeepaliveSuccess
	}{
		{name: "no outcome", answer: &shimv1.SendKeepaliveSuccess{}},
		{name: "a skip with no reason", answer: skippedAnswer(&shimv1.SendKeepaliveSkipped{})},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatal("SetBeatOutcome returned; want a panic")
				}
			}()

			// Act
			SetBeatOutcome(&agentreplv1.SendHiddenKeepalivesWorkspace{}, tt.answer)
		})
	}
}

func TestBeatSubmitFailedIsTheSubmitFailure(t *testing.T) {
	// Arrange
	err := fmt.Errorf("wrapped: %w", &BeatSubmitFailed{Detail: "stream closed"})

	// Act
	is := errors.Is(err, ErrBeatSubmitFailed)

	// Assert
	if !is {
		t.Fatalf("errors.Is(%v, ErrBeatSubmitFailed) = false, want true", err)
	}
}
