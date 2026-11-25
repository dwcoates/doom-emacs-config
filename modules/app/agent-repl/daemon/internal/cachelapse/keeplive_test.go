package cachelapse

import (
	"context"
	"errors"
	"go/ast"
	"go/token"
	"slices"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/sourcescan"
)

// keepLiveHarness is a visible workspace whose cache was warmed ten minutes
// ago, so no scheduled beat is due: only a keep-live sends one now.
func keepLiveHarness(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.shim.answer = sent()
	return h
}

// parkedKeepLiveHarness is keepLiveHarness with its session parked.
func parkedKeepLiveHarness(t *testing.T) *harness {
	t.Helper()
	h := keepLiveHarness(t)
	h.db.sessions[theWorkspace] = parkedSession(theWorkspace)
	h.serving = false
	return h
}

// keepLive begins a keep-live run on the harness's workspace and answers the
// channel its answer arrives on.
func (h *harness) keepLive() chan *agentreplv1.KeepLiveWorkspaceResponse {
	done := make(chan *agentreplv1.KeepLiveWorkspaceResponse, 1)
	h.keeper.apply(context.Background(), edge{ws: theWorkspace, kind: edgeKeepLive, keepLive: done, why: "test"})
	return done
}

// settle applies every queued edge, and every edge those queue, in order.
func (h *harness) settle() {
	for {
		edges := h.keeper.take()
		if len(edges) == 0 {
			return
		}
		for _, e := range edges {
			h.keeper.apply(context.Background(), e)
		}
	}
}

// restoreUp ends the harness's one restore with its session up and served.
func (h *harness) restoreUp(t *testing.T) {
	t.Helper()
	if len(h.restoring) != 1 {
		t.Fatalf("restores = %+v, want one", h.restoring)
	}
	h.db.sessions[theWorkspace] = liveSession(theWorkspace)
	h.serving = true
	h.restoring[0].done(nil)
	h.keeper.SessionUp(theWorkspace)
	h.settle()
	h.keeper.beatsDue(context.Background())
}

// vendorAnswers restates the cache's lapse an interval after now, the answer
// the awaited keep-alive's stand-down waits on.
func (h *harness) vendorAnswers() {
	h.keeper.OnCacheLapse(theWorkspace, stated(now.Add(KeepaliveInterval+time.Minute), nil))
	h.settle()
}

// standDownSettles reports the one stand-down asked as OUTCOME.
func (h *harness) standDownSettles(t *testing.T, outcome drain.CycleOutcome) {
	t.Helper()
	if len(h.cycled) != 1 || h.cycleDones[0] == nil {
		t.Fatalf("stand-downs = %+v, want one waited on", h.cycled)
	}
	h.cycleDones[0](outcome)
	h.settle()
}

// answer reads the run's answer, failing when none came.
func answerOf(t *testing.T, done chan *agentreplv1.KeepLiveWorkspaceResponse) *agentreplv1.KeepLiveWorkspaceResponse {
	t.Helper()
	select {
	case resp := <-done:
		return resp
	default:
		t.Fatal("the keep-live answered nothing")
		return nil
	}
}

func TestAParkedWorkspaceIsStartedBeatenAckedAndStoodDown(t *testing.T) {
	// Arrange
	h := parkedKeepLiveHarness(t)

	// Act
	done := h.keepLive()
	h.restoreUp(t)
	h.vendorAnswers()
	h.standDownSettles(t, drain.CycleOutcome{Down: true, Why: "stood an idle session down until its next keep-alive"})

	// Assert
	beaten := answerOf(t, done).GetSuccess().GetBeaten()
	if beaten.GetStarted() == nil || beaten.GetAck().GetAnswered() == nil || beaten.GetStoodDown() == nil {
		t.Fatalf("answer = %v, want started, answered and stood down", beaten)
	}
	if h.shim.beats != 1 || !h.cycled[0].beat.Equal(now) {
		t.Fatalf("beats %d, stand-down %+v; want one keep-alive and its stand-down at the beat", h.shim.beats, h.cycled)
	}
	if len(h.keeper.keepLives) != 0 {
		t.Fatalf("runs = %v, want the run ended", h.keeper.keepLives)
	}
}

func TestAKeepLiveRecordsItsStepsAndTrigger(t *testing.T) {
	// Arrange
	h := parkedKeepLiveHarness(t)

	// Act
	h.keepLive()
	h.restoreUp(t)
	h.vendorAnswers()
	h.standDownSettles(t, drain.CycleOutcome{Down: true})

	// Assert
	sentRec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opDrive, Message: "sent the workspace's keep-alive"})
	if !ok || sentRec.Context["trigger"] != triggerKeepLive {
		t.Fatalf("sent record %+v, want it under the keep-live trigger", sentRec)
	}
	ended, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: "info", Operation: opDrive, Message: "the keep-live ended"})
	want := []string{"restore_queued", "restore_began", "keepalive_sent", "vendor_answered", "stand_down_asked", "stood_down"}
	if !ok || ended.Context["outcome"] != "beaten_stood_down" || !slices.Equal(ended.Context["steps"].([]string), want) {
		t.Fatalf("ended record %+v, want outcome beaten_stood_down and steps %v", ended, want)
	}
}

func TestAScheduledBeatRecordsTheScheduledTrigger(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())

	// Assert
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Operation: opDrive, Message: "sent the workspace's keep-alive"})
	if !ok || rec.Context["trigger"] != triggerScheduled {
		t.Fatalf("sent record %+v, want it under the scheduled trigger", rec)
	}
}

func TestAKeepLiveIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		want    func(*agentreplv1.KeepLiveWorkspaceError) bool
	}{
		{name: "for a cold-gated workspace", arrange: func(h *harness) { h.gate = &conversationv1.SessionCold{} },
			want: func(e *agentreplv1.KeepLiveWorkspaceError) bool { return e.GetColdGated() != nil }},
		{name: "for a closed workspace", arrange: func(h *harness) { h.db.records[theWorkspace] = beatable(closed(), 10*time.Minute) },
			want: func(e *agentreplv1.KeepLiveWorkspaceError) bool { return e.GetClosed() != nil }},
		{name: "for an unknown workspace", arrange: func(h *harness) { delete(h.db.records, theWorkspace) },
			want: func(e *agentreplv1.KeepLiveWorkspaceError) bool { return e.GetUnknownWorkspace() != nil }},
		{name: "for a hidden workspace, which owes no keep-alive", arrange: func(h *harness) { h.db.records[theWorkspace] = beatable(backlogged(), 10*time.Minute) },
			want: func(e *agentreplv1.KeepLiveWorkspaceError) bool { return e.GetOwesNoKeepalive().GetReason() != "" }},
		{name: "when the registry cannot be read", arrange: func(h *harness) { h.db.readErr = errors.New("the store is gone") },
			want: func(e *agentreplv1.KeepLiveWorkspaceError) bool {
				return e.GetRegistryUnreadable().GetDetail() == "the store is gone"
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := parkedKeepLiveHarness(t)
			tt.arrange(h)

			// Act
			done := h.keepLive()

			// Assert
			if resp := answerOf(t, done); !tt.want(resp.GetError()) {
				t.Fatalf("answer = %v, want the refusal", resp)
			}
			if len(h.restoring) != 0 || h.shim.beats != 0 || len(h.keeper.keepLives) != 0 {
				t.Fatalf("restores %+v, beats %d, runs %v; want nothing started or sent", h.restoring, h.shim.beats, h.keeper.keepLives)
			}
		})
	}
}

func TestAKeepLiveOnABusySessionSendsNothingAndStopsNothing(t *testing.T) {
	tests := []struct {
		name   string
		reason *shimv1.SendKeepaliveSkipped
		want   func(*agentreplv1.KeepLiveWorkspaceBusy) bool
	}{
		{name: "the main agent is busy",
			reason: &shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_MainAgentBusy{MainAgentBusy: &shimv1.SendKeepaliveSkippedMainAgentBusy{Detail: "a turn is in flight"}}},
			want: func(b *agentreplv1.KeepLiveWorkspaceBusy) bool {
				return b.GetMainAgent().GetDetail() == "a turn is in flight"
			}},
		{name: "detached work is live",
			reason: &shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_DetachedWorkLive{DetachedWorkLive: &shimv1.SendKeepaliveSkippedDetachedWorkLive{LiveItems: 2}}},
			want:   func(b *agentreplv1.KeepLiveWorkspaceBusy) bool { return b.GetDetachedWork().GetLiveItems() == 2 }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := keepLiveHarness(t)
			h.shim.answer = skipped(tt.reason)

			// Act
			done := h.keepLive()

			// Assert
			if resp := answerOf(t, done); !tt.want(resp.GetSuccess().GetBusy()) {
				t.Fatalf("answer = %v, want busy", resp)
			}
			if len(h.cycled) != 0 || len(h.keeper.answering) != 0 {
				t.Fatalf("stand-downs %+v, answers awaited %v; want nothing stopped or awaited", h.cycled, h.keeper.answering)
			}
		})
	}
}

func TestAnAlreadyUpWorkspaceIsBeatenAndLeftToTheOrdinaryRules(t *testing.T) {
	tests := []struct {
		name      string
		workedAgo *time.Duration
		outcome   *drain.CycleOutcome
		wantRule  string
		wantDowns int
	}{
		{name: "within its grace it stays up with no stand-down asked", workedAgo: ptr(time.Minute),
			wantRule: "the workspace's resource allocation holds its shim up; the shim stays up until its hold ends"},
		{name: "past its grace the stand-down's own rule keeps it up", outcome: &drain.CycleOutcome{Why: "remote control is on; the shim stays up for claude.ai"},
			wantRule: "remote control is on; the shim stays up for claude.ai", wantDowns: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := keepLiveHarness(t)
			h.workedAgo(testHoldAfterWork + time.Minute)
			if tt.workedAgo != nil {
				h.workedAgo(*tt.workedAgo)
			}

			// Act
			done := h.keepLive()
			h.vendorAnswers()
			if tt.outcome != nil {
				h.standDownSettles(t, *tt.outcome)
			}

			// Assert
			beaten := answerOf(t, done).GetSuccess().GetBeaten()
			if beaten.GetAlreadyUp() == nil || beaten.GetLeftUp().GetRule() != tt.wantRule {
				t.Fatalf("answer = %v, want already up and left up by %q", beaten, tt.wantRule)
			}
			if h.shim.beats != 1 || len(h.restoring) != 0 || len(h.cycled) != tt.wantDowns {
				t.Fatalf("beats %d, restores %+v, stand-downs %+v; want one beat, no restore, %d stand-downs", h.shim.beats, h.restoring, h.cycled, tt.wantDowns)
			}
		})
	}
}

func ptr[T any](v T) *T { return &v }

func TestAKeepLiveStepThatOutrunsItsBound(t *testing.T) {
	tests := []struct {
		name string
		// reach drives the run to the step whose bound fires, then moves the
		// clock past it and lets the loop take its deadlines.
		reach     func(t *testing.T, h *harness)
		wantStep  func(*agentreplv1.KeepLiveWorkspaceTimedOut) bool
		wantBound time.Duration
		// wantDowns is the stand-downs asked by the time the run ends: the
		// start's timeout asks one for the session the run started.
		wantDowns int
	}{
		{name: "start", reach: func(t *testing.T, h *harness) {
			h.clock.Set(now.Add(KeepLiveStartBound))
			h.keeper.keepLivesDue()
		}, wantStep: func(s *agentreplv1.KeepLiveWorkspaceTimedOut) bool { return s.GetStart() != nil }, wantBound: KeepLiveStartBound, wantDowns: 1},
		{name: "ack", reach: func(t *testing.T, h *harness) {
			h.restoreUp(t)
			h.clock.Set(now.Add(AnswerBound))
			h.keeper.answersDue(context.Background())
		}, wantStep: func(s *agentreplv1.KeepLiveWorkspaceTimedOut) bool { return s.GetAck() != nil }, wantBound: AnswerBound, wantDowns: 1},
		{name: "stand-down", reach: func(t *testing.T, h *harness) {
			h.restoreUp(t)
			h.vendorAnswers()
			h.clock.Set(now.Add(KeepLiveStandDownBound))
			h.keeper.keepLivesDue()
		}, wantStep: func(s *agentreplv1.KeepLiveWorkspaceTimedOut) bool { return s.GetStandDown() != nil }, wantBound: KeepLiveStandDownBound, wantDowns: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := parkedKeepLiveHarness(t)
			done := h.keepLive()

			// Act
			tt.reach(t, h)

			// Assert
			timed := answerOf(t, done).GetError().GetTimedOut()
			if !tt.wantStep(timed) || timed.GetBoundMs() != tt.wantBound.Milliseconds() || timed.GetExpectedMs() == 0 {
				t.Fatalf("answer = %v, want the %s step timed out at %v", timed, tt.name, tt.wantBound)
			}
			if !h.found(dlog.RecordQuery{Level: "error", Operation: opDrive, Message: "the keep-live outran its step's bound"}) {
				t.Fatalf("records = %+v, want the timeout recorded at ERROR", h.log.Records())
			}
			if len(h.cycled) != tt.wantDowns || len(h.keeper.keepLives) != 0 {
				t.Fatalf("stand-downs %+v, runs %v; want %d stand-downs and the run ended", h.cycled, h.keeper.keepLives, tt.wantDowns)
			}
		})
	}
}

func TestAKeepLiveThatFails(t *testing.T) {
	tests := []struct {
		name      string
		reach     func(t *testing.T, h *harness)
		want      func(*agentreplv1.KeepLiveWorkspaceError) bool
		wantDowns int
	}{
		{name: "to restore the session answers start_failed", reach: func(t *testing.T, h *harness) {
			h.restoring[0].done(errors.New("the vendor would not start"))
			h.settle()
		}, want: func(e *agentreplv1.KeepLiveWorkspaceError) bool {
			return e.GetStartFailed().GetDetail() == "the vendor would not start"
		}},
		{name: "to submit the keep-alive answers beat_failed and stands its start down", reach: func(t *testing.T, h *harness) {
			h.shim.beatErr = &BeatSubmitFailed{Detail: "the vendor refused the request"}
			h.restoreUp(t)
		}, want: func(e *agentreplv1.KeepLiveWorkspaceError) bool {
			return e.GetBeatFailed().GetDetail() == "the vendor refused the request"
		}, wantDowns: 1},
		{name: "on a lapsed cache answers cache_lapsed", reach: func(t *testing.T, h *harness) {
			h.shim.answer = skipped(&shimv1.SendKeepaliveSkipped{Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: now.UnixMilli()}}})
			h.restoreUp(t)
		}, want: func(e *agentreplv1.KeepLiveWorkspaceError) bool {
			return e.GetCacheLapsed().GetLapsedAtMs() == now.UnixMilli()
		}},
		{name: "to stand the session down answers stand_down_failed", reach: func(t *testing.T, h *harness) {
			h.restoreUp(t)
			h.vendorAnswers()
			h.standDownSettles(t, drain.CycleOutcome{Failed: true, Why: "the park failed; its record above says why"})
		}, want: func(e *agentreplv1.KeepLiveWorkspaceError) bool {
			return e.GetStandDownFailed().GetDetail() == "the park failed; its record above says why"
		}, wantDowns: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := parkedKeepLiveHarness(t)
			done := h.keepLive()

			// Act
			tt.reach(t, h)

			// Assert
			if resp := answerOf(t, done); !tt.want(resp.GetError()) {
				t.Fatalf("answer = %v, want the failure", resp)
			}
			if len(h.cycled) != tt.wantDowns || len(h.keeper.keepLives) != 0 {
				t.Fatalf("stand-downs %+v, runs %v; want %d and the run ended", h.cycled, h.keeper.keepLives, tt.wantDowns)
			}
			if !h.found(dlog.RecordQuery{Level: "info", Operation: opDrive, Message: "the keep-live ended"}) {
				t.Fatalf("records = %+v, want the run's end recorded", h.log.Records())
			}
		})
	}
}

func TestASecondKeepLiveJoinsTheRunUnderWay(t *testing.T) {
	// Arrange
	h := parkedKeepLiveHarness(t)
	first := h.keepLive()

	// Act
	second := h.keepLive()
	h.restoreUp(t)
	h.vendorAnswers()
	h.standDownSettles(t, drain.CycleOutcome{Down: true})

	// Assert
	if answerOf(t, first).GetSuccess() == nil || answerOf(t, second).GetSuccess() == nil || len(h.restoring) != 1 {
		t.Fatalf("restores %+v; want one run answering both calls", h.restoring)
	}
}

func TestAKeepLiveEndsWithTheBeatTimedFromTheClockAgain(t *testing.T) {
	// Arrange
	h := keepLiveHarness(t)
	h.workedAgo(time.Minute)
	h.keepLive()

	// Act
	h.vendorAnswers()

	// Assert
	if due := h.keeper.beats[theWorkspace]; !due.Equal(now.Add(KeepaliveInterval)) {
		t.Fatalf("next beat = %v, want an interval after the keep-alive", due)
	}
}

// TestTheVerbAndTheScheduledBeatShareOneSubroutine fails when the keep-live
// verb or the scheduled beat reaches the shim's keep-alive other than through
// sendBeat, so the two can never drift apart.
func TestTheVerbAndTheScheduledBeatShareOneSubroutine(t *testing.T) {
	// Arrange
	callers := map[string][]string{}
	sourcescan.EachSourceIn(t, ".", false, func(_ *token.FileSet, f *ast.File) {
		for _, decl := range f.Decls {
			fn, ok := decl.(*ast.FuncDecl)
			if !ok || fn.Body == nil {
				continue
			}
			ast.Inspect(fn.Body, func(n ast.Node) bool {
				if call, ok := n.(*ast.CallExpr); ok {
					if sel, ok := call.Fun.(*ast.SelectorExpr); ok {
						callers[sel.Sel.Name] = append(callers[sel.Sel.Name], fn.Name.Name)
					}
				}
				return true
			})
		}
	})

	// Assert
	if got, want := callers["sendBeat"], []string{"beginKeepLive", "beatsDue"}; !sameSet(got, want) {
		t.Fatalf("sendBeat is called from %v, want only %v", got, want)
	}
	// SendBackloggedKeepalives' beat (`SPC j R`) keeps its cadence held and owns
	// no stand-down, so it is the one other caller of the shim's keep-alive.
	if got, want := callers["Beat"], []string{"sendBeat", "beat"}; !sameSet(got, want) {
		t.Fatalf("the shim's keep-alive is asked from %v, want only %v", got, want)
	}
	if got, want := callers["settleAnswer"], []string{"answered", "answeredFailed", "answersDue"}; !sameSet(got, want) {
		t.Fatalf("the answer is settled from %v, want only %v", got, want)
	}
}

func sameSet(got, want []string) bool {
	got, want = slices.Clone(got), slices.Clone(want)
	slices.Sort(got)
	slices.Sort(want)
	return slices.Equal(got, want)
}

func TestAKeepLiveCallThatEndsFirstAbandonsTheWaitNotTheRun(t *testing.T) {
	// Arrange
	h := keepLiveHarness(t)
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act
	resp, err := h.keeper.KeepLive(ctx, theWorkspace)

	// Assert
	if resp != nil || !errors.Is(err, context.Canceled) {
		t.Fatalf("KeepLive = %v, %v; want the call's own cancellation", resp, err)
	}
	if edges := h.keeper.take(); len(edges) != 1 || edges[0].kind != edgeKeepLive {
		t.Fatalf("queued = %+v, want the run still queued for the loop", edges)
	}
}
