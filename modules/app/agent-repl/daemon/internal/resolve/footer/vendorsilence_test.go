package footer

import (
	"testing"
	"time"

	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// silenceLine reads the published working arm's vendor-silence line, failing
// when the cell carries another line.
func silenceLine(t *testing.T, h *harness) (*frontendv1.FooterStatusWorkingSalient, *frontendv1.FooterStatusActivityVendorSilence) {
	t.Helper()
	salient := working(t, h).GetActivity().GetSalient()
	line := salient.GetVendorSilence()
	if line == nil {
		t.Fatalf("activity = %v, want the vendor-silence line", working(t, h).GetActivity())
	}
	return salient, line
}

func TestHeardOfWordsTheFrameAndReadsWhereTheResponseStands(t *testing.T) {
	tests := []struct {
		name        string
		frame       classifiedFrame
		subagent    bool
		wantStalled bool
		wantText    string
		wantCause   string
	}{
		{name: "thinking streams a response", frame: classifiedFrame{arm: "thinking", phaseArm: "update", phase: phaseRunning, ok: true}, wantStalled: true, wantText: "thinking", wantCause: "thinking.update"},
		{name: "reply text streams a response", frame: classifiedFrame{arm: "response", phaseArm: "update", phase: phaseRunning, ok: true}, wantStalled: true, wantText: "reply text", wantCause: "response.update"},
		{name: "a landed reply is still the response", frame: classifiedFrame{arm: "response", phaseArm: "success", phase: phaseLanded, ok: true}, wantStalled: true, wantText: "reply text", wantCause: "response.success"},
		{name: "a call surfacing is the response", frame: classifiedFrame{arm: "bash", phaseArm: "start", phase: phaseRunning, ok: true}, wantStalled: true, wantText: "bash started", wantCause: "bash.start"},
		{name: "a call landing closes the exchange", frame: classifiedFrame{arm: "bash", phaseArm: "success", phase: phaseLanded, ok: true}, wantText: "bash finished", wantCause: "bash.success"},
		{name: "a multi-word arm reads as words", frame: classifiedFrame{arm: "mcp_tool_call", phaseArm: "failure", phase: phaseLanded, ok: true}, wantText: "mcp tool call finished", wantCause: "mcp_tool_call.failure"},
		{name: "a lifecycle-less item closes the exchange", frame: classifiedFrame{arm: "context_injected", phaseArm: "memory", phase: phaseNone, ok: true}, wantText: "context injected", wantCause: "context_injected.memory"},
		{name: "a subagent's frame says whose it is", frame: classifiedFrame{arm: "thinking", phaseArm: "update", phase: phaseRunning, ok: true}, subagent: true, wantStalled: true, wantText: "a subagent's thinking", wantCause: "thinking.update"},
		{name: "a frame naming no item", frame: classifiedFrame{phase: phaseNone}, wantText: "an activity frame", wantCause: "."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := heardOf(tt.frame, tt.subagent)

			// Assert
			if got.stalled != tt.wantStalled || got.text != tt.wantText || got.cause != tt.wantCause {
				t.Fatalf("heardOf = %+v, want stalled=%v text=%q cause=%q", got, tt.wantStalled, tt.wantText, tt.wantCause)
			}
		})
	}
}

func TestTheWaitingStepStandsWithTheVendorSilenceLine(t *testing.T) {
	tests := []struct {
		name         string
		frames       [][2]protoreflect.Name
		wantStalled  bool
		wantLastHead string
	}{
		{name: "a call returned and the next response never started", frames: [][2]protoreflect.Name{{"read", "start"}, {"read", "success"}}, wantLastHead: "read finished"},
		{name: "a response started and went quiet", frames: [][2]protoreflect.Name{{"response", "start"}}, wantStalled: true, wantLastHead: "reply text"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			inTurn(h)
			for _, f := range tt.frames {
				h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", f[0], f[1]))
			}
			heardAt := h.clock.Now()

			// Act
			h.clock.Advance(DefaultVendorSilence + 12*time.Second)

			// Assert
			if got := stepName(working(t, h)); got != "waiting" {
				t.Fatalf("step = %q, want waiting", got)
			}
			salient, line := silenceLine(t, h)
			if salient.GetAt().GetAtMs() != heardAt.UnixMilli() {
				t.Fatalf("at = %d, want the last-heard instant %d", salient.GetAt().GetAtMs(), heardAt.UnixMilli())
			}
			var lastHeard string
			if tt.wantStalled {
				lastHeard = line.GetStalled().GetLastHeard().GetText()
				if line.GetStalled() == nil {
					t.Fatalf("response = %v, want stalled", line.GetResponse())
				}
			} else {
				lastHeard = line.GetNotStarted().GetLastHeard().GetText()
				if line.GetNotStarted() == nil {
					t.Fatalf("response = %v, want not_started", line.GetResponse())
				}
			}
			if lastHeard != tt.wantLastHead {
				t.Fatalf("last heard = %q, want %q", lastHeard, tt.wantLastHead)
			}
		})
	}
}

func TestTheVendorSilenceLineOutranksTheSharedSalientLines(t *testing.T) {
	// Arrange: a deploy's progress stands, which would otherwise be the line.
	h := newHarness(t)
	inTurn(h)
	deploying(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "response", "start"))

	// Act
	h.clock.Advance(DefaultVendorSilence)

	// Assert
	silenceLine(t, h)
}

func TestTheThinkingStepCarriesNoVendorSilenceLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "response", "start"))

	// Act
	h.clock.Advance(DefaultVendorSilence - time.Second)

	// Assert
	if got := working(t, h).GetActivity().GetSalient().GetVendorSilence(); got != nil {
		t.Fatalf("activity carries the vendor-silence line under %q", stepName(working(t, h)))
	}
}

func TestTheVendorSilenceRecordCarriesWhatWasLastHeard(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "thinking", "update"))

	// Act
	h.clock.Advance(DefaultVendorSilence)

	// Assert
	records := recordsOf(h.log.Records(), "daemon.footer.vendor_silence")
	var found bool
	for _, rec := range records {
		if rec.Context["last_heard"] == "thinking" && rec.Context["stalled"] == true && rec.Context["last_heard_edge"] == "thinking.update" {
			found = true
		}
	}
	if !found {
		t.Fatalf("records = %+v, want one naming the thinking frame as stalled", records)
	}
}

func TestTheSilencesEndIsRecordedWithItsCauseAndLength(t *testing.T) {
	tests := []struct {
		name      string
		end       func(h *harness)
		wantCause string
	}{
		{name: "the vendor's next frame", end: func(h *harness) {
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-2", "response", "start"))
		}, wantCause: "response.start"},
		{name: "the turn's terminal", end: func(h *harness) {
			turn := testTurnID
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)
		}, wantCause: "agent_terminal"},
		{name: "the turn cleared", end: func(h *harness) {
			h.r.SetTurn(testWS, nil)
		}, wantCause: "turn_cleared"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			inTurn(h)
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "success"))
			h.clock.Advance(DefaultVendorSilence + 5*time.Second)

			// Act
			tt.end(h)

			// Assert
			records := recordsOf(h.log.Records(), "daemon.footer.vendor_silence_ended")
			if len(records) != 1 {
				t.Fatalf("records = %+v, want exactly one silence end", records)
			}
			ctx := records[0].Context
			if ctx["cause"] != tt.wantCause || ctx["last_heard"] != "read finished" || ctx["silent_ms"] != (DefaultVendorSilence+5*time.Second).Milliseconds() {
				t.Fatalf("context = %+v, want cause %q, last heard read finished, silent %v", ctx, tt.wantCause, DefaultVendorSilence+5*time.Second)
			}
		})
	}
}

func TestASilenceTheStripNeverDrewRecordsNoEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	inTurn(h)
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "response", "start"))
	h.clock.Advance(DefaultVendorSilence - time.Second)

	// Act
	h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "response", "update"))

	// Assert
	if records := recordsOf(h.log.Records(), "daemon.footer.vendor_silence_ended"); len(records) != 0 {
		t.Fatalf("records = %+v, want none: the strip never read waiting", records)
	}
}

func TestAnAnsweredAskStartsTheSilenceClockAgainFromItsAnswer(t *testing.T) {
	closeQuestion := &conversationv1.AgentQuestion{Id: &conversationv1.AgentQuestionId{Value: "q-1"},
		Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{}}}
	closePermission := &conversationv1.AgentPermission{Id: &conversationv1.AgentPermissionId{Value: "ask-1"},
		Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{}}}
	tests := []struct {
		name  string
		open  func(h *harness)
		close func(h *harness)
	}{
		{name: "a question",
			open:  func(h *harness) { h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "which?")) },
			close: func(h *harness) { h.r.OnQuestion(testWS, mainAgent, closeQuestion) }},
		{name: "a consent ask",
			open: func(h *harness) {
				h.r.OnPermission(testWS, mainAgent, permissionStart("ask-1", "Claude wants to read foo"))
			},
			close: func(h *harness) { h.r.OnPermission(testWS, mainAgent, closePermission) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the turn has been heard from, and its ask waits on the
			// user past the silence threshold.
			h := newHarness(t)
			inTurn(h)
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "start"))
			h.r.OnActivity(testWS, mainAgent, itemFrame(t, "u-1", "read", "success"))
			tt.open(h)
			h.clock.Advance(DefaultVendorSilence * 3)

			// Act: the user answers.
			tt.close(h)

			// Assert: thinking now, and waiting only once the vendor stays
			// silent past the threshold from the answer.
			if got := stepName(working(t, h)); got == "waiting" {
				t.Fatalf("step = %q right after the answer, want the wait on the user never counted as the vendor's silence", got)
			}
			answered := h.clock.Now()
			h.clock.Advance(DefaultVendorSilence + time.Second)
			salient, line := silenceLine(t, h)
			if salient.GetAt().GetAtMs() != answered.UnixMilli() || line.GetNotStarted().GetLastHeard().GetText() != "the answer went out" {
				t.Fatalf("silence line at %d (%v), want it from the answer at %d", salient.GetAt().GetAtMs(), line, answered.UnixMilli())
			}
		})
	}
}
