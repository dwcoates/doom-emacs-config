package feed

import (
	"reflect"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// THE CHAT SINK: a turn's prose, its concluded answer and its end, told to
// the chat service as the feed draws them.

// chatEdge is one call the resolver made on its chat sink.
type chatEdge struct {
	kind     string
	ws       ids.WorkspaceID
	turn     ids.TurnID
	markdown string
}

// fakeChatSink records the edges in order.
type fakeChatSink struct{ edges []chatEdge }

func (f *fakeChatSink) AnswerProse(ws ids.WorkspaceID, turn ids.TurnID, markdown string) {
	f.edges = append(f.edges, chatEdge{"prose", ws, turn, markdown})
}

func (f *fakeChatSink) TurnAnswered(ws ids.WorkspaceID, turn ids.TurnID, markdown string) {
	f.edges = append(f.edges, chatEdge{"answered", ws, turn, markdown})
}

func (f *fakeChatSink) TurnEnded(ws ids.WorkspaceID, turn ids.TurnID) {
	f.edges = append(f.edges, chatEdge{"ended", ws, turn, ""})
}

func (h *harness) streamProse(unit string, fragments ...string) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame(unit, &conversationv1.AgentResponseStart{}, nil), nil, nil)
	for _, f := range fragments {
		h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame(unit, &conversationv1.AgentResponseUpdate{NewMarkdown: f}, nil), nil, nil)
	}
}

func TestChatSinkIsToldEachEdgeOfATurn(t *testing.T) {
	cases := []struct {
		name string
		act  func(h *harness)
		want []chatEdge
	}{
		{
			name: "streamed prose is told as it grows",
			act: func(h *harness) {
				h.deliverPrompt("turn-1", "why?")
				h.streamProse("unit-1", "Be", "cause")
			},
			want: []chatEdge{
				{"prose", testWorkspace, "turn-1", ""},
				{"prose", testWorkspace, "turn-1", "Be"},
				{"prose", testWorkspace, "turn-1", "Because"},
			},
		},
		{
			name: "a concluded turn tells its settled answer, then its end",
			act: func(h *harness) {
				h.deliverPrompt("turn-1", "why?")
				h.concludeAnswer("turn-1", "unit-1", "Because.")
			},
			want: []chatEdge{
				{"prose", testWorkspace, "turn-1", "Because."},
				{"answered", testWorkspace, "turn-1", "Because."},
				{"ended", testWorkspace, "turn-1", ""},
			},
		},
		{
			name: "a failed turn tells only its end",
			act: func(h *harness) {
				h.deliverPrompt("turn-1", "why?")
				h.terminal("turn-1", nil, maxTurnsFailure())
			},
			want: []chatEdge{{"ended", testWorkspace, "turn-1", ""}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)

			// Act
			tc.act(h)

			// Assert
			if !reflect.DeepEqual(h.chat.edges, tc.want) {
				t.Fatalf("edges = %+v, want %+v", h.chat.edges, tc.want)
			}
		})
	}
}

func TestAReplayedConclusionTellsItsAnswerButNotProseOrEnd(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "why?")
	h.resolver.mu.Lock()
	h.resolver.state(testWorkspace).plane = planeHistory
	h.resolver.mu.Unlock()

	// Act
	h.concludeAnswer("turn-1", "unit-1", "Because.")

	// Assert
	want := []chatEdge{{"answered", testWorkspace, "turn-1", "Because."}}
	if !reflect.DeepEqual(h.chat.edges, want) {
		t.Fatalf("edges = %+v, want %+v", h.chat.edges, want)
	}
}

func TestAnInheritedConclusionTellsTheSinkNothing(t *testing.T) {
	// Arrange: a fork's inherited past is its ancestor's turns.
	h := newHarness(t)
	h.deliverPrompt("turn-1", "why?")
	h.resolver.mu.Lock()
	h.resolver.state(testWorkspace).plane = planeInherited
	h.resolver.mu.Unlock()

	// Act
	h.concludeAnswer("turn-1", "unit-1", "Because.")

	// Assert
	if len(h.chat.edges) != 0 {
		t.Fatalf("edges = %+v, want none for an inherited turn", h.chat.edges)
	}
}

func TestANoticeIsNeverToldAsProse(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.deliverPrompt("turn-1", "why?")

	// Act
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseFrame("unit-1", &conversationv1.AgentResponseSuccess{
		Prose:      &conversationv1.AgentResponseProse{Markdown: "API Error: overloaded"},
		Authorship: &conversationv1.AgentResponseSuccess_SynthesizedNotice{SynthesizedNotice: &conversationv1.AgentResponseSynthesizedNotice{}},
	}, nil), nil, nil)

	// Assert
	if len(h.chat.edges) != 0 {
		t.Fatalf("edges = %+v, want none for a notice", h.chat.edges)
	}
}
