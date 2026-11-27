package main

import (
	"context"
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/wsm"
)

// enqueueSubmitter is a prompt handler that answers one outcome and records
// what it was asked to submit.
type enqueueSubmitter struct {
	outcome prompthandler.Outcome
	err     error
	origins []conversationv1.PromptOrigin
	texts   []string
}

var _ prompthandler.Submitter = (*enqueueSubmitter)(nil)

func (s *enqueueSubmitter) Submit(_ context.Context, _ ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, _ *ids.WorkspaceID, _ wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	s.origins = append(s.origins, origin)
	s.texts = append(s.texts, said.GetContent().GetBlocks()[0].GetText().GetText())
	return s.outcome, s.err
}

func TestEnqueuePromptsCompactForQueue(t *testing.T) {
	const turn = ids.TurnID("turn-compact")
	tests := []struct {
		name      string
		bind      bool
		submitErr error
		turn      ids.TurnID
		closed    wsm.TurnClose
		awaitErr  error
		wantErr   string
	}{
		{name: "a completed compaction answers no error", bind: true, turn: turn, closed: wsm.CloseCompleted},
		{name: "an unbound handler is a wiring defect", wantErr: "before the prompt handler existed"},
		{name: "a refused submission is answered", bind: true, submitErr: errors.New("cold gate"), wantErr: "cold gate"},
		{name: "a submission answering no turn is answered", bind: true, wantErr: "answered no turn"},
		{name: "a failed wait is answered", bind: true, turn: turn, awaitErr: errors.New("watcher closed"), wantErr: "watcher closed"},
		{name: "a compaction that did not complete is answered", bind: true, turn: turn, closed: wsm.CloseKilled, wantErr: "not completed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			submitter := &enqueueSubmitter{outcome: prompthandler.Outcome{Turn: tt.turn}, err: tt.submitErr}
			var awaited []ids.TurnID
			p := &enqueuePrompts{awaitTurnEnd: func(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) (wsm.TurnClose, error) {
				awaited = append(awaited, turn)
				return tt.closed, tt.awaitErr
			}}
			if tt.bind {
				p.bind(submitter)
			}

			// Act
			err := p.compactForQueue(context.Background(), "ws-1", "keep the purpose")

			// Assert
			if tt.wantErr == "" {
				if err != nil {
					t.Fatalf("compactForQueue = %v, want nil", err)
				}
				if len(awaited) != 1 || awaited[0] != turn {
					t.Fatalf("awaited = %v, want the compaction's turn", awaited)
				}
			} else if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("compactForQueue = %v, want an error naming %q", err, tt.wantErr)
			}
		})
	}
}

func TestEnqueuePromptsCompactForQueueSubmitsTheEnqueueCompaction(t *testing.T) {
	// Arrange
	submitter := &enqueueSubmitter{outcome: prompthandler.Outcome{Turn: "turn-compact"}}
	p := &enqueuePrompts{awaitTurnEnd: func(context.Context, ids.WorkspaceID, ids.TurnID) (wsm.TurnClose, error) {
		return wsm.CloseCompleted, nil
	}}
	p.bind(submitter)

	// Act
	if err := p.compactForQueue(context.Background(), "ws-1", "keep the purpose"); err != nil {
		t.Fatalf("compactForQueue: %v", err)
	}

	// Assert
	if len(submitter.origins) != 1 || submitter.origins[0] != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_ENQUEUE_COMPACTION {
		t.Fatalf("origins = %v, want the enqueue compaction's", submitter.origins)
	}
	if submitter.texts[0] != prompthandler.CompactionText("keep the purpose") {
		t.Fatalf("text = %q, want the compaction with its instructions", submitter.texts[0])
	}
}

func TestVerbsForwarderSetModelBeforeTheVerbsIsAWiringDefect(t *testing.T) {
	// Arrange
	f := &verbsForwarder{}

	// Act
	err := f.setModel(context.Background(), "ws-1", "sonnet")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "before the workspace verbs existed") {
		t.Fatalf("setModel = %v, want the wiring defect", err)
	}
}
