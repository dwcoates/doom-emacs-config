package server

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/revision"
)

// WORDS HIGHLIGHTED IN A FINAL RESPONSE. The daemon is the only holder of each
// workspace's highlight (endpoint_select_response_text.proto): while one is
// held, the prompt the user sends revises the highlighted response instead of
// starting a turn, and the highlight ends with the prompt that took it.

// Revisions runs the revisions a highlighted prompt asks for
// (internal/revision).
type Revisions interface {
	// Revise marks the response pending and queues the revision.
	// revision.ErrNotRevisable refuses a row that is no settled final
	// response.
	Revise(ws ids.WorkspaceID, ask revision.Ask) error
}

// responseHighlight is one workspace's held highlight.
type responseHighlight struct {
	row   *frontendv1.FeedId
	words string
}

// responseHighlights is every workspace's held highlight.
type responseHighlights struct {
	mu   sync.Mutex
	held map[ids.WorkspaceID]responseHighlight
}

// set holds HIGHLIGHT for WS, or clears it when HIGHLIGHT is nil.
func (h *responseHighlights) set(ws ids.WorkspaceID, highlight *responseHighlight) {
	h.mu.Lock()
	defer h.mu.Unlock()
	if highlight == nil {
		delete(h.held, ws)
		return
	}
	h.held[ws] = *highlight
}

// take answers WS's highlight and ends it, false when none is held.
func (h *responseHighlights) take(ws ids.WorkspaceID) (responseHighlight, bool) {
	h.mu.Lock()
	defer h.mu.Unlock()
	held, ok := h.held[ws]
	delete(h.held, ws)
	return held, ok
}

// SelectResponseText holds the words highlighted in a final response, or
// clears them.
func (s *server) SelectResponseText(
	ctx context.Context,
	req *connect.Request[agentreplv1.SelectResponseTextRequest],
) (*connect.Response[agentreplv1.SelectResponseTextResponse], error) {
	const rpc = "SelectResponseText"
	if err := validateSelectResponseTextRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SelectResponseTextResponse{}
	subject, cerr, done := s.subjectFor(ctx, rpc, req.Msg.GetWorkspace(), resp)
	if done {
		return answer(resp, cerr)
	}
	ws := subject.Record.ID
	highlighted := req.Msg.GetSelection().GetHighlighted()
	if highlighted == nil {
		s.highlights.set(ws, nil)
		subject.Log.Debug(rpc, "the response highlight was cleared", nil)
		resp.Result = &agentreplv1.SelectResponseTextResponse_Success{Success: &agentreplv1.SelectResponseTextSuccess{}}
		return connect.NewResponse(resp), nil
	}
	row := highlighted.GetRow()
	if _, ok := s.deps.Feed.RevisableFinalResponse(ws, row); !ok {
		return answer(resp, s.refuse(subject.Log, rpc, resp, refusal{
			Arm:    "not_a_final_response",
			Reason: fmt.Sprintf("the row %q is not a settled final response of this workspace's root feed", row.GetValue()),
		}))
	}
	s.highlights.set(ws, &responseHighlight{row: row, words: highlighted.GetWords()})
	subject.Log.Debug(rpc, "words of a final response were highlighted; the next prompt revises it", dlog.Context{
		"row": row.GetValue(), "words": len(highlighted.GetWords()),
	})
	s.responseTextHighlighted(subject.Log, ws, row)
	resp.Result = &agentreplv1.SelectResponseTextResponse_Success{Success: &agentreplv1.SelectResponseTextSuccess{}}
	return connect.NewResponse(resp), nil
}

// responseTextHighlighted pushes response_text_highlighted onto the
// workspace's host stream: the reader highlighted words the next prompt
// revises, so Emacs puts the keyboard in the composer in insert state. It
// rides the host EVENT stream, never a topic, so a reconnect never focuses
// the composer again for a highlight made long before (as
// chatTextHighlighted).
func (s *server) responseTextHighlighted(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId) {
	log.Info("SelectResponseText", "words of a final response were highlighted; asked the host to focus the composer",
		dlog.Context{"row": row.GetValue()})
	s.hostEventStream(ws).Emit(&agentreplv1.WatchHostWorkspaceResponse{
		Push: &agentreplv1.WatchHostWorkspaceResponse_ResponseTextHighlighted{
			ResponseTextHighlighted: &agentreplv1.HostWorkspaceResponseTextHighlighted{},
		},
	})
}

// validateSelectResponseTextRequest requires the workspace and a selection: a
// highlight names its row and words that are not blank.
func validateSelectResponseTextRequest(req *agentreplv1.SelectResponseTextRequest) *connect.Error {
	if err := validateWorkspaceRef("workspace", req.GetWorkspace()); err != nil {
		return err
	}
	switch state := req.GetSelection().GetState().(type) {
	case *agentreplv1.SelectResponseTextSelection_Cleared:
		return nil
	case *agentreplv1.SelectResponseTextSelection_Highlighted:
		if err := validateFeedID("selection.highlighted.row", state.Highlighted.GetRow()); err != nil {
			return err
		}
		if strings.TrimSpace(state.Highlighted.GetWords()) == "" {
			return invalid("selection.highlighted.words", "the highlighted words are required")
		}
		return nil
	}
	return invalid("selection.state", "a selection is highlighted or cleared")
}

// submitRevision is SubmitPrompt's body for a prompt sent while words of a
// final response were highlighted: the response is revised to answer it, and
// no turn is minted.
func (s *server) submitRevision(
	subject resolved,
	req *agentreplv1.SubmitPromptRequest,
	resp *agentreplv1.SubmitPromptResponse,
	held responseHighlight,
) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	const rpc = "SubmitPrompt"
	err := s.deps.Revisions.Revise(subject.Record.ID, revision.Ask{Row: held.row, Words: held.words, Said: req.GetSaid()})
	switch {
	case errors.Is(err, revision.ErrNotRevisable):
		// THE HIGHLIGHTED RESPONSE IS GONE (a rollback took it, a reload
		// dropped it): the prompt is refused rather than delivered as a turn
		// the user did not mean to send.
		return answer(resp, s.refuse(subject.Log, rpc, resp, submitRefusal(s.fill(refusal{
			Arm:      "reference_response_unresolvable",
			Reason:   fmt.Sprintf("the highlighted row %q is no longer a settled final response of this workspace", held.row.GetValue()),
			NotFound: true,
		}))))
	case err != nil:
		return nil, fail(subject.Log, rpc, err)
	}
	subject.Log.Info(rpc, "the prompt revises the highlighted final response; no turn was minted", dlog.Context{"row": held.row.GetValue()})
	resp.Result = &agentreplv1.SubmitPromptResponse_Success{Success: &agentreplv1.SubmitPromptSuccess{
		Outcome: &agentreplv1.SubmitPromptSuccess_ResponseRevising{ResponseRevising: &agentreplv1.SubmitPromptResponseRevising{Row: held.row}},
	}}
	return connect.NewResponse(resp), nil
}
