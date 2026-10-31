// Package revision revises a final response from words the user highlighted in
// it.
//
// THE PROMPT IS NOT A TURN. While words of a final response are highlighted
// (agentrepl.v1.SelectResponseText), the prompt the user sends asks for that
// response rewritten to answer it: the daemon composes the revision brief
// ahead of what the user said, asks the workspace's shim to run it on a side
// query (shim.v1.ReviseResponse), and the revised prose lands as the
// response_revision page line, which the feed draws over the response's own
// bubble. Nothing new appears in the feed.
//
// ONE LANE PER WORKSPACE. A workspace's revisions run one after another, in
// the order they were asked for, and each reads the response as it stands when
// it starts, so a second revision of the same response revises the first's
// result rather than racing it.
//
// WHERE A REVISION STANDS is the feed's: Revise marks the response pending
// before it returns, a failure marks it failed with the cause, and the page
// line ends the pending state when the revised prose lands.
//
// THE WORKSPACE READS `updating` WHILE ITS LANE RUNS (owner request,
// 2026-10-30): the lane raises the status fact when it starts and lowers it
// when it drains, both under the lane's own lock, so a lane started just as
// another drained can never have its raise undone by the old lane's lower.
package revision

import (
	"context"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
)

// Brief is the brief a revision is composed from (prompts/).
const Brief = "response-revision-from-highlight"

// The records' operations.
const (
	opRevise = "daemon.revision.revise"
	opRun    = "daemon.revision.run"
)

// Shims finds a workspace's live shim.
type Shims interface {
	// Client answers the workspace's shim client, and false when it has no
	// live session.
	Client(ws ids.WorkspaceID) (shimclient.Client, bool)
}

// Feed is the feed's side of a revision: which responses can be revised, and
// where a revision stands.
type Feed interface {
	// RevisableFinalResponse answers the settled final response ROW names.
	RevisableFinalResponse(ws ids.WorkspaceID, row *frontendv1.FeedId) (feed.RevisableResponse, bool)
	// SetResponseRevision states where ROW's revision stands.
	SetResponseRevision(ws ids.WorkspaceID, row *frontendv1.FeedId, state *frontendv1.FeedResponseRevision) bool
}

// Status is the workspace status's side of a revision: whether one runs.
type Status interface {
	// SetUpdating raises (true) or lowers (false) the fact that a final
	// response of the workspace is being updated.
	SetUpdating(ws ids.WorkspaceID, updating bool)
}

// Deps is what a Reviser needs.
type Deps struct {
	Shims  Shims
	Feed   Feed
	Status Status
	// PromptsDir holds the brief, read at use time.
	PromptsDir string
	// Life bounds every revision: the daemon's lifetime.
	Life context.Context
	// Log is the package's canonical logger.
	Log dlog.Logger
}

// Ask is one revision asked for.
type Ask struct {
	// Row is the final response's row on the workspace's root feed.
	Row *frontendv1.FeedId
	// Words are the words highlighted in it.
	Words string
	// Said is what the user sent while they were highlighted.
	Said *conversationv1.UserSaid
}

// ErrNotRevisable refuses a revision of a row that is no settled final
// response.
var ErrNotRevisable = fmt.Errorf("the highlighted row is not a settled final response")

// Reviser runs every workspace's revisions, one lane per workspace.
type Reviser struct {
	deps  Deps
	mu    sync.Mutex
	lanes map[ids.WorkspaceID][]Ask
	// done, when set, is told each time a lane finishes a revision (tests).
	done func()
}

// New builds a Reviser.
func New(deps Deps) *Reviser {
	return &Reviser{deps: deps, lanes: map[ids.WorkspaceID][]Ask{}}
}

// Revise marks the response pending and queues ASK on the workspace's lane.
// ErrNotRevisable refuses a row that is no settled final response, with
// nothing marked.
func (r *Reviser) Revise(ws ids.WorkspaceID, ask Ask) error {
	if _, ok := r.deps.Feed.RevisableFinalResponse(ws, ask.Row); !ok {
		return ErrNotRevisable
	}
	r.deps.Feed.SetResponseRevision(ws, ask.Row, &frontendv1.FeedResponseRevision{
		State: &frontendv1.FeedResponseRevision_Pending{Pending: &frontendv1.FeedResponseRevisionPending{}},
	})
	r.mu.Lock()
	queued := r.lanes[ws]
	r.lanes[ws] = append(queued, ask)
	start := len(queued) == 0
	if start {
		r.deps.Status.SetUpdating(ws, true)
	}
	r.mu.Unlock()
	r.deps.Log.Info(opRevise, "a revision of a final response was asked for", dlog.Context{
		"workspace": string(ws), "row": ask.Row.GetValue(), "words": len(ask.Words), "queued_ahead": len(queued),
	})
	if start {
		go r.drain(ws)
	}
	return nil
}

// drain runs WS's lane until it is empty.
func (r *Reviser) drain(ws ids.WorkspaceID) {
	for {
		r.mu.Lock()
		next := r.lanes[ws][0]
		r.mu.Unlock()
		r.run(ws, next)
		r.mu.Lock()
		rest := r.lanes[ws][1:]
		if len(rest) == 0 {
			delete(r.lanes, ws)
			r.deps.Status.SetUpdating(ws, false)
		} else {
			r.lanes[ws] = rest
		}
		r.mu.Unlock()
		if r.done != nil {
			r.done()
		}
		if len(rest) == 0 {
			return
		}
	}
}

// run runs one revision to its outcome, marking the response failed on any
// failure.
func (r *Reviser) run(ws ids.WorkspaceID, ask Ask) {
	log := r.deps.Log.With(dlog.Context{"workspace": string(ws), "row": ask.Row.GetValue()})
	fail := func(cause, arm string) {
		log.Error(opRun, "a final response's revision failed; the response stands as it was", dlog.Context{"cause": cause, "arm": arm})
		r.deps.Feed.SetResponseRevision(ws, ask.Row, &frontendv1.FeedResponseRevision{
			State: &frontendv1.FeedResponseRevision_Failed{Failed: &frontendv1.FeedResponseRevisionFailed{Text: "Couldn't revise: " + cause}},
		})
	}
	// THE RESPONSE AS IT STANDS NOW, so a revision queued behind another
	// revises what the first one wrote.
	response, ok := r.deps.Feed.RevisableFinalResponse(ws, ask.Row)
	if !ok {
		fail("the response is no longer a final response of this workspace", "not_revisable")
		return
	}
	instruction, err := r.compose(response.Markdown, ask)
	if err != nil {
		fail(err.Error(), "brief")
		return
	}
	client, ok := r.deps.Shims.Client(ws)
	if !ok {
		fail("the workspace has no running session", "no_shim")
		return
	}
	started := time.Now()
	answer, err := client.ReviseResponse(r.deps.Life, &shimv1.ReviseResponseRequest{
		Response:    &conversationv1.AgentActivityId{Value: response.Unit},
		Instruction: instruction,
	})
	elapsed := time.Since(started).Milliseconds()
	if err != nil {
		fail(fmt.Sprintf("the session could not be asked: %v", err), "transport")
		return
	}
	switch result := answer.GetResult().(type) {
	case *shimv1.ReviseResponseResponse_Success:
		log.Info(opRun, "the shim wrote the response's revision; the feed draws it from the page line", dlog.Context{
			"unit": response.Unit, "model": result.Success.GetModel(), "duration_ms": elapsed,
		})
	case *shimv1.ReviseResponseResponse_Error:
		cause, arm := refusalOf(result.Error)
		fail(cause, arm)
	default:
		fail("the session answered no outcome", "unset")
	}
}

// compose is the revision's instruction: the brief, spliced with the response
// and the highlighted words, ahead of every block the user sent.
func (r *Reviser) compose(markdown string, ask Ask) (*conversationv1.UserSaid, error) {
	brief, err := prompts.Load(r.deps.PromptsDir, Brief)
	if err != nil {
		return nil, fmt.Errorf("the revision brief could not be read: %w", err)
	}
	text, err := brief.Splice(map[string]string{"response": markdown, "words": ask.Words})
	if err != nil {
		return nil, fmt.Errorf("the revision brief could not be composed: %w", err)
	}
	blocks := make([]*conversationv1.UserContentBlock, 0, len(ask.Said.GetContent().GetBlocks())+1)
	blocks = append(blocks, &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: text}},
	})
	blocks = append(blocks, ask.Said.GetContent().GetBlocks()...)
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}, nil
}

// refusalOf is a refusal's cause, as the response's failed state says it, and
// its arm, for the records.
func refusalOf(refused *shimv1.ReviseResponseError) (string, string) {
	switch cause := refused.GetCause().(type) {
	case *shimv1.ReviseResponseError_NoSession:
		return "the workspace has no running session", "no_session"
	case *shimv1.ReviseResponseError_NoForkPoint:
		return "the conversation could not be forked: " + cause.NoForkPoint.GetDetail(), "no_fork_point"
	case *shimv1.ReviseResponseError_VendorFailed:
		return cause.VendorFailed.GetDetail(), "vendor_failed"
	case *shimv1.ReviseResponseError_EmptyAnswer:
		return "the model answered with nothing", "empty_answer"
	default:
		return "the session refused with no cause", "unset"
	}
}
