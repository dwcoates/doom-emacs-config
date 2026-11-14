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
// THE AGENT IS TOLD. The fork persists nothing, so the vendor transcript keeps
// the response's original words; a revision that lands is owed to the agent
// (wsm.OweRevision), and the workspace's next prompt carrying a workspace
// note tells it what its response now says (prompthandler).
//
// THE WORKSPACE READS `updating` WHILE ITS LANE RUNS (owner request,
// 2026-10-30): the lane raises the status fact when it starts and lowers it
// when it drains, both under the lane's own lock, so a lane started just as
// another drained can never have its raise undone by the old lane's lower.
package revision

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/prompts"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// Bound is how long one ReviseResponse call may take: just over twice its
// measured healthy worst case. Five revisions logged on 2026-11-13
// (daemon.revision.run's duration_ms) took 18.3s, 20.3s, 27.5s, 42.9s and
// 53.5s; the bound is 2.2 times the slowest.
const Bound = 120 * time.Second

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

// Owed is the store's side of a revision: what the agent is owed.
type Owed interface {
	// OweRevision records a revised response the agent has yet to be told
	// of.
	OweRevision(ctx context.Context, id wsm.WorkspaceID, owed wsm.OwedRevision) error
}

// Deps is what a Reviser needs.
type Deps struct {
	Shims  Shims
	Feed   Feed
	Status Status
	// Owed records each landed revision for the agent to be told of.
	Owed Owed
	// Bound bounds each ReviseResponse call (Bound in production).
	Bound time.Duration
	// Now is the clock a landed revision is stamped with.
	Now func() time.Time
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

// New builds a Reviser. A Reviser with no bound or no record of what the
// agent is owed is a wiring defect, refused at construction.
func New(deps Deps) *Reviser {
	if deps.Bound <= 0 || deps.Owed == nil || deps.Now == nil {
		panic(fmt.Sprintf("revision: New needs a positive Bound, Owed and Now (bound %v, owed set %v, now set %v)", deps.Bound, deps.Owed != nil, deps.Now != nil))
	}
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
		inflight.Go(func() { r.drain(ws) })
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
	ctx, cancel := context.WithTimeout(r.deps.Life, r.deps.Bound)
	defer cancel()
	answer, err := client.ReviseResponse(ctx, &shimv1.ReviseResponseRequest{
		Response:    &conversationv1.AgentActivityId{Value: response.Unit},
		Instruction: instruction,
	})
	elapsed := time.Since(started).Milliseconds()
	if err != nil {
		if errors.Is(ctx.Err(), context.DeadlineExceeded) {
			fail(fmt.Sprintf("the session did not answer within %v (its healthy worst case was 53.5s on 2026-11-13; it ran %dms)", r.deps.Bound, elapsed), "timed_out")
			return
		}
		fail(fmt.Sprintf("the session could not be asked: %v", err), "transport")
		return
	}
	switch result := answer.GetResult().(type) {
	case *shimv1.ReviseResponseResponse_Success:
		revised := result.Success.GetProse().GetMarkdown()
		if revised == "" {
			fail("the session answered a written revision with no words", "no_prose")
			return
		}
		log.Info(opRun, "the shim wrote the response's revision; the feed draws it from the page line", dlog.Context{
			"unit": response.Unit, "model": result.Success.GetModel(), "duration_ms": elapsed,
		})
		r.owe(ws, response, revised, log)
	case *shimv1.ReviseResponseResponse_Error:
		cause, arm := refusalOf(result.Error)
		fail(cause, arm)
	default:
		fail("the session answered no outcome", "unset")
	}
}

// owe records the landed revision for the agent to be told of. A record that
// fails is logged at ERROR: the revision stands on the page, but the agent
// will build on the words it wrote.
func (r *Reviser) owe(ws ids.WorkspaceID, response feed.RevisableResponse, revised string, log dlog.Logger) {
	owed := wsm.OwedRevision{Unit: response.Unit, Original: response.Markdown, Revised: revised, OwedAtNs: r.deps.Now().UnixNano()}
	if err := r.deps.Owed.OweRevision(r.deps.Life, ws, owed); err != nil {
		log.Error(opRun, "the revision landed but could not be recorded for the agent; its next prompt will not say what the response now says",
			dlog.Context{"unit": response.Unit, "cause": err.Error()})
		return
	}
	log.Debug(opRun, "the revision is owed to the agent's next prompt", dlog.Context{"unit": response.Unit, "revised_chars": len(revised)})
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
