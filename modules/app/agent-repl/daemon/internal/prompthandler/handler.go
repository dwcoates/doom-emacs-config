package prompthandler

import (
	"context"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

// The handler's operation names. Every logical branch records under one of
// them, per the logging contract.
const (
	opNew       = "daemon.prompthandler.new"
	opSubmit    = "daemon.prompthandler.submit"
	opRecognize = "daemon.prompthandler.recognize"
	opPanel     = "daemon.prompthandler.panel"
	opRefuse    = "daemon.prompthandler.refuse"
)

// handler is SubmitPrompt's body: thin, stateless, done at submission. It owns
// NO execution and NO response formatting.
type handler struct {
	deps Deps
}

// newHandler validates the wiring and builds the handler.
func newHandler(deps Deps) (*handler, error) {
	switch {
	case deps.Log == nil:
		return nil, fmt.Errorf("the prompt handler needs log surfaces")
	case deps.DB == nil:
		return nil, fmt.Errorf("the prompt handler needs a state client")
	case deps.Queue == nil:
		return nil, fmt.Errorf("the prompt handler needs the prompt queue")
	case deps.Feed == nil:
		return nil, fmt.Errorf("the prompt handler needs the feed resolver")
	case deps.Panels == nil:
		return nil, fmt.Errorf("the prompt handler needs a panel source")
	}
	if deps.MintTurn == nil {
		deps.MintTurn = wsm.NewTurnID
	}
	deps.Log.Global().Debug(opNew, "the prompt handler is wired", nil)
	return &handler{deps: deps}, nil
}

// Submit runs one submission, in the one order the contract fixes: the origin
// is checked before anything is minted or mirrored, the addressed feed is
// checked against the workspace, recognition forks the answer, and only an
// ordinary prompt reaches the queue.
func (h *handler) Submit(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, idempotencyKey string, origin conversationv1.PromptOrigin, target *feedid.Ref) (Outcome, error) {
	log, err := h.logger(ctx, ws)
	if err != nil {
		return Outcome{}, err
	}
	log = log.With(dlog.Context{"origin": origin.String(), "idempotency_key": idempotencyKey})

	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		log.Warn(opSubmit, "the submission carries no origin", nil)
		return Outcome{}, ErrOriginRequired
	}
	if target != nil && target.WS != ws {
		log.Warn(opSubmit, "the addressed feed belongs to another workspace",
			dlog.Context{"feed_workspace": string(target.WS)})
		return Outcome{}, ErrFeedNotInWorkspace
	}

	text := saidText(said)
	got := recognize(text)
	log.Debug(opRecognize, "recognized the submission", dlog.Context{
		"recognition": recognitionName(got.kind), "command": got.literal,
	})

	switch got.kind {
	case RecognizedPanel:
		return h.answerPanel(ctx, ws, got, log)
	case RecognizedRefused:
		return h.refuse(ws, got, log), nil
	case RecognizedAct:
		return h.act(ctx, ws, got, idempotencyKey, origin, log)
	}

	turn, err := h.mintTurn(ctx, ws, idempotencyKey, log)
	if err != nil {
		return Outcome{}, err
	}
	disposition, err := h.deps.Queue.Submit(ctx, promptqueue.Submission{
		WS: ws, Turn: turn, Said: said, Origin: origin, Target: target,
	})
	if err != nil {
		return Outcome{}, err
	}
	log.Info(opSubmit, "the prompt was forwarded to the queue", dlog.Context{
		"turn": string(turn), "delivered": disposition.Delivered, "parked": disposition.Parked(),
	})
	return Outcome{Recognition: RecognizedNone, Turn: turn, Disposition: disposition}, nil
}

// answerPanel resolves a recognized panel and MIRRORS it into the root feed as
// a non-durable row, so the answer is both returned and drawn.
func (h *handler) answerPanel(ctx context.Context, ws ids.WorkspaceID, got recognized, log dlog.Logger) (Outcome, error) {
	panel, err := h.deps.Panels(ctx, ws, got.spec.command)
	if err != nil {
		log.Error(opPanel, "the panel could not be resolved", dlog.Context{
			"command": got.literal, "cause": err.Error(),
		})
		return Outcome{}, fmt.Errorf("answer %s on %q: %w", got.literal, ws, err)
	}
	if panel == nil {
		log.Error(opPanel, "the panel source answered nothing", dlog.Context{"command": got.literal})
		return Outcome{}, fmt.Errorf("answer %s on %q: the panel source answered nothing", got.literal, ws)
	}
	h.deps.Feed.UpsertCommandPanel(ws, feedPanel(panel))
	log.Info(opPanel, "answered a recognized command with its panel", dlog.Context{"command": got.literal})
	return Outcome{Recognition: RecognizedPanel, Panel: panel}, nil
}

// refuse answers a recognized-but-unsupported command and mirrors its refusal
// card, with the add-support offer, into the root feed.
func (h *handler) refuse(ws ids.WorkspaceID, got recognized, log dlog.Logger) Outcome {
	h.deps.Feed.UpsertCommandRefused(ws, got.literal, RefusalReason(got.literal), true)
	log.Info(opRefuse, "recognized a command the daemon neither answers nor forwards",
		dlog.Context{"command": got.literal})
	return Outcome{Recognition: RecognizedRefused, RefusedCommand: got.literal}
}

// act sends a session-acting command down the queue's ONE delivery path. The
// two context cuts run as turns, so they mint one and the answer carries it; a
// model change does not.
func (h *handler) act(ctx context.Context, ws ids.WorkspaceID, got recognized, idempotencyKey string, origin conversationv1.PromptOrigin, log dlog.Logger) (Outcome, error) {
	act := promptqueue.Act{Kind: ActCommands[got.spec.command], Value: got.arg, Origin: origin}
	if act.Kind != promptqueue.ActSetModel {
		turn, err := h.mintTurn(ctx, ws, idempotencyKey, log)
		if err != nil {
			return Outcome{}, err
		}
		act.Turn = turn
	}
	if err := h.deps.Queue.SubmitSessionAct(ctx, ws, act); err != nil {
		return Outcome{}, err
	}
	log.Info(opSubmit, "a session-acting command went down the one delivery path",
		dlog.Context{"command": got.literal, "act": act.Kind, "turn": string(act.Turn)})
	return Outcome{Recognition: RecognizedAct, Turn: act.Turn, Act: act}, nil
}

// mintTurn mints the turn the daemon acknowledges with, claiming the client's
// idempotency key first: a retried request must not be a second turn.
func (h *handler) mintTurn(ctx context.Context, ws ids.WorkspaceID, idempotencyKey string, log dlog.Logger) (ids.TurnID, error) {
	turn := h.deps.MintTurn()
	if idempotencyKey == "" {
		log.Debug(opSubmit, "the submission carried no idempotency key", dlog.Context{"turn": string(turn)})
		return turn, nil
	}
	existing, err := h.deps.DB.ClaimIdempotencyKey(ctx, ws, idempotencyKey, turn)
	if err != nil {
		log.Error(opSubmit, "the idempotency key could not be claimed", dlog.Context{"cause": err.Error()})
		return "", fmt.Errorf("claim the idempotency key on %q: %w", ws, err)
	}
	if existing != nil {
		log.Warn(opSubmit, "the idempotency key already claimed a turn",
			dlog.Context{"existing_turn": string(*existing)})
		return "", ErrDuplicateSubmission
	}
	return turn, nil
}

// logger resolves a workspace's durable logger. Failing to resolve the
// workspace is an invariant violation, never a reason to write globally.
func (h *handler) logger(ctx context.Context, ws ids.WorkspaceID) (dlog.Logger, error) {
	record, err := h.deps.DB.Workspace(ctx, ws)
	if err != nil {
		h.deps.Log.Global().Error(opSubmit, "could not resolve the workspace",
			dlog.Context{"workspace": string(ws), "cause": err.Error()})
		return nil, fmt.Errorf("resolve workspace %q: %w", ws, err)
	}
	log, err := h.deps.Log.Workspace(record.Dir)
	if err != nil {
		h.deps.Log.Global().Error(opSubmit, "could not open the workspace log sink",
			dlog.Context{"workspace": string(ws), "dir": record.Dir, "cause": err.Error()})
		return nil, fmt.Errorf("open the log sink for %q: %w", ws, err)
	}
	return log.With(dlog.Context{"workspace": string(ws)}), nil
}

// RefusalReason composes the sentence a refusal card draws. It is composed HERE
// because the contract fixes composition on the daemon, and one author is what
// keeps the card and the rpc's answer saying the same thing.
func RefusalReason(command string) string {
	return command + " is not supported here"
}

// feedPanel converts the rpc's panel answer into the feed's row arm. The two
// carry the same four views; the conversion exists because the feed draws only
// what it can draw.
func feedPanel(panel *agentreplv1.SubmitPromptCommandPanel) *frontendv1.FeedCommandPanel {
	switch arm := panel.GetPanel().(type) {
	case *agentreplv1.SubmitPromptCommandPanel_Status:
		return &frontendv1.FeedCommandPanel{Panel: &frontendv1.FeedCommandPanel_Status{Status: arm.Status}}
	case *agentreplv1.SubmitPromptCommandPanel_Todos:
		return &frontendv1.FeedCommandPanel{Panel: &frontendv1.FeedCommandPanel_Todos{Todos: arm.Todos}}
	case *agentreplv1.SubmitPromptCommandPanel_Mcp:
		return &frontendv1.FeedCommandPanel{Panel: &frontendv1.FeedCommandPanel_Mcp{Mcp: arm.Mcp}}
	case *agentreplv1.SubmitPromptCommandPanel_Context:
		return &frontendv1.FeedCommandPanel{Panel: &frontendv1.FeedCommandPanel_Context{Context: arm.Context}}
	default:
		return nil
	}
}

// saidText renders a submission's text: the text blocks, joined. Recognition
// reads it and nothing else — an image carries no command.
func saidText(said *conversationv1.UserSaid) string {
	out := ""
	for _, block := range said.GetContent().GetBlocks() {
		if text, ok := block.GetBlock().(*conversationv1.UserContentBlock_Text); ok {
			if out != "" {
				out += "\n"
			}
			out += text.Text.GetText()
		}
	}
	return out
}

// recognitionName renders a recognition for a log record.
func recognitionName(r Recognition) string {
	switch r {
	case RecognizedNone:
		return "prompt"
	case RecognizedPanel:
		return "panel"
	case RecognizedRefused:
		return "refused"
	case RecognizedAct:
		return "session_act"
	default:
		return fmt.Sprintf("recognition(%d)", r)
	}
}
