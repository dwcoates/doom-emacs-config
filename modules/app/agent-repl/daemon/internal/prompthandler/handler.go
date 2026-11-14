package prompthandler

import (
	"context"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/holdfold"
	"agent-repld/internal/ids"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/prompts"
	"agent-repld/internal/runecap"
	"agent-repld/internal/sessioncommand"
	"agent-repld/internal/wsm"
)

// The handler's operation names. Every logical branch records under one of
// them, per the logging contract.
// BriefContextSummary is the brief the note carrying a queue-bypass switch's
// summary of the queue conversation is composed from.
const BriefContextSummary = "queue-bypass-context-summary"

// BriefRevisionsTold is the brief the note telling the agent what its revised
// final responses now say is composed from.
const BriefRevisionsTold = "response-revision-told"

// BriefRenameTold is the brief the note telling the agent the workspace's new
// name is composed from.
const BriefRenameTold = "workspace-renamed"

const (
	opNew       = "daemon.prompthandler.new"
	opSubmit    = "daemon.prompthandler.submit"
	opRecognize = "daemon.prompthandler.recognize"
	opPanel     = "daemon.prompthandler.panel"
	opRefuse    = "daemon.prompthandler.refuse"
	// opContextSummary files the summary a queue-bypass switch carried into
	// the bypass conversation, delivered by the workspace's next prompt.
	opContextSummary = "daemon.prompthandler.context_summary"
	// opRevisionsTold files the revised final responses a workspace's next
	// prompt tells its agent of.
	opRevisionsTold = "daemon.prompthandler.revisions_told"
	// opRenameTold files the new name a workspace's next prompt tells its
	// agent.
	opRenameTold = "daemon.prompthandler.rename_told"
	// opPlacementTold files the subsection placement note a prompt-less
	// root's next prompt carries.
	opPlacementTold = "daemon.prompthandler.placement_told"
)

// handler is SubmitPrompt's body: thin, stateless, done at submission. It owns
// NO execution and NO response formatting.
type handler struct {
	deps Deps
	// keys serializes the submissions of one idempotency key in this process;
	// see claim.go.
	keys *inflight
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
	case deps.Asks == nil:
		return nil, fmt.Errorf("the prompt handler needs a standing-ask answerer")
	case deps.Summaries == nil:
		return nil, fmt.Errorf("the prompt handler needs the workspace summaries' keeper")
	case deps.Placements == nil:
		return nil, fmt.Errorf("the prompt handler needs the owed placement notes")
	case deps.PromptsDir == "":
		return nil, fmt.Errorf("the prompt handler needs the briefs' directory")
	}
	if deps.MintTurn == nil {
		deps.MintTurn = wsm.NewTurnID
	}
	deps.Log.Global().Debug(opNew, "the prompt handler is wired", nil)
	return &handler{deps: deps, keys: newInflight()}, nil
}

// movedOn tells the wiring the workspace moved on, when anything listens.
func (h *handler) movedOn(ctx context.Context, ws ids.WorkspaceID) {
	if h.deps.MovedOn != nil {
		h.deps.MovedOn(ctx, ws)
	}
}

// Submit runs one submission, in the one order the contract fixes: the origin
// is checked before anything is minted or mirrored, the addressed feed is
// checked against the workspace, recognition forks the answer, and only an
// ordinary prompt reaches the queue.
func (h *handler) Submit(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, idempotencyKey string, origin conversationv1.PromptOrigin, sentBy *ids.WorkspaceID, delivery wsm.Delivery, target *feedid.Ref) (Outcome, error) {
	return h.submit(ctx, ws, said, idempotencyKey, origin, sentBy, delivery, target, nil)
}

// SubmitThreadQuestion is Submit for a zettel chat thread: an ordinary prompt
// reaches the queue carrying where its question was asked from. A thread
// question is never classified (the queue holds it for the running turn's
// end), so its delivery is the ordinary one and it addresses the root feed.
func (h *handler) SubmitThreadQuestion(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, idempotencyKey string, origin conversationv1.PromptOrigin, ask *wsm.ThreadAsk) (Outcome, error) {
	if ask == nil {
		panic("prompthandler: SubmitThreadQuestion needs the question's ask")
	}
	return h.submit(ctx, ws, said, idempotencyKey, origin, nil, wsm.DeliveryOrdinary, nil, ask)
}

// submit is Submit's body; ask is nil for every submission but a thread
// question's.
func (h *handler) submit(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, idempotencyKey string, origin conversationv1.PromptOrigin, sentBy *ids.WorkspaceID, delivery wsm.Delivery, target *feedid.Ref, ask *wsm.ThreadAsk) (Outcome, error) {
	log, err := h.logger(ctx, ws)
	if err != nil {
		return Outcome{}, err
	}
	log = log.With(dlog.Context{"origin": origin.String(), "idempotency_key": idempotencyKey, "delivery": delivery.String()})

	if origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_UNSPECIFIED {
		log.Warn(opSubmit, "the submission carries no origin", nil)
		return Outcome{}, ErrOriginRequired
	}
	if err := promptorigin.CheckSender(origin, sentBy != nil); err != nil {
		log.Error(opSubmit, "the submission's sender disagrees with its origin", dlog.Context{"sender_named": sentBy != nil})
		return Outcome{}, err
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

	c, err := h.mintTurn(ctx, ws, idempotencyKey, log)
	if err != nil {
		return Outcome{}, err
	}
	defer c.done()
	// A TYPED ANSWER IS DECIDED UNDER THE KEY'S CLAIM, so a retry of a send
	// that already answered an ask answers duplicate_submission rather than
	// typing the same text into the next question, or reaching the session as
	// a prompt.
	if answerable(ctx, said, origin, delivery) {
		answered, err := h.deps.Asks.AnswerStandingAsk(ctx, ws, target, text)
		if err != nil {
			return Outcome{}, err
		}
		if answered {
			if err := h.accept(ctx, ws, c, log); err != nil {
				return Outcome{}, err
			}
			h.movedOn(ctx, ws)
			log.Info(opSubmit, "the send was delivered as a standing ask's typed answer; no turn was minted", nil)
			return Outcome{Recognition: RecognizedAskAnswer}, nil
		}
	}
	turn := c.turn
	// A RE-DRIVEN INGRESS ENTRY IS CLAIMED FOR ITS TURN BEFORE THE QUEUE SEES
	// IT, so the held section drops the entry in the very publication that
	// draws the turn as a held prompt or a prompt row: never in both places,
	// never in neither.
	if isRedrive(ctx) && c.key != "" {
		h.deps.Feed.ClaimIngress(ws, c.key, turn)
	}
	said, err = h.askForOwedSummary(ctx, ws, said, origin, sentBy, target, ask)
	if err != nil {
		return Outcome{}, err
	}
	// THE NOTES THE DAEMON OWES THE AGENT ride the prompt, each taken once:
	// the name, then the revised responses, then the context summary open it,
	// the summary nearest the person's words, and the subsection placement
	// note closes it, after the summary ask. A failure puts back every note
	// already taken, and so does a refusal by the queue.
	carries := carriesWorkspaceNote(said, origin, sentBy, target, ask)
	var restores []func()
	restoreAll := func() {
		for _, restore := range restores {
			restore()
		}
	}
	for _, open := range []func(context.Context, ids.WorkspaceID, *conversationv1.UserSaid, bool, dlog.Logger) (*conversationv1.UserSaid, func(), error){
		h.closeWithOwedPlacement, h.openWithContextSummary, h.openWithOwedRevisions, h.openWithOwedRename,
	} {
		opened, restore, err := open(ctx, ws, said, carries, log)
		if err != nil {
			restoreAll()
			return Outcome{}, err
		}
		said, restores = opened, append(restores, restore)
	}
	disposition, err := h.deps.Queue.Submit(ctx, promptqueue.Submission{
		WS: ws, Turn: turn, Said: said, Origin: origin, SentBy: sentBy, Target: target, Delivery: delivery, Ask: ask,
	})
	if err != nil {
		restoreAll()
		return Outcome{}, err
	}
	if err := h.accept(ctx, ws, c, log); err != nil {
		return Outcome{}, err
	}
	h.movedOn(ctx, ws)
	log.Info(opSubmit, "the prompt was forwarded to the queue", dlog.Context{
		"turn": string(turn), "delivered": disposition.Delivered, "parked": disposition.Parked(),
		"thread_question": ask != nil,
	})
	return Outcome{Recognition: RecognizedNone, Turn: turn, Disposition: disposition}, nil
}

// carriesWorkspaceNote reports whether a prompt may carry a note the daemon
// owes the workspace's agent. Only a prompt that carries a directive does —
// the user's own, or another workspace's agent's — and only one to the main
// agent: a thread question, a bubble-addressed prompt and a daemon-born brief
// are never anyone's statement of the workspace's task. Nor does a slash
// command the vendor runs: the vendor reads the whole prompt as the command
// and its argument, so a note with it would become part of the command.
func carriesWorkspaceNote(said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *ids.WorkspaceID, target *feedid.Ref, ask *wsm.ThreadAsk) bool {
	if target != nil || ask != nil || (sentBy == nil && !promptorigin.UserSent(origin)) {
		return false
	}
	return !sessioncommand.Parse(saidText(said)).Slash
}

// askForOwedSummary closes SAID with the note asking the workspace's agent for
// its summary while none stands (wssummary): a new workspace, or one whose
// /clear dropped it. Only a prompt that carries a workspace note asks
// (carriesWorkspaceNote). The note rides as its own text block, wholly a meta
// span, so the bubble draws the user's words alone.
func (h *handler) askForOwedSummary(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, sentBy *ids.WorkspaceID, target *feedid.Ref, ask *wsm.ThreadAsk) (*conversationv1.UserSaid, error) {
	if !carriesWorkspaceNote(said, origin, sentBy, target, ask) {
		return said, nil
	}
	note, owed, err := h.deps.Summaries.OwedNote(ctx, ws)
	if err != nil || !owed {
		return said, err
	}
	return withNote(said, note, noteCloses), nil
}

// Where withNote puts its note.
const (
	noteOpens  = true
	noteCloses = false
)

// withNote answers SAID with NOTE as a text block of its own, ahead of every
// block the person sent when OPENS (noteOpens), after them otherwise. The one
// way a daemon note rides a prompt, so the bubble draws the person's words
// alone whichever note it carries.
func withNote(said *conversationv1.UserSaid, note string, opens bool) *conversationv1.UserSaid {
	block := &conversationv1.UserContentBlock{
		Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: note}},
	}
	blocks := make([]*conversationv1.UserContentBlock, 0, len(said.GetContent().GetBlocks())+1)
	if opens {
		blocks = append(blocks, block)
	}
	blocks = append(blocks, said.GetContent().GetBlocks()...)
	if !opens {
		blocks = append(blocks, block)
	}
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{Blocks: blocks}}
}

// owedNote is one note the daemon owes a workspace's agent, delivered by the
// workspace's next prompt that carries a workspace note (carriesWorkspaceNote)
// and TAKEN from the store, so exactly one prompt carries it. T is what is
// owed.
type owedNote[T any] struct {
	// op is the operation every record of the note carries.
	op string
	// take takes what is owed, false when nothing is.
	take func(context.Context, ids.WorkspaceID) (T, bool, error)
	// restore puts back what a prompt the queue refused had taken.
	restore func(context.Context, ids.WorkspaceID, T) error
	// compose is the meta-wrapped note telling it.
	compose func(T) (string, error)
	// fields is what each record says of it.
	fields func(T) dlog.Context
	// closes rides the note after the person's words; it opens them
	// otherwise.
	closes bool
	// The records: the take that failed, the note that could not be
	// composed, the restore that failed, and the note delivered. An empty
	// failure record is the take's owner's: it records its own failures,
	// so they are recorded once.
	takeFailed, composeFailed, restoreFailed, told string
}

// openWithOwed opens SAID with NOTE when it is owed, or closes it with NOTE
// when the note closes. Only a prompt that carries a workspace note carries
// it. What it takes, RESTORE puts back for
// the next prompt when the queue refuses this one; what cannot be composed is
// put back before the failure is answered, so nothing owed is lost.
func openWithOwed[T any](ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, carries bool, note owedNote[T], log dlog.Logger) (_ *conversationv1.UserSaid, restore func(), _ error) {
	none := func() {}
	if !carries {
		return said, none, nil
	}
	owed, found, err := note.take(ctx, ws)
	if err != nil {
		if note.takeFailed != "" {
			log.Error(note.op, note.takeFailed, dlog.Context{"cause": err.Error()})
		}
		return nil, none, fmt.Errorf("take what %q's next prompt owes (%s): %w", ws, note.op, err)
	}
	if !found {
		return said, none, nil
	}
	restore = func() {
		if err := note.restore(context.WithoutCancel(ctx), ws, owed); err != nil && note.restoreFailed != "" {
			log.Error(note.op, note.restoreFailed, dlog.WithCause(note.fields(owed), err))
		}
	}
	text, err := note.compose(owed)
	if err != nil {
		log.Error(note.op, note.composeFailed, dlog.WithCause(note.fields(owed), err))
		restore()
		return nil, none, fmt.Errorf("compose what %q's next prompt owes (%s): %w", ws, note.op, err)
	}
	log.Info(note.op, note.told, note.fields(owed))
	return withNote(said, text, !note.closes), restore, nil
}

// owedPlacement is the subsection placement note a prompt-less root owes,
// taken with the restore that owes it again.
type owedPlacement struct {
	note    string
	restore func()
}

// closeWithOwedPlacement closes SAID with the note asking the workspace's
// agent to file itself into a subsection, owed because it was created with no
// first message to carry it (workspace.Verbs.TakeOwedPlacementNote). The
// verbs compose the note and record their own failures; a prompt the queue
// refuses owes it again.
func (h *handler) closeWithOwedPlacement(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, carries bool, log dlog.Logger) (*conversationv1.UserSaid, func(), error) {
	return openWithOwed(ctx, ws, said, carries, owedNote[owedPlacement]{
		op: opPlacementTold,
		take: func(ctx context.Context, ws ids.WorkspaceID) (owedPlacement, bool, error) {
			note, owed, restore, err := h.deps.Placements.TakeOwedPlacementNote(ctx, ws)
			return owedPlacement{note: note, restore: restore}, owed, err
		},
		restore: func(_ context.Context, _ ids.WorkspaceID, owed owedPlacement) error {
			owed.restore()
			return nil
		},
		compose: func(owed owedPlacement) (string, error) { return owed.note, nil },
		fields:  func(owed owedPlacement) dlog.Context { return dlog.Context{"note_chars": len(owed.note)} },
		closes:  true,
		told:    "the prompt closes with the note asking the agent to file the workspace into a subsection",
	}, log)
}

// openWithContextSummary opens SAID with the summary of the workspace's queue
// conversation that a queue-bypass switch carried into its bypass
// conversation (wsm.TakeOwedContextSummary), meta-wrapped through the
// BriefContextSummary brief, so it is in context when the agent reads the
// prompt and no agent turn is started for it.
func (h *handler) openWithContextSummary(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, carries bool, log dlog.Logger) (*conversationv1.UserSaid, func(), error) {
	return openWithOwed(ctx, ws, said, carries, owedNote[string]{
		op:      opContextSummary,
		take:    h.deps.DB.TakeOwedContextSummary,
		restore: h.deps.DB.RestoreOwedContextSummary,
		compose: func(summary string) (string, error) { return contextSummaryNote(h.deps.PromptsDir, summary) },
		fields: func(summary string) dlog.Context {
			return dlog.Context{"summary_chars": len(summary), "brief": BriefContextSummary}
		},
		takeFailed:    "could not take the context summary the workspace's next prompt owes; the prompt was not forwarded",
		composeFailed: "could not compose the note carrying the context summary; the prompt was not forwarded",
		restoreFailed: "could not put back the context summary of a prompt the queue refused; it is lost",
		told:          "the prompt opens with the context summary the queue-bypass switch carried",
	}, log)
}

// openWithOwedRevisions opens SAID with the note telling the workspace's
// agent what its revised final responses now say (wsm.TakeOwedRevisions):
// each revision ran on a fork that persisted nothing, so the agent's own
// record still holds its original words.
func (h *handler) openWithOwedRevisions(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, carries bool, log dlog.Logger) (*conversationv1.UserSaid, func(), error) {
	return openWithOwed(ctx, ws, said, carries, owedNote[[]wsm.OwedRevision]{
		op: opRevisionsTold,
		take: func(ctx context.Context, ws ids.WorkspaceID) ([]wsm.OwedRevision, bool, error) {
			owed, err := h.deps.DB.TakeOwedRevisions(ctx, ws)
			return owed, len(owed) > 0, err
		},
		restore: h.deps.DB.RestoreOwedRevisions,
		compose: func(owed []wsm.OwedRevision) (string, error) { return revisionsNote(h.deps.PromptsDir, owed) },
		fields: func(owed []wsm.OwedRevision) dlog.Context {
			units := make([]string, 0, len(owed))
			for _, one := range owed {
				units = append(units, one.Unit)
			}
			return dlog.Context{"units": units, "brief": BriefRevisionsTold}
		},
		takeFailed:    "could not take the revisions the workspace's next prompt owes its agent; the prompt was not forwarded",
		composeFailed: "could not compose the note telling the agent of its revised responses; the prompt was not forwarded",
		restoreFailed: "could not put back the revisions of a prompt the queue refused; the agent will not be told of them",
		told:          "the prompt opens by telling the agent what its revised responses now say",
	}, log)
}

// openWithOwedRename opens SAID with the note telling the workspace's agent
// the workspace's new name (wsm.TakeOwedRename): a session is told its name
// only at its start, so after a rename its instructions name the old one.
func (h *handler) openWithOwedRename(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, carries bool, log dlog.Logger) (*conversationv1.UserSaid, func(), error) {
	return openWithOwed(ctx, ws, said, carries, owedNote[string]{
		op:            opRenameTold,
		take:          h.deps.DB.TakeOwedRename,
		restore:       h.deps.DB.RestoreOwedRename,
		compose:       func(name string) (string, error) { return renameNote(h.deps.PromptsDir, name) },
		fields:        func(name string) dlog.Context { return dlog.Context{"name": name, "brief": BriefRenameTold} },
		takeFailed:    "could not take the name the workspace's next prompt owes its agent; the prompt was not forwarded",
		composeFailed: "could not compose the note telling the agent the workspace's new name; the prompt was not forwarded",
		restoreFailed: "could not put back the name of a prompt the queue refused; the agent will not be told it",
		told:          "the prompt opens by telling the agent the workspace's new name",
	}, log)
}

// renameNote composes the meta-wrapped note telling the agent the workspace's
// new NAME, from the BriefRenameTold brief in PROMPTSDIR.
func renameNote(promptsDir, name string) (string, error) {
	brief, err := prompts.Load(promptsDir, BriefRenameTold)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefRenameTold, err)
	}
	note, err := brief.Splice(map[string]string{"name": name})
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefRenameTold, err)
	}
	return prompts.Wrap(note + "\n\n"), nil
}

// revisionHeadRunes is how much of a response's original words the note
// quotes to name which response was revised.
const revisionHeadRunes = 200

// revisionsNote composes the meta-wrapped note telling the agent what each of
// OWED now says, from the BriefRevisionsTold brief in PROMPTSDIR. Each names
// its response by the words it opened with, and quotes it as revised whole.
func revisionsNote(promptsDir string, owed []wsm.OwedRevision) (string, error) {
	brief, err := prompts.Load(promptsDir, BriefRevisionsTold)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefRevisionsTold, err)
	}
	parts := make([]string, 0, len(owed))
	for _, one := range owed {
		parts = append(parts, fmt.Sprintf("Your response that opened with:\n\n%s\n\nnow reads:\n\n%s", runecap.Head(one.Original, revisionHeadRunes), one.Revised))
	}
	note, err := brief.Splice(map[string]string{"revisions": strings.Join(parts, "\n\n---\n\n")})
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefRevisionsTold, err)
	}
	return prompts.Wrap(note + "\n\n"), nil
}

// contextSummaryNote composes the meta-wrapped note carrying SUMMARY from the
// BriefContextSummary brief in PROMPTSDIR.
func contextSummaryNote(promptsDir, summary string) (string, error) {
	brief, err := prompts.Load(promptsDir, BriefContextSummary)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefContextSummary, err)
	}
	note, err := brief.Splice(map[string]string{"summary": summary})
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefContextSummary, err)
	}
	return prompts.Wrap(note + "\n\n"), nil
}

// answerPanel resolves a recognized panel and MIRRORS it into the root feed as
// a non-durable row, so the answer is both returned and drawn.
func (h *handler) answerPanel(ctx context.Context, ws ids.WorkspaceID, got recognized, log dlog.Logger) (Outcome, error) {
	panel, err := h.deps.Panels(ctx, ws, got.spec.Command)
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
	act := promptqueue.Act{Kind: ActCommands[got.spec.Command], Value: got.arg, Origin: origin}
	var c claim
	if act.Kind != promptqueue.ActSetModel {
		minted, err := h.mintTurn(ctx, ws, idempotencyKey, log)
		if err != nil {
			return Outcome{}, err
		}
		c = minted
		defer c.done()
		act.Turn = c.turn
	}
	if err := h.deps.Queue.SubmitSessionAct(ctx, ws, act); err != nil {
		return Outcome{}, err
	}
	if err := h.accept(ctx, ws, c, log); err != nil {
		return Outcome{}, err
	}
	h.movedOn(ctx, ws)
	log.Info(opSubmit, "a session-acting command went down the one delivery path",
		dlog.Context{"command": got.literal, "act": act.Kind, "turn": string(act.Turn)})
	return Outcome{Recognition: RecognizedAct, Turn: act.Turn, Act: act}, nil
}

// mintTurn mints the turn the daemon acknowledges with, claiming the client's
// idempotency key first: a retried request must not be a second turn, and a
// retry of a submission the queue never accepted must not be refused.
//
// The returned claim holds the key against a concurrent submission of it in
// this process until the caller calls done.
func (h *handler) mintTurn(ctx context.Context, ws ids.WorkspaceID, idempotencyKey string, log dlog.Logger) (claim, error) {
	turn := h.deps.MintTurn()
	if idempotencyKey == "" {
		log.Debug(opSubmit, "the submission carried no idempotency key", dlog.Context{"turn": string(turn)})
		return claim{turn: turn}, nil
	}
	release, waited, err := h.keys.acquire(ctx, ws, idempotencyKey)
	if err != nil {
		log.Info(opSubmit, "the caller gave up waiting on the key's in-flight submission; nothing was claimed",
			dlog.Context{"cause": err.Error()})
		return claim{}, fmt.Errorf("wait for the in-flight submission of the idempotency key on %q: %w", ws, err)
	}
	if waited {
		log.Debug(opSubmit, "waited for the key's in-flight submission to finish before claiming", nil)
	}
	got, err := h.deps.DB.ClaimIdempotencyKey(ctx, ws, idempotencyKey, turn)
	if err != nil {
		release()
		log.Error(opSubmit, "the idempotency key could not be claimed", dlog.Context{"cause": err.Error()})
		return claim{}, fmt.Errorf("claim the idempotency key on %q: %w", ws, err)
	}
	switch got.Standing {
	case wsm.ClaimAccepted:
		release()
		fields := dlog.Context{"existing_turn": string(got.Turn), "evidence": got.Evidence}
		if isRedrive(ctx) {
			log.Info(opSubmit, "the re-driven submission's idempotency key was already accepted; the earlier submission stands", fields)
			return claim{}, ErrDuplicateSubmission
		}
		log.Warn(opSubmit, "the idempotency key already claimed a turn", fields)
		return claim{}, ErrDuplicateSubmission
	case wsm.ClaimRedriven:
		// THE RETRY KEEPS THE FIRST SUBMISSION'S TURN ID. A process that died
		// after the shim accepted that turn and before the stamp committed left
		// exactly this claim, and the shim answers a repeated start of a turn
		// id it already accepted as a no-op; a fresh id would start the prompt
		// a second time.
		// A RE-DRIVE MEETS THIS CLAIM ON EVERY RETRY of an entry the queue
		// keeps refusing, so there it is DEBUG: the re-driver records the
		// entry's outcome itself.
		level := log.Info
		if isRedrive(ctx) {
			level = log.Debug
		}
		level(opSubmit, "the key's earlier submission never reached the queue's acceptance; the retry is driven under the same turn",
			dlog.Context{"turn": string(got.Turn), "offered_turn": string(turn), "reopened": got.Reopened})
	case wsm.ClaimMinted:
		log.Debug(opSubmit, "claimed the idempotency key", dlog.Context{"turn": string(got.Turn)})
	default:
		release()
		log.Error(opSubmit, "the idempotency claim answered a standing the handler does not know",
			dlog.Context{"standing": got.Standing.String()})
		return claim{}, fmt.Errorf("claim the idempotency key on %q: unknown standing %s", ws, got.Standing)
	}
	return claim{turn: got.Turn, key: idempotencyKey, release: release}, nil
}

// accept stamps the claim accepted once the queue took the submission, so only
// now does a retry of the key answer duplicate_submission.
//
// THE STAMP OUTLIVES THE REQUEST. The queue already took the prompt, so a
// caller whose context ends in this instant must not leave the claim reading
// unaccepted -- its retry would be re-driven into a second delivery.
func (h *handler) accept(ctx context.Context, ws ids.WorkspaceID, c claim, log dlog.Logger) error {
	if c.key == "" {
		return nil
	}
	if err := h.deps.DB.AcceptIdempotencyKey(context.WithoutCancel(ctx), ws, c.key, c.turn); err != nil {
		log.Error(opSubmit, "the queue accepted the submission but its idempotency claim could not be recorded accepted; a retry of the key would be driven again",
			dlog.Context{"turn": string(c.turn), "cause": err.Error()})
		return fmt.Errorf("record the idempotency key accepted on %q: %w", ws, err)
	}
	return nil
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
	// RESOLVING A NAMED WORKSPACE'S SINK IS A TOTAL FUNCTION, and a prompt is
	// never lost over WHERE its narration is written. A directory that cannot
	// host a durable sink routes to the central sink carrying
	// `unroutable_workspace'; only the WORKSPACE READ above can refuse.
	log := h.deps.Log.WorkspaceOrCentral(record.Dir)
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

// answerable reports whether a submission can be a typed answer to a standing
// ask: a plain user send (TypedAnswerOrigins), with the ordinary delivery,
// that is not a re-drive of an earlier attempt, carrying typed words and no
// image. A deferred prompt asked to run as its own turn; a re-drive's ask may
// be long gone; an image cannot be part of an answer.
func answerable(ctx context.Context, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, delivery wsm.Delivery) bool {
	if !TypedAnswerOrigins[origin] || delivery != wsm.DeliveryOrdinary || isRedrive(ctx) {
		return false
	}
	words := false
	for _, block := range said.GetContent().GetBlocks() {
		switch b := block.GetBlock().(type) {
		case *conversationv1.UserContentBlock_Image:
			return false
		case *conversationv1.UserContentBlock_Text:
			words = words || strings.TrimSpace(b.Text.GetText()) != ""
		}
	}
	return words
}

// saidText renders a submission's text for recognition: holdfold.SaidText,
// the one reading the classifier and the hold tray share. An image carries no
// command, and a reply's quote block is not the person's words, so a command
// typed as a reply is still recognized.
func saidText(said *conversationv1.UserSaid) string {
	return holdfold.SaidText(said)
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
	case RecognizedAskAnswer:
		return "ask_answer"
	default:
		return fmt.Sprintf("recognition(%d)", r)
	}
}
