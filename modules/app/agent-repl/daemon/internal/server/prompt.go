package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptorigin"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/replyquote"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// SubmitPrompt is the composer's submission, whole. The handler validates,
// resolves the workspace, decodes the addressed feed and DELEGATES to the
// prompt handler; recognition, mirroring and delivery all live there.
func (s *server) SubmitPrompt(
	ctx context.Context,
	req *connect.Request[agentreplv1.SubmitPromptRequest],
) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
	const rpc = "SubmitPrompt"
	if err := validateSubmitPromptRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.SubmitPromptResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, submitRefusal(*r)))
	}
	// A CHAT THREAD'S PROMPT IS A QUESTION, asked of the thread's graph: it
	// takes the chat's highlight rather than a feed selection, and addresses
	// no sub-feed.
	if subject.Record.IsThread() {
		if req.Msg.Feed != nil {
			return nil, invalid("feed", "a chat thread has no sub-feed to address")
		}
		if req.Msg.GetSentBy() != nil {
			return nil, invalid("sent_by", "a chat thread's questions are the user's, never an agent's")
		}
		return s.submitThreadQuestion(ctx, subject, req.Msg, resp)
	}
	sentBy, senderRefused, err := s.submitSender(ctx, subject, req.Msg.GetSentBy())
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	if senderRefused != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, *senderRefused))
	}
	// EVERY ANSWER FROM HERE ON IS ALSO THE SENDER'S ROW, for a prompt an agent
	// sent: refuse and accept draw how the send fared in the sender's feed.
	sent := sentPrompt{sentBy: sentBy, recipient: subject.Record.Name, key: req.Msg.GetIdempotencyKey(), said: req.Msg.GetSaid()}
	refuse := func(r refusal) (*connect.Response[agentreplv1.SubmitPromptResponse], error) {
		s.drawSent(sent, feed.SentFate{Refused: &r.Reason})
		return answer(resp, s.refuse(subject.Log, rpc, resp, r))
	}
	if command, refused := agentSlashCommand(req.Msg.GetOrigin(), req.Msg.GetSaid()); refused {
		return refuse(s.agentSlashRefusal(command))
	}

	// The FeedId is decoded HERE, before the handler is called: a value that
	// does not decode never becomes a handler call, which is why the prompt
	// handler produces no feed_undecodable of its own.
	var target *feedid.Ref
	if req.Msg.Feed != nil {
		ref, decodeErr := feedid.Decode(req.Msg.GetFeed())
		if decodeErr != nil {
			return refuse(submitRefusal(s.fill(refusal{
				Arm: "feed_undecodable",
				Reason: fmt.Sprintf("the feed id %q does not decode: %v",
					req.Msg.GetFeed().GetValue(), decodeErr),
			})))
		}
		if ref.WS != subject.Record.ID {
			return refuse(submitRefusal(s.fill(refusal{
				Arm: "feed_not_in_workspace",
				Reason: fmt.Sprintf("the feed id %q belongs to workspace %q",
					req.Msg.GetFeed().GetValue(), ref.WS),
			})))
		}
		target = &ref
	}

	// REPLY-TO: when the feed has a bubble SELECTED as this prompt is accepted
	// — a final response, a prompt, or any other selected bubble — the DAEMON
	// quotes it into the prompt as its own leading block BEFORE the prompt
	// reaches the shim, so the agent knows the new message refers to it and
	// the record keeps the quote apart from the person's words (replyquote). The daemon is the
	// selection's only holder, so the reply is always the one the webapp was
	// drawing. The selection is read ONCE, and only that selection is ended
	// after the send: a row selected in the meantime stays selected.
	// THE QUOTE READS THE SELECTED BUBBLE, WHETHER OR NOT IT CAN STILL BE
	// SELECTED: selectability gates only MAKING a selection, and a response
	// selected while it was its turn's latest row turns unselectable the moment
	// a later row proves it interim, with the reader's selection still on it.
	// A selected row the daemon cannot resolve to a landed bubble is REFUSED,
	// never dropped: delivering the user's message shorn of the reference they
	// saw would silently change what they said.
	// AN AGENT'S PROMPT TAKES NO SELECTION: the selection is the user's, in
	// the composer they are reading, so an agent's prompt neither quotes it nor
	// ends it.
	said := req.Msg.GetSaid()
	var (
		sentWith          *frontendv1.FeedId
		sentWithSelection bool
	)
	if sentBy == nil {
		sentWith, _, sentWithSelection = s.currentSelection(subject.Record.ID)
	}
	if ref := sentWith; sentWithSelection {
		quoted, ok := s.deps.Feed.BubbleText(subject.Record.ID, ref)
		if !ok {
			return refuse(submitRefusal(s.fill(refusal{
				Arm: "reference_response_unresolvable",
				Reason: fmt.Sprintf("the referenced row %q is not a landed bubble of this workspace's root feed",
					ref.GetValue()),
				NotFound: true,
			})))
		}
		said = replyquote.Quote(said, quoted.Markdown, quoted.Prompt, ref.GetValue())
	}

	// VALIDATED ABOVE through the same mapping, so a failure here is a
	// defect, surfaced as one.
	delivery, err := prompthandler.DeliveryOf(req.Msg.Delivery)
	if err != nil {
		return nil, fail(subject.Log, rpc, err)
	}
	// THE USER STEPPING IN TAKES A ONE-SHOT BACK, before the prompt reaches
	// the agent: it no longer finishes unattended. An agent's prompt never
	// does (promptorigin.UserSent).
	if sentBy == nil && promptorigin.UserSent(req.Msg.GetOrigin()) {
		if err := s.deps.Verbs.TakeBackOneShot(ctx, subject.Record.ID, workspace.TakenBackByUserPrompt); err != nil {
			return nil, fail(subject.Log, rpc, err)
		}
	}
	outcome, err := s.deps.Prompts.Submit(ctx, subject.Record.ID, said,
		req.Msg.GetIdempotencyKey(), req.Msg.GetOrigin(), sentBy, delivery, target)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return refuse(submitRefusal(modelActRefused(err, bubbleRefused(askRefused(refused)))))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	cerr := s.encodeSubmitOutcome(subject.Log, resp, outcome)
	// A SEND THE PERSON MADE IS TIMED TO ITS ECHO by the page (webecho.go),
	// once its row is drawn.
	if echo, ok := promptEchoOf(req.Msg, outcome); ok && cerr == nil && resp.GetError() == nil {
		s.pushPromptEcho(subject.Log, subject.Record.ID, echo)
	}
	if cerr == nil {
		s.drawSent(sent, sentFateOf(outcome.Disposition))
	}
	// AN ACCEPTED PROMPT ENDS THE SELECTION it was sent with, a reply target
	// or a prompt alike (a genuine success — a minted turn, a hold, a command
	// answer — never a refusal arm or a transport error), and the webapp
	// returns to the tail where the new prompt lands.
	// A TYPED ANSWER TO A STANDING ASK is not the prompt the selection was
	// chosen for, so it leaves the selection standing.
	if sentWithSelection && cerr == nil && resp.GetError() == nil && outcome.Recognition != prompthandler.RecognizedAskAnswer {
		s.endSelection(subject.Log, subject.Record.ID, sentWith, returnToTail(), "prompt_sent")
	}
	return answer(resp, cerr)
}

// sentPrompt is an agent-sent prompt as its sender's row draws it: no sender
// for a prompt no agent sent.
type sentPrompt struct {
	sentBy    *ids.WorkspaceID
	recipient string
	key       string
	said      *conversationv1.UserSaid
}

// drawSent draws SENT's row in its sender's feed with FATE, and nothing for a
// prompt no agent sent.
func (s *server) drawSent(sent sentPrompt, fate feed.SentFate) {
	if sent.sentBy == nil {
		return
	}
	s.deps.Feed.SentPrompt(*sent.sentBy, sent.recipient, sent.key, sent.said, fate)
}

// sentFateOf is how an accepted send fared: held behind the recipient's work,
// or refused outright by a merge's refusing lease (RefusedArm), or delivered.
func sentFateOf(d promptqueue.Disposition) feed.SentFate {
	if d.RefusedArm != "" {
		why := fmt.Sprintf("the workspace refused the prompt (%s)", d.RefusedArm)
		return feed.SentFate{Refused: &why}
	}
	return feed.SentFate{Queued: d.Parked()}
}

// submitSender resolves the workspace an agent-sent prompt names as its
// sender, nil for a prompt no agent sent (validation has paired the two). A
// sender that is no other registered workspace is refused `unknown_sender`:
// the recipient's feed heads the prompt with it, and no workspace is known to
// have sent it.
func (s *server) submitSender(ctx context.Context, subject resolved, ref *workspacev1.WorkspaceRef) (*ids.WorkspaceID, *refusal, error) {
	if ref == nil {
		return nil, nil, nil
	}
	id := ids.WorkspaceID(ref.GetId())
	_, err := s.deps.DB.Workspace(ctx, id)
	switch {
	case id == subject.Record.ID, errors.Is(err, wsm.ErrNotFound):
		r := submitRefusal(s.fill(refusal{
			Arm:    "unknown_sender",
			Reason: fmt.Sprintf("workspace %s is no other registered workspace to have sent the prompt", id),
			Fields: map[string]any{"sent_by": ref},
		}))
		return nil, &r, nil
	case err != nil:
		subject.Log.Error(opSubmit, "could not read the workspace an agent-sent prompt names as its sender",
			dlog.Context{"sent_by": string(id), "cause": err.Error()})
		return nil, nil, fmt.Errorf("read sender %q: %w", id, err)
	}
	return &id, nil, nil
}

// encodeSubmitOutcome renders the handler's outcome as SubmitPromptSuccess.
//
// A HOLD IS AN ANSWER: a prompt the queue parked still answers with the turn it
// minted, because the composer matches its own row against that turn id.
//
// A SESSION-ACT command answers by WHAT IT DID. The two context cuts (/clear,
// /compact) reach the vendor AS TURNS, so they answer with the minted turn the
// composer matches its own row against; an act that mints no turn (/model
// <arg>, and the topbar picker's setter path that meets recognition here)
// answers `SubmitPromptSuccess.command_acted`, which is empty because the set
// arm is the whole assertion — the visible effect arrives on the component
// streams. (Landing 6.)
func (s *server) encodeSubmitOutcome(
	log dlog.Logger,
	resp *agentreplv1.SubmitPromptResponse,
	outcome prompthandler.Outcome,
) *connect.Error {
	switch outcome.Recognition {
	case prompthandler.RecognizedPanel:
		log.Debug("daemon.server.submit_prompt", "answered a recognized command panel", nil)
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_CommandPanel{CommandPanel: outcome.Panel},
			},
		}
		return nil
	case prompthandler.RecognizedRefused:
		log.Debug("daemon.server.submit_prompt", "answered a recognized command refusal",
			dlog.Context{"command": outcome.RefusedCommand})
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_CommandRefused{
					CommandRefused: &agentreplv1.SubmitPromptCommandRefused{
						Command: outcome.RefusedCommand,
					},
				},
			},
		}
		return nil
	case prompthandler.RecognizedAskAnswer:
		log.Debug("daemon.server.submit_prompt", "answered that the send was a standing ask's typed answer", nil)
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_AskAnswered{AskAnswered: &agentreplv1.SubmitPromptAskAnswered{}},
			},
		}
		return nil
	case prompthandler.RecognizedAct:
		if outcome.Turn != "" {
			log.Debug("daemon.server.submit_prompt", "answered the turn a context-cutting act runs as",
				dlog.Context{"act": outcome.Act.Kind, "turn": string(outcome.Turn)})
			resp.Result = &agentreplv1.SubmitPromptResponse_Success{
				Success: &agentreplv1.SubmitPromptSuccess{
					Outcome: &agentreplv1.SubmitPromptSuccess_Turn{
						Turn: &agentreplv1.SubmitPromptTurn{
							Turn: &conversationv1.TurnId{Value: string(outcome.Turn)},
						},
					},
				},
			}
			return nil
		}
		log.Debug("daemon.server.submit_prompt", "answered an acted session command",
			dlog.Context{"act": outcome.Act.Kind})
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_CommandActed{
					CommandActed: &agentreplv1.SubmitPromptCommandActed{},
				},
			},
		}
		return nil
	default:
		if arm := outcome.Disposition.RefusedArm; arm != "" {
			return s.refuse(log, "SubmitPrompt", resp, submitRefusal(s.fill(refusal{
				Arm:    arm,
				Reason: "the delivery path refused the submission",
			})))
		}
		log.Debug("daemon.server.submit_prompt", "answered the minted turn",
			dlog.Context{"turn": string(outcome.Turn), "parked": outcome.Disposition.Parked()})
		resp.Result = &agentreplv1.SubmitPromptResponse_Success{
			Success: &agentreplv1.SubmitPromptSuccess{
				Outcome: &agentreplv1.SubmitPromptSuccess_Turn{
					Turn: &agentreplv1.SubmitPromptTurn{
						Turn: &conversationv1.TurnId{Value: string(outcome.Turn)},
					},
				},
			},
		}
		return nil
	}
}

// RequestCommandSupport spawns a support workspace from the daemon-composed
// add-support brief. The brief's absence is LOUD, never papered over.
func (s *server) RequestCommandSupport(
	ctx context.Context,
	req *connect.Request[agentreplv1.RequestCommandSupportRequest],
) (*connect.Response[agentreplv1.RequestCommandSupportResponse], error) {
	const rpc = "RequestCommandSupport"
	if err := validateRequestCommandSupportRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.RequestCommandSupportResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	created, err := s.deps.Verbs.RequestCommandSupport(ctx, subject.Record.ID, req.Msg.GetCommand())
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	subject.Log.Info("daemon.server.request_command_support", "spawned a support workspace",
		dlog.Context{"command": req.Msg.GetCommand(), "created": string(created.ID)})
	resp.Result = &agentreplv1.RequestCommandSupportResponse_Success{
		Success: &agentreplv1.RequestCommandSupportSuccess{Workspace: created.Ref()},
	}
	return connect.NewResponse(resp), nil
}

// opSubmit is the operation EVERY typed refusal of SubmitPrompt is filed
// under. It is the prompt handler's own op, deliberately: a refused submission
// is part of the prompt's account and belongs beside the delivery path's
// records rather than under a name of its own.
const opSubmit = "daemon.prompthandler.submit"

// submitRefusal marks one SubmitPrompt refusal as the RECORDED ANSWER it is.
//
// GROUNDED (owner's report, 2026-09-14): three prompts were refused inside
// thirteen seconds and the daemon wrote NO record of any of them — the refusal
// path logs at DEBUG, and production runs at INFO, so the whole episode existed
// only in the editor's log. A refusal is an answer the daemon gave; an answer
// nothing records is an invisible action, which is a logging defect. Every arm
// is now one INFO record naming the arm, exactly as AdoptWebWorkspace's
// ordinary refusal has been since it was ruled one.
func submitRefusal(r refusal) refusal {
	r.Info = true
	r.Op = opSubmit
	return r
}

// The bubble-addressed submit refusals the SHIM makes, and the kind each one
// takes on the LANDED arm `SubmitPromptError.bubble_refused {detail; kind:
// not_deliverable | agent_busy}` (landing 7). The daemon never judges a
// subagent's turn itself: it relays the shim's verdict by name.
const (
	// armBubbleRefused is the landed SubmitPromptError arm both shapes answer
	// under.
	armBubbleRefused = "bubble_refused"
	// armModelNotInCatalog is SubmitPromptError.model_not_in_catalog: a `/model`
	// act named a model the session's catalog does not hold.
	armModelNotInCatalog = "model_not_in_catalog"
	// armModelRefused is SubmitPromptError.model_refused: the vendor refused the
	// model change a `/model` act asked for.
	armModelRefused = "model_refused"
	// bubbleKindNotDeliverable is the shim's UpdateAgentFailure.not_deliverable:
	// the SDK has no route to the addressed subagent.
	bubbleKindNotDeliverable = "not_deliverable"
	// bubbleKindAgentBusy is the shim's UpdateAgentFailure.agent_busy: the
	// addressed subagent's own turn is already running.
	bubbleKindAgentBusy = "agent_busy"
)

// bubbleRefused folds the shim's two bubble-addressed submit refusals onto the
// one landed arm, naming the kind as the arm's own nested oneof and keeping the
// shim's sentence as `detail`. Any other refusal passes through untouched.
// modelActRefused renames a refused `/model` act onto SubmitPrompt's own model
// arms. The shim names its SetSessionModel refusals by ITS verb's arms
// (`model_not_in_catalog`, `vendor_refused`), and `vendor_refused` is too
// generic a name for a prompt-level answer: a client must be able to tell "the
// model you asked for was refused" from any other refusal, and say so rather
// than hold the prompt as if the daemon were unreachable. Every other refusal
// passes through unchanged.
func modelActRefused(err error, r refusal) refusal {
	var shim *workspace.ShimRefusal
	if !errors.As(err, &shim) || shim.Verb != "SetSessionModel" {
		return r
	}
	switch shim.Arm {
	case workspace.ArmShimModelNotInCatalog:
		r.Arm = armModelNotInCatalog
	case "vendor_refused":
		r.Arm = armModelRefused
	}
	return r
}

// askRefused names a typed answer's refusal by the landed
// SubmitPromptError.ask_not_standing arm when the ask it was routed to settled
// first: the shim's `no_open_ask` (the agent stopped asking before the answer
// reached it) is that same fact said by the shim.
func askRefused(r refusal) refusal {
	if r.Arm == workspace.ArmShimNoOpenAsk {
		r.Arm = workspace.ArmAskNotStanding
	}
	return r
}

func bubbleRefused(r refusal) refusal {
	var kind string
	switch r.Arm {
	case bubbleKindNotDeliverable:
		kind = bubbleKindNotDeliverable
	case bubbleKindAgentBusy:
		kind = bubbleKindAgentBusy
	default:
		return r
	}
	r.Arm = armBubbleRefused
	// The map is COPIED rather than mutated: it may be the component's own.
	fields := make(map[string]any, len(r.Fields)+1)
	for name, value := range r.Fields {
		fields[name] = value
	}
	fields["kind"] = refusalarm.Nested(kind)
	r.Fields = fields
	return r
}
