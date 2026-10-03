package workspace

import (
	"context"
	"fmt"
	"math"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/sessionwatcher"
)

// THE QUEUE'S DELIVERY SURFACE.
//
// The prompt queue delivers through a narrow Sender rather than through a shim
// client, so it is testable against a fake instead of a process. The narrowing
// lives here, beside the fleet, because the fleet is what holds the client and
// because every shim refusal is already given its typed arm in this package —
// a second unwrapping elsewhere would answer the same failures differently.

// A TURN OPENING NEVER REPLAYS HISTORY (owner rule, 2026-09-23; feed paging
// on demand, 2026-10-02). The main agent's standing watch serves everything
// the turn writes live, so the page owes the views nothing: StartTurn opens
// tail_only when the daemon holds nothing of the main agent, and otherwise
// catches up from the newest pointer it holds (see sender.known). The page's
// size is the store's; the daemon states none.

// coldThresholdPolicy is the context size, in tokens, above which a MODEL
// CHANGE is refused as an unasked cold-cache cost rather than paid. It is
// stated as the largest size the field can carry, so no context is above it:
// the user picking a served model IS the request to pay, and the daemon's job
// here is to state a policy rather than to leave the field unset and have the
// shim read zero (see SetModel).
const coldThresholdPolicy = math.MaxUint64

// Sender answers the workspace's queue delivery surface, false when no session
// is up. It is the promptqueue.ClientFunc the queue is wired with.
func (f *Fleet) Sender(ws ids.WorkspaceID) (promptqueue.Sender, bool) {
	client, ok := f.Client(ws)
	if !ok {
		return nil, false
	}
	// A HELD SHIM WITH NO SESSION DELIVERS NOTHING: a turn needs a session.
	// The queue reads this "no session up" and revives, and the revival
	// starts the session on the held shim. A shim parked at its cold gate
	// stays the queue's to address, as it always was: the gate refuses a
	// submission by its own name before any delivery.
	if !f.Live(ws) {
		return nil, false
	}
	return &sender{client: client, known: func() *conversationv1.HistoryPointer {
		// Read AT THE CALL, not here: the queue may hold this sender across a
		// wait, and the pointer the turn must be bounded by is the newest one
		// the main watch has been served when the turn is handed over.
		if watcher, ok := f.sessionWatcher(ws); ok {
			return watcher.MainKnownThrough()
		}
		return nil
	}}, true
}

// Watcher answers the workspace's session watcher, false when no session is
// up. It is the promptqueue.WatcherFunc: an accepted turn is handed over to
// the watcher that will see it end.
func (f *Fleet) Watcher(ws ids.WorkspaceID) (promptqueue.Watcher, bool) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return nil, false
	}
	return session.watcher, true
}

// sessionWatcher answers the workspace's watcher on its FULL surface, for the
// callers that drive its lifecycle rather than a turn's. Watcher above narrows
// it to the prompt queue's contract, which carries none of that.
func (f *Fleet) sessionWatcher(ws ids.WorkspaceID) (sessionwatcher.Watcher, bool) {
	f.mu.RLock()
	session, ok := f.sessions[ws]
	f.mu.RUnlock()
	if !ok || session.watcher == nil {
		return nil, false
	}
	return session.watcher, true
}

// sender narrows a shim client to the queue's delivery surface.
type sender struct {
	client shimclientSender
	// known answers the main agent's watch's newest served pointer, nil when
	// it was served none (or no watcher is up). StartTurn states it as
	// known_through.
	known func() *conversationv1.HistoryPointer
}

// shimclientSender is the slice of the shim client the delivery surface uses.
// It is named so the adapter can be exercised against a fake client without
// the whole Client interface behind it.
type shimclientSender interface {
	StartTurn(ctx context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error)
	UpdateAgent(ctx context.Context, req *shimv1.UpdateAgentRequest) (*shimv1.UpdateAgentResponse, error)
	KillTurn(ctx context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error)
	SetSessionModel(ctx context.Context, req *shimv1.SetSessionModelRequest) (*shimv1.SetSessionModelResponse, error)
	SetSessionPermissionMode(ctx context.Context, req *shimv1.SetSessionPermissionModeRequest) (*shimv1.SetSessionPermissionModeResponse, error)
}

// StartTurn opens the turn and hands back the shim's success WHOLE: the queue
// needs the prompt's agent for SetMainAgent and the opening page for the
// watcher's turn handover, and neither is recoverable from an error.
func (s *sender) StartTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (*shimv1.StartTurnSuccess, error) {
	return s.startTurn(ctx, turn, said, origin, turnStart{})
}

// JoinRunningTurn sends the prompt to join the daemon's turn in flight after
// its current tool call (StartTurnRequest.join_running_turn), answered exactly
// as StartTurn is.
func (s *sender) JoinRunningTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) (*shimv1.StartTurnSuccess, error) {
	return s.startTurn(ctx, turn, said, origin, turnStart{join: true})
}

// StartInterjection opens the turn of a prompt that interrupted the running
// one, with NOTE for the agent alone (StartTurnRequest.vendor_note), answered
// exactly as StartTurn is.
func (s *sender) StartInterjection(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, note string) (*shimv1.StartTurnSuccess, error) {
	return s.startTurn(ctx, turn, said, origin, turnStart{note: note})
}

// turnStart is how a StartTurn differs between the verbs that make one.
type turnStart struct {
	// join asks the shim to join the running turn.
	join bool
	// note is the vendor note, empty for none.
	note string
}

// startTurn is the one StartTurn call every verb makes.
func (s *sender) startTurn(ctx context.Context, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin, how turnStart) (*shimv1.StartTurnSuccess, error) {
	req := &shimv1.StartTurnRequest{
		Turn:            &conversationv1.TurnId{Value: string(turn)},
		Said:            said,
		Origin:          origin,
		JoinRunningTurn: how.join,
	}
	if held := s.knownThrough(); held != nil {
		req.Opening = &shimv1.StartTurnRequest_KnownThrough{KnownThrough: held}
	} else {
		req.Opening = &shimv1.StartTurnRequest_TailOnly{TailOnly: &shimv1.StartTurnTailOnly{}}
	}
	if how.note != "" {
		req.VendorNote = &how.note
	}
	response, err := s.client.StartTurn(ctx, req)
	if err != nil {
		return nil, err
	}
	if failure := response.GetFailure(); failure != nil {
		return nil, &ShimRefusal{Verb: "StartTurn", Arm: startTurnArm(failure), Detail: failure.GetDetail()}
	}
	success := response.GetSuccess()
	if success == nil {
		return nil, fmt.Errorf("shim StartTurn answered neither success nor failure for turn %q", turn)
	}
	return success, nil
}

// knownThrough is the pointer StartTurn is bounded by, nil when there is none.
func (s *sender) knownThrough() *conversationv1.HistoryPointer {
	if s.known == nil {
		return nil
	}
	return s.known()
}

// PromptAgent delivers a bubble-composer prompt to one agent, through the same
// UpdateAgent path every other agent-addressed input takes.
func (s *sender) PromptAgent(ctx context.Context, agent *conversationv1.AgentId, said *conversationv1.UserSaid) error {
	response, err := s.client.UpdateAgent(ctx, &shimv1.UpdateAgentRequest{
		Target: agent,
		Input: &conversationv1.AgentInput{
			Input: &conversationv1.AgentInput_Prompt{Prompt: said},
		},
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "UpdateAgent", Arm: updateAgentArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// KillTurn interrupts the open turn for an interjection.
func (s *sender) KillTurn(ctx context.Context, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error {
	return killTurn(ctx, s.client, turn, force, commandedBy)
}

// turnKiller is the one shim call killTurn needs, satisfied by both the queue's
// narrowed client and the verbs' whole one.
type turnKiller interface {
	KillTurn(ctx context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error)
}

// killTurn is THE ONE BUILDER of a KillTurn request and the one reading of its
// answer, shared by the queue's sender and the verbs' shim adapter. The two
// were line-for-line copies; one of them growing a field the other forgot
// would make the same stop say different things depending on who asked.
//
// commandedBy travels exactly as the caller stated it, nil included: the shim
// records it verbatim as the interrupted terminal's `by_user` cause.
func killTurn(ctx context.Context, client turnKiller, turn ids.TurnID, force bool, commandedBy *conversationv1.AgentInterruptedByUser) error {
	response, err := client.KillTurn(ctx, &shimv1.KillTurnRequest{
		Turn:        &conversationv1.TurnId{Value: string(turn)},
		Force:       force,
		CommandedBy: commandedBy,
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "KillTurn", Arm: killTurnArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// SetModel switches the session's model, stating THE DAEMON'S COLD-THRESHOLD
// POLICY on the call.
//
// The threshold is what the shim measures against, and the shim refuses `cold`
// when the context is STRICTLY ABOVE it, so a threshold of zero refuses every
// switch on a conversation that has had one assistant turn — which is every
// conversation a user would ever change the model of. The daemon used to send
// the field unset, so the shim read zero, and a model the daemon itself had
// just served in the topbar's own catalog came back refused. The refusal then
// then had no `SetModelError` arm to land on and left the rpc as a transport
// error, so the topbar's model cell read as an unreachable daemon rather than
// as the model the user picked. `SetModelError.cold` (landing 10) is that arm,
// and the refusal now relays by name.
//
// THE PICK IS THE CONSENT. The cold gate exists so a cold context is never
// paid UNASKED — the resume path pays it with nobody having chosen it. A model
// change is the opposite case: the user selected an option the daemon served,
// on a surface whose whole purpose is to change the model, and the contract's
// remediation menu (pay | clear | compact) has no answering path from this verb
// to choose between. So the policy this verb states is `coldThresholdPolicy`:
// no context is above it, the switch is paid, and the shim's own `cold` arm
// relays onto `SetModelError.cold` if the shim ever raises it for a reason of
// its own — answered with AnswerColdGate, then set the model again.
func (s *sender) SetModel(ctx context.Context, model string) error {
	response, err := s.client.SetSessionModel(ctx, &shimv1.SetSessionModelRequest{
		Model:               &conversationv1.AgentModel{Name: model},
		ColdThresholdTokens: coldThresholdPolicy,
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "SetSessionModel", Arm: setModelArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// SetPermissionMode switches the session's permission mode.
func (s *sender) SetPermissionMode(ctx context.Context, mode string) error {
	response, err := s.client.SetSessionPermissionMode(ctx, &shimv1.SetSessionPermissionModeRequest{
		PermissionMode: permissionMode(mode),
	})
	if err != nil {
		return err
	}
	if failure := response.GetFailure(); failure != nil {
		return &ShimRefusal{Verb: "SetSessionPermissionMode", Arm: setPermissionModeArm(failure), Detail: failure.GetDetail()}
	}
	return nil
}

// startTurnArm names a StartTurn refusal's arm.
func startTurnArm(failure *shimv1.StartTurnFailure) string {
	switch {
	case failure.GetTurnAlreadyOpen() != nil:
		return "turn_already_open"
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	case failure.GetQueryDead() != nil:
		return "query_dead"
	default:
		return ArmShimUnspecified
	}
}

// setModelArm names a SetSessionModel refusal's arm.
func setModelArm(failure *shimv1.SetSessionModelFailure) string {
	switch {
	case failure.GetCold() != nil:
		return ArmShimCold
	case failure.GetModelNotInCatalog() != nil:
		return ArmShimModelNotInCatalog
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	default:
		return ArmShimUnspecified
	}
}

// setPermissionModeArm names a SetSessionPermissionMode refusal's arm.
func setPermissionModeArm(failure *shimv1.SetSessionPermissionModeFailure) string {
	switch {
	case failure.GetNoSession() != nil:
		return ArmShimNoSession
	case failure.GetVendorRefused() != nil:
		return "vendor_refused"
	default:
		return ArmShimUnspecified
	}
}

// The compile-time assertions that the fleet answers the queue's two source
// functions and that the watcher answers the queue's watcher slice.
var (
	_ promptqueue.ClientFunc  = (*Fleet)(nil).Sender
	_ promptqueue.WatcherFunc = (*Fleet)(nil).Watcher
	_ promptqueue.Sender      = (*sender)(nil)
	_ promptqueue.Watcher     = (sessionwatcher.Watcher)(nil)
)
