package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/merge"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// THE ONE REFUSAL PATH. Every component states its refusal as an ARM NAME —
// workspace.Refusal.Arm, merge.RefusalError.Arm, or a documented sentinel — and
// the contract spells the same condition with the same arm name under every rpc
// that can raise it. So the mapping from a component's refusal onto an rpc's
// `<Rpc>Error` arm is ONE function driven by that name, rather than forty
// hand-written switches that would drift from the protos.
//
// An arm the rpc's error message does not carry is not a bug in the mapping: it
// is a refusal the CONTRACT has no arm for yet, and it answers through
// UnlandedArm with a row in daemon/ERROR-ARMS.md.

// refusal is one component refusal, normalized: which arm the contract owes it,
// why, whether it is an unknown-id refusal, and the values the arm's own fields
// take.
type refusal struct {
	// Arm is the intended `<Rpc>Error` arm name.
	Arm string
	// Reason is the sentence the refusal carries as evidence.
	Reason string
	// NotFound marks an unknown-id refusal, which answers CodeNotFound rather
	// than CodeFailedPrecondition when no arm exists.
	NotFound bool
	// Fields are the arm message's own field values, keyed by proto field name.
	Fields map[string]any
	// Info marks a refusal that is an ORDINARY answer rather than a warning —
	// AdoptWebWorkspace's no_transfer_announced on every non-handover page
	// boot. It is logged at INFO and never as a fault.
	Info bool
}

// asRefusal normalizes a component error into a refusal, reporting false for an
// ordinary failure that must surface as an error rather than an answer.
func (s *server) asRefusal(err error) (refusal, bool) {
	var wsRefusal *workspace.Refusal
	if errors.As(err, &wsRefusal) {
		return s.fill(refusal{
			Arm:      wsRefusal.Arm,
			Reason:   wsRefusal.Reason,
			NotFound: wsRefusal.NotFound,
			// The verb's own arm-field values travel with the refusal: an arm
			// that carries evidence (base_ref_unresolved's `ref`) is set from
			// what the verb stated, never left empty beside a prose sentence.
			Fields: wsRefusal.Fields,
		}), true
	}
	var mergeRefusal *merge.RefusalError
	if errors.As(err, &mergeRefusal) {
		return s.fill(refusal{Arm: mergeRefusal.Arm, Reason: mergeRefusal.Reason}), true
	}
	var confirm *workspace.ConfirmRequired
	if errors.As(err, &confirm) {
		return s.fill(refusal{
			Arm:    "confirm_required",
			Reason: fmt.Sprintf("%d detached agents are live", confirm.LiveAgentCount),
			Fields: map[string]any{"live_agent_count": int64(confirm.LiveAgentCount)},
		}), true
	}
	var shimRefusal *workspace.ShimRefusal
	if errors.As(err, &shimRefusal) {
		return s.fill(refusal{Arm: shimRefusal.Arm, Reason: shimRefusal.Detail}), true
	}

	switch {
	// The prompt handler's own refusals.
	case errors.Is(err, prompthandler.ErrFeedNotInWorkspace):
		return s.fill(refusal{Arm: "feed_not_in_workspace", Reason: err.Error()}), true
	case errors.Is(err, prompthandler.ErrDuplicateSubmission):
		return s.fill(refusal{Arm: "duplicate_submission", Reason: err.Error()}), true

	// The prompt queue's refusals, as promptqueue/api.go maps them.
	case errors.Is(err, promptqueue.ErrMerging):
		return s.fill(refusal{Arm: "merging", Reason: err.Error()}), true
	case errors.Is(err, promptqueue.ErrNoSession):
		return s.fill(refusal{Arm: "no_session", Reason: err.Error()}), true
	case errors.Is(err, promptqueue.ErrNoSuchHold):
		return s.fill(refusal{Arm: "no_such_hold", Reason: err.Error(), NotFound: true}), true
	case errors.Is(err, promptqueue.ErrAlreadyDelivered):
		return s.fill(refusal{Arm: "already_delivered", Reason: err.Error()}), true
	case errors.Is(err, promptqueue.ErrAcceptNotApplicable):
		return s.fill(refusal{Arm: "accept_not_applicable", Reason: err.Error()}), true
	case errors.Is(err, promptqueue.ErrReleaseRefused):
		return s.fill(refusal{Arm: "release_refused", Reason: err.Error()}), true

	// The feed resolver's page-walk refusals.
	case errors.Is(err, feed.ErrNoWalk):
		return s.fill(refusal{Arm: "no_walk_standing", Reason: err.Error()}), true
	case errors.Is(err, feed.ErrUnknownFeed):
		return s.fill(refusal{Arm: "feed_not_in_workspace", Reason: err.Error()}), true

	// The drain controller's one refusal.
	case errors.Is(err, drain.ErrNothingScheduled):
		return s.fill(refusal{Arm: "nothing_scheduled", Reason: err.Error()}), true

	// The rollout controller's three, per ERROR-ARMS "Drain and rollout
	// refusal arms".
	case errors.Is(err, rollout.ErrNoTransferAnnounced):
		return s.fill(refusal{Arm: "no_transfer_announced", Reason: err.Error()}), true
	case errors.Is(err, rollout.ErrNotYetAdopted):
		return s.fill(refusal{Arm: "not_yet_adopted", Reason: err.Error()}), true
	case errors.Is(err, rollout.ErrParticipantNotExpected):
		return s.fill(refusal{Arm: "participant_not_expected", Reason: err.Error()}), true
	}
	return refusal{}, false
}

// fill supplies the arm-field values every refusal shares: the successor
// address a transferring_away arm carries, and the sentence the text- or
// detail-bearing arms carry.
func (s *server) fill(r refusal) refusal {
	// The map is COPIED rather than filled in place: it may be the component's
	// own, and the shared text/detail values below are the transport's
	// business, not the verb's.
	fields := make(map[string]any, len(r.Fields)+3)
	for name, value := range r.Fields {
		fields[name] = value
	}
	r.Fields = fields
	if _, ok := r.Fields["address"]; !ok && r.Arm == workspace.ArmTransferringAway {
		r.Fields["address"] = s.deps.SuccessorAddress()
	}
	if _, ok := r.Fields["text"]; !ok {
		r.Fields["text"] = r.Reason
	}
	if _, ok := r.Fields["detail"]; !ok {
		r.Fields["detail"] = r.Reason
	}
	return r
}

// refuse answers one refusal on resp, or hands back the Connect error for an
// arm the contract does not carry yet. resp is mutated in place; a nil return
// means the refusal is now the response's `error` arm.
func (s *server) refuse(log dlog.Logger, rpc string, resp proto.Message, r refusal) *connect.Error {
	if setResponseError(resp, r.Arm, r.Fields) {
		if r.Info {
			log.Info(rpc, "answered a typed refusal", dlog.Context{"arm": r.Arm, "reason": r.Reason})
		} else {
			log.Debug(rpc, "answered a typed refusal", dlog.Context{"arm": r.Arm, "reason": r.Reason})
		}
		return nil
	}
	return UnlandedArm(log, rpc, r.Arm, r.Reason, r.NotFound)
}

// setResponseError populates resp's `result.error` arm with the named
// `<Rpc>Error` arm, reporting false when this rpc's error message carries no
// such arm.
func setResponseError(resp proto.Message, arm string, fields map[string]any) bool {
	if resp == nil || arm == "" {
		return false
	}
	message := resp.ProtoReflect()
	errField := message.Descriptor().Fields().ByName("error")
	if errField == nil || errField.Kind() != protoreflect.MessageKind {
		return false
	}
	errMessage := message.NewField(errField).Message()
	if !setArm(errMessage, arm, fields) {
		return false
	}
	message.Set(errField, protoreflect.ValueOfMessage(errMessage))
	return true
}

// setArm populates one `<Rpc>Error` arm by name, filling whichever of the arm
// message's own fields the refusal supplied.
func setArm(errMessage protoreflect.Message, arm string, fields map[string]any) bool {
	armField := errMessage.Descriptor().Fields().ByName(protoreflect.Name(arm))
	if armField == nil || armField.Kind() != protoreflect.MessageKind || armField.ContainingOneof() == nil {
		return false
	}
	armMessage := errMessage.NewField(armField).Message()
	for i := 0; i < armMessage.Descriptor().Fields().Len(); i++ {
		field := armMessage.Descriptor().Fields().Get(i)
		value, ok := fields[string(field.Name())]
		if !ok {
			continue
		}
		switch field.Kind() {
		case protoreflect.StringKind:
			if text, ok := value.(string); ok {
				armMessage.Set(field, protoreflect.ValueOfString(text))
			}
		case protoreflect.Int64Kind:
			if number, ok := value.(int64); ok {
				armMessage.Set(field, protoreflect.ValueOfInt64(number))
			}
		case protoreflect.Int32Kind:
			if number, ok := value.(int32); ok {
				armMessage.Set(field, protoreflect.ValueOfInt32(number))
			}
		}
	}
	errMessage.Set(armField, protoreflect.ValueOfMessage(armMessage))
	return true
}

// resolved is a per-workspace rpc's resolved subject: the registry record and
// the workspace's own log sink.
type resolved struct {
	Record wsm.Workspace
	Log    dlog.Logger
}

// resolveRef keys a client's echoed WorkspaceRef on `id`, REFUSES a ref whose
// `dir` disagrees with the registry, and then refuses a workspace this daemon
// does not serve. Every per-workspace rpc goes through here BEFORE it
// delegates: the verbs repeat the check for their own callers, but the rpcs
// that do not route through the verbs would otherwise have none.
func (s *server) resolveRef(ctx context.Context, rpc string, ref *workspacev1.WorkspaceRef) (resolved, *refusal, error) {
	return s.resolveRefLogging(ctx, rpc, ref, true)
}

// resolveStreamRef is resolveRef for a STANDING STREAM. A refused stream open
// is transport-closed by ruling, not an unlanded arm, so the standing refusals
// are NOT warned here: the caller records them at INFO through TransportClosed.
func (s *server) resolveStreamRef(ctx context.Context, rpc string, ref *workspacev1.WorkspaceRef) (resolved, *refusal, error) {
	return s.resolveRefLogging(ctx, rpc, ref, false)
}

// resolveRefLogging is the shared body; warnStanding selects whether a standing
// refusal is warned as an unlanded arm.
func (s *server) resolveRefLogging(
	ctx context.Context,
	rpc string,
	ref *workspacev1.WorkspaceRef,
	warnStanding bool,
) (resolved, *refusal, error) {
	id := ids.WorkspaceID(ref.GetId())
	record, err := s.deps.DB.Workspace(ctx, id)
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			r := s.fill(refusal{
				Arm:      workspace.ArmUnknownWorkspace,
				Reason:   fmt.Sprintf("no workspace %q is registered", id),
				NotFound: true,
			})
			return resolved{}, &r, nil
		}
		s.log.Error(rpc, "could not read the workspace record",
			dlog.Context{"workspace": string(id), "cause": err.Error()})
		return resolved{}, nil, fmt.Errorf("%s: workspace %q: %w", rpc, id, err)
	}
	if dir := ref.GetDir(); dir != "" && dir != record.Dir {
		r := s.fill(refusal{
			Arm: workspace.ArmWorkspaceRefMismatch,
			Reason: fmt.Sprintf("the echoed ref names dir %q; the registry holds %q",
				dir, record.Dir),
			Fields: map[string]any{"registry_dir": record.Dir},
		})
		return resolved{}, &r, nil
	}
	log, err := s.deps.Log.Workspace(record.Dir)
	if err != nil {
		s.log.Error(rpc, "could not resolve the workspace log sink",
			dlog.Context{"dir": record.Dir, "cause": err.Error()})
		return resolved{}, nil, fmt.Errorf("%s: resolve log sink %q: %w", rpc, record.Dir, err)
	}
	log = log.With(dlog.Context{"workspace": string(id)})

	standing, err := s.deps.Ownership.Standing(ctx, id)
	if err != nil {
		log.Error(rpc, "could not determine the serving standing", dlog.Context{"cause": err.Error()})
		return resolved{}, nil, fmt.Errorf("%s: serving standing %q: %w", rpc, id, err)
	}
	switch standing {
	case workspace.StandingOwned:
		return resolved{Record: record, Log: log}, nil, nil
	case workspace.StandingTransferringAway:
		r := s.fill(refusal{
			Arm:    workspace.ArmTransferringAway,
			Reason: fmt.Sprintf("workspace %q has been handed to a successor daemon", id),
		})
		if warnStanding {
			log.Warn(opUnlandedArm+".standing", "refused a workspace this daemon no longer serves",
				dlog.Context{"rpc": rpc, "arm": r.Arm})
		}
		return resolved{}, &r, nil
	case workspace.StandingNotYetAdopted:
		r := s.fill(refusal{
			Arm:    workspace.ArmNotYetAdopted,
			Reason: fmt.Sprintf("workspace %q has not been adopted by this daemon yet", id),
		})
		if warnStanding {
			log.Warn(opUnlandedArm+".standing", "refused a workspace this daemon has not adopted",
				dlog.Context{"rpc": rpc, "arm": r.Arm})
		}
		return resolved{}, &r, nil
	default:
		return resolved{}, nil, fmt.Errorf("%s: workspace %q: unknown serving standing %d", rpc, id, standing)
	}
}

// fail renders an ordinary (non-refusal) failure as a Connect internal error,
// logged at ERROR so nothing is swallowed.
func fail(log dlog.Logger, rpc string, err error) *connect.Error {
	log.Error(rpc, "the rpc failed", dlog.Context{"cause": err.Error()})
	return connect.NewError(connect.CodeInternal, err)
}

// answer renders a handler's finished response. A refusal is already ENCODED
// into resp, so a nil Connect error means the response — refusal or success —
// is what the caller gets; a non-nil one is the unlanded-arm error, which
// travels as a Connect error instead. It exists so no handler writes the
// typed-nil trap of returning a nil *connect.Error as a non-nil error.
func answer[R any](resp *R, cerr *connect.Error) (*connect.Response[R], error) {
	if cerr != nil {
		return nil, cerr
	}
	return connect.NewResponse(resp), nil
}

// workspaceLog answers a workspace's own log sink. A workspace-bound record
// that cannot resolve its sink is an invariant violation, never a global write.
func (s *server) workspaceLog(ctx context.Context, rpc string, ws ids.WorkspaceID) (dlog.Logger, error) {
	record, err := s.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("%s: workspace %q: %w", rpc, ws, err)
	}
	log, err := s.deps.Log.Workspace(record.Dir)
	if err != nil {
		return nil, fmt.Errorf("%s: resolve log sink %q: %w", rpc, record.Dir, err)
	}
	return log.With(dlog.Context{"workspace": string(ws)}), nil
}

// answerRefusal renders a verb's error onto resp, RENAMING the arm where one
// rpc spells a shared condition differently from the verb that raised it. The
// rename table is per-rpc and explicit, so no arm is silently reinterpreted.
func (s *server) answerRefusal(
	log dlog.Logger,
	rpc string,
	resp proto.Message,
	err error,
	rename map[string]string,
) *connect.Error {
	refused, ok := s.asRefusal(err)
	if !ok {
		return fail(log, rpc, err)
	}
	if to, renamed := rename[refused.Arm]; renamed {
		refused.Arm = to
	}
	return s.refuse(log, rpc, resp, refused)
}
