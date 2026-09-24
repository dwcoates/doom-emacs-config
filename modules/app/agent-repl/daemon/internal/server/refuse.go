package server

import (
	"context"
	"errors"
	"fmt"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	conversationv1 "agentrepl/proto/conversation/v1"
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
	// Op is the operation the record is filed under, empty to file it under
	// the rpc's own name (which is what every refusal did before SubmitPrompt
	// needed its refusals findable beside the prompt handler's own records).
	Op string
}

// asRefusal normalizes a component error into a refusal, reporting false for an
// ordinary failure that must surface as an error rather than an answer.
func (s *server) asRefusal(err error) (refusal, bool) {
	if wsRefusal, ok := workspace.AsRefusal(err); ok {
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
	if mergeRefusal, ok := merge.Refused(err); ok {
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
	// THE STANDING EDIT'S OWN TURN rides the being_edited arm, so the refusal
	// names the prompt that is being edited rather than only saying one is.
	var beingEdited *promptqueue.BeingEditedError
	if errors.As(err, &beingEdited) {
		return s.fill(refusal{
			Arm:    "being_edited",
			Reason: err.Error(),
			Fields: map[string]any{"editing_turn": &conversationv1.TurnId{Value: string(beingEdited.Turn)}},
		}), true
	}
	// THE COLD GATE'S OWN SENTENCE, not this package's error string: the arm's
	// `detail` is what a client shows the user, and it must read the way the
	// gate card and the footer's cold-gate line read.
	var coldGate *promptqueue.ColdGateRefusal
	if errors.As(err, &coldGate) {
		return s.fill(refusal{Arm: "cold_gate", Reason: coldGate.Detail}), true
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
	case errors.Is(err, promptqueue.ErrNotHeld):
		return s.fill(refusal{Arm: "not_held", Reason: err.Error()}), true
	case errors.Is(err, promptqueue.ErrNotEditing):
		return s.fill(refusal{Arm: "not_editing", Reason: err.Error()}), true
	case errors.Is(err, promptqueue.ErrNoEditor):
		return s.fill(refusal{Arm: "no_editor", Reason: err.Error()}), true

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
	case errors.Is(err, rollout.ErrJoining):
		return s.fill(refusal{Arm: "joining", Reason: err.Error()}), true
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
		op := rpc
		if r.Op != "" {
			op = r.Op
		}
		if r.Info {
			log.Info(op, "answered a typed refusal", dlog.Context{"arm": r.Arm, "reason": r.Reason})
		} else {
			log.Debug(op, "answered a typed refusal", dlog.Context{"arm": r.Arm, "reason": r.Reason})
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

// nestedArm is a refusal field value naming an arm of a oneof INSIDE the
// `<Rpc>Error` arm message — SubmitPromptBubbleRefused's `kind`. The nested
// arms are empty messages, so the arm NAME is the whole value; the refusal
// keys it by the oneof's own name.
type nestedArm string

// setArm populates one `<Rpc>Error` arm by name, filling whichever of the arm
// message's own fields the refusal supplied.
func setArm(errMessage protoreflect.Message, arm string, fields map[string]any) bool {
	armField := errMessage.Descriptor().Fields().ByName(protoreflect.Name(arm))
	if armField == nil || armField.Kind() != protoreflect.MessageKind || armField.ContainingOneof() == nil {
		return false
	}
	armMessage := errMessage.NewField(armField).Message()
	if !setNestedArms(armMessage, fields) {
		return false
	}
	for i := 0; i < armMessage.Descriptor().Fields().Len(); i++ {
		field := armMessage.Descriptor().Fields().Get(i)
		value, ok := fields[string(field.Name())]
		if !ok {
			continue
		}
		// A REPEATED FIELD IS SET BEFORE THE SCALAR SWITCH, because a repeated
		// string's Kind() is StringKind too: falling through would try a
		// `value.(string)` on a slice, fail it, and leave the arm's evidence
		// EMPTY beside a prose sentence that says it has some.
		// ForgetWorkspaceHasChildren.children is the first such arm.
		if field.IsList() {
			setListField(armMessage, field, value)
			continue
		}
		// A MESSAGE-KIND FIELD IS SET BEFORE THE SCALAR SWITCH TOO, for the
		// same reason a repeated one is: an arm whose evidence is itself a
		// message — BindWorkspaceSessionTranscriptHeld's `workspace` — would
		// otherwise fall through every scalar case and leave the arm naming
		// nothing beside a sentence that names a workspace.
		if field.Kind() == protoreflect.MessageKind {
			if message, ok := value.(proto.Message); ok && message != nil {
				armMessage.Set(field, protoreflect.ValueOfMessage(message.ProtoReflect()))
			}
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
		case protoreflect.Uint32Kind:
			if number, ok := value.(uint32); ok {
				armMessage.Set(field, protoreflect.ValueOfUint32(number))
			}
		case protoreflect.BoolKind:
			if flag, ok := value.(bool); ok {
				armMessage.Set(field, protoreflect.ValueOfBool(flag))
			}
		}
	}
	errMessage.Set(armField, protoreflect.ValueOfMessage(armMessage))
	return true
}

// setListField fills one REPEATED arm field from the refusal's value. Only the
// string element kind is supported, which is every repeated arm field the
// contract spells today; an element kind or value type it cannot convert
// leaves the field unset rather than writing a wrong value, and the refusal's
// prose still states the evidence.
func setListField(armMessage protoreflect.Message, field protoreflect.FieldDescriptor, value any) {
	if field.Kind() != protoreflect.StringKind {
		return
	}
	items, ok := value.([]string)
	if !ok {
		return
	}
	list := armMessage.NewField(field).List()
	for _, item := range items {
		list.Append(protoreflect.ValueOfString(item))
	}
	armMessage.Set(field, protoreflect.ValueOfList(list))
}

// setNestedArms selects the arm of every NESTED oneof the refusal named,
// reporting false when it named one this message does not carry — an arm the
// contract does not spell is answered as an unlanded arm, never silently
// dropped beside a bare sentence.
func setNestedArms(armMessage protoreflect.Message, fields map[string]any) bool {
	oneofs := armMessage.Descriptor().Oneofs()
	for i := 0; i < oneofs.Len(); i++ {
		oneof := oneofs.Get(i)
		value, ok := fields[string(oneof.Name())]
		if !ok {
			continue
		}
		name, ok := value.(nestedArm)
		if !ok {
			return false
		}
		nested := oneof.Fields().ByName(protoreflect.Name(name))
		if nested == nil || nested.Kind() != protoreflect.MessageKind {
			return false
		}
		armMessage.Set(nested, protoreflect.ValueOfMessage(armMessage.NewField(nested).Message()))
	}
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

	// THE SERVING STANDING IS DECIDED FIRST, BEFORE ANY STATE IS TOUCHED. A
	// workspace this daemon transferred away is answered `transferring_away`
	// no matter what shape the outgoing daemon's shared state is in: the
	// handover's exit closes the state client while requests for the
	// transferred workspace are still arriving, and reading the registry
	// first turned the standing answer the contract owes into
	// "sql: database is closed". The standing is held in memory by the
	// rollout controller, so it costs no read.
	standing, err := s.deps.Ownership.Standing(ctx, id)
	if err != nil {
		if endedOnCancel(err) {
			s.log.Info(rpc, "the serving standing was not determined; the request's context was cancelled",
				dlog.Context{"stream": rpc, "workspace": string(id), "cause": err.Error()})
		} else {
			s.log.Error(rpc, "could not determine the serving standing",
				dlog.Context{"workspace": string(id), "cause": err.Error()})
		}
		return resolved{}, nil, fmt.Errorf("%s: serving standing %q: %w", rpc, id, err)
	}
	switch standing {
	case workspace.StandingOwned:
	case workspace.StandingTransferringAway:
		r := s.fill(refusal{
			Arm:    workspace.ArmTransferringAway,
			Reason: fmt.Sprintf("workspace %q has been handed to a successor daemon", id),
		})
		if warnStanding {
			s.log.Warn(opUnlandedArm+".standing", "refused a workspace this daemon no longer serves",
				dlog.Context{"rpc": rpc, "arm": r.Arm, "workspace": string(id)})
		}
		return resolved{}, &r, nil
	case workspace.StandingNotYetAdopted:
		r := s.fill(refusal{
			Arm:    workspace.ArmNotYetAdopted,
			Reason: fmt.Sprintf("workspace %q has not been adopted by this daemon yet", id),
		})
		if warnStanding {
			s.log.Warn(opUnlandedArm+".standing", "refused a workspace this daemon has not adopted",
				dlog.Context{"rpc": rpc, "arm": r.Arm, "workspace": string(id)})
		}
		return resolved{}, &r, nil
	default:
		return resolved{}, nil, fmt.Errorf("%s: workspace %q: unknown serving standing %d", rpc, id, standing)
	}
	return s.resolveRegistered(ctx, rpc, ref)
}

// resolveRegistered resolves a workspace ref against the registry ALONE,
// without the serving-standing gate resolveRefLogging puts in front of it.
//
// It exists for the one rpc whose answer must not depend on which daemon
// serves the workspace: ClientLog (see subjectForClientLog). Every other rpc
// is INTAKE and goes through the standing gate first.
func (s *server) resolveRegistered(ctx context.Context, rpc string, ref *workspacev1.WorkspaceRef) (resolved, *refusal, error) {
	id := ids.WorkspaceID(ref.GetId())

	// THE SHARED STATE IS NOT READ ONCE IT IS BEING TORN DOWN. Close() ends
	// the server's lifetime BEFORE the state client is closed underneath it,
	// and an h2c connection the client already holds can still carry a new
	// rpc through the shutdown grace. Such a request is the daemon going
	// away, and it says so as UNAVAILABLE rather than surfacing whatever the
	// half-closed state client happens to return.
	//
	// ORDERED, NOT CHECKED. A lifetime check followed by the read left a
	// window in which Close and the state client's close both ran between the
	// two, and the read met "sql: database is closed" at ERROR — the webapp
	// layer's roster area lost a run to a ClientLog landing in it during the
	// drain's exit. readRegistry holds Close off for the read's duration.
	var record wsm.Workspace
	ended, err := s.readRegistry(func() error {
		var readErr error
		record, readErr = s.deps.DB.Workspace(ctx, id)
		return readErr
	})
	if ended {
		s.log.Info(rpc, "refused a request that arrived while the daemon was shutting down",
			dlog.Context{"workspace": string(id)})
		return resolved{}, nil, connect.NewError(connect.CodeUnavailable,
			fmt.Errorf("%s: workspace %q: %w", rpc, id, errServingEnded))
	}
	if err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			r := s.fill(refusal{
				Arm:      workspace.ArmUnknownWorkspace,
				Reason:   fmt.Sprintf("no workspace %q is registered", id),
				NotFound: true,
			})
			return resolved{}, &r, nil
		}
		if endedOnCancel(err) {
			s.log.Info(rpc, "the workspace record was not read; the request's context was cancelled",
				dlog.Context{"stream": rpc, "workspace": string(id), "cause": err.Error()})
		} else {
			s.log.Error(rpc, "could not read the workspace record",
				dlog.Context{"workspace": string(id), "cause": err.Error()})
		}
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
	// A REGISTERED WORKSPACE ALWAYS RESOLVES TO A SINK. When its directory
	// cannot host one — a scratch path, a worktree that has been deleted — the
	// records go centrally with the workspace named on them, and the rpc still
	// reaches its handler: refusing here turned every per-workspace verb on
	// such a workspace into an internal error raised by logging, in place of
	// the handler's own typed answer.
	log := s.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(id)})
	return resolved{Record: record, Log: log}, nil, nil
}

// errServingEnded is the shutdown refusal a resolution answers once the
// surface's lifetime has ended. It is recorded at INFO where it is decided,
// and failResolution recognises it so it is not restated as a failure.
var errServingEnded = errors.New("the daemon is shutting down")

// readRegistry runs one registry read of a request's resolution, unless the
// surface has closed. `ended` reports that Close has begun, in which case the
// read was never made: the state client may already be closed beneath it.
//
// The read holds the registry gate shared, so Close -- which takes it
// exclusively -- cannot complete while a read is in flight, and the daemon
// closes the state client only after Close has returned.
func (s *server) readRegistry(read func() error) (ended bool, err error) {
	s.registry.RLock()
	defer s.registry.RUnlock()
	if s.registryClosed {
		return true, nil
	}
	return false, read()
}

// failResolution renders a failed workspace RESOLUTION. The two endings that
// are not failures -- the daemon shutting down under the request, and the
// request's own context ending -- were already recorded at INFO where they
// were decided, so they keep their Connect code and add no ERROR. Everything
// else is a failure and goes through fail.
func failResolution(log dlog.Logger, rpc string, err error) *connect.Error {
	if errors.Is(err, errServingEnded) || endedOnCancel(err) {
		var coded *connect.Error
		if errors.As(err, &coded) {
			return coded
		}
		return connect.NewError(connect.CodeCanceled, err)
	}
	return fail(log, rpc, err)
}

// fail renders an ordinary (non-refusal) failure as a Connect internal error,
// logged at ERROR so nothing is swallowed.
func fail(log dlog.Logger, rpc string, err error) *connect.Error {
	// A failure that already NAMED its Connect code keeps it: the shutdown
	// refusal is UNAVAILABLE, and restamping it internal would tell the client
	// the daemon broke rather than that it went away.
	var coded *connect.Error
	if errors.As(err, &coded) {
		log.Error(rpc, "the rpc failed", dlog.Context{"cause": err.Error(), "code": coded.Code().String()})
		return coded
	}
	log.Error(rpc, "the rpc failed", dlog.Context{"cause": err.Error()})
	return connect.NewError(connect.CodeInternal, err)
}

// endedOnCancel reports whether err is the CLIENT'S OWN request context ending
// — a cancellation or a deadline — rather than something that broke. Every
// read a standing stream performs runs under that context, and it is cancelled
// on the client leaving and on the daemon's orderly exit alike, so the two are
// the same fact and neither is a fault.
func endedOnCancel(err error) bool {
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}

// endStream renders a STANDING STREAM's failure. A cancelled request context is
// the client leaving: the stream ends quietly, recorded once at INFO naming the
// stream, and the handler returns no error. Every other failure keeps fail's
// ERROR record and its internal Connect error.
//
// It returns `error` rather than *connect.Error precisely so a quiet ending is
// an untyped nil, never the typed-nil trap a *connect.Error return would carry.
func endStream(log dlog.Logger, rpc string, err error) error {
	if endedOnCancel(err) {
		log.Info(rpc, "the standing stream ended when its client's context was cancelled",
			dlog.Context{"stream": rpc, "cause": err.Error()})
		return nil
	}
	return fail(log, rpc, err)
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

// workspaceLog answers a workspace's own log sink.
//
// RESOLVING A NAMED WORKSPACE'S SINK IS A TOTAL FUNCTION. Only the WORKSPACE
// READ can fail here: a workspace the state store will not name has no records
// to attribute. Once it is named, a directory that cannot host a durable sink
// -- a scratch path, a deleted worktree, a directory that does not exist yet --
// is an ORDINARY outcome, and the records go to the central sink carrying
// `unroutable_workspace' so the line still says which workspace it is about.
// Serving a workspace must never fail, or withhold what it was going to
// publish, over WHERE its narration is written.
func (s *server) workspaceLog(ctx context.Context, rpc string, ws ids.WorkspaceID) (dlog.Logger, error) {
	record, err := s.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("%s: workspace %q: %w", rpc, ws, err)
	}
	return s.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{"workspace": string(ws)}), nil
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
