package workspace

import (
	"context"
	"errors"
	"fmt"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

const opStandingClose = "daemon.workspace.standing_close"

// CloseWaits is everything that blocks a close: the quietness blockers
// (blockedClose) and, for a user's or an agent's close, the family rule
// (wsm.HasOpenChildrenError). Each clears on its own, which is what lets a
// close stand (agentrepl.v1.CloseWorkspaceRequest.stand).
type CloseWaits struct {
	// Blocked is the quietness evidence, nil while the workspace is quiet.
	Blocked *footer.CloseBlocked
	// Family is the open children and workspaces landing into it, nil while
	// it has none.
	Family *wsm.HasOpenChildrenError
}

// None reports whether nothing blocks the close.
func (w CloseWaits) None() bool { return w.Blocked == nil && w.Family == nil }

// standing is the footer's standing close state over W: the quietness
// evidence, marked standing, with one composed sentence over every blocker.
// Nil when nothing blocks.
func (w CloseWaits) standing() *footer.CloseBlocked {
	if w.None() {
		return nil
	}
	out := footer.CloseBlocked{Reason: "open_children", Standing: true}
	var parts []string
	if w.Blocked != nil {
		out = *w.Blocked
		out.Standing = true
		parts = append(parts, w.Blocked.Detail)
	}
	if w.Family != nil {
		if len(w.Family.Children) > 0 {
			parts = append(parts, "its open children have not closed: "+familyNames(w.Family.Children))
		}
		if len(w.Family.LandingInto) > 0 {
			parts = append(parts, "the workspaces landing into it have not closed: "+familyNames(w.Family.LandingInto))
		}
	}
	out.Detail = "waiting to close: " + strings.Join(parts, "; ")
	return &out
}

// Proto is W on the wire (agentrepl.v1.WorkspaceCloseWaits): the quietness
// evidence with its composed sentence, and the family refusal's members.
func (w CloseWaits) Proto() *agentreplv1.WorkspaceCloseWaits {
	out := &agentreplv1.WorkspaceCloseWaits{}
	if w.Blocked != nil {
		out.Blocked = closeBlockedProto(w.Blocked)
	}
	if f := w.Family; f != nil {
		out.OpenChildren = &agentreplv1.WorkspaceHasOpenChildren{
			Parent: f.Parent.Ref(), Children: familyRefs(f.Children), LandingInto: familyRefs(f.LandingInto),
		}
	}
	return out
}

// closeBlockedProto is BLOCKED on the wire (agentrepl.v1.CloseWorkspaceBlocked):
// THE ONE MAPPING of the quietness evidence, which the `blocked` refusal
// (refuseBlockedClose) and a standing close's waits (CloseWaits.Proto) both
// carry, so the two can never say it differently.
func closeBlockedProto(b *footer.CloseBlocked) *agentreplv1.CloseWorkspaceBlocked {
	return &agentreplv1.CloseWorkspaceBlocked{
		TurnInFlight: b.TurnInFlight, LiveWork: b.LiveWork, HeldPrompts: b.HeldPrompts,
		MergeQueued: b.MergeQueued, Summary: b.Detail,
	}
}

// messageFields is every scalar field of M keyed by its proto name, its
// default included: a wire message as a Refusal's Fields carry it, so an arm
// built from a message states every field the message does. A message, list
// or map field is a caller's defect, since Fields carries scalars alone.
func messageFields(m proto.Message) map[string]any {
	r := m.ProtoReflect()
	fields := r.Descriptor().Fields()
	out := make(map[string]any, fields.Len())
	for i := 0; i < fields.Len(); i++ {
		fd := fields.Get(i)
		if fd.IsList() || fd.IsMap() || fd.Message() != nil {
			panic(fmt.Sprintf("workspace: %s.%s is no scalar field, which a refusal's fields cannot carry", r.Descriptor().FullName(), fd.Name()))
		}
		out[string(fd.Name())] = r.Get(fd).Interface()
	}
	return out
}

// familyRefs is MEMBERS as workspace refs, in their order.
func familyRefs(members []wsm.FamilyRef) []*workspacev1.WorkspaceRef {
	refs := make([]*workspacev1.WorkspaceRef, len(members))
	for i, member := range members {
		refs[i] = member.Ref()
	}
	return refs
}

// familyNames is MEMBERS' names, comma-separated, in their order.
func familyNames(members []wsm.FamilyRef) string {
	names := make([]string, len(members))
	for i, member := range members {
		names[i] = member.Name
	}
	return strings.Join(names, ", ")
}

// closeWaits judges every rule a close of RECORD by BY applies, answering
// what blocks it, or a read that failed (recorded at ERROR). OWNTURN discounts
// the turn in flight (blockedClose). The family rule is read only for a
// user's or an agent's close of an open row, as wsm.MarkClosed applies it.
func (v *verbs) closeWaits(ctx context.Context, log dlog.Logger, record wsm.Workspace, by wsm.CloseCause, ownTurn bool) (CloseWaits, error) {
	blocked, err := v.blockedClose(ctx, record.ID, ownTurn)
	if err != nil {
		log.Error(opClose, "could not judge whether the workspace is quiet", dlog.Context{"cause": err.Error()})
		return CloseWaits{}, fmt.Errorf("close %q: %w", record.ID, err)
	}
	waits := CloseWaits{Blocked: blocked}
	if record.Closed || !by.IsGesture() {
		return waits, nil
	}
	err = v.deps.DB.RequireNoOpenChildren(ctx, record.ID)
	var family *wsm.HasOpenChildrenError
	switch {
	case err == nil:
	case errors.As(err, &family):
		waits.Family = family
	default:
		log.Error(opClose, "could not read whether the workspace has open children", dlog.Context{"cause": err.Error()})
		return CloseWaits{}, fmt.Errorf("close %q: read its open children: %w", record.ID, err)
	}
	return waits, nil
}

// CloseStanding is a close that STANDS when something that clears on its own
// blocks it (agentrepl.v1.CloseWorkspaceRequest.stand): the running turn is
// interrupted first, as every close's is, then a quiet workspace with no open
// children closes now (answering nil), and any other is registered as the
// workspace's standing close request by BY, answering what it waits on.
func (v *verbs) CloseStanding(ctx context.Context, ws ids.WorkspaceID, by wsm.CloseCause) (*CloseWaits, error) {
	record, log, err := v.owned(ctx, "CloseWorkspace", ws)
	if err != nil {
		return nil, err
	}
	interrupted, err := v.interruptBeforeLeaving(ctx, log, ws, opClose, "close")
	if err != nil {
		return nil, err
	}
	waits, err := v.closeWaits(ctx, log, record, by, interrupted)
	if err != nil {
		return nil, err
	}
	if waits.None() {
		return nil, v.Close(ctx, ws, by)
	}
	if err := v.standClose(ctx, log, ws, by, waits); err != nil {
		return nil, err
	}
	return &waits, nil
}

// standClose registers WS's standing close request by BY, stands what it
// waits on in the footer, and reports the workspace to the standing closes,
// so a blocker that cleared between the judgment and the write is judged
// again rather than missed.
func (v *verbs) standClose(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, by wsm.CloseCause, waits CloseWaits) error {
	if err := v.deps.DB.RequestClose(ctx, ws, wsm.CloseRequest{At: v.now(), By: by}); err != nil {
		log.Error(opStandingClose, "could not register the standing close request", dlog.Context{"by": string(by), "cause": err.Error()})
		return fmt.Errorf("close %q: register the standing close request: %w", ws, err)
	}
	v.deps.Footer.SetClosing(ws, waits.standing())
	log.Info(opStandingClose, "the close stands; the daemon closes the workspace once nothing blocks it", dlog.Context{
		"by": string(by), "waits_on": waitsOn(waits)})
	v.deps.StandingCloses.Reconsider(ws)
	return nil
}

// waitsOn is W's composed sentence for a record, "nothing" when it is empty.
func waitsOn(w CloseWaits) string {
	if s := w.standing(); s != nil {
		return s.Detail
	}
	return "nothing"
}

// ReconsiderCloseRequest judges WS's standing close request again: a closed
// workspace or one with no request is left alone; one something still blocks
// keeps waiting, its footer state brought up to date; a quiet one with no open
// children is closed through the ordinary close, as the request's asker, with
// no turn interrupted. A workspace this daemon does not serve (handed away,
// not yet adopted) is left to the daemon that does, whose own edges judge it.
func (v *verbs) ReconsiderCloseRequest(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.owned(ctx, opStandingClose, ws)
	if err != nil {
		if _, refused := AsRefusal(err); refused {
			return nil
		}
		return err
	}
	if record.Closed || record.CloseRequest == nil {
		return nil
	}
	by := record.CloseRequest.By
	waits, err := v.closeWaits(ctx, log, record, by, false)
	if err != nil {
		return err
	}
	if !waits.None() {
		v.deps.Footer.SetClosing(ws, waits.standing())
		log.Info(opStandingClose, "the standing close still waits", dlog.Context{"waits_on": waitsOn(waits)})
		return nil
	}
	err = v.closeQuietly(ctx, ws, by)
	if !isCloseRefusal(err) {
		if err == nil {
			log.Info(opStandingClose, "nothing blocks the standing close any more; the workspace is closed", dlog.Context{"by": string(by)})
		}
		return err
	}
	// SOMETHING BLOCKED AGAIN between the judgment and the close (a turn
	// started, a child opened): the request keeps standing, the footer draws
	// what it waits on again over the one-time refusal the close stood, and
	// the edge that clears that blocker judges it again.
	log.Info(opStandingClose, "the standing close was blocked again as it was carried out; it keeps waiting", dlog.Context{"cause": err.Error()})
	waits, err = v.closeWaits(ctx, log, record, by, false)
	if err != nil {
		return err
	}
	v.deps.Footer.SetClosing(ws, waits.standing())
	return nil
}

// ReconsiderCloseRequests judges every standing close request again
// (ReconsiderCloseRequest), for an edge that may clear a blocker of any
// workspace: a workspace closing or retargeted leaves its ancestors' and its
// landing target's families. Every request is judged; the failures are
// answered together.
func (v *verbs) ReconsiderCloseRequests(ctx context.Context) error {
	standing, err := v.deps.DB.CloseRequests(ctx)
	if err != nil {
		v.deps.Log.Global().Error(opStandingClose, "could not read the standing close requests", dlog.WithCause(nil, err))
		return fmt.Errorf("reconsider the standing close requests: %w", err)
	}
	var errs []error
	for _, ws := range standing {
		errs = append(errs, v.ReconsiderCloseRequest(ctx, ws))
	}
	return errors.Join(errs...)
}

// CloseWithdrawal names what withdrew a standing close request.
type CloseWithdrawal string

// What withdraws a standing close request. A close clears it too, in the
// registry's own MarkClosed.
const (
	// WithdrawnByRequest is WithdrawWorkspaceClose: the workspace's agent, the
	// user, or another workspace's agent asked.
	WithdrawnByRequest CloseWithdrawal = "request"
	// WithdrawnByUserPrompt is a prompt the user sent the workspace: the user
	// works with it again, so it is never closed under them.
	WithdrawnByUserPrompt CloseWithdrawal = "user_prompt"
	// WithdrawnByHeldPromptRelease is the user releasing a held prompt to the
	// workspace's session, which delivers the user's own intent.
	WithdrawnByHeldPromptRelease CloseWithdrawal = "held_prompt_released"
)

// WithdrawCloseRequest withdraws WS's standing close request, as BY says,
// answering whether one stood; the footer's waiting state goes with it.
func (v *verbs) WithdrawCloseRequest(ctx context.Context, ws ids.WorkspaceID, by CloseWithdrawal) (bool, error) {
	_, log, err := v.owned(ctx, "WithdrawWorkspaceClose", ws)
	if err != nil {
		return false, err
	}
	withdrawn, err := v.deps.DB.WithdrawCloseRequest(ctx, ws)
	if err != nil {
		log.Error(opStandingClose, "could not withdraw the standing close request", dlog.Context{"withdrawn_by": string(by), "cause": err.Error()})
		return false, fmt.Errorf("withdraw the close request of %q: %w", ws, err)
	}
	if !withdrawn {
		log.Debug(opStandingClose, "no close request stood to withdraw", dlog.Context{"withdrawn_by": string(by)})
		return false, nil
	}
	v.deps.Footer.SetClosing(ws, nil)
	log.Info(opStandingClose, "withdrew the standing close request; the workspace stays open", dlog.Context{"withdrawn_by": string(by)})
	return true, nil
}
