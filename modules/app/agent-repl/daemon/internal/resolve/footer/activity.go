package footer

import (
	"time"

	"google.golang.org/protobuf/encoding/prototext"
	"google.golang.org/protobuf/reflect/protoreflect"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
)

// THE ACTIVITY CELL is the strip's finest cell, and every status arm ALWAYS
// sets it. Every line belongs to exactly one TIER, defined by what ends it
// (owner rulings, 2026-09-28, 2026-09-30 and 2026-10-01; agent-repl AGENTS.md
// "Footer activity lines are salient, transient, or enduring"):
//
//	salient    ends when the system state it describes stops being true, never
//	           on a timer. Each status arm declares its own salient kinds.
//	transient  ends when a newer transient replaces it or its expiry passes
//	           (transient.go).
//	enduring   never ends (enduring.go).
//
// PRECEDENCE IS BY TIER, and this file applies it: a standing salient line
// outranks every other tier, so the cell carries EITHER the arm's salient line
// OR the unpinned tiers beneath it (`unpinned`): the transient over the
// enduring line. Within the salient tier the kind that explains the standing substatus ranks first, then an escalating
// fault, then a deploy's progress (`update`), then the rest — each arm below
// states its own order, which is footer.proto's.
//
// Every salient line ships the instant it began standing; the client ticks the
// relative age from it.

// stamp renders an activity's standing instant.
func stamp(t time.Time) *frontendv1.FooterStatusActivityAt {
	return &frontendv1.FooterStatusActivityAt{AtMs: epochMs(t)}
}

// salientFault is the standing ESCALATING fault that claims `status`, with the
// instant it began standing, or nil. A non-escalating fault is never salient:
// it was announced as a transient when it opened (faults.go).
func (r *resolver) salientFault(s *wsState, status string) (*frontendv1.FooterStatusActivityFault, time.Time) {
	fault := r.standingFault(s)
	if fault == nil || fault.Status != status {
		return nil, time.Time{}
	}
	return &frontendv1.FooterStatusActivityFault{Kind: fault.Kind, Detail: fault.Detail}, fault.At
}

// idleActivity resolves the cell idle, turn_failed and degraded share: the
// dead-query line first, because it explains a `turn_failed` status; then the
// shared salient lines; then unpinned.
func (r *resolver) idleActivity(s *wsState) *frontendv1.FooterStatusIdleActivity {
	if s.queryDied != nil {
		return &frontendv1.FooterStatusIdleActivity{Tier: &frontendv1.FooterStatusIdleActivity_Salient{
			Salient: &frontendv1.FooterStatusIdleSalient{
				At: stamp(s.queryDied.at),
				Kind: &frontendv1.FooterStatusIdleSalient_QueryDied{
					QueryDied: &frontendv1.FooterStatusActivityQueryDied{Text: s.queryDied.text}},
			}}}
	}
	if line, ok := r.sharedSalient(s); ok {
		return &frontendv1.FooterStatusIdleActivity{Tier: &frontendv1.FooterStatusIdleActivity_Salient{
			Salient: fillShared(&frontendv1.FooterStatusIdleSalient{}, line)}}
	}
	return &frontendv1.FooterStatusIdleActivity{Tier: &frontendv1.FooterStatusIdleActivity_Unpinned{Unpinned: r.unpinned(s)}}
}

// workingActivity resolves the cell while a turn runs: a compaction first,
// because it explains the `compacting` step; then a retry; then the shared
// salient lines; then unpinned, which is most of a turn.
func (r *resolver) workingActivity(s *wsState) *frontendv1.FooterStatusWorkingActivity {
	salient := func(at time.Time) *frontendv1.FooterStatusWorkingSalient {
		return &frontendv1.FooterStatusWorkingSalient{At: stamp(at)}
	}
	var line *frontendv1.FooterStatusWorkingSalient
	switch shared, ok := r.sharedSalient(s); {
	case s.compaction != nil:
		line = salient(s.compaction.at)
		line.Kind = &frontendv1.FooterStatusWorkingSalient_Compaction{
			Compaction: &frontendv1.FooterStatusActivityCompaction{Text: s.compaction.text}}
	case ok:
		line = fillShared(&frontendv1.FooterStatusWorkingSalient{}, shared)
	default:
		return &frontendv1.FooterStatusWorkingActivity{Tier: &frontendv1.FooterStatusWorkingActivity_Unpinned{Unpinned: r.unpinned(s)}}
	}
	return &frontendv1.FooterStatusWorkingActivity{Tier: &frontendv1.FooterStatusWorkingActivity_Salient{Salient: line}}
}

// interruptedActivity resolves the cell while interrupted: a stopped turn has
// no line of its own, so only the shared salient lines can stand.
func (r *resolver) interruptedActivity(s *wsState) *frontendv1.FooterStatusInterruptedActivity {
	if line, ok := r.sharedSalient(s); ok {
		return &frontendv1.FooterStatusInterruptedActivity{Tier: &frontendv1.FooterStatusInterruptedActivity_Salient{
			Salient: fillShared(&frontendv1.FooterStatusInterruptedSalient{}, line)}}
	}
	return &frontendv1.FooterStatusInterruptedActivity{Tier: &frontendv1.FooterStatusInterruptedActivity_Unpinned{Unpinned: r.unpinned(s)}}
}

// mergingActivity resolves the cell merging, merge_failed and merged share:
// the merge step's own line first, because it explains the step; then the
// shared salient lines; then unpinned. The step's line is the orchestrator's,
// and it is cleared when its step ends.
func (r *resolver) mergingActivity(s *wsState) *frontendv1.FooterStatusMergingActivity {
	if s.merge.Line != nil {
		return &frontendv1.FooterStatusMergingActivity{Tier: &frontendv1.FooterStatusMergingActivity_Salient{
			Salient: &frontendv1.FooterStatusMergingSalient{
				At:   stamp(s.merge.LineAt),
				Kind: &frontendv1.FooterStatusMergingSalient_MergeStep{MergeStep: s.merge.Line},
			}}}
	}
	if line, ok := r.sharedSalient(s); ok {
		return &frontendv1.FooterStatusMergingActivity{Tier: &frontendv1.FooterStatusMergingActivity_Salient{
			Salient: fillShared(&frontendv1.FooterStatusMergingSalient{}, line)}}
	}
	return &frontendv1.FooterStatusMergingActivity{Tier: &frontendv1.FooterStatusMergingActivity_Unpinned{Unpinned: r.unpinned(s)}}
}

// backgroundActivity resolves the cell while detached work runs: the detached
// work's landings are the feed's, so only the shared salient lines can stand.
func (r *resolver) backgroundActivity(s *wsState) *frontendv1.FooterStatusBackgroundActivity {
	if line, ok := r.sharedSalient(s); ok {
		return &frontendv1.FooterStatusBackgroundActivity{Tier: &frontendv1.FooterStatusBackgroundActivity_Salient{
			Salient: fillShared(&frontendv1.FooterStatusBackgroundSalient{}, line)}}
	}
	return &frontendv1.FooterStatusBackgroundActivity{Tier: &frontendv1.FooterStatusBackgroundActivity_Unpinned{Unpinned: r.unpinned(s)}}
}

// blockedActivity resolves the cell while blocked: the kind that explains the
// step first — the auth prompt under `auth` — then an escalating fault that
// claims `blocked`; then the shared salient lines; then unpinned, where the
// enduring usage figures explain a usage-limit block.
func (r *resolver) blockedActivity(s *wsState) *frontendv1.FooterStatusBlockedActivity {
	salient := func(at time.Time) *frontendv1.FooterStatusBlockedSalient {
		return &frontendv1.FooterStatusBlockedSalient{At: stamp(at)}
	}
	fault, faultAt := r.salientFault(s, "blocked")
	shared, sharedOK := r.sharedSalient(s)
	var line *frontendv1.FooterStatusBlockedSalient
	switch {
	case s.blocked != nil && s.blocked.kind == blockedAuth && s.authLine != nil:
		line = salient(s.authLine.at)
		line.Kind = &frontendv1.FooterStatusBlockedSalient_Authenticating{
			Authenticating: &frontendv1.FooterStatusActivityAuthenticating{Line: s.authLine.text}}
	case s.blocked == nil && s.retryBlocks():
		// The `api_retrying` substatus always draws its retry line.
		line = salient(s.retrying.at)
		line.Kind = &frontendv1.FooterStatusBlockedSalient_Retrying{Retrying: s.retrying.line()}
	case fault != nil:
		line = salient(faultAt)
		line.Kind = &frontendv1.FooterStatusBlockedSalient_Fault{Fault: fault}
	case sharedOK:
		line = fillShared(&frontendv1.FooterStatusBlockedSalient{}, shared)
	default:
		return &frontendv1.FooterStatusBlockedActivity{Tier: &frontendv1.FooterStatusBlockedActivity_Unpinned{Unpinned: r.unpinned(s)}}
	}
	return &frontendv1.FooterStatusBlockedActivity{Tier: &frontendv1.FooterStatusBlockedActivity_Salient{Salient: line}}
}

// disconnectedActivity resolves the cell while the link is not serving: the
// bring-up failure first, because it explains the `start_failed` step; then an
// escalating fault that claims `disconnected`; then the shared salient lines;
// then unpinned (a step with no line of its own: starting, a severed link being
// retried with no fault yet).
func (r *resolver) disconnectedActivity(s *wsState, log dlog.Logger) *frontendv1.FooterStatusDisconnectedActivity {
	salient := func(at time.Time) *frontendv1.FooterStatusDisconnectedSalient {
		return &frontendv1.FooterStatusDisconnectedSalient{At: stamp(at)}
	}
	fault, faultAt := r.salientFault(s, "disconnected")
	shared, sharedOK := r.sharedSalient(s)
	var line *frontendv1.FooterStatusDisconnectedSalient
	switch {
	case s.startFailed != nil:
		if !s.startFailed.announced {
			s.startFailed.announced = true
			log.Info("daemon.footer.start_failed_activity",
				"the footer composed the bring-up failure's standing line",
				dlog.Context{
					"workspace_dir":   s.dir,
					"dropped_prompts": s.startFailed.dropped,
					"detail":          s.startFailed.detail,
				})
		}
		line = salient(s.startFailed.at)
		line.Kind = &frontendv1.FooterStatusDisconnectedSalient_StartFailed{
			StartFailed: &frontendv1.FooterStatusActivityStartFailed{
				Detail:         s.startFailed.detail,
				DroppedPrompts: s.startFailed.dropped,
			}}
	case fault != nil:
		line = salient(faultAt)
		line.Kind = &frontendv1.FooterStatusDisconnectedSalient_Fault{Fault: fault}
	case sharedOK:
		line = fillShared(&frontendv1.FooterStatusDisconnectedSalient{}, shared)
	default:
		return &frontendv1.FooterStatusDisconnectedActivity{Tier: &frontendv1.FooterStatusDisconnectedActivity_Unpinned{Unpinned: r.unpinned(s)}}
	}
	return &frontendv1.FooterStatusDisconnectedActivity{Tier: &frontendv1.FooterStatusDisconnectedActivity_Salient{Salient: line}}
}

// closingActivity resolves the cell while closing: the refusal first, because
// it explains the `blocked` step; then the shared salient lines; then
// unpinned.
func (r *resolver) closingActivity(s *wsState) *frontendv1.FooterStatusClosingActivity {
	if s.closing != nil {
		return &frontendv1.FooterStatusClosingActivity{Tier: &frontendv1.FooterStatusClosingActivity_Salient{
			Salient: &frontendv1.FooterStatusClosingSalient{
				At: stamp(s.closingAt),
				Kind: &frontendv1.FooterStatusClosingSalient_CloseBlocked{
					CloseBlocked: &frontendv1.FooterStatusActivityCloseBlocked{Text: s.closing.Detail}},
			}}}
	}
	if line, ok := r.sharedSalient(s); ok {
		return &frontendv1.FooterStatusClosingActivity{Tier: &frontendv1.FooterStatusClosingActivity_Salient{
			Salient: fillShared(&frontendv1.FooterStatusClosingSalient{}, line)}}
	}
	return &frontendv1.FooterStatusClosingActivity{Tier: &frontendv1.FooterStatusClosingActivity_Unpinned{Unpinned: r.unpinned(s)}}
}

// loadingActivity resolves the cell while context is injected: the injected
// item is the `context_injected` transient the same injection raised, so only
// the shared salient lines can stand.
func (r *resolver) loadingActivity(s *wsState) *frontendv1.FooterStatusLoadingActivity {
	if line, ok := r.sharedSalient(s); ok {
		return &frontendv1.FooterStatusLoadingActivity{Tier: &frontendv1.FooterStatusLoadingActivity_Salient{
			Salient: fillShared(&frontendv1.FooterStatusLoadingSalient{}, line)}}
	}
	return &frontendv1.FooterStatusLoadingActivity{Tier: &frontendv1.FooterStatusLoadingActivity_Unpinned{Unpinned: r.unpinned(s)}}
}

// waitingSalient is the one salient line a waiting status stands on. It is
// composed BY THE SAME DECISION that picks the waiting step (status.go
// waiting, wakeup), so the kind that explains the step always exists and ranks
// first: a waiting session is by construction parked on something, and the
// contract gives its cell no unpinned branch.
func waitingSalient(at time.Time, kind func(*frontendv1.FooterStatusWaitingSalient)) *frontendv1.FooterStatusWaitingActivity {
	salient := &frontendv1.FooterStatusWaitingSalient{At: stamp(at)}
	kind(salient)
	return &frontendv1.FooterStatusWaitingActivity{Salient: salient}
}

// activityLine is the published activity line as the log states it: the tier,
// the kind arm that stands and what it says. The zero value is no line.
type activityLine struct {
	tier string
	kind string
	text string
}

// name is the tier and kind for the record ("salient.update",
// "transient.tool_call", "enduring"), "none" when no line stands.
func (l activityLine) name() string {
	switch {
	case l.tier == "":
		return "none"
	case l.kind == "":
		return l.tier
	default:
		return l.tier + "." + l.kind
	}
}

// activityLineOf reads the one activity line a status carries, whichever status
// arm, tier and kind it is. It reads the contract's SHAPE — every status arm has
// an `activity` field; its line is a salient kind, a transient kind, or the
// enduring line — rather than enumerating the arms, so a kind added to the
// contract is recorded the day it is published.
//
// THE `at` AND `expiry` STAMPS ARE NOT PART OF THE LINE, and neither are the
// enduring line's figures: the line is what KIND of thing the reader sees and
// what it says, and a re-stamp or a moved percentage is not a change to it.
func activityLineOf(status *frontendv1.FooterStatus) activityLine {
	arm, ok := messageArm(status.ProtoReflect(), "status")
	if !ok {
		return activityLine{}
	}
	activityField := arm.Descriptor().Fields().ByName("activity")
	if activityField == nil || activityField.Kind() != protoreflect.MessageKind || !arm.Has(activityField) {
		return activityLine{}
	}
	activity := arm.Get(activityField).Message()
	// The waiting cell has no tier oneof: its salient line is its only field.
	if salientField := activity.Descriptor().Fields().ByName("salient"); salientField != nil && activity.Descriptor().Oneofs().ByName("tier") == nil {
		return kindLine("salient", activity.Get(salientField).Message())
	}
	tier := activity.WhichOneof(activity.Descriptor().Oneofs().ByName("tier"))
	if tier == nil || tier.Kind() != protoreflect.MessageKind {
		return activityLine{}
	}
	cell := activity.Get(tier).Message()
	if tier.Name() == "salient" {
		return kindLine("salient", cell)
	}
	transientField := cell.Descriptor().Fields().ByName("transient")
	if cell.Has(transientField) {
		return kindLine("transient", cell.Get(transientField).Message())
	}
	return activityLine{tier: "enduring"}
}

// messageArm answers the message set in the named oneof, if any.
func messageArm(m protoreflect.Message, oneof string) (protoreflect.Message, bool) {
	o := m.Descriptor().Oneofs().ByName(protoreflect.Name(oneof))
	if o == nil {
		return nil, false
	}
	field := m.WhichOneof(o)
	if field == nil || field.Kind() != protoreflect.MessageKind {
		return nil, false
	}
	return m.Get(field).Message(), true
}

// kindLine reads a line message's `kind` oneof: its arm name, and its `text`
// field when it has one (otherwise the arm's whole content).
func kindLine(tier string, line protoreflect.Message) activityLine {
	kinds := line.Descriptor().Oneofs().ByName("kind")
	if kinds == nil {
		return activityLine{}
	}
	kindField := line.WhichOneof(kinds)
	if kindField == nil {
		return activityLine{}
	}
	out := activityLine{tier: tier, kind: string(kindField.Name())}
	if kindField.Kind() != protoreflect.MessageKind {
		out.text = line.Get(kindField).String()
		return out
	}
	kind := line.Get(kindField).Message()
	if text := kind.Descriptor().Fields().ByName("text"); text != nil && text.Kind() == protoreflect.StringKind {
		out.text = kind.Get(text).String()
		return out
	}
	out.text = prototext.MarshalOptions{}.Format(kind.Interface())
	return out
}
