package health

import (
	"fmt"
	"strconv"
	"strings"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/wsm"
)

// The DAEMON-scope fault kinds, spelled exactly as DaemonFault's kind oneof
// spells its arms. A recorded fault's Kind is one of these, which is what lets
// the reporter answer with a typed arm rather than a sentence.
const (
	// KindAdoptionWindowExpired is a handover whose adoption window ran out.
	KindAdoptionWindowExpired = "adoption_window_expired"
	// KindLogSinkPoisoned is a durable sink that can no longer be written.
	KindLogSinkPoisoned = "log_sink_poisoned"
	// KindSuccessorSpawnFailed is a handover whose successor would not start.
	KindSuccessorSpawnFailed = "successor_spawn_failed"
	// KindPromptsDirMissing is an absent prompts directory: every composed
	// brief would fail.
	KindPromptsDirMissing = "prompts_dir_missing"
	// KindWsmReadOnly is a state client that opened read-only, so nothing can
	// be recorded.
	KindWsmReadOnly = "wsm_read_only"
	// KindDeployFailed is a deploy whose build, install or service restart
	// did not go through. Its evidence is a DeployFailure's.
	KindDeployFailed = "deploy_failed"
)

// The SESSION-scope fault kinds, spelled exactly as SessionFault's kind oneof
// spells its arms.
const (
	// KindShimStartFailed is a shim that would not come up.
	KindShimStartFailed = "shim_start_failed"
	// KindShimDied is a shim that exited while serving.
	KindShimDied = "shim_died"
	// KindLinkSevered is a daemon-to-shim link that stopped serving.
	KindLinkSevered = "link_severed"
	// KindResumeFailed is a resume the shim refused.
	KindResumeFailed = "resume_failed"
	// KindBounceDied is a relaunch whose replacement shim died.
	KindBounceDied = "bounce_died"
	// KindBounceUnknown is a relaunch whose outcome could not be determined.
	KindBounceUnknown = "bounce_unknown"
	// KindClassifierFailed is a headless classifier run that failed.
	KindClassifierFailed = "classifier_failed"
	// KindShimReported is a fault the SHIM reported on its diagnostics push,
	// respelled onto this vocabulary with the shim's own component and kind
	// kept as evidence.
	KindShimReported = "shim_reported"
	// KindSessionAbsent is the reporter's own observation that a workspace has
	// no live session at all. It has no typed arm: it is not a fault the shim
	// or the daemon RAISED, it is the liveness probe's answer, so it is
	// reported as an untyped detail line.
	KindSessionAbsent = "session_absent"
	// KindConversationAbandoned is a bring-up whose recorded conversation had
	// no transcript on disk: the session came up FRESH and the old vendor
	// session id is left behind. It has no typed arm — it is reported as an
	// untyped detail line — because the workspace has a LIVE session and the
	// fault is the record of what was abandoned, not a failure to serve.
	KindConversationAbandoned = "conversation_abandoned"
	// KindWatchOpenRefused is a shim watch OPEN refused for a handle NOTHING
	// announced: the daemon and the shim disagree about what exists. It is
	// NOT a severed link — the shim answered — and it has no typed arm, so it
	// is reported as an untyped detail line carrying the refused handle.
	KindWatchOpenRefused = "watch_open_refused"
	// KindStateUnreadable is the liveness self-check's own fault when the
	// state client will not answer. It likewise has no typed arm.
	KindStateUnreadable = "daemon_state_unreadable"
	// KindFinalAnswerUnresolved is a turn that concluded with no green answer
	// standing: the terminal named no answer while the turn drew response
	// prose, the answer it named resolves to no drawn response row, or an open
	// response fold went silent. The session is SERVING — the prose is on
	// screen — so it never escalates the status; see footer.go's partition.
	KindFinalAnswerUnresolved = "final_answer_unresolved"
	// KindRelaunchResumeFailed is the LEGACY spelling the rollout controller
	// used for a relaunch whose resume failed. It names the same class as
	// KindResumeFailed and renders through the same arm; only records written
	// before the spelling was unified still carry it.
	KindRelaunchResumeFailed = "relaunch_resume_failed"
	// KindBounceDisposition is an ORDINARY reconciled bounce disposition: a
	// session the bounce preserved or rolled, recorded and closed in one
	// breath so the per-session accounting survives without polluting the
	// open fault set.
	KindBounceDisposition = "bounce_disposition"
)

// armlessSessionKinds are the recorded fault kinds the SessionFault / HostFault
// oneof does not spell an arm for. THE ARM IS THE FAULT CLASS -- both surfaces
// refuse a fault whose oneof is unset -- so a fault of one of these kinds is
// kept off the wire entirely rather than sent as a prose-only line that would
// break the whole push. The fault itself is recorded, loudly, at the site that
// opened it; this set only governs what the two RENDERERS can carry.
//
// THE FIVE KINDS THAT WERE OWED AN ARM NOW HAVE ONE (2026-09-12): the surfaces
// spell conversation_abandoned, session_absent, watch_open_refused,
// daemon_state_unreadable and the workspace-scoped adoption_window_expired,
// and each was removed from this set in the commit that switched its renderer
// onto the landed arm. What remains here is armless BY DESIGN, not by debt.
var armlessSessionKinds = map[string]struct{}{
	// An ordinary reconciled bounce disposition, opened and closed in one
	// breath. It needs no arm: it is per-session accounting, never a standing
	// condition a host view should draw.
	KindBounceDisposition: {},
}

// ArmlessSessionKind reports whether kind is one the SessionFault / HostFault
// oneof has no arm for, and so cannot be carried on either surface.
func ArmlessSessionKind(kind string) bool {
	_, ok := armlessSessionKinds[kind]
	return ok
}

// daemonFault renders one recorded fault as the typed DaemonFault the wire
// carries. A kind with no typed arm answers with the detail line alone rather
// than being dropped: an unreportable fault is still a fault.
func daemonFault(f wsm.Fault) *agentreplv1.DaemonFault {
	out := &agentreplv1.DaemonFault{Detail: faultDetail(f)}
	switch f.Kind {
	case KindAdoptionWindowExpired:
		arm := &agentreplv1.DaemonFaultAdoptionWindowExpired{}
		if id := f.Evidence["workspace"]; id != "" {
			arm.Workspace = &workspacev1.WorkspaceRef{Id: id, Dir: f.Evidence["workspace_dir"]}
		}
		out.Kind = &agentreplv1.DaemonFault_AdoptionWindowExpired{AdoptionWindowExpired: arm}
	case KindLogSinkPoisoned:
		out.Kind = &agentreplv1.DaemonFault_LogSinkPoisoned{
			LogSinkPoisoned: &agentreplv1.DaemonFaultLogSinkPoisoned{Sink: f.Evidence["sink"]},
		}
	case KindSuccessorSpawnFailed:
		out.Kind = &agentreplv1.DaemonFault_SuccessorSpawnFailed{
			SuccessorSpawnFailed: &agentreplv1.DaemonFaultSuccessorSpawnFailed{Detail: evidenceDetail(f)},
		}
	case KindPromptsDirMissing:
		out.Kind = &agentreplv1.DaemonFault_PromptsDirMissing{
			PromptsDirMissing: &agentreplv1.DaemonFaultPromptsDirMissing{Path: f.Evidence["path"]},
		}
	case KindWsmReadOnly:
		out.Kind = &agentreplv1.DaemonFault_WsmReadOnly{WsmReadOnly: &agentreplv1.DaemonFaultWsmReadOnly{}}
	case KindStateUnreadable:
		out.Kind = &agentreplv1.DaemonFault_DaemonStateUnreadable{
			DaemonStateUnreadable: &agentreplv1.DaemonFaultDaemonStateUnreadable{Cause: evidenceCause(f)},
		}
	case KindDeployFailed:
		if arm := DeployFailureOf(f).arm(); arm != nil {
			out.Kind = &agentreplv1.DaemonFault_DeployFailed{DeployFailed: arm}
		}
	}
	return out
}

// sessionFault renders one recorded fault as the typed SessionFault, reporting
// false when the fault's kind has no arm to render it through.
//
// THE ARM IS THE FAULT CLASS. `SessionFault.kind' is a oneof the consumer
// refuses when it is unset, so a prose-only fault is not a lesser answer, it is
// a malformed message that costs the consumer the WHOLE response. A kind with
// no arm is therefore withheld from the wire and answered false; it stays
// recorded, and loud, at the site that opened it.
func sessionFault(f wsm.Fault) (*agentreplv1.SessionFault, bool) {
	out := &agentreplv1.SessionFault{Detail: faultDetail(f)}
	switch f.Kind {
	case KindShimStartFailed:
		out.Kind = &agentreplv1.SessionFault_ShimStartFailed{
			ShimStartFailed: &agentreplv1.SessionFaultShimStartFailed{
				ExitCode:   exitCode(f),
				StderrTail: f.Evidence["stderr_tail"],
			},
		}
	case KindShimDied:
		out.Kind = &agentreplv1.SessionFault_ShimDied{
			ShimDied: &agentreplv1.SessionFaultShimDied{ExitCode: exitCode(f)},
		}
	case KindLinkSevered:
		out.Kind = &agentreplv1.SessionFault_LinkSevered{LinkSevered: &agentreplv1.SessionFaultLinkSevered{}}
	case KindResumeFailed, KindRelaunchResumeFailed:
		out.Kind = &agentreplv1.SessionFault_ResumeFailed{
			ResumeFailed: &agentreplv1.SessionFaultResumeFailed{Cause: evidenceCause(f)},
		}
	case KindBounceDied:
		out.Kind = &agentreplv1.SessionFault_BounceDied{BounceDied: &agentreplv1.SessionFaultBounceDied{}}
	case KindBounceUnknown:
		out.Kind = &agentreplv1.SessionFault_BounceUnknown{BounceUnknown: &agentreplv1.SessionFaultBounceUnknown{}}
	case KindClassifierFailed:
		out.Kind = &agentreplv1.SessionFault_ClassifierFailed{
			ClassifierFailed: &agentreplv1.SessionFaultClassifierFailed{Detail: evidenceDetail(f)},
		}
	case KindShimReported:
		out.Kind = &agentreplv1.SessionFault_ShimReported{
			ShimReported: &agentreplv1.SessionFaultShimReported{
				Component: f.Evidence["component"],
				Kind:      f.Evidence["kind"],
			},
		}
	case KindConversationAbandoned:
		out.Kind = &agentreplv1.SessionFault_ConversationAbandoned{
			ConversationAbandoned: &agentreplv1.SessionFaultConversationAbandoned{
				VendorSessionId: f.Evidence["vendor_session_id"],
			},
		}
	case KindSessionAbsent:
		out.Kind = &agentreplv1.SessionFault_SessionAbsent{
			SessionAbsent: &agentreplv1.SessionFaultSessionAbsent{},
		}
	case KindWatchOpenRefused:
		out.Kind = &agentreplv1.SessionFault_WatchOpenRefused{
			WatchOpenRefused: &agentreplv1.SessionFaultWatchOpenRefused{
				Operation: f.Evidence["operation"],
				Handle:    f.Evidence["handle"],
			},
		}
	case KindStateUnreadable:
		out.Kind = &agentreplv1.SessionFault_DaemonStateUnreadable{
			DaemonStateUnreadable: &agentreplv1.SessionFaultDaemonStateUnreadable{Cause: evidenceCause(f)},
		}
	case KindAdoptionWindowExpired:
		out.Kind = &agentreplv1.SessionFault_AdoptionWindowExpired{
			AdoptionWindowExpired: &agentreplv1.SessionFaultAdoptionWindowExpired{
				AdoptionWindow: f.Evidence["adoption_window"],
			},
		}
	case KindFinalAnswerUnresolved:
		out.Kind = &agentreplv1.SessionFault_FinalAnswerUnresolved{
			FinalAnswerUnresolved: &agentreplv1.SessionFaultFinalAnswerUnresolved{
				Turn: f.Evidence["turn"],
				Unit: f.Evidence["unit"],
				Why:  f.Evidence["why"],
			},
		}
	default:
		return nil, false
	}
	return out, true
}

// StartFailedDetail composes the ONE line a bring-up failure explains itself
// with, out of the very evidence the `shim_start_failed` fault carries — the
// exit code and the stderr tail a spawn death records, or the refusal text an
// adoption records instead. The footer draws it, the fault carries it, and
// they read it from this one site, so a start failure cannot say two different
// things about itself on two surfaces.
//
// The stderr tail is a RING of the shim's last output and the strip has one
// line to draw, so its last non-empty line is what stands for it. Where there
// is no evidence at all, the fault's own detail is the line.
func StartFailedDetail(f wsm.Fault) string {
	tail := lastLine(f.Evidence["stderr_tail"])
	code, coded := f.Evidence["exit_code"]
	switch {
	case coded && tail != "":
		return fmt.Sprintf("exit %s: %s", code, tail)
	case coded:
		return fmt.Sprintf("exit %s", code)
	case tail != "":
		return tail
	default:
		return f.Detail
	}
}

// lastLine is the last non-empty line of a multi-line tail, trimmed.
func lastLine(text string) string {
	lines := strings.Split(text, "\n")
	for i := len(lines) - 1; i >= 0; i-- {
		if line := strings.TrimSpace(lines[i]); line != "" {
			return line
		}
	}
	return ""
}

// EvidenceExitCode is exitCode, exported for the HOST stream's HostFault,
// which fills the very same SessionFaultShimStartFailed / SessionFaultShimDied
// arms. One reader means the two surfaces cannot disagree about what a
// recorded exit code is.
func EvidenceExitCode(f wsm.Fault) int32 { return exitCode(f) }

// exitCode reads a recorded exit code. A missing or unparsable one answers
// zero, because the arm's field is not optional and an absent exit code is
// reported through the detail line rather than invented as a number.
func exitCode(f wsm.Fault) int32 {
	raw, ok := f.Evidence["exit_code"]
	if !ok {
		return 0
	}
	code, err := strconv.ParseInt(raw, 10, 32)
	if err != nil {
		return 0
	}
	return int32(code)
}

// evidenceDetail is the arm's own detail field: the recorded evidence when
// there is one, else the fault's prose detail, so the arm is never empty when
// the record said something.
func evidenceDetail(f wsm.Fault) string {
	if detail := f.Evidence["detail"]; detail != "" {
		return detail
	}
	return f.Detail
}

// evidenceCause is evidenceDetail for the arms that spell the field "cause".
func evidenceCause(f wsm.Fault) string {
	if cause := f.Evidence["cause"]; cause != "" {
		return cause
	}
	return f.Detail
}

// faultDetail renders one fault record as the wire's single detail line. The
// kind leads so a reader can tell two faults apart without a second field.
func faultDetail(f wsm.Fault) string {
	if f.Detail == "" {
		return f.Kind
	}
	return f.Kind + ": " + f.Detail
}

// selfCheckFault is the reporter's own fault when its state client will not
// answer. It is the one fault the daemon can always detect about itself.
//
// IT CARRIES ITS ARM. This was the last site that put a fault on the wire with
// the `kind' oneof unset, and every consumer reads an unset kind as a contract
// breach -- so the one fault the daemon can always report about itself was the
// one it could not report at all.
func selfCheckFault(err error) *agentreplv1.DaemonFault {
	return &agentreplv1.DaemonFault{
		Detail: fmt.Sprintf("%s: %v", KindStateUnreadable, err),
		Kind: &agentreplv1.DaemonFault_DaemonStateUnreadable{
			DaemonStateUnreadable: &agentreplv1.DaemonFaultDaemonStateUnreadable{Cause: err.Error()},
		},
	}
}
