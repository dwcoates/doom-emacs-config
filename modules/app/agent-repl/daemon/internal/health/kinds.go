package health

import (
	"fmt"
	"strconv"

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
	// KindDeployScriptFailed is a self-reload whose deploy script failed.
	KindDeployScriptFailed = "deploy_script_failed"
	// KindSuccessorSpawnFailed is a handover whose successor would not start.
	KindSuccessorSpawnFailed = "successor_spawn_failed"
	// KindPromptsDirMissing is an absent prompts directory: every composed
	// brief would fail.
	KindPromptsDirMissing = "prompts_dir_missing"
	// KindWsmReadOnly is a state client that opened read-only, so nothing can
	// be recorded.
	KindWsmReadOnly = "wsm_read_only"
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
	// KindStateUnreadable is the liveness self-check's own fault when the
	// state client will not answer. It likewise has no typed arm.
	KindStateUnreadable = "daemon_state_unreadable"
)

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
	case KindDeployScriptFailed:
		out.Kind = &agentreplv1.DaemonFault_DeployScriptFailed{
			DeployScriptFailed: &agentreplv1.DaemonFaultDeployScriptFailed{Detail: evidenceDetail(f)},
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
	}
	return out
}

// sessionFault renders one recorded fault as the typed SessionFault. The same
// rule holds: a kind with no typed arm keeps its detail line.
func sessionFault(f wsm.Fault) *agentreplv1.SessionFault {
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
	case KindResumeFailed:
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
	}
	return out
}

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
func selfCheckFault(err error) *agentreplv1.DaemonFault {
	return &agentreplv1.DaemonFault{
		Detail: fmt.Sprintf("%s: %v", KindStateUnreadable, err),
	}
}
