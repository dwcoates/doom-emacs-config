package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/wsm"
)

// opBriefFault is the operation a brief's own fault records carry.
const opBriefFault = "daemon.workspace.brief_fault"

// composeBrief reads the daemon brief NAME from this daemon's prompts
// directory and splices VALUES into it, answering the text a daemon-born
// prompt is sent as.
//
// EVERY DAEMON BRIEF THAT IS EVIDENCE ABOUT THE PROMPTS DIRECTORY COMPOSES
// THROUGH HERE, so the three things that go with reading one cannot drift
// apart between call sites: a brief that will not load or splice is recorded
// at ERROR under the caller's operation, raises its fault, and is answered as
// an error naming the brief; one that composes closes those faults, because
// the conditions they record no longer hold. The caller decides only how the
// error travels on (a refusal arm, or a wrapped error).
//
// THE FAULT SAYS WHAT FAILED. A brief that will not READ is evidence about
// the prompts directory every brief reads from (prompts_dir_missing). A brief
// that read but will not SPLICE is about that one brief, whose placeholders
// and values disagree, so it raises brief_unsplicable, which names the brief
// and paints no row: every other brief still composes.
func (v *verbs) composeBrief(ctx context.Context, log dlog.Logger, op, name string, values map[string]string) (string, error) {
	brief, err := v.load(v.deps.PromptsDir, name)
	if err != nil {
		failed := fmt.Errorf("the %s brief is unreadable: %w", name, err)
		log.Error(op, "could not read a daemon brief", dlog.Context{"brief": name, "cause": err.Error()})
		v.raisePromptsFault(ctx, log, failed.Error())
		return "", failed
	}
	text, err := v.splice(brief, values)
	if err != nil {
		failed := fmt.Errorf("the %s brief will not splice: %w", name, err)
		log.Error(op, "could not splice a daemon brief", dlog.Context{"brief": name, "cause": err.Error()})
		v.raiseBriefFault(ctx, log, name, failed.Error(), err)
		return "", failed
	}
	v.clearPromptsFault(ctx, log)
	v.clearBriefFault(ctx, log, name)
	return text, nil
}

// raiseBriefFault records brief NAME's splice failure as a daemon-scoped
// brief_unsplicable fault naming it. At most one stands per brief: a second
// identical row would tell the operator nothing the first did not.
func (v *verbs) raiseBriefFault(ctx context.Context, log dlog.Logger, name, detail string, cause error) {
	v.raiseDaemonFault(ctx, log, opBriefFault, wsm.Fault{
		Kind:     health.KindBriefUnsplicable,
		Detail:   detail,
		Evidence: map[string]string{"brief": name, "cause": cause.Error()},
		OpenedAt: v.now(),
	}, func(f wsm.Fault) bool { return f.Evidence["brief"] == name }, dlog.Context{"brief": name})
}

// clearBriefFault closes brief NAME's standing brief_unsplicable fault: the
// brief just composed, so the disagreement it recorded is over. Another
// brief's fault stands.
func (v *verbs) clearBriefFault(ctx context.Context, log dlog.Logger, name string) {
	health.CloseOnEdge(ctx, health.ReporterFaults(v.deps.Health), log, health.EdgeBriefComposed,
		health.EdgeScope{DaemonOnly: true, Match: func(f wsm.Fault) bool { return f.Evidence["brief"] == name }}, v.now())
}
