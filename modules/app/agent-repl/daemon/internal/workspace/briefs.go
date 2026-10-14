package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
)

// composeBrief reads the daemon brief NAME from this daemon's prompts
// directory and splices VALUES into it, answering the text a daemon-born
// prompt is sent as.
//
// EVERY DAEMON BRIEF THAT IS EVIDENCE ABOUT THE PROMPTS DIRECTORY COMPOSES
// THROUGH HERE, so the three things that go with reading one cannot drift
// apart between call sites: a brief that will not load or splice is recorded
// at ERROR under the caller's operation, raises the daemon-scoped
// prompts_dir_missing fault (raisePromptsFault), and is answered as an error
// naming the brief; one that composes closes that fault, because the
// condition it records no longer holds. The caller decides only how the error
// travels on (a refusal arm, or a wrapped error).
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
		v.raisePromptsFault(ctx, log, failed.Error())
		return "", failed
	}
	v.clearPromptsFault(ctx, log)
	return text, nil
}
