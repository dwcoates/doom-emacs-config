package workspace

import (
	"fmt"
	"strings"
)

// The brief names the one-shot decoration reads from the prompts directory AT
// USE TIME. A missing brief is LOUD: the workspace is not created with an
// undecorated prompt, because an agent told to implement but never told what
// completion means would leave the workspace hanging.
const (
	// BriefAutonomousPreamble is the "do not wait for further instructions"
	// preamble every one-shot prompt opens with.
	BriefAutonomousPreamble = "workspace-autonomous-preamble"
	// BriefOneShotSuccessSuffix is the success-gated wrap-up instruction.
	BriefOneShotSuccessSuffix = "oneshot-success-suffix"
	// BriefOneShotCreatePrFollowup is the CICD-gated second stage of the
	// open-pr finish.
	BriefOneShotCreatePrFollowup = "oneshot-create-pr-then-close-followup"
	// BriefAddSupport is the add-support brief RequestCommandSupport composes.
	BriefAddSupport = "add-support-slash-command"
)

// The harness-injection markers. Text the USER never typed is bracketed with
// them at the point it is composed; they are the ONE source of truth for "this
// span was injected", and the feed resolver strips them from the drawn row
// while the full text stays on the record.
const (
	// MetaOpen opens a harness-injected span.
	MetaOpen = "<!--agent-repl:meta-->"
	// MetaClose closes one.
	MetaClose = "<!--/agent-repl:meta-->"
)

// The wrap-up spellings the one-shot finishes name. They are constants because
// the agent must invoke EXACTLY these commands; a drifted spelling leaves the
// agent invoking something the flow does not expect.
const (
	// WorkspaceSkill is the slash command naming the workspace lifecycle skill.
	WorkspaceSkill = "/create-or-update-workspace"
	// CreatePrSkill is the pr-creation skill the open-pr finish invokes.
	CreatePrSkill = "/create-or-update-pr"
	// selfMergeActionPhrase is what the self-merge finish accomplishes.
	selfMergeActionPhrase = "merge this workspace back into its source"
	// openPrActionPhrase is what the open-pr finish's first stage
	// accomplishes.
	openPrActionPhrase = "push and queue this branch for merge"
)

// metaWrap brackets a harness-injected span.
func metaWrap(text string) string { return MetaOpen + text + MetaClose }

// createPrCommand renders the pr invocation for an open-pr finish. The base
// flags are the flow's own (a patch PR rebased onto the refreshed base); the
// two request-carried flags are appended only when asked for, so the agent is
// never told to self-certify a change the user did not self-certify.
func createPrCommand(pr *OneShotOpenPr) string {
	cmd := CreatePrSkill + " --patch --rebase"
	if pr != nil && pr.AddToMergeQueue {
		cmd += " --add-to-merge-queue"
	}
	if pr != nil && pr.SelfCertified {
		cmd += " --self-certified"
	}
	return cmd
}

// decorateOneShot composes a one-shot workspace's first message: the
// autonomous preamble, the user's own words, and the success-gated wrap-up.
// Everything the user did not type is meta-wrapped, so the drawn bubble is the
// user's words alone while the agent receives the whole composition verbatim.
//
// Every brief is read HERE, at use time, so editing one takes effect on the
// next one-shot without a daemon bounce.
func (v *verbs) decorateOneShot(raw string, finish *OneShotFinish) (string, error) {
	if finish == nil {
		return "", fmt.Errorf("a one-shot workspace needs a finish action")
	}
	preamble, err := v.load(v.deps.PromptsDir, BriefAutonomousPreamble)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefAutonomousPreamble, err)
	}
	preambleText, err := v.splice(preamble, nil)
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefAutonomousPreamble, err)
	}

	suffix, err := v.oneShotSuffix(finish)
	if err != nil {
		return "", err
	}
	return metaWrap(preambleText) + raw + metaWrap(suffix), nil
}

// oneShotSuffix renders the finish action's success-gated wrap-up. The
// self-merge finish is one gate (implementation, tests, commits); the open-pr
// finish is two, the second gating on the pr flow's own CICD result.
func (v *verbs) oneShotSuffix(finish *OneShotFinish) (string, error) {
	success, err := v.load(v.deps.PromptsDir, BriefOneShotSuccessSuffix)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefOneShotSuccessSuffix, err)
	}

	switch {
	case finish.SelfMerge:
		text, err := v.splice(success, map[string]string{
			"invocation":    "the " + WorkspaceSkill + " merge skill",
			"action_phrase": selfMergeActionPhrase,
		})
		if err != nil {
			return "", fmt.Errorf("splice the %s brief: %w", BriefOneShotSuccessSuffix, err)
		}
		return text, nil

	case finish.OpenPr != nil:
		prCommand := createPrCommand(finish.OpenPr)
		first, err := v.splice(success, map[string]string{
			"invocation":    "`" + prCommand + "`",
			"action_phrase": openPrActionPhrase,
		})
		if err != nil {
			return "", fmt.Errorf("splice the %s brief: %w", BriefOneShotSuccessSuffix, err)
		}
		followup, err := v.load(v.deps.PromptsDir, BriefOneShotCreatePrFollowup)
		if err != nil {
			return "", fmt.Errorf("read the %s brief: %w", BriefOneShotCreatePrFollowup, err)
		}
		second, err := v.splice(followup, map[string]string{
			"create_pr_command": prCommand,
			"wrapup_command":    WorkspaceSkill + " close",
		})
		if err != nil {
			return "", fmt.Errorf("splice the %s brief: %w", BriefOneShotCreatePrFollowup, err)
		}
		return first + second, nil

	default:
		return "", fmt.Errorf("a one-shot finish names neither a self merge nor a pull request")
	}
}

// finishOrigin names a one-shot finish for the creation job's record, so the
// turn that concludes with the success marker can act on it even after a
// daemon restart.
func finishOrigin(finish *OneShotFinish) string {
	switch {
	case finish == nil:
		return ""
	case finish.SelfMerge:
		return "self_merge"
	case finish.OpenPr != nil:
		flags := []string{"open_pr"}
		if finish.OpenPr.SelfCertified {
			flags = append(flags, "self_certified")
		}
		if finish.OpenPr.AddToMergeQueue {
			flags = append(flags, "add_to_merge_queue")
		}
		return strings.Join(flags, "+")
	default:
		return ""
	}
}
