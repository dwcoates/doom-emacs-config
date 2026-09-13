package workspace

import (
	"context"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
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
// Every brief is read HERE, at use time, from the REPOSITORY'S OWN POLICY
// SOURCE, so editing one takes effect on the next one-shot without a daemon
// bounce.
func (v *verbs) decorateOneShot(raw string, finish *OneShotFinish, policy prompts.Source) (string, error) {
	if finish == nil {
		return "", fmt.Errorf("a one-shot workspace needs a finish action")
	}
	preamble, err := v.load(policy.Dir, BriefAutonomousPreamble)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefAutonomousPreamble, err)
	}
	preambleText, err := v.splice(preamble, nil)
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefAutonomousPreamble, err)
	}

	suffix, err := v.oneShotSuffix(finish, policy)
	if err != nil {
		return "", err
	}
	return prompts.Wrap(preambleText) + raw + prompts.Wrap(suffix), nil
}

// oneShotSuffix renders the finish action's success-gated wrap-up. The
// self-merge finish is one gate (implementation, tests, commits); the open-pr
// finish is two, the second gating on the pr flow's own CICD result.
func (v *verbs) oneShotSuffix(finish *OneShotFinish, policy prompts.Source) (string, error) {
	success, err := v.load(policy.Dir, BriefOneShotSuccessSuffix)
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
		// Only the FIRST gate rides the opening prompt: implementation, tests
		// and commits, then the pr command. The second gate — the CICD-gated
		// wrap-up — is submitted as a POST-PROMPT when the turn concludes, so
		// the agent is never told how to finish before it has started.
		text, err := v.splice(success, map[string]string{
			"invocation":    "`" + createPrCommand(finish.OpenPr) + "`",
			"action_phrase": openPrActionPhrase,
		})
		if err != nil {
			return "", fmt.Errorf("splice the %s brief: %w", BriefOneShotSuccessSuffix, err)
		}
		return text, nil

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

// openPrFollowup composes the CICD-gated SECOND stage of the open-pr finish:
// once the pr command's own /check-cicd reports PASS, close the workspace
// rather than merging it, because the change lands through CICD and a local
// merge would duplicate the commits the merge queue already owns.
//
// It is composed at CONCLUSION, not at creation, so an edited brief takes
// effect on the very next one-shot that finishes.
func (v *verbs) openPrFollowup(pr *OneShotOpenPr, policy prompts.Source) (string, error) {
	followup, err := v.load(policy.Dir, BriefOneShotCreatePrFollowup)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefOneShotCreatePrFollowup, err)
	}
	text, err := v.splice(followup, map[string]string{
		"create_pr_command": createPrCommand(pr),
		"wrapup_command":    WorkspaceSkill + " close",
	})
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefOneShotCreatePrFollowup, err)
	}
	return text, nil
}

// parseFinishOrigin reads back the finish a creation job recorded. It is
// finishOrigin's inverse, and they live beside each other so the recorded
// spelling and the acted-on action cannot drift.
func parseFinishOrigin(recorded string) (*OneShotFinish, error) {
	if recorded == "" {
		return nil, nil
	}
	parts := strings.Split(recorded, "+")
	switch parts[0] {
	case "self_merge":
		if len(parts) > 1 {
			return nil, fmt.Errorf("the self-merge finish takes no flags, got %q", recorded)
		}
		return &OneShotFinish{SelfMerge: true}, nil
	case "open_pr":
		pr := &OneShotOpenPr{}
		for _, flag := range parts[1:] {
			switch flag {
			case "self_certified":
				pr.SelfCertified = true
			case "add_to_merge_queue":
				pr.AddToMergeQueue = true
			default:
				return nil, fmt.Errorf("unknown finish flag %q in %q", flag, recorded)
			}
		}
		return &OneShotFinish{OpenPr: pr}, nil
	default:
		return nil, fmt.Errorf("unknown finish action %q", recorded)
	}
}

// opOneShotPolicySource is the operation every one-shot create records its
// chosen policy source under. A one-shot's whole composition comes from that
// directory, so which directory it was is the first thing an investigation of
// a wrongly-decorated one-shot needs.
const opOneShotPolicySource = "daemon.workspace.oneshot_policy_source"

// oneShotPolicyBriefs are the briefs a one-shot's policy MUST hold, for the
// finish it names. The preamble and the success suffix ride the opening
// prompt of every one-shot; the pull-request follow-up is composed only when
// an open-pr finish concludes, so only an open-pr create requires it.
func oneShotPolicyBriefs(finish *OneShotFinish) []string {
	briefs := []string{BriefAutonomousPreamble, BriefOneShotSuccessSuffix}
	if finish != nil && finish.OpenPr != nil {
		briefs = append(briefs, BriefOneShotCreatePrFollowup)
	}
	return briefs
}

// policySourceFor answers where repoDir's one-shot and merge policy is read
// from: the daemon's corpus for the repository the daemon's own checkout lives
// in, and the repository's own `.agent-repl/prompts` for every other. The
// corpus is NEVER a fallback for another repository.
func (v *verbs) policySourceFor(repoDir string) prompts.Source {
	return prompts.SourceFor(repoDir, v.deps.CheckoutRoot, v.deps.PromptsDir)
}

// requireOneShotPolicy refuses a one-shot create whose repository states no
// policy of its own, BEFORE anything is minted or materialized. The chosen
// source is recorded for every one-shot create, refused or not.
//
// Owner ruling, 2026-09-12: the repository defines its one-shot policy through
// files in its tree, the daemon detects the absence, and Emacs surfaces the
// refusal. A repository with no such config does not inherit the daemon's own.
func (v *verbs) requireOneShotPolicy(log dlog.Logger, repoDir string, finish *OneShotFinish) (prompts.Source, error) {
	policy := v.policySourceFor(repoDir)
	log.Info(opOneShotPolicySource, "chose the one-shot policy source", dlog.Context{
		"repository_root": policy.RepositoryRoot,
		"source":          policy.Kind,
		"policy_dir":      policy.Dir,
	})
	if policy.Kind != prompts.SourceRepository {
		return policy, nil
	}
	missing := v.deps.Policy.Missing(policy.Dir, oneShotPolicyBriefs(finish))
	if len(missing) == 0 {
		return policy, nil
	}
	return policy, refuseWith(log, "CreateWorkspace", ArmOneShotPolicyMissing,
		fmt.Sprintf("the repository %q states no one-shot policy: %s holds none of %s",
			policy.RepositoryRoot, policy.Dir, strings.Join(missing, ", ")),
		false, map[string]any{
			"repository_root": policy.RepositoryRoot,
			"policy_dir":      policy.Dir,
			"missing_files":   missing,
		})
}

// repositoryRootOf answers a registered workspace's repository main checkout
// root, which is what its policy source is derived from. The workspace record
// names its repository by id; the registry holds the directory.
func (v *verbs) repositoryRootOf(ctx context.Context, repo ids.RepoID) (string, error) {
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		return "", fmt.Errorf("read the repository registry: %w", err)
	}
	for _, repository := range repositories {
		if repository.ID == repo {
			return repository.Dir, nil
		}
	}
	return "", fmt.Errorf("no repository %q is registered", repo)
}
