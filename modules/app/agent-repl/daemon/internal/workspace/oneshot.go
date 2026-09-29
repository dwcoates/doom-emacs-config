package workspace

import (
	"context"
	"fmt"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// The brief names the one-shot decoration reads from the prompts directory AT
// USE TIME. A missing brief is LOUD: the workspace is not created with an
// undecorated prompt, because an agent told to implement but never told what
// completion means would leave the workspace hanging.
const (
	// BriefAutonomousPreamble is the "do not wait for further instructions"
	// preamble every one-shot prompt opens with.
	BriefAutonomousPreamble = "workspace-autonomous-preamble"
	// BriefOneShotCompletionDirective is the repository's ONE plain-English
	// statement of what is to be done when the work is done. The daemon
	// appends it to the commission and performs nothing itself.
	BriefOneShotCompletionDirective = "oneshot-completion-directive"
	// BriefAddSupport is the add-support brief RequestCommandSupport composes.
	BriefAddSupport = "add-support-slash-command"
)

// completionDirectiveLead is the LITERAL sentence that introduces the
// repository's completion directive. It is a constant because the directive is
// a plain-English file with no placeholders: the daemon supplies the framing
// and the repository supplies the instruction, and a drifted framing would
// leave an agent reading policy prose with no idea it is an instruction.
const completionDirectiveLead = "when you're all done, please do the following postprocessing directive: "

// decorateOneShot composes a one-shot workspace's first message: the
// autonomous preamble, the user's own words, and the repository's completion
// directive behind the framing sentence. Everything the user did not type is
// meta-wrapped, so the drawn bubble is the user's words alone while the agent
// receives the whole composition verbatim.
//
// Every brief is read HERE, at use time, from the REPOSITORY'S OWN POLICY
// SOURCE, so editing one takes effect on the next one-shot without a daemon
// bounce.
//
// THERE IS NO FINISH ACTION (owner ruling, 2026-09-12). The agent carries the
// directive out itself; the daemon never merges, opens a pull request, or
// submits a follow-up on the turn's conclusion.
func (v *verbs) decorateOneShot(raw string, policy prompts.Source) (string, error) {
	preamble, err := v.load(policy.Dir, BriefAutonomousPreamble)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefAutonomousPreamble, err)
	}
	preambleText, err := v.splice(preamble, nil)
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefAutonomousPreamble, err)
	}

	// THE DIRECTIVE IS PLAIN ENGLISH AND SPLICES NOTHING. It declares no
	// placeholders, so the nil value map is what refuses one that declares any:
	// the daemon has nothing to fill a placeholder in a repository's own
	// completion statement from.
	directive, err := v.load(policy.Dir, BriefOneShotCompletionDirective)
	if err != nil {
		return "", fmt.Errorf("read the %s brief: %w", BriefOneShotCompletionDirective, err)
	}
	directiveText, err := v.splice(directive, nil)
	if err != nil {
		return "", fmt.Errorf("splice the %s brief: %w", BriefOneShotCompletionDirective, err)
	}
	return prompts.Wrap(preambleText) + raw + prompts.Wrap("\n"+completionDirectiveLead+directiveText), nil
}

// opOneShotPolicySource is the operation every one-shot create records its
// chosen policy source under. A one-shot's whole composition comes from that
// directory, so which directory it was is the first thing an investigation of
// a wrongly-decorated one-shot needs.
const opOneShotPolicySource = "daemon.workspace.oneshot_policy_source"

// oneShotPolicyBriefs are the briefs a one-shot's policy MUST hold. Both ride
// the opening prompt of EVERY one-shot, so the set is fixed: there is no
// finish to vary it by.
func oneShotPolicyBriefs() []string {
	return []string{BriefAutonomousPreamble, BriefOneShotCompletionDirective}
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
func (v *verbs) requireOneShotPolicy(log dlog.Logger, repoDir string) (prompts.Source, error) {
	policy := v.policySourceFor(repoDir)
	log.Info(opOneShotPolicySource, "chose the one-shot policy source", dlog.Context{
		"repository_root": policy.RepositoryRoot,
		"source":          policy.Kind,
		"policy_dir":      policy.Dir,
	})
	if policy.Kind != prompts.SourceRepository {
		return policy, nil
	}
	missing := v.deps.Policy.Missing(policy.Dir, oneShotPolicyBriefs())
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

// repositoryRegisteredAt reports whether the registry holds a repository at a
// NORMALIZED directory. It is the write path's half of the repository
// invariant: a create names its repository by directory, and a directory the
// registry does not carry is refused rather than minted.
func (v *verbs) repositoryRegisteredAt(ctx context.Context, repoDir string) (bool, error) {
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		return false, fmt.Errorf("read the repository registry: %w", err)
	}
	_, found := wsm.RepositoryAt(repositories, repoDir)
	return found, nil
}

// repositoryRootOf answers a registered workspace's repository main checkout
// root, which is what its policy source is derived from. The workspace record
// names its repository by id; the registry holds the directory.
func (v *verbs) repositoryRootOf(ctx context.Context, repo ids.RepoID) (string, error) {
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		return "", fmt.Errorf("read the repository registry: %w", err)
	}
	if repository, found := wsm.RepositoryWithID(repositories, repo); found {
		return repository.Dir, nil
	}
	return "", fmt.Errorf("no repository %q is registered", repo)
}
