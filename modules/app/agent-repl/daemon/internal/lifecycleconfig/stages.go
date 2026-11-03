package lifecycleconfig

import (
	"fmt"
	"path/filepath"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"
)

// This file is WHICH HOOKS A STAGE RUNS FOR A WORKSPACE: the stages, the two
// kinds of workspace (by originator), and the built-in defaults a kind left
// unset runs. Every runner of a stage (the create, the finish, the deploy)
// asks Hooks, so the choice between a repository's list and the default is
// made in one place.

// Stage is one of a workspace's lifecycle stages, in the order a workspace
// meets them.
type Stage int

// The stages.
const (
	StageCreation Stage = iota
	StageFinish
	StagePreDeploy
	StageDeploy
	StagePostDeploy
)

// Stages are every stage, in order.
var Stages = []Stage{StageCreation, StageFinish, StagePreDeploy, StageDeploy, StagePostDeploy}

// Key is the stage's key in the config file, which is also its spelling in
// AGENT_REPL_HOOK_STAGE.
func (s Stage) Key() string {
	switch s {
	case StageCreation:
		return "creation"
	case StageFinish:
		return "finish"
	case StagePreDeploy:
		return "pre_deploy"
	case StageDeploy:
		return "deploy"
	case StagePostDeploy:
		return "post_deploy"
	}
	panic(fmt.Sprintf("lifecycleconfig: no stage %d", int(s)))
}

// EnvHookStage is the environment variable every script hook reads its stage
// from (Stage.Key).
const EnvHookStage = "AGENT_REPL_HOOK_STAGE"

// Originator is the kind of workspace a hook list is for, decided only by who
// created the workspace (agentrepl.v1.WorkspaceOriginator).
type Originator int

// The kinds. THE ZERO VALUE IS NO KIND: every creator states its originator,
// so a create that left it unset is a defect, never a default.
const (
	// OriginatorUserOrWorkspace is a workspace the user or another
	// workspace created.
	OriginatorUserOrWorkspace Originator = iota + 1
	// OriginatorEmojiAck is a workspace the Slack emoji-ack system created.
	OriginatorEmojiAck
)

// Originators are both kinds.
var Originators = []Originator{OriginatorEmojiAck, OriginatorUserOrWorkspace}

// String is the kind's key in the config file, and its spelling in records.
func (o Originator) String() string {
	switch o {
	case OriginatorUserOrWorkspace:
		return "user_or_workspace"
	case OriginatorEmojiAck:
		return "emoji_ack"
	}
	panic(fmt.Sprintf("lifecycleconfig: no originator %d", int(o)))
}

// ParseOriginator reads a kind's spelling back (Originator.String).
func ParseOriginator(spelled string) (Originator, error) {
	for _, o := range Originators {
		if o.String() == spelled {
			return o, nil
		}
	}
	return 0, fmt.Errorf("lifecycleconfig: %q names no kind of workspace", spelled)
}

// Builtins is what the built-in defaults need from the running daemon.
type Builtins struct {
	// CheckoutRoot is the agent-repl module checkout the daemon was deployed
	// from, where the built-in scripts live.
	CheckoutRoot string
}

// The built-in defaults' texts and script. Each exists only for an emoji-ack
// workspace, which always has a Slack origin, so each may name the thread.
const (
	// DefaultCreationPrompt is the emoji-ack creation stage's default: the
	// thread is told work started, in the agent's own words.
	DefaultCreationPrompt = "This workspace was created for a Slack message: " + SlackThreadPlaceholder + ". " +
		"Before you start the work, post one brief reply in that message's thread, in your own words, " +
		"saying that work on it has started. Post it with the send-to-slack skill."
	// DefaultFinishPrompt is the emoji-ack finish stage's default: the thread
	// is told the pull request was created, and the post is stored as the
	// link announcing it, which the post-deploy default reacts on.
	DefaultFinishPrompt = "Post a brief reply in the thread of the Slack message this workspace was created for (" +
		SlackThreadPlaceholder + ") saying that this work's pull request was created, with a hyperlink to the pull request. " +
		"Post it with the send-to-slack skill. Then store that Slack message's permalink as an owned link announcing the pull request: " +
		"`agent-repld workspace link here -url <the Slack message's permalink> -label <two to eight words> -owned -announces <the pull request's url>`. " +
		"The pull request's url must already be one of this workspace's links. If this work opened no pull request, post nothing and store nothing."
	// DefaultPostDeployScript is the emoji-ack post-deploy stage's default,
	// relative to the agent-repl checkout: it reacts `:merged:` on every Slack
	// message whose stored link announces a merged pull request.
	DefaultPostDeployScript = "bin/hooks/slack-merged-reaction.sh"
	// DefaultPostDeployTimeoutSeconds bounds that script: it makes one daemon
	// read, then one GitHub read and one Slack call per announcing link.
	DefaultPostDeployTimeoutSeconds = 120
)

// hasDefault reports whether STAGE has a built-in default for KIND, which is
// also where a `builtin` hook may stand.
func (s Stage) hasDefault(kind Originator) bool {
	return kind == OriginatorEmojiAck && (s == StageCreation || s == StageFinish || s == StagePostDeploy)
}

// defaultHooks is STAGE's built-in default for KIND: every default runs for
// any target. Empty where the stage has none.
func (b Builtins) defaultHooks(stage Stage, kind Originator) []*repoconfigv1.Hook {
	if !stage.hasDefault(kind) {
		return nil
	}
	switch stage {
	case StageCreation:
		return []*repoconfigv1.Hook{PromptHook(DefaultCreationPrompt)}
	case StageFinish:
		return []*repoconfigv1.Hook{PromptHook(DefaultFinishPrompt)}
	case StagePostDeploy:
		if b.CheckoutRoot == "" {
			panic("lifecycleconfig: the post-deploy default asked for with no agent-repl checkout to find its script in")
		}
		return []*repoconfigv1.Hook{ScriptHook(filepath.Join(b.CheckoutRoot, DefaultPostDeployScript), DefaultPostDeployTimeoutSeconds)}
	}
	panic("lifecycleconfig: a stage with a default has no hooks for it")
}

// Hooks answers the hooks STAGE runs, in order, for a workspace of KIND whose
// target is (toMain) or is not the repository's main branch: the kind's list
// when the config sets one, its `builtin` hooks replaced by the default, and
// otherwise the kind's built-in default. CFG nil is a repository with no
// config, which runs the defaults alone. The deploy stage's one hook is
// DeployHook's.
func (b Builtins) Hooks(cfg *repoconfigv1.LifecycleConfig, stage Stage, kind Originator, toMain bool) []*repoconfigv1.Hook {
	if stage == StageDeploy {
		panic("lifecycleconfig: the deploy stage's hook is DeployHook's")
	}
	list := listOf(cfg, stage, kind)
	if list == nil {
		return b.defaultHooks(stage, kind)
	}
	var out []*repoconfigv1.Hook
	for _, entry := range list.GetHooks() {
		if !RunsFor(entry, toMain) {
			continue
		}
		if entry.GetHook().GetBuiltin() != nil {
			out = append(out, b.defaultHooks(stage, kind)...)
			continue
		}
		out = append(out, entry.GetHook())
	}
	return out
}

// DeployHook is the deploy stage's hook for a workspace of KIND whose target
// is (toMain) or is not the repository's main branch, nil when it has none.
func DeployHook(cfg *repoconfigv1.LifecycleConfig, kind Originator, toMain bool) *repoconfigv1.Hook {
	entry := deployEntryOf(cfg, kind)
	if entry == nil || !RunsFor(entry, toMain) {
		return nil
	}
	return entry.GetHook()
}

// ReleaseScript is the deploy-stage script a hold's release runs once for the
// deploys it skipped, of either kind: the user_or_workspace kind's main-scoped
// script, else the emoji_ack kind's; nil when neither kind's deploy hook is
// one.
func ReleaseScript(cfg *repoconfigv1.LifecycleConfig) *repoconfigv1.HookScript {
	for _, kind := range []Originator{OriginatorUserOrWorkspace, OriginatorEmojiAck} {
		if script := DeployHook(cfg, kind, true).GetScript(); script != nil {
			return script
		}
	}
	return nil
}

// listOf is KIND's list in STAGE, nil when the config leaves it unset (or
// there is no config).
func listOf(cfg *repoconfigv1.LifecycleConfig, stage Stage, kind Originator) *repoconfigv1.LifecycleHookList {
	if cfg == nil {
		return nil
	}
	var emoji, other *repoconfigv1.LifecycleHookList
	switch stage {
	case StageCreation:
		emoji, other = cfg.GetCreation().GetEmojiAck(), cfg.GetCreation().GetUserOrWorkspace()
	case StageFinish:
		emoji, other = cfg.GetFinish().GetEmojiAck(), cfg.GetFinish().GetUserOrWorkspace()
	case StagePreDeploy:
		emoji, other = cfg.GetPreDeploy().GetEmojiAck(), cfg.GetPreDeploy().GetUserOrWorkspace()
	case StagePostDeploy:
		emoji, other = cfg.GetPostDeploy().GetEmojiAck(), cfg.GetPostDeploy().GetUserOrWorkspace()
	default:
		panic("lifecycleconfig: the deploy stage holds one hook per kind, no list")
	}
	if kind == OriginatorEmojiAck {
		return emoji
	}
	return other
}

// deployEntryOf is KIND's deploy hook entry, nil when unset.
func deployEntryOf(cfg *repoconfigv1.LifecycleConfig, kind Originator) *repoconfigv1.HookEntry {
	if kind == OriginatorEmojiAck {
		return cfg.GetDeploy().GetEmojiAck()
	}
	return cfg.GetDeploy().GetUserOrWorkspace()
}
