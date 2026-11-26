package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"
)

// createRefusals is the create describer with one case per
// CreateWorkspaceError arm.
var createRefusals = func() []describedRefusal {
	e := &agentreplv1.CreateWorkspaceError{}
	ref := func(dir string) *workspacev1.WorkspaceRef { return &workspacev1.WorkspaceRef{Dir: dir} }
	plain := func(arm, want string) refusalCase { return refusalCase{name: arm, err: armed(e, arm, nil), want: want} }
	with := func(arm string, fields map[string]any, want string) refusalCase {
		return refusalCase{name: arm, err: armedWith(e, arm, fields), want: want}
	}
	set := func(name, arm string, value proto.Message, want string) refusalCase {
		return refusalCase{name: name, err: armed(e, arm, value), want: want}
	}
	cases := []refusalCase{
		plain("ungated_without_consent", "an ungated permission mode was asked for without the explicit consent"),
		plain("no_slug", "no slug could be derived for the workspace's branch and directory"),
		with("brief_missing", map[string]any{"name": "b.md"}, "the brief b.md is absent"),
		plain("unknown_repository", "the daemon has no repository by the one the create named"),
		with("worktree_creation_failed", map[string]any{"detail": "git"}, "creating the worktree failed (git)"),
		with("spawn_failed", map[string]any{"detail": "exec"}, "spawning the new workspace's shim failed (exec)"),
		set("one_shot_policy_missing", "one_shot_policy_missing", &agentreplv1.CreateWorkspaceOneShotPolicyMissing{RepositoryRoot: "/r", PolicyDir: "/r/.p", MissingFiles: []string{"a", "b"}},
			"a one-shot was asked for in /r, which states no one-shot policy of its own (/r/.p lacks a, b)"),
		with("naming_failed", map[string]any{"cause": "timeout", "attempts": uint32(3), "model": "haiku"},
			"the daemon could not name the workspace (timeout after 3 attempts with haiku); name it with -name"),
		with("inside_temporary_directory", map[string]any{"dir": "/tmp/x", "temporary_root": "/tmp"}, "/tmp/x lies inside the temporary directory /tmp"),
		set("unknown_fork_source", "unknown_fork_source", &agentreplv1.CreateWorkspaceUnknownForkSource{Workspace: ref("/w/f")}, "the daemon has no workspace /w/f to fork"),
		set("fork_source_has_no_conversation", "fork_source_has_no_conversation", &agentreplv1.CreateWorkspaceForkSourceHasNoConversation{Workspace: ref("/w/f")},
			"/w/f has no conversation to fork"),
		set("fork_source_has_no_finished_turn", "fork_source_has_no_finished_turn", &agentreplv1.CreateWorkspaceForkSourceHasNoFinishedTurn{Workspace: ref("/w/f")},
			"/w/f is still in its first turn, so a fork would carry nothing; create the workspace with a fresh context, carrying what it should know in the prompt, or fork once that turn ends"),
		with("name_reserved", map[string]any{"branch": "x/y"}, "the name x/y is claimed by a create in flight; ask again once that create settles"),
		set("name_held/open", "name_held", &agentreplv1.CreateWorkspaceNameHeld{Branch: "x/y", Workspace: ref("/w/y"), Open: true},
			"the name x/y is already the name or branch of the open workspace at /w/y, which has not landed"),
		set("name_held/closed", "name_held", &agentreplv1.CreateWorkspaceNameHeld{Branch: "x/y", Workspace: ref("/w/y")},
			"the name x/y is already the name or branch of the closed workspace at /w/y, which has not landed"),
		with("name_branch_exists", map[string]any{"branch": "x/y", "head": "abc123"},
			"the branch x/y already exists (at abc123), and no workspace the daemon serves holds it; choose another name"),
		set("invalid_name/slug", "invalid_name", &agentreplv1.CreateWorkspaceInvalidName{Name: "a/B", Component: "B", Rule: &agentreplv1.CreateWorkspaceInvalidName_Slug{Slug: &agentreplv1.CreateWorkspaceInvalidNameSlug{}}},
			`the name "a/B" breaks the naming rule: its last component "B" is not a slug`),
		set("invalid_name/prefix", "invalid_name", &agentreplv1.CreateWorkspaceInvalidName{Name: "A/b", Component: "A", Rule: &agentreplv1.CreateWorkspaceInvalidName_Prefix{Prefix: &agentreplv1.CreateWorkspaceInvalidNamePrefix{}}},
			`the name "A/b" breaks the naming rule: its component "A" before the last is not a prefix`),
		set("unknown_context_workspace", "unknown_context_workspace", &agentreplv1.CreateWorkspaceUnknownContextWorkspace{Workspace: ref("/w/c")},
			"the daemon has no workspace /w/c for the one-shot's context"),
		set("unknown_owner", "unknown_owner", &agentreplv1.CreateWorkspaceUnknownOwner{Owner: ref("/w/o")}, "the daemon has no workspace /w/o to be the parent"),
		set("unknown_landing_target", "unknown_landing_target", &agentreplv1.CreateWorkspaceUnknownLandingTarget{Workspace: ref("/w/t")},
			"the workspace to land into, /w/t, is unknown or of another repository, and no branch crosses repositories"),
		with("unknown_source_branch", map[string]any{"branch": "b"}, "the source branch b does not exist in the repository"),
		with("unknown_subagent", map[string]any{"agent_id": "a1"}, "no workspace's session of the repository holds the subagent a1"),
		set("unknown_close_after", "unknown_close_after", &agentreplv1.CreateWorkspaceUnknownCloseAfter{Workspace: ref("/w/c")},
			"the workspace to close once the one-shot lands, /w/c, is not open, is in another repository, or is the requester itself"),
		set("unknown_requester", "unknown_requester", &agentreplv1.CreateWorkspaceUnknownRequester{Workspace: ref("/w/r")}, "the daemon has no requesting workspace /w/r"),
		with("origin_fetch_failed", map[string]any{"detail": "offline"}, "fetching origin's primary branch failed (offline)"),
		with("transferring_away", map[string]any{"address": "127.0.0.1:9"},
			"the daemon is handing its workspaces to its successor at 127.0.0.1:9 and admits no new create; create it again there"),
		plain("standing_down", "the daemon is standing down and admits no new create; create it again once the next daemon serves"),
		set("model_not_in_catalog", "model_not_in_catalog", &agentreplv1.CreateWorkspaceModelNotInCatalog{Model: "x", ConfigDir: "/c",
			Options: []*conversationv1.ModelOption{{Model: &conversationv1.AgentModel{Name: "opus"}}, {Model: &conversationv1.AgentModel{Name: "haiku"}}}},
			"the model x is not in the catalog of the login at /c, which offers opus, haiku"),
		with("model_catalog_unknown", map[string]any{"model": "x", "config_dir": "/c"},
			"no session on the login at /c has served a model catalog yet, so the model x is unknown; create it without -model to run under the daemon's default"),
		with("lifecycle_config_invalid", map[string]any{"path": "/r/l.yaml", "detail": "bad"}, "the repository's /r/l.yaml is unusable (bad), so its creation stage cannot be read"),
		with("creation_hook_failed", map[string]any{"path": "/r/h.sh", "detail": "exit 1", "log_path": "/l"},
			"the creation hook /r/h.sh failed (exit 1; its log is /l); the create was undone, and the repository's main workspace was asked to find out why"),
		with("placement_on_child", map[string]any{"detail": "c"}, "the placement names a child, which sits wherever its parent does (c)"),
		with("placement_on_merge_workspace", map[string]any{"detail": "m"}, "a merge workspace is drawn nowhere until revealed, so it takes no placement (m)"),
		with("placement_requires_workspace", map[string]any{"detail": "u"}, "the user asked, not a workspace, and only a workspace places what it creates (u)"),
		with("unknown_placement_subsection", map[string]any{"name": "s"}, "the repository has no subsection s"),
		plain("blank_placement_subsection_name", "the new subsection's name is blank"),
		with("gather_unknown_workspace", map[string]any{"name": "g"}, "the repository holds no workspace g to gather"),
		set("gather_not_gatherable", "gather_not_gatherable", &agentreplv1.CreateWorkspaceGatherNotGatherable{Workspace: ref("/w/g"), Detail: "closed"},
			"the workspace at /w/g cannot be gathered (closed)"),
		set("requested_by_deploy_mediator", "requested_by_deploy_mediator",
			&agentreplv1.CreateWorkspaceRequestedByDeployMediator{Workspace: ref("/w/m"), Detail: "do the work in your own session"},
			"the requester at /w/m is its repository's deploy mediator, which never creates a workspace: do the work in your own session"),
		set("requested_by_production_mediator", "requested_by_production_mediator",
			&agentreplv1.CreateWorkspaceRequestedByProductionMediator{Workspace: ref("/w/p"), Detail: "dispatch it"},
			"the requester at /w/p is its repository's production mediator, which creates its children only by dispatching them: dispatch it"),
	}
	cases = append(cases, parentNotOpenCases(e, "create it again")...)
	return []describedRefusal{{name: "create", empty: e, oneof: "cause", cases: cases,
		describe: func(m proto.Message) (string, error) {
			return describeCreateRefusal(m.(*agentreplv1.CreateWorkspaceError))
		}}}
}()

func TestDescribeCreateRefusals(t *testing.T) {
	testDescribedRefusals(t, createRefusals)
}
