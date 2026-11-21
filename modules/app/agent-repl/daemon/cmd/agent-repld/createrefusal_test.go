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
		with("name_reserved", map[string]any{"branch": "x/y"}, "the name x/y is reserved by a pending async merge"),
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
		set("unknown_requester", "unknown_requester", &agentreplv1.CreateWorkspaceUnknownRequester{Workspace: ref("/w/r")}, "the daemon has no requesting workspace /w/r"),
		with("async_merge_unknown_branch", map[string]any{"branch": "b"}, "the branch b does not exist in the repository"),
		with("async_merge_unknown_subagent", map[string]any{"agent_id": "a1"}, "no workspace's session holds the subagent a1"),
		set("async_merge_unknown_maker", "async_merge_unknown_maker", &agentreplv1.CreateWorkspaceAsyncMergeUnknownMaker{Workspace: ref("/w/m")},
			"the maker workspace /w/m is unknown or in another repository"),
		set("async_merge_context_not_maker", "async_merge_context_not_maker", &agentreplv1.CreateWorkspaceAsyncMergeContextNotMaker{Maker: ref("/w/m")},
			"the async merge's context is not a fork of its maker /w/m"),
		with("async_merge_name_taken", map[string]any{"branch": "m"}, "the name m is already a workspace, a branch, or another pending async merge's; ask again with another name"),
		with("async_merge_name_off_queue", map[string]any{"name": "m"},
			"the name m is held by a merge that left its queue without landing and whose request stands; put it back with `agent-repld merge-queue requeue m`"),
		with("placement_behind_not_in_line", map[string]any{"behind": "other"}, "the merge other the async merge was to go behind is not in its queue"),
		plain("async_merge_unknown_close_after", "the workspace to close after the merge is not open, is in another repository, or is the requester itself"),
		plain("op_id_with_async_merge", "an op id came with an async merge, whose progress never rides the create's channel"),
		plain("version_increment_required", "the merge lands on agent-repl's master and an agent asked for it, so it needs -version-part minor or patch and -changelog-line TEXT"),
		set("version_increment_unexpected/unversioned_repository", "version_increment_unexpected", &agentreplv1.MergeWorkspaceVersionIncrementUnexpected{
			Reason: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected_UnversionedRepository{UnversionedRepository: &agentreplv1.MergeWorkspaceVersionIncrementUnexpectedUnversionedRepository{}}},
			"the merge carries a version increment, but its repository's deploy config versions nothing; ask again without -version-part or -changelog-line"),
		set("version_increment_unexpected/not_default_branch", "version_increment_unexpected", &agentreplv1.MergeWorkspaceVersionIncrementUnexpected{
			Reason: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected_NotDefaultBranch{NotDefaultBranch: &agentreplv1.MergeWorkspaceVersionIncrementUnexpectedNotDefaultBranch{}}},
			"the merge carries a version increment, but it lands into a parent workspace's checkout, not the main branch; ask again without -version-part or -changelog-line"),
		plain("async_merge_requester_merging", "the requester's own merge is running; nothing was recorded"),
		plain("async_merge_requester_session_deleted", "the requester's session was deleted, so the merge's report could never reach it; nothing was recorded"),
		with("origin_fetch_failed", map[string]any{"detail": "offline"}, "fetching origin's primary branch failed (offline)"),
		with("async_merge_no_deploy_config", map[string]any{"path": "/r/l.yaml"}, "the repository has no /r/l.yaml, so nothing says what its deploy does"),
		with("async_merge_deploy_config_invalid", map[string]any{"path": "/r/l.yaml", "detail": "bad"}, "the repository's /r/l.yaml is unusable (bad)"),
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
