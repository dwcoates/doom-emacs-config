package main

import (
	"errors"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"google.golang.org/protobuf/proto"
)

// refusalCase is one refusal arm and the sentence its verb says for it.
type refusalCase struct {
	name string
	err  proto.Message
	want string
}

// finishRefusalCases holds one case per FinishWorkspaceError arm.
var finishRefusalCases = []refusalCase{
	{name: "unknown_workspace", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.FinishWorkspaceUnknownWorkspace{}}},
		want: "the daemon has no workspace ws"},
	{name: "workspace_ref_mismatch", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.FinishWorkspaceWorkspaceRefMismatch{RegistryDir: "/w/ws"}}},
		want: "ws lives at /w/ws, not at the directory the request named"},
	{name: "transferring_away", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_TransferringAway{TransferringAway: &agentreplv1.FinishWorkspaceTransferringAway{Address: "127.0.0.1:9"}}},
		want: "the daemon is restarting and handed ws to its successor at 127.0.0.1:9; finish it again"},
	{name: "not_yet_adopted", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_NotYetAdopted{NotYetAdopted: &agentreplv1.FinishWorkspaceNotYetAdopted{}}},
		want: "the daemon is restarting and has not taken ws over yet; finish it again once the restart finishes"},
	{name: "no_layout_facts", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_NoLayoutFacts{NoLayoutFacts: &agentreplv1.FinishWorkspaceNoLayoutFacts{}}},
		want: "ws records no merge geometry, so nothing says where it lands; record it by restating where the workspace lands: `agent-repld workspace set ws -lands-into main` (or `-lands-into WS`), then finish it again"},
	{name: "session_deleted", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_SessionDeleted{SessionDeleted: &agentreplv1.FinishWorkspaceSessionDeleted{}}},
		want: "ws's session has been deleted"},
	{name: "already_queued", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_AlreadyQueued{AlreadyQueued: &agentreplv1.FinishWorkspaceAlreadyQueued{}}},
		want: "ws's deploy is already waiting in the queue"},
	{name: "already_deploying", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_AlreadyDeploying{AlreadyDeploying: &agentreplv1.FinishWorkspaceAlreadyDeploying{}}},
		want: "ws's deploy is already running"},
	{name: "version_increment_required", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_VersionIncrementRequired{VersionIncrementRequired: &agentreplv1.MergeWorkspaceVersionIncrementRequired{}}},
		want: "ws lands on a versioned main branch; finish it again with -version-part minor or patch and -changelog-line TEXT"},
	{name: "version_increment_unexpected", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_VersionIncrementUnexpected{VersionIncrementUnexpected: &agentreplv1.MergeWorkspaceVersionIncrementUnexpected{}}},
		want: "ws does not land on a versioned main branch; finish it again without -version-part or -changelog-line"},
	{name: "has_open_children", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_HasOpenChildren{HasOpenChildren: &agentreplv1.WorkspaceHasOpenChildren{Children: []*workspacev1.WorkspaceRef{{Id: "c1"}, {Id: "c2"}}}}},
		want: "ws has 2 open children; resolve and close each before finishing it"},
	{name: "requester_may_not_land", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_RequesterMayNotLand{RequesterMayNotLand: &agentreplv1.FinishWorkspaceRequesterMayNotLand{}}},
		want: "the requester is neither ws, nor its owner, nor the user, so it may not finish it"},
	{name: "no_branch_checked_out", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_NoBranchCheckedOut{NoBranchCheckedOut: &agentreplv1.FinishWorkspaceNoBranchCheckedOut{Dir: "/w/ws"}}},
		want: "ws's worktree /w/ws has no branch checked out, so there is no branch to deploy"},
	{name: "no_deploy_config", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_NoDeployConfig{NoDeployConfig: &agentreplv1.LifecycleConfigMissing{Path: "/r/.agent-repl/deploy.yaml"}}},
		want: "ws's repository has no /r/.agent-repl/deploy.yaml, so nothing says what finishing does there"},
	{name: "deploy_config_invalid", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_DeployConfigInvalid{DeployConfigInvalid: &agentreplv1.LifecycleConfigInvalid{Path: "/r/.agent-repl/deploy.yaml", Detail: "bad yaml"}}},
		want: "ws's repository's /r/.agent-repl/deploy.yaml cannot be read (bad yaml)"},
	{name: "placement_behind_not_in_line", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_PlacementBehindNotInLine{PlacementBehindNotInLine: &agentreplv1.MergeQueuePlacementBehindNotInLine{Behind: "other"}}},
		want: "the merge other that ws was to go behind is not in its queue"},
	{name: "merge_off_queue", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_MergeOffQueue{MergeOffQueue: &agentreplv1.FinishWorkspaceMergeOffQueue{}}},
		want: "ws's merge left its queue without landing and its request stands; put it back with `agent-repld merge-queue requeue ws`, or end it with `agent-repld merge-queue discard ws` before finishing again"},
	{name: "placement_conflicts_with_stored", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_PlacementConflictsWithStored{PlacementConflictsWithStored: &agentreplv1.FinishWorkspacePlacementConflictsWithStored{
		Stored:    &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Front{Front: &agentreplv1.MergeQueuePlacementFront{}}},
		Requested: &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Behind{Behind: &agentreplv1.MergeQueuePlacementBehind{Merge: "other"}}},
	}}},
		want: "ws's create fixed its finish at the front of the merge queue, and this finish asked for behind=other; finish again with no -placement"},
	{name: "rule_checks_failed", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_RuleChecksFailed{RuleChecksFailed: &agentreplv1.MergeRuleChecksFailed{
		Worktree: "/w/a", Suites: []string{"repo-scans"}, FailingTests: "- TestX", Log: "/l"}}},
		want: "ws's branch failed its rule suites (repo-scans) in /w/a, so its merge was not admitted; fix the failing cases below, commit, and ask again\n\n- TestX"},
}

// oneShotRefusalCases holds one case per SetWorkspaceOneShotError arm.
var oneShotRefusalCases = []refusalCase{
	{name: "unknown_workspace", err: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.SetWorkspaceOneShotUnknownWorkspace{}}},
		want: "the daemon has no workspace ws"},
	{name: "workspace_ref_mismatch", err: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.SetWorkspaceOneShotWorkspaceRefMismatch{RegistryDir: "/w/ws"}}},
		want: "ws lives at /w/ws, not at the directory the request named"},
	{name: "closed", err: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_Closed{Closed: &agentreplv1.SetWorkspaceOneShotClosed{}}},
		want: "ws is closed"},
	{name: "no_creation_job", err: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_NoCreationJob{NoCreationJob: &agentreplv1.SetWorkspaceOneShotNoCreationJob{}}},
		want: "ws records no creation job to mark; record it by restating where the workspace lands: `agent-repld workspace set ws -lands-into main` (or `-lands-into WS`), then mark it again"},
	{name: "requester_may_not_set", err: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_RequesterMayNotSet{RequesterMayNotSet: &agentreplv1.SetWorkspaceOneShotRequesterMayNotSet{}}},
		want: "the requester is neither ws, nor its owner, nor the user, so it may not change its one-shot standing"},
	{name: "main_workspace", err: &agentreplv1.SetWorkspaceOneShotError{Cause: &agentreplv1.SetWorkspaceOneShotError_MainWorkspace{MainWorkspace: &agentreplv1.SetWorkspaceOneShotMainWorkspace{}}},
		want: "ws is its repository's main workspace, which lands nothing itself and is never a one-shot"},
}

// retargetRefusalCases holds one case per SetWorkspaceParentError arm.
var retargetRefusalCases = []refusalCase{
	{name: "unknown_workspace", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.SetWorkspaceParentUnknownWorkspace{}}},
		want: "the daemon has no workspace ws"},
	{name: "workspace_ref_mismatch", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_WorkspaceRefMismatch{WorkspaceRefMismatch: &agentreplv1.SetWorkspaceParentWorkspaceRefMismatch{RegistryDir: "/w/ws"}}},
		want: "ws lives at /w/ws, not at the directory the request named"},
	{name: "transferring_away", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_TransferringAway{TransferringAway: &agentreplv1.SetWorkspaceParentTransferringAway{Address: "127.0.0.1:9"}}},
		want: "the daemon is restarting and handed ws to its successor at 127.0.0.1:9; retarget it again"},
	{name: "not_yet_adopted", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_NotYetAdopted{NotYetAdopted: &agentreplv1.SetWorkspaceParentNotYetAdopted{}}},
		want: "the daemon is restarting and has not taken ws over yet; retarget it again once the restart finishes"},
	{name: "unknown_parent", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_UnknownParent{UnknownParent: &agentreplv1.SetWorkspaceParentUnknownParent{}}},
		want: "the daemon has no workspace by the new parent's name"},
	{name: "parent_ref_mismatch", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_ParentRefMismatch{ParentRefMismatch: &agentreplv1.SetWorkspaceParentParentRefMismatch{RegistryDir: "/w/p"}}},
		want: "the new parent lives at /w/p, not at the directory the request named"},
	{name: "cycle", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_Cycle{Cycle: &agentreplv1.SetWorkspaceParentCycle{}}},
		want: "the new parent is ws itself or one of its descendants"},
	{name: "no_merge_layout", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_NoMergeLayout{NoMergeLayout: &agentreplv1.SetWorkspaceParentNoMergeLayout{}}},
		want: "ws records no merge geometry to keep; restate where it lands with -lands-into main or -lands-into WS (a repository's main workspace never has any)"},
	{name: "cold_gate", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_ColdGate{ColdGate: &agentreplv1.SetWorkspaceParentColdGate{}}},
		want: "ws is parked at its cold gate; the user must answer the gate before it can be retargeted"},
	{name: "parent_not_open", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_ParentNotOpen{ParentNotOpen: &agentreplv1.WorkspaceParentNotOpen{Parent: &workspacev1.WorkspaceRef{Id: "p1", Dir: "/w/p"}}}},
		want: "the new parent or landing target /w/p is not open"},
	{name: "unknown_landing_target", err: &agentreplv1.SetWorkspaceParentError{Cause: &agentreplv1.SetWorkspaceParentError_UnknownLandingTarget{UnknownLandingTarget: &agentreplv1.SetWorkspaceParentUnknownLandingTarget{Workspace: &workspacev1.WorkspaceRef{Id: "t1", Dir: "/w/t"}}}},
		want: "ws cannot land into /w/t: it is unknown, of another repository, or ws itself or a workspace landing into it"},
}

// workspaceRefusals is each describer with its cases.
var workspaceRefusals = []describedRefusal{
	{name: "finish", cases: finishRefusalCases, empty: &agentreplv1.FinishWorkspaceError{}, oneof: "cause",
		describe: func(e proto.Message) (string, error) {
			return describeFinishRefusal("ws", e.(*agentreplv1.FinishWorkspaceError))
		}},
	{name: "one-shot", cases: oneShotRefusalCases, empty: &agentreplv1.SetWorkspaceOneShotError{}, oneof: "cause",
		describe: func(e proto.Message) (string, error) {
			return describeOneShotRefusal("ws", e.(*agentreplv1.SetWorkspaceOneShotError))
		}},
	{name: "retarget", cases: retargetRefusalCases, empty: &agentreplv1.SetWorkspaceParentError{}, oneof: "cause",
		describe: func(e proto.Message) (string, error) {
			return describeRetargetRefusal("ws", e.(*agentreplv1.SetWorkspaceParentError))
		}},
}

func TestDescribeWorkspaceRefusalsSayEachArm(t *testing.T) {
	for _, verb := range workspaceRefusals {
		for _, tt := range verb.cases {
			t.Run(verb.name+"/"+tt.name, func(t *testing.T) {
				// Act
				got, err := verb.describe(tt.err)

				// Assert
				if err != nil || got != tt.want {
					t.Fatalf("describe = %q, %v; want %q", got, err, tt.want)
				}
			})
		}
	}
}

func TestDescribeWorkspaceRefusalsCoverEveryArm(t *testing.T) {
	for _, verb := range workspaceRefusals {
		t.Run(verb.name, func(t *testing.T) {
			// Arrange
			cased := make([]proto.Message, 0, len(verb.cases))
			for _, tt := range verb.cases {
				cased = append(cased, tt.err)
			}

			// Act / Assert
			for _, missing := range uncasedArms(verb.empty, "cause", cased) {
				t.Errorf("%s arm %s has no case: give its describer a sentence for it and a case here", verb.empty.ProtoReflect().Descriptor().Name(), missing)
			}
		})
	}
}

func TestDescribeWorkspaceRefusalWithNoSentenceIsABug(t *testing.T) {
	for _, verb := range workspaceRefusals {
		t.Run(verb.name, func(t *testing.T) {
			// Act
			got, err := verb.describe(verb.empty)

			// Assert
			if !errors.Is(err, errUndescribedWorkspaceRefusal) || got != "" {
				t.Fatalf("describe = %q, %v; want the undescribed-refusal bug", got, err)
			}
		})
	}
}

// parentNotOpenCases are the parent_not_open cases of EMPTY, one per state
// of the parent, AGAIN being what to do once it is open.
func parentNotOpenCases(empty proto.Message, again string) []refusalCase {
	parent := &workspacev1.WorkspaceRef{Id: "p", Dir: "/w/p"}
	return []refusalCase{
		{name: "parent_not_open/backlogged", err: armed(empty, "parent_not_open", &agentreplv1.WorkspaceParentNotOpen{Parent: parent, State: &agentreplv1.WorkspaceParentNotOpen_Backlogged{Backlogged: &agentreplv1.WorkspaceParentNotOpenBacklogged{}}}),
			want: "the parent /w/p is backlogged; show it, then " + again},
		{name: "parent_not_open/closed", err: armed(empty, "parent_not_open", &agentreplv1.WorkspaceParentNotOpen{Parent: parent, State: &agentreplv1.WorkspaceParentNotOpen_Closed{Closed: &agentreplv1.WorkspaceParentNotOpenClosed{}}}),
			want: "the parent /w/p is closed; reopen it, then " + again},
		{name: "parent_not_open/closing", err: armed(empty, "parent_not_open", &agentreplv1.WorkspaceParentNotOpen{Parent: parent, State: &agentreplv1.WorkspaceParentNotOpen_Closing{Closing: &agentreplv1.WorkspaceParentNotOpenClosing{}}}),
			want: "the parent /w/p closes once its queued merge lands, so it takes no open child"},
	}
}

// openChildrenCase is the has_open_children case of EMPTY, AGAIN being what
// to do once the children are gone.
func openChildrenCase(empty proto.Message, again string) refusalCase {
	return refusalCase{name: "has_open_children", err: armed(empty, "has_open_children", &agentreplv1.WorkspaceHasOpenChildren{
		Children: []*workspacev1.WorkspaceRef{{Id: "c1"}, {Id: "c2"}}, LandingInto: []*workspacev1.WorkspaceRef{{Id: "l1"}}}),
		want: "ws has 2 open children and 1 open workspaces landing into it, backlogged ones included; close or merge each, then " + again}
}

// workspaceSettingRefusals is each `workspace set`, summary, link, find and
// place describer with its cases.
var workspaceSettingRefusals = func() []describedRefusal {
	backlogged := &agentreplv1.SetWorkspaceBackloggedError{}
	priority := &agentreplv1.SetWorkspacePriorityError{}
	name := &agentreplv1.SetWorkspaceNameError{}
	remote := &agentreplv1.UpdateRemoteControlModeError{}
	bypass := &agentreplv1.UpdateQueueBypassModeError{}
	correspondence := &agentreplv1.SetWorkspaceCorrespondenceError{}
	summary := &agentreplv1.SetWorkspaceSummaryError{}
	links := &agentreplv1.ChangeWorkspaceLinksError{}
	find := &agentreplv1.FindWorkspacesError{}
	arrange := &agentreplv1.ArrangeSidebarError{}
	return []describedRefusal{
		{name: "backlogged", empty: backlogged, oneof: "cause",
			cases: append(append(sharedWorkspaceCases(backlogged, "set it again", theFourWorkspaceArms...),
				openChildrenCase(backlogged, "hide it again")),
				parentNotOpenCases(backlogged, "show ws again")...),
			describe: func(e proto.Message) (string, error) {
				return describeBackloggedRefusal("ws", e.(*agentreplv1.SetWorkspaceBackloggedError))
			}},
		{name: "priority", empty: priority, oneof: "cause",
			cases: sharedWorkspaceCases(priority, "set it again", theFourWorkspaceArms...),
			describe: func(e proto.Message) (string, error) {
				return describePriorityRefusal("ws", e.(*agentreplv1.SetWorkspacePriorityError))
			}},
		{name: "name", empty: name, oneof: "cause",
			cases: append(sharedWorkspaceCases(name, "rename it again", theFourWorkspaceArms...),
				refusalCase{name: "invalid_name/slug", err: armed(name, "invalid_name", &agentreplv1.CreateWorkspaceInvalidName{Name: "a/B C", Component: "B C",
					Rule: &agentreplv1.CreateWorkspaceInvalidName_Slug{Slug: &agentreplv1.CreateWorkspaceInvalidNameSlug{}}}),
					want: `the name "a/B C" breaks the naming rule: its last component "B C" is not a slug`},
				refusalCase{name: "invalid_name/prefix", err: armed(name, "invalid_name", &agentreplv1.CreateWorkspaceInvalidName{Name: "A A/b", Component: "A A",
					Rule: &agentreplv1.CreateWorkspaceInvalidName_Prefix{Prefix: &agentreplv1.CreateWorkspaceInvalidNamePrefix{}}}),
					want: `the name "A A/b" breaks the naming rule: its component "A A" before the last is not a prefix`},
				refusalCase{name: "name_taken/workspace", err: armed(name, "name_taken", &agentreplv1.SetWorkspaceNameTaken{Name: "x/y",
					Holder: &agentreplv1.SetWorkspaceNameTaken_Workspace{Workspace: &workspacev1.WorkspaceRef{Dir: "/w/y"}}}),
					want: "the name x/y is already the name or branch of the workspace at /w/y"},
				refusalCase{name: "name_taken/pending", err: armed(name, "name_taken", &agentreplv1.SetWorkspaceNameTaken{Name: "x/y",
					Holder: &agentreplv1.SetWorkspaceNameTaken_Pending{Pending: &agentreplv1.SetWorkspaceNameTakenPending{}}}),
					want: "the name x/y is reserved by a pending async merge or claimed by a create in flight"},
				refusalCase{name: "merging", err: armed(name, "merging", nil),
					want: "a merge holds or names ws, and its branch and directory cannot move until the merge has landed or left the queue"},
				refusalCase{name: "branch_published", err: armed(name, "branch_published", &agentreplv1.SetWorkspaceNameBranchPublished{Upstream: "origin/y"}),
					want: "ws's branch tracks origin/y, and moving it would part it from that remote branch"},
				refusalCase{name: "branch_exists", err: armed(name, "branch_exists", &agentreplv1.SetWorkspaceNameBranchExists{Branch: "x/y"}),
					want: "a branch named x/y already exists that no workspace holds"},
				refusalCase{name: "directory_exists", err: armed(name, "directory_exists", &agentreplv1.SetWorkspaceNameDirectoryExists{Dir: "/w/y"}),
					want: "the directory /w/y already exists"},
				refusalCase{name: "moving", err: armed(name, "moving", nil),
					want: "ws's branch and directory are moving for its previous rename right now; rename it again once that ends"},
			),
			describe: func(e proto.Message) (string, error) {
				return describeNameRefusal("ws", e.(*agentreplv1.SetWorkspaceNameError))
			}},
		{name: "remote control", empty: remote, oneof: "cause",
			cases: append(sharedWorkspaceCases(remote, "set it again", theFourWorkspaceArms...),
				refusalCase{name: "vendor_refused", err: armedWith(remote, "vendor_refused", map[string]any{"detail": "no bridge"}),
					want: "the vendor refused ws's remote-control bridge (no bridge); the mode is still recorded as asked, and the refusal rides its warning strip"}),
			describe: func(e proto.Message) (string, error) {
				return describeRemoteControlRefusal("ws", e.(*agentreplv1.UpdateRemoteControlModeError))
			}},
		{name: "queue bypass", empty: bypass, oneof: "cause",
			cases: append(sharedWorkspaceCases(bypass, "set it again", "unknown_workspace", "workspace_ref_mismatch", "transferring_away"),
				refusalCase{name: "work_account", err: armed(bypass, "work_account", nil),
					want: "ws's session spends as a work account, which never holds the queue bypass mode"},
				refusalCase{name: "held_elsewhere", err: armed(bypass, "held_elsewhere", &agentreplv1.UpdateQueueBypassModeHeldElsewhere{Workspace: &workspacev1.WorkspaceRef{Dir: "/w/other"}}),
					want: "the workspace at /w/other already holds its repository's queue bypass mode"},
				refusalCase{name: "turn_in_flight", err: armed(bypass, "turn_in_flight", nil),
					want: "ws has a turn in flight; end or interrupt it, then set the mode again"},
				refusalCase{name: "switch_failed", err: armedWith(bypass, "switch_failed", map[string]any{"detail": "bind"}),
					want: "ws's conversation switch did not complete (bind)"},
				refusalCase{name: "no_bypass_conversation", err: armed(bypass, "no_bypass_conversation", nil),
					want: "the context named ws's parked bypass conversation, and it parks none; nothing changed"},
				refusalCase{name: "compaction_failed", err: armedWith(bypass, "compaction_failed", map[string]any{"detail": "timeout"}),
					want: "the compaction or summary the context asked for did not complete (timeout); ws holds the mode with its bypass conversation bound, and its next start asks for the compaction again"}),
			describe: func(e proto.Message) (string, error) {
				return describeQueueBypassRefusal("ws", e.(*agentreplv1.UpdateQueueBypassModeError))
			}},
		{name: "correspondence", empty: correspondence, oneof: "cause",
			cases: append(sharedWorkspaceCases(correspondence, "", "unknown_workspace", "workspace_ref_mismatch"),
				refusalCase{name: "closed", err: armed(correspondence, "closed", nil), want: "ws is closed, so it runs no agent to wait"},
				refusalCase{name: "unknown_awaited", err: armed(correspondence, "unknown_awaited", &agentreplv1.SetWorkspaceCorrespondenceUnknownAwaited{Awaited: &workspacev1.WorkspaceRef{Dir: "/w/a"}}),
					want: "the awaited workspace /w/a is not registered, or is ws itself"},
				refusalCase{name: "requester_may_not_set", err: armed(correspondence, "requester_may_not_set", nil),
					want: "the requester is neither ws nor the user, so it may not mark what ws awaits"}),
			describe: func(e proto.Message) (string, error) {
				return describeCorrespondenceRefusal("ws", e.(*agentreplv1.SetWorkspaceCorrespondenceError))
			}},
		{name: "summary", empty: summary, oneof: "cause",
			cases: append(sharedWorkspaceCases(summary, "set the summary again", theFourWorkspaceArms...),
				refusalCase{name: "invalid_summary of the long summary", err: &agentreplv1.SetWorkspaceSummaryError{Cause: &agentreplv1.SetWorkspaceSummaryError_InvalidSummary{InvalidSummary: &agentreplv1.SetWorkspaceSummaryInvalidSummary{
					Part: &agentreplv1.SetWorkspaceSummaryInvalidSummary_LongSummary{LongSummary: &agentreplv1.SetWorkspaceSummaryInvalidLongSummary{LongSummary: "Hi.", Reason: "one sentence"}}}}},
					want: `the long summary "Hi." is not two to four sentences in one paragraph of at most eighty words (one sentence)`},
				refusalCase{name: "invalid_summary of the short summary", err: &agentreplv1.SetWorkspaceSummaryError{Cause: &agentreplv1.SetWorkspaceSummaryError_InvalidSummary{InvalidSummary: &agentreplv1.SetWorkspaceSummaryInvalidSummary{
					Part: &agentreplv1.SetWorkspaceSummaryInvalidSummary_ShortSummary{ShortSummary: &agentreplv1.SetWorkspaceSummaryInvalidShortSummary{ShortSummary: "hi", Reason: "one word"}}}}},
					want: `the short summary "hi" is not one plain sentence of two to fifteen words (one word)`}),
			describe: func(e proto.Message) (string, error) {
				return describeSummaryRefusal("ws", e.(*agentreplv1.SetWorkspaceSummaryError))
			}},
		{name: "links", empty: links, oneof: "cause",
			cases: append(sharedWorkspaceCases(links, "change the links again", theFourWorkspaceArms...),
				refusalCase{name: "invalid_link", err: armedWith(links, "invalid_link", map[string]any{"url": "ftp://x", "reason": "not http"}),
					want: "ftp://x is not a link the browser opens (not http)"},
				refusalCase{name: "invalid_label", err: armedWith(links, "invalid_label", map[string]any{"label": "x", "reason": "one word"}),
					want: `the label "x" is not two to eight words in one sentence (one word)`},
				refusalCase{name: "unknown_link", err: armedWith(links, "unknown_link", map[string]any{"url": "https://x"}),
					want: "ws holds no link https://x"},
				refusalCase{name: "too_many_shown", err: armedWith(links, "too_many_shown", map[string]any{"named": uint32(6), "limit": uint32(5)}),
					want: "the menu shows at most 5 links, and 6 were named"},
				refusalCase{name: "unknown_announced_link", err: armedWith(links, "unknown_announced_link", map[string]any{"url": "https://pr"}),
					want: "the link announces https://pr, which ws does not hold"},
				refusalCase{name: "pull_request_owned_elsewhere", err: armed(links, "pull_request_owned_elsewhere", &agentreplv1.ChangeWorkspaceLinksPullRequestOwnedElsewhere{
					Url: "https://pr", OwnerName: "other", Owner: &workspacev1.WorkspaceRef{Dir: "/w/other"}}),
					want: "the pull request https://pr is already owned by other at /w/other, and a pull request has one owning workspace"},
				refusalCase{name: "subpage_of_owned_link", err: armed(links, "subpage_of_owned_link", &agentreplv1.ChangeWorkspaceLinksSubpageOfOwnedLink{
					Url: "https://gns/key/a.b", OwnedPageUrl: "https://gns/key/a"}),
					want: "https://gns/key/a.b is a subpage of the GNS page https://gns/key/a, which ws already owns: store it as a reference, or leave it out"},
				refusalCase{name: "owned_link_unshown", err: armed(links, "owned_link_unshown", &agentreplv1.ChangeWorkspaceLinksOwnedLinkUnshown{
					OwnedUrl: "https://o", ReferenceUrl: "https://r"}),
					want: "the show leaves the owned link https://o out while it names the reference https://r: show every owned link first"}),
			describe: func(e proto.Message) (string, error) {
				return describeLinksRefusal("ws", e.(*agentreplv1.ChangeWorkspaceLinksError))
			}},
		{name: "find", empty: find, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_repository", err: armed(find, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"},
				{name: "unknown_creator", err: armed(find, "unknown_creator", nil), want: "the daemon has no workspace by the one -created-by named"},
				{name: "unknown_parent", err: armed(find, "unknown_parent", nil), want: "the daemon has no workspace by the one -parent named"},
				{name: "unknown_caller", err: armed(find, "unknown_caller", nil), want: "the daemon has no workspace for the calling directory"},
				{name: "unknown_login_workspace", err: armed(find, "unknown_login_workspace", nil), want: "the daemon has no workspace by the one -login named"},
				{name: "unknown_login_root", err: armed(find, "unknown_login_root", &agentreplv1.FindWorkspacesUnknownLoginRoot{Known: []string{"/home/u/.claude", "/home/u/.claude-chesscom"}}),
					want: "the -login-root root is not an account root the daemon knows; it knows /home/u/.claude, /home/u/.claude-chesscom"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeFindRefusal(e.(*agentreplv1.FindWorkspacesError))
			}},
		{name: "arrange", empty: arrange, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_repository", err: armed(arrange, "unknown_repository", nil), want: "the daemon has no repository by the one the change named"},
				{name: "unknown_subsection", err: armed(arrange, "unknown_subsection", &agentreplv1.ArrangeSidebarUnknownSubsection{Subsection: &agentreplv1.SubsectionRef{Id: "s1"}}),
					want: "the repository has no subsection s1"},
				{name: "unknown_workspace", err: armed(arrange, "unknown_workspace", &agentreplv1.ArrangeSidebarUnknownWorkspace{Workspace: &workspacev1.WorkspaceRef{Dir: "/w/ws"}}),
					want: "the daemon has no workspace /w/ws"},
				{name: "workspace_ref_mismatch", err: armed(arrange, "workspace_ref_mismatch", &agentreplv1.ArrangeSidebarWorkspaceRefMismatch{Workspace: &workspacev1.WorkspaceRef{Id: "ws-1"}, RegistryDir: "/w/ws"}),
					want: "ws-1 lives at /w/ws, not at the directory the change named"},
				{name: "transferring_away", err: armedWith(arrange, "transferring_away", map[string]any{"address": "127.0.0.1:9"}),
					want: "the daemon is restarting and handed a workspace the change named to its successor at 127.0.0.1:9; place it again"},
				{name: "not_yet_adopted", err: armed(arrange, "not_yet_adopted", nil),
					want: "the daemon is restarting and has not taken a workspace the change named over yet; place it again once the restart finishes"},
				{name: "blank_name", err: armed(arrange, "blank_name", nil), want: "a subsection's name is blank"},
				{name: "other_repository", err: armed(arrange, "other_repository", nil), want: "the change relates workspaces or subsections of different repositories"},
				{name: "anchor_elsewhere", err: armed(arrange, "anchor_elsewhere", nil),
					want: "what the change placed things before moved since the sidebar was read; read it again and place them again"},
				{name: "no_open_workspace", err: armed(arrange, "no_open_workspace", nil),
					want: "the new subsection would hold no open workspace, so the change that created it would delete it"},
				{name: "main_workspace_not_sortable", err: armed(arrange, "main_workspace_not_sortable", nil),
					want: "the change would move a repository's main workspace, or the family it roots, which agents never sort"},
				{name: "not_a_child", err: armed(arrange, "not_a_child", nil),
					want: "the workspace is top-level, so it has no parent whose children it could be reordered among"},
				{name: "not_siblings", err: armed(arrange, "not_siblings", nil),
					want: "the workspace to move before is not a child of the moved child's parent"},
				{name: "no_repository_block", err: armed(arrange, "no_repository_block", nil), want: "the change named a repository block the parent does not have"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeArrangeRefusal(e.(*agentreplv1.ArrangeSidebarError))
			}},
	}
}()

func TestDescribeWorkspaceSettingRefusals(t *testing.T) {
	testDescribedRefusals(t, workspaceSettingRefusals)
}
