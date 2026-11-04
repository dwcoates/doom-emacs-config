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
	{name: "owner_lands", err: &agentreplv1.FinishWorkspaceError{Cause: &agentreplv1.FinishWorkspaceError_OwnerLands{OwnerLands: &agentreplv1.FinishWorkspaceOwnerLands{Owner: &workspacev1.WorkspaceRef{Id: "o1", Dir: "/w/owner"}}}},
		want: "ws is landed by its owner /w/owner, not by itself"},
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
var workspaceRefusals = []struct {
	name     string
	cases    []refusalCase
	empty    proto.Message
	describe func(proto.Message) (string, error)
}{
	{name: "finish", cases: finishRefusalCases, empty: &agentreplv1.FinishWorkspaceError{},
		describe: func(e proto.Message) (string, error) { return describeFinishRefusal("ws", e.(*agentreplv1.FinishWorkspaceError)) }},
	{name: "one-shot", cases: oneShotRefusalCases, empty: &agentreplv1.SetWorkspaceOneShotError{},
		describe: func(e proto.Message) (string, error) {
			return describeOneShotRefusal("ws", e.(*agentreplv1.SetWorkspaceOneShotError))
		}},
	{name: "retarget", cases: retargetRefusalCases, empty: &agentreplv1.SetWorkspaceParentError{},
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
