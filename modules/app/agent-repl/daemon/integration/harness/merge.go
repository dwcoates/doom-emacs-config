package harness

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/deployconfig"
)

// merge.go holds the merge-request shapes and the landing wait that the daemon
// integration suite and the cross-system e2e suite share, so the two cannot
// drift on what a merge request names or on what "the landing deployed" means.

// The agent API's required choices, for the suites' requests. A suite's
// request names each choice it makes; these are the spellings.

// User is the user asking.
func User() *agentreplv1.WorkspaceRequester {
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_User{User: &agentreplv1.WorkspaceRequesterUser{}}}
}

// AskedBy is WS's agent asking.
func AskedBy(ws *workspacev1.WorkspaceRef) *agentreplv1.WorkspaceRequester {
	return &agentreplv1.WorkspaceRequester{Asker: &agentreplv1.WorkspaceRequester_Workspace{Workspace: &agentreplv1.WorkspaceRequesterWorkspace{Workspace: ws}}}
}

// Fresh is a fresh conversation.
func Fresh() *agentreplv1.WorkspaceContextOrigin {
	return &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fresh{Fresh: &agentreplv1.WorkspaceContextOriginFresh{}}}
}

// Fork is a fork of WS's conversation.
func Fork(ws *workspacev1.WorkspaceRef) *agentreplv1.WorkspaceContextOrigin {
	return &agentreplv1.WorkspaceContextOrigin{Origin: &agentreplv1.WorkspaceContextOrigin_Fork{Fork: &agentreplv1.WorkspaceContextOriginFork{Workspace: ws}}}
}

// Peer is a workspace standing on its own, landing by default on the primary
// branch.
func Peer() *agentreplv1.WorkspaceRelationship {
	return &agentreplv1.WorkspaceRelationship{Kind: &agentreplv1.WorkspaceRelationship_Peer{Peer: &agentreplv1.WorkspaceRelationshipPeer{}}}
}

// Child is a child of PARENT, landing by default into PARENT's branch when the
// two share a repository.
func Child(parent *workspacev1.WorkspaceRef) *agentreplv1.WorkspaceRelationship {
	return &agentreplv1.WorkspaceRelationship{Kind: &agentreplv1.WorkspaceRelationship_Child{Child: &agentreplv1.WorkspaceRelationshipChild{Parent: parent}}}
}

// Notice is the owner notice TEXT, nil for none (a child that forks its
// owner, or a peer).
func Notice(text string) *agentreplv1.WorkspaceOwnerNotice {
	if text == "" {
		return nil
	}
	return &agentreplv1.WorkspaceOwnerNotice{Text: text}
}

// LandsInto is the landing target into WS's branch.
func LandsInto(ws *workspacev1.WorkspaceRef) *agentreplv1.WorkspaceLandingTarget {
	return &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Workspace{Workspace: &agentreplv1.WorkspaceLandingTargetWorkspace{Workspace: ws}}}
}

// LandsOnPrimary is the landing target on the primary branch.
func LandsOnPrimary() *agentreplv1.WorkspaceLandingTarget {
	return &agentreplv1.WorkspaceLandingTarget{Target: &agentreplv1.WorkspaceLandingTarget_Primary{Primary: &agentreplv1.WorkspaceLandingTargetPrimary{}}}
}

// Back and Front are a merge's placements.
func Back() *agentreplv1.MergeQueuePlacement {
	return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Back{Back: &agentreplv1.MergeQueuePlacementBack{}}}
}

// Front is the front of the queue.
func Front() *agentreplv1.MergeQueuePlacement {
	return &agentreplv1.MergeQueuePlacement{Place: &agentreplv1.MergeQueuePlacement_Front{Front: &agentreplv1.MergeQueuePlacementFront{}}}
}

// CreateEdit shapes a create request.
type CreateEdit func(*agentreplv1.CreateWorkspaceRequest)

// Create is a standard create of a peer in REPOSITORY asked by the user, with
// a fresh conversation, landing on the primary branch, shaped by EDITS.
func Create(repository *workspacev1.RepositoryRef, edits ...CreateEdit) *agentreplv1.CreateWorkspaceRequest {
	req := &agentreplv1.CreateWorkspaceRequest{
		Repository:    repository,
		Form:          &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Relationship: Peer()}},
		Requester:     User(),
		ContextOrigin: Fresh(),
	}
	for _, edit := range edits {
		edit(req)
	}
	return req
}

// Standard edits the standard form.
func Standard(edit func(*agentreplv1.CreateWorkspaceStandard)) CreateEdit {
	return func(req *agentreplv1.CreateWorkspaceRequest) { edit(req.GetStandard()) }
}

// FinishEdit changes a FinishWorkspace request.
type FinishEdit func(*agentreplv1.FinishWorkspaceRequest)

// FinishRequest is a FinishWorkspace request of WS's own work, asked by the
// user at the back of the queue; EDITS change it.
func FinishRequest(ws *workspacev1.WorkspaceRef, edits ...FinishEdit) *agentreplv1.FinishWorkspaceRequest {
	req := &agentreplv1.FinishWorkspaceRequest{Workspace: ws, Requester: User(), Placement: Back()}
	for _, edit := range edits {
		edit(req)
	}
	return req
}

// AtFront places a deploy at the front of its queue.
func AtFront() FinishEdit {
	return func(req *agentreplv1.FinishWorkspaceRequest) { req.Placement = Front() }
}

// AsyncMerge is an async merge in REPOSITORY of BRANCH, asked by REQUESTER's
// agent, which made the branch: its conversation is forked, the branch lands
// on the primary branch from the back of the queue, in a merge workspace
// named NAME, shaped further by EDITS.
func AsyncMerge(repository *workspacev1.RepositoryRef, requester *workspacev1.WorkspaceRef, name, branch string, edits ...func(*agentreplv1.CreateWorkspaceAsyncMerge)) *agentreplv1.CreateWorkspaceRequest {
	async := &agentreplv1.CreateWorkspaceAsyncMerge{
		Name: name,
		Source: &agentreplv1.CreateWorkspaceAsyncMergeSource{Branch: branch, Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker{
			Maker: &agentreplv1.CreateWorkspaceAsyncMergeMaker_Workspace{Workspace: &agentreplv1.CreateWorkspaceAsyncMergeMakerWorkspace{Workspace: requester}}}},
		Target:    &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Primary{Primary: &agentreplv1.CreateWorkspaceAsyncMergeTargetPrimary{}}},
		Placement: Back(),
	}
	for _, edit := range edits {
		edit(async)
	}
	return &agentreplv1.CreateWorkspaceRequest{Repository: repository, Requester: AskedBy(requester), ContextOrigin: Fork(requester),
		Form: &agentreplv1.CreateWorkspaceRequest_AsyncMerge{AsyncMerge: async}}
}

// WithInstruction gives an async merge its resolving instruction.
func WithInstruction(instruction string) func(*agentreplv1.CreateWorkspaceAsyncMerge) {
	return func(async *agentreplv1.CreateWorkspaceAsyncMerge) { async.Instruction = &instruction }
}

// ClosingOnLanding names the workspace an async merge closes once it lands.
func ClosingOnLanding(ws *workspacev1.WorkspaceRef) func(*agentreplv1.CreateWorkspaceAsyncMerge) {
	return func(async *agentreplv1.CreateWorkspaceAsyncMerge) {
		async.CloseAfter = &agentreplv1.CreateWorkspaceAsyncMergeCloseAfter{Workspace: ws}
	}
}

// CleaningUp asks an async merge to remove the branch's worktree once it lands.
func CleaningUp() func(*agentreplv1.CreateWorkspaceAsyncMerge) {
	return func(async *agentreplv1.CreateWorkspaceAsyncMerge) { async.CleanupWorktree = true }
}

// IntoMaker lands an async merge into its maker's branch.
func IntoMaker() func(*agentreplv1.CreateWorkspaceAsyncMerge) {
	return func(async *agentreplv1.CreateWorkspaceAsyncMerge) {
		async.Target = &agentreplv1.CreateWorkspaceAsyncMergeTarget{Target: &agentreplv1.CreateWorkspaceAsyncMergeTarget_Maker{Maker: &agentreplv1.CreateWorkspaceAsyncMergeTargetMaker{}}}
	}
}

// AsyncPatch has an async merge move agent-repl's version by a patch, with
// LINE as the tag's changelog line: what an agent's merge onto agent-repl's
// own master must carry.
func AsyncPatch(line string) func(*agentreplv1.CreateWorkspaceAsyncMerge) {
	return func(async *agentreplv1.CreateWorkspaceAsyncMerge) {
		async.VersionIncrement = &agentreplv1.MergeWorkspaceVersionIncrement{ChangelogLine: line,
			Increment: &agentreplv1.MergeWorkspaceVersionIncrement_Patch{Patch: &agentreplv1.MergeWorkspaceVersionIncrementPatch{}}}
	}
}

// AsyncAtFront places an async merge at the front of its queue.
func AsyncAtFront() func(*agentreplv1.CreateWorkspaceAsyncMerge) {
	return func(async *agentreplv1.CreateWorkspaceAsyncMerge) { async.Placement = Front() }
}

// IsLandingDeployed reports whether a run-log record is the ONE deploy a
// landing in the daemon's own checkout runs, reported done.
func IsLandingDeployed(r LogRecord) bool {
	return r.Operation == "daemon.deploy.landing" && r.Message == "the landing is deployed"
}

// AwaitLandingDeployed waits for the landing's deploy. A landing carrying work
// always deploys, and a test that ends mid-build kills the build under the
// deploy and reads the kill as a failed deploy.
//
// THE BOUND IS MergeChainTimeout, whatever the daemon's own wait bound: the
// deploy is a chain of subprocesses (the build, then one install per
// artifact), like the merge it follows. Bounded by one DefaultTimeout wait, a
// landing's deploy under a full gate was still installing at +4.4s when the
// wait cut it (2026-10-14).
func (d *Daemon) AwaitLandingDeployed() {
	d.t.Helper()
	d.AwaitLogRecordWithin(d.RunLogPath(), "the landing's deploy", MergeChainTimeout, IsLandingDeployed)
}

// PromptHooks are TEXTS as prompt hooks, in order.
func PromptHooks(texts ...string) []*repoconfigv1.DeployHook {
	var out []*repoconfigv1.DeployHook
	for _, text := range texts {
		out = append(out, deployconfig.PromptHook(text))
	}
	return out
}

// PreDeployPrompts are a request's deploy hooks: TEXTS as its pre-deploy
// prompts, in place of the repository's.
func PreDeployPrompts(texts ...string) *agentreplv1.CreateWorkspaceDeployHooks {
	return &agentreplv1.CreateWorkspaceDeployHooks{PreDeploy: PromptHooks(texts...)}
}

// PostDeployPrompts are a request's deploy hooks: TEXTS as its post-deploy
// prompts, in place of the repository's.
func PostDeployPrompts(texts ...string) *agentreplv1.CreateWorkspaceDeployHooks {
	return &agentreplv1.CreateWorkspaceDeployHooks{PostDeploy: PromptHooks(texts...)}
}
