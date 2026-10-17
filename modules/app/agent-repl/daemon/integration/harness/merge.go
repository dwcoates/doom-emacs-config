package harness

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
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

// Primary is cut from and landing on the primary branch.
func Primary() *agentreplv1.WorkspaceTopology {
	return &agentreplv1.WorkspaceTopology{Shape: &agentreplv1.WorkspaceTopology_Primary{Primary: &agentreplv1.WorkspaceTopologyPrimary{}}}
}

// ChildOf is cut from OWNER's branch and landing into it, carrying NOTICE to
// an owner that lands it (empty for a child that forks its owner).
func ChildOf(owner *workspacev1.WorkspaceRef, notice string) *agentreplv1.WorkspaceTopology {
	child := &agentreplv1.WorkspaceTopologyChild{Owner: owner}
	if notice != "" {
		child.OwnerNotice = &agentreplv1.WorkspaceTopologyChildOwnerNotice{Text: notice}
	}
	return &agentreplv1.WorkspaceTopology{Shape: &agentreplv1.WorkspaceTopology_Child{Child: child}}
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

// Create is a standard create in REPOSITORY asked by the user, with a fresh
// conversation, cut from the primary branch, shaped by EDITS.
func Create(repository *workspacev1.RepositoryRef, edits ...CreateEdit) *agentreplv1.CreateWorkspaceRequest {
	req := &agentreplv1.CreateWorkspaceRequest{
		Repository:    repository,
		Form:          &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{Topology: Primary()}},
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

// MergeMode sets a MergeWorkspace request's mode.
type MergeMode func(*agentreplv1.MergeWorkspaceRequest)

// MergeRequest is a MergeWorkspace request of WS's own branch in MODE, asked
// by the user at the back of the queue; EDITS change either.
func MergeRequest(ws *workspacev1.WorkspaceRef, mode MergeMode, edits ...MergeMode) *agentreplv1.MergeWorkspaceRequest {
	req := &agentreplv1.MergeWorkspaceRequest{Workspace: ws, Requester: User(), Placement: Back()}
	mode(req)
	for _, edit := range edits {
		edit(req)
	}
	return req
}

// OneShot is the SYNC mode: the requester's own branch, merged in place,
// closing the requester once it lands.
func OneShot() MergeMode {
	return func(req *agentreplv1.MergeWorkspaceRequest) {
		req.Mode = &agentreplv1.MergeWorkspaceRequest_OneShot{OneShot: &agentreplv1.MergeWorkspaceOneShot{
			Landing: &agentreplv1.MergeWorkspaceOneShot_OwnBranch{OwnBranch: &agentreplv1.MergeWorkspaceOneShotOwnBranch{}}}}
	}
}

// OneShotMergedUpstream is the one-shot whose branch already merged upstream.
func OneShotMergedUpstream() MergeMode {
	return func(req *agentreplv1.MergeWorkspaceRequest) {
		req.Mode = &agentreplv1.MergeWorkspaceRequest_OneShot{OneShot: &agentreplv1.MergeWorkspaceOneShot{
			Landing: &agentreplv1.MergeWorkspaceOneShot_MergedUpstream{MergedUpstream: &agentreplv1.MergeWorkspaceOneShotMergedUpstream{}}}}
	}
}

// AtFront places a merge at the front of its queue.
func AtFront() MergeMode {
	return func(req *agentreplv1.MergeWorkspaceRequest) { req.Placement = Front() }
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
