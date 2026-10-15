package harness

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

// merge.go holds the merge-request shapes and the landing wait that the daemon
// integration suite and the cross-system e2e suite share, so the two cannot
// drift on what a merge request names or on what "the landing deployed" means.

// MergeMode sets a MergeWorkspace request's mode.
type MergeMode func(*agentreplv1.MergeWorkspaceRequest)

// MergeRequest is a MergeWorkspace request from WS in MODE.
func MergeRequest(ws *workspacev1.WorkspaceRef, mode MergeMode) *agentreplv1.MergeWorkspaceRequest {
	req := &agentreplv1.MergeWorkspaceRequest{Workspace: ws}
	mode(req)
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

// Async is the ASYNC mode: BRANCH, merged in a hidden merge workspace named
// NAME, shaped further by EDITS (an instruction, a workspace to close, ...).
func Async(name, branch string, edits ...func(*agentreplv1.MergeWorkspaceAsync)) MergeMode {
	return func(req *agentreplv1.MergeWorkspaceRequest) {
		async := &agentreplv1.MergeWorkspaceAsync{Name: name, Branch: branch}
		for _, edit := range edits {
			edit(async)
		}
		req.Mode = &agentreplv1.MergeWorkspaceRequest_Async{Async: async}
	}
}

// WithInstruction gives an async merge its resolving instruction.
func WithInstruction(instruction string) func(*agentreplv1.MergeWorkspaceAsync) {
	return func(async *agentreplv1.MergeWorkspaceAsync) { async.Instruction = &instruction }
}

// ClosingOnLanding names the workspace an async merge closes once it lands.
func ClosingOnLanding(ws *workspacev1.WorkspaceRef) func(*agentreplv1.MergeWorkspaceAsync) {
	return func(async *agentreplv1.MergeWorkspaceAsync) {
		async.OneShot = &agentreplv1.MergeWorkspaceAsyncOneShot{Workspace: ws}
	}
}

// CleaningUp asks an async merge to remove the branch's worktree once it lands.
func CleaningUp() func(*agentreplv1.MergeWorkspaceAsync) {
	return func(async *agentreplv1.MergeWorkspaceAsync) { async.CleanupWorktree = true }
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
