package harness

import (
	agentreplv1 "agentrepl/proto/agentrepl/v1"
)

// merge.go holds the merge-request shapes and the landing wait that the daemon
// integration suite and the cross-system e2e suite share, so the two cannot
// drift on what a merge request names or on what "the landing deployed" means.

// OwnBranch is a MergeWorkspace source naming the requester's own branch, run
// IN PLACE. With keepOpen false the workspace closes once its branch lands.
func OwnBranch(keepOpen bool) *agentreplv1.MergeWorkspaceSource {
	return &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_OwnBranch{
		OwnBranch: &agentreplv1.MergeWorkspaceSourceOwnBranch{Run: &agentreplv1.MergeWorkspaceSourceOwnBranch_InPlace{
			InPlace: &agentreplv1.MergeWorkspaceSourceOwnBranchInPlace{KeepOpen: keepOpen}}}}}
}

// OwnBranchAsync is a MergeWorkspace source naming the requester's whole own
// branch, run ASYNC in a hidden merge workspace named NAME.
func OwnBranchAsync(name string) *agentreplv1.MergeWorkspaceSource {
	return ownBranchDetached(&agentreplv1.MergeWorkspaceSourceOwnBranchDetached{Name: name,
		Content: &agentreplv1.MergeWorkspaceSourceOwnBranchDetached_WholeBranch{WholeBranch: &agentreplv1.MergeWorkspaceSourceOwnBranchDetachedWholeBranch{}}})
}

// OwnBranchPick is a MergeWorkspace source naming only the changes of the
// requester's own branch INSTRUCTION names, run ASYNC in a hidden merge
// workspace named NAME.
func OwnBranchPick(name, instruction string) *agentreplv1.MergeWorkspaceSource {
	return ownBranchDetached(&agentreplv1.MergeWorkspaceSourceOwnBranchDetached{Name: name,
		Content: &agentreplv1.MergeWorkspaceSourceOwnBranchDetached_Pick{Pick: &agentreplv1.MergeWorkspaceSourceOwnBranchDetachedPick{Instruction: instruction}}})
}

// ownBranchDetached wraps an async run as an own-branch source.
func ownBranchDetached(run *agentreplv1.MergeWorkspaceSourceOwnBranchDetached) *agentreplv1.MergeWorkspaceSource {
	return &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_OwnBranch{
		OwnBranch: &agentreplv1.MergeWorkspaceSourceOwnBranch{Run: &agentreplv1.MergeWorkspaceSourceOwnBranch_Detached{Detached: run}}}}
}

// BranchSource is a MergeWorkspace source naming a branch that is no
// workspace's.
func BranchSource(name string) *agentreplv1.MergeWorkspaceSource {
	return &agentreplv1.MergeWorkspaceSource{Source: &agentreplv1.MergeWorkspaceSource_Branch{
		Branch: &agentreplv1.MergeWorkspaceSourceBranch{Name: name}}}
}

// IsLandingDeployed reports whether a run-log record is the ONE deploy a
// landing in the daemon's own checkout runs, reported done.
func IsLandingDeployed(r LogRecord) bool {
	return r.Operation == "daemon.deploy.landing" && r.Message == "the landing is deployed"
}

// AwaitLandingDeployed waits for the landing's deploy. A landing carrying work
// always deploys, and a test that ends mid-build kills the build under the
// deploy and reads the kill as a failed deploy.
func (d *Daemon) AwaitLandingDeployed() {
	d.t.Helper()
	d.AwaitLogRecord(d.RunLogPath(), "the landing's deploy", IsLandingDeployed)
}
