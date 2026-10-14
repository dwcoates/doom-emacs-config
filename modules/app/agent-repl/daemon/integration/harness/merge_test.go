package harness

import (
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestOneShotMergesTheRequestersOwnBranch(t *testing.T) {
	// Act
	req := MergeRequest(&workspacev1.WorkspaceRef{Id: "ws"}, OneShot())

	// Assert
	if req.GetWorkspace().GetId() != "ws" || req.GetOneShot().GetOwnBranch() == nil {
		t.Fatalf("MergeRequest(OneShot) = %v, want the one-shot own-branch arm for ws", req)
	}
}

func TestOneShotMergedUpstreamNamesTheUpstreamLanding(t *testing.T) {
	// Act
	req := MergeRequest(&workspacev1.WorkspaceRef{Id: "ws"}, OneShotMergedUpstream())

	// Assert
	if req.GetOneShot().GetMergedUpstream() == nil {
		t.Fatalf("MergeRequest(OneShotMergedUpstream) = %v, want the merged-upstream landing", req)
	}
}

func TestAsyncNamesItsBranchAndCarriesItsEdits(t *testing.T) {
	// Arrange
	child := &workspacev1.WorkspaceRef{Id: "child"}

	// Act
	req := MergeRequest(&workspacev1.WorkspaceRef{Id: "ws"}, Async("fix", "agent-fix", WithInstruction("rebase it"), ClosingOnLanding(child), CleaningUp()))

	// Assert
	async := req.GetAsync()
	if async.GetName() != "fix" || async.GetBranch() != "agent-fix" || async.GetInstruction() != "rebase it" ||
		async.GetOneShot().GetWorkspace().GetId() != "child" || !async.GetCleanupWorktree() {
		t.Fatalf("MergeRequest(Async) = %v, want every edit applied", req)
	}
}

func TestIsLandingDeployedRecognizesOnlyTheLandingsDoneRecord(t *testing.T) {
	cases := []struct {
		name   string
		record LogRecord
		want   bool
	}{
		{name: "the landing's done record", record: LogRecord{Operation: "daemon.deploy.landing", Message: "the landing is deployed"}, want: true},
		{name: "another record of the landing's deploy", record: LogRecord{Operation: "daemon.deploy.landing", Message: "deploying the landing"}},
		{name: "the same words under another operation", record: LogRecord{Operation: "daemon.deploy.run", Message: "the landing is deployed"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act, Assert
			if got := IsLandingDeployed(tc.record); got != tc.want {
				t.Fatalf("IsLandingDeployed(%+v) = %v, want %v", tc.record, got, tc.want)
			}
		})
	}
}
