package harness

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestFinishRequestNamesTheWorkspace(t *testing.T) {
	// Act
	req := FinishRequest(&workspacev1.WorkspaceRef{Id: "ws"})

	// Assert
	if req.GetWorkspace().GetId() != "ws" {
		t.Fatalf("FinishRequest = %v, want ws", req)
	}
}

func TestAsyncMergeNamesItsBranchAndMakerAndCarriesItsEdits(t *testing.T) {
	// Arrange
	child := &workspacev1.WorkspaceRef{Id: "child"}
	ws := &workspacev1.WorkspaceRef{Id: "ws"}

	// Act
	req := AsyncMerge(&workspacev1.RepositoryRef{Id: "repo"}, ws, "fix", "agent-fix", WithInstruction("rebase it"), ClosingOnLanding(child), CleaningUp(), IntoMaker(), AsyncAtFront())

	// Assert
	async := req.GetAsyncMerge()
	if async.GetName() != "fix" || async.GetSource().GetBranch() != "agent-fix" || async.GetInstruction() != "rebase it" ||
		async.GetCloseAfter().GetWorkspace().GetId() != "child" || !async.GetCleanupWorktree() || async.GetTarget().GetMaker() == nil || async.GetPlacement().GetFront() == nil {
		t.Fatalf("AsyncMerge = %v, want every edit applied", req)
	}
	if async.GetSource().GetMaker().GetWorkspace().GetWorkspace().GetId() != "ws" || req.GetContextOrigin().GetFork().GetWorkspace().GetId() != "ws" ||
		req.GetRequester().GetWorkspace().GetWorkspace().GetId() != "ws" {
		t.Fatalf("AsyncMerge = %v, want ws as maker, fork and requester", req)
	}
}

func TestAsyncPatchMovesTheVersionByAPatch(t *testing.T) {
	// Act
	req := AsyncMerge(&workspacev1.RepositoryRef{Id: "repo"}, &workspacev1.WorkspaceRef{Id: "ws"}, "fix", "agent-fix", AsyncPatch("Fix it"))

	// Assert
	v := req.GetAsyncMerge().GetVersionIncrement()
	if v.GetPatch() == nil || v.GetChangelogLine() != "Fix it" {
		t.Fatalf("version increment = %v, want a patch with its line", v)
	}
}

func TestFinishRequestIsTheUsersAtTheBackUnlessEdited(t *testing.T) {
	// Act
	plain := FinishRequest(&workspacev1.WorkspaceRef{Id: "ws"})
	front := FinishRequest(&workspacev1.WorkspaceRef{Id: "ws"}, AtFront())

	// Assert
	if plain.GetRequester().GetUser() == nil || plain.GetPlacement().GetBack() == nil || front.GetPlacement().GetFront() == nil {
		t.Fatalf("plain %v, front %v; want the user's at the back, and at the front", plain, front)
	}
}

func TestCreateIsTheUsersFreshPrimaryStandardUnlessEdited(t *testing.T) {
	// Act
	req := Create(&workspacev1.RepositoryRef{Id: "repo"}, Standard(func(s *agentreplv1.CreateWorkspaceStandard) {
		s.Topology = ChildOf(&workspacev1.WorkspaceRef{Id: "owner"}, "")
	}))

	// Assert
	if req.GetRequester().GetUser() == nil || req.GetContextOrigin().GetFresh() == nil || req.GetStandard().GetTopology().GetChild().GetOwner().GetId() != "owner" {
		t.Fatalf("Create = %v, want the user's fresh create edited into a child", req)
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

func TestPreAndPostDeployPromptsFillTheirOwnStage(t *testing.T) {
	// Act
	pre := PreDeployPrompts("lint", "check")
	post := PostDeployPrompts("announce")

	// Assert
	if len(pre.GetPreDeploy()) != 2 || pre.GetPreDeploy()[1].GetPrompt().GetText() != "check" || len(pre.GetPostDeploy()) != 0 {
		t.Fatalf("PreDeployPrompts = %v, want both prompts in order, pre-deploy alone", pre)
	}
	if len(post.GetPostDeploy()) != 1 || post.GetPostDeploy()[0].GetPrompt().GetText() != "announce" || len(post.GetPreDeploy()) != 0 {
		t.Fatalf("PostDeployPrompts = %v, want the prompt, post-deploy alone", post)
	}
}
