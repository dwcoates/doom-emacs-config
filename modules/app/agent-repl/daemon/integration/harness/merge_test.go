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
		s.Relationship = Child(&workspacev1.WorkspaceRef{Id: "owner"})
	}))

	// Assert
	if req.GetRequester().GetUser() == nil || req.GetContextOrigin().GetFresh() == nil || req.GetStandard().GetRelationship().GetChild().GetParent().GetId() != "owner" {
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
