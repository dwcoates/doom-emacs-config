package server

import (
	"context"
	"regexp"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/sourcescan"
)

func TestResolveAllNamingResolvesEveryRefInOrder(t *testing.T) {
	// Arrange.
	h, target := swapHarness(t)
	s := h.Server.(*server)

	// Act.
	subjects, refused, done := s.resolveAllNaming(context.Background(), "MoveWorkspace",
		&agentreplv1.MoveWorkspaceResponse{}, []*workspacev1.WorkspaceRef{target, ref()})

	// Assert.
	if done || refused != nil {
		t.Fatalf("resolveAllNaming = (%v, %v), want no refusal", refused, done)
	}
	if len(subjects) != 2 || subjects[0].Record.ID != "ws-2" || subjects[1].Record.ID != testWorkspaceID {
		t.Fatalf("subjects = %+v, want ws-2 then %s", subjects, testWorkspaceID)
	}
}

func TestResolveAllNamingAnswersTheFirstRefusalNamingItsRef(t *testing.T) {
	// Arrange.
	h, _ := swapHarness(t)
	s := h.Server.(*server)
	resp := &agentreplv1.MoveWorkspaceResponse{}
	missing := &workspacev1.WorkspaceRef{Id: "ws-nope"}

	// Act.
	subjects, refused, done := s.resolveAllNaming(context.Background(), "MoveWorkspace", resp,
		[]*workspacev1.WorkspaceRef{missing, &workspacev1.WorkspaceRef{Id: "ws-also-nope"}})

	// Assert.
	if !done || refused != nil || subjects != nil {
		t.Fatalf("resolveAllNaming = (%v, %v, %v), want done with the refusal encoded on the response", subjects, refused, done)
	}
	if got := resp.GetError().GetUnknownWorkspace().GetWorkspace().GetId(); got != "ws-nope" {
		t.Fatalf("refused ref = %q, want the first unknown ref ws-nope", got)
	}
}

func TestValidateWorkspacePairRefusesEachMalformedPair(t *testing.T) {
	tests := []struct {
		name              string
		workspace, target *workspacev1.WorkspaceRef
		field             string
	}{
		{name: "no workspace", target: &workspacev1.WorkspaceRef{Id: "ws-2"}, field: "workspace"},
		{name: "no target", workspace: ref(), field: "target"},
		{name: "one workspace twice", workspace: ref(), target: ref(), field: "target"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			err := validateWorkspacePair(tt.workspace, tt.target, "named twice")

			// Assert.
			if err == nil || !regexp.MustCompile(regexp.QuoteMeta(tt.field)).MatchString(err.Message()) {
				t.Fatalf("validateWorkspacePair = %v, want a refusal of %q", err, tt.field)
			}
		})
	}
}

func TestValidateWorkspacePairAcceptsTwoWorkspaces(t *testing.T) {
	// Act.
	err := validateWorkspacePair(ref(), &workspacev1.WorkspaceRef{Id: "ws-2"}, "named twice")

	// Assert.
	if err != nil {
		t.Fatalf("validateWorkspacePair = %v, want nil", err)
	}
}

func TestEveryRefListIsResolvedThroughResolveAllNaming(t *testing.T) {
	// Arrange: a loop over refs calling resolveNaming is resolveAllNaming
	// written again.
	handRolled := regexp.MustCompile(`for _, ref := range [^\n]*\n[^\n]*resolveNaming\(`)
	var offenders []string

	// Act.
	for _, file := range sourcescan.Production(t) {
		if handRolled.Match(file.Source) && file.Name != "workspaces.go" {
			offenders = append(offenders, file.Name)
		}
	}
	occurrences := 0
	for _, file := range sourcescan.Production(t) {
		occurrences += len(handRolled.FindAll(file.Source, -1))
	}

	// Assert: the one loop left is resolveAllNaming's own.
	if len(offenders) != 0 || occurrences != 1 {
		t.Fatalf("hand-rolled ref loops in %v (%d in all), want only resolveAllNaming's", offenders, occurrences)
	}
}
