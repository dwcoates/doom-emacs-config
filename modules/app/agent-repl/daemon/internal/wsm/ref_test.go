package wsm

import (
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestRefNamesTheWorkspaceByIdAndDir(t *testing.T) {
	tests := []struct {
		name string
		ref  func() *workspacev1.WorkspaceRef
	}{
		{name: "a workspace record", ref: func() *workspacev1.WorkspaceRef {
			return Workspace{ID: "w-1", Dir: "/repos/one/w-1", Name: "one", Branch: "branch/w-1"}.Ref()
		}},
		{name: "a family member", ref: func() *workspacev1.WorkspaceRef {
			return FamilyRef{ID: "w-1", Dir: "/repos/one/w-1"}.Ref()
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the fixture is the case.

			// Act.
			got := tc.ref()

			// Assert.
			if got.GetId() != "w-1" || got.GetDir() != "/repos/one/w-1" {
				t.Fatalf("Ref() = %v, want id w-1 and dir /repos/one/w-1", got)
			}
		})
	}
}

func TestRefAnswersAFreshMessageEachCall(t *testing.T) {
	// Arrange.
	ws := Workspace{ID: "w-1", Dir: "/d"}

	// Act.
	first, second := ws.Ref(), ws.Ref()
	first.Dir = "/changed"

	// Assert: a caller mutating its ref never reaches another's.
	if second.GetDir() != "/d" {
		t.Fatalf("second Ref().Dir = %q, want /d", second.GetDir())
	}
}

func TestWorkspaceFamilyRefCarriesTheIDTheDirAndTheName(t *testing.T) {
	// Arrange
	ws := Workspace{ID: "w-1", Dir: "/repos/one/w-1", Name: "one"}

	// Act
	got := ws.FamilyRef()

	// Assert
	if got != (FamilyRef{ID: "w-1", Dir: "/repos/one/w-1", Name: "one"}) {
		t.Fatalf("FamilyRef() = %+v, want one (w-1) at /repos/one/w-1", got)
	}
}
