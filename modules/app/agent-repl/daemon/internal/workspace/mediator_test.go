package workspace

import "testing"

func TestTheMediatorIsMadeHiddenOnItsModelWithNoPrompt(t *testing.T) {
	// Act
	spec := mediatorSpec("/repo", "deploy-mediator")

	// Assert
	if spec.RepoDir != "/repo" || spec.Name != "deploy-mediator" || !spec.Hidden || spec.Model != mediatorModel || spec.InitialPrompt != "" {
		t.Fatalf("spec = %+v, want a hidden, unprompted deploy-mediator on %s", spec, mediatorModel)
	}
}
