package workspace

import (
	"testing"

	"agent-repld/internal/lifecycleconfig"
)

func TestNewCreateSpecStatesItsOriginator(t *testing.T) {
	for _, originator := range lifecycleconfig.Originators {
		t.Run(originator.String(), func(t *testing.T) {
			// Act
			spec := NewCreateSpec(originator)

			// Assert
			if got := spec.Originator(); got != originator {
				t.Fatalf("Originator() = %v, want %v", got, originator)
			}
		})
	}
}

func TestABareCreateSpecStatesNoOriginator(t *testing.T) {
	// Act
	got := CreateSpec{}.Originator()

	// Assert
	if got != 0 {
		t.Fatalf("Originator() = %v, want the zero value validateCreate refuses", got)
	}
}

func TestACreateSpecFromNewCreateSpecPassesValidation(t *testing.T) {
	// Arrange
	f := newFixture(t)
	spec := NewCreateSpec(lifecycleconfig.OriginatorUserOrWorkspace)
	spec.RepoDir, spec.InitialPrompt = mainWorktree(t, f), "fix the login bug"

	// Act
	err := f.verbs.(*verbs).validateCreate(f.log.logger, spec)

	// Assert
	if err != nil {
		t.Fatalf("validateCreate = %v, want the spec accepted", err)
	}
}
