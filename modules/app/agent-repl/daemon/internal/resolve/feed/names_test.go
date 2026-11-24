package feed

import (
	"errors"
	"strings"
	"testing"
)

func TestAPrimedWorkspacesNameIsReadUntilAReadAnswersIt(t *testing.T) {
	// Arrange: the registry fails the first read of the name.
	h := newHarness(t)
	h.names[otherWorkspace] = "review-bot"
	h.nameErr = errors.New("database is locked")
	h.resolver.prime(otherWorkspace)
	h.nameErr = nil

	// Act.
	h.resolver.prime(otherWorkspace)
	h.names[otherWorkspace] = "renamed-elsewhere"
	h.resolver.prime(otherWorkspace)

	// Assert: read again after the failure, then left as the read answered.
	name, err := h.resolver.knownName(otherWorkspace)
	if err != nil || name != "review-bot" {
		t.Fatalf("name = %q, %v; want review-bot, read once a read answered it", name, err)
	}
}

func TestARenamesPushIsTheNameDrawn(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.names[otherWorkspace] = "review-bot"
	h.resolver.prime(otherWorkspace)

	// Act.
	h.resolver.SetWorkspaceName(otherWorkspace, "better-name")

	// Assert.
	if name, err := h.resolver.knownName(otherWorkspace); err != nil || name != "better-name" {
		t.Fatalf("name = %q, %v; want the rename's", name, err)
	}
}

func TestAWorkspaceNamedWithoutItsNameReadPanics(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	defer func() {
		// Assert.
		got, _ := recover().(string)
		if !strings.Contains(got, "never-named") {
			t.Fatalf("panic = %q, want the unread workspace named", got)
		}
	}()

	// Act.
	h.resolver.knownName("never-named")
}
