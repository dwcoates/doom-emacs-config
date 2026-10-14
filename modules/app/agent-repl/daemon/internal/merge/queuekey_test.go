package merge

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/wsm"
)

func TestQueueKeyOfKeysTheMatchedRepositoryByItsCommonDir(t *testing.T) {
	tests := []struct {
		name  string
		scope func(h *harness) RepositoryScope
	}{
		{name: "matched by id", scope: func(*harness) RepositoryScope { return RepositoryScope{ID: theRepository} }},
		{name: "matched by dir", scope: func(h *harness) RepositoryScope { return RepositoryScope{Dir: h.repoRoot} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			h := newHarness(t)

			// Act.
			got, err := h.o.queueKeyOf(context.Background(), "daemon.merge.test", "a test request", tt.scope(h))

			// Assert.
			if err != nil || got != wsm.RepoKey(h.repoRoot+"/.git") {
				t.Fatalf("queueKeyOf = %q, %v; want %q", got, err, h.repoRoot+"/.git")
			}
		})
	}
}

func TestQueueKeyOfRefusesAnEmptyRef(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.o.queueKeyOf(context.Background(), "daemon.merge.test", "a test request", RepositoryScope{})

	// Assert.
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("queueKeyOf = %v, want the unknown_repository refusal", err)
	}
	if !loggedLike(h, logLike{level: "warn", op: "daemon.merge.test", text: "refused a test request scoped by an empty repository ref"}) {
		t.Fatalf("no warning names the refused request: %+v", h.logs.Records())
	}
}

func TestQueueKeyOfRefusesARefNoRegisteredRepositoryMatches(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	_, err := h.o.queueKeyOf(context.Background(), "daemon.merge.test", "a test request", RepositoryScope{Dir: "/no/such/repository"})

	// Assert.
	if refusal, ok := Refused(err); !ok || refusal.Arm != ArmUnknownRepository {
		t.Fatalf("queueKeyOf = %v, want the unknown_repository refusal", err)
	}
	if !loggedLike(h, logLike{level: "warn", op: "daemon.merge.test", text: "refused a test request for a repository the registry does not hold",
		context: map[string]string{"dir": "/no/such/repository"}}) {
		t.Fatalf("no warning names the unmatched ref: %+v", h.logs.Records())
	}
}

func TestQueueKeyOfSurfacesAFailedCommonDirRead(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.git.commonDirErr = errors.New("git exploded")

	// Act.
	_, err := h.o.queueKeyOf(context.Background(), "daemon.merge.test", "a test request", RepositoryScope{ID: theRepository})

	// Assert.
	if err == nil || !errors.Is(err, h.git.commonDirErr) {
		t.Fatalf("queueKeyOf = %v, want the CommonDir failure", err)
	}
	if _, refused := Refused(err); refused {
		t.Fatalf("a git failure was answered as a refusal: %v", err)
	}
}
