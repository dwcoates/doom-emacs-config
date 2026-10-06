package workspace

import (
	"context"
	"errors"
	"fmt"
	"testing"

	"claude-repld/internal/tempdirs"
	"claude-repld/internal/wsm"
)

// insideTemp is the registry's refusal of a scratch folder, as the store
// answers it (wrapped, the way wsm wraps it).
var insideTemp = fmt.Errorf("wsm: %w", &tempdirs.InsideError{
	Dir: "/private/var/folders/ab/cd/T/scratch", Root: "/private/var/folders",
})

// assertTemporaryRefusal checks the arm, its two fields, and the INFO record
// every refusal is filed under.
func assertTemporaryRefusal(t *testing.T, f *fixture, err error, rpc string) {
	t.Helper()
	refusal := asRefusal(t, err, ArmInsideTemporaryDirectory)
	if refusal.Rpc != rpc {
		t.Errorf("refusal rpc = %q, want %q", refusal.Rpc, rpc)
	}
	if refusal.Fields["dir"] != "/private/var/folders/ab/cd/T/scratch" || refusal.Fields["temporary_root"] != "/private/var/folders" {
		t.Errorf("refusal fields = %v, want the dir and the temporary root", refusal.Fields)
	}
	awaitRecord(t, f, "info", opRefusal)
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" || r.Level == "warn" {
			t.Errorf("a refusal is an answer, but it was recorded at %s: %+v", r.Level, r)
		}
	}
}

func TestRegisterAnswersTheRegistrysTemporaryRefusalByArm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.registerErr = insideTemp

	// Act.
	_, err := f.verbs.Register(context.Background(), worktreeDir(t), wsm.RegisterFacts{})

	// Assert.
	assertTemporaryRefusal(t, f, err, "RegisterWorkspace")
}

func TestRegisterRepositoryAnswersTheRegistrysTemporaryRefusalByArm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := worktreeDir(t)
	f.git.mainWorktree = dir
	f.db.registerRepoErr = insideTemp

	// Act.
	_, err := f.verbs.RegisterRepository(context.Background(), dir)

	// Assert.
	assertTemporaryRefusal(t, f, err, "RegisterRepository")
}

func TestCreateRefusesATemporaryRepositoryBeforeBuildingAnything(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	f.db.refuseTemporaryErr = insideTemp

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	assertTemporaryRefusal(t, f, err, "CreateWorkspace")
	if len(f.git.created) != 0 || len(f.db.registered) != 0 {
		t.Fatalf("a refused create built worktrees %+v and registered %d workspaces, want none", f.git.created, len(f.db.registered))
	}
}

func TestCreateSurfacesARepositoryTheGuardCannotJudge(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	f.db.refuseTemporaryErr = errFake

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if _, refused := AsRefusal(err); refused || !errors.Is(err, errFake) {
		t.Fatalf("Create = %v, want the guard's own failure, not a refusal", err)
	}
	awaitRecord(t, f, "error", opCreate)
}
