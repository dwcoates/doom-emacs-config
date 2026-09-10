package workspace

import (
	"context"
	"errors"
	"path/filepath"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"
)

func TestResolveKeysOnTheId(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	dir := t.TempDir()
	f.workspace("w1", dir)

	// Act.
	got, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1", Dir: dir})

	// Assert.
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if got.ID != "w1" {
		t.Fatalf("Resolve() = %q, want w1", got.ID)
	}
}

func TestResolveRefusesADirThatDisagreesWithTheRegistry(t *testing.T) {
	// Arrange: the client echoes a stale directory.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1", Dir: t.TempDir()})

	// Assert.
	asRefusal(t, err, ArmWorkspaceRefMismatch)
}

func TestResolveAcceptsARefThatEchoesNoDir(t *testing.T) {
	// Arrange: a client holding only the id still names one workspace.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	got, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	if err != nil || got.ID != "w1" {
		t.Fatalf("Resolve() = (%q, %v), want w1", got.ID, err)
	}
}

func TestResolveAcceptsAnUnnormalizedButEquivalentDir(t *testing.T) {
	// Arrange: the echoed dir spells the same tree with a "." element.
	f := newFixture(t)
	dir := t.TempDir()
	f.workspace("w1", dir)

	// Act.
	got, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{
		Id: "w1", Dir: filepath.Join(dir, "."),
	})

	// Assert.
	if err != nil || got.ID != "w1" {
		t.Fatalf("Resolve() = (%q, %v), want w1", got.ID, err)
	}
}

func TestResolveRefusesAnUnknownId(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "nope"})

	// Assert.
	refusal := asRefusal(t, err, ArmUnknownWorkspace)
	if !refusal.NotFound {
		t.Fatal("the unknown-workspace refusal is not marked not-found")
	}
}

func TestResolveRefusesARefWithNoId(t *testing.T) {
	// Arrange: a path is never an identity.
	f := newFixture(t)
	dir := t.TempDir()
	f.workspace("w1", dir)

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Dir: dir})

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestResolveRefusesANilRef(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.Resolve(context.Background(), nil)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestResolveRefusesAWorkspaceTransferringAway(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingTransferringAway

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	asRefusal(t, err, ArmTransferringAway)
}

func TestResolveRefusesAWorkspaceNotYetAdopted(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.standing = StandingNotYetAdopted

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	asRefusal(t, err, ArmNotYetAdopted)
}

func TestResolveSurfacesAnOwnershipProbeFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.owner.err = errors.New("rollout is not answering")

	// Act.
	_, err := f.verbs.Resolve(context.Background(), &workspacev1.WorkspaceRef{Id: "w1"})

	// Assert.
	if err == nil {
		t.Fatal("Resolve() = nil error, want the probe failure surfaced")
	}
	if _, ok := AsRefusal(err); ok {
		t.Fatalf("Resolve() = %v, want a failure rather than a refusal", err)
	}
}
