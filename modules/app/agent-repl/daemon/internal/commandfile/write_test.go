package commandfile

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestWritePublishesAFileTheIngressGlobMatches(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	entries := []Entry{{Type: TypeMerge, ProjectDir: "/tree"}}

	// Act.
	name, err := Write(dir, entries)

	// Assert.
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	if matched, _ := filepath.Match(DefaultGlob, name); !matched {
		t.Fatalf("Write published %q, which the ingress glob %q does not match", name, DefaultGlob)
	}
}

func TestWriteRoundTripsThroughTheIngressParse(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	entries := []Entry{
		{Type: TypeCreate, GitRoot: "/repo", Name: "merge-queue/x-landing", BaseRef: "feat/x"},
		{Type: TypeMerge, ProjectDir: "/repo-worktrees/x-landing", Workspace: "merge-queue/x-landing"},
	}

	// Act.
	name, err := Write(dir, entries)
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	data, err := os.ReadFile(filepath.Join(dir, name))
	if err != nil {
		t.Fatalf("read the published file: %v", err)
	}
	got, err := parse(data)

	// Assert.
	if err != nil {
		t.Fatalf("the ingress's parse refused the published file: %v", err)
	}
	if !reflect.DeepEqual(got, entries) {
		t.Fatalf("parsed %+v, want %+v", got, entries)
	}
}

func TestWriteLeavesOnlyThePublishedFile(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	name, err := Write(dir, []Entry{{Type: TypeMerge, ProjectDir: "/tree"}})
	if err != nil {
		t.Fatalf("Write: %v", err)
	}
	listing, err := os.ReadDir(dir)

	// Assert.
	if err != nil {
		t.Fatalf("list the ingress directory: %v", err)
	}
	if len(listing) != 1 || listing[0].Name() != name {
		t.Fatalf("the ingress directory holds %v, want only %q", listing, name)
	}
}

func TestWriteMintsADistinctNamePerFile(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	entries := []Entry{{Type: TypeMerge, ProjectDir: "/tree"}}

	// Act.
	first, err := Write(dir, entries)
	if err != nil {
		t.Fatalf("first Write: %v", err)
	}
	second, err := Write(dir, entries)
	if err != nil {
		t.Fatalf("second Write: %v", err)
	}

	// Assert.
	if first == second {
		t.Fatalf("two writes published one name %q; concurrent producers would overwrite each other", first)
	}
}

func TestWriteRefusesAnInvalidEntryAndWritesNothing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	_, err := Write(dir, []Entry{{Type: TypeMerge}})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the ingress would quarantine") {
		t.Fatalf("Write error = %v, want the ingress parse's refusal", err)
	}
	if listing, _ := os.ReadDir(dir); len(listing) != 0 {
		t.Fatalf("a refused write left %v behind", listing)
	}
}

func TestWriteRefusesAnEmptyArray(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	_, err := Write(dir, nil)

	// Assert.
	if err == nil {
		t.Fatal("Write accepted an empty command array, which asks for nothing")
	}
}
