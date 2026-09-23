package main

import (
	"errors"
	"strings"
	"testing"

	storev1 "agentrepl/proto/store/v1"
	"agentrepl/shim-store/internal/server"
)

// AN EMPTY CATALOG SAYS SO. "No shapes" and "the tool did not run" must not
// look alike in a terminal.
func TestAnEmptyCatalogIsStatedRatherThanPrintedAsNothing(t *testing.T) {
	// Arrange.
	var out strings.Builder

	// Act.
	if err := render(&out, nil, false); err != nil {
		t.Fatalf("render = %v, want nil", err)
	}

	// Assert.
	if !strings.Contains(out.String(), "the residue shape catalog is empty") {
		t.Fatalf("output = %q, want the empty catalog stated", out.String())
	}
}

func TestARowShowsItsCountKindAndStructure(t *testing.T) {
	// Arrange.
	var out strings.Builder
	row := &storev1.ResidueShapeRow{
		ShapeHash: strings.Repeat("a", 64), Kind: "unparsed",
		KeyStructure: "{a:string}", Count: 7,
	}

	// Act.
	if err := render(&out, []*storev1.ResidueShapeRow{row}, false); err != nil {
		t.Fatalf("render = %v, want nil", err)
	}

	// Assert.
	for _, want := range []string{"count=7", "kind=unparsed", "structure: {a:string}"} {
		if !strings.Contains(out.String(), want) {
			t.Fatalf("output = %q, want it to contain %q", out.String(), want)
		}
	}
}

// THE EXAMPLE IS OPT-IN, and the renderer honors that rather than printing raw
// vendor bytes into a structure survey.
func TestTheExampleIsWithheldUnlessAskedFor(t *testing.T) {
	// Arrange.
	var out strings.Builder
	row := &storev1.ResidueShapeRow{
		ShapeHash: strings.Repeat("a", 64), Kind: "unparsed",
		KeyStructure: "{a:string}", FirstExample: []byte("the raw line"),
	}

	// Act.
	if err := render(&out, []*storev1.ResidueShapeRow{row}, false); err != nil {
		t.Fatalf("render = %v, want nil", err)
	}

	// Assert.
	if strings.Contains(out.String(), "the raw line") {
		t.Fatalf("output = %q, want the example withheld", out.String())
	}
}

func TestTheExampleIsShownWhenAskedFor(t *testing.T) {
	// Arrange.
	var out strings.Builder
	row := &storev1.ResidueShapeRow{
		ShapeHash: strings.Repeat("a", 64), Kind: "unparsed",
		KeyStructure: "{a:string}", FirstExample: []byte("the raw line"),
	}

	// Act.
	if err := render(&out, []*storev1.ResidueShapeRow{row}, true); err != nil {
		t.Fatalf("render = %v, want nil", err)
	}

	// Assert.
	if !strings.Contains(out.String(), "the raw line") {
		t.Fatalf("output = %q, want the example shown", out.String())
	}
}

// THE HASH IS ABBREVIATED ON SCREEN and never truncated below what is there, so
// a short or malformed digest cannot slice out of range.
func TestAShortHashIsPrintedWhole(t *testing.T) {
	// Arrange.
	var out strings.Builder
	row := &storev1.ResidueShapeRow{ShapeHash: "abc", Kind: "unparsed", KeyStructure: "{}"}

	// Act.
	if err := render(&out, []*storev1.ResidueShapeRow{row}, false); err != nil {
		t.Fatalf("render = %v, want nil", err)
	}

	// Assert.
	if !strings.Contains(out.String(), "abc  count=") {
		t.Fatalf("output = %q, want the short hash printed whole", out.String())
	}
}

// failingWriter is a stdout that refuses every write, as a closed pipe does.
type failingWriter struct{ err error }

func (w failingWriter) Write([]byte) (int, error) { return 0, w.err }

// A FAILED WRITE IS THE TOOL'S FAILURE, never a listing that silently stopped.
func TestRenderReportsAWriteItCouldNotMake(t *testing.T) {
	// Arrange.
	writeErr := errors.New("broken pipe")

	// Act.
	err := render(failingWriter{err: writeErr}, nil, false)

	// Assert.
	if !errors.Is(err, writeErr) {
		t.Fatalf("render = %v, want the write failure", err)
	}
}

// THE TOOL REACHES THE RUNNING STORE WITH NO ARGUMENT, which means honoring the
// same environment variable the store's own --socket defaults from.
func TestTheSocketDefaultHonorsTheStoresEnvironmentVariable(t *testing.T) {
	// Arrange.
	t.Setenv(server.EnvSocket, "/tmp/a-private-store.sock")

	// Act.
	got := socketDefault()

	// Assert.
	if got != "/tmp/a-private-store.sock" {
		t.Fatalf("socketDefault = %q, want the environment's socket", got)
	}
}

func TestTheSocketDefaultFallsBackToTheCacheDirectory(t *testing.T) {
	// Arrange.
	t.Setenv(server.EnvSocket, "")
	t.Setenv("XDG_CACHE_HOME", "/tmp/cache")

	// Act.
	got := socketDefault()

	// Assert.
	if got != "/tmp/cache/agent-repl/sock/store.sock" {
		t.Fatalf("socketDefault = %q, want the cache-dir path", got)
	}
}
