package scans

import (
	"go/ast"
	"go/parser"
	"go/token"
	"os"
	"path/filepath"
	"reflect"
	"regexp"
	"slices"
	"strings"
	"testing"
)

// TestEveryCursorWaitGoesThroughAwaitCursors pins that each cursor wait hands
// its predicate to awaitCursors and polls nothing itself, so the bound, the
// interval and the failure message stay one loop's.
func TestEveryCursorWaitGoesThroughAwaitCursors(t *testing.T) {
	// Arrange.
	file, err := parser.ParseFile(token.NewFileSet(), "kit/stack/world.go", nil, 0)
	if err != nil {
		t.Fatalf("parse kit/stack/world.go: %v", err)
	}
	waits := map[string]bool{"awaitCursorAdvanceWithin": false, "AwaitFileIngested": false}

	for _, decl := range file.Decls {
		fn, ok := decl.(*ast.FuncDecl)
		if !ok {
			continue
		}
		if _, isWait := waits[fn.Name.Name]; !isWait {
			continue
		}
		// Act.
		ast.Inspect(fn.Body, func(n ast.Node) bool {
			switch n := n.(type) {
			case *ast.ForStmt:
				// A predicate may range over the cursors it is handed; a
				// plain for loop is a poll of the wait's own.
				t.Errorf("%s polls in a loop of its own; hand a predicate to awaitCursors", fn.Name.Name)
			case *ast.CallExpr:
				if id, ok := n.Fun.(*ast.Ident); ok && id.Name == "awaitCursors" {
					waits[fn.Name.Name] = true
				}
			}
			return true
		})
	}

	// Assert.
	for name, calls := range waits {
		if !calls {
			t.Errorf("%s does not wait through awaitCursors", name)
		}
	}
}

func TestEveryProgramThisSuiteBuildsGoesThroughProgramOnce(t *testing.T) {
	// A hand-rolled once-builder drifts from programOnce's prebuilt lookup
	// and its failure; only programOnce may call testenv.ProgramOr.
	src, err := os.ReadFile("kit/stack/main.go")
	if err != nil {
		t.Fatal(err)
	}
	calls := regexp.MustCompile(`testenv\.ProgramOr\(`).FindAllIndex(src, -1)
	if len(calls) != 1 {
		t.Fatalf("kit/stack/main.go calls testenv.ProgramOr %d times, want once, inside ProgramOnce.Require", len(calls))
	}
}

// TestEveryBookReadGoesThroughReadAgentBook pins that no test opens a book by
// hand: the store's success arm and failure arm are read in one place.
func TestEveryBookReadGoesThroughReadAgentBook(t *testing.T) {
	for _, name := range []string{"remainder_e2e_test.go", "emacs_interrupt_e2e_test.go", "subagents_e2e_test.go"} {
		// Act.
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}

		// Assert.
		if strings.Contains(string(raw), ".OpenAgentSession(") {
			t.Fatalf("%s opens a store book by hand; use kit.ReadAgentBook", name)
		}
	}
}

// emacsclientExec is a sandbox Exec of emacsclient written out by hand.
var emacsclientExec = regexp.MustCompile(`\.Exec\([^\n]*"emacsclient"`)

// TestEveryServerCallRidesTheEmacsclientHelper fails a hand-rolled
// emacsclient Exec anywhere in the layer: such a call would be bounded by its
// own rule rather than the one the helper decides.
func TestEveryServerCallRidesTheEmacsclientHelper(t *testing.T) {
	// Arrange
	files, err := e2eSources("*.go")
	if err != nil {
		t.Fatalf("glob the layer's sources: %v", err)
	}

	// Act
	var sites []string
	for _, f := range files {
		src, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		for _, m := range emacsclientExec.FindAll(src, -1) {
			sites = append(sites, f+": "+string(m))
		}
	}

	// Assert
	// Spelled in two halves so this file's own source is not a match.
	want := []string{"kit/stack/emacs.go: .Exec(ctx, " + `"emacs` + `client"`}
	if !slices.Equal(sites, want) {
		t.Fatalf("emacsclient Exec sites = %q, want only the helper's %q", sites, want)
	}
}

// TestTheLayerDriverIsKilledOnlyThroughKillGroup pins that every kill of the
// driver's group goes through killGroup, so none drops the kill's error.
func TestTheLayerDriverIsKilledOnlyThroughKillGroup(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("kit/stack/webapplayer_server.go")
	if err != nil {
		t.Fatalf("read the driver's source: %v", err)
	}

	// Act
	raw := regexp.MustCompile(`syscall\.Kill\(`).FindAllIndex(src, -1)
	group := regexp.MustCompile(`harness\.KillGroup\(`).FindAllIndex(src, -1)
	viaHelper := regexp.MustCompile(`s\.killGroup\(\)`).FindAllIndex(src, -1)

	// Assert: no raw kill, and the one group kill is killProcessGroup's own.
	if len(raw) != 0 {
		t.Fatalf("webapplayer_server_test.go calls syscall.Kill %d time(s), want none: the group kill is harness.KillGroup's", len(raw))
	}
	if len(group) != 1 {
		t.Fatalf("webapplayer_server_test.go calls harness.KillGroup %d time(s), want only killProcessGroup's one", len(group))
	}
	if len(viaHelper) < 3 {
		t.Fatalf("webapplayer_server_test.go calls s.killGroup() %d time(s), want the warm-up, run and stop sites", len(viaHelper))
	}
}

// TestEveryWorldChecksItsServicesThroughOneHelper pins the extraction: an
// owned World, the shared World's views and an EmacsWorld all check that
// their services outlived the test through requireServicesOutlivedTheTest.
func TestEveryWorldChecksItsServicesThroughOneHelper(t *testing.T) {
	// Arrange
	sites := map[string]string{
		"kit/stack/world.go":       "t.Cleanup(func() { requireServicesOutlivedTheTest(t, store, sidecar, zettel) })",
		"kit/stack/sharedworld.go": "t.Cleanup(func() { requireServicesOutlivedTheTest(t, s.world.Store, s.world.Sidecar, s.world.Zettel) })",
		"kit/stack/emacs_world.go": "t.Cleanup(func() { requireServicesOutlivedTheTest(t, store, sidecar, zettel) })",
	}

	for file, call := range sites {
		// Act
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}

		// Assert
		if !strings.Contains(string(body), call) {
			t.Errorf("%s does not check its services through requireServicesOutlivedTheTest", file)
		}
		// The helper itself is the one place that spells the check.
		want := map[string]int{"kit/stack/world.go": 1, "kit/stack/sharedworld.go": 0, "kit/stack/emacs_world.go": 0}[file]
		if got := strings.Count(string(body), "the store exited before test cleanup"); got != want {
			t.Errorf("%s spells the service check %d times, want %d: only the helper spells it", file, got, want)
		}
	}
}

// TestNoTestMintsAWorkspaceByHand pins the extraction: a fresh fake-git
// workspace is minted through World.NewWorkspace or NewRepoWorkspace, never by
// pairing harness.NewRepo with harness.Register at the call site.
func TestNoTestMintsAWorkspaceByHand(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`(\w+) := harness\.NewRepo\(t\)\n\s*(?:ws :=|return) harness\.Register\(t, \w+\.Daemon, (\w+)\.Dir\)`)
	files, err := e2eSources("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var found []string
	for _, f := range files {
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		for _, m := range handRolled.FindAllStringSubmatch(string(body), -1) {
			if m[1] == m[2] && f != "kit/stack/world.go" {
				found = append(found, f)
			}
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("files minting a workspace by hand: %s; use World.NewWorkspace or NewRepoWorkspace", strings.Join(found, ", "))
	}
}

// TestNoTestRequiresAnOpenByHand pins the extraction: a test that needs its
// workspace open and nothing else calls World.RequireOpen.
func TestNoTestRequiresAnOpenByHand(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`OpenWorkspace\(w\.Ctx\(\), [^\n]*\n\s*if err != nil \{\n\s*t\.Fatalf\([^\n]*\n\s*\}\n\s*if \w+\.Msg\.GetSuccess\(\) == nil \{\n\s*t\.Fatalf`)
	files, err := e2eSources("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var found []string
	for _, f := range files {
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		if n := len(handRolled.FindAllString(string(body), -1)); n > 0 && !(f == "kit/stack/world.go" && n == 1) {
			found = append(found, f)
		}
	}

	// Assert
	if len(found) > 0 {
		t.Fatalf("files requiring an open by hand: %s; use World.RequireOpen", strings.Join(found, ", "))
	}
}

// handRolledTurnEnded is the match kit.EndsTurn owns, written out by hand.
var handRolledTurnEnded = regexp.MustCompile(`GetTurn\(\)\.GetValue\(\) == \w+\.GetValue\(\) && \w+\.GetTurnEnded\(\) != nil`)

// TestTurnEndedMatchIsShared fails on any test in this suite that writes the
// turn-ended match out by hand instead of calling kit.EndsTurn, so a divergent
// copy cannot drift from the one every wait shares.
func TestTurnEndedMatchIsShared(t *testing.T) {
	t.Parallel()
	files, err := e2eSources("*_test.go")
	if err != nil {
		t.Fatalf("glob the suite's sources: %v", err)
	}
	var offenders []string
	for _, f := range files {
		src, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		// endsTurn's own body is kit.EndsTurn's, outside this package.
		n := len(handRolledTurnEnded.FindAll(src, -1))
		if n > 0 {
			offenders = append(offenders, f)
		}
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled turn-ended matches in %v; call kit.EndsTurn(turn) instead", offenders)
	}
}

// TestKeepaliveWorldIsTheOneCompressedCadence pins keepaliveWorld as the one
// place a world compresses the daemon's keep-alive cadence, so a test that
// hand-rolls its own cannot drift from it.
func TestKeepaliveWorldIsTheOneCompressedCadence(t *testing.T) {
	// Arrange
	files, err := e2eSources("*_test.go")
	if err != nil {
		t.Fatalf("listing the e2e test files: %v", err)
	}

	// Act
	sites := map[string]int{}
	for _, file := range files {
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("reading %s: %v", file, err)
		}
		if n := strings.Count(string(src), "KeepaliveInterval"+": "); n > 0 {
			sites[file] = n
		}
	}

	// Assert
	if want := map[string]int{"keepalive_e2e_test.go": 1}; !reflect.DeepEqual(sites, want) {
		t.Errorf("worlds compressing the keep-alive cadence = %v, want only keepaliveWorld's %v", sites, want)
	}
}

// e2eSources is the e2e package's files matching pattern, and every file of
// kit/stack, the harness the package's worlds are built from: what a scan of
// the suite's own sources reads.
func e2eSources(pattern string) ([]string, error) {
	files, err := filepath.Glob(pattern)
	if err != nil {
		return nil, err
	}
	stack, err := filepath.Glob(filepath.Join("kit", "stack", "*.go"))
	if err != nil {
		return nil, err
	}
	return append(files, stack...), nil
}
