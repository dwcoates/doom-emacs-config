package reuse

import (
	"os"
	"path/filepath"
	"testing"

	"agentrepl/testrun/roster"
)

// graphSuite is a script suite whose scripts come from bin/.
var graphSuite = roster.Suite{Name: "graph", Kind: roster.Script, Path: "bin/test-g.sh", Reuse: &roster.Reuse{Scripts: []string{"bin/"}}}

// graphFixture writes bin/ scripts into the fixture: test-g.sh sources
// lib-a.sh, which runs helper.sh; other.sh is named by nobody.
func graphFixture(t *testing.T) fixture {
	t.Helper()
	f := newFixture(t)
	bin := filepath.Join(f.d.Module, "bin")
	for name, body := range map[string]string{
		"test-g.sh": `. "$(dirname "$0")/lib-a.sh"`,
		"lib-a.sh":  `"$BIN/helper.sh" --go`,
		"helper.sh": "echo hi",
		"other.sh":  "echo unrelated",
	} {
		if err := os.WriteFile(filepath.Join(bin, name), []byte(body), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return f
}

func TestAScriptGraphPlacesEveryScriptItsScriptsName(t *testing.T) {
	// Arrange
	f := graphFixture(t)

	// Act
	set, err := SuiteInputs(f.d, graphSuite)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, p := range []string{"/bin/test-g.sh", "/bin/lib-a.sh", "/bin/helper.sh"} {
		if !set.Contains(fixtureModule + p) {
			t.Errorf("the envelope does not place %s", p)
		}
	}
	if set.Contains(fixtureModule + "/bin/other.sh") {
		t.Error("the envelope places a script no placed script names")
	}
}

func TestAScriptGraphPlacesItsDirectorysListing(t *testing.T) {
	// Arrange
	f := graphFixture(t)

	// Act
	set, err := SuiteInputs(f.d, graphSuite)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	found := false
	for _, d := range set.Dirs() {
		if d == fixtureModule+"/bin" {
			found = true
		}
	}
	if !found {
		t.Fatalf("listings %v, want bin's listing placed so a glob over it sees every name", set.Dirs())
	}
}

func TestAChangeToAScriptNoPlacedScriptNamesMovesNoKey(t *testing.T) {
	// Arrange
	f := graphFixture(t)
	set, err := SuiteInputs(f.d, graphSuite)
	if err != nil {
		t.Fatal(err)
	}
	before := mustKey(t, f.d.Repo, set, Facts{})

	// Act
	if err := os.WriteFile(filepath.Join(f.d.Module, "bin", "other.sh"), []byte("echo changed"), 0o644); err != nil {
		t.Fatal(err)
	}
	after := mustKey(t, f.d.Repo, set, Facts{})

	// Assert
	if after.Digest != before.Digest {
		t.Fatal("a change to a script the suite never names moved its key")
	}
}

func TestAScriptGraphLeavesOutWhatGitIgnores(t *testing.T) {
	// Arrange: lib-a.sh names a binary a build leaves in bin/.
	f := graphFixture(t)
	if err := os.WriteFile(filepath.Join(f.d.Module, "bin", "lib-a.sh"), []byte(`"$BIN/helper.sh"; "$BIN/built-tool"`), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(f.d.Module, "bin", "built-tool"), []byte("binary"), 0o755); err != nil {
		t.Fatal(err)
	}
	f.d.Ignored = Ignored{fixtureModule + "/bin/built-tool"}

	// Act
	set, err := SuiteInputs(f.d, graphSuite)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if set.Contains(fixtureModule + "/bin/built-tool") {
		t.Fatal("the envelope places a file git ignores")
	}
}

func TestAScriptGraphNamesNoLinkToADirectory(t *testing.T) {
	// Arrange: bin/skill links to a directory, and the script names it.
	f := graphFixture(t)
	if err := os.MkdirAll(filepath.Join(f.d.Module, "prompts", "skill"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(filepath.Join(f.d.Module, "prompts", "skill"), filepath.Join(f.d.Module, "bin", "skill")); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(f.d.Module, "bin", "test-g.sh"), []byte(`. "$(dirname "$0")/lib-a.sh"; ls skill`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	set, err := SuiteInputs(f.d, graphSuite)

	// Assert
	if err != nil {
		t.Fatalf("SuiteInputs: %v, want a link to a directory left out of the graph", err)
	}
	if set.Contains(fixtureModule + "/bin/skill") {
		t.Fatal("the envelope places a link to a directory as a script")
	}
}
