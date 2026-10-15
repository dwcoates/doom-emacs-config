package reposcans

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"

	"agent-repld/internal/merge"
	"agentrepl/testrun/roster"
)

// moduleRoot is where the agent-repl module sits in the repository: a changed
// path, as the merge gate's selection reads it, is spelled from the
// repository root.
const moduleRoot = "modules/app/agent-repl/"

// TestEveryGoSuiteIsSelectedByEveryLocalModuleItReplaces derives each Go
// suite's local dependencies from its own go.mod (`replace ... => ../...`) and
// checks a change in any of them selects the suite, so a new cross-module
// import can never leave a merge's gate blind to it.
func TestEveryGoSuiteIsSelectedByEveryLocalModuleItReplaces(t *testing.T) {
	// Arrange: the module root on disk, two directories up.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}
	replace := regexp.MustCompile(`=>\s*(\.\.?/\S*)`)
	checked := 0
	for _, suite := range roster.Suites {
		if suite.Kind != roster.GoModule && suite.Kind != roster.E2E {
			continue
		}
		body, err := os.ReadFile(filepath.Join(root, suite.Path, "go.mod"))
		if err != nil {
			t.Fatalf("read %s's go.mod: %v", suite.Name, err)
		}
		for _, m := range replace.FindAllStringSubmatch(string(body), -1) {
			dep, err := filepath.Rel(root, filepath.Join(root, suite.Path, m[1]))
			if err != nil || strings.HasPrefix(dep, "..") {
				t.Fatalf("%s replaces %s outside the module: %v", suite.Name, m[1], err)
			}
			path := moduleRoot + filepath.ToSlash(dep) + "/x.go"
			checked++

			// Act.
			got := merge.SelectSuites([]string{path})

			// Assert.
			if !got.Full && !slices.Contains(got.Suites, suite.Name) {
				t.Errorf("a change at %s (a module %s replaces) selects %v, which leaves out %s", path, suite.Name, got.Suites, suite.Name)
			}
		}
	}
	if checked == 0 {
		t.Fatal("no go.mod replaced any local module; the walk read nothing")
	}
}

// TestEveryVitestSuiteIsSelectedByEveryTreeItImports walks each Vitest
// suite's sources and checks that every relative import reaching OUTSIDE the
// suite's own directory (the generated TS bindings, the shared logging
// module) selects the suite.
func TestEveryVitestSuiteIsSelectedByEveryTreeItImports(t *testing.T) {
	// Arrange: the module root on disk, two directories up.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}
	importRe := regexp.MustCompile(`(?:from|import)\s*\(?\s*['"](\.\.?/[^'"]+)['"]`)
	checked := map[string]bool{}
	for _, suite := range roster.Suites {
		if suite.Kind != roster.Vitest {
			continue
		}
		suiteDir := filepath.Join(root, suite.Path)
		for _, sub := range []string{"src", "test"} {
			err := filepath.WalkDir(filepath.Join(suiteDir, sub), func(path string, d os.DirEntry, err error) error {
				if err != nil {
					return err
				}
				if d.IsDir() && d.Name() == "node_modules" {
					return filepath.SkipDir
				}
				if d.IsDir() || !strings.HasSuffix(path, ".ts") {
					return nil
				}
				body, err := os.ReadFile(path)
				if err != nil {
					return err
				}
				for _, m := range importRe.FindAllStringSubmatch(string(body), -1) {
					target := filepath.Join(filepath.Dir(path), m[1])
					if rel, _ := filepath.Rel(suiteDir, target); !strings.HasPrefix(rel, "..") {
						continue
					}
					dep, err := filepath.Rel(root, target)
					if err != nil || strings.HasPrefix(dep, "..") {
						return fmt.Errorf("%s imports %s outside the module", path, m[1])
					}
					changed := moduleRoot + filepath.ToSlash(dep) + ".ts"
					if checked[suite.Name+" "+changed] {
						continue
					}
					checked[suite.Name+" "+changed] = true

					// Act.
					got := merge.SelectSuites([]string{changed})

					// Assert.
					if !got.Full && !slices.Contains(got.Suites, suite.Name) {
						t.Errorf("a change at %s (imported by %s) selects %v, which leaves out %s", changed, suite.Name, got.Suites, suite.Name)
					}
				}
				return nil
			})
			if err != nil {
				t.Fatalf("walk %s/%s: %v", suite.Name, sub, err)
			}
		}
	}
	if len(checked) == 0 {
		t.Fatal("no Vitest suite imports anything outside itself; the walk read nothing")
	}
}

// TestNoSystemsIntegrationSuiteRunsAnotherSystem pins the rule the selection
// rests on (AGENTS.md, "A system's integration tests run that system alone"):
// no system's integration sources build, locate or start another system's
// real binary. Each entry names what running that other system would require
// a source to say; a line that is only a comment is not read.
func TestNoSystemsIntegrationSuiteRunsAnotherSystem(t *testing.T) {
	// Arrange: the module root on disk, two directories up.
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatalf("resolve the module root: %v", err)
	}
	tests := []struct {
		dir string
		// skip is a subdirectory that is not this system's own suite: the
		// daemon's integration harness is also the end-to-end suite's, and
		// stages real services for it.
		skip      string
		forbidden []*regexp.Regexp
	}{
		{dir: "daemon/integration", skip: "harness", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`ServiceBinaries\s*:`), regexp.MustCompile(`shim-store"`), regexp.MustCompile(`shim-sidecar"`)}},
		{dir: "agent-shim/claude/shim-sidecar/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`"shim-store"`), regexp.MustCompile(`"agent-repld"`), regexp.MustCompile(`storeBinary`)}},
		{dir: "agent-shim/shim-store/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`"shim-claude-sidecar"\)`), regexp.MustCompile(`shim-sidecar"`), regexp.MustCompile(`"agent-repld"`), regexp.MustCompile(`dist/main\.js`)}},
		{dir: "agent-shim/claude/shim/test/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`shim-store`), regexp.MustCompile(`shim-sidecar`), regexp.MustCompile(`agent-repld"`)}},
		{dir: "webapp/test/integration", forbidden: []*regexp.Regexp{
			regexp.MustCompile(`daemon/bin`), regexp.MustCompile(`cmd/agent-repld`), regexp.MustCompile(`shim-store`)}},
	}
	for _, tt := range tests {
		t.Run(tt.dir, func(t *testing.T) {
			// Act.
			err := filepath.WalkDir(filepath.Join(root, tt.dir), func(path string, d os.DirEntry, err error) error {
				if err != nil {
					return err
				}
				if d.IsDir() && tt.skip != "" && d.Name() == tt.skip {
					return filepath.SkipDir
				}
				if d.IsDir() || !(strings.HasSuffix(path, ".go") || strings.HasSuffix(path, ".ts")) {
					return nil
				}
				body, err := os.ReadFile(path)
				if err != nil {
					return err
				}
				for i, line := range strings.Split(string(body), "\n") {
					trimmed := strings.TrimSpace(line)
					if strings.HasPrefix(trimmed, "//") || strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "/*") {
						continue
					}
					// Assert.
					for _, re := range tt.forbidden {
						if re.MatchString(line) {
							t.Errorf("%s:%d runs another system for real (%s): %s", path, i+1, re, trimmed)
						}
					}
				}
				return nil
			})
			if err != nil {
				t.Fatalf("walk %s: %v", tt.dir, err)
			}
		})
	}
}
