package suites

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
)

// The ERT chunk driver (testrun/ert/driver.el) is run here the way an ERT
// chunk runs it, against a fixture test directory: batch Emacs is the
// driver's own runtime, as it is the ERT suite's.

func ertDriver(t *testing.T) string {
	t.Helper()
	p, err := filepath.Abs(filepath.Join("..", "..", "ert", "driver.el"))
	if err != nil {
		t.Fatal(err)
	}
	return p
}

// runDriver runs one chunk of the fixture dir and answers its output and exit status.
func runDriver(t *testing.T, dir string, chunk, roster []string) (string, int) {
	t.Helper()
	return runDriverNamed(t, dir, chunk, roster, nil)
}

// runDriverNamed runs one chunk narrowed to each file's names in NAMES (a
// file with none runs whole), as the suite's chunk builder passes them.
func runDriverNamed(t *testing.T, dir string, chunk, roster []string, names map[string][]string) (string, int) {
	t.Helper()
	form := "(agent-repl-testrun-ert " + strconv.Quote(dir+"/") + " " + lispList(chunk) + " " + lispList(roster) + ")"
	if byFile := lispNamesByFile(chunk, names); byFile != "" {
		form = "(agent-repl-testrun-ert " + strconv.Quote(dir+"/") + " " + lispList(chunk) + " " + lispList(roster) + " " + byFile + ")"
	}
	return runDriverForm(t, dir, form)
}

// runDriverForm evaluates FORM in a batch Emacs with the driver loaded, from
// DIR, and answers its output and exit status.
func runDriverForm(t *testing.T, dir, form string) (string, int) {
	t.Helper()
	cmd := exec.Command("emacs", "-batch", "-Q", "-l", "ert", "-l", ertDriver(t), "--eval", form)
	cmd.Dir = dir
	out, err := cmd.CombinedOutput()
	var exitErr *exec.ExitError
	switch {
	case err == nil:
		return string(out), 0
	case errors.As(err, &exitErr):
		return string(out), exitErr.ExitCode()
	}
	t.Fatalf("run emacs: %v\n%s", err, out)
	return "", 0
}

func ertFixture(t *testing.T, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	write(t, filepath.Join(dir, "test-helpers.el"), "(ert-deftest fixture-helper-test () (should t))\n", 0o644)
	for name, body := range files {
		write(t, filepath.Join(dir, name), body, 0o644)
	}
	return dir
}

func TestERTDriver(t *testing.T) {
	passing := "(ert-deftest fixture-a-passes () (should t))\n"
	failing := "(ert-deftest fixture-b-fails () (should nil))\n"
	tests := []struct {
		name      string
		files     map[string]string
		chunk     []string
		roster    []string
		wantExit  int
		wantItems []string
		wantOut   string
	}{
		{
			name:      "a passing chunk times every file it ran, the helpers included",
			files:     map[string]string{"test-a.el": passing, "test-b.el": failing},
			chunk:     []string{"test-helpers.el", "test-a.el"},
			roster:    []string{"test-helpers.el", "test-a.el", "test-b.el"},
			wantExit:  0,
			wantItems: []string{"test-helpers.el", "test-a.el"},
		},
		{
			name:      "a chunk without the helpers still loads them first and times only its files",
			files:     map[string]string{"test-a.el": passing},
			chunk:     []string{"test-a.el"},
			roster:    []string{"test-helpers.el", "test-a.el"},
			wantExit:  0,
			wantItems: []string{"test-a.el"},
		},
		{
			name:      "a failing test fails the chunk with exit 1",
			files:     map[string]string{"test-b.el": failing},
			chunk:     []string{"test-b.el"},
			roster:    []string{"test-helpers.el", "test-b.el"},
			wantExit:  1,
			wantItems: []string{"test-b.el"},
		},
		{
			name:     "a chunk file off the roster is refused",
			files:    map[string]string{"test-a.el": passing},
			chunk:    []string{"test-a.el"},
			roster:   []string{"test-helpers.el"},
			wantExit: 2,
			wantOut:  "test-a.el is not on the roster",
		},
		{
			name: "a loaded test defined by a file off the roster breaks the run",
			files: map[string]string{
				"test-a.el": passing + "(load (expand-file-name \"stray.el\" (file-name-directory load-file-name)) nil t)\n",
				"stray.el":  "(ert-deftest fixture-stray () (should t))\n",
			},
			chunk:    []string{"test-a.el"},
			roster:   []string{"test-helpers.el", "test-a.el"},
			wantExit: 2,
			wantOut:  "no chunk would run it",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := ertFixture(t, tt.files)

			// Act
			out, exit := runDriver(t, dir, tt.chunk, tt.roster)

			// Assert
			if exit != tt.wantExit {
				t.Fatalf("exit = %d, want %d\n%s", exit, tt.wantExit, out)
			}
			if tt.wantOut != "" && !strings.Contains(out, tt.wantOut) {
				t.Fatalf("output lacks %q:\n%s", tt.wantOut, out)
			}
			if tt.wantItems == nil {
				return
			}
			if _, err := ParseItemLines([]byte(out), tt.wantItems); err != nil {
				t.Fatalf("item lines: %v\n%s", err, out)
			}
		})
	}
}

func TestERTDriverRefusesAnInteractiveEmacs(t *testing.T) {
	// Arrange: the driver's guard, evaluated with batch mode masked.
	form := "(let ((noninteractive nil)) (condition-case err (agent-repl-testrun-ert \"/\" nil nil) (user-error (princ (cadr err)) (kill-emacs 0))) (kill-emacs 1))"
	cmd := exec.Command("emacs", "-batch", "-Q", "-l", "ert", "-l", ertDriver(t), "--eval", form)
	cmd.Env = os.Environ()

	// Act
	out, err := cmd.CombinedOutput()

	// Assert
	if err != nil || !strings.Contains(string(out), "only for use in batch mode") {
		t.Fatalf("err = %v, out = %s", err, out)
	}
}

func TestERTDriverRunsOnlyTheNamedTests(t *testing.T) {
	// Arrange: a file with a passing and a failing test; only the passing one
	// is named.
	dir := ertFixture(t, map[string]string{"test-a.el": "(ert-deftest fixture-a-passes () (should t))\n(ert-deftest fixture-a-fails () (should nil))\n"})

	// Act
	out, exit := runDriverNamed(t, dir, []string{"test-a.el"}, []string{"test-helpers.el", "test-a.el"}, map[string][]string{"test-a.el": {"fixture-a-passes"}})

	// Assert
	if exit != 0 || !strings.Contains(out, "fixture-a-passes") || strings.Contains(out, "fixture-a-fails") {
		t.Fatalf("exit = %d, want 0 with only fixture-a-passes run:\n%s", exit, out)
	}
}

func TestERTDriverRefusesANameNoChunkFileDefines(t *testing.T) {
	// Arrange
	dir := ertFixture(t, map[string]string{"test-a.el": "(ert-deftest fixture-a-passes () (should t))\n"})

	// Act
	out, exit := runDriverNamed(t, dir, []string{"test-a.el"}, []string{"test-helpers.el", "test-a.el"}, map[string][]string{"test-a.el": {"fixture-nope"}})

	// Assert
	if exit != 2 || !strings.Contains(out, "test-a.el defines no test") || !strings.Contains(out, "fixture-nope") {
		t.Fatalf("exit = %d, want 2 naming the missing test:\n%s", exit, out)
	}
}

// twoFileFixture is a fixture of two files, each with a passing and a failing
// test.
func twoFileFixture(t *testing.T) string {
	t.Helper()
	return ertFixture(t, map[string]string{
		"test-a.el": "(ert-deftest fixture-a-passes () (should t))\n(ert-deftest fixture-a-fails () (should nil))\n",
		"test-b.el": "(ert-deftest fixture-b-passes () (should t))\n(ert-deftest fixture-b-fails () (should nil))\n",
	})
}

func TestERTDriverScopesEachFilesNamesToThatFile(t *testing.T) {
	tests := []struct {
		name    string
		names   map[string][]string
		exit    int
		ran     []string
		skipped []string
	}{
		{name: "two named files each run their own names",
			names: map[string][]string{"test-a.el": {"fixture-a-passes"}, "test-b.el": {"fixture-b-passes"}},
			exit:  0, ran: []string{"fixture-a-passes", "fixture-b-passes"}, skipped: []string{"fixture-a-fails", "fixture-b-fails"}},
		{name: "a whole file beside a named one runs whole",
			names: map[string][]string{"test-a.el": {"fixture-a-passes"}},
			exit:  1, ran: []string{"fixture-a-passes", "fixture-b-passes", "fixture-b-fails"}, skipped: []string{"fixture-a-fails"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := twoFileFixture(t)

			// Act
			out, exit := runDriverNamed(t, dir, []string{"test-a.el", "test-b.el"}, []string{"test-helpers.el", "test-a.el", "test-b.el"}, tt.names)

			// Assert
			if exit != tt.exit {
				t.Fatalf("exit = %d, want %d:\n%s", exit, tt.exit, out)
			}
			for _, name := range tt.ran {
				if !strings.Contains(out, name) {
					t.Errorf("%s did not run:\n%s", name, out)
				}
			}
			for _, name := range tt.skipped {
				if strings.Contains(out, name) {
					t.Errorf("%s ran, want it left out:\n%s", name, out)
				}
			}
		})
	}
}

func TestERTDriverRefusesNamesOutsideTheirFile(t *testing.T) {
	tests := []struct {
		name  string
		chunk []string
		names map[string][]string
		want  string
	}{
		{name: "a name the other chunk file defines", chunk: []string{"test-a.el", "test-b.el"},
			names: map[string][]string{"test-a.el": {"fixture-b-passes"}}, want: "test-a.el defines no test"},
		{name: "names for a file outside the chunk", chunk: []string{"test-a.el"},
			names: map[string][]string{"test-a.el": {"fixture-a-passes"}, "test-b.el": {"fixture-b-passes"}}, want: "test-b.el, which is not in the chunk"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := twoFileFixture(t)
			form := "(agent-repl-testrun-ert " + strconv.Quote(dir+"/") + " " + lispList(tt.chunk) + " " + lispList([]string{"test-helpers.el", "test-a.el", "test-b.el"}) +
				" " + lispNamesByFile([]string{"test-a.el", "test-b.el"}, tt.names) + ")"

			// Act
			out, exit := runDriverForm(t, dir, form)

			// Assert
			if exit != 2 || !strings.Contains(out, tt.want) {
				t.Fatalf("exit = %d, want 2 saying %q:\n%s", exit, tt.want, out)
			}
		})
	}
}
