package tempdirs

import (
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"claude-repld/internal/dirpath"
	"claude-repld/internal/envc"
)

// canon is dirpath.Canonical for a test, failing it on error.
func canon(t *testing.T, dir string) string {
	t.Helper()
	out, err := dirpath.Canonical(dir)
	if err != nil {
		t.Fatalf("canonicalize %q: %v", dir, err)
	}
	return out
}

// guard builds the production guard (no test root) for this process.
func guard(t *testing.T) Guard {
	t.Helper()
	g, err := New(os.TempDir(), "")
	if err != nil {
		t.Fatalf("build the guard: %v", err)
	}
	return g
}

func TestCheckRefusesADirectoryInsideATemporaryRoot(t *testing.T) {
	processTemp := t.TempDir()
	tests := []struct {
		name string
		dir  string
		root string
	}{
		{name: "the /tmp root itself", dir: "/tmp", root: "/tmp"},
		{name: "beneath /tmp", dir: "/tmp/agent-repl-tempdirs-test/repo", root: "/tmp"},
		{name: "beneath /tmp in its resolved spelling", dir: canon(t, "/tmp") + "/agent-repl-tempdirs-test/repo", root: "/tmp"},
		{name: "beneath /var/tmp", dir: "/var/tmp/agent-repl-tempdirs-test/repo", root: "/var/tmp"},
		{name: "beneath /var/folders", dir: "/var/folders/ab/cdef/T/agent-repl-tempdirs-test", root: "/var/folders"},
		{name: "beneath the process's own temporary directory", dir: filepath.Join(processTemp, "repo"), root: os.TempDir()},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := guard(t)

			// Act
			err := g.Check(tc.dir)

			// Assert
			inside, ok := AsInside(err)
			if !ok {
				t.Fatalf("Check(%q) = %v, want an InsideError", tc.dir, err)
			}
			if want := canon(t, tc.dir); inside.Dir != want {
				t.Errorf("Dir = %q, want the canonical %q", inside.Dir, want)
			}
			if !within(inside.Dir, inside.Root) || !slices.Contains(g.Roots(), canon(t, tc.root)) {
				t.Errorf("Root = %q, want a root holding %q (%q among %v)", inside.Root, inside.Dir, canon(t, tc.root), g.Roots())
			}
		})
	}
}

func TestCheckAllowsADirectoryOutsideEveryTemporaryRoot(t *testing.T) {
	tests := []struct {
		name string
		dir  string
	}{
		{name: "an ordinary directory", dir: "/usr/local/agent-repl-tempdirs-test/repo"},
		{name: "a sibling sharing the /tmp prefix", dir: "/tmpfoo/repo"},
		{name: "a sibling sharing the resolved /tmp prefix", dir: canon(t, "/tmp") + "foo/repo"},
		{name: "a sibling sharing the /var/tmp prefix", dir: "/var/tmpfoo"},
		{name: "a sibling sharing the /var/folders prefix", dir: "/var/foldersx/repo"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := guard(t)

			// Act
			err := g.Check(tc.dir)

			// Assert
			if err != nil {
				t.Fatalf("Check(%q) = %v, want nil", tc.dir, err)
			}
		})
	}
}

func TestCheckJudgesASymlinkByWhereItLeads(t *testing.T) {
	tests := []struct {
		name    string
		target  string
		refused bool
		root    string
	}{
		{name: "a link into another temporary root is refused under that root", target: "/var/tmp", refused: true, root: "/var/tmp"},
		{name: "a link out of every temporary root is allowed", target: "/usr", refused: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := guard(t)
			link := filepath.Join(t.TempDir(), "link")
			if err := os.Symlink(tc.target, link); err != nil {
				t.Fatalf("symlink: %v", err)
			}

			// Act
			err := g.Check(link)

			// Assert
			inside, ok := AsInside(err)
			if !tc.refused {
				if err != nil {
					t.Fatalf("Check(%q -> %q) = %v, want nil", link, tc.target, err)
				}
				return
			}
			if !ok {
				t.Fatalf("Check(%q -> %q) = %v, want an InsideError", link, tc.target, err)
			}
			if want := canon(t, tc.root); inside.Root != want {
				t.Errorf("Root = %q, want %q (the link's target, not its location)", inside.Root, want)
			}
		})
	}
}

func TestCheckRefusesARelativeDirectoryAsAFailureNotATemporaryAnswer(t *testing.T) {
	// Arrange
	g := guard(t)

	// Act
	err := g.Check("relative/repo")

	// Assert
	if err == nil {
		t.Fatal("Check of a relative dir = nil, want an error")
	}
	if _, ok := AsInside(err); ok {
		t.Fatalf("Check of a relative dir = %v, an InsideError; want a canonicalization failure", err)
	}
}

func TestCheckOnAnUnbuiltGuardPanics(t *testing.T) {
	// Arrange
	var g Guard
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("Check on the zero Guard did not panic")
		}
	}()

	// Act
	_ = g.Check("/usr")
}

func TestInsideErrorNamesTheDirectoryAndTheRoot(t *testing.T) {
	// Arrange
	err := &InsideError{Dir: "/private/tmp/scratch", Root: "/private/tmp"}

	// Act
	got := err.Error()

	// Assert
	want := "/private/tmp/scratch is inside the temporary directory /private/tmp; agent-repl does not register temporary folders"
	if got != want {
		t.Fatalf("Error() = %q, want %q", got, want)
	}
}

func TestRootsAreCanonicalSortedAndDistinct(t *testing.T) {
	// Arrange
	want := []string{canon(t, "/tmp"), canon(t, "/var/folders"), canon(t, "/var/tmp")}
	slices.Sort(want)

	// Act
	got, err := Roots("/tmp")

	// Assert
	if err != nil {
		t.Fatalf("Roots: %v", err)
	}
	if !slices.Equal(got, want) {
		t.Fatalf("Roots(/tmp) = %v, want %v", got, want)
	}
}

func TestRootsRefusesAMisconfiguredTemporaryDirectory(t *testing.T) {
	tests := []struct {
		name   string
		tmpdir string
	}{
		{name: "empty", tmpdir: ""},
		{name: "the filesystem root", tmpdir: "/"},
		{name: "relative", tmpdir: "tmp"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := Roots(tc.tmpdir)

			// Assert
			if err == nil {
				t.Fatalf("Roots(%q) = nil error, want a refusal", tc.tmpdir)
			}
		})
	}
}

func TestTestRootExemptsOnlyWhatLiesBeneathIt(t *testing.T) {
	testRoot := t.TempDir()
	sibling := t.TempDir()
	tests := []struct {
		name    string
		dir     string
		refused bool
	}{
		{name: "the test root itself", dir: testRoot, refused: false},
		{name: "beneath the test root", dir: filepath.Join(testRoot, "repo"), refused: false},
		{name: "a temporary sibling of the test root", dir: sibling, refused: true},
		{name: "the temporary root above the test root", dir: "/tmp", refused: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g, err := New(os.TempDir(), testRoot)
			if err != nil {
				t.Fatalf("New: %v", err)
			}

			// Act
			err = g.Check(tc.dir)

			// Assert
			if _, inside := AsInside(err); inside != tc.refused {
				t.Fatalf("Check(%q) = %v, want refused=%v", tc.dir, err, tc.refused)
			}
		})
	}
}

func TestNewRefusesATestRootThatExemptsNothingOrEverything(t *testing.T) {
	tests := []struct {
		name     string
		testRoot string
	}{
		{name: "outside every temporary root", testRoot: "/usr"},
		{name: "a temporary root itself", testRoot: "/tmp"},
		{name: "relative", testRoot: "scratch"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			_, err := New(os.TempDir(), tc.testRoot)

			// Assert
			if err == nil {
				t.Fatalf("New(testRoot=%q) = nil error, want a refusal", tc.testRoot)
			}
		})
	}
}

func TestFromEnvHonorsTheTestRootOnlyUnderTheVendorGuard(t *testing.T) {
	testRoot := t.TempDir()
	tests := []struct {
		name     string
		forbid   string
		testRoot string
		wantErr  bool
		exempted bool
	}{
		{name: "a live daemon with no test root", forbid: "", testRoot: "", wantErr: false, exempted: false},
		{name: "a live daemon handed a test root refuses to boot", forbid: "", testRoot: testRoot, wantErr: true},
		{name: "a test-run daemon honors its test root", forbid: "1", testRoot: testRoot, wantErr: false, exempted: true},
		{name: "a test-run daemon with no test root refuses temporary folders", forbid: "1", testRoot: "", wantErr: false, exempted: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			getenv := func(key string) string {
				if key == EnvTestRoot {
					return tc.testRoot
				}
				return ""
			}

			// Act
			g, err := FromEnv(envc.Load(), os.TempDir(), getenv)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("FromEnv error = %v, wantErr %v", err, tc.wantErr)
			}
			if err != nil {
				return
			}
			_, refused := AsInside(g.Check(filepath.Join(testRoot, "repo")))
			if refused == tc.exempted {
				t.Fatalf("a dir beneath the test root: refused=%v, want exempted=%v", refused, tc.exempted)
			}
		})
	}
}

func TestARefusalIsMarkedSeamMissingOnlyOnATestRunWithNoTestRoot(t *testing.T) {
	testRoot := t.TempDir()
	tests := []struct {
		name     string
		forbid   string
		testRoot string
		want     bool
	}{
		{name: "a live daemon", forbid: "", testRoot: "", want: false},
		{name: "a test-run daemon whose harness stated no root", forbid: "1", testRoot: "", want: true},
		{name: "a test-run daemon refusing outside its stated root", forbid: "1", testRoot: testRoot, want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv(envc.EnvForbidVendorCalls, tc.forbid)
			g, err := FromEnv(envc.Load(), os.TempDir(), func(key string) string {
				if key == EnvTestRoot {
					return tc.testRoot
				}
				return ""
			})
			if err != nil {
				t.Fatalf("FromEnv: %v", err)
			}

			// Act
			inside, ok := AsInside(g.Check("/var/tmp/agent-repl-seam-test"))

			// Assert
			if !ok {
				t.Fatal("Check of a /var/tmp folder was not refused")
			}
			if inside.SeamMissing != tc.want {
				t.Fatalf("SeamMissing = %v, want %v", inside.SeamMissing, tc.want)
			}
		})
	}
}

func TestMissingSeamNamesTheVariableToSet(t *testing.T) {
	// Arrange
	err := &InsideError{Dir: "/tmp/x", Root: "/tmp", SeamMissing: true}

	// Act
	said := err.MissingSeam()

	// Assert
	if !strings.Contains(said, EnvTestRoot) || !strings.Contains(said, "/tmp/x") {
		t.Fatalf("MissingSeam() = %q, want it to name %s and the dir", said, EnvTestRoot)
	}
}
