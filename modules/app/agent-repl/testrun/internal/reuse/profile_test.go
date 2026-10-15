package reuse

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// THE PROFILE IS THE GUARANTEE, so it is tested against the kernel itself:
// each case runs a real process under a real profile. The testrun suite is
// never confined (roster: it has no Reuse), because a sandboxed process cannot
// start another sandbox.

func TestTheProfileRefusesEveryReadOutsideTheInputSet(t *testing.T) {
	tests := []struct {
		name    string
		argv    func(root string) []string
		allowed bool
	}{
		{"a file in a flat directory", catOf("pkg/a.go"), true},
		{"a file in a tree", catOf("prompts/p.md"), true},
		{"a file in a generated tree", catOf("pkg/gen/out.js"), true},
		{"a placed file", catOf("go.mod"), true},
		{"a flat directory's listing", lsOf("pkg"), true},
		{"a file outside the set", catOf("other/x.go"), false},
		{"a file in a flat directory's subdirectory", catOf("pkg/sub/deep.go"), false},
		{"a flat directory's subdirectory's listing", lsOf("pkg/sub"), false},
		{"a listed directory inside a flat one", lsOf("pkg/listed"), true},
		{"a file in a listed directory inside a flat one", catOf("pkg/listed/l.go"), false},
		{"a generated tree's listing inside a flat directory", lsOf("pkg/gen"), true},
		{"a flat directory inside a flat one", lsOf("pkg/inner"), true},
		{"a file of a flat directory inside a flat one", catOf("pkg/inner/i.go"), true},
		{"the checkout root's listing", lsOf("."), false},
		{"the metadata of a file outside the set", func(root string) []string {
			return []string{"/usr/bin/stat", filepath.Join(root, "other", "x.go")}
		}, false},
		{"a listed setuid tool", func(string) []string { return []string{"/bin/ps", "-o", "pid=", "-p", "1"} }, true},
		{"a file outside the set, read by a child process", func(root string) []string {
			return []string{"/bin/sh", "-c", "cat " + filepath.Join(root, "other", "x.go")}
		}, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := writeTree(t, baseFiles)
			write(t, root, "pkg/listed/l.go", "package listed")
			write(t, root, "pkg/inner/i.go", "package inner")
			set := baseSet(t)
			for _, err := range []error{set.AddDir("pkg/listed"), set.AddFlatDir("pkg/inner")} {
				if err != nil {
					t.Fatal(err)
				}
			}
			profile, err := Profile(root, []string{root}, set)
			if err != nil {
				t.Fatal(err)
			}
			path := filepath.Join(t.TempDir(), "p.sb")
			if err := os.WriteFile(path, []byte(profile), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			out, err := exec.Command(SandboxExec, append([]string{"-f", path}, tt.argv(root)...)...).CombinedOutput()

			// Assert
			if allowed := err == nil; allowed != tt.allowed {
				t.Fatalf("allowed = %v, want %v: %v\n%s", allowed, tt.allowed, err, out)
			}
			if !tt.allowed && !strings.Contains(string(out), "Operation not permitted") {
				t.Fatalf("the refusal was not the sandbox's:\n%s", out)
			}
		})
	}
}

func TestTheProfileRefusesASiblingCheckoutWhole(t *testing.T) {
	// Arrange
	root := writeTree(t, baseFiles)
	sibling := writeTree(t, map[string]string{"pkg/a.go": "package pkg"})
	profile, err := Profile(root, []string{root, sibling}, baseSet(t))
	if err != nil {
		t.Fatal(err)
	}
	path := filepath.Join(t.TempDir(), "p.sb")
	if err := os.WriteFile(path, []byte(profile), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	out, err := exec.Command(SandboxExec, "-f", path, "/bin/cat", filepath.Join(sibling, "pkg", "a.go")).CombinedOutput()

	// Assert
	if err == nil {
		t.Fatalf("a sibling checkout's copy of an input was readable:\n%s", out)
	}
}

func TestAProfileRootMustBeAbsolute(t *testing.T) {
	// Act
	_, err := Profile("relative", nil, NewInputSet())

	// Assert
	if err == nil {
		t.Fatal("a relative checkout root was accepted")
	}
}

func TestAFlatDirectoryWithAQuoteInItsPathIsRefused(t *testing.T) {
	// Arrange
	set := NewInputSet()
	if err := set.AddFlatDir(`we"ird`); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := Profile("/r", []string{"/r"}, set)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "double quote") {
		t.Fatalf("err = %v, want the quote refused", err)
	}
}

func catOf(p string) func(string) []string {
	return func(root string) []string { return []string{"/bin/cat", filepath.Join(root, filepath.FromSlash(p))} }
}

func lsOf(p string) func(string) []string {
	return func(root string) []string { return []string{"/bin/ls", filepath.Join(root, filepath.FromSlash(p))} }
}
