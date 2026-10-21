package sandboxscripts

import (
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
)

// The sandbox image compiles the Go dependency graph in a layer of its own,
// keyed by WHICH packages are imported (the go-deps-list stage), and only
// then the checkout's own packages. These cases pin both halves: the list
// names no package of ours, and no Go source reaches the image before the
// dependency layer.

// dockerfile reads sandbox/Dockerfile.
func dockerfile(t *testing.T) string {
	t.Helper()
	b, err := os.ReadFile(filepath.Join(filepath.Dir(filepath.Dir(sandboxScript(t))), "Dockerfile"))
	if err != nil {
		t.Fatalf("read Dockerfile: %v", err)
	}
	return string(b)
}

// goDepsTemplate is the `go list -f` template the go-deps-list stage runs.
func goDepsTemplate(t *testing.T) string {
	t.Helper()
	m := regexp.MustCompile(`go list -e -deps -test \\\s*\n\s*-f '([^']+)'`).FindStringSubmatch(dockerfile(t))
	if m == nil {
		t.Fatalf("the Dockerfile has no `go list -e -deps -test -f '...'` step")
	}
	return m[1]
}

// TestTheGoDependencyListNamesNoPackageOfOurs runs the Dockerfile's own
// template over a fixture: a main module whose package and test import the
// standard library and a module reached through a local `replace`, the shape
// of every module here. Only standard-library packages may come out.
func TestTheGoDependencyListNamesNoPackageOfOurs(t *testing.T) {
	t.Parallel()
	// Arrange
	root := t.TempDir()
	writeFile(t, filepath.Join(root, "lib", "go.mod"), "module example.com/lib\n\ngo 1.24\n")
	writeFile(t, filepath.Join(root, "lib", "lib.go"), "package lib\n\nimport \"strings\"\n\nfunc Up(s string) string { return strings.ToUpper(s) }\n")
	writeFile(t, filepath.Join(root, "app", "go.mod"),
		"module example.com/app\n\ngo 1.24\n\nrequire example.com/lib v0.0.0\n\nreplace example.com/lib => ../lib\n")
	writeFile(t, filepath.Join(root, "app", "app.go"), "package app\n\nimport \"example.com/lib\"\n\nfunc Run() string { return lib.Up(\"x\") }\n")
	writeFile(t, filepath.Join(root, "app", "app_test.go"), "package app\n\nimport (\n\t\"bytes\"\n\t\"testing\"\n)\n\nfunc TestRun(t *testing.T) { _ = bytes.NewBuffer(nil) }\n")
	cmd := exec.Command("go", "list", "-e", "-deps", "-test", "-f", goDepsTemplate(t), "./...")
	cmd.Dir = filepath.Join(root, "app")
	cmd.Env = append(os.Environ(), "GOFLAGS=-mod=mod", "GOWORK=off", "GOPROXY=off")

	// Act
	out, err := cmd.Output()

	// Assert
	if err != nil {
		t.Fatalf("go list = %v", err)
	}
	got := strings.Fields(string(out))
	for _, want := range []string{"strings", "bytes", "testing"} {
		if !slices.Contains(got, want) {
			t.Errorf("the list lacks the standard-library import %q", want)
		}
	}
	for _, pkg := range got {
		if strings.HasPrefix(pkg, "example.com/") {
			t.Errorf("the list names %q, a package of the checkout's own", pkg)
		}
	}
}

// TestNoGoSourceReachesTheImageBeforeTheDependencyLayer: in the image's own
// stage, the first COPY of go-sources comes after the dependency compile, or
// every source edit would rebuild that compile and give each image its own copy.
func TestNoGoSourceReachesTheImageBeforeTheDependencyLayer(t *testing.T) {
	t.Parallel()
	// Arrange
	body := dockerfile(t)
	froms := regexp.MustCompile(`(?m)^FROM .*$`).FindAllStringIndex(body, -1)
	final := body[froms[len(froms)-1][0]:]

	// Act
	deps := strings.Index(final, "xargs -r go build")
	sources := strings.Index(final, "go-sources/")

	// Assert
	if deps < 0 {
		t.Fatalf("the image's stage compiles no dependency list")
	}
	if sources >= 0 && sources < deps {
		t.Fatalf("go-sources reach the image's stage before the dependency compile")
	}
}
