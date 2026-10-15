package vitestmeasure

import (
	"bytes"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
)

func TestParseArgs(t *testing.T) {
	// Act: the webapp, found from this test's own place in the repository.
	root := filepath.Join("..", "..", "..", "..")
	a, err := ParseArgs([]string{"-package", filepath.Join(root, "webapp"), "-config", "vitest.integration.config.ts", "-mutate", "src/a.ts, src/b/**/*.ts", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if a.System != "webapp" || !filepath.IsAbs(a.Package) || a.Runs != 3 || !reflect.DeepEqual(a.Mutate, []string{"src/a.ts", "src/b/**/*.ts"}) {
		t.Fatalf("args = %+v", a)
	}
	if !strings.HasSuffix(a.Trace, filepath.Join("testrun", "vitest", "redundancy-trace.mjs")) {
		t.Fatalf("trace = %s", a.Trace)
	}
}

func TestParseArgsRefuses(t *testing.T) {
	tests := []struct {
		name string
		argv []string
		want string
	}{
		{name: "no -package", argv: []string{"-mutate", "src/a.ts", "-out", "/o"}, want: "-package is required"},
		{name: "no -mutate", argv: []string{"-package", ".", "-out", "/o"}, want: "-mutate is required"},
		{name: "a stray argument", argv: []string{"-package", ".", "-mutate", "a", "-out", "/o", "x"}, want: "unexpected arguments: x"},
		{name: "a package outside the repository", argv: []string{"-package", "/", "-mutate", "a", "-out", "/o"}, want: "holds testrun/roster"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := ParseArgs(tt.argv, &bytes.Buffer{})

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestParseArgsResolvesThePackagesRealPath(t *testing.T) {
	// Arrange: the webapp reached through a symlink, as /tmp is /private/tmp.
	abs, err := filepath.Abs(filepath.Join("..", "..", "..", "..", "webapp"))
	if err != nil {
		t.Fatal(err)
	}
	real, err := filepath.EvalSymlinks(abs)
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(t.TempDir(), "webapp")
	if err := os.Symlink(real, link); err != nil {
		t.Fatal(err)
	}

	// Act
	a, err := ParseArgs([]string{"-package", link, "-mutate", "src/a.ts", "-out", "/o"}, &bytes.Buffer{})

	// Assert
	if err != nil || a.Package != real {
		t.Fatalf("package = %q, %v, want %q", a.Package, err, real)
	}
}
