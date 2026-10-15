package suites

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestRunEnsure(t *testing.T) {
	tests := []struct {
		name    string
		script  string
		wantErr string
	}{
		{name: "a script that succeeds is run with its arguments and the caller's env", script: "#!/bin/sh\necho \"$1 $RUN_ENSURE_PROBE\" > \"$(dirname \"$0\")/called\"\n"},
		{name: "a failure names what failed and carries the whole output", script: "#!/bin/sh\necho on stdout\necho on stderr >&2\nexit 3\n", wantErr: "suites: make it ready: exit status 3\non stdout\non stderr\n"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			path := filepath.Join(dir, "ensure.sh")
			write(t, path, tt.script, 0o755)
			t.Setenv("RUN_ENSURE_PROBE", "inherited")

			// Act
			err := runEnsure("make it ready", path, "arg")

			// Assert
			if tt.wantErr != "" {
				if err == nil || err.Error() != tt.wantErr {
					t.Fatalf("err = %q, want %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			got, readErr := os.ReadFile(filepath.Join(dir, "called"))
			if readErr != nil || string(got) != "arg inherited\n" {
				t.Fatalf("script saw %q (%v), want its argument and the inherited env", got, readErr)
			}
		})
	}
}

func TestEveryEnsureScriptRunsThroughRunEnsure(t *testing.T) {
	// A hand-rolled exec of an ensure script would drift from runEnsure's
	// environment and error shape; only ensure.go may collect combined output.
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	for _, f := range files {
		if strings.HasSuffix(f, "_test.go") || f == "ensure.go" {
			continue
		}
		src, err := os.ReadFile(f)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(string(src), "CombinedOutput(") {
			t.Errorf("%s collects a command's combined output itself; run it through runEnsure", f)
		}
	}
}
