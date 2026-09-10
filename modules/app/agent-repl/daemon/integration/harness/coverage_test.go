package harness

import (
	"os"
	"path/filepath"
	"slices"
	"testing"
)

func TestCoverageRootReadsTheEnvironment(t *testing.T) {
	tests := []struct {
		name    string
		set     bool
		value   string
		want    string
		enabled bool
	}{
		{name: "unset is off"},
		{name: "empty is off", set: true},
		{name: "a directory is on", set: true, value: "/tmp/cov", want: "/tmp/cov", enabled: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			os.Unsetenv(CoverageEnvVar)
			if tc.set {
				t.Setenv(CoverageEnvVar, tc.value)
			}

			// Act.
			root, enabled := CoverageRoot(), CoverageEnabled()

			// Assert.
			if root != tc.want {
				t.Errorf("CoverageRoot() = %q, want %q", root, tc.want)
			}
			if enabled != tc.enabled {
				t.Errorf("CoverageEnabled() = %v, want %v", enabled, tc.enabled)
			}
		})
	}
}

func TestCoverageDirCreatesThePerBinaryDirectory(t *testing.T) {
	// Arrange.
	root := t.TempDir()

	// Act.
	dir, err := CoverageDir(root, "claude-repld")

	// Assert.
	if err != nil {
		t.Fatalf("CoverageDir: %v", err)
	}
	if want := filepath.Join(root, "claude-repld"); dir != want {
		t.Fatalf("CoverageDir = %q, want %q", dir, want)
	}
	info, err := os.Stat(dir)
	if err != nil || !info.IsDir() {
		t.Fatalf("CoverageDir did not create %s: %v", dir, err)
	}
}

func TestCoverageDirIsInertWhenTheRootIsEmpty(t *testing.T) {
	// Arrange, Act.
	dir, err := CoverageDir("", "claude-repld")

	// Assert.
	if err != nil {
		t.Fatalf("CoverageDir: %v", err)
	}
	if dir != "" {
		t.Fatalf("CoverageDir = %q, want the empty string", dir)
	}
}

func TestCoverageDirRefusesAnUnnamedBinary(t *testing.T) {
	// Arrange, Act.
	_, err := CoverageDir(t.TempDir(), "")

	// Assert.
	if err == nil {
		t.Fatal("CoverageDir accepted an unnamed binary; want an error")
	}
}

func TestCoverageDirReportsAnUncreatableDirectory(t *testing.T) {
	// Arrange: a regular file where the root must be a directory.
	file := filepath.Join(t.TempDir(), "not-a-dir")
	if err := os.WriteFile(file, nil, 0o644); err != nil {
		t.Fatalf("write the blocking file: %v", err)
	}

	// Act.
	_, err := CoverageDir(file, "claude-repld")

	// Assert.
	if err == nil {
		t.Fatal("CoverageDir accepted an uncreatable directory; want an error")
	}
}

func TestCoverageEnvNamesGocoverdir(t *testing.T) {
	// Arrange.
	root := t.TempDir()

	// Act.
	env, err := CoverageEnv(root, "shim-store")

	// Assert.
	if err != nil {
		t.Fatalf("CoverageEnv: %v", err)
	}
	want := []string{"GOCOVERDIR=" + filepath.Join(root, "shim-store")}
	if !slices.Equal(env, want) {
		t.Fatalf("CoverageEnv = %v, want %v", env, want)
	}
}

func TestCoverageEnvIsInertWhenTheRootIsEmpty(t *testing.T) {
	// Arrange, Act.
	env, err := CoverageEnv("", "shim-store")

	// Assert.
	if err != nil {
		t.Fatalf("CoverageEnv: %v", err)
	}
	if env != nil {
		t.Fatalf("CoverageEnv = %v, want nil", env)
	}
}

func TestNodeCoverageEnvNamesTheShimDirectory(t *testing.T) {
	// Arrange.
	root := t.TempDir()

	// Act.
	env, err := NodeCoverageEnv(root)

	// Assert.
	if err != nil {
		t.Fatalf("NodeCoverageEnv: %v", err)
	}
	want := []string{"NODE_V8_COVERAGE=" + filepath.Join(root, NodeCoverageDirName)}
	if !slices.Equal(env, want) {
		t.Fatalf("NodeCoverageEnv = %v, want %v", env, want)
	}
}

func TestNodeCoverageEnvIsInertWhenTheRootIsEmpty(t *testing.T) {
	// Arrange, Act.
	env, err := NodeCoverageEnv("")

	// Assert.
	if err != nil {
		t.Fatalf("NodeCoverageEnv: %v", err)
	}
	if env != nil {
		t.Fatalf("NodeCoverageEnv = %v, want nil", env)
	}
}

func TestCoverageBuildArgsInstrumentOnlyWhenEnabled(t *testing.T) {
	tests := []struct {
		name string
		root string
		want []string
	}{
		{name: "off", root: "", want: nil},
		{name: "on", root: "/tmp/cov", want: []string{"-cover"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := CoverageBuildArgs(tc.root)

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("CoverageBuildArgs(%q) = %v, want %v", tc.root, got, tc.want)
			}
		})
	}
}
