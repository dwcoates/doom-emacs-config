package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestWebappLayerCoverageEnvHandsEachDriverADirectoryOfItsOwn(t *testing.T) {
	// Arrange
	root := t.TempDir()

	// Act
	first, err := webappLayerCoverageEnv(root)
	if err != nil {
		t.Fatal(err)
	}
	second, err := webappLayerCoverageEnv(root)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	dirs := map[string]bool{}
	for _, env := range [][]string{first, second} {
		if len(env) != 1 || !strings.HasPrefix(env[0], WebappLayerCoverageEnv+"=") {
			t.Fatalf("env = %v, want one %s assignment", env, WebappLayerCoverageEnv)
		}
		dir := strings.TrimPrefix(env[0], WebappLayerCoverageEnv+"=")
		if filepath.Dir(dir) != filepath.Join(root, "webapp-layer") {
			t.Fatalf("%s is not under %s/webapp-layer", dir, root)
		}
		if st, err := os.Stat(dir); err != nil || !st.IsDir() {
			t.Fatalf("%s was not made: %v", dir, err)
		}
		dirs[dir] = true
	}
	if len(dirs) != 2 {
		t.Fatalf("two drivers share %v", dirs)
	}
}

func TestWebappLayerCoverageEnvIsNothingWithoutCoverage(t *testing.T) {
	// Act
	env, err := webappLayerCoverageEnv("")

	// Assert
	if err != nil || env != nil {
		t.Fatalf("env = %v, %v, want nothing", env, err)
	}
}

func TestWebappLayerCoverageEnvRefusesAnUnwritableRoot(t *testing.T) {
	// Arrange: the root is a file, so nothing can be made under it.
	root := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(root, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := webappLayerCoverageEnv(root)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the webapp layer's coverage directory") {
		t.Fatalf("err = %v, want the directory named", err)
	}
}
