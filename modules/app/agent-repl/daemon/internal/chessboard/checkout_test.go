package chessboard

import (
	"errors"
	"os"
	"path/filepath"
	"testing"
)

// checkoutWithCLI makes a directory holding sdks/cli, as a checkout does.
func checkoutWithCLI(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, cliDir), 0o755); err != nil {
		t.Fatalf("make the cli dir: %v", err)
	}
	return dir
}

// envOf answers a getenv over a fixed map.
func envOf(vars map[string]string) func(string) string {
	return func(key string) string { return vars[key] }
}

func TestResolveCheckoutPrefersTheEngineDir(t *testing.T) {
	// Arrange.
	direct := checkoutWithCLI(t)
	root := t.TempDir()

	// Act.
	got, err := resolveCheckout(envOf(map[string]string{EngineDirEnv: direct, MultiRepoRootEnv: root}))

	// Assert.
	if err != nil || got != direct {
		t.Fatalf("resolveCheckout() = %q, %v; want %q", got, err, direct)
	}
}

func TestResolveCheckoutDerivesTheDirFromTheMultiRepoRoot(t *testing.T) {
	// Arrange.
	root := t.TempDir()
	want := filepath.Join(root, engineRepoName)
	if err := os.MkdirAll(filepath.Join(want, cliDir), 0o755); err != nil {
		t.Fatalf("make the checkout: %v", err)
	}

	// Act.
	got, err := resolveCheckout(envOf(map[string]string{MultiRepoRootEnv: root}))

	// Assert.
	if err != nil || got != want {
		t.Fatalf("resolveCheckout() = %q, %v; want %q", got, err, want)
	}
}

func TestResolveCheckoutRefusesWhenNothingNamesIt(t *testing.T) {
	// Act.
	_, err := resolveCheckout(envOf(nil))

	// Assert.
	if !errors.Is(err, errNoCheckout) {
		t.Fatalf("resolveCheckout() error = %v, want errNoCheckout", err)
	}
}

func TestResolveCheckoutRefusesATreeWithoutTheCLI(t *testing.T) {
	// Act.
	_, err := resolveCheckout(envOf(map[string]string{EngineDirEnv: t.TempDir()}))

	// Assert.
	if !errors.Is(err, errNoCheckout) {
		t.Fatalf("resolveCheckout() error = %v, want errNoCheckout", err)
	}
}

func TestResolveCheckoutRefusesACLIThatIsAFile(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, "sdks"), 0o755); err != nil {
		t.Fatalf("make sdks: %v", err)
	}
	if err := os.WriteFile(filepath.Join(dir, cliDir), nil, 0o644); err != nil {
		t.Fatalf("write the file: %v", err)
	}

	// Act.
	_, err := resolveCheckout(envOf(map[string]string{EngineDirEnv: dir}))

	// Assert.
	if !errors.Is(err, errNoCheckout) {
		t.Fatalf("resolveCheckout() error = %v, want errNoCheckout", err)
	}
}
