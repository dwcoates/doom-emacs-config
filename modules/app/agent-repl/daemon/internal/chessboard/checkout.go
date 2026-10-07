package chessboard

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
)

// The environment the explanation-engine checkout is named by. These are the
// CEE CLI's own variables (explanation-engine sdks/cli/internal/repopath), read
// in the CEE CLI's own order, so the daemon and the cee-webapp it starts
// resolve the same tree; the daemon then states the resolved tree to
// cee-webapp explicitly rather than relying on the two resolving alike.
const (
	// EngineDirEnv names the checkout directly.
	EngineDirEnv = "CEE_AGENT_EXPLANATION_ENGINE_DIR"
	// MultiRepoRootEnv names the directory the checkout sits in, under its
	// conventional name.
	MultiRepoRootEnv = "MULTI_REPO_ROOT"
)

// engineRepoName is the checkout's conventional directory name under the
// multi-repo root.
const engineRepoName = "explanation-engine"

// cliDir is the CEE CLI's directory inside the checkout.
const cliDir = "sdks/cli"

// errNoCheckout is a checkout no environment variable names, or one that does
// not hold the CEE CLI.
var errNoCheckout = errors.New("chessboard: no explanation-engine checkout")

// resolveCheckout answers the explanation-engine checkout: EngineDirEnv when
// set, else MultiRepoRootEnv's explanation-engine. The resolved tree must hold
// the CEE CLI; one that does not is reported, never guessed past.
func resolveCheckout(getenv func(string) string) (string, error) {
	dir := getenv(EngineDirEnv)
	if dir == "" {
		root := getenv(MultiRepoRootEnv)
		if root == "" {
			return "", fmt.Errorf("%w: neither %s nor %s is set", errNoCheckout, EngineDirEnv, MultiRepoRootEnv)
		}
		dir = filepath.Join(root, engineRepoName)
	}
	info, err := os.Stat(filepath.Join(dir, cliDir))
	if err != nil {
		return "", fmt.Errorf("%w: %s holds no %s: %v", errNoCheckout, dir, cliDir, err)
	}
	if !info.IsDir() {
		return "", fmt.Errorf("%w: %s's %s is not a directory", errNoCheckout, dir, cliDir)
	}
	return dir, nil
}
