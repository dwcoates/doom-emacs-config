package main

import (
	"fmt"
	"path/filepath"

	"agent-repld/internal/childproc"
)

// stableDir is the working directory a serving daemon runs in.
//
// THE DIRECTORY A DAEMON IS STARTED IN IS NOT ITS OWN. Emacs spawns it with
// whatever `default-directory` the current buffer has, and every successor and
// replacement a rollout spawns inherits it, so one launch from a workspace's
// buffer left every later daemon standing in that workspace's worktree. When
// the worktree was removed (its merge landed with cleanup), each child the
// daemon started without a directory of its own inherited a deleted one: the
// naming call's `claude -p` refused with "The current working directory was
// deleted", and every create that needed a name failed (2026-10-18, four
// one-shot dispatches). It is childproc.Stable, the directory a child with no
// directory of its own is started in.
const stableDir = childproc.Stable

// pathFlags is every path the command line hands the daemon, by flag name.
// --store-socket is not among them: parseFlags already refuses a relative one
// (resolveStoreSocket).
func pathFlags(opts options) map[string]string {
	return map[string]string{
		"state-dir":             opts.stateDir,
		"webapp-dist":           opts.webapp,
		"shim-main":             opts.shim,
		"prompts-dir":           opts.promptsDir,
		"multi-repo-config-dir": opts.multiRepoConfigDir,
		"default-config-dir":    opts.defaultConfigDir,
	}
}

// enterStableDir moves this process to stableDir and answers the directory it
// was started in, for the boot record.
//
// A RELATIVE PATH FLAG IS REFUSED FIRST: it named a path under the directory
// the daemon was started in, and after the move it would silently name one
// under stableDir instead.
func enterStableDir(opts options, getwd func() (string, error), chdir func(string) error) (string, error) {
	for name, path := range pathFlags(opts) {
		if path != "" && !filepath.IsAbs(path) {
			return "", fmt.Errorf("agent-repld: --%s %q is relative; the daemon runs in %s, so every path it is handed must be absolute", name, path, stableDir)
		}
	}
	startedIn, err := getwd()
	if err != nil {
		// A DELETED START DIRECTORY IS WHY THE MOVE EXISTS, so it is named in
		// the boot record rather than failing the boot.
		startedIn = fmt.Sprintf("unreadable: %v", err)
	}
	if err := chdir(stableDir); err != nil {
		return "", fmt.Errorf("agent-repld: move to %s: %w", stableDir, err)
	}
	return startedIn, nil
}
