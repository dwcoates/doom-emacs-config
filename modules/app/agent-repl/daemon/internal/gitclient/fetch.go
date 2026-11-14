package gitclient

import (
	"context"
	"fmt"
	"os"
	"path/filepath"

	"agent-repld/internal/dlog"
)

// This file holds the daemon's ONE network git: FetchBranch.
//
// A FETCH THE DAEMON RUNS WRITES ONLY A REF NOTHING ELSE WRITES. A whole-remote
// `git fetch origin` updates every remote-tracking ref, so any other fetch in
// the same repository (the user's, an agent's, an editor's) racing it on ANY
// branch failed it with "cannot lock ref ... is at X but expected Y", and a
// create cut from origin was refused for a branch it never read (2026-11-05,
// explanation-engine). So the daemon names one branch and fetches it into its
// own namespace, refs/agent-repl/<remote>/<branch>, which no other process
// writes: no fetch outside the daemon can race it, structurally. It also writes
// no tag (--no-tags), no FETCH_HEAD (--no-write-fetch-head), recurses into no
// submodule and starts no maintenance, so a daemon fetch changes nothing in the
// user's view of the repository but its own ref.
//
// THE DAEMON'S OWN FETCHES OF ONE REPOSITORY ARE SERIALIZED by a record lock in
// the repository's common dir (fetchLockName): a create's fetch and the
// divergence sweep's, or two daemons' across a handover, would otherwise race
// each other on the same private ref. The lock is the kernel's and dies with
// its holder, the fetch's own git, which takes it through a helper process
// that holds no other lock (fetchlock.go).
//
// A GIT THAT PRUNES REFS (`git pack-refs`, `git gc`) LOCKS THE PRIVATE REF too,
// for as long as its pruning runs, and a fetch meeting that lock waits for it
// in git's own lock acquisition (core.filesRefLockTimeout, fetchRefLockWait)
// rather than failing at once on git's default bound.

// privateRefRoot is the namespace every daemon fetch writes into.
const privateRefRoot = "refs/agent-repl/"

// fetchLockName is the record lock, in the repository's common dir, that one
// daemon fetch of the repository holds at a time.
const fetchLockName = "agent-repl-fetch.lock"

// fetchRefLockWait is how long a fetch's git waits for a ref lock another git
// holds (core.filesRefLockTimeout, milliseconds): three times the slowest
// `git pack-refs --all` measured over this repository's 22,618 packed refs and
// 200 loose ones on a host about half busy (60ms of 40-60ms, 2026-11-14).
const fetchRefLockWait = "180"

// fetchEnv is the environment a fetch's git runs under beyond the scrubbed
// one: the helper's lock file and git, and the ref lock wait.
func fetchEnv(lockPath string) []string {
	return []string{
		fetchLockEnv + "=" + lockPath,
		fetchGitEnv + "=" + gitBinary,
		"GIT_CONFIG_COUNT=1",
		"GIT_CONFIG_KEY_0=core.filesRefLockTimeout",
		"GIT_CONFIG_VALUE_0=" + fetchRefLockWait,
	}
}

// PrivateRemoteRef is the ref FetchBranch writes remote's branch into: the one
// ref the daemon reads origin's view of that branch from.
func PrivateRemoteRef(remote, branch string) string {
	return privateRefRoot + remote + "/" + branch
}

// fetchRefspec is the forced refspec that fetches remote's branch into its
// private ref. Forced, because the private ref follows the remote wherever it
// goes, a force-push included, exactly as a remote-tracking ref would.
func fetchRefspec(remote, branch string) string {
	return "+refs/heads/" + branch + ":" + PrivateRemoteRef(remote, branch)
}

// FetchBranch fetches remote's branch into PrivateRemoteRef(remote, branch) and
// answers that ref. GIT_TERMINAL_PROMPT=0 (pinned on every git) makes a remote
// that wants a credential fail rather than wait. The fetch's git runs through
// the fetch lock's helper (this executable), which holds the repository's
// fetch lock for git's whole run. A failure is git's or the helper's, recorded
// at ERROR by run with the directory, the argument vector (the refspec) and
// the stderr that says which.
func (c *client) FetchBranch(ctx context.Context, repoDir, remote, branch string) (string, error) {
	const operation = "daemon.gitclient.fetch_branch"
	refspec := fetchRefspec(remote, branch)
	common, err := c.CommonDir(ctx, repoDir)
	if err != nil {
		return "", err
	}
	helper, err := os.Executable()
	if err != nil {
		c.log.Global().Error(operation, "the fetch lock's helper (this executable) could not be located", dlog.Context{
			"dir": repoDir, "refspec": refspec, "lock": filepath.Join(common, fetchLockName), "cause": err.Error(),
		})
		return "", fmt.Errorf("gitclient: fetching %s in %s: locating the fetch lock's helper: %w", refspec, repoDir, err)
	}
	if _, err := c.runVia(ctx, operation, helper, repoDir, fetchEnv(filepath.Join(common, fetchLockName)), "fetch", "--no-tags", "--no-write-fetch-head",
		"--recurse-submodules=no", "--no-auto-maintenance", remote, refspec); err != nil {
		return "", err
	}
	return PrivateRemoteRef(remote, branch), nil
}
