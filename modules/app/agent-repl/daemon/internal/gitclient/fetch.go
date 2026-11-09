package gitclient

import (
	"context"
	"fmt"
	"path/filepath"

	"agent-repld/internal/dlog"
	"agent-repld/internal/inflight"
	"agent-repld/internal/recordlock"
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
// its holder (internal/recordlock).

// privateRefRoot is the namespace every daemon fetch writes into.
const privateRefRoot = "refs/agent-repl/"

// fetchLockName is the record lock, in the repository's common dir, that one
// daemon fetch of the repository holds at a time.
const fetchLockName = "agent-repl-fetch.lock"

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
// that wants a credential fail rather than wait. A failure is git's, recorded
// at ERROR by run with the directory, the argument vector (the refspec) and
// git's own stderr.
func (c *client) FetchBranch(ctx context.Context, repoDir, remote, branch string) (string, error) {
	const operation = "daemon.gitclient.fetch_branch"
	refspec := fetchRefspec(remote, branch)
	common, err := c.CommonDir(ctx, repoDir)
	if err != nil {
		return "", err
	}
	lock, err := holdFetchLock(ctx, filepath.Join(common, fetchLockName))
	if err != nil {
		c.log.Global().Error(operation, "the repository's fetch lock could not be taken", dlog.Context{
			"dir": repoDir, "refspec": refspec, "lock": filepath.Join(common, fetchLockName), "cause": err.Error(),
		})
		return "", fmt.Errorf("gitclient: fetching %s in %s: %w", refspec, repoDir, err)
	}
	defer lock.Release()
	if _, err := c.run(ctx, operation, repoDir, "fetch", "--no-tags", "--no-write-fetch-head",
		"--recurse-submodules=no", "--no-auto-maintenance", remote, refspec); err != nil {
		return "", err
	}
	return PrivateRemoteRef(remote, branch), nil
}

// holdFetchLock waits for the exclusive record lock at PATH, or for CTX to
// end. A wait CTX abandons goes on in the background and releases the lock the
// moment it is granted, so an abandoned wait never keeps the lock.
func holdFetchLock(ctx context.Context, path string) (*recordlock.Record, error) {
	f, err := recordlock.OpenFile(path)
	if err != nil {
		return nil, err
	}
	type granted struct {
		rec *recordlock.Record
		err error
	}
	wait := make(chan granted, 1)
	// inflight: joined: the function that starts it waits for it.
	go func() {
		rec, err := recordlock.BlockExclusive(f, path)
		wait <- granted{rec, err}
	}()
	select {
	case g := <-wait:
		return g.rec, g.err
	case <-ctx.Done():
		inflight.Standing(func() {
			if g := <-wait; g.err == nil {
				g.rec.Release()
			}
		})
		return nil, ctx.Err()
	}
}
