package workspace

import (
	"claude-repld/internal/account"
	"claude-repld/internal/wsm"
)

// accountRootFor answers the account root a workspace's session spends from.
//
// TWO INPUTS, IN ONE ORDER. The account package routes BY PATH and takes
// nothing else — that is its own invariant and it still holds. What sits above
// it is the reader's CHOICE (SelectAccount, owner ruling 2026-09-13): the
// account cell offers every root the daemon knows, and picking one makes this
// workspace's session spend as that account until somebody picks another. A
// choice therefore OUTRANKS the routing, and a workspace nobody has chosen for
// follows the routing exactly as it always has.
//
// It is read at every start, never inherited from the session row's own
// ConfigDir: that field records where the session last CAME UP, which is the
// fact portAcrossAccounts compares against to decide whether the transcript
// has to move.
func accountRootFor(accounts account.Resolver, dir string, session wsm.Session) string {
	if session.SelectedConfigDir != "" {
		return session.SelectedConfigDir
	}
	return accounts.ConfigDirFor(dir)
}

// spawnRootFor answers the account root a BOUNCE spawns its replacement shim
// under: the reader's choice first, then the root the session is recorded
// under, then the path routing.
//
// IT IS NOT accountRootFor, and the middle term is the difference. A cold
// bring-up re-decides the routing every time (daemon.md 10a), but a bounce
// replaces the process under a session that is already filed somewhere — so it
// spawns where that session LIVES, and only a choice moves it.
func spawnRootFor(accounts account.Resolver, dir string, session wsm.Session) string {
	if session.SelectedConfigDir != "" {
		return session.SelectedConfigDir
	}
	if session.ConfigDir != "" {
		return session.ConfigDir
	}
	return accounts.ConfigDirFor(dir)
}

// offersColdCompaction answers whether a session spending from CONFIGDIR is
// offered the harness's own compaction -- the cold gate's compact choice, a
// throwaway summarizing session the shim runs. The work (multi-repo) account
// never is: it does not pay for harness summaries. The vendor CLI's own
// auto-compaction is a different mechanism and is on for every account.
func offersColdCompaction(accounts account.Resolver, configDir string) bool {
	return !accounts.IsMultiRepo(configDir)
}
