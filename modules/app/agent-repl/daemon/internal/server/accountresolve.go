// accountresolve.go holds the ONE rule that decides which Claude account a
// workspace's CLI runs under.
//
// THE RULE, in full:
//
//  1. The account a HUMAN SELECTED for this workspace, through the webapp's
//     account switcher (registry.Record.ConfigDirOverride). A selection is
//     durable, it survives every later bring-up, and a workspace created from
//     a workspace that carries one inherits it.
//  2. Otherwise the account the workspace's PATH names:
//     $MULTI_REPO_ROOT's account under that root, the CLI's default elsewhere
//     (session.AccountConfigDirFor).
//
// THERE IS NO THIRD INPUT. Not the account a source session happened to run
// under, not one a create command named, not a per-workspace key in the
// editor's state file. Each of those was a second answer to a question this
// rule already answers, and a second answer that can disagree is what put one
// workspace's session in ~/.claude while its parent repo — and every
// transcript anyone would look for — lived under ~/.claude-chesscom.
//
// EMPTY MEANS NO SELECTION, everywhere in this file. An explicit selection of
// the default account is carried as that root's absolute path, so the
// "unset vs. deliberately default" collision that produced the bug cannot be
// written down at all.
package server

import (
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// AccountResolver answers "which account does this workspace run under?" from
// the durable selection plus the path, so no caller has to combine them.
type AccountResolver struct {
	// Reg is the persistent session registry the selection is read from.
	// A nil registry means no selection can be consulted, and the path
	// answers alone.
	Reg *registry.Registry
	// Logf receives the resolution account. Optional.
	Logf func(string, ...any)
}

func (r AccountResolver) logf(format string, args ...any) {
	if r.Logf != nil {
		r.Logf(format, args...)
	}
}

// SelectionFor reports the account a human selected for the workspace at cwd,
// or "" when nobody has selected one.
//
// It reads the NEWEST record carrying a selection rather than the newest
// record: a workspace accumulates many records, every restore supersedes the
// last, and only some of them were written after the switch. Taking the newest
// record outright would lose the selection the moment an older record sorted
// last, which is the same accident that once let a stale tombstone shadow a
// live conversation.
func (r AccountResolver) SelectionFor(cwd string) string {
	if r.Reg == nil || cwd == "" {
		return ""
	}
	var selection, from string
	for _, rec := range r.Reg.All() {
		if rec.CWD != cwd || rec.ConfigDirOverride == "" {
			continue
		}
		if selection == "" || rec.CreatedAt > from {
			selection, from = rec.ConfigDirOverride, rec.CreatedAt
		}
	}
	return selection
}

// Resolve returns the account for the workspace at cwd: the selection when one
// exists, else the account cwd's path names.
//
// pathHint is used when cwd does not exist yet — a create resolving its account
// before the worktree is planned passes the repo it is cut from.
func (r AccountResolver) Resolve(cwd, pathHint string) (string, error) {
	if selection := r.SelectionFor(cwd); selection != "" {
		r.logf("account-resolve: cwd=%q SELECTED config_dir=%q — a human chose this account and it outranks the path",
			cwd, selection)
		return selection, nil
	}
	path := cwd
	if path == "" {
		path = pathHint
	}
	routed, err := session.AccountConfigDirFor(path)
	if err != nil {
		return "", err
	}
	r.logf("account-resolve: path=%q config_dir=%q — nobody has selected an account for this workspace, so its path answers",
		path, routed)
	return routed, nil
}

// InheritSelection returns the selection a NEW workspace takes from the source
// workspace it was created from, or "" when the source has none.
//
// ONLY THE SELECTION TRAVELS. The source's resolved ConfigDir deliberately
// does not: a parent that merely sits under $MULTI_REPO_ROOT has chosen
// nothing, and letting its resolved account ride along would move a child
// whose own path answers differently. A child with no source workspace at all
// therefore has nothing to inherit and falls to its path — which is why a
// one-shot pinned to a repo lands on that repo's account no matter which
// workspace the keystroke came from.
func (r AccountResolver) InheritSelection(sourceCWD string) string {
	if sourceCWD == "" {
		return ""
	}
	selection := r.SelectionFor(sourceCWD)
	if selection != "" {
		r.logf("account-resolve: INHERITING selection config_dir=%q from source cwd=%q", selection, sourceCWD)
	}
	return selection
}
