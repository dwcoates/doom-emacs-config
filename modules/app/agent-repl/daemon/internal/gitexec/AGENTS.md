# daemon/internal/gitexec/

The single builder for every git command the daemon runs.

`gitexec.Command(ctx, dir, args...)` returns `git -C dir args...` with the
inherited repository bindings stripped from the child environment, so `-C dir`
is the ONLY thing that selects a repository.

## Why the stripping exists

Git exports its repository bindings into every process a HOOK runs:

- `GIT_DIR`
- `GIT_INDEX_FILE`
- `GIT_WORK_TREE`
- `GIT_OBJECT_DIRECTORY`
- `GIT_COMMON_DIR`
- `GIT_PREFIX`

Those bindings OUTRANK or silently redirect `-C dir`. A daemon (or a test run,
or a build) launched from a repository's pre-commit hook therefore inherits a
pointer to the HOOK'S repository, and an unstripped `git -C /some/worktree ...`
reads and WRITES that other repository instead of the one the caller named.

This is not hypothetical. The repository's own pre-commit hook runs this very
Go suite, and the leak has already:

- hung the merge end-to-end tests, because a fixture's cherry-pick was being
  driven against the live checkout rather than the temp-dir fixture, and
- flipped `core.bare` on the live checkout, which breaks `git` and `go build`
  for everything else until it is manually unset.

A misdirected READ is a wrong answer; a misdirected WRITE corrupts a real
working tree. Stripping makes both structurally impossible rather than
improbable, which is why it lives in one place instead of being re-derived per
package.

## Rules

- NEVER build an `exec.Cmd` for `git` anywhere in the daemon by hand. Call
  `gitexec.Command`.
- NEVER narrow `gitexec.StrippedVars` to "the ones that matter". Every variable
  in the list is a repository selector, and the cheap removal is the whole
  point.
- Test fixtures that shell out to git for ARRANGE steps are subject to the same
  hazard (their `git commit` would rewrite the caller's real index). They use
  `gitexec.Command`, or `gitexec.StripEnv` when they must assemble their own
  command.

## Consumers

- `internal/workspace/merge` (`merge.Driver`'s rebase-and-merge driver)
- `internal/workspace/geometry` (branch and main-worktree derivation)
- `internal/workspace/postmerge` (worktree/trunk probing)
- `internal/reload`
