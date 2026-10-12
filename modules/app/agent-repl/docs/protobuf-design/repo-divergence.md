# The repository section's label hover shows ahead/behind of the primary branch

The owner asked (2026-10-11) that hovering a repository's name in the sidebar's
repository grouping show how many commits the repository is ahead of and behind
its primary branch, computed by the daemon asynchronously: once at startup and
every five minutes after.

## Interpretation

"Ahead of and behind the repository's primary branch" is read as the repository's
primary branch (the registry's `DefaultBranch`, the branch top-level merges land
on) counted against the remote branch it tracks (`<branch>@{upstream}`, e.g.
`origin/master`). The main worktree's own checkout is normally that very branch,
so counting the checkout against itself would always read zero; the upstream is
the only comparison that carries information. A count against a stale
remote-tracking ref would never move "behind", so every measurement fetches the
upstream's remote first. The owner gave the go-ahead to execute without a design
round, so this reading was taken as a judgment call, not a ruling.

## Landed changes

1. `frontend.v1.RosterRepoSection` gains `RosterRepoDivergence divergence = 6`,
   always set, with the oneof `state`: `pending` (nothing measured since the
   daemon started), `counted { branch, upstream, ahead, behind }`,
   `no_upstream { branch }` and `failed { reason }`.
   - Why on the section rather than on `RosterSectionHeader`: the header message
     is shared with the maturity stages, the hidden band and the merged band,
     none of which has a primary branch. A field there would mean nothing in
     three of its four uses.
   - Why a failed arm rather than keeping the last good count: a count whose
     fetch failed may be arbitrarily stale, and showing it as current would
     state a fact the daemon no longer knows. The failure's full evidence is in
     the daemon log under `daemon.repodivergence.*`; the arm carries a short
     reason for the hover.
   - Why `no_upstream` is not a failure: a branch that tracks nothing is an
     ordinary answer (a repository with no remote), not a measurement that broke.
   - Consumers: the webapp draws the arm as the label's `title`; Emacs decodes
     and validates it (its codec refuses unknown fields) and draws nothing.
   - Consequences accepted: one `git fetch <remote>` per registered repository
     every five minutes (network traffic, and remote-tracking refs move under
     the user's feet as any IDE's background fetch would); a repository
     registered after a sweep shows `pending` until the next one.
   - Sweep: nothing obviated; the change is purely additive.
