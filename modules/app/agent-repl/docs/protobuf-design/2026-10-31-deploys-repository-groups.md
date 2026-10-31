# Deploys: one group per repository

The Deploys section drew every repository's merge queue in one list at its
top, then a single "Recently Deployed" heading with one "(K today, last D
ago)" across every repository, then one merged group per repository. The
owner asked (2026-10-31) for the section to be organized by repository
instead: each repository is one group, headed by its name and its own
"(K today, last D ago)", with its queue (while one is active) above its
recent deploys. A group with an active queue opens by itself and one
without folds by itself; a click on the name folds or opens it, and the
automatic rule wins again the moment the queue changes. A double click on
the name pauses or resumes the repository's queue.

Design mode: autonomous.

## Assumptions

- The objective, in the owner's terms: "The RECENTLY DEPLOYED subsection
  should be made repository-specific in the main deploy queues section."
- Systems: the daemon's sidebar resolver
  (`daemon/internal/resolve/sidebar/sections.go`), the webapp's sidebar
  (`webapp/src/sidebar/`), and the Emacs wire codec and roster walker
  (`lisp/wire-roster.el`, `lisp/roster.el`), which decode the section
  strictly and walk its merged rows for the workspace switcher.
- Non-additive: the section's separate queue list, its "Recently Deployed"
  heading, and the merged group's repository-fold arm all retire.

## Iteration sequence

One increment: `frontend.v1.RosterMergedSection` and its group message.

## Landed changes

### 1. A Deploys group is one repository: its queue and its recent deploys

WHAT changed:
- `frontend.v1.RosterMergedRepoGroup` gains a `activity` oneof:
  `queued` (carrying the repository's `frontend.v1.RosterRepoMergeQueue`)
  or `idle`. The set arm is whether the repository has an active queue.
- `frontend.v1.RosterMergedRepoGroup.drawn` (the `shown` /
  `repository_collapsed` arms) retires. A group no longer follows its
  repository section's fold: it has a fold of its own, decided on the page.
- `frontend.v1.RosterMergedSection.queues`, `merged_label` and the message
  `frontend.v1.RosterMergeQueueRepoGroup` retire. The section's `groups`
  holds one group per repository with an active queue or a deploy in the
  last 24 hours, in the repository sections' order.

WHY: the owner wants the queue for a repository drawn above that
repository's recent deploys, under one header carrying that repository's
deploy summary.

Consequences:
- The group's fold is page-local, as the "Recently Deployed" expansion it
  replaces was: a glance at the rail, never shared across pages or kept
  across a restart. Its default is read off the `activity` arm (queued is
  open, idle is folded), and a click overrides it until the arm or the
  queue's line changes, or the user leaves the rail.
- The queue's "(N)" count of merges in line retires with
  `frontend.v1.RosterMergeQueueRepoGroup.count`: the group header carries the
  deploy summary instead, as the owner's agreed layout draws it.
- The group's "(K today, last D ago)" stays computed on the page from its
  merged rows' instants, as the section-wide one was, because it ticks with
  the clock (the count drops at midnight, the age counts up).
- The Emacs roster walker walks every merged row as not collapsed: a merged
  workspace has no tab, and no repository fold hides it any longer.
- The merged rows of a collapsed repository section are now drawn in its
  Deploys group. Rejected alternative: keep hiding the group with the
  repository's fold. It would hide an active queue behind a fold the owner
  never asked to govern it, and a group folds by itself already.
