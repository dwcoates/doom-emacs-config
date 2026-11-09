# An async merge is named by its own name in the merge queue

The owner (2026-11-09): in the sidebar's Merge Queue section, an async merge must be listed under the async merge's own name, not the name of the workspace that requested it, because one workspace can have many async merges in flight and they must be told apart in the queue. A workspace landing its own branch (a `workspace finished` included) keeps being listed under its own name.

This change ran as a one-shot workspace with contract changes pre-approved by the owner, so the design was settled here and landed directly. No field is added or retired: the change is to what `frontend.v1.RosterMergeQueueSubject.name` carries, stated in its comments.

## Core design principle: a merge is named by what it is, and credited to where it is drawn

- The principle: the subject's `name` names the merge, and its `workspace` names where a click takes the user.
- Consequences:
  - A sync merge is named by the landing workspace, and credited to it.
  - An async merge is named by its own name, and credited to its requester.

## Landed changes

### 1. `frontend.v1.RosterMergeQueueSubject.name` is the async merge's own name for an async merge

- What: for an async merge, `name` is its merge workspace's registry name, or, before that workspace is forked, the name its detached request reserved (`wsm.MergeSource.DetachName`). For a sync merge it stays the credited workspace's name.
- Why: the requester's name drew several async merges from one workspace identically (owner's screenshot: `escape-emacs-agent-repl` three times in one queue).
- Consequences: the daemon's `merge.creditOf` answers the name with the credit, carried on `sidebar.QueueEntry.AsyncName`. An entry whose `AsyncName` contradicts its `Async` is an invariant violation, recorded and not drawn. The webapp draws `name` verbatim, so it needs no change.

### 2. A click on an async merge still selects its requester and reveals the merge's own bubble

- What: `frontend.v1.RosterMergeQueueSubject.workspace` stays the requester, and `reveal` stays the head row of that merge's bubble on the requester's root feed.
- Why: the merge workspace is hidden until it needs the user. Selecting it from the queue would reveal it for good. The requester's feed already draws this very merge's bubble, so the click lands on the merge named, not just on its requester.
- Consequences: the queue controls that address a merge by `workspace` (move, move-to, evict) still resolve the requester. That gap is recorded in `org/todo.md`.

### 3. `frontend.v1.RosterMergeQueueHead.stages` states that a waiting rebase can precede the queue stage

- What: the comment now says a rebase the merge had while it waited in line belongs to its rebasing stage, which can therefore begin before the queue stage.
- Why: the owner's screenshot showed "rebasing, queue, tests" and asked whether it was a bug. The ledger showed each merge's waiting-member rebase began at about 11:50 and its queue tab only at admission at 12:08. The order is the contract's "the order each began".
- Consequences: none on the wire. The same diagnosis found the front's run overwriting the waiting rebase's ledger rounds (round numbers restarted at one on a shared ledger), fixed in the daemon (`merge.run.openLedger`).
