# Waiting on another repository's change

- **WAITING ON ANOTHER REPOSITORY'S CHANGE.** When this workspace's work
  must wait until another workspace's change (often in another repository)
  has landed. Never poll for any of these.
  - The pull request is known: `workspace pr-subscribe here -pr URL -events
    state,merge_queue`. Act when the `state` notice says it merged.
  - The pull request is not opened yet: `workspace subscribe OTHER -events
    pull_request,closed`, naming OTHER by its workspace name, in any
    repository. On the `pull_request` notice, `pr-subscribe` to the pull
    request it names, then `workspace unsubscribe OTHER` if nothing else is
    needed from it. `workspace status OTHER` shows the pull requests it
    already owns under `links`.
  - A change landing through agent-repl's own merge queue (no pull request)
    is told by `workspace subscribe OTHER` (the default lifecycle bundle
    tells landed, then deployed when the running build carries it, or evicted
    with a reason; `-events landed` alone when only the landing matters).
    OTHER need not have a merge in line yet. The queue as a whole is told by
    `/agent-repl-merge-queue`'s `merge-queue subscribe -repository DIR`, which
    tells only depleted, paused and resumed.
