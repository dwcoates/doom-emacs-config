# Pull requests a workspace owns or follows

- **PULL REQUESTS.** The verbs for pull requests this workspace owns, and for
  following one it does not own (WS is usually `here`).
  - `workspace pr-comment WS -pr URL|OWNER/REPO#N (-body TEXT | -body-file
    PATH) [-in-reply-to COMMENT_ID]` posts a comment, or with `-in-reply-to`
    a reply in a line comment's thread, through the daemon.
  - Replies on an owned pull request ALWAYS go through `pr-comment`, never
    `gh pr comment` or `gh api` directly. The daemon records the comment's
    id and never tells the workspace its own reply; a reply posted any
    other way comes back to it as a new comment. The user comments under
    the same GitHub login, and those comments do reach the workspace.
  - `workspace pr-subscriptions WS` lists the pull requests the workspace
    follows, each with its health.
  - `workspace pr-subscribe WS -pr URL|#N|OWNER/REPO#N [-events
    comment,review,review_comment,state,checks,merge_queue,merged]` and
    `workspace pr-unsubscribe WS -pr URL|OWNER/REPO#N` follow a pull request
    in any GitHub repository. `merged` tells only the merge; `state` tells
    it too, along with a close, a reopen and readiness for review.
  - An agent whose work waits on a pull request merging, owned or not,
    follows it with `workspace pr-subscribe here -pr URL -events
    state,merge_queue` and NEVER polls GitHub by hand: no `gh pr view` loops,
    no background polling. The daemon ends the subscription on merge or
    close.
  - An owned pull request whose follow the user unsubscribed (its owned link
    shows `ownership.owned.follow.unsubscribed`) is followed again only with
    that narrowed event set, `workspace pr-subscribe here -pr URL -events
    state,merge_queue`. NEVER set the link `-owned` again to do it: that
    restores the full comment, review and checks stream the user turned off.
  - An agent DOES call `pr-subscribe` to follow a pull request it does not
    own whose merge its own work waits on, typically another workspace's,
    often in another repository, usually with `-events state,merge_queue`
    (`state` tells the merge, which ends the subscription; `merge_queue`
    tells its progress through GitHub's merge queue). Name it by url or
    `OWNER/REPO#N`: `#N` resolves only in this workspace's own repository.
    Store such a pull request as a `-reference` link too. A pull request that
    already ended is answered `already_ended` with its status, so subscribing
    late misses nothing.
