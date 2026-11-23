# The links menu, removing links and reading them

  - The menu shows at most five links, and a new link is shown while the
    menu has room. Once it is full, a newly set `-owned` link, or a relabel
    to `-owned` of an unshown one, is shown in place of the oldest shown
    reference (the least recently set). It is stored unshown only when the
    menu shows no reference. A new `-reference` link is stored unshown.
  - When a link went unshown, the answer's `links.menu` is `full` with
    `unshown` above zero. Then name the five the menu shows with `workspace
    link here -show URL -show URL ...`. A show that leaves an owned link
    unshown while it shows a reference is refused `owned_link_unshown`,
    naming `owned_url` and `reference_url`, and nothing changes. The one
    exception is when the workspace's owned links alone are more than five.
    Which links to rank first is the metaprompt's ("Workspace links").
  - `workspace link here -remove URL` removes one link, and
    `workspace link here -clear` removes them all.
  - `workspace status here` shows every stored link and its placement as
    `links`, each with its `ownership`: `reference`, or `owned` with a
    `follow` standing of `following` (with the subscription and its
    health), `ended` (merged or closed), `unsubscribed`, or `not_followable`
    (not a github.com pull request).
  - Only http and https urls are accepted; anything else is refused
    `invalid_link` with the reason.
