# Storing links

- **LINKS.** The topbar's summary opens this workspace's links: a click on
  it opens the one shown link, or drops a menu of the shown links' labels,
  most recently set first, when there are several.
  - The metaprompt says when to store one ("Workspace links").
  - `workspace link here -url URL -label TEXT (-owned [-announces URL] |
    -reference)` stores a
    link, or relabels one already stored. The label is two to eight words
    in one sentence; any other is refused `invalid_label` with the reason.
  - Exactly one of `-owned` and `-reference` is named on every set, and a
    relabel names it anew.
    - `-owned`: this workspace's work made what the link points at (a pull
      request it opened, a doc or a ticket it wrote).
    - `-reference`: it exists apart from the work, which only points at it
      (the thread or ticket the task came from, a doc it read, a pull
      request it did not open, even one its work amends).
    - `-announces URL2` (only with `-owned`) names the stored link this one
      announces, such as a Slack post announcing a pull request. URL2 must
      already be one of this workspace's links, else it is refused
      `unknown_announced_link`. A relabel sets the announcement anew.
      `workspace status here` shows it as `ownership.owned.announces.url`.
    - The daemon posts nothing to Slack. What a Slack thread is told is the
      repository's lifecycle hooks'. The emoji-ack finish default stores the
      announcing link, and the emoji-ack post-deploy default reacts on it.
    - The daemon follows an owned github.com pull request: every comment,
      review, line comment, edit, deletion, state change and finished check
      arrives as a notice (a deferred prompt the feed draws no row for). It
      ends the subscription itself when the pull request merges or closes,
      when the link is removed, or when it is set again as `-reference`.
    - A pull request has ONE owning workspace. Storing it `-owned` while
      another open workspace owns it is refused `pull_request_owned_elsewhere`,
      naming the owner; store it `-reference` instead.
    - A GNS page (`https://<gns host>/key/<key>`, `/keys/<key>` or
      `/app/<key>`) whose dotted key extends the key of a GNS page this
      workspace already holds `-owned` (`teams.x.notes` under `teams.x`) is
      a subpage of it. Storing such a subpage `-owned` is refused
      `subpage_of_owned_link`, naming the owned page as `owned_page_url`,
      and nothing is stored.
