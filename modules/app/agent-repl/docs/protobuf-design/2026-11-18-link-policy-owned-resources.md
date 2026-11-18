# Link policy: owned resources, GNS subpages, owned links first (2026-11-18)

The owner asked for three link rules. Every resource a workspace's work
creates is stored as an owned link at once (a harness rule, no contract
change). A GNS subpage under a GNS page the workspace already owns gets no
separate link, which the daemon enforces. And the summary menu prefers owned
links over references, which the daemon also enforces. The last two each add
one refusal arm to `agentrepl.v1.ChangeWorkspaceLinksError`.

Designed autonomously (the default mode); every assumption is stated below.

## Assumptions

- GNS url structure, from the GNS CLI's own `gns open --help` and the urls
  seen across this machine's transcripts and skills (observed behavior):
  the Knowledge UI addresses a key as `/key/<key>` (its page), `/keys/<key>`
  (a prefix browse) and `/app/<key>` (an app stored at the key, with view
  state in the query). Keys are dotted names (`teams.core.docs`). Other
  routes (`/agents`, `/pipes`, `/search`, `/api`, `/tools`) name no key in
  the hierarchy. `gns://` deep links never reach the daemon, since a link
  must be http or https.
- A GNS host is one whose name carries "gns", the rule the topbar's link
  icon already used. It now lives in one place, `daemon/internal/gnsurl`.

## Landed changes

### `agentrepl.v1.ChangeWorkspaceLinksError.subpage_of_owned_link` (arm 11)

- WHAT: a new refusal arm, `agentrepl.v1.ChangeWorkspaceLinksSubpageOfOwnedLink`,
  carrying the refused `url` and the `owned_page_url` of the owned link whose
  GNS page contains it.
- WHY: the owner's rule is that a workspace adding a subpage under a GNS page
  it already owns stores no separate link, and that the daemon enforces it.
- "Subpage of" means: both urls are GNS pages on the same host, and the
  subpage's key extends the owned page's key by one or more dotted segments.
  The route plays no part, so an `/app/teams.x.review.app` is a subpage of an
  owned `/key/teams.x`. A page is never its own subpage, so relabeling the
  owned page is stored. `teams.core-chess` is not under `teams.core`, because
  the match is on whole dotted segments.
- Only an OWNED set is checked. A subpage stored as a reference, or one under
  a page the workspace holds only as a reference, is stored as usual.
- Why a refusal arm and not a success arm: nothing was recorded, which is
  what every `ChangeWorkspaceLinksError` arm says. The agent's harness tells
  it never to make such a set, so meeting the arm means the agent broke the
  rule, and the arm names the page that already stands for the subpage.
- Consequences:
  - The check reads the workspace's owned links inside the set's own
    transaction (`daemon/internal/wsm/workspacelinks.go`
    `refuseSubpageOfOwnedInTx`), so a subpage can never slip in beside an
    owned page that another set is storing at the same time.
  - The store logs the refusal at DEBUG (`isLinkRefusal` in
    `daemon/internal/wsm/open.go`'s `writeTx`), and the verb records it once
    as the INFO refusal record.
  - Storing a parent page owned after its subpages were stored owned is
    allowed. The subpages' links stay, since nothing removes links on the
    agent's behalf.
