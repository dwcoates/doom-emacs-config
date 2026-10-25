# Parsed prose: the daemon parses every tree, the webapp only draws

The metaprompt asks every response to be a numbered TLDR tree. The webapp used
to parse those trees itself, line by line, from the raw markdown the daemon
served. The owner wants trees to be first-class: prompts that answer a tree's
labels (`3.1.2 - my answer`), links from a tree's labels to the prompts that
answered them, and error correction of the trees the model writes (missing or
wrong labels). That needs one parse, in one place, that every program reads.

## Core design principles

### The daemon is the only program that parses prose

- **In the owner's terms:** "I'm not sure ANY service other than daemon needs to
  actually do any parsing -- daemon can parse with the treesitter grammar, and
  ship the parsed representation over the wire."
- **Consequences:** every wire field the webapp renders as markdown, and that can
  hold a tree, carries `frontend.v1.ParsedProse` instead of a string. The webapp
  drops `web-tree-sitter`, its wasm assets, and the boot-time grammar load. The
  daemon links the tree-sitter-tldr grammar through cgo.
- **Reopens:** the earlier landing on this branch where the webapp parsed with the
  grammar's wasm (`webapp/src/tree-parse.ts`, `tree-grammar.ts`).
- **Does not claim:** that Emacs never parses. The composer may still parse the
  prompt being typed (tldr_prompt) for highlighting, because that text has not
  reached the daemon yet.

### A tree is a graph, not lines

- **In the owner's terms:** "This should be a relational datastructure", then
  "flatten it out into a single list, and use identifiers to relate nodes".
- **Consequences:** the rails and connectors are not on the wire; the webapp draws
  them from the graph. Nodes are one flat list in reading order, each with an id
  (its index) and, for a branch, its parent's id. Children are NOT listed: the
  owner asked whether to include them for convenience, and they were left out so
  the two directions can never disagree, and because a children list would change
  on every streaming push while parent-only nodes never change once served.
- **Does not claim:** that the webapp loses its wrapper. It still wraps each node to
  the bubble's live width, which only the webapp can measure.

### A label is derived from the node's place

- **In the owner's terms:** missing labels are filled in, wrong ones fixed ("it can
  fix ones it got wrong too"), numbered by position ("rule B, count position").
  The owner proposed ordering first and then filling gaps, which always ends at
  numbering by position, so labels are simply derived from structure.
- **Consequences:** `ProseTreeNode.label` is always the parent's label, a dot, and
  the node's position among its siblings; roots count 1, 2, 3. Labels are unique by
  construction, so a prompt reference names exactly one node. A root line with a
  marker but no label (`🔧 text` at column 0) becomes a root once a branch follows
  it ("If they have branches under them, then they can be retroactively fixed").
- **Accepted cost:** prose that cites the model's own labels ("see 3.2") can point at
  the wrong node after renumbering.
- **Requires:** a full set of test cases for the repair (owner: "we'll want a number
  of test cases for this error correction tree code").

## Landed changes

### 1. `frontend/v1/prose.proto` and the fields that carry it

- **What:** new messages `ParsedProse`, `ProseBlock` (arms `ProsePlain`, `ProseTree`),
  `ProseTree`, `ProseTreeHeader`, `ProseTreeNode` (place arms `ProseTreeNodeRoot`
  with its marker, `ProseTreeNodeBranch` with its parent), `ProseTreeNodeId`,
  `ProseTreeNodeMarker`, `ProseTreeNodeLabel`, `ProseTreeNodeText`, `ProseTreeCode`.
  The string field of each of these became `ParsedProse prose` under a new number,
  the old number and name reserved: `FeedResponseProse.markdown`,
  `FeedPlanProse.markdown`, `FeedContextCutSummary.markdown`,
  `FeedSubagentResultReport.text`, `FeedPeerMessage.body` (now `prose = 4`, beside
  master's `from = 3`), and `FeedTextBlock.text`. `FeedQuoteBlock` keeps master's
  shape (`heading`, `quoted`, `closing`): master retired its markdown `text` so the
  quoted bubble is drawn verbatim as a code block, never parsed, which leaves no
  prose in it to carry. `HeldPrompt` gained `FeedUserPromptBody body`, the drawing of
  its `said`, because the held card used to draw the record type directly and so had
  nowhere to carry a parse; `said` stays the payload that resends and classifier
  forms read.
- **Why:** these are the places the webapp drew text through the tree-aware
  markdown path (`webapp/src/bubble/body.ts` `markdownSlot`). Texts drawn with plain
  `renderMarkdown` (skill documents, news digests, markdown tool output) never drew
  trees and stay strings.
- **Decisions inside the shape:**
  - A root's marker sits on the root arm, a branch's parent on the branch arm, so a
    marker on a branch or a parent on a root is unrepresentable.
  - Every drawn element (header, label, marker, text) is its own message, per the
    package's figma→idl convention.
  - A tree inside a language-less code fence is found by the grammar's own fence rule,
    not `markdown-it`'s. The owner accepted the rare difference (`~~~` fences, fences
    indented four spaces): "yeah".
- **Consequences:**
  - Lines the model already wrapped are rejoined BY STRUCTURE in the daemon: a line
    under a node's rails with no connector and no label continues that node. The
    webapp's column comparison (which used its width model) is not ported, so Go
    holds no second width model.
  - The blank lines between a tree's roots are no longer taken from the text; the
    webapp draws its own spacing.
  - Malformed rails (stray indents, ASCII connectors, broken vertical rails) are
    redrawn correctly instead of shown as written.
  - While a response streams, a later push only appends nodes: served ids, labels and
    parents never change. A node may gain a code block once a later line shows the
    block was beneath it. The digits of a label still being typed on the last line are
    not drawn as text.
  - The held card draws `body` through the same block vocabulary as the prompt row
    it becomes, so an image is the daemon's resolved `src`: a host-path image draws
    as the picture rather than its path, and one the daemon cannot resolve arrives
    as an unsupported block, which the held card draws as nothing.
  - The webapp's tests draw trees from the daemon's own parse: the daemon's prose
    suite writes `webapp/test/fixtures/parsed-prose.json` from
    `prose-inputs.json` and fails while it is stale.
  - The golden answers recorded from the old webapp parser
    (`tree-sitter-tldr/tldr/test/golden/golden.json`) move to the daemon: its reading
    of the grammar's parse is held to them, below the structure and repair layers.

### 2. `ParsedProse.source`, and a root without a marker

- **What:** `ParsedProse` gained `ProseSource source`: the text with its main tree's
  labels rewritten in place to the labels its nodes carry. `ProseTreeNodeRoot.marker`
  became optional.
- **Why:**
  - The daemon reads served text back: `daemon/internal/resolve/feed/selection.go`
    takes a selected bubble's markdown for the Emacs response view's search and for
    the copy a reply prepends to the prompt. With the parse on the wire, that text
    had no home. Carrying the repaired text keeps one parse (no second renderer of
    trees back into text in Go) and makes a quoted response name the same labels
    the reader sees, so the agent and the person agree on what `3.1.2` means.
  - A fragment of a tree can open on a branch with no root above it
    (`├── 1.1 Detail`). By its place it is a root, so it has no parent, and it has
    no marker to carry.
- **Consequences:**
  - Only the main tree is repaired in `source`; a tree inside a code fence keeps its
    written labels there. A quoted response was already repaired when it was served,
    so its quote carries repaired labels anyway.
  - The webapp reads `source` only to count a text's characters for its log records.
- **Retraction of change 1's rejoin consequence:** rejoining wrapped lines by
  structure is not needed. The tree region already ends at the first line that is
  neither a root, a branch, a blank line nor an interior fence (findTreeRegion's rule,
  ported exactly), so a wrapped line the model wrote never sits inside a tree. The
  error was assuming the region kept continuation lines; it never did.

### 3. `HeldIngressPrompt.body`, the ingress card's drawing

- **What:** no new field. Master gave `HeldIngressPrompt` its `FeedUserPromptBody body = 4`
  while this change was in flight, drawn by the same call as `HeldPrompt.body`. Its text
  blocks are `FeedTextBlock`s, so change 1 makes them carry the daemon's parse too.
- **Why:** an ingress card is a prompt the editor wrote while no daemon answered, and
  it must draw the same blocks its prompt row will draw.
- **Consequences:**
  - The daemon's ingress card draws exactly the blocks its prompt row will draw once
    the queue accepts the entry.
  - An entry the EDITOR draws while no daemon is in reach (`EditorIngressEntry`) has
    no daemon to parse it, so the webapp draws its record: its words as the markdown
    they were typed as (`webapp/src/bubble/body.ts` `unparsedProse`), a tree in them
    drawn as markdown rather than as a tree. This is the one place the webapp draws
    text no daemon served, and it parses nothing there either.

### 4. A prompt's entries, and the responses they answer

- **What:** `ProseBlock` gained the arm `ProseEntry entry = 3` (`ProseEntryLabel`,
  `ProseEntryText`), and `FeedResponse` gained `repeated FeedResponseAnswer answers =
  10` (`label`, `prompt`).
- **Why:** the owner's original ask: a prompt answers a response's tree by label
  (`3.1.2 - my answer`), and a label in the response links to the prompt that
  answered it. Both ends need the parse, which only the daemon does.
- **Decisions inside the shape:**
  - Only a PROMPT's text is read for entries (the `tldr_prompt` grammar, linked into
    the daemon through `tree-sitter-tldr/tldr_prompt/src`). A response's numbered
    lines are its tree's.
  - A line opening on a label and a marker is a pasted tree's root, not an entry: the
    grammar's own heading rule, with the marker the deciding token.
  - An entry's text is markdown, not a nested parse: blocks stay flat.
  - The link lives on the RESPONSE (`answers`), because that is the row the reader
    clicks a label in; an entry finds the response it answers by the rule below, so
    the prompt carries no reference of its own.
  - **The binding rule:** an entry answers the nearest response before its prompt, on
    the same feed, whose tree holds a node with the entry's label. A reply to an older
    response's label therefore binds to that response as long as no newer one reuses
    the label.
- **Consequences:**
  - A response is re-published when a prompt answering it is drawn, and when such a
    prompt is removed by a rollback.

### 5. Clicking a linked label or an entry, and resending an edited prompt

- **What:**
  - `SelectFeedRowRequest.move` gained `answer` (`SelectFeedRowAnswer`: the
    answering prompt's row and the label) and `entry` (`SelectFeedRowEntry`: a
    prompt's row and one entry's label).
  - `frontend.v1.FeedSelectionRowViewport` gained `reveal`
    (`FeedSelectionRowViewportReveal.entry_label`).
  - `WatchHostWorkspaceResponse` gained the event `prompt_entry_clicked`
    (`HostWorkspacePromptEntryClicked`: row, label, text).
  - `RollBackRequest` gained `optional RollBackResend resend` (an idempotency key,
    an origin, and an `edit` oneof: `whole` with a `UserSaid`, or `entry` with a
    label and its new text); `RollBackSuccess` gained `optional SubmitPromptResponse
    resend`; `RollBackError.cause` gained `entry_not_found`.
- **Why:** the owner's original ask. Clicking a label selects and reveals the prompt
  that answered it, flashing the entry. Clicking an entry fills the composer with
  it, and RET then rolls the conversation back to that prompt and resends it with
  that entry edited; RET on any selected prompt resends with the composer's words.
- **Decisions inside the shape:**
  - The resend rides the rollback request, not a second call after it: the daemon
    submits it in the same handler, so no Emacs round trip sits between the cut and
    the resend, and an entry naming a label the prompt does not hold is refused
    before anything is rolled back.
  - The resend's answer is SubmitPrompt's own response type, so a refused resend
    reads exactly as a refused send does.
  - Revealing the answering prompt moves the feed (it is elsewhere), unlike a bubble
    click, which never does: a viewport arm of its own says so.

### 6. A prompt's entries drawn as their tree, and the composer's entries answered live

- **What:**
  - `ProseBlock.entry` (tag 3) is retired. A run of a prompt's entries is one
    `ProseEntryTree entries = 4`: nodes in reading order, each a `root` or a `branch`
    under an earlier node, labelled, and either an `answer` (the entry's words) or a
    `path` (an ancestor a deeper entry's label implies).
  - `agentrepl.v1.ReportComposerEntries` (the composer's entry labels and the one
    point is on), answered with state: `frontend.v1.FeedComposerEntry` on the root
    feed's watch (`WatchFeedResponse.composer_entry`, tag 6) and
    `agentrepl.v1.HostComposerEntries` on the host view (`HostWorkspace.composer_entries`,
    tag 7).
  - `agentrepl.v1.FindEntryNeighbor` (the next or previous node of an entry's target
    tree).
  - `conversation.v1.UserQuoteBlock.quoted_row` and
    `frontend.v1.FeedQuoteBlock.quoted_row`: the row a reply quoted.
  - `HostWorkspacePromptEntryClicked` moved to tag 13 of `WatchHostWorkspaceResponse`:
    master took 12 for `select_requested` first.
- **Why:** the owner's asks of 2026-10-24.
  - A sent prompt's entries read as the tree of the labels they answer (labels only,
    "option B").
  - While an entry is typed in Emacs, the node it answers is highlighted in the feed,
    that node's whole subtree as one box, from the column the node's label starts at
    to the widest highlighted line (first its own line only; the owner widened it on
    2026-10-24), until point leaves the line.
  - A label that answers nothing is marked in the composer.
  - `o` and `O` on an entry line open the next or previous node's entry.
  - An explicitly selected response is what entries answer, while typing and once
    sent.
- **Decisions inside the shape:**
  - **An entry is its one line.** The lines after it are the prompt's prose, and a
    line of prose between two entries ends the run; blank lines do not.
  - **The tree is the labels'**, built by the daemon with no response read: `5.5.1`
    implies `5` and `5.5`. Siblings keep the order their first entry appeared in; two
    entries answering one label are two sibling nodes.
  - **One target rule** (`daemon/internal/resolve/feed/composer.go` `entryTarget`)
    binds a sent prompt's entries and the composer's: a reply to a response with a
    tree answers that response when it holds the label, and nothing otherwise; any
    other entry answers the nearest earlier response holding the label. A reply
    names its response by the quote's `quoted_row`, which the daemon records at
    submit; a quote recorded before it existed binds as any prompt does.
  - **While a prompt is selected the composer answers nothing**: it is editing a sent
    prompt's entry (or replying to a prompt), so nothing is highlighted and no label
    is marked.
  - **The highlight and the marks are state the daemon restates**, on a report, a
    selection change and any change to a response's tree, so a late page or a new
    response is never left with a stale highlight. Emacs reports one call at a time,
    the latest owed (`lisp/core.el` `agent-repl-serial-send`).
  - **The neighbor is asked for the line the key was pressed on**, so its answer can
    never belong to a different line.
  - **The webapp draws the highlight as one stylesheet rule** over the node's lines
    (`webapp/src/feed/composer-entry.ts`), so a repaint of the response cannot drop it.

### 7. Answering a response's branch from the feed

- **What:**
  - `SelectFeedRowRequest.move` gained `branch` (`SelectFeedRowBranch`: a response's
    row and one node's label, tag 10).
  - `WatchHostWorkspaceResponse` gained the event `compose_entry`
    (`HostWorkspaceComposeEntry`: the label, tag 14).
  - `feed.Resolver.ResponseNode` answers whether a response's tree holds a node.
- **Why:** the owner's ask of 2026-10-24.
  - A response's branch is boxed while the pointer is on it, drawn as the composer's
    box is (`webapp/src/feed/subtree-box.ts`).
  - A click on it selects the response, opens a blank `LABEL - ` entry on a line of
    its own at the composer's end, and puts the keyboard there in insert state with
    point right after it.
- **Decisions inside the shape:**
  - **The click never toggles.** It answers the node, so the response stays the one
    replied to, as a click on a prompt's entry keeps its prompt.
  - **The click names the node, not its text.** Emacs opens the entry itself
    (`agent-repl-prompt-entries-opening`, the words `o` and `O` open with), so the
    event carries only the label.
  - **The hover is the webapp's alone.** It is drawn from the pointer and changes no
    state, so no daemon round trip sits under the pointer.

### 8. A prompt's entry reveals the response node it answers

- **What:**
  - `SelectFeedRowEntry` now selects the response the entry answers, by the one target
    rule (`feed.Resolver.EntryTarget`), and never the prompt.
  - `frontend.v1.FeedSelectionRowViewport` gained `reveal_node`
    (`FeedSelectionRowViewportRevealNode.label`, tag 4).
  - `WatchHostWorkspaceResponse.prompt_entry_clicked` (section 5) is retired: tag 13 and
    its name are reserved, and `HostWorkspacePromptEntryClicked` is gone.
- **Why:** the owner's ruling of 2026-10-24. A click on a prompt's entry should not
  fill the composer at all. It should highlight the branch the entry answers, with
  its subtree, in that response.
- **Decisions inside the shape:**
  - **The click selects the response**, centered as a linked label's click centers
    its prompt, so a collapsed response opens and the node is in view.
  - **The box is the selection's**: it stands while the selection naming the node
    does, and the next selection push takes it down
    (`webapp/src/feed/revealed-node.ts`).
  - **The click asks nothing of Emacs.** RET on a selected prompt still resends the
    composer's words whole. `RollBackResend.edit.entry` has no client since the click
    stopped filling the composer.

### 9. `C-o` descends an entry to its node's first child

- **What:** `EntryNeighborDirection` gained `first_child` (tag 3), and the daemon's
  `feed.NeighborWay` replaced the next-or-previous flag.
- **Why:** the owner's ask of 2026-10-24. On an entry line in the composer, filled
  or not, `C-o` relabels the entry to answer its node's first child: `3.2 - `
  becomes `3.2.1 - `.
- **Decisions inside the shape:**
  - **The entry keeps its words**, and point keeps its place after the label.
  - **Off an entry line `C-o` is what it was** (Evil's own binding), through a
    conditional binding (`agent-repl-prompt-entries--descend-binding`).
  - **A node with no first child keeps its label**, and the composer flashes why.
