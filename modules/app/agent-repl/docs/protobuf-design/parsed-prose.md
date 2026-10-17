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
  - The webapp ignores `source`.
- **Retraction of change 1's rejoin consequence:** rejoining wrapped lines by
  structure is not needed. The tree region already ends at the first line that is
  neither a root, a branch, a blank line nor an interior fence (findTreeRegion's rule,
  ported exactly), so a wrapped line the model wrote never sits inside a tree. The
  error was assuming the region kept continuation lines; it never did.
