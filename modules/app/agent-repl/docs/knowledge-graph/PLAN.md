# Knowledge graph and chat: planning record

Owner-led planning, started 2026-10-17. This file is the living record of what
has been decided, what is proposed, and what is set aside. It is updated as the
planning goes, for the whole of the planning process. The project lives
inside agent-repl.

## Ground rules

- Every term is settled during planning, before any code uses it.
- Settled terms go in a glossary in the project's `AGENTS.md`.
- Code, protos and docs use only glossary terms as identifiers, with no synonyms.
- Renaming a term changes the glossary and every place that uses it in the same commit.
- This plan is updated continually as decisions are made, throughout the project.

## Vision

agent-repl grows from managing coding agents into also organizing general
knowledge. Its new parts:

- a chatbot with persistent conversations;
- chats organized into a knowledge graph of questions asked of answers, recursively;
- asking about a highlighted passage of anything on screen (an agent's
  response, a chat answer, later a book), with the question and answer landing
  in the graph as a child of the text that was highlighted.

agent-repl is heading toward being a general tool rather than only an agent
manager. That is context for this project, not part of it: renaming agent-repl
and moving it out of the doom config into its own repository both come at a
later date.

Prior art: `~/workspace/notertame` (a question-and-answer card app with
`Datum.parentId` follow-ups, chosen by hand, and a swipeable card stack).

## Decided

### Organization

- One engine runs on one workstation and holds all state, the knowledge graph
  included. The engine is the daemon, shim, store and sidecar, plus the
  protobuf contract.
- Clients differ only in the roles they fill:
  - input client: Emacs today, could be vim or something else later;
  - output client: the webapp, which is the stable one;
  - combined input and output: a desktop shell or a mobile app.
- Views are what is on screen, and every view is webapp code: the agent feed, the
  chat, a book reader, later others. Hosts put the webapp on screen: Emacs
  through xwidget, a desktop shell (Electron or Tauri), a mobile shell. A
  shell has no features of its own beyond native app chrome.
- Clients hold no domain logic. Anything a second client would have to
  reimplement belongs in the daemon.
- Remote clients, such as the future mobile app, reach the workstation's
  daemon only through a proxy service over HTTP. The mobile app is not built
  now. It will eventually work with agent-repl's workspaces too, not only the
  knowledge graph, so remote access is designed once for the whole contract.
- Each user's own workstation is the server. No shared server ever holds a
  user's data in readable form, so their information stays private.
- All computation on persisted content, embeddings included, runs on the
  workstation, where the graph lives. A remote client only sends requests and
  shows results.
- The graph gets its own service in the engine, the only owner of the graph
  database, the way the store owns the record database. The store cannot be
  reused: its only callers are the shim and the sidecar, and the daemon never
  calls `store.v1`. The service calls the Messages API itself, and the daemon
  stays the one endpoint every client talks to, passing graph calls on. A
  change confined to the graph service rolls out without a daemon rollout.
- The code may be regrouped by client, but only as directory renames and the
  reference updates they need. No code is reorganized. An illustration of the
  layout (names not settled):

  ```text
  proto/
  services/  daemon, shim, store, sidecar, proxy
  clients/   emacs, webapp, desktop, mobile
  ```

- The contract must work over HTTP, so local and remote clients share it.
- A remote client has nothing to show while the workstation is offline,
  unless caching is added later.

### Data model

- A node is one question and its answer. Every node has a title that makes
  sense on its own.
- A node's content is versioned: each revision is frozen, and correcting an
  answer adds a new revision. The UI for corrections comes later.
- A node can have several context parents, which makes the graph a directed
  acyclic graph. A node's parents must already exist when it is created, so
  cycles cannot happen.
- A highlighted passage is not an entity of its own. It is an optional field
  on a parent link: the passage names the revision it was taken from, and
  includes its quoted words.
- A highlight also captures surrounding text, and how much depends on the
  view: the bubble in an agent feed, a passage, page or several pages in a book
  (chosen by a classifier), and the same approach as a book for a chat.
- A conversation is a path through the graph, not a stored object.

### Highlight UX

- In the Emacs host, a prompt sent while text is highlighted in the current
  view takes that highlight as its context. The highlight clears once the
  prompt is sent.
- The daemon owns the highlight, as it already owns the whole-response
  selection (`frontend.v1.FeedSelectionResponse`, see
  `proto/src/agentrepl/v1/endpoint_submit_prompt.proto`). A text highlight
  becomes a second kind of selection beside it.
- Every view uses one shared highlight component, so building highlights for
  answers also builds them for books. A book is a source text in the graph,
  and the reader is the view that shows one.
- The webapp has its own input box, used whenever the host provides no input
  client.

### Chat UX

- A normal chat stream of question and answer bubbles, one path at a time.
- Sibling questions (questions asked of the same answer) stack under that
  answer. Swiping left or right on a bubble cycles through its siblings and
  redraws everything below it.
- Below a node, the stream follows the least-seen child at every level.
  Ties go to the newest child.
- Each sibling's branch is picked once per visit to its parent, so swiping
  back and forth does not change it. After the parent leaves the path and
  comes back, branches are picked again from the sighting counts. The pick is
  determined only by those counts, so children not looked at keep their
  branch.
- A visit lasts while its parent stays on the path on screen. Scrolling the
  parent out of view does not end it, and swiping deeper down the path does
  not end the visits of the parents above.

## Proposed, not yet settled

- Chat nodes run on the Messages API, with context built from the graph,
  instead of Agent SDK sessions. A linear SDK transcript does not match a
  chain of earlier nodes, and a node with several parents has no single
  session to resume.
- Highlights in agent feeds land in the knowledge graph too (inferred from the
  owner's answer on surrounding text, not yet confirmed).
- How sightings are counted: a bubble fully on screen, recorded by the daemon.
- Requirements:
  - what it does: persistent chat stored as the graph, asking about a highlight
    in any view, similarity search over the graph, reading and annotating
    books later, and a simpler way to work with agent-repl's agents from
    desktop and mobile;
  - how it behaves: the workstation is the only place state is stored, one
    contract serves every client, and tests never reach a real vendor.
- Remote access: privacy comes from end-to-end encryption, not from who runs
  the machine in the middle. The daemon listens only on `127.0.0.1` today and
  trusts every local caller, so remote access also needs device pairing and
  authentication. Candidates for how a phone reaches the workstation:
  - our own relay that forwards only end-to-end encrypted traffic, which the
    workstation dials out to;
  - a mesh VPN built on WireGuard, such as Tailscale, or Headscale for a
    self-hosted coordination server;
  - ruled out: opening a port on the workstation (exposed and fragile), and
    Cloudflare Tunnel (Cloudflare can read the traffic it terminates).
- Stack:
  - protobuf over Connect, as agent-repl already uses; Connect runs over HTTP,
    so the proxy can carry it unchanged;
  - SQLite through `modernc.org/sqlite`; that driver cannot load vector
    extensions, so similarity search runs in Go over embeddings held in
    memory, which is fast enough for one person's graph;
  - the webapp is not committed to staying framework-free; the choice goes
    by what gives the best results. A framework that updates the page
    directly without a virtual DOM (Solid, Svelte) is closest to how the
    webapp works today;
  - desktop shell candidates: Electron, Tauri, Wails (Go, with the operating
    system's web view), or no shell at all (an installable web app).

## Known complications

- Several parents makes context grow with how many branches meet, on top of
  how deep the chain is, and it loses prompt caching: a combination of
  branches starts with text no earlier request did.
- agent-repl's workspace assumes a git worktree plus an Agent SDK session
  (`workspace.v1.WorkspaceRef.dir`).
- Embeddings need a provider, and Anthropic does not offer one.
- "Context" already means the model's context. The surrounding text of a
  highlight needs its own term.

## Set aside: automatic routing

Routing decides where a new prompt lands in the graph, so the user never has
to find the old conversation by hand. Set aside until the fundamentals are
settled. Ideas so far:

- embedding similarity over titles and answers finds candidate nodes;
- an LLM judge picks the context parent or parents, or starts a new root;
- the model's context is built from the parents' earlier chains plus any highlighted passage;
- each routing decision is saved and shown, and the user can reparent the
  node, because a wrong attach made silently corrupts the graph with no trace;
- in the chat, a filing away from the path on screen has to be visible.

## Glossary candidates

Nothing here is settled yet.

| Concept | Candidates |
|---|---|
| The question-and-answer unit | open (notertame used `datum`) |
| A source text, such as a book | open |
| A frozen version of a node's content | `revision` |
| The link from a question to a parent | open |
| A highlighted passage | `excerpt` (preferred), `passage`, `quote` |
| Text around a highlight | `surround`, `setting`, `vicinity` |
| One stay of a parent on the path on screen | `visit` |
| One counted instance of a bubble being seen | `sighting` |

## Open questions

- How much of agent-repl's elisp is domain logic that a second input client would have to rewrite? (audit)
- Should the proxy be built here, or should an existing tunnel be used?
- Should the mobile app host the webapp in a web view rather than be built natively?
- Is the least-seen pick for exploring the graph, or for remembering what's in it (spaced repetition)?
- What does swiping on an answer bubble cycle through: the pair's siblings, or that answer's revisions?
- Should questions about a highlight sit on the passage, in the sibling stack, or both?
- With Emacs as the host, is the webapp's input box hidden or available alongside it?
- In the chat, does a new prompt always attach to the end of the path on screen?
- Is a separate overview of the graph wanted, or is the chat the only way to see it?
- Which embedding provider: a local model on the workstation, or a hosted API
  called from the workstation? Chat answers already send content to
  Anthropic, so a hosted embedding API adds one more vendor that sees it.
- How does a remote client reach the workstation: our own relay, or a mesh VPN?
- Which framework for the webapp, if any, and are existing views ported to it?
- Tauri or Electron for the desktop shell?
