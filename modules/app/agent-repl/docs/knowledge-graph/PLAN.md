# Knowledge graph and chat: planning record

Owner-led planning, started 2026-10-17. This file is the living record of what
has been decided, what is proposed, and what is set aside. It is updated
continually, throughout the project. The project lives inside agent-repl.

## Ground rules

- Every term is settled during planning, before any code uses it.
- Settled terms go in a glossary in the project's `AGENTS.md`.
- Code, protos and docs use only glossary terms as identifiers, with no synonyms.
- A glossary entry may list known synonyms, so they are understood when they
  come up in conversation. Code never uses a synonym.
- A glossary entry may give a code identifier that differs from the spoken
  term, when the spoken term would clash in code. Conversation and docs use
  the spoken term, and code uses the identifier.
- Renaming a term changes the glossary and every place that uses it in the same commit.
- This plan is updated continually as decisions are made, throughout the project.

## Vision

agent-repl grows from managing coding agents into also organizing general
knowledge. Its new parts:

- a chatbot with persistent conversations;
- chats organized into a knowledge graph of questions asked of answers, recursively;
- asking about a highlighted passage of anything on screen (an agent's
  response, a chat answer, later a book), with the question and answer landing
  in the graph, traceable back to the text that was highlighted.

The knowledge graph is its own system, orthogonal to agent-repl, called
zettel (after a Zettelkasten, a box of linked note slips).

agent-repl is heading toward being a general tool rather than only an agent
manager. That is context for this project, not part of it: renaming agent-repl
and moving it out of the doom config into its own repository both come at a
later date.

Prior art: `~/workspace/notertame` (a question-and-answer card app with
`Datum.parentId` follow-ups, chosen by hand, and a swipeable card stack).

## Decided

### Contributor and consumer UIs

- A contributor UI is any interface that adds to the data model. Examples:
  - agent-repl's feed: highlight text in a response bubble and ask about it,
    and the question and answer land in zettel;
  - a book reader: read as normal, highlight text and ask about it;
  - the chat, in the style of claude.ai or ChatGPT, whose questions and
    answers map onto the data model.
- A consumer UI renders the data model. Example: flash cards for spaced
  repetition over question-and-answer pairs.
- One UI can be both: the branching chat both adds questions and renders the
  graph.
- Books and agent feeds stay in their contributor UIs and are not stored in
  the graph. Only questions and answers are.

### Organization

- One engine runs on one workstation and holds all state. The engine is the
  daemon, shim, store, sidecar and the zettel service, plus the protobuf
  contract.
- Clients differ only in the roles they fill:
  - input client: Emacs today, could be vim or something else later;
  - output client: the webapp, which is the stable one;
  - combined input and output: a mobile app later.
- Views are what is on screen, and every view is webapp code: the agent feed,
  the chat, a book reader, later others. Hosts put the webapp on screen: Emacs
  through xwidget, later a Tauri shell. A shell has no features of its own
  beyond native app chrome.
- Clients hold no domain logic. Anything a second client would have to
  reimplement belongs in the daemon.
- Chats converge onto agent-repl's architecture (owner, 2026-10-24):
  - chats share the daemon, shim and store with agent workspaces;
  - a thread is to a shim instance what a worktree is to one today, one to one;
  - the store keeps every transcript, chats' included, as agent-repl's;
  - the zettel service keeps its own database, holding only the graph:
    nodes, parents, premises, revisions, embeddings and sightings.
  This keeps zettel separable from agent-repl, leaves the store's role
  unchanged, keeps embedding and premise work off the store (which every
  workspace depends on), and gives corrected revisions a natural home.
- The zettel service is the only owner of the graph database. The daemon
  stays the one endpoint every client talks to, and passes graph calls on to
  it. A change confined to the zettel service rolls out without a daemon
  rollout.
- Remote clients, such as the future mobile app, reach the workstation's
  daemon only through a relay over HTTP. The mobile app is not built now. It
  will eventually work with agent-repl's workspaces too, so remote access is
  designed once for the whole contract.
- Each user's own workstation is the server. No shared server ever holds a
  user's data in readable form.
- All computation on persisted content, embeddings included, runs on the
  workstation. A remote client only sends requests and shows results.
- The contract must work over HTTP, so local and remote clients share it.
- A remote client has nothing to show while the workstation is offline,
  unless caching is added later.
- The code may be regrouped by client, but only as directory renames and the
  reference updates they need. No code is reorganized. An illustration of the
  layout (names not settled):

  ```text
  proto/
  services/  daemon, shim, store, sidecar, zettel, relay
  clients/   emacs, webapp, desktop, mobile
  ```

### Answering

- Chat answers are billed to the Claude subscription, not an API key. They
  run through agent-repl's existing shim (`agent-shim/claude/shim`), which
  drives the Agent SDK (the renamed Claude Code SDK) under the same logins
  agent-repl uses. One adapter knows the SDK, the topbar's model selection,
  token count and login already work off shim sessions, answers stream as
  `conversation.v1`, which the webapp already renders, and the shim's fake
  SDK keeps tests off the real vendor.
- Every thread has one shim, and the shim has one session (owner,
  2026-10-24). Every prompt entered in the thread goes to that session, and
  the responses flow back into the thread and into zettel. The session is
  chronological, so the model sees the whole thread's history, not only the
  path to the node being asked of; the owner calls this ideal, because
  sibling branches are useful context. A question asked of an earlier answer
  carries that answer's quote and context, the way a reply to a past response
  carries it today.
- Later, maybe: limit how much of other branches reaches the model, say to
  the nearest three or four nodes rather than twenty. One chronological
  session holds everything, so a limit would need the shim to manage what
  the session carries.
- Threads are not attached to git repositories or worktrees. A thread's
  shim runs in its own directory, `~/zettel/threads/<thread-name>/`, under a
  canonical `~/zettel` directory (owner, 2026-10-24). A thread's name is
  generated once, when it is created, so its directory never moves.
  `~/zettel` is fixed; it is not configurable.
- To the daemon, a thread is a new kind of workspace (owner, 2026-10-24): its
  directory is its thread directory instead of a worktree, it is left out of
  workspace tabs and cycling and out of every repository, merge and deploy
  feature, and it inherits every session-management feature with no code
  duplicated.
- Following premises feeds the model's context when answering: premises are
  added to the prompt as extra context.
- Anthropic's Agent SDK docs say third-party developers may not offer
  claude.ai login for their products unless approved. zettel runs on its
  owner's own login, as agent-repl does.

### Data model

- A node is a node in the graph. It holds at least a question and its answer,
  plus whatever else belongs with them. Every node has a title that makes
  sense on its own.
- A node's content is versioned: each revision is frozen, and correcting an
  answer adds a new revision. The UI for corrections comes later.
- A node copies its question, its answer (as the first revision) and any
  quote when it is created. Questions and answers will become editable, and
  the store's entries must never change, so zettel owns its own copy. A
  node's link back into the store (its thread's session and the turn's place, or a quoted
  agent-feed bubble) is for navigation only. The store deletes no
  conversation records today (it prunes only its write ledger), but a node
  must not depend on that; a target that is gone is shown as gone.
- A node has at most one parent: where its question came from. The parent is
  either a node in zettel, along with the revision the question was asked
  of, or a place outside it, such as a book page or an agent feed's response
  bubble. A node whose parent is outside zettel is a root. Parents form a
  tree, which gives the chat stream and its sibling stacks one well-defined
  path to each node. A node's parent never changes.
- A quote is the exact words highlighted when the question was asked, stored
  with the parent. A highlight also captures surrounding text, its context;
  how much depends on the view: the bubble in an agent feed, a passage, page
  or several pages in a book (chosen by a classifier), and the same approach
  as a book for a chat.
- A thread is one chat: a root node and every node beneath it. Branching
  within a thread (several questions asked of one answer) makes it a tree.
  The sidebar lists threads.
- A premise says one node is conceptual groundwork for another, regardless of
  which was asked first. Asking about quarks one week and nuclear physics the
  next still makes nuclear physics a premise of quarks. A premise is not a
  parent.
  - zettel finds premises itself, on the workstation, whenever a node is
    inserted. Similarity search finds candidates, and since similarity has no
    direction, a judge decides whether either node is groundwork for the
    other. The search runs both ways: the new node may build on older nodes,
    and older nodes may build on it.
  - Premises point from the more general node to the more specific one, so
    in principle they never loop. Loops are still impossible
    programmatically: the zettel service is the only writer, and it refuses,
    in the same transaction as the insert, any premise that would close a
    loop.
  - Adding or removing premises by hand is left for later, if ever needed.

### Highlight UX

- In the Emacs host, a prompt sent while text is highlighted in the current
  view takes that highlight as its context. The highlight clears once the
  prompt is sent.
- The daemon owns the highlight, as it already owns the whole-response
  selection (`frontend.v1.FeedSelectionResponse`, see
  `proto/src/agentrepl/v1/endpoint_submit_prompt.proto`). A text highlight
  becomes a second kind of selection beside it.
- Every view uses one shared highlight component, so building highlights for
  answers also builds them for books.
- The webapp has no input box of its own. Input comes from agent-repl's
  composer in Emacs, the same as for agent workspaces. The mobile app will
  need its own input later. The webapp already has a composer
  (`webapp/src/composer/composer.ts`), drawn in production only inside
  subagent bubbles, which a host without Emacs could turn on.

### Chat UX

- The first implementation includes the chat, the principal contributor UI.
  It looks much like the ChatGPT and claude.ai chat interfaces, slick and
  professional, in agent-repl's existing colors (the light and dark themes'
  variables in `webapp/src/styles.css`).
- Design mock: `docs/knowledge-graph/chat-mock.html` (open it directly in a
  browser). Its chat stream is the reference; its sidebar is not.
- The sidebar's head stays exactly as it is (the purple search icon, the
  version, everything else). The only change to existing UI is that the
  "Workspaces" title becomes clickable, opening a dropdown of apps: today
  Workspaces and Chat. With Chat selected, everything below the head
  changes: the Open, Hidden, Recently Merged and other bands give way to a
  list of threads, the most recently interacted with first. The search icon
  then searches the threads' contents.
- The topbar stays essentially unchanged (login, model selection, token
  count and the rest). Where it shows the workspace name, it shows the
  thread's name, generated automatically when the thread is created, and the
  purple summary is the thread's, computed the way a workspace's is.
- A chat stream of question and answer bubbles, one path at a time.
- Sibling questions (questions asked of the same answer) stack under that
  answer. Swiping left or right on a bubble cycles through its siblings and
  redraws everything below it.
- Below a node, the stream follows the least-seen child at every level. Ties
  go to the newest child.
- Each sibling's branch is picked once per visit to its parent, so swiping
  back and forth does not change it. After the parent leaves the path and
  comes back, branches are picked again from the sighting counts. The pick is
  determined only by those counts, so children not looked at keep their
  branch.
- A visit lasts while its parent stays on the path on screen. Scrolling the
  parent out of view does not end it, and swiping deeper down the path does
  not end the visits of the parents above.
- Each answer shows its premises as grey links that jump to that node's path.

### Stack

- The whole webapp is built with React: new views are written in it, and the
  existing views, the agent feed included, are ported to it, so shared pieces
  such as the highlight component exist once. The port keeps the existing
  look and feel, and the feed's current speed (a tab switch in the low tens
  of milliseconds). Long views draw only the rows on screen.
- The first implementation supports only the Emacs host, through xwidget.
  The Tauri shell first comes into play with the mobile app (Tauri 2 builds
  for iOS and Android, and on Apple platforms uses the same WebKit as
  xwidget). The webapp is bundled into the app, or remote origins are
  allowed explicitly, because Tauri blocks remote content from native
  features by default.
- Remote clients reach the workstation through our own relay. The
  workstation dials out to it, it forwards only end-to-end encrypted traffic
  over outbound HTTPS, and pairing and encryption are built on a proven
  protocol such as Noise. Chosen over a mesh VPN because a VPN takes iOS's
  single VPN slot and needs setup on every device.

### Judgement calls (owner delegated, 2026-10-24; tweak after)

- Swiping an answer bubble cycles the same siblings as swiping its question,
  since the question and answer are one node. Revisions get their own control
  when corrections arrive.
- A question asked about a highlight sits in the sibling stack like any other
  child, and its quoted words are also marked in the parent answer, as the
  mock shows.
- No separate graph overview for now; the chat is the way to see the graph.
- In the chat, a new prompt attaches to the end of the path on screen, or to
  the highlighted answer when there is a highlight.
- Embeddings run natively on the workstation (an M4 MacBook Pro): Qwen3
  Embedding, through Ollama on Metal, with the model size chosen by
  measurement.

## Phases

1. Chats working locally, in Emacs and its xwidget webapp: threads,
   answering through the shim, the zettel service with nodes, revisions and
   sightings, the chat view in React, the sidebar and topbar changes, and the
   Emacs bindings.
2. Premises and embeddings: similarity search, the premise judge, premise
   links in the chat. Highlights in agent feeds landing in zettel.
3. The agent feed ported to React.
4. Remote access and other hosts: the relay, the Tauri shell, the mobile app.
   Who hosts the relay is decided then.

## Phase 1 as built (2026-10-24)

What landed, by system. The contract decisions are in
`docs/protobuf-design/zettel-phase1.md`.

- **zettel** (`zettel/`): the service, a launchd singleton
  (`com.agentrepl.zettel`) serving `zettel.v1` on
  `~/.cache/agent-repl/sock/zettel.sock`, its graph in `~/zettel/zettel.db`.
  Nodes, revisions (revision 1 so far) and sightings. `RecordExchange` is
  idempotent by the daemon turn, so an answer learned twice is one node.
- **The daemon:**
  - a thread is a workspace of kind `thread`, in the zettel home
    (`~/zettel/threads/<name>/`), which `ListRepositories` never lists, so no
    repository feature sees it;
  - `CreateThread` names the thread from its question (the naming call),
    starts a chat session (`SessionProfile.chat`: no tools, no settings, the
    corpus's `prompts/chat-thread-system.md`), submits the question and
    selects the thread;
  - a thread's question carries where it was asked from on its submission,
    hold and turn row (`ThreadAsk`, `ThreadQuestion`), so every copy the
    queue makes of it carries it;
  - the chat service (`internal/chat`) composes each thread's view: the
    shown path with its per-visit picks (least seen, newest among ties), the
    sibling labels, the quote marks, the held highlight, and the question in
    flight with its answer streaming; it records each answered question in
    zettel from the feed's concluded answer (`feed.ChatSink`);
  - watching a thread replays its newest feed page, so an answer the daemon
    missed recording is recorded then;
  - the roster's app switcher and thread section; threads are in no
    repository section, band or tab;
  - deploys build, install and restart zettel; every boot keeps it loaded and
    current.
- **The webapp:** React, used for the chat view; a thread's page mounts the
  chat in place of the feed. The rail's title is the app switcher, and the
  Chat app lists the threads with a new-thread button.
- **Emacs:** `SPC TAB t` (new thread), `SPC TAB T` (most recent), `SPC TAB .`
  (pick by name); every thread is an Emacs workspace the tab bar never draws;
  the sidebar's new-thread button opens the prompt.

### Judgement calls made while building

- A new thread opens once created (the owner's navigation ruling: "on Enter
  the new thread opens in the webapp"), unlike a new workspace.
- A second question asked while a thread's first is still being answered is
  refused with its own arm (`thread_awaiting_first_answer`): there is no
  node yet to ask it of. The composer keeps the text.
- A question asked while another is in flight, with no highlight, attaches
  to the end of the path on screen, which is the in-flight question's
  parent, so it becomes the in-flight question's sibling.
- While a question is in flight, the shown path is the way to its parent,
  whatever the picks say.
- Picks and the highlight are in the daemon's memory: a restarted daemon
  starts a fresh visit.
- A thread's Emacs workspace is named `chat/<name>`, so no thread's name can
  take a tab's.
- The rail's search narrows threads by name; searching chat contents needs
  zettel's text search (phase 2).
- No test world runs a real zettel. The daemon harness and the Emacs e2e
  world stage a placeholder zettel for deploys and point the daemon's zettel
  socket at an unserved path (`UnservedZettelEnv`), so no test reaches the
  owner's own `zettel.sock`. A chat e2e area needs a real zettel in the world,
  as the store and the sidecar have; it is not built yet.

### Deferred from phase 1

- Deleting, renaming and archiving threads (the repository verbs refuse a
  thread, since it has no repository).
- Answer revisions beyond the first.
- Searching chat contents.

## Proposed, not yet settled

- Requirements:
  - what it does: persistent chat stored as the graph, asking about a
    highlight in any view, similarity search over the graph, reading and
    annotating books later, and a simpler way to work with agent-repl's
    agents from mobile;
  - how it behaves: the workstation is the only place state is stored, one
    contract serves every client, and tests never reach a real vendor.
- The zettel service's contract: a launchd-managed singleton like the store,
  serving a new `zettel.v1` package over Connect on a unix socket. The daemon
  is its only caller, and gains new client-facing RPCs that the webapp and
  Emacs call, which it resolves into `zettel.v1` calls.
- Ingestion by cursor: a finished chat turn becomes a node through two
  writes (the store's transcript, then zettel's node). zettel records the
  last place it ingested per session, and the daemon replays from there
  whenever the two connect, so a crash between the writes can never skip a
  turn.
- Premise inference runs after the answer is shown, never delaying it, and a
  node durably records that its premises are still pending, so a restart
  picks up where it left off.
- Tables in zettel's SQLite database:
  - `node`: id, created time, title, question text, root node id, its parent
    (a node and the revision asked of, or a place outside zettel), the quote
    and the question context, and its link back into the store;
  - `revision`: node id, revision number, answer text, created time, the
    model that wrote it, and whether it was generated or a correction;
  - `premise`: the general node, the specific node, created time, and what
    proposed it;
  - `embedding`: node id, revision, embedding model, vector;
  - premise inference state per node;
  - sighting counts per node.
- Stack: protobuf over Connect, as agent-repl already uses, which the relay
  can carry unchanged; SQLite through `modernc.org/sqlite`, which cannot load
  vector extensions, so similarity search runs in Go over embeddings held in
  memory, fast enough for one person's graph.
- Remote access: privacy comes from end-to-end encryption, not from who runs
  the relay. The daemon listens only on `127.0.0.1` today and trusts every
  local caller, so remote access also needs device pairing and
  authentication.
- How sightings are counted: a bubble fully on screen, recorded by the daemon.
- Navigation (owner, 2026-10-24, decided):
  - threads are not selectable the way workspaces are (`M-{`, `M-}`,
    `M-<number>`, `SPC p p`), because users do not bounce between threads
    the way they do between workspaces;
  - the Chat view of the sidebar has a button for a new thread, and Emacs
    has a binding for one that works like the one-shot prompt: a minibuffer
    prompt, and on Enter the new thread opens in the webapp;
  - Emacs has a binding for switching to the most recent thread;
  - proposed keys, under `SPC TAB`: `t` for a new thread, `T` for the most
    recent thread (`SPC TAB R` already opens the most recent workspace),
    and `SPC TAB .` for picking a thread by name from a minibuffer list (owner);
  - in the Chat view the topbar's tab bar is shown, as it is when
    agent-repl's panels are closed, so a workspace is one click away;
  - the sidebar's view always follows what is shown: showing a thread
    shows the Chat view with the tab bar, and showing a workspace shows the
    Workspaces view. The view is derived from what is shown rather than kept
    as separate state, so the two can never disagree, and picking Chat in
    the dropdown shows the most recent thread.
  - Opening a thread counts as interacting with it, for its place in the
    sidebar list.

## Known complications

- The shim assumes a workspace directory: its cwd, folder trust and
  per-session locks. Resolved: a thread's directory is
  `~/zettel/threads/<name>/`.
- Threads inherit every part of the daemon's session management that still
  applies (list agreed by the owner, 2026-10-24), to be checked off one by
  one during implementation:
  - inherited: the cold-context gate (`StartSession` refusing a cold context
    with its cost), model and effort selection, interrupt (`KillTurn`),
    rollback (`RollBackSession`), hibernation and keepalive, kill, the name
    and purple summary (`GatherTitleDigest`), account selection, compaction;
  - not applicable: permission mode, bash, workflows, subagents, and every
    worktree, merge and deploy feature.
- The shim hard-codes `settingSources: ["user", "project", "local"]`
  (`agent-shim/claude/shim/src/sdk/real-query.ts`), which loads skills,
  `CLAUDE.md` and memory from `~/.claude`. A chat must not inherit them.
  Resolved: a chat session's profile loads no settings and offers no tools.
- The premise judge needs model calls, which the zettel service would make
  back through the daemon and a shim.
- Premises feeding the model's context make the prompt grow with how many
  premises a node has.
- agent-repl's workspace assumes a git worktree plus an Agent SDK session
  (`workspace.v1.WorkspaceRef.dir`).
- Embeddings need a provider, and Anthropic does not offer one.

## Set aside: automatic routing

Routing decides where a new prompt lands in the graph, so the user never has
to find the old conversation by hand. Set aside until the fundamentals are
settled. Ideas so far:

- embedding similarity over titles and answers finds candidate nodes;
- an LLM judge picks the parent, or starts a new root;
- each routing decision is saved and shown, and the user can reparent the
  node, because a wrong attach made silently corrupts the graph with no trace;
- in the chat, a filing away from the path on screen has to be visible.

## Glossary candidates

Only rows marked settled are settled.

| Concept | Term |
|---|---|
| The knowledge-graph system itself | `zettel` (settled) |
| A node in the graph, holding at least a question and its answer | `node` (settled) |
| One chat: a root node and every node beneath it | `thread` (settled); known synonym "chat" |
| A UI that adds to the data model | `contributor` (settled) |
| A UI that renders the data model | `consumer` (settled) |
| Where a node's question came from: a node in zettel, or a place in a contributor UI | `parent` (settled); known synonym "provenance" |
| The exact words highlighted when the question was asked, stored with the parent | `quote` (settled) |
| Text around a highlight | `context` in conversation and docs, `questionContext` in code (settled) |
| A node that is conceptual groundwork for another | `premise` (settled) |
| The service that carries remote clients' encrypted traffic to the workstation | `relay` (settled) |
| A frozen version of a node's content | `revision` |
| One stay of a parent on the path on screen | `visit` |
| One counted instance of a bubble being seen | `sighting` |

## Open questions

- Is the least-seen pick for exploring the graph, or for remembering what's in it (spaced repetition)?
- How much of agent-repl's elisp is domain logic that a second input client would have to rewrite? (audit)
