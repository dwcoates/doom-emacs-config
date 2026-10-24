# zettel phase 1: chat threads — protobuf design record

The owner wants agent-repl to also be a general knowledge tool: a chat, in the
style of claude.ai, whose questions and answers land in a knowledge graph
(zettel). Phase 1 gets chats working locally in Emacs and its xwidget webapp.
The planning record is `docs/knowledge-graph/PLAN.md`; this file records the
contract decisions. The owner delegated every contract decision on 2026-10-24
("implement everything, contracts included"), so every entry below is the
implementer's call, recorded with its reasoning.

## Core design principles

- **A thread is a kind of workspace to the daemon.** It reuses the whole
  session lifecycle (shim, turns, interrupt, rollback, hibernation, name and
  summary, model and effort, account) and is left out of every repository,
  merge, deploy and worktree feature and out of the tab bar.
  Consequences: the daemon's workspace record gains a kind; every repository
  feature refuses or skips threads. It does not claim threads appear in the
  workspace roster's sections: they appear only in the roster's thread section.
- **Zettel holds the graph only; transcripts stay in the store.** A node
  copies its question and answer, because answers become revisable and store
  entries never change. Nothing in zettel.v1 points into the store for
  content.
- **The webapp derives nothing** (standing agent-repl rule). Which branch is
  shown, sibling labels, which words are marked, which app the sidebar shows:
  all daemon-resolved.

## Landed changes

### 1. `zettel.v1`, a seventh surface

What: a new package (`zettel.proto` shared types, six endpoint files,
`service Zettel`): BeginExchange, CompleteExchange, AbandonExchange,
ListPendingExchanges, GetThread, RecordSightings. `proto/Makefile` compiles the
new root; `proto/AGENTS.md` lists the surface.

Why: the zettel service owns the graph database; the daemon is its only
caller. The routing test (who produces, where it is routed) puts these
messages in their own package, produced by the zettel service and consumed by
the daemon.

Consequences and reasoning:
- **Exchanges make the question durable before the answer.** A question's
  origin (its parent, its quote) is only known at submit time. BeginExchange
  records it then; CompleteExchange turns it into a node with revision 1. A
  turn that ends with no answer (interrupt, failure, rollback) abandons it, so
  `node` rows are always answered. The alternative, writing the node only at
  turn end, would need the daemon to hold the origin in memory across the
  turn, and a daemon restart mid-turn would lose it.
- **`ExchangeTurn` is the reconciliation key.** ListPendingExchanges plus the
  turn id lets a restarted daemon settle exchanges whose turns ended while it
  was down. (Ingestion by cursor in the plan is realized this way: the pending
  table is the cursor.)
- **One parent, a tree per thread.** `NodeParent` is a oneof of `node` (with
  the revision asked of and an optional quote) and `thread_start`. The
  agent-feed contributor's parent arm (a place outside zettel) is not modeled
  yet; it lands with that contributor in phase 2 as a new arm.
- **The quote rides the parent link** (`NodeParentNode.quote`), since only a
  question asked of an answer can quote it. Its surrounding text is
  `question_context` in code and "context" in conversation (glossary rule).
- **Thread identity is the daemon's workspace id**, wrapped as
  `zettel.v1.ThreadId`. Zettel never mints or parses it.
- **Node titles and premises are not in phase 1.** Both need model calls the
  plan schedules for phase 2 (titles with the premise judge). `Node` gains
  them as additive fields then.
- **Sightings are counted in zettel**, beside the node, because they are
  graph data (the least-seen pick is a property of the graph's use) and the
  plan lists them in zettel's database.

### 2. `shim.v1.StartSessionRequest.profile`

What: `SessionProfile` oneof `agent` / `chat`; the chat arm carries the whole
system prompt text.

Why: a chat session must load no filesystem settings (no skills, CLAUDE.md,
memory), have no tools, and run under a chat system prompt instead of the
coding preset. The shim is a vendor adapter, so the daemon owns the prompt's
wording and passes it verbatim.

Consequences: the shim refuses an unset profile instead of assuming agent.
Every StartSession the daemon sends sets it. The profile applies to resumes
too, because every query the shim opens is built from it.

### 3. `frontend.v1.ChatView` (`chat.proto`)

What: the chat page's component, designed figma→idl from the agreed mock
(`docs/knowledge-graph/chat-mock.html`); the layout drawing is in the file
header. `ChatView` is `empty` or `conversation`; a conversation is the shown
path's exchanges (root first, one per node) and an optional pending question.

Consequences:
- `ChatExchange.node` imports `zettel.v1.NodeId`: a typed identity is a join
  key the webapp echoes back (SwitchChatSibling, SelectChatText,
  ReportChatSightings), so it is imported, never restated.
- Sibling labels ("2 of 3") are daemon-composed text.
- Quote marks and the held highlight are listed as words; the webapp marks
  them in the rendered answer.

### 4. Sidebar: `RosterAppSwitcher` and `RosterThreadSection`

What: `WorkspaceRoster.app_switcher` (the rail title, which app is shown, the
dropdown's options) and `WorkspaceRoster.threads` (the thread list and the
new-thread button). `SidebarApp` is the echo token for the dropdown.

Why: the owner ruled that the sidebar head stays as it is, the "Workspaces"
title becomes a dropdown of apps, and with Chat shown the bands give way to a
list of threads, most recently interacted with first. Which app is shown
follows what is on screen, so it is daemon state resolved from the selected
workspace's kind.

Consequences: threads appear in no repository section, band or tab. The
thread section is always set (Emacs reads it to pick threads by name and to
find the most recent).

### 5. `agentrepl.v1` chat verbs

What: ShowSidebarApp (sidebar), and a CHAT section: CreateThread,
RequestNewThread, WatchChat, SwitchChatSibling, ReportChatSightings,
SelectChatText. WatchPage gains the `chat` subscription arm; WatchDaemon gains
`DaemonNewThreadRequested`; AdoptWebWorkspaceSuccess gains `page` (agent or
thread), which decides whether the page mounts the feed or the chat.

Consequences and reasoning:
- **The new-thread button goes through Emacs.** The webapp takes no typed
  input (Emacs's composer is the input), so the button asks the daemon to
  push `DaemonNewThreadRequested` and Emacs opens its new-thread prompt.
- **Showing an app is a selection.** ShowSidebarApp selects the app's most
  recent workspace and lets the roster's `current` move the editor, exactly
  as a row click does; there is no separate "shown app" state to disagree
  with the selection.
- **The highlight is daemon-held,** as the whole-response selection already
  is: the next prompt in the thread consumes it.
- Thread-addressed verbs carry the standard workspace-resolution refusal arms
  (the daemon maps refusals onto arms by name) plus `not_a_thread`.

### 6. Revision of 1: one idempotent `RecordExchange` replaces the exchange lifecycle

What: BeginExchange, CompleteExchange, AbandonExchange, ListPendingExchanges,
`ExchangeId`, `PendingExchange` and `GetThreadSuccess.pending` are removed.
`RecordExchange` records a question and its answer as a node in one call,
idempotent by the daemon turn; `Node.turn` names that turn.

Why, and the root cause of the earlier shape being wrong: the exchange was keyed
by the daemon turn at SUBMIT time, but the daemon's prompt queue re-mints a
held prompt's turn (a vendor-blocked prompt is re-held under a new turn,
`promptqueue/rehold.go`), so an exchange begun at submit could be orphaned from
the turn that finally answers it. The design reasoned from the RPC surface
without checking the queue's identity rules. The structural fix moves the
question's origin onto the daemon's own durable prompt records (the
submission, the held prompt and the turn row carry it, so every copy the queue
makes carries it too), and zettel learns the question only together with its
answer.

Consequences:
- Zettel holds answered nodes only; the chat view's question in flight is the
  daemon's (its turn row and the feed's live answer).
- A turn's answer can be learned more than once (the live ending, then a
  history replay after a restart); idempotency by turn makes that one node, so
  a restart between answer and record loses nothing and duplicates nothing.
- A turn that ends without an answer (interrupt, failure) never reaches zettel.
