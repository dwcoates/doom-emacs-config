# Session command catalog — slash-command completion in the Emacs composer

The owner asked (2026-10-11) that any word starting with `/` in the Emacs input
buffer complete against the running workspace's known skills, and specifically
EVERY command the user added to the Claude runtime (user, project, local and
plugin skills and commands), not only the closed set the daemon recognizes.

The owner directed this change to run without stopping for design gates, so
each decision below was settled by the orchestrator and is recorded here with
its reasoning for later review.

## Core design principles

- **The runtime is the authority on what it answers.** The catalog is the
  vendor SDK's own `supportedCommands()` answer read from the live process,
  never a list assembled by scanning skill directories.
  - Consequence: no consumer duplicates the vendor's discovery rules (settings
    sources, plugin enablement, MCP prompts).
  - Does not claim: that the daemon's recognition set (`conversation.v1.SessionCommand`)
    should grow from it. Recognition stays a closed set.

## Landed changes

### 1. `conversation.v1.SessionCommandCatalog` + `SessionCommandCatalogEntry`

- WHAT: a standing-set message (whole catalog per statement) with one entry per
  command: name without slash, optional description, optional argument hint,
  aliases.
- WHY: the composer needs names to complete and the runtime's own description
  and argument hint to draw beside each candidate.
- Kept separate from `SessionCommand` on purpose: that enum is what the daemon
  suppresses a user message for, while the catalog is what a composer offers.

### 2. `conversation.v1.SessionUpdate.command_catalog = 35`

- WHAT: the shim states the catalog on the session stream, on every read that
  changed it, and restates it once on every `WatchSession` open after the
  opening re-announcement.
- WHY not `SessionStarted`: the catalog CAN change during a session (the user
  adds a skill), so it is a standing session fact, not a fact at start. The
  restatement on watch open covers a late-attaching daemon structurally, the
  same way the network-resume waits do.
- Alternative rejected: an on-demand shim rpc. It would need a live shim, so a
  parked workspace would offer no completions, and every keystroke-driven
  refresh would cost a vendor control round trip.
- Consequences:
  - The shim reads `supportedCommands()` once the start settles and again on
    every session-facts re-read (turn end), under its own fault component.
  - The daemon must route the new arm (`sessionwatcher.routeSessionUpdateLocked`),
    else it falls to the unrouted WARN.

### 3. `agentrepl.v1.ListSlashCommands` (HOST section)

- WHAT: unary rpc, `WorkspaceRef` in, success `oneof catalog { stated, unstated }`
  out, error arms the usual four workspace-resolution refusals.
- WHY: Emacs has no session stream of its own; it pulls the daemon's cached
  standing catalog when the user starts a `/` word.
- "No catalog yet" is an answer (`unstated`), never an error.
- Consequence (accepted): the daemon keeps the last stated catalog per
  workspace for the daemon's lifetime, so a parked session still completes.
  A daemon restart forgets it until the next watch-open restatement.
