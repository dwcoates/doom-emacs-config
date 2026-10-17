# agent-repl map

Every path below is relative to the module root, `modules/app/agent-repl/` in
the repository root. Cite them in an answer as full `~/` paths in the current
worktree.

## What agent-repl is

agent-repl runs Claude coding agents in **workspaces**. A workspace is a git
worktree and branch with one agent conversation in it. Emacs is the editor
around them, and a browser view drawn inside Emacs is where the conversation is
read and written. Finished work lands on master through a **merge queue**.

## How it works, in one pass

| Part | Where | What it does |
|---|---|---|
| Emacs frontend | `lisp/*.el`, loaded by `config.el` | Keys and commands, tabs, panels, the composer, the editor popup. It asks the daemon for everything and holds no workspace state of its own. |
| Daemon (`agent-repld`) | `daemon/` (Go) | The resident service and the source of truth. It creates, names, merges and restarts workspaces, runs the merge queue, and composes every view the webapp draws. It outlives Emacs. |
| Shim | `agent-shim/claude/shim/` (TypeScript) | One process per workspace session. It drives the Claude SDK and reports what the agent does. |
| Sidecar | `agent-shim/claude/shim-sidecar/` (Go) | Reads Claude's own transcripts on disk. |
| Store | `agent-shim/shim-store/` (Go) | The durable record of every conversation. The shim and the sidecar write to it. |
| Webapp | `webapp/` (TypeScript) | The conversation view: feed, composer, topbar, sidebar, footer. It draws what the daemon composed and derives nothing. |
| Contract | `proto/src/` | The protobuf messages between all of the above. Its comments are normative documentation. |

Workspace prompts the system sends on its own (one-shot directives, merge
repair briefs, workspace naming) are plain files in `prompts/`. The agent's
standing instructions are `metaprompt.md`.

## Where each kind of answer lives

### Using a feature: keys, commands, what the user sees

- `docs/USER-GUIDE.md` — every user control added since the guide began, with its limits.
- `lisp/keybindings.el` — every key agent-repl binds (`SPC o`, `SPC TAB`, `SPC j`, `M-1`..`M-0`, and the rest), with the command each runs.
- The command's own docstring, found by searching `lisp/` for the command name.
- `AGENTS.md` sections on status colors, the footer, the topbar, the tab bar and the feed define what each visible state means.

### Workspaces: creating, one-shots, forks, children, merging

- `lisp/verbs.el` — every workspace command (create, fork, one-shot, help, close, kill, merge).
- `docs/DYNAMIC-WORKSPACE-CREATION.md` — the creation modes and their rulings.
- `docs/ONE-SHOT-POLICY.md` and `prompts/README.md` — a repository's one-shot policy and the prompt files.
- `.claude/skills/merge-queue/SKILL.md` at the repository root — how work lands on master.
- `daemon/internal/workspace/` and `daemon/internal/merge/` — the daemon side.

### The sidebar's arrangement: subsections, drag order, the tab bar's order

- `proto/src/agentrepl/v1/endpoint_arrange_sidebar.proto` and `endpoint_get_sidebar_arrangement.proto` — the contract: repositories' order, subsections, and which subsection a workspace sits in, which the Emacs tab bar walks.
- `webapp/src/sidebar/subsections.ts`, `drag.ts` and `arrange.ts` — the "+" that names a subsection, the drags, and the hold that keeps a drop on screen.
- `daemon/internal/wsm/arrange.go` and `daemon/internal/resolve/sidebar/arrangement.go` — the stored arrangement and the one order both surfaces draw.

### How it works inside: architecture and design

- `AGENTS.md` — the module's rules and vocabulary, and the first read for any design question.
- `daemon/ARCHITECTURE.md` — the daemon's package map and seams.
- `proto/AGENTS.md` — the protobuf packages and which system owns each.
- `agent-shim/AGENTS.md` — the shim, sidecar and store, and how they divide the work.
- `webapp/AGENTS.md` — the webapp's layout, seams and DOM hooks.
- `design-*.md` at the module root and `docs/protobuf-design/` — design records for specific features.
- `docs/investigations/` — dated investigations and plans.

### Changing it: building, testing, logging, deploying

- `AGENTS.md` — test rules, the test runner (`bin/test-all.sh`, `bin/background.sh`), deploy and the merge queue.
- `lisp/AGENTS.md`, `daemon/AGENTS.md`, `webapp/AGENTS.md`, `agent-shim/*/AGENTS.md`, `proto/AGENTS.md` — each system's build and test commands.
- `logging-contract.md` and `docs/LOGGING.md` — the logging contract every system follows.
- `.claude/skills/manage-agent-repl-runtime/` — build, deploy, bounce, hot-reload.
- `.claude/skills/debug-emacs-agent-repl/` — investigating a failure, reading logs.

### Code, by system

| System | Start here |
|---|---|
| Emacs | `lisp/` — one file per concern (`verbs.el` commands, `keybindings.el` keys, `panels.el` windows, `input.el` composer, `host.el` daemon connection). |
| Daemon | `daemon/internal/` — one package per concern, listed in `daemon/ARCHITECTURE.md`. |
| Shim | `agent-shim/claude/shim/src/`. |
| Webapp | `webapp/src/` — one directory per surface (`feed`, `footer`, `topbar`, `sidebar`, `composer`). |
| Contract | `proto/src/<package>/v1/*.proto`. |
