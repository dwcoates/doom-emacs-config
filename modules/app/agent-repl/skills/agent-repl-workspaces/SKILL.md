---
name: agent-repl-workspaces
description: 'Work with agent-repl workspaces through the agent API: create a workspace (a child of this one or of another, or a peer, with its landing target (its parent''s branch or the main branch), a fresh context or one forked from a workspace''s conversation, as a one-shot or not), prompt another workspace, ask many workspaces one question and get their answers back as one update (`workspace gather`), read a workspace''s status, list them all, or find workspaces open or closed by repository, time, standing or text with their summaries, mark this workspace a one-shot or as awaiting another workspace''s answer (which draws it in the background, yellow), store its links (owned or a reference; an owned pull request is followed for its comments, reviews and checks; the topbar name''s link menu), reply on a pull request it owns (`pr-comment`), send a desktop notification on behalf of a workspace whose click opens it in Emacs (`workspace notify`), open a link the user must visit in the browser profile its session signs in as (`workspace open-link`), set its topbar summary (`workspace summary`), rename a workspace, hide or show a workspace, set its priority, turn a workspace''s remote control on or off, retarget its parent, adopt an existing workspace as this one''s child, reorganize this workspace''s work across repositories (move to the repository the work principally belongs to and govern one workspace in each other repository it touches, proactively and without asking), read its repository''s sidebar subsections and file it into one, finish a workspace (deploy it as its repository''s `.agent-repl/lifecycle.yaml` says), open, close, kill, compact, set a model (`workspace model`, which compacts first), restart (on its own, to unstick a workspace or refresh its harness), interrupt or select one, warm a workspace''s cache through the daemon''s own keep-alive without leaving its shim up (`workspace keep-live`), pass another workspace''s cold gate (`workspace pass-gate`, a Haiku compaction of all its context), and subscribe this workspace to another''s turns, asks, landing, deploy, eviction, cold gate and close, follow a pull request or a workspace''s pull requests in any repository, follow a GitHub merge queue, be told once when a GitHub Actions workflow run finishes (`run-subscribe`), and register a repository with agent-repl (`repository register`). Use whenever work should be split off into another workspace, handed to one, or followed; whenever this workspace commits to finishing the rest of its work unattended, or is told to one-shot itself or to land its work when done (mark it a one-shot); whenever a one-shot is reopened or prompted for more work after it finished (take the mark back); whenever this workspace ends a turn to wait for another workspace (mark it awaiting) and when that answer arrives (clear the mark); whenever this workspace has no summary or its high-level task materially changed (set its summary); whenever this workspace''s task has clearly moved away from its name (rename it, without asking); whenever a repository''s lifecycle config (`.agent-repl/lifecycle.yaml`) or one of its hooks is written or read; whenever work is in a repository agent-repl does not know yet (register it before creating workspaces there); whenever this workspace''s work principally belongs to another repository or touches several (reorganize it); whenever this workspace''s work waits on a GitHub Actions workflow run finishing (subscribe, never poll `gh run view`); and whenever a workspace must be closed, compacted, killed or have its cold gate passed; and whenever a parked workspace''s session must be touched or warmed, above all in bulk (keep it live). Replaces /create-or-update-workspace and the workspace command files.'
argument-hint: "create | async-merge | prompt | gather | gather-status | status | list | find | set | summary | open-link | link | finished | open | close | kill | compact | model | restart | interrupt | select | keep-live | pass-gate | subscribe | unsubscribe | run-subscribe | subsections | place | repository register"
allowed-tools: Bash(~/.claude/skills/agent-repl-workspaces/agent-repld:*)
---

## What This Skill Does

Every operation is one `agent-repld workspace <verb>` (or `agent-repld repository register`) command over the serving
daemon's RPCs, run through `~/.claude/skills/agent-repl-workspaces/agent-repld`.
The DAEMON owns each command's meaning, flags and refusals: run
`agent-repld workspace` with no verb for the usage of every verb, and read a
refusal's printed arm rather than guessing. This skill says WHEN and WHY.

- A workspace is named by its roster name, by its directory (anything with a
  `/`), or as `here` for the calling workspace.
- Any command takes `-dry-run`: it reads what it needs and prints every
  request that would change something instead of sending it.
- WHO ASKED is the calling workspace, found from the working directory. A call
  from a directory that is no workspace (a subagent's worktree) is refused.
  `-as-user` says the user asks; pass it ONLY when the user typed the command
  themselves or explicitly asked you to act as them.

## Runbooks

This file is a router. Each runbook holds one verb or one decision and stays
short, so read ONLY the runbooks the task at hand needs, with the Read tool, at
`~/.claude/skills/agent-repl-workspaces/runbooks/NAME.md`. Read a runbook
BEFORE running its verb for the first time in a session.

| when | read |
|---|---|
| creating a workspace: how it relates to its creator, where it lands, the choice table | `create-relationship-landing` |
| creating a workspace: fresh or forked context, work in another repository | `create-context-fork` |
| creating a workspace: name, priority, backlogged, one-shot, hooks, handing over edits | `create-flags` |
| creating a workspace: filing it into a subsection at birth | `create-placement` |
| this workspace's work belongs to another repository or touches several | `reorganize` |
| sending a prompt to another workspace, or receiving one | `prompt` |
| a desktop notification on a workspace's behalf | `notify` |
| opening a link the user must visit, in their browser | `open-link` |
| reading one workspace's status, or listing the sidebar's workspaces | `status-list` |
| finding workspaces, open or closed, by repository, time, standing or text | `find` |
| following a workspace's turns, asks, landing, deploy, eviction or close | `subscribe` |
| asking several workspaces one question | `gather` |
| registering a repository agent-repl does not know | `repository-register` |
| reading the sidebar's subsections, or filing a workspace into one | `subsections-place` |
| marking this workspace a one-shot, or taking the mark back | `mark-one-shot` |
| ending a turn to wait for another workspace's answer | `mark-awaiting` |
| storing a link (owned or reference) | `links` |
| the links menu, removing links, reading them back | `links-menu` |
| replying on, or following, a pull request | `pull-requests` |
| following a GitHub merge queue | `github-merge-queues` |
| waiting on a GitHub Actions workflow run | `run-subscribe` |
| following, reading or posting in a Slack thread, or the Slack emoji initiator's settings | `slack-threads` |
| waiting on another workspace's or repository's change | `waiting-on-change` |
| setting the topbar summary | `summary` |
| renaming this workspace | `rename` |
| adopting a workspace as a child, or retargeting its landing | `adopt` |
| `no_layout_facts`, `no_creation_job` or `no_merge_layout` refusals | `no-merge-geometry` |
| turning remote control on or off | `remote-control` |
| taking or releasing queue bypass mode | `queue-bypass` |
| moving into or out of the backlog, or setting priority | `backlog-priority` |
| finishing, opening or closing a workspace | `finish-open-close` |
| compacting a workspace or setting its model | `compact-model` |
| reading or writing `.agent-repl/lifecycle.yaml`: the config and its five stages | `lifecycle-config` |
| lifecycle: creation, finish, workspace kinds, built-in defaults | `lifecycle-stages` |
| lifecycle: `concurrent:` and hook entries | `lifecycle-concurrent-hooks` |
| lifecycle: failed hooks, deploy hold, `queue_drained`, `gate_checks` | `lifecycle-failures-queue` |
| lifecycle: agent-repl's own config and what the config decides | `lifecycle-example` |
| restarting, interrupting or selecting a workspace | `restart-interrupt-select` |
| warming or touching a parked workspace's session | `keep-live` |
| `workspace kill`, or passing a cold gate | `destructive` |

## Rules that always hold

These are the rules a session breaks by NOT having read a runbook, so they stay
here.

- **CRITICAL: Always call the INSTALLED front door,
  `~/.claude/skills/agent-repl-workspaces/agent-repld`, never a worktree's own
  copy.**
- **CRITICAL: On a refusal, report the printed arm and stop.** The daemon
  enforces every rule in the runbooks; never work around a refusal.
- **CRITICAL: Never write a workspace command file.** The command-file ingress
  is retired.
- **CRITICAL: Separate work ALWAYS starts in a created workspace.** Never cut a
  worktree or a branch by hand for it (`git worktree add`, `git checkout -b`,
  `git switch -c`). Read `create-relationship-landing` first.
- **CRITICAL: A workspace talks to another WORKSPACE only through
  `workspace prompt`, never `SendMessage`, and NEVER broadcasts to the
  fleet.** Read `prompt` first.
- **CRITICAL: Never poll for what a subscription tells.** A pull request, a
  merge queue, a workflow run or another workspace's landing is followed with
  its subscribe verb (`pull-requests`, `github-merge-queues`, `run-subscribe`,
  `waiting-on-change`), never by `gh` loops.
- **CRITICAL: Run these ONLY when the user asked, or approved the specific act:**
  `workspace kill` (`destructive`), `workspace interrupt` and `workspace select`
  (`restart-interrupt-select`), `workspace compact` and `workspace model` on
  another workspace (`compact-model`), and `-remote-control` and `-queue-bypass`
  (`remote-control`, `queue-bypass`).
- Store every link the work is about or creates (`links`), set the summary when
  none stands (`summary`), and mark this workspace awaiting before ending a turn
  that waits on another workspace (`mark-awaiting`): the metaprompt says when,
  the runbooks say how.
