---
name: agent-repl-workspaces
description: 'Work with agent-repl workspaces through the agent API: create a workspace (a child of this one or of another, or a primary workspace off the main branch, with a fresh context or one forked from a workspace''s conversation, as a one-shot or not), prompt another workspace, read a workspace''s status or list them all, mark this workspace a one-shot or as awaiting another workspace''s answer (which draws it in the background, yellow), hide or show a workspace, set its priority, retarget its parent, merge a workspace, open, close, kill, compact, restart, interrupt or select one, answer a cold gate, and subscribe this workspace to another''s turns, asks, landing and close. Use whenever work should be split off into another workspace, handed to one, or followed; whenever this workspace commits to finishing the rest of its work unattended (mark it a one-shot); whenever this workspace ends a turn to wait for another workspace (mark it awaiting) and when that answer arrives (clear the mark); whenever a child created with a note to its owner must be landed or closed by its owner; and whenever a workspace must be closed, compacted, killed or have its gate answered. Replaces /create-or-update-workspace and the workspace command files.'
argument-hint: "create | async-merge | prompt | status | list | set | merge | open | close | kill | compact | restart | interrupt | select | gate | subscribe | unsubscribe"
allowed-tools: Bash(~/.claude/skills/agent-repl-workspaces/agent-repld:*)
---

## What This Skill Does

Every operation is one `agent-repld workspace <verb>` command over the serving
daemon's RPCs, run through `~/.claude/skills/agent-repl-workspaces/agent-repld`.
The DAEMON owns each command's meaning, flags and refusals: run
`agent-repld workspace` with no verb for the usage of every verb, and read a
refusal's printed arm rather than guessing. This skill says WHEN and WHY.

- A workspace is named by its roster name, by its directory (anything with a
  `/`), or as `here` for the calling workspace.
- WHO ASKED is the calling workspace, found from the working directory. A call
  from a directory that is no workspace (a subagent's worktree) is refused.
  `-as-user` says the user asks; pass it ONLY when the user typed the command
  themselves or explicitly asked you to act as them.

## Creating a workspace (`workspace create`)

NOTHING IS IMPLIED. Every create names its repository (`-repository here` or
a main-checkout directory), its topology and its context:

- `-topology primary` is cut from the main branch and lands on it.
  `-topology child -owner WS` is cut from WS's branch and lands into WS.
- `-context fresh` starts an empty conversation. `-context fork -fork WS`
  copies WS's conversation (cut at its last concluded turn).

Choose the combination from what the work needs:

| want | topology | context | how common |
|---|---|---|---|
| a tangential fix landed while this work goes on, a parallel subtask, a second approach | `primary` | `fork -fork here` | common |
| a piece of this workspace's unlanded feature that lands back into it | `child -owner here` | `fork -fork here` | common |
| a piece of another workspace W's work, landing into W, with W's whole context | `child -owner W` | `fork -fork W` | common |
| a fix on main that needs another workspace Y's context | `primary` | `fork -fork Y` | uncommon |
| a truly tangential bug, or a review that must not be biased by this context | `primary` | `fresh` | situational |
| a fix sent into another workspace X's unlanded branch | `child -owner X` | `fork -fork here` | rare |
| fresh work on W's unlanded branch | `child -owner W` | `fresh` | situational |

- A CHILD THAT DOES NOT FORK ITS OWNER is its owner's to arbitrate and land:
  give `-owner-notice` a plain-English note telling the owner what is coming
  and why. The owner is told with the daemon's addendum, and only the owner
  lands it (`workspace merge`). Such a child is never a one-shot.
- A child that forks its owner has the owner's whole context and lands itself;
  it takes no notice.
- `-one-shot -prompt TEXT` makes a workspace that finishes its commission
  unattended and lands itself by the repository's own completion directive.
  Without `-one-shot`, `-prompt` is the opening prompt and the workspace stays
  open for the user.
- **CRITICAL: Separate work ALWAYS starts in a created workspace.** Never cut a
  worktree or a branch by hand for it (`git worktree add`, `git checkout -b`,
  `git switch -c`): a hand-cut worktree is one no workspace owns and nothing
  cleans up.
- A create is SYNTHESIS, not investigation. Write what the new workspace must
  do and put the investigation into its prompt: run no tests, crawl no
  threads and read nothing large first. At most one shallow lookup to name
  the work.
- Create ONE workspace unless the user asked for more. A process-style ask
  ("one per failing test") is one workspace whose prompt does the enumerating
  and creates the rest.
- Omit `-name` and the daemon names the workspace from its prompt. A name you
  give is a short lowercase hyphenated slug naming the SUBJECT (never the
  channel, the person or the thread it came from), with no prefix or suffix of
  your own.
- Give `-priority` or `-model` only when the user named one.
- To hand uncommitted edits to the new workspace, snapshot them with
  `SHA=$(git stash create "<what>")` (which touches neither the working tree
  nor the shared stash stack) and open its prompt with "First, run
  `git stash apply <SHA>`". Always the SHA, never `stash@{N}`, and `apply`,
  never `pop`. Store or tag the SHA if the gap will be long, since `git gc`
  may prune it.
- Afterwards, tell the user each workspace's name and what it was asked to do.
- The workspaces a new one derives from (its owner, and the one it forks) are
  told when it is created, when it lands and when it closes without landing.
  Nothing needs to be relayed by hand.
- A branch no workspace owns (a subagent's) is landed through an async merge,
  not a create: see `/agent-repl-merge-queue`.

## Talking to a workspace

- **CRITICAL: A workspace talks to another WORKSPACE only through
  `workspace prompt`, never `SendMessage`.** `SendMessage` stays the way to
  talk to this workspace's own subagents. `workspace prompt` goes through
  agent-repld, which holds the prompt behind the recipient's running work,
  waits out its cold gate, and draws the send in both workspaces' feeds; a
  `SendMessage` to another session goes around all of it.
- `workspace prompt WS TEXT` submits TEXT to WS as this agent's prompt, from
  this workspace: WS's feed heads it with a left arrow, then this workspace's
  name, and this workspace's feed shows it sent, with WS's name then a right
  arrow, and whether it was held or refused. A prompt that begins with a slash command is refused: an agent never
  clears another workspace's conversation.
- **A REQUESTED UPDATE IS SENT WHEN ITS CONDITION IS MET, unasked.** When
  another workspace asks to be told once something happens ("tell me when
  this lands", "send me the results once the tests pass"), send the update
  with `workspace prompt` as soon as that condition is met, without asking the
  user for confirmation: the other workspace's request is the authorization.
- `workspace status WS` reads one workspace: its activity, its merge target,
  its repository, whether it is a one-shot, whether it awaits another
  workspace, and a cold gate it is parked at. `workspace list` reads every
  workspace the daemon serves.
- `workspace subscribe WS -events ...` has this workspace's agent told, as a
  prompt drawn nowhere, when WS's turn starts or ends, it needs input, it lands
  or it closes. Subscribe instead of polling; `workspace unsubscribe WS` ends
  it. A subscription ends with either workspace's close.

## Marking this workspace (`workspace set`)

- **ONE-SHOT.** When this workspace commits to finishing the REST of its work
  unattended (the user said "land it when done", or the remaining work needs
  no further input), run `workspace set here -one-shot`. The answer carries
  the repository's completion directive: follow it when the work is done.
  `-not-one-shot` takes the mark back when the user re-engages. A child its
  owner lands cannot be marked.
- **AWAITING CORRESPONDENCE.** When this workspace ends a turn to wait for
  another workspace W to answer (you prompted W, or W is finishing something
  this work needs), run `workspace set here -await W` before ending the turn.
  The workspace is drawn in the background (yellow) while it waits. When W's
  answer arrives, run `workspace set here -not-awaiting` FIRST, then act on
  it. The daemon cannot tell whether a reply is the one awaited, so clearing
  the mark is always yours.
- `-hidden` / `-shown`, `-priority`, and `-parent WS` / `-top-level` (a
  retarget, which re-applies the landing rules) set the rest.

## Landing, opening, closing

- `workspace merge WS -placement back|front` lands WS's own branch where its
  topology says. Run it on `here` to land this workspace (see
  `/agent-repl-merge-queue` for when, and for the version increment a landing
  on agent-repl's master carries), or on a child whose owner you are, after
  reviewing its work against yours.
- `workspace open WS` reopens a closed workspace.
- `workspace close WS` closes a workspace whose work is done or abandoned; its
  worktree stays. `close here` closes this workspace once the current turn
  ends.
- `workspace compact WS [-instructions TEXT]` compacts WS's conversation.
  Compact only when the user asked for it.

## Restarting, interrupting, selecting

- **CRITICAL: Each of these runs ONLY when the user asked for it.** Each acts
  on what the user is looking at or waiting on.
- `workspace restart WS` relaunches WS's session at once and reloads its page.
- `workspace interrupt WS` stops WS's running turn. A stop while WS has live
  detached agents is refused once, naming them: `-confirm-agents` stops them
  too, and `-all-agents` stops every live agent instead of the turn.
- `workspace select WS` makes WS the workspace the editor and the webapp show.

## Destructive and gated acts

- **CRITICAL: `workspace kill` and every `workspace gate` answer (`pay`,
  `clear`, `compact`, `compact-and-resume`, with `-model` and `-scope`) run
  ONLY when the user explicitly asked for that act or explicitly approved it.**
  The daemon cannot observe that approval, so this rule is yours to keep.
- `workspace status` names a cold gate a workspace is parked at; report it to
  the user and ask, rather than answering it yourself.

## Notes

- **CRITICAL: Always call the INSTALLED front door,
  `~/.claude/skills/agent-repl-workspaces/agent-repld`, never a worktree's own
  copy.**
- **CRITICAL: On a refusal, report the printed arm and stop.** The daemon
  enforces every rule above; never work around a refusal.
- **CRITICAL: Never write a workspace command file.** The command-file ingress
  is retired.
