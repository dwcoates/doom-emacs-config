---
name: agent-repl-workspaces
description: 'Work with agent-repl workspaces through the agent API: create a workspace (a child of this one or of another, or a primary workspace off the main branch, with a fresh context or one forked from a workspace''s conversation, as a one-shot or not), prompt another workspace, read a workspace''s status, list them all, or find workspaces open or closed by repository, time, standing or text with their summaries, mark this workspace a one-shot or as awaiting another workspace''s answer (which draws it in the background, yellow), set its principal link (the url its topbar summary opens), rename a workspace, hide or show a workspace, set its priority, retarget its parent, read its repository's sidebar subsections and file it into one, finish a workspace (deploy it as its repository''s `.agent-repl/deploy.yaml` says), open, close, kill, compact, restart, interrupt or select one, answer a cold gate, and subscribe this workspace to another''s turns, asks, landing and close. Use whenever work should be split off into another workspace, handed to one, or followed; whenever this workspace commits to finishing the rest of its work unattended, or is told to one-shot itself or to land its work when done (mark it a one-shot); whenever a one-shot is reopened or prompted for more work after it finished (take the mark back); whenever this workspace ends a turn to wait for another workspace (mark it awaiting) and when that answer arrives (clear the mark); whenever a link supplied in the conversation best reflects this workspace''s task (set its principal link, without asking); whenever this workspace''s task has clearly moved away from its name (rename it, without asking); whenever a child created with a note to its owner must be landed or closed by its owner; whenever a repository''s deploy config or a deploy hook is written or read; and whenever a workspace must be closed, compacted, killed or have its gate answered. Replaces /create-or-update-workspace and the workspace command files.'
argument-hint: "create | async-merge | prompt | status | list | find | set | finished | open | close | kill | compact | restart | interrupt | select | gate | subscribe | unsubscribe | subsections | place"
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
- Any command takes `-dry-run`: it reads what it needs and prints every
  request that would change something instead of sending it.
- WHO ASKED is the calling workspace, found from the working directory. A call
  from a directory that is no workspace (a subagent's worktree) is refused.
  `-as-user` says the user asks; pass it ONLY when the user typed the command
  themselves or explicitly asked you to act as them.

## Creating a workspace (`workspace create`)

NOTHING IS IMPLIED. Every create names its repository (`-repository here` or
a main-checkout directory), its topology and its context:

- `-topology primary` is cut from the main branch and lands on it. Add
  `-origin-master` to cut it from origin's main branch, fetched as the create
  runs, when the work must start from what is pushed rather than the local
  main branch.
  `-topology child -owner WS` is cut from WS's branch and lands into WS.
- `-context fresh` starts an empty conversation. `-context fork -fork WS`
  copies WS's conversation (cut at its last concluded turn).
- **A workspace created in another repository is NEVER a fork.**
  `-context fork -fork WS` needs WS to belong to the repository named by
  `-repository`; a cross-repository create always takes `-context fresh`,
  and its `-prompt` carries everything the new workspace needs.
  - The forked conversation is about WS's repository: its files, paths,
    branch, worktree, and its own `AGENTS.md`, `CLAUDE.md` and skills. In
    another repository those references name code that is not there, so
    the forked context misleads more than it helps.
  - The daemon refuses a cross-repository fork (as `unknown_fork_source`),
    so trying one only costs a round trip.

Choose the combination from what the work needs (every `fork` row is within
one repository):

| want | topology | context | how common |
|---|---|---|---|
| a tangential fix landed while this work goes on, a parallel subtask, a second approach | `primary` | `fork -fork here` | common |
| a piece of this workspace's unlanded feature that lands back into it | `child -owner here` | `fork -fork here` | common |
| a piece of another workspace W's work, landing into W, with W's whole context | `child -owner W` | `fork -fork W` | common |
| a fix on main that needs another workspace Y's context | `primary` | `fork -fork Y` | uncommon |
| a truly tangential bug, or a review that must not be biased by this context | `primary` | `fresh` | situational |
| a fix sent into another workspace X's unlanded branch | `child -owner X` | `fork -fork here` | rare |
| fresh work on W's unlanded branch | `child -owner W` | `fresh` | situational |
| work in a different repository from this workspace's (`-repository DIR`) | `primary` | `fresh` | situational |

- A CHILD THAT DOES NOT FORK ITS OWNER is its owner's to arbitrate and land:
  give `-owner-notice` a plain-English note telling the owner what is coming
  and why. The owner is told with the daemon's addendum, and only the owner
  lands it (`workspace finished`). Such a child is never a one-shot.
- A child that forks its owner has the owner's whole context and lands itself;
  it takes no notice.
- `-one-shot -prompt TEXT` makes a workspace that finishes its commission
  unattended and then runs `workspace finished here`, as the daemon's
  completion directive tells it.
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
- `-pre-deploy-prompt TEXT`, `-pre-deploy-script PATH:SECONDS`,
  `-post-deploy-prompt TEXT` and `-post-deploy-script PATH:SECONDS` (each
  repeatable, kept in order) REPLACE the repository's hooks of that stage for
  every deploy of the workspace; a stage given none runs the repository's.
  Give them only when the user asked for that workspace's deploys to differ.
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
- `ListAgents` never lists workspaces: an open workspace that is idle or
  cold has no running Claude process, so it is missing there however open it
  is. Find workspaces with `workspace list`, `workspace status` and
  `workspace find`. agent-repl's shim notes this on every `ListAgents`
  answer, and refuses a `SendMessage` to an `agent-repl:NAME` address.
- `workspace prompt WS TEXT` submits TEXT to WS as this agent's prompt, from
  this workspace: WS's feed heads it with a left arrow, then this workspace's
  name, and this workspace's feed shows it sent, with WS's name then a right
  arrow, and whether it was held or refused. A prompt that begins with a slash command is refused: an agent never
  clears another workspace's conversation.
- A prompt another workspace sent reaches you as a message from another
  session, `from="agent-repl:NAME"`. That is the workspace NAME, and the
  host's messaging tool it points you at is `workspace prompt NAME TEXT`.
  A prompt between two workspaces whose permission classes differ (one
  bypasses permissions, the other does not) is held by the vendor, so
  agent-repl refuses to deliver it rather than let it stall.
- **A REQUESTED UPDATE IS SENT WHEN ITS CONDITION IS MET, unasked.** When
  another workspace asks to be told once something happens ("tell me when
  this lands", "send me the results once the tests pass"), send the update
  with `workspace prompt` as soon as that condition is met, without asking the
  user for confirmation: the other workspace's request is the authorization.
- `workspace status WS` reads one workspace: its activity, its merge target,
  its repository, whether it is a one-shot, whether it awaits another
  workspace, and a cold gate it is parked at. `workspace list` reads every
  workspace the daemon serves.
- `workspace find [-repository here|DIR] [-since WHEN] [-until WHEN]
  [-standing open|closed|landed] [-text TEXT] [-limit N|-all]` lists the
  workspaces the daemon's registry holds, OPEN OR CLOSED (including ones
  closed long ago that the roster no longer shows), each with its name,
  branch, repository, its summary (the one-sentence task summary the
  webapp's topbar draws as the workspace's title), when it was created and
  last active, and whether it is open, closed or landed, most recently
  active first. WHEN is an age (`90m`, `36h`, `7d`), a date (`2026-10-20`) or
  an RFC 3339 instant: `-since` keeps workspaces active at or after WHEN,
  `-until` keeps workspaces created at or before WHEN. `-text` matches name,
  branch or summary case-insensitively. It answers 50 by default; `-limit N`
  or `-all` change that, and a cut answer is a `partial` extent naming how
  many were omitted. Zettel chat threads and async-merge workspaces are not
  answered. Use it whenever the agent needs to find the workspace that did
  some piece of work, or what ran in a repository or a timeframe, including
  closed workspaces; `workspace list` reads only what the daemon serves now.
- `workspace subscribe WS -events ...` has this workspace's agent told, as a
  prompt drawn nowhere, when WS's turn starts or ends, it needs input, it lands
  or it closes. Subscribe instead of polling; `workspace unsubscribe WS` ends
  it. A subscription ends with this workspace's close (re-opening does not
  restore it) and when WS is forgotten; WS closing is an event it tells, and
  it goes on following WS if WS is opened again. A notice that waited
  behind your turns lists every change since the last one you were told,
  oldest first, and its status is WS as it stands when the notice is made.

## Filing a workspace into a subsection

- `workspace subsections WS` prints the sidebar arrangement of WS's
  repository: its subsections by name, each with the workspaces in it, and
  its workspaces in no subsection, in drawn order.
- `workspace place WS... -subsection NAME` files the named workspaces'
  families at the bottom of their repository's subsection named NAME. It
  keeps their one-shot marks, so a one-shot that files itself still finishes
  unattended. An unknown or ambiguous NAME is refused, listing the
  subsections there are.
- `workspace place WS... -new-subsection NAME` creates a subsection named
  NAME as this workspace's agent, with the named workspaces in it. A
  subsection an agent creates is removed on its own once none of its
  workspaces is open; one the user creates stays even when empty.
- A new top-level workspace whose repository has a subsection, or at least
  two other open workspaces in no subsection, is told so at the end of its
  first message. File it once, without asking: into the one subsection that
  clearly fits its task, or else with two or more clearly related
  unsectioned workspaces into a new subsection, or else leave it where it is.

## Marking this workspace (`workspace set`)

- **ONE-SHOT.** When to mark this workspace a one-shot is the metaprompt's
  rule ("One-shot workspaces"). The verb is `workspace set here -one-shot`,
  and its answer carries the daemon's completion directive: follow it when the
  work is done. A child its owner lands cannot be marked.
- **BACK FROM A ONE-SHOT.** A workspace that keeps the mark would finish
  itself again, unattended, when the user meant to keep working in it.
  - The daemon clears the mark itself when the user takes the workspace back:
    a prompt the user submits from the editor or the webapp clears it before
    the prompt is delivered, and so does reopening a closed one-shot.
  - A prompt from another workspace, or one the daemon submits, leaves it.
    When such a prompt asks a one-shot that finished for more work, or the
    workspace was restored to do more, run `workspace set here -not-one-shot`
    at once, without asking, before doing the new work.
- **AWAITING CORRESPONDENCE.** When this workspace ends a turn to wait for
  another workspace W to answer (you prompted W, or W is finishing something
  this work needs), run `workspace set here -await W` before ending the turn.
  The workspace is drawn in the background (yellow) while it waits. When W's
  answer arrives, run `workspace set here -not-awaiting` FIRST, then act on
  it. The daemon cannot tell whether a reply is the one awaited, so clearing
  the mark is always yours.
- **PRINCIPAL LINK.** The topbar draws this workspace's summary as a link to
  its principal link; a click opens it in the browser of the session's login.
  - Set it PROACTIVELY, WITHOUT asking: whenever a link supplied in the
    conversation (a pull request, a ticket, a design doc, a dashboard) best
    reflects this workspace's current summary or task, or is otherwise the
    most salient link, run `workspace set here -principal-link URL`.
  - Judge each new link against the ones supplied before it, and against the
    standing link `workspace status here` shows as `principalLink`. When the
    new one fits the task better, set it; there is only ever one.
  - `-no-principal-link` clears it, when no link reflects the task any more.
  - Only http and https urls are accepted; anything else is refused
    `invalid_link` with the reason.
- **RENAME.** `workspace set here -name NAME` renames this workspace: its
  name only. Its branch and its worktree directory keep the names they were
  created with.
  - Rename it PROACTIVELY, WITHOUT asking, when its task has clearly moved
    away from the name it has. A one-off detour is not a reason to rename.
  - NAME follows the same rule as `create -name`: a lowercase hyphenated slug,
    with an optional prefix such as `ABC/`. A name another workspace holds as
    its name or its branch is refused `name_taken`, and a malformed one
    `invalid_name`.
  - The old name stops resolving at once. Name the workspace by its new name
    from then on, and read its branch from `workspace status`, never from
    its name.
- `-hidden` / `-shown`, `-priority`, and `-parent WS` / `-top-level` (a
  retarget, which re-applies the landing rules) set the rest.

## Finishing, opening, closing

- `workspace finished WS -placement back|front` says WS's work is done and
  queues its DEPLOY, which its repository's deploy config decides (below). Run
  it on `here` when this workspace is done (see `/agent-repl-merge-queue` for
  when, and for the version increment a deploy onto a versioned main branch
  carries), or on a child whose owner you are, after reviewing its work
  against yours. WS closes once its deploy succeeds.
- `workspace open WS` reopens a closed workspace.
- `workspace close WS` closes a workspace whose work is done or abandoned; its
  worktree stays. `close here` closes this workspace once the current turn
  ends.
- `workspace compact WS [-instructions TEXT]` compacts WS's conversation.
  Compact only when the user asked for it.

## Deploying (`.agent-repl/deploy.yaml`)

- **What a finished workspace's deploy does is its repository's, never the
  caller's.** The repository commits `.agent-repl/deploy.yaml` at its main
  checkout root. A repository with no such file refuses `workspace finished`
  and `workspace async-merge` (`no_deploy_config`); a file that does not parse
  or leaves a required field unset is refused naming the field
  (`deploy_config_invalid`). The file is read at every deploy, so an edit
  applies to the next one.
- A deploy runs THREE STAGES in order, in the deploying workspace's own feed
  bubble, one tab per hook. Each stage runs one deploy at a time, in queue
  order, unless `concurrent:` names it:

| stage | holds | runs | may be concurrent |
|---|---|---|---|
| `pre_deploy` | `hooks` (a list), then the optional built-in `local_merge` | before anything lands | yes, without a `local_merge` |
| `deploy` | one hook, optional | after the local merge landed | yes, with `pre_deploy` |
| `post_deploy` | `hooks` (a list) | last | no |

- **`concurrent:` names a leading run of stages that every deploy waiting in
  the repository's queue runs side by side, ahead of its turn.** Its one key
  is the stage the run goes through: `through_pre_deploy: {}`, or
  `through_deploy: {}` for `pre_deploy` and `deploy`. Unset, every stage runs
  one deploy at a time.
  - After its concurrent stages, a deploy waits until every deploy queued
    ahead of it has finished, then runs the rest one at a time. A deploy whose
    concurrent stages finish first still enters the next stage in queue
    order. Its footer reads `awaiting turn: N ahead` while it waits.
  - Use it when a stage waits on something outside the daemon, such as a
    deploy hook that opens a pull request and waits for it to merge: that
    wait then holds no other deploy back.
  - A concurrent stage between two one-at-a-time stages cannot be written.
    `concurrent:` beside `pre_deploy.local_merge` is refused
    (`deploy_config_invalid`), because a local merge lands on the target,
    which one deploy at a time moves. So is `concurrent: {}` with no stage.

- Every hook entry is `hook:` (exactly one of `script:` or `prompt:`) plus
  `runs_for:` (`main: {}`, only when the deploy lands on the repository's main
  branch in its main checkout, or `any_target: {}`, also when it lands in a
  parent workspace's checkout).
  - A **script** hook is `path:` (relative to the main checkout root) and
    `timeout_seconds:`. It runs from the main checkout root, with
    `AGENT_REPL_CALLING_WORKSPACE` (the deploying workspace's worktree),
    `AGENT_REPL_DEPLOY_LEDGER` (the deploy's ledger) and
    `AGENT_REPL_DEPLOY_STAGE` (`pre_deploy`, `deploy` or `post_deploy`) set.
    Its tab shows the tail of its output, its exit and its log's path.
  - A **prompt** hook is `text:`. It is sent to the deploying workspace's own
    session, and to no other workspace, ever; its tab is that turn.
- `local_merge:` is the built-in local landing: `tests:` names the test
  runner (`agent_repl_suites: {script_path: ...}`), and `version_tag: {}`
  makes every agent's deploy onto main carry a version increment and tag the
  landing. Only a repository with a local merge races its queue; a
  repository with `concurrent:` runs its concurrent stages ahead of its turn
  instead.
- **A failed hook fails the deploy**: a script that exits non-zero, runs past
  its timeout or cannot start, or a prompt whose turn does not complete. The
  workspace stays open and is handed the failure to investigate. A failure
  after the local merge landed keeps the landing.
- **Hooks act on the repository as it stands when they run** (its current
  tip), not on a snapshot of the deploy that queued them. A script hook a
  restart cut short runs again, so a script must bear running twice.
- **A repository's deploy stage can be HELD** (`/agent-repl-runtime`,
  `runtime deploy-hold`): its deploys still run `pre_deploy` (agent-repl's
  merges still land on master) and `post_deploy`, and SKIP their deploy hook.
  Releasing the hold runs a skipped script deploy hook once.
- A deploy hook asks the runtime to roll out what landed with
  `agent-repld runtime roll-out-landing -deploy "$AGENT_REPL_DEPLOY_LEDGER"`.
- agent-repl's own config:

```yaml
pre_deploy:
  local_merge:
    tests:
      agent_repl_suites:
        script_path: modules/app/agent-repl/bin/test-all.sh
    version_tag: {}
deploy:
  hook:
    script:
      path: modules/app/agent-repl/bin/deploy-landing.sh
      timeout_seconds: 60
  runs_for:
    main: {}
```

- WHAT THE CONFIG DECIDES, AND WHAT THE DAEMON ALWAYS DOES:

| configured per repository | fixed in the daemon |
|---|---|
| every pre-deploy, deploy and post-deploy hook, and each one's scope | the queue: its order, placement, pause, eviction and racing window |
| which leading stages run concurrently (`concurrent`) | that the rest are entered one deploy at a time, in queue order |
| whether a local merge runs, and its test runner | the local merge's own steps: rebase with conflict resolution, the test gate with fixing attempts, the non-fast-forward merge commit |
| whether the repository is versioned (`version_tag`) | the version arithmetic and the tag's form |
| the rollout, as a deploy hook calling `roll-out-landing` | when a requested rollout fires (after the deploy's report) |
| | holding the deploy stage, the hand-back of a failure, closing the workspace on success, the bubble, footer and roster |

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
