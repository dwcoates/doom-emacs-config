---
name: agent-repl-workspaces
description: 'Work with agent-repl workspaces through the agent API: create a workspace (a child of this one or of another, or a peer, with its landing target (its parent''s branch or the main branch), a fresh context or one forked from a workspace''s conversation, as a one-shot or not), prompt another workspace, read a workspace''s status, list them all, or find workspaces open or closed by repository, time, standing or text with their summaries, mark this workspace a one-shot or as awaiting another workspace''s answer (which draws it in the background, yellow), store its links (the topbar summary''s menu), rename a workspace, hide or show a workspace, set its priority, retarget its parent, adopt an existing workspace as this one''s child, reorganize this workspace''s work across repositories (move to the repository the work principally belongs to and govern one workspace in each other repository it touches, proactively and without asking), read its repository''s sidebar subsections and file it into one, finish a workspace (deploy it as its repository''s `.agent-repl/deploy.yaml` says), open, close, kill, compact, restart, interrupt or select one, answer a cold gate, and subscribe this workspace to another''s turns, asks, landing and close, and register a repository with agent-repl (`repository register`). Use whenever work should be split off into another workspace, handed to one, or followed; whenever this workspace commits to finishing the rest of its work unattended, or is told to one-shot itself or to land its work when done (mark it a one-shot); whenever a one-shot is reopened or prompted for more work after it finished (take the mark back); whenever this workspace ends a turn to wait for another workspace (mark it awaiting) and when that answer arrives (clear the mark); whenever this workspace''s task has clearly moved away from its name (rename it, without asking); whenever a child created with a note to its owner must be landed or closed by its owner; whenever a repository''s deploy config or a deploy hook is written or read; whenever work is in a repository agent-repl does not know yet (register it before creating workspaces there); whenever this workspace''s work principally belongs to another repository or touches several (reorganize it); and whenever a workspace must be closed, compacted, killed or have its gate answered. Replaces /create-or-update-workspace and the workspace command files.'
argument-hint: "create | async-merge | prompt | status | list | find | set | link | finished | open | close | kill | compact | restart | interrupt | select | gate | subscribe | unsubscribe | subsections | place | repository register"
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

## Creating a workspace (`workspace create`)

NOTHING IS IMPLIED. Every create names its repository (`-repository here` or
a main-checkout directory), its relationship, its context, and, when it is
not the relationship's default, its landing target:

- `-relationship` is how the new workspace stands to its creator's work:
  - `child -parent WS` works on part of WS's work and keeps coordinating
    with it. WS is its parent and OWNER: the sidebar nests it under WS (under
    a divider naming its repository when that is not WS's), WS FOLLOWS it
    automatically without `workspace subscribe` (WS is told its turn ends,
    its questions, its landing and its close), and WS reviews it and closes
    it. WS cannot be closed, hidden, finished or merged away while the child
    is open.
  - `peer` is tangential, self-sufficient, fire-and-forget: top level,
    followed by nobody, with no owner, and it always lands itself. The
    natural one-shot. Who created it is still recorded (`createdBy` in its
    status).
- `-lands-into` is the landing target, SEPARATE from the relationship and
  optional. A workspace is always cut from what it lands into.
  - Unset is the relationship's default: a child lands into its parent when
    the parent is of the same repository, and on its own repository's main
    branch otherwise (no branch crosses repositories); a peer lands on main.
  - `-lands-into main` opts out of the default, e.g. a parallel piece the
    parent waits on and picks up from main.
  - `-lands-into WS` lands into any open workspace of the SAME repository,
    e.g. a fix sent into another workspace's unlanded branch.
  - `-origin-master` cuts the workspace from origin's main branch, fetched as
    the create runs, when the work must start from what is pushed rather than
    the local main branch; it goes only with `-lands-into main`.
- `-context fresh` starts an empty conversation. `-context fork -fork WS`
  copies WS's conversation (cut at its last concluded turn).
- **A workspace created in another repository forks only to MOVE this
  workspace.** `-context fork -fork here` with another `-repository` is the
  move in "Reorganizing across repositories" below, where this workspace's
  conversation continues in the repository its work belongs to. Every other
  cross-repository create takes `-context fresh`, and its `-prompt` carries
  everything the new workspace needs.
  - The forked conversation is about the source's repository: its files,
    paths, branch, worktree, and its own `AGENTS.md`, `CLAUDE.md` and
    skills. Only the agent that is moving knows which of that still applies,
    so nobody forks another workspace's conversation into a repository.
  - The moved workspace's first message tells it which repository its
    worktree is of, so it reads that repository's `AGENTS.md` before working.
- **Cross-repository work goes through a workspace in the other repository.**
  When workspace A in repository foo needs work in both foo and another
  repository baz:
  - A does the foo work itself in its own workspace, and does the baz work
    by creating a workspace B as its child:
    `workspace create -repository <baz> -relationship child -parent here`.
    B nests under A in the sidebar, under a divider naming baz, and A owns
    and lands it. A NEVER edits baz's checkout or cuts branches in it by
    hand. Work in baz that is truly separate from A's is a `peer` instead.
  - If baz is not yet registered with agent-repl, A first registers it with
    `repository register` (see "Registering a repository" below), then
    creates B.
  - B is created `-context fresh` (per the cross-repository rule above),
    with everything B needs carried in its `-prompt` and an `-owner-notice`
    telling A what is coming.
  - A coordinates with B through `workspace prompt` and the awaiting mark
    (`workspace set -await`), e.g. integrating B's result; A follows B
    without subscribing. Example: baz owns the protobufs and foo holds the
    C++ implementation: A asks B for the protobuf change, marks itself
    awaiting, then builds A's C++ integration against B's protobufs.
  - A lands B's change through baz's deploy (`workspace finished B`) once
    it has reviewed it, and lands foo's own.

Choose the combination from what the work needs (every `fork` row is within
one repository, except the move below):

| want | relationship | lands into | context | how common |
|---|---|---|---|---|
| a tangential fix landed while this work goes on, a parallel subtask, a second approach | `peer` | main | `fork -fork here` | common |
| a piece of this workspace's unlanded feature that lands back into it | `child -parent here` | here (default) | `fork -fork here` | common |
| a piece of another workspace W's work, landing into W, with W's whole context | `child -parent W` | W (default) | `fork -fork W` | common |
| a fix on main that needs another workspace Y's context | `peer` | main | `fork -fork Y` | uncommon |
| a truly tangential bug, or a review that must not be biased by this context | `peer` | main | `fresh` | situational |
| a fix sent into another workspace X's unlanded branch | `child -parent here` | `-lands-into X` | `fork -fork here` | rare |
| fresh work on W's unlanded branch | `child -parent W` | W (default) | `fresh` | situational |
| a piece this workspace waits on and picks up from main | `child -parent here` | `-lands-into main` | `fork -fork here` or `fresh` | situational |
| coordinated work in a different repository (`-repository DIR`) | `child -parent here` | its main (default) | `fresh` | situational |
| separate work in a different repository | `peer` | its main | `fresh` | situational |

- A CHILD WHOSE CONTEXT IS NOT FORKED FROM ITS PARENT is its parent's to
  arbitrate and land: give `-owner-notice` a plain-English note telling the
  parent what is coming and why. The parent is told with the daemon's
  addendum, and only the parent (or the user) finishes it
  (`workspace finished`). Such a child is never a one-shot.
- A child forked from its parent has the parent's whole context and lands
  itself; it takes no notice. A peer never takes a notice.
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
- The workspaces a new one derives from (its parent, and the one it forks) are
  told when it is created, when it lands and when it closes without landing.
  Nothing needs to be relayed by hand.
- A branch no workspace owns (a subagent's) is landed through an async merge,
  not a create: see `/agent-repl-merge-queue`.

## Reorganizing across repositories

Every piece of work has ONE repository it principally belongs to, and gets
ONE workspace in each other repository it touches. This workspace organizes
itself that way PROACTIVELY and WITHOUT asking, when its prompt says to run
this skill or as soon as it is clear the work belongs elsewhere, and tells the
user in the same response what it reorganized and why.

1. Decide the repository the work principally belongs to, and every other
   repository it touches. Register any agent-repl does not know yet
   (`repository register`).
2. When the principal repository is this one, do not move. Give each other
   repository it touches one child of this workspace: create it as
   "Cross-repository work goes through a workspace in the other repository"
   says, or adopt one that already exists there (`workspace set WS -parent
   here`, "ADOPT" below). This workspace governs its children.
3. When the principal repository is another one, MOVE:
   - a. Read this workspace's priority, one-shot mark and links with
     `workspace status here`.
   - b. Create the governor: `workspace create -repository <principal>
     -relationship peer -lands-into main -origin-master -context fork -fork here -prompt TEXT`,
     adding `-priority P` and `-one-shot` when this workspace has them.
   - c. TEXT tells the governor that it is this workspace moved, what is done
     and what remains, and to do these first:
     - re-establish every subscription this session held to anything outside
       agent-repl (a Slack thread's `/gns-sockets subscribe <url> respond`,
       for one), since a fork is a new session;
     - store this workspace's links with `workspace link here`;
     - adopt each workspace step d names (`workspace set WS -parent here`).
   - d. Hand this workspace's children to the governor: TEXT names each
     child, and the governor adopts them. Work this workspace already
     committed or left uncommitted in its own repository stays on its
     branch: rename this workspace to the piece it now holds (`workspace set
     here -name NAME`), keep it open, and name it in TEXT among the
     workspaces the governor adopts. With no such work, run `workspace close
     here` once the governor has adopted this workspace's children.
4. Edits made by hand in another repository's checkout move to that
   repository's one workspace: `SHA=$(git -C <checkout> stash create "<what>")`,
   then `git stash apply <SHA>` in the workspace's worktree, then restore
   those files in the checkout so the edits live in one place.

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
- **CRITICAL: NEVER broadcast to the fleet.** Every workspace a prompt
  reaches wakes and resends its whole context. A notice every workspace
  needs goes in the harness (`/agent-repl-update-harness`), never in a
  prompt to each one. The daemon refuses a prompt that would take this
  workspace past 3 distinct unrelated recipients in an hour
  (`fan_out_refused`); its family, its merge's requester and workspaces that
  prompted it do not count. `workspace prompt -as-user` lifts the cap ONLY
  when the user explicitly asked for that send.
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
- `workspace status WS` reads one workspace: its activity, its merge target
  (`primary` or `workspace{workspace}`), its repository, its `relationship`
  (child{parent} or peer), its `owner` (the workspace and `ownerLands`; absent
  for a peer), `createdBy`, its open `children`, whether it is a one-shot,
  whether it awaits another workspace, and a cold gate it is parked at. `workspace list` reads every
  workspace the daemon serves.
- `workspace find [-repository here|DIR] [-since WHEN] [-until WHEN]
  [-standing open|closed|landed] [-text TEXT] [-created-by WS] [-limit N|-all]` lists the
  workspaces the daemon's registry holds, OPEN OR CLOSED (including ones
  closed long ago that the roster no longer shows), each with its name,
  branch, repository, its summary (the one-sentence task summary the
  webapp's topbar draws as the workspace's title), when it was created and
  last active, whether it is open, closed or landed, its `createdBy` and,
  for a child, its `parent`, most recently active first. WHEN is an age (`90m`, `36h`, `7d`), a date (`2026-10-20`) or
  an RFC 3339 instant: `-since` keeps workspaces active at or after WHEN,
  `-until` keeps workspaces created at or before WHEN. `-text` matches name,
  branch or summary case-insensitively. `-created-by WS` (`-created-by here`)
  keeps only the workspaces WS's agent asked for: the way a workspace
  accounts for every workspace it made, open or closed, when its conversation
  was compacted and no longer remembers them. It answers 50 by default; `-limit N`
  or `-all` change that, and a cut answer is a `partial` extent naming how
  many were omitted. Zettel chat threads and async-merge workspaces are not
  answered. Use it whenever the agent needs to find the workspace that did
  some piece of work, or what ran in a repository or a timeframe, including
  closed workspaces; `workspace list` reads only what the daemon serves now.
- `workspace subscribe WS -events ...` has this workspace's agent told, as a
  prompt drawn nowhere, when WS's turn starts or ends, it needs input, it lands
  or it closes. Subscribe instead of polling, to follow a peer or an unrelated
  workspace (a parent already follows its children); `workspace unsubscribe WS`
  ends it. A subscription ends with this workspace's close (re-opening does not
  restore it) and when WS is forgotten; WS closing is an event it tells, and
  it goes on following WS if WS is opened again. A notice that waited
  behind your turns lists every change since the last one you were told,
  oldest first, and its status is WS as it stands when the notice is made.

## Registering a repository (`repository register`)

Use it when work is in a repository agent-repl does not know yet, BEFORE
creating workspaces there with `-repository DIR`.

- `agent-repld repository register here|PATH`: PATH is any path inside a git
  repository (relative paths count from the cwd); `here` is the cwd, which
  need NOT be an agent-repl workspace.
- One unary call to the daemon's RegisterRepository: it resolves the
  repository's main worktree through git and is idempotent by that dir
  (success prints `already_known: true` when it was already registered). It
  also opens the main worktree as a workspace (`workspace_already_known`).
- Output is protojson. Refusals print an arm and exit 6: `not_in_a_repository`,
  `unreadable_path`, `inside_temporary_directory` (a path under the temp root).
- `-dry-run` prints the request without sending it.
- agent-repl's own repository never needs registering: every daemon boot
  registers it, with its main worktree open, when the registry does not hold
  it, so a first start opens on it.

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
  first message, and a top-level workspace leaving its one-shot state is sent
  the same brief as a prompt of its own. File it once, without asking: into the one subsection that
  clearly fits its task, or else with two or more clearly related
  unsectioned workspaces into a new subsection, or else leave it where it is.

## Marking this workspace (`workspace set`)

- **ONE-SHOT.** When to mark this workspace a one-shot is the metaprompt's
  rule ("One-shot workspaces"). The verb is `workspace set here -one-shot`,
  and its answer carries the daemon's completion directive: follow it when the
  work is done. A child its parent lands cannot be marked, and neither can a
  repository's main workspace (refused `main_workspace`); what a main
  workspace told to one-shot itself does instead is the metaprompt's rule
  ("A repository's main workspace is never a one-shot").
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
- **LINKS.** The topbar's summary opens this workspace's links: a click on
  it opens the one shown link, or drops a menu of the shown links' labels,
  most recently set first, when there are several.
  - The metaprompt says when to store one ("Workspace links").
  - `workspace link here -url URL -label TEXT -owned|-reference` stores a
    link, or relabels one already stored. The label is two to eight words
    in one sentence; any other is refused `invalid_label` with the reason.
  - Exactly one of `-owned` and `-reference` is named on every set, and a
    relabel names it anew.
    - `-owned`: this workspace's work made what the link points at (a pull
      request it opened, a doc or a ticket it wrote).
    - `-reference`: it exists apart from the work, which only points at it
      (the thread or ticket the task came from, a doc it read).
    - The daemon reads it: a workspace created for a Slack message
      (`create -slack-thread URL`) has every owned link posted into that
      thread when its finish's deploy ends. The daemon makes that post, and
      the one saying work began, itself; the agent posts neither.
  - The menu shows at most five links. While it has room a new link is
    shown; once it is full a new link is stored unshown, and the answer's
    `links.menu` is `full` with `unshown` above zero. Then name the five the
    menu shows with `workspace link here -show URL -show URL ...`.
  - `workspace link here -remove URL` removes one link, and
    `workspace link here -clear` removes them all.
  - `workspace status here` shows every stored link and its placement as
    `links`.
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
- **ADOPT.** `workspace set WS -parent here` makes an existing workspace
  this workspace's child. Its conversation is untouched. It nests under this
  workspace in the sidebar, and this workspace owns it, follows it and is
  told of it as an owner is. A retarget (`-parent` or `-top-level`) that
  names no `-lands-into` KEEPS where WS lands now: its branch was cut from
  there, and moving it would merge it into a branch it was never cut from.
  The relationship's landing default applies only at `workspace create`.
  `-lands-into WS|main` changes the landing, alone (the relationship is
  kept) or with `-parent`/`-top-level`. To also land an adopted workspace
  into this one, add `-lands-into here`, ONLY when its branch was cut from
  this workspace's; otherwise keep its landing. `-top-level` makes it a
  top-level peer.
- `-hidden` / `-shown` and `-priority` set the rest.

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
- **A workspace closes, without asking, a workspace it created once that
  workspace has fulfilled its goal.** When this workspace has judged (from the
  child's reply, its status, or its landing) that the created workspace did
  what it was asked, it runs `workspace close WS` at once, never `workspace
  kill`, and needs no permission from the user. It then tells the user in the
  same response which workspace it closed and why. A workspace whose goal is
  not yet met, or whose work this workspace has not reviewed, stays open.
  A child that landed or finished through `workspace finished` closes itself,
  so this covers the children that do not.
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
