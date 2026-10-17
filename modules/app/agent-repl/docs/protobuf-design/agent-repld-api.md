# The agent-repld API and the skills over it

Design record, opened 2026-10-16 in agent-repl-skill-migration. It continues
`workspace-source-and-context.md` (the topology of a workspace) and records the
command surface an agent uses, and the skills that teach it. Nothing here is a
contract yet: each section says what the owner has ruled and what is open.

## Principles (ruled)

- Every agent-facing operation is an `agent-repld <noun> <verb>` command that
  calls the daemon's RPCs directly. No agent-facing operation writes a command
  file.
- A noun names what the command acts on. A command with no workspace argument
  acts on the calling workspace, found from the working directory.
- The daemon owns each command's meaning, flags and refusals. Skills say WHEN
  and WHY to use a command and point at it; they never restate its rules, so
  two skills mentioning one command cannot disagree.
- Skills are organized by the agent's task, not one per noun. They may overlap.
  Every agent-facing command is reachable from at least one skill, and every
  command a skill names exists (both to be checked by tests).
- Skill names start with `agent-repl-`; slash completion matches by substring
  (landed 2026-10-16), so `/profile` still finds `/agent-repl-profile`.

## Core design principles

Owner-stated, project-wide; every sketch is checked against them before it is
offered.

1. NO DEFAULTS. Every create and every merge names each of its choices; each
   choice and value is documented in the proto comments and the skills.
   Consequences: no `optional` field may stand for an implicit choice (the
   context origin is a required oneof with a `fresh` arm, not an unset
   field). Reopens: the "optional fork source, unset is fresh" ruling.
   Settled 2026-10-17 (owner delegated the contract design): the reopening
   holds; ancillary settings (model, priority, ungated consent, op id) keep
   their optional fields, since none of them decides where work comes from,
   lands, or what context it starts with.
2. THE DAEMON ENFORCES THE RULES. Every combination the rules refuse is
   refused by the daemon; skills and proto comments state the same rules but
   are never the enforcement. Consequences: wherever the daemon lacks the
   fact a rule needs, the contract must carry it (a gap is an API change, not
   a skill caveat). Does not claim: that callers may skip stating a choice
   because the daemon would refuse a wrong one.
3. A BRANCH OWNED BY A WORKSPACE LANDS ONLY INTO THAT WORKSPACE; landing it
   anywhere else is that workspace's responsibility. A branch cut from main
   lands only on main. Consequences: the branch source must distinguish main,
   a workspace's branch, and a branch no workspace owns. Reopens: the async
   merge's "any named branch onto the requester's target".
4. WHOEVER HOLDS THE WORK'S CONTEXT LANDS IT. Forked from the branch's owner,
   the new workspace lands itself; otherwise the owner arbitrates and lands,
   told by a required notice (with a daemon addendum), reopened if closed.
   Does not claim: any guard against landing into a checkout its agent is
   editing.
5. WORKSPACES THAT ARE BRANCHED FROM OR FORKED FROM ARE TOLD: a brief notice
   when the derived workspace is created, lands, or closes without landing;
   one combined notice when both.
6. A SUBAGENT'S WORK LANDS ONLY THROUGH AN ASYNC MERGE, forked from the
   workspace that spawned the subagent, onto main or into that workspace's
   branch. No subagent transcript is ever forked.
7. AN ASYNC MERGE FORKS WHEN IT STARTS, not when it is requested.
8. THE AGENT API IS RPC ONLY. No agent-facing operation writes a command file;
   the command-file ingress is retired.

## Scope (settled 2026-10-17)

- Objective: an agent manages workspaces and the merge queue only through
  explicit, documented `agent-repld` RPC calls, and the daemon refuses every
  combination the rules forbid.
- Built: the create contract (branch source, merge target, context origin;
  landing and notice), one-shot merge with no target, async and subagent
  merges as creates that land when done, workspace and queue subscriptions,
  `workspace set`, `editor eval`, status reads, the rollout hold under
  `runtime`.
- No new on-screen component; the editor and webapp callers are ported with
  behavior unchanged.
- Systems: the protos (`agentrepl.v1`, possibly `workspace.v1`), the daemon and
  its CLI, the Emacs lisp, the webapp, the Emacs test fake daemon, the skills,
  install manifests and metaprompt; the shim likely untouched.
- Which existing fields and features are replaced or removed (including queue
  racing) is decided during the proto increments, not up front.

## Iteration sequence (settled 2026-10-17)

Transport (the existing ConnectRPC `agentrepl.v1.AgentRepl`), then groups by
command noun (Workspaces with landing and status; Merge queue; Editor;
Runtime), then each group's rpc list, then shared conventions (the existing
success-or-failure result; refusals as error arms), then shapes one rpc at a
time. The owner then delegated every remaining design ruling to the agent
("I think you got all the ideas, implement yourself"), so the shapes below were
decided and landed by the agent without per-shape review; each entry records
why, so the owner can revisit any of them.

## Landed changes

All on branch `contract/agent-repld-api`, 2026-10-17.

1. TOPOLOGY IS STRUCTURAL (`workspace_topology.proto`). A standard or one-shot
   create carries `WorkspaceTopology`, whose arm names both the branch source
   and the merge target: `primary` (main onto main) or `child` (an owner's
   branch into that owner). The refused combinations are unrepresentable
   rather than validated. This refines the earlier "three independent fields":
   the three choices are still each always made, but source and target are
   one choice because the ownership rule ties each source to one target.
   `base_ref` is retired: a workspace is always cut from its source's tip.
2. CONTEXT ORIGIN IS REQUIRED (`WorkspaceContextOrigin`: `fresh` or
   `fork{workspace}`), independent of the topology. Replaces `parent.fork`
   and `fork_source`; `CreateWorkspaceParent`, `CreateWorkspaceFork` and
   `CreateWorkspaceForkSource` are deleted.
3. REQUESTER IS REQUIRED (`WorkspaceRequester`: `user` or `workspace`) on
   CreateWorkspace and MergeWorkspace, because notices, reports and the
   owner-lands rule depend on who asked.
4. OWNER NOTICE. `WorkspaceTopologyChild.owner_notice` is required exactly
   when the context is not a fork of the owner, and refused otherwise; such a
   child is refused as a one-shot and refused landing itself. Delivered with
   the daemon's addendum as `PROMPT_ORIGIN_OWNER_NOTICE`.
5. DERIVED-WORKSPACE NOTICES. `PROMPT_ORIGIN_DERIVED_WORKSPACE_NOTICE`: brief
   daemon notices to workspaces branched from or forked from, on create, land,
   and close without landing. No request field: the daemon derives them.
6. ASYNC MERGE MOVES INTO CREATEWORKSPACE as a third form,
   `CreateWorkspaceAsyncMerge`. Its source is a branch NO workspace owns plus
   its maker (`subagent{agent_id}`, verified against the transcript on disk,
   or `workspace{ref}` for a branch an agent cut by hand), its target is
   `primary` or `maker`, and its context must fork the maker. Answered with a
   new `async_merge_accepted` arm because its workspace does not exist until
   the merge starts; `op_id` is refused beside it. DECISION beyond the
   discussion: a hand-cut branch (the common "land a tangential fix while my
   branch carries other work") is kept as a maker kind, because the rules
   otherwise left it nowhere to land. DECISION: the merge workspace's
   readiness stays daemon-driven (its resolving turn settles, then the queue
   lands it), rather than its agent calling MergeWorkspace, because the
   daemon already knows when that turn ends and a step the agent must
   remember can be forgotten.
7. MERGEWORKSPACE LANDS ONLY A WORKSPACE'S OWN BRANCH, in the target recorded
   at creation; the `async` arm is retired. New: `requester`, required
   `placement` (`MergeQueuePlacement` back/front, which before existed only in
   the command file), and refusals `owner_lands` and `requester_may_not_land`.
8. VERSION-INCREMENT MESSAGES MOVED unchanged into
   `merge_version_increment.proto` (pure move, same names), since both
   MergeWorkspace and the async merge carry them.
9. STATUS. `WorkspaceStatus` and `MergeQueueStatus` (agent-facing, not views),
   read by `GetWorkspaceStatus` and `GetMergeQueueStatus`. DECISION: no
   streaming variant yet; programs already follow the roster and footer
   streams, and a stream can be added later carrying the same status.
10. SUBSCRIPTIONS. `SubscribeWorkspace`/`UnsubscribeWorkspace` (events: turn
    started, turn ended, needs input, landed, closed; notices as
    `PROMPT_ORIGIN_WORKSPACE_NOTICE`) and `SubscribeMergeQueue`/
    `UnsubscribeMergeQueue` (moved out of UpdateMergeQueue with the same
    detail levels). Each subscribe answers with the current status, so
    nothing slips between reading and following. "Not subscribed" is now a
    success answer, per the outcome convention.
11. ROLLOUTS. `UpdateRollouts` (hold/release) replaces UpdateMergeQueue's
    rollout arms; release answers `up_to_date`, `rolling_out{commit}` or
    `start_failed{detail}`.
12. EDITOR EVAL. `EvaluateInEditor` (agent → daemon), push arm
    `WatchHostWorkspaceResponse.evaluate` (daemon → Emacs, with a daemon-minted
    `EditorEvaluationToken`), and `AnswerEditorEvaluation` (Emacs → daemon),
    sharing `EditorEvaluationOutcome` (value, read error, eval error, plus
    `*Messages*` lines). This is the first daemon→Emacs request that is
    answered; every earlier host push was a fact nothing acked.

13. LIFECYCLE ACTS (owner, 2026-10-17). The agent API covers opening,
    closing and killing a workspace through the existing OpenWorkspace,
    CloseWorkspace and KillWorkspace, plus a new `CompactWorkspace`
    (instructions optional), submitted through the prompt queue as
    `PROMPT_ORIGIN_WORKSPACE_COMPACT`. CLEARING IS NOT SUPPORTED. To make that
    a daemon rule rather than a skill caveat, agent-sent prompts carry the new
    origin `PROMPT_ORIGIN_AGENT_SENT`, and such a prompt that begins with a
    slash command is refused (`SubmitPromptError.slash_command_from_agent`), so
    `/clear` cannot arrive as a prompt either. Nuking and forgetting stay out
    of the agent API: both destroy or drop records, and the owner asked for
    neither.

14. SELF-CLOSE WAITS FOR ITS TURN (owner, 2026-10-17). CloseWorkspace gains
    a required `requester`. When the requester is the workspace being closed,
    the asking turn is not a blocker; if everything else is quiet the close is
    answered `closes_when_turn_ends` and performed by the daemon at the edge
    where that turn ends, so the close can never race the turn that asked for
    it. A blocker that appears before then abandons the close into the
    footer's close-blocked state. Kill never waits.

15. COLD GATES (owner, 2026-10-17). The agent API passes a workspace's
    compaction (cold) gate through the existing AnswerColdGate: pay and
    resume, clear, compact, or compact and resume, with every compaction
    option (model, scope). `WorkspaceStatusActivity` gains `cold_gate`
    carrying the gate's `frontend.v1.FeedId`, the token AnswerColdGate needs,
    so an agent can answer a gate it did not draw. Clearing at a gate is a gate
    answer, distinct from the `/clear` prompt the API refuses. SKILL RULE: the
    workspace skill states that a kill or a gate answer is performed only when
    the user explicitly asked for it or explicitly approved it. The daemon
    cannot observe that approval, so it is a skill rule, not a daemon refusal.

Not changed, deliberately: queue racing (its removal is its own change);
`SetWorkspaceParent` (the retarget; the daemon re-applies the ownership rules
when it runs, and `agent-repld workspace set` calls it); hiding and showing
(`SetWorkspaceHidden` already serves `workspace set hidden`, per the owner's
request that the API and the workspace skill cover it).

Consequences, accepted:

- Every caller of CreateWorkspace and MergeWorkspace breaks: the editor
  (`lisp/verbs.el`, `lisp/wire-verbs.el` and the fake daemon), the webapp
  (`sidebar/create.ts`, `sidebar/roster.ts`, `feed/bubble-more.ts`), the
  daemon's command-file ingress (being removed) and `agent-repld merge-queue`.
- The daemon must locate a subagent's transcript under a workspace's session
  to verify an async merge's maker (vetting register item 1).
- The daemon must hold durable workspace subscriptions alongside the
  merge-queue ones, and correlate editor evaluations by token with a
  deadline.

## No defaults (ruled 2026-10-16)

- Creating and merging workspaces have NO defaults. Every create names its
  branch source, its merge target and its context origin explicitly; every
  merge names what it lands. Each field and each of its values is documented in
  the proto comments and in the skills that use it, so every call is a
  specific, intentional one.
- The combinations are settled one at a time with the owner (see "The
  combinations" below): for each, whether the API supports it, how common it
  is, and example situations, to guide the skills' text.

## Delivery (ruled 2026-10-16)

- The whole change lands at once, on a `contract/` integration branch that
  reaches master only when every system is green.
- The editor's and the webapp's create commands are ported to the new fields
  with their behavior unchanged.
- The CEE pull request removing `/create-or-update-workspace` opens only after
  the doom side has landed, confirmed with the owner first.

## Topology (ruled; see workspace-source-and-context.md)

- A create has three independent fields: branches from, merges into, and its
  context origin. Under "No defaults" the context origin is REQUIRED, naming
  fresh or the workspace forked; the earlier "optional, unset is fresh"
  wording is superseded.
- An async merge is a create with "land when done" attached; it needs no shape
  of its own.
- `workspace merge` names no target: the daemon lands in the target recorded
  when the workspace was created, from the requester's request.

## The surface (draft)

```text
agent-repld workspace   create | prompt | set | merge | close | open | status | subscribe
agent-repld merge-queue status | dequeue | move-up | move-down | pause | resume
                        disable-racing | enable-racing | subscribe
agent-repld editor      eval
agent-repld runtime     deploy | bounce | health | rollouts hold | rollouts release
agent-repld call        <Method> [JSON]     # debugging escape hatch, outside the agent API
```

- `workspace prompt` sends a message into the workspace's conversation
  (`SubmitPrompt`).
- `workspace set` (ruled, renamed from `update`) changes the workspace's
  settings: priority, hidden, model, effort, permission mode.
- `workspace merge` lands the workspace's own branch in its recorded target
  (today's one-shot); it carries what is known only once the work is done: queue
  placement (front or back) and the version increment with its changelog line.
- `runtime rollouts` (ruled, moved from `merge-queue`): holding stops the
  redeploy a landing on agent-repl's master triggers. RELEASING DEPLOYS
  (ruled): if master's tip is ahead of the running build, releasing rolls it
  out, so no landing is left undeployed by a hold. (Today release only ends the
  hold, and the build lags until the next landing or a hand deploy.)
- `call` (ruled) stays a top-level debugging tool: it reaches every unary RPC by
  name, so it is a way around the API, not part of it.
- `runtime` beyond `rollouts` (deploy, bounce, health) is fleshed out in a later
  workspace.

## `editor eval` (proposed)

One RPC answering in its response, replacing `runtime-eval-code`'s
fire-and-forget command file whose result arrives later as a prompt.

- Request: the elisp text, and the workspace to evaluate it for.
- Response, a oneof:
  - success: the value printed with `prin1`, whether that print reads back,
    and the `*Messages*` lines the evaluation produced;
  - failure: read error, eval error (error symbol and data), timeout, or no
    editor attached.
- Errors are caught in Emacs's one handler (a `condition-case` around read and
  eval), not by wrapping the text in the CLI, so no request can bypass it and
  malformed text is reported as a read error.
- The timeout is measured first, then set just over twice the healthy latency.

## Skills (proposed)

| skill | task | was |
|---|---|---|
| `/agent-repl-workspaces` | interacting with workspaces | CEE's `/create-or-update-workspace` |
| `/agent-repl-merge-queue` | landing work and controlling the queue | `/merge-queue` |
| `/agent-repl-runtime` | operating the runtime; evaluating elisp as a runbook | `/manage-agent-repl-runtime`, `/runtime-eval-code` |
| `/agent-repl-debug` | investigating failures | `/debug-emacs-agent-repl` |
| `/agent-repl-profile` | profiling the live runtime | `/profile-agent-repl` |
| `/agent-repl-tests` | writing tests; later, running them | `/write-tests` |
| `/agent-repl-install` | setting up a machine | `/install-agent-repl` |
| (removed) | | `/agent-repl-help` |
| `/show-chess-game` | a feature, unchanged | |

## Open

- Nothing ruled open; the contract design comes next.

## Settled along the way (owner, 2026-10-16)

- `workspace subscribe` offers: turn started, turn ended, needs input, landed,
  closed; each delivered as a prompt in the subscriber's session.
- A merge target may change after creation only through `workspace set`,
  which re-applies the ownership and landing rules (replacing today's
  `SetWorkspaceParent` retarget).
- A create always cuts a NEW worktree from the tip of the branch it names as
  its source (main's, W's, or the unowned branch's), read when the create
  runs; it never takes over an existing worktree.
- "Merge me" (`workspace merge`) lands when the agent's turn ends.
- `/agent-repl-install`'s description says both: run it when a dependency is
  missing, and update its manifests when a change adds or changes one.
- The command-file ingress is retired entirely; Emacs and `runtime-eval-code`
  move onto RPCs.
- The skill renames update every reference: the metaprompt's merge-queue rule,
  the `agent-repl-merge-queue-guard` hook, the requirement manifests and the
  memory notes.
- `/agent-repl-help` and `SPC o h` are removed; a fuller help system comes
  later.

- A fork cuts at the forked workspace's last concluded turn; leaving out a
  turn in flight is acceptable.
- The notices also cover a derived workspace that closes without landing.
- Landing into a workspace's checkout while its agent is working there is
  acceptable; no extra guard.

## Rules the combinations reduce to (owner, 2026-10-16)

- A BRANCH OWNED BY A WORKSPACE W LANDS ONLY INTO W. Branching from W and
  landing anywhere else (main included) is refused: landing W's work elsewhere
  is W's responsibility.
- WHO LANDS INTO W: when the new workspace's context is forked from W, it has
  W's whole context and may land itself (land when done allowed). Otherwise W
  arbitrates and lands it: the request carries a required plain-English notice,
  forwarded to W with the daemon's addendum that landing is W's
  responsibility; land when done is refused; a closed W is reopened first.
- A BRANCH NO WORKSPACE OWNS (a subagent's) may land on main.
- MAIN LANDS ONLY INTO MAIN: branching from main and landing into any other
  branch is refused.
- NOTICES: every workspace that is branched from or forked from gets a brief
  notice when the derived workspace is created and when it lands; one combined
  notice when it is both.

## The combinations

Three axes, each with three values, from the requester's point of view:

- CONTEXT ORIGIN: self (fork the requester's transcript), other (fork another
  workspace X's transcript), fresh.
- BRANCH SOURCE: self (the requester's branch), other (another branch), main.
- MERGE TARGET: self (the requester's branch), other (another workspace's
  branch), main.

Each of the 27 combinations records: supported or refused (with the real
invariant), how common it is, and example situations.

| # | context | branch source | merge target | supported | prevalence | example situations |
|---|---|---|---|---|---|---|
| 1 | self | main | main | yes | common | tangential fix landed while work goes on; parallel subtask; trying a second approach; handing the work off with its context |
| 2 | self | self | self | yes | common | splitting a feature still in progress into pieces that land back into it; addressing review findings on the feature before it lands on main |
| 3 | self | self | main | NO: a workspace's branch lands only into that workspace (owner, 2026-10-16) | never | none |
| 4 | self | a branch no workspace owns (e.g. a subagent's) | main | yes; a branch an open workspace owns may not be the source here, since it lands only into that workspace (owner, 2026-10-16) | common | landing a subagent's finished branch; combining several branches into one landing; rebasing an old branch onto main and landing only the fix; finishing a nearly done branch and landing it |
| 5 | self | other (X) | the same other (X) | yes, but never with "land when done": the request must carry a plain-English NOTICE, which the daemon forwards to X with a guaranteed addendum saying that landing the work into X is X's responsibility; X lands it (`workspace merge <child>`); a closed X is reopened automatically first; the created workspace is told to communicate with X (owner, 2026-10-16) | rare | sending a fix the requester found into X's unlanded branch; handing X a piece it needs; unblocking X on a problem the requester already understands |
| 5b | self | other (B) | a different other (C) | NO: not for lack of any use, but its value is lower than the risk of encouraging workspaces to conflate their responsibilities through cross talk between unrelated workspaces (owner, 2026-10-16). A judgment for this cell only, not a general rule. | never | none |
| O1 | other (Y) | main | main | yes: to use Y's context to fix a problem on main, fork Y and branch from main (owner, 2026-10-16) | uncommon | one workspace finds a problem another workspace is already looking at, and fires off a one-shot forked from that workspace, from main onto main |
| O2 | other (Y) | Y | Y | yes, and it may land itself: forked from Y, it has Y's whole context, so Y need not arbitrate; Y is notified on creation and on landing (owner, 2026-10-16) | common | the subagent pattern with full workspaces: pieces of Y's work done in workspaces forked from Y, each landing into Y's branch |
| O3 | other (Y) | Y | main | NO: a workspace's branch lands only into that workspace (owner, 2026-10-16) | never | none |
| O4 | other (Y) | any branch | a different branch | NO: with another workspace's context, branch source and merge target never differ, except main into main (owner, 2026-10-16) | never | none |
| O5 | other (Y) | main | any other branch | NO (owner, 2026-10-16) | never | none |
| F1 | fresh | main | main | yes, situational (owner, 2026-10-16) | situational | a truly tangential bug the current context does not help with; a review that must not be biased by the existing context |
| F2 | fresh | W | W | yes, situational; W arbitrates and lands, since the new workspace did not fork W's context (owner, 2026-10-16) | situational | as F1, on W's unlanded work |
| U1 | forked from the workspace that spawned the subagent (owner, 2026-10-16) | a branch no workspace owns (the subagent's) | main | yes | common | landing a subagent's work directly on main |
| U2 | forked from the workspace that spawned the subagent | a branch no workspace owns (the subagent's) | the spawning workspace's branch | yes (owner, 2026-10-16) | common | many concurrent subagents landing into the requester's branch as each finishes |

## Enforcement and its gaps

The daemon refuses every combination the rules above refuse; the rules are
also written into the proto comments and the skills. What the daemon knows
today, and what it lacks:

- Branch ownership: derivable from the workspace registry (each workspace row
  records its branch). A closed workspace still owns its branch (it is reopened
  when needed); a forgotten one does not.
- An unowned branch: resolvable as a git ref; its worktree may or may not still
  exist, which does not matter since a create always cuts from the branch tip.
- No subagent context is ever forked (owner, 2026-10-16): U1 forks the workspace
  that spawned the subagent, so ForkSession needs no sidechain support. The
  request names that workspace explicitly; the daemon can check that the
  subagent's transcript (`<session>/subagents/agent-<id>.jsonl`) lies under
  that workspace's session.
- GAP: tying a subagent to its branch. Nothing records which branch a
  subagent worked on, so the request names both and the daemon can only check
  that each exists.
- A call from a directory that is no workspace (e.g. a subagent's worktree) has
  no requester and is refused.

## Subagent merges (owner, 2026-10-16)

- Landing a subagent's work is always an ASYNC MERGE, of a special kind: one
  that names the subagent it derives from. It never makes a full workspace;
  its merge workspace stays hidden, like every async merge's.
- U1 (onto main) and U2 (into the spawning workspace's branch) are equally
  valid uses.
- The request names the subagent's id; the daemon finds the spawning workspace
  by locating the subagent's transcript under that workspace's session, so no
  owner field states the same fact a second time.
- EVERY async merge (not only a subagent's) creates its merge workspace, and
  forks its context, WHEN THE MERGE STARTS, not when it is requested, so the
  fork carries every landing that happened while the merge waited (B, landing
  after A and C, knows they landed). The fork point is still the requester's
  last concluded turn; a turn in flight is left out, which is acceptable.
- Queue racing is being removed (owner, 2026-10-16), so merges run one at a
  time and each forks after the previous one landed. (On master as of
  2026-10-16 racing is still present: `daemon/internal/merge/racing.go`.)
