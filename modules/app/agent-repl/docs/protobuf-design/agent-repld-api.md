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

- Which workspace events `workspace subscribe` offers (proposed: turn started,
  turn ended, needs input, landed, closed), delivered as prompts in the
  subscriber's session.
- Whether a workspace's merge target may change after creation (`workspace
  set`), given that `SetWorkspaceParent` retargets today.
- Whether a create always cuts a new worktree from the branch tip, never taking
  over an existing worktree (recommended: always cut).
- Whether "merge me" lands at the end of the agent's turn (recommended) or
  immediately.
- `/agent-repl-install`'s description change: run on its own when a dependency
  is missing, or update its manifests when a change adds one, or both.
- What `SPC o h` becomes once `/agent-repl-help` is removed.
- The fork source's default leaves out the requester's current turn (the fork
  point is its last concluded turn).
- Whether the command-file ingress is retired entirely, which needs its other
  writers (Emacs) moved onto RPCs.

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
| O1 | other (Y) | main | main | yes: to use Y's context to fix a problem on main, fork Y and branch from main (owner, 2026-10-16) | to settle | to settle |
| O2 | other (Y) | Y | Y | yes, and it may land itself: forked from Y, it has Y's whole context, so Y need not arbitrate; Y is notified on creation and on landing (owner, 2026-10-16) | to settle | to settle |
| O3 | other (Y) | Y | main | NO: a workspace's branch lands only into that workspace (owner, 2026-10-16) | never | none |
| O4 | other (Y) | any branch | a different branch | NO: with another workspace's context, branch source and merge target never differ, except main into main (owner, 2026-10-16) | never | none |
| O5 | other (Y) | main | any other branch | NO (owner, 2026-10-16) | never | none |
| F1 | fresh | main | main | yes, situational (owner, 2026-10-16) | situational | a truly tangential bug the current context does not help with; a review that must not be biased by the existing context |
| F2 | fresh | W | W | yes, situational; W arbitrates and lands, since the new workspace did not fork W's context (owner, 2026-10-16) | situational | as F1, on W's unlanded work |
| U1 | the subagent's own context (typically) | a branch no workspace owns (the subagent's) | main | yes (owner, 2026-10-16) | common | landing a subagent's work: a workspace bootstrapped from the subagent's branch and the subagent's context, so it can repair and land that work |

## Enforcement and its gaps

The daemon refuses every combination the rules above refuse; the rules are
also written into the proto comments and the skills. What the daemon knows
today, and what it lacks:

- Branch ownership: derivable from the workspace registry (each workspace row
  records its branch). A closed workspace still owns its branch (it is reopened
  when needed); a forgotten one does not.
- An unowned branch: resolvable as a git ref; its worktree may or may not still
  exist, which does not matter since a create always cuts from the branch tip.
- GAP: a SUBAGENT'S CONTEXT. A subagent's transcript lives under its parent
  session (`<session>/subagents/agent-<id>.jsonl`, sidechain records), and
  nothing today forks it: ForkSession forks only a workspace's main
  transcript. The context origin needs a fourth kind, "subagent S of
  workspace W", and the shim must fork a sidechain transcript into a session
  of its own.
- GAP: tying a subagent to its branch. Nothing records which branch a
  subagent worked on, so the request names both and the daemon can only check
  that each exists.
- A call from a directory that is no workspace (e.g. a subagent's worktree) has
  no requester and is refused.
