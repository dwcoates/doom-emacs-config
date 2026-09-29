# Footer: the `working` status and an activity line through every lull

## Problem

Between the moments the feed moves, a turn can go quiet: a tool call's result
has landed and is on its way back to the model, and nothing new is drawn until
the model's next response starts. The footer's activity line is usually empty
then, so the user cannot tell that anything is happening. The owner wants the
invariant: while a turn runs, EITHER the feed is visibly moving (a tool call
running, a response streaming) OR the activity line says something — for
example "✅ Bash finished — the agent is handling the result", salient, and
cleared the moment the next feed item lands.

Separately, the footer's in-turn status is `thinking` with a `thinking`
substatus that stands for the whole turn. The owner wants the status called
`working`, and its substatus to name what the main agent is doing right now:
`thinking` only while the agent is running a (sync) inference call,
`executing` while a (sync) tool call runs, `writing` for a file write,
`reading` for a read, and so on.

## Context

- **The working steps.** While a turn is in flight the footer's step names
  what the MAIN agent is doing now: `thinking` (an inference call, no sync
  tool running), `executing` (bash, MCP tools, and any tool without its own
  step), `reading` (read), `writing` (write, edit), `searching` (grep, glob,
  web search), `fetching` (web fetch), `delegating` (a sync subagent);
  `submitting`, `clearing` and `compacting` stand as they are. Owner agreed to
  the separate searching / fetching / delegating steps.
- **The lull line: from the moment a feed item LANDS until the next SURFACES.**
  The quiet stretch is between one feed item landing fully and the next feed
  item surfacing partially. The activity line names what just landed (for a
  tool call: ✅ or ❌, the tool, and that the agent is handling it) and CLEARS
  THE MOMENT THE NEXT ITEM SURFACES — a response that starts streaming clears
  it at its first fragment, not when it finishes. The owner: this stretch
  between items is "the main point".
- **Not only while working.** The lull line also serves the `background`
  status (for example a subagent's message to the main agent surfacing).
  While a turn is in flight the footer reads `working` even if background work
  also runs, and background items are NOT surfaced on the activity line then.
- **Every activity line is one line.** An activity update is always a single
  line, truncated with an ellipsis when it would overflow; codified in the
  webapp's AGENTS.md.

## Landed changes

### 1. `FooterStatus.working` (was `thinking`), its steps, and `quiet_stretch`

- **What.**
  - `FooterStatus.thinking = 2` is renamed `working = 2`, and its messages
    `FooterStatusThinking*` / `FooterSubStatusThinking*` become
    `FooterStatusWorking*` / `FooterSubStatusWorking*`. The tag numbers are
    unchanged; `render-colors.json`'s `footer_status` key follows the arm name
    (still red). The roster's `thinking` arm is untouched.
  - New `FooterStatusWorking.substatus` arms: `executing = 6`, `reading = 7`,
    `writing = 8`, `searching = 9`, `fetching = 10`, `delegating = 11`
    (`activity` already holds tag 5).
  - New activity kind `FooterStatusActivityQuietStretch { string text }` on
    `FooterStatusWorkingActivity.quiet_stretch = 11` and
    `FooterStatusBackgroundActivity.quiet_stretch = 7`.
  - The rendering rules state the activity cell is always exactly one line,
    truncated with an ellipsis.
- **Why.** Items 1–3 of the plan below. One composed `text`, like the
  compaction line: the daemon words the line, the client draws it verbatim.
- **Precedence.** Fault, update, notification, compaction, hook, retry, then
  the quiet-stretch line, then the injected item, rate limit and budget. A
  notification outranks the line only until the next feed item lands: that
  landing replaces the notification (the notification's documented lifetime,
  "shown until the next activity replaces it").

## Implementation notes

- **The step and the line are the footer resolver's**
  (`daemon/internal/resolve/footer/quietstretch.go`). Every AgentActivity item
  arm is read through one table (`feedKinds`: label, step, whether the feed
  draws it) and every item oneof arm through another (`itemPhases`); unit
  tests pin that both cover the whole contract.
- **Only the main agent's items count while a turn runs**, so the watcher names
  the main agent to the footer as it does to the feed
  (`sessionwatcher.FooterSink.OnMainAgent`).
- **A prompt's delivery is a landing**: the turn-open edge stands
  "✅ Prompt delivered — awaiting response..." until the first item surfaces.
- **Detached work leaves the turn**: a detached announcement lands its unit
  ("✅ Moved to background — continuing..."), a monitor hands off at its
  first frame, and later frames of either are never a step.
- **Background lines** stand from a detached run's own terminal
  ("✅ Subagent finished", "❌ Bash failed") and from any item landing with no
  turn in flight; they are dropped when no detached work remains.

## THE PLAN (owner-approved 2026-09-29; read this whole file after any compaction)

### What changes — exactly this, nothing else

1. **Rename the footer's in-turn status `thinking` → `working`.** COSMETIC
   ONLY: same semantics, same moments, same colour. The roster, sidebar and
   tab bar are NOT touched at all (their `thinking` arm stays as it is).
2. **The working status's substatus names what the MAIN agent is doing now**
   (sync only): `thinking` (an inference call, no sync tool running),
   `executing` (bash, MCP tools, and any tool without its own step),
   `reading` (read), `writing` (write, edit), `searching` (grep, glob, web
   search), `fetching` (web fetch), `delegating` (a sync subagent).
   `submitting`, `clearing`, `compacting` stand as they are. Derived in the
   daemon's footer resolver from the activity starts/ends it already receives
   (`footer.resolver.OnActivity`); no shim change expected.
3. **The footer's activity section shows useful information during QUIET
   STRETCHES.** The activity section is and stays ONE line; nothing is added
   to it. During a quiet stretch its one line says what is happening in the
   gap, e.g. `✅ Bash finished — handling result...` or
   `❌ Read failed — handling failure...`. NEVER say "agent" in these lines.
   Other items that land (a prompt delivered, a response settled, a
   subagent's message) get a line of the same form.
4. **The activity section is always exactly one line**, truncated with an
   ellipsis when it would overflow. The footer itself (barring its expanded
   section) never grows in height. The status and substatus may wrap on word
   boundaries. Codify the one-line activity rule in the webapp's AGENTS.md.

### Definitions (codify in `modules/app/agent-repl/AGENTS.md`)

- **QUIET STRETCH**: the period between the moment a feed item has FULLY
  LANDED and the moment the next feed item FIRST SURFACES (partially — it need
  not have landed). The quiet-stretch line CLEARS THE MOMENT THE NEXT ITEM
  SURFACES (a streaming response clears it at its first fragment, not when it
  finishes).
- The line is legal under the `working` status AND under the `background`
  status (e.g. a subagent's message to the main agent surfacing). While a turn
  is in flight the status is `working` even if background work runs, and
  background items are NOT surfaced on the activity line then.
- Precedence: a standing fault, a deploy update, a hook and a retry outrank
  the quiet-stretch line.
- Invariant: while a turn is in flight, either a feed item is surfacing /
  running or the activity line is set; the daemon records an ERROR for a quiet
  stretch with no line, so a missed gap is loud.

### No scope creep — what that means

- Touch only items 1–4 above.
- No change to the roster, tab bar or sidebar; no rename there.
- No line-count or layout change anywhere except the activity section's
  one-line rule; feed items keep their layout.
- No other footer status, activity kind, colour or visual style changes.
- No new RPCs. The footer API (`frontend.v1` footer.proto) may be extended
  ONLY where genuinely required for the new information; expected: the
  renamed status message/arm, the new substatus arms, and the quiet-stretch
  activity kind(s) on the working and background activity oneofs.
- Other systems change only as far as they must to consume the new shapes
  (daemon footer resolver, webapp footer, generated bindings, tests).
- Anything noticed outside this line is MENTIONED at the end, never done.

### How the work runs

- Owner-approved: NO pauses for the owner between steps. Take it home
  end-to-end; stop only if development DISCOVERS something truly
  consequential I am not comfortable resolving myself.
- Do the work myself in-session (no implementation subagents).
- Order: proto shapes in `frontend/v1/footer.proto` (landed + recorded in
  this file as they are decided) → regenerate bindings → daemon footer
  resolver (+ unit tests, one edge case per test, canonical logging) → webapp
  footer (+ vitest, typecheck) → daemon integration / e2e as touched → AGENTS.md
  (agent-repl quiet-stretch definition; webapp one-line activity rule) →
  REMEDIATION-CHANGELOG line → full test-all green.
- Commit granularly on `workspace-turn-notifications` (atomic units;
  extractions as their own commits); consolidation sweep before landing.

### Then: the pending notifications branch (DO NOT FORGET)

- Wait for another workspace/agent to notify that the e2e fixes are on local
  `master`. On that notification: `git rebase master`, then run EVERY suite
  (`bin/test-all.sh`, including `e2e-emacs` — the owner started Docker).
- Remaining known failures before that: `TestBashDetachedStartAndComplete`,
  `TestBashDetachedNonzeroExit` (shim/sidecar both write the detached shell's
  terminal row; the shim's lacks the exit code; last writer wins). Owner
  expects these fixed on master; if still red, fix or surface.
- When ALL tests pass: CHERRY-PICK (not merge; the merge queue is broken and
  bypassed) the branch's commits onto local `master`.
- Deploying the landed build is deferred until this footer work resolves
  (the new daemon requires `WatchDaemonEmacs.focus`, so daemon and Emacs must
  update together).
- Quit Docker when done with it.
