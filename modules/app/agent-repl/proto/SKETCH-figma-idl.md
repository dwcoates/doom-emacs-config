# SKETCH — figma→idl proto reorganization + compliance messages

Status: DRAFT for review, **round 3** (sketch gate open — not a frozen
contract). Edit freely; annotations welcome inline. The durable evidence layer
is frozen.

Round 1 was wire-neutral relocations + additive fields only. **Rounds 2 and 3
are not.** Round 2 restructured `ConversationItem` and replaced the async
design outright, because the round-1 async model was wrong at the root.
Round 3 revised the async kind arms from `{agent, journal, spool}` to
`{agent, journal, shell, unclassified}` and grounded a shell's settle path in
its exit status. Wire compatibility is deliberately not claimed while the
shape is still being iterated; retired field numbers are `reserved` so nothing
can be quietly reused if the reshape is accepted. See Parts 1b and 1c.

The materialized per-file form of this sketch is `figma-idl-draft/`; its
README carries the round-2 and round-3 change lists and the declaration
accounting. The `.proto` files themselves carry NO process narration — comments
there state what a message or field is and means, never how the draft got its
shape. This document and that README are where the history lives.

## Part 1 — new messages and additive changes

```proto
// ============================================================
// topbar.proto — NEW component file + NEW resolution
// The topbar today scrapes SessionView/WorkspaceState; this gives it
// resolved props. ModelOption RELOCATES here (the topbar owns the selector);
// SetModelCmd RELOCATES here (the selector's event).
// ============================================================

// One workspace's topbar, resolved completely by the daemon. The client
// renders these fields verbatim; nothing here is derived client-side.
message TopbarView {
  // The workspace this topbar describes; addressing only, never displayed
  // differently than given.
  string workspace = 1;
  // The exact title line the topbar shows (workspace name, or name + branch
  // when the daemon resolves a branch worth showing). Pre-composed so the
  // client never concatenates identity fragments.
  string title = 2;
  // The session identity line shown in the hover/expanded state, verbatim.
  string session_line = 3;
  // The model selector's current selection, as the display string the
  // selector button shows (e.g. "opus-5"). Empty means the selector renders
  // its placeholder.
  string model_display = 4;
  // The selectable models, in display order. RELOCATED ModelOption (field
  // semantics unchanged); the selector renders exactly this list.
  repeated ModelOption model_options = 5;
  // The connectivity glyph, resolved: which glyph and which color class to
  // draw, and the tooltip text. The client never maps connectivity enums.
  TopbarConnectivity connectivity = 6;
}

// The topbar's connectivity indicator, fully resolved. `tone` names a color
// class from the shared render-colors vocabulary table (the one deliberate
// shared authority — see design note 4); `glyph` and `title` are literal.
message TopbarConnectivity {
  // Vocabulary color-class name (e.g. "ok", "degraded", "severed").
  string tone = 1;
  // The literal glyph character(s) the indicator draws.
  string glyph = 2;
  // Tooltip text, verbatim.
  string title = 3;
}

// ============================================================
// footer.proto — ADDITIVE fields on ProgressView (existing fields untouched)
// Kills the two client-side derivations: RenderState→{word,tone,breathing}
// mapping in progress-footer.ts, and mergeStatusChip() text composition.
// ============================================================

// ADDITIVE to existing ProgressView:
message ProgressView {
  // ... existing fields 1..22 unchanged ...

  // The phase word, resolved: exactly what the footer's phase cell renders.
  // Replaces the client-side RenderState→word/tone/breathing table. The
  // footer's own copy of the phase fact (duplicate-don't-share):
  // WorkspaceState remains the authority for state; this is its footer
  // projection.
  FooterPhase phase = 23;
  // The merge chip, resolved: exactly the text and tooltip the chip renders,
  // or absent when no merge run is publishing. The footer's own projection of
  // MergeStatus (which other surfaces keep reading for their own
  // projections).
  FooterMergeChip merge_chip = 24;
}

// The footer phase cell's props. These are resolved DISPLAY properties, not a
// state: the state lives in WorkspaceState; the daemon projects it into the
// exact strings/flags the cell draws, so the client holds no mapping table.
message FooterPhase {
  // The word the cell renders, verbatim (e.g. "thinking", "compacting").
  string word = 1;
  // Vocabulary color-class name for the cell (shared render-colors table).
  string tone = 2;
  // Whether the word carries the breathing animation. True exactly for the
  // active phases; the client applies the animation without knowing which
  // phases are active.
  bool breathing = 3;
}

// The footer merge chip's props: the composed text ("merge queued 2/3") and
// its tooltip, resolved daemon-side from the merge run's own status. Absence
// of this message = no chip.
message FooterMergeChip {
  // Chip text, verbatim.
  string text = 1;
  // Tooltip, verbatim.
  string title = 2;
}

// ============================================================
// response-bubble.proto — assistant (purple) bubble arm + NEW resolved stamp
// The bubble's usage stamp today is client-converted from durable vendor
// shapes; this carries the daemon-resolved figures instead. Durable rows
// stay frozen as evidence; this is a view projection.
// ============================================================

// ADDITIVE to the assistant conversation-item arm:
//   ResponseUsageStamp usage_stamp = <next free field number>;

// The per-response usage stamp rendered on an assistant bubble's corner.
// Every figure is daemon-resolved via the canonical TokenUsage derivation
// (internal/tokenusage); the client renders verbatim and computes nothing.
message ResponseUsageStamp {
  // The response's expensive input: canonical input_misses total
  // (written + unwritten). The headline figure.
  int64 expensive_input_tokens = 1;
  // Cache-served input, for the expanded stamp's breakdown row.
  int64 cache_read_tokens = 2;
  // Output tokens, as billed.
  int64 output_tokens = 3;
  // The model that produced the response, display form. Empty for synthetic
  // records (never fabricated).
  string model = 4;
}

// ============================================================
// tokens-menu.proto — NEW component file + NEW resolution
// The counter menu today computes its rows from TokenUsageTotals buckets
// client-side (tokens.ts). This is the resolved replacement: the daemon
// computes every row; tokens.ts becomes a verbatim renderer.
// ============================================================

// The token-breakdown menu, fully resolved. Sections and rows arrive in
// display order; the client renders exactly this tree.
message TokenBreakdownView {
  // The workspace/session this breakdown describes.
  string workspace = 1;
  string session_id = 2;
  // Menu sections, in display order (e.g. "this turn", "session", per-model).
  repeated TokenBreakdownSection sections = 3;
}

// One titled section of the breakdown menu.
message TokenBreakdownSection {
  // Section heading, verbatim.
  string label = 1;
  // Rows, in display order.
  repeated TokenBreakdownRow rows = 2;
}

// One row of the breakdown menu. All numbers are resolved; the share is
// precomputed so the client does no arithmetic.
message TokenBreakdownRow {
  // Row label, verbatim (e.g. "uncached input", "fresh input", "cache read").
  string label = 1;
  // The row's token count.
  int64 tokens = 2;
  // The row's share of its section's basis, in permille (0-1000), already
  // rounded. -1 means "no share applies to this row" and the client omits
  // the percentage; 0 is a real zero percent.
  int32 share_permille = 3;
  // True when the row is a headline (rendered unindented/emphasized), false
  // for detail rows. Layout fact resolved daemon-side.
  bool emphasized = 4;
  // Indent depth for nested detail rows; 0 = top level.
  int32 depth = 5;
}

// ============================================================
// feed.proto — the compaction summary arm (unchanged from round 1)
// ============================================================

// The compaction summary bubble (the purple-washed summary block after a
// compaction). Explicit arm so the wash is a component, not an inference.
message CompactionSummaryItem {
  // The summary text, verbatim (markdown).
  string summary = 1;
  // When the compaction completed, unix millis.
  int64 compacted_at_ms = 2;
  // The resolved expensive-input cost of producing this summary (canonical
  // derivation), so the bubble can carry its own cost note; -1 when the
  // result's usage was unavailable (never fabricated as 0).
  int64 expensive_input_tokens = 3;
}
```

## Part 1b — ROUND 2: AgentEmission and the async bubble family

**Round 1's async design is withdrawn.** It modeled a detached agent as
`AsyncNoticeItem` — a resolved sentence saying that something was happening
elsewhere. That is not what the product does. A detached agent is a live,
growing, recursively nested conversation; a workflow is a live table of
journal rows; a background shell is a live byte spool. Round 2 models the
thing rather than a sentence about the thing.

### The idea

Everything an agent can produce in a conversation becomes ONE message,
`AgentEmission`, and that message is used in **both** places agent output
appears:

- wrapped by `ConversationItem` in the top-level feed, and
- wrapped by `AsyncAgentUpdate` inside a detached agent's bubble.

A detached agent is not a second, weaker kind of conversation — it is the
same conversation happening somewhere else. Saying that once on the wire means
every renderer written for the main feed renders a detached agent for free,
and the webapp's hand-written JSONL transcript parser has nothing left to do.

### What it retires on the client

Everything below is presently derived client-side, per frontend:

| Client machinery | Replaced by |
|---|---|
| `watchers.ts` three-tier identity ladder (classified → notification → **regexes over English result prose**) | `AgentToolCall.spawned_bubble_id`, resolved daemon-side |
| `async-stream.ts` `parseTranscript` (hand-written JSONL fold) | `AsyncAgentUpdate.emissions` — typed `AgentEmission`s |
| `async-stream.ts` `parseJournal` | `AsyncWorkflowJournalUpdate.rows` — typed `AsyncWorkflowJournalRow`s |
| `watcher-poll.ts` HTTP byte-offset tail poller | `AsyncOutputAppend` pushed on the existing frame stream |
| `stream-member.ts` seven-rule status precedence table | `AsyncLiveness` — one resolved oneof |
| `subfeed.ts` recursive walk + depth cap 5 + visited-set | `AsyncBubble.parent_bubble_id` — a flat tree of pointers |
| `STREAM_ITEM_CAP = 200` and the "N earlier entries not shown" notice | `AsyncFold { dropped_before, tail_cap }` |

### The proto

```proto
// ============================================================
// agent-emission.proto — NEW file. THE agent-output vocabulary.
// A oneof of everything an agent can produce, extracted from round-1
// ConversationItem's agent-output arms. Carries no uuid/ts/ordering: its two
// wrappers stamp those differently.
// ============================================================

message AgentEmission {
  oneof emission {
    // FROM ConversationItem.assistant_message = 10.
    AgentResponse response = 1;
    // FROM the thinking content blocks inside that same arm. Round-1
    // ConversationItem had no thinking arm; every renderer split it back out
    // of ApiAssistantMessage.content to draw the grey block. See the
    // exclusivity invariant on AgentResponse.body.
    AgentThinking thinking = 2;
    // FROM ConversationItem.tool_use = 12.
    AgentToolCall tool_call = 3;
    // FROM ConversationItem.tool_result = 13.
    AgentToolResult tool_result = 4;
    // FROM ConversationItem.tool_use_result = 14. Also THE subagent/task chip:
    // data.v1.ToolUseResult's own oneof already carries AgentAsyncLaunch,
    // AgentResult, TaskOutputResult, TaskStopResult and WorkflowLaunchResult,
    // so no parallel chip message is invented.
    AgentToolOutcome tool_outcome = 5;
    // FROM ConversationItem.skill_body = 34.
    SkillBodyItem skill_body = 6;
    // FROM ConversationItem.result = 15. Agent output, not feed bookkeeping:
    // a detached agent's transcript ends with exactly this record, and it is
    // where a bubble learns it finished and whether it failed.
    agentshim.data.v1.ResultMessage turn_result = 7;
  }
}

// ============================================================
// response-bubble.proto — now owns the arms it draws
// ============================================================

message AgentResponse {
  // EXCLUSIVITY INVARIANT: thinking blocks are NOT carried here — the daemon
  // emits them as AgentThinking and strips them from `content`. Both halves
  // are load bearing: without the strip a frontend draws the reasoning twice;
  // without the arm a frontend reaches into a durable message's content array
  // to find the grey block, which is what both frontends do today. Applies
  // uniformly to the main feed and the detached fold.
  agentshim.data.v1.ApiAssistantMessage body = 1;
  // Absent when the response carried no usage record; never zeros.
  ResponseUsageStamp usage_stamp = 2;
}

message AgentThinking {
  agentshim.data.v1.ThinkingBlock body = 1;
}

// ============================================================
// tool-call.proto — now owns the arms it draws, and publishes the
// daemon's async CLASSIFICATION verdict
// ============================================================

message AgentToolCall {
  agentshim.data.v1.ToolUseBlock call = 1;
  // THE CLASSIFICATION VERDICT. Non-empty exactly when this call detached
  // work that has its own AsyncBubble, and then equal to that bubble's id.
  // This is what retires watchers.ts's identity ladder. Empty means "this
  // call detached nothing" and that is the only reading of empty; a
  // detachment the daemon could not classify is a SystemFailureItem, never a
  // call silently rendered as synchronous.
  string spawned_bubble_id = 2;
}

message AgentToolResult {
  agentshim.data.v1.ToolResultBlock result = 1;
}

message AgentToolOutcome {
  agentshim.data.v1.ToolUseResult structured = 1;
  // ToolUseResult carries no correlation id of its own — the transcript
  // associates it POSITIONALLY with the tool_result line it rides, and a
  // positional association does not survive being pushed as its own emission.
  string tool_use_id = 2;
  // Same contract as AgentToolCall.spawned_bubble_id. On both messages
  // because the sidecar is where classification is READ from, while the call
  // is where a frontend needs the answer FIRST (the card is drawn before the
  // outcome lands). Same string whenever both are set.
  string spawned_bubble_id = 3;
}

// ============================================================
// async-bubble.proto — REPLACES round 1 entirely; kind arms revised in
// round 3 to {agent, journal, shell, unclassified}
// ============================================================

// One piece of detached work, and the unit every later update is addressed to.
message AsyncBubble {
  // THE ROUTING HANDLE. Stable for the bubble's whole life, unique within a
  // session, and the only thing an update carries to say where it lands. The
  // daemon mints it when it classifies the spawning tool call, and stamps the
  // same string on that call's AgentToolCall.spawned_bubble_id — so a
  // frontend never derives an id, it only ever matches one.
  //
  // Never empty. A detachment the daemon cannot attribute to a tool call at
  // all is a daemon fault surfaced as a SystemFailureItem, not a bubble with
  // a blank id. (Work whose TOOL is unrecognized is a different thing and has
  // its own kind arm — see `unclassified` below.)
  string id = 1;
  // The tool_use id of the call that spawned this work, for a frontend that
  // draws the bubble attached to its originating card rather than
  // free-standing. Empty only for work that no tool call spawned.
  string origin_tool_use_id = 2;
  // The id of the AsyncBubble this one was spawned FROM, empty at the top
  // level. Detached agents dispatch detached agents, so bubbles form a tree —
  // but the tree is expressed by parent POINTERS, not by nesting bubbles
  // inside each other, so an update addressed to a bubble deep in the tree is
  // routed by one map lookup rather than a recursive walk. Nothing here needs
  // a depth bound to terminate.
  string parent_bubble_id = 3;
  // The face the collapsed fold shows: the agent's task description, the
  // workflow's name, or the shell's command line. Empty means the daemon had
  // no label for the work, and the client shows the id.
  string label = 4;
  // When the work was launched, unix millis.
  int64 started_at_ms = 5;
  // Live or settled, and with what outcome. See AsyncLiveness.
  AsyncLiveness liveness = 6;
  // WHAT KIND of work this is, carrying that kind's content as folded so far.
  // A newly opened bubble carries an empty body; a bubble arriving in a
  // reconnect snapshot carries everything the daemon has folded to date.
  //
  // A skill invocation is deliberately absent: it is a synchronous card
  // (AgentEmission.skill_body), never detached work. Should skills ever
  // detach, they arrive as a new arm here. Nothing is pre-declared for that,
  // because a speculative arm is a shape no producer has to honour and no
  // consumer can test against.
  oneof kind {
    AsyncAgentBubble agent = 10;
    AsyncWorkflowJournal journal = 11;
    AsyncShellBubble shell = 12;
    AsyncUnclassifiedBubble unclassified = 13;
  }
}

// A detached agent: a whole conversation happening elsewhere.
message AsyncAgentBubble {
  // The agent's conversation so far, in emission order, in EXACTLY the
  // vocabulary the top-level feed uses. A frontend's renderer for a response
  // bubble, a thinking block or a tool card is the same code in both places.
  repeated AgentEmission emissions = 1;
  // Tail-cap accounting for `emissions`. See AsyncFold.
  AsyncFold fold = 2;
}

// A Workflow run's journal: the step log a Workflow launch writes.
//
// ONE PRODUCER. This kind exists for the Workflow tool and nothing else — its
// rows are the entries of that run's journal.jsonl step log, one row per
// logged step. It is not a general-purpose progress table, and work of any
// other kind never arrives here.
message AsyncWorkflowJournal {
  // The steps logged so far, in order.
  repeated AsyncWorkflowJournalRow rows = 1;
  // Tail-cap accounting for `rows`. See AsyncFold.
  AsyncFold fold = 2;
}

// A backgrounded shell command: an opaque byte spool with a command line.
message AsyncShellBubble {
  // The command line that was backgrounded, verbatim, for the bubble's header.
  // Empty only when the daemon launched the process without a reconstructible
  // command line.
  string command = 1;
  // Everything the command has written so far. See AsyncOutputSpool.
  AsyncOutputSpool output = 2;
}

// A spawn whose tool the daemon does not recognize.
//
// AN EXPLICIT ARM, NOT A FALLBACK. Unrecognized work is a first-class kind on
// this contract: the daemon states that it could not classify the tool, names
// the tool it could not classify, and streams the work's output anyway. A
// renderer must draw this bubble — it is real work the user started, and
// dropping it would make a running process invisible. The one thing a
// renderer may not do is guess: an unrecognized tool is not silently treated
// as a shell, an agent or a workflow.
message AsyncUnclassifiedBubble {
  // The tool that spawned the work, verbatim as the agent named it. This is
  // the fact that makes the arm useful rather than merely honest: it is what
  // tells a maintainer which tool needs a classification, and what lets a
  // frontend label the bubble with something truer than "unknown".
  string tool_name = 1;
  // Everything the work has written so far. See AsyncOutputSpool.
  AsyncOutputSpool output = 2;
}

// A verbatim byte spool and its delivery cursor.
//
// Deliberately unparsed and unstructured — this output is bytes, and
// pretending otherwise is how a renderer starts guessing at ANSI, line framing
// and JSON.
message AsyncOutputSpool {
  // Everything spooled so far, verbatim.
  string text = 1;
  // Bytes delivered so far, so a reconnecting client resumes rather than
  // re-fetches. Also the sequencing check on AsyncOutputAppend: an append
  // whose from_offset does not equal this value is a gap and must be rejected
  // loudly, not applied.
  uint64 through_offset = 2;
}

// One logged step of a Workflow run: a dot, a name and a detail line.
message AsyncWorkflowJournalRow {
  // The step's name — the phase or agent it reports on — verbatim.
  string label = 1;
  // The step's detail line, verbatim: the result on success, the error text on
  // failure, the prompt while running.
  string detail = 2;
  // The step's state, as arms rather than an enum, so that a state added later
  // is an arm a reader must handle rather than an integer it silently renders
  // as something else.
  oneof status {
    AsyncWorkflowStepRunning running = 10;
    AsyncWorkflowStepDone done = 11;
    AsyncWorkflowStepFailed failed = 12;
  }
}

// The step is still executing.
message AsyncWorkflowStepRunning {}

// The step completed successfully.
message AsyncWorkflowStepDone {}

// The step failed. The failure text is the row's `detail`; this arm carries no
// second copy of it.
message AsyncWorkflowStepFailed {}

// Live-or-settled, expressed as arms so that "settled" and "settled with what
// outcome" are one indivisible fact. A settled bubble with no outcome is
// unrepresentable.
message AsyncLiveness {
  oneof state {
    AsyncLive live = 1;
    AsyncSettled settled = 2;
  }
}

// The work is still running.
message AsyncLive {
  // When the daemon last saw this work produce anything, unix millis. 0 means
  // it has produced nothing since launch. It supports a "quiet for a while"
  // affordance and is NOT a liveness verdict: a silent agent is still a live
  // agent.
  int64 last_activity_ms = 1;
}

// The work has finished, one way or another.
message AsyncSettled {
  // When it finished, unix millis.
  int64 settled_at_ms = 1;
  // The process exit status, for work that IS a process — a shell bubble, and
  // an unclassified bubble the daemon ran as one. Absent for work with no exit
  // status of its own (an agent, a workflow), and absence is the only reading
  // of "this work did not exit, it concluded".
  //
  // It sits BESIDE the outcome rather than inside an outcome arm because the
  // exit status and the verdict are two different facts about one ending. The
  // daemon resolves `outcome` FROM this code — that mapping is not a client's
  // to make — while the code itself stays on the wire so a shell's card can
  // show "exited 137" rather than an unexplained red dot. Putting the code
  // inside `done` and `error` would force every reader to check two arms for
  // one field; putting the verdict inside the exit status would leave work
  // with no exit status unable to say how it ended.
  AsyncShellExit shell_exit = 2;
  // HOW it finished. Exactly one arm is always set.
  oneof outcome {
    AsyncOutcomeDone done = 10;
    AsyncOutcomeError error = 11;
    AsyncOutcomeKilled killed = 12;
  }
}

// A process's exit status.
message AsyncShellExit {
  // The exit code, as the shell reports it. A signal-terminated process
  // reports the conventional 128+N form, because that is what its exit status
  // literally is. 0 is a real zero and always means clean exit.
  int32 code = 1;
}

// Finished successfully. For a process, the daemon resolved this from an exit
// code of 0 (see AsyncSettled.shell_exit).
message AsyncOutcomeDone {}

// Finished by failing. For a process, the daemon resolved this from a nonzero
// exit code (see AsyncSettled.shell_exit).
message AsyncOutcomeError {
  // The failure, resolved for display. Empty when the source reported failure
  // without a reason — never filled with a manufactured one. A process's exit
  // code is NOT restated here; it rides AsyncSettled.shell_exit.
  string message = 1;
}

// Stopped from outside before it finished: an explicit stop, a cancellation,
// a session teardown. Distinct from error because the work did not fail — it
// was not allowed to conclude. A killed process still carries its exit status
// on AsyncSettled.shell_exit.
message AsyncOutcomeKilled {
  // Who or what stopped it, resolved for display. Empty when unattributed.
  string reason = 1;
}

// Tail-cap accounting for a bubble whose folded content is capped.
//
// The cap is a resolved daemon fact rather than per-frontend policy, so that
// two frontends cannot silently disagree about what the user is being shown:
// a fold that drops its oldest entries and says nothing is indistinguishable
// from a complete one.
//
// It applies to the item-counted kinds (an agent's emissions, a workflow's
// rows). A byte spool has no items to count and carries its delivery cursor
// instead — see AsyncOutputSpool.
message AsyncFold {
  // How many entries were dropped off the FRONT to honour the cap. 0 means
  // the fold is complete and no "earlier entries" notice is drawn.
  int64 dropped_before = 1;
  // The cap the daemon applied, so a notice can name it and so a complete
  // fold is distinguishable from one capped at exactly the limit.
  int32 tail_cap = 2;
}

// One incremental push to one bubble: the id routes it, the arm types it.
// Never a re-send of the whole bubble — an agent running for an hour would
// otherwise re-transmit its entire transcript on every new line.
message AsyncBubbleUpdate {
  // Which bubble this lands on. Must name a bubble the receiver has already
  // opened; an update for an unknown id is a gap and must be rejected loudly
  // and resynced, never buffered in the hope its bubble shows up.
  string bubble_id = 1;
  // WHAT changed. The arm MUST match the bubble's kind — a journal update
  // addressed to a shell bubble is a daemon bug and is rejected, not
  // coerced. Only `liveness` is kind-independent.
  //
  // `shell` and `unclassified` are distinct arms carrying the SAME message:
  // the arm names which kind the update is for, while the payload is one byte
  // append at one offset in both cases. The two kinds differ in what they ARE,
  // not in how their output arrives, so there is no axis along which a
  // duplicated pair of append messages could evolve apart — only a
  // gap-detection rule that would have to be kept identical in two places.
  oneof update {
    AsyncAgentUpdate agent = 10;
    AsyncWorkflowJournalUpdate journal = 11;
    AsyncOutputAppend shell = 12;
    AsyncOutputAppend unclassified = 13;
    AsyncLivenessUpdate liveness = 14;
  }
}

// New output from a detached agent.
message AsyncAgentUpdate {
  // The emissions produced since the last update, in order. THE SAME message
  // the top-level feed carries — see AgentEmission. A frontend appends these
  // to the bubble's emission list and renders them with its ordinary feed
  // renderers.
  repeated AgentEmission emissions = 1;
  // The bubble's fold accounting AFTER applying this update. Restated rather
  // than deltaed: it is small, and a dropped-count that drifts is worse than
  // one that is re-sent.
  AsyncFold fold = 2;
}

// New steps in a Workflow run's journal.
message AsyncWorkflowJournalUpdate {
  // Rows logged since the last update, in order.
  //
  // Rows are APPEND-ONLY and are never revised in place: a step that starts
  // running and later completes emits a running row and then a done row, and
  // the daemon does not rewrite history. A frontend that wants one row per
  // step collapses them by label; a frontend that wants the log renders them
  // all. Neither reading is imposed here.
  repeated AsyncWorkflowJournalRow rows = 1;
  // The bubble's fold accounting after applying this update.
  AsyncFold fold = 2;
}

// New bytes on a spool.
message AsyncOutputAppend {
  // The bytes, verbatim, to append.
  string text = 1;
  // The spool offset these bytes START at. MUST equal the bubble's current
  // AsyncOutputSpool.through_offset; anything else is a gap. Carried
  // explicitly so a gap is detectable at all — a bare append cannot tell a
  // lost chunk from a quiet one.
  uint64 from_offset = 2;
}

// A liveness transition: live to settled, or a settled outcome changing (a
// running agent that is then killed).
message AsyncLivenessUpdate {
  // The bubble's liveness after the transition.
  AsyncLiveness liveness = 1;
}

// The async push frame: bubbles that opened, and updates to bubbles already
// open.
message AsyncBubbleDelta {
  string workspace = 1;
  string session_id = 2;
  // Bubbles opening for the first time, or re-delivered in full after a
  // resync. A bubble the receiver already knows is REPLACED by this copy.
  repeated AsyncBubble opened = 3;
  // Incremental pushes to bubbles already open, in order.
  repeated AsyncBubbleUpdate updates = 4;
  // Frontends persist this for reconnect resync.
  uint64 through_seq = 5;
}

// ============================================================
// frame.proto — ADDITIVE
// ============================================================

// On FrontendFrame's oneof: its own frame rather than more ConversationDelta
// items, because a detached agent produces at its own rate and must not flood
// the conversation that dispatched it.
//   AsyncBubbleDelta async_bubble_delta = 20;
// On StateSnapshot: every bubble the session still holds, folded to date, so
// a reconnecting client resumes a running agent instead of re-parsing its
// transcript from byte zero. Each carries its own AsyncFold, so the client
// also knows exactly what it is NOT being shown.
//   repeated AsyncBubble async_bubbles = 11;
```

## Part 1c — ROUND 2: ConversationItem restructured

`ConversationItem` becomes feed-level PACKAGING around a payload.

**Why.** Round 1 had it carry, side by side, six arms of agent output and
seven arms of everything else. That made agent output a thing only the
top-level feed could express — so a detached agent's conversation, which is
the same output arriving from somewhere else, had no way to be said on the
wire at all, and every frontend parsed the agent's raw JSONL transcript itself
to recover it. Extracting `AgentEmission` is what makes the async family above
possible; restructuring `ConversationItem` is the other half of the same move.

**Wire compatibility is not claimed for this round** — the shape is still
being iterated ahead of contract freeze. Retired numbers are `reserved` so
that if the reshape is accepted, no retired number is ever reused for
something with different semantics.

```proto
message ConversationItem {
  reserved 16, 17, 18;  // round-1 retirements, unchanged
  reserved "api_error", "compact_boundary", "compact_boundary_line";
  // RESTRUCTURED (round 2): the agent-output arms, now inside AgentEmission.
  // 40 was usage_stamp, which rode the envelope in round 1 only because its
  // arm was a data.v1 message this package must not amend; AgentResponse
  // wraps that message now, so the stamp sits on the thing it stamps.
  reserved 10, 12, 13, 14, 15, 34, 40;
  reserved "assistant_message", "tool_use", "tool_result", "tool_use_result",
      "result", "skill_body", "usage_stamp";

  // ---- Feed packaging: identity, ordering, provenance ----
  string uuid = 1;
  int64 ts_ms = 2;
  string request_id = 3;
  ConversationSource source = 4;

  // ---- Payload ----
  oneof item {
    // EVERYTHING the agent produced, in the one vocabulary that also carries
    // a detached agent's output.
    AgentEmission agent = 5;

    // ---- Non-agent items: things the agent did not emit ----
    agentshim.data.v1.ApiUserMessage user_message = 11;
    // Agent-CAUSED but not agent-emitted: a question addressed to the user
    // and answered by a command, so it is an interaction, not an utterance,
    // and it has no meaning inside a detached agent's fold.
    agentshim.core.v1.PermissionItem permission = 30;
    SystemFailureItem system_failure = 31;
    agentshim.core.v1.ContextCleared context_cleared = 32;
    agentshim.core.v1.ContextCompacted context_compacted = 33;
    SessionCommandItem session_command = 35;
    // The bubble's OPENING state, anchored where the work was launched.
    // Everything it produces afterwards arrives as AsyncBubbleUpdate on its
    // own delta — a detached agent emitting a thousand lines must not insert
    // a thousand rows into the conversation it was dispatched from.
    AsyncBubble async_bubble = 38;
    CompactionSummaryItem compaction_summary = 39;
  }

  // ---- Resolution stamps: facts about the item, not the item ----
  repeated TokenUtilization token_utilization = 36;
  // Stays on the envelope: it is the SESSION's ledger for the turn,
  // reconciled across subagents, with no meaning for one detached agent.
  TurnAccounting turn_accounting = 37;
}
```

## Part 4 — ROUND 4 REMEDIATION TABLE

Every field removed from the webapp-facing surface, what it was actually for,
and what does that job now. Three cuts share this table: session identity,
durable persistence shapes, and the failure class/type pair.

### 4a. Session identity → an opaque generation fence

The frontend's vocabulary loses "session" entirely. `current-vs-stale` is the
only concept a renderer needs, and the fence is the only token it needs to
answer it. It is compared **byte-wise** and never parsed.

| Removed field | What it was actually for | Post-change mechanism |
|---|---|---|
| `ConversationDelta.session_id = 2` | staleness stamp on a per-workspace push | `ConversationDelta.fence = 5`; client compares to `WorkspaceState.fence` |
| `TypingDelta.session_id = 2` | same | `TypingDelta.fence = 4` |
| `SessionInitView.session_id = 2` | same | `SessionInitView.fence = 4` |
| `QueueView.session_id = 2` | same | `QueueView.fence = 4` |
| `TokenBreakdownView.session_id = 2` | same | `TokenBreakdownView.fence = 4` |
| `TaskCatalog.session_id = 2` | same | `TaskCatalog.fence = 4` |
| `AsyncBubbleDelta.session_id = 2` | same | `AsyncBubbleDelta.fence = 6` |
| `HeartbeatView.session_id = 2` | same | `HeartbeatView.fence = 4` |
| `ProgressView.session_id = 2` | documented "CORRELATION ONLY"; `store.ts:1131-1134` confirms it never writes identity | `ProgressView.fence = 25` — correlation without naming a session |
| `ResyncCmd.session_id = 2` + `controller_generation_id = 3` | the outbound precondition pair, captured at decision time (`resync-snapshot.ts:19-29`) and refused daemon-side on mismatch | `ResyncCmd.fence = 4` — one echo, same capture-at-decision-time rule, same daemon-side refusal |
| `CreateSessionCmd`-side `observed_claude_session_id` (`CommandAck` field 7) | log attribution before the first push; explicitly "MUST NOT PERSIST OR FEED BACK" | unchanged for the host; a rendering frontend never sends `CreateSessionCmd` (`main.ts:1388` — only the unaddressed page, which the primary deployment does not use) |
| `QueueEntryRevivalHold.session_id = 1` | rendered only as a `data-revival-session-id` DOM attribute (`render.ts:572`) | removed outright; the entry already rides its workspace's queue, so the arm's presence is the whole fact |
| `SessionView.session_id`, `.claude_session_id`, … | Emacs's session catalog | **unchanged — host surface.** See the splits below |
| `QueryTerminationFailure.agent_repl_session_id = 1` | displayed as card text (`render.ts:2246`) | reserved; the card's account is daemon-composed prose. The **vendor** conversation stays typed — see 4d |
| `SessionResumeFailure.agent_repl_session_id = 1` | same | reserved; `claude_session_id` (vendor) stays |

**Added:** `WorkspaceState.fence = 19` — the authoritative current fence, and
the value every fenced push is compared against. WorkspaceState remains the
**sole identity authority**; the fence is its projection, minted anew whenever
the owning session or controller generation rotates, so "the fence changed" and
"what I was talking to was replaced" are the same event.

### 4b. Transport addressing — architecture notes, not proto changes

These are session-addressed HTTP/WS routes. Removing session identity from the
push surface does not by itself move them; each needs a workspace-scoped
endpoint with the daemon resolving workspace → live session server-side.

| Current addressing | Site | Remediation |
|---|---|---|
| `/sessions/{id}/stream` | `address.ts:76` | workspace-scoped stream endpoint. The `?workspace=` page **already** uses `/workspace-stream?workspace=…` (`address.ts:74`) and is the primary deployment; the `?session=` address is what retires |
| `GET /sessions` membership probe | `ws.ts:452-461` (`makeSessionExistsProbe`) | a workspace-existence probe; `onGone` then means "this workspace is gone", which is what `FailureWorkspaceGone` now says |
| `/sessions/{id}/account`, `/login`, `/tasks/{id}/tail`, `/add-support`, `/chess-game` | `account.ts:36,118`; `login.ts:69,92,104`; `watcher-poll.ts:41`; `unsupported.ts:67`; `chess-game.ts:222` | workspace-scoped equivalents. `watcher-poll.ts` retires outright under round 2 (async updates are pushed) |
| `SessionIdentityGate` | `session-identity.ts` (whole file) | deletes: it exists only because the session-scoped URLs need a non-empty id before they can be built |
| `createSession` → `SessionView` correlation set | `command-dispatch.ts:265, 698-715` | host-only concern; the primary deployment never creates from the browser |

### 4c. Durable evidence → resolved component views

The webapp must only ever see resolved views. The daemon already owns the
digestion — `internal/tokenusage` (canonical shape), `internal/frontend`
(`AggregateTokenUtilization`), `internal/sessioncontroller` (the turn-accounting
reducer and its reconciliation) — so this moves the last mile, not the work.

| Removed from the push surface | What the client did with it | Post-change mechanism |
|---|---|---|
| `ConversationItem.token_utilization = 36` (`repeated TokenUtilization`) | per-response rows in the token menu, incl. `JSON.stringify(toJson(TokenUtilizationSchema, …))` dumped into a menu row (`tokens.ts:411`) | reserved. `TokenBreakdownView` (resolved rows) |
| `ConversationItem.turn_accounting = 37` (`TurnAccounting`) | the accounting chip: verdict from the oneof arm, prose from `problems.map(p => p.kind).join(", ")`, evidence-presence sentence from five `!== undefined` checks, and tps/quota/token arithmetic (`turn-accounting.ts:16-84`) | reserved. `FooterAccountingCell` — `summary` composed daemon-side, `oneof verdict {complete, incomplete, invalid}`, problems as **display-ready phrases** |
| `SessionView.token_utilization = 22` (`SessionTokenUtilization`) | every row of the token menu, incl. per-model canonical conversion the client had to do itself (`tokens.ts:354-361`, whose comment says the daemon *cannot* resolve it — the durable row is frozen) | reserved. `TokenBreakdownView`, where the daemon resolves per-model rows because a view is not a frozen row |
| the topbar's accounting text | `topbar.ts:176-180`, same client-side summary | `TopbarView.accounting_line` — a resolved string, since the topbar renders text and no verdict |

**`durable.proto` keeps every message, name, package and field number
unchanged.** The finding that permits this to be a pure push-surface change:
no `agentshim.frontend.v1` message is ever `Any`-wrapped (`frontend.proto` does
not import `any.proto`; nothing calls `anypb.New`), and persistence is binary
protobuf into BLOB columns (`statedb/turnaccounting.go:26-39`,
`statedb/tokenutilization.go:27-33`) with **no type-name discriminator
anywhere** — so the constraint on these messages is field-number and population
stability, not name stability. Nothing was renamed regardless, because nothing
needed to be.

**Achieved invariant:** no file in the draft imports `durable.proto`, and no
message outside it names a type declared in it. Verified mechanically.

### 4d. Failure class + type string → one typed kind vocabulary

`ErrorClass` was an enum for a **condition**, and `error_type` was a free string
carrying 50 values no renderer could match on. `FailureKind` replaces both.

| Removed | What it was for | Post-change mechanism |
|---|---|---|
| `enum ErrorClass` | the card's color — the only thing any renderer branched on (`render.ts:2200`, `progress-footer.ts:775`) | deleted. Each `FailureKind` arm belongs to exactly one side and its comment states which; the color keys on the kind |
| `SystemFailureItem.error_class = 1` | same | gone with the message |
| `SystemFailureItem.error_type = 2` | reached the DOM as an inert `data-error-type` attribute with **no CSS rule**; the only real branch on it was a store-side lifecycle decision (`store.ts:729-745`, retract-vs-settle for connectivity windows) | the arm IS the type. The retract-vs-settle decision becomes the `FailureCardView.lifecycle` arms, which the daemon sets |
| `SystemFailureItem.source_detail = 4` (free text carrying smuggled structs) | rendered by exactly one surface (`render.ts:2202-2204`) | `FailureCardView.detail` for the prose; the structured facts inside it become **typed fields on the owning arm** (see below) |
| `SystemFailureItem.resolved_at_ms = 5` (0 = open) | a magic zero distinguishing open from settled | `oneof lifecycle {open, resolved, terminal}` — and `terminal` is a state the old field could not express at all |
| `SystemFailureItem.item_uuid = 6` | addressing from outside the feed | `FailureCardRef`, embedded by the surfaces that need to point at a card |
| `oneof structured_detail {session_resume, query_termination}` | typed evidence for two kinds only | those two are now **arms of the kind oneof** (`session_resume_failed`, `query_termination`), carrying the same evidence messages |

**Vendor identity is CONTENT and stays.** This is the deliberate asymmetry with
4a and the two must be read together:

- **Fencing identity is removed.** An agent-repl `session_id` used to decide
  whether a push is current, or to address a route, is machinery the renderer
  should never have held. The fence replaces it.
- **Vendor identity is typed INTO the arm.** A `claude_session_id` on a
  vendor-side failure is a *fact about the conversation the failure happened
  in* — something the card shows so the user can find the transcript or quote
  the request. It is content, like the message text, and it belongs on the arm
  that owns it: `VendorFailureContext {claude_session_id, api_request_id,
  api_message_id}` on the vendor arms, `claude_session_id` on
  `FailureConversationUnresumable`, and the existing vendor identity inside
  `QueryTerminationFailure`.

The distinction is not "which ids survive" but "what the id is for": routing and
staleness go, displayable facts stay.

### 4e. One failure vocabulary, per-surface carriers

Structure derived from a survey of where failures actually render, not from the
classifier outward. `errors.proto` becomes the vocabulary file; each surface
gets its own resolved props in its own component file, under
duplicate-don't-share.

| Surface (survey site) | What it actually reads | Its carrier |
|---|---|---|
| feed failure card — `render.ts:2198-2217` | all six fields + the typed detail union; window-reconciles on `failure:${uuid}` | `FailureCardView` in **failure-card.proto** — kind, message, detail, `oneof lifecycle` |
| footer failure row — `progress-footer.ts:768-779` | **only** `uuid`, `errorClass`, `message`. No evidence, no stamp; stands until the next turn | `FooterFailureRow` in **footer.proto** — message, tone, `FailureCardRef` |
| roster rail notice — `sidebar.ts:646-674` | a bare string; self-clears after 4000 ms | `RosterNotice` in **sidebar.proto** — text, `auto_clear_ms` |
| command refusal — `command-dispatch.ts:639-641` → `CommandAck.failure` | the kind and a card to point at | `CommandAck.failure` (`FailureKind`) + `failure_card` (`FailureCardRef`) in **frame.proto** |
| async settled error — round 3 | an outcome message on the bubble | `AsyncOutcomeError` in **async-bubble.proto**, already its own carrier |
| terminal session account — `SessionView.death` | Emacs's dead-session card | `FailureCardView` on the host surface |
| topbar `#conn-status` / `#remediation` — `main.ts:1005,1237,1263,827` | plain strings, timed self-clear | already resolved strings on `TopbarView` |
| boot failure — `main.ts:1693-1699` | hand-written DOM, store-free, duplicating the card's shape | `FailureBootFailed` arm; the duplication is a client bug the vocabulary makes unnecessary |

**Client-local failures.** The webapp mints 8 of its own (`local-failure.ts`
plus two in `command-dispatch.ts`), always `INTERNAL`, always with
`detail:{kind:"none"}`, evidence packed into `source_detail`. **Decision: they
are arms of the same `FailureKind` oneof**, numbered from 100 up, with the
producer split stated on the message: the daemon mints every arm below 100, a
frontend mints only those from 100 up, and neither sets the other's. One
vocabulary means one renderer and one set of tests; a separate local vocabulary
would be a second shape for the same picture. The number split makes the
producer boundary unbreakable rather than conventional, and it mirrors the
`client.` namespace the daemon already reserves (`errclass.ClientPrefix`,
`IsDaemonType`).

`client.session_gone` becomes `FailureWorkspaceGone` — the one local kind whose
name carried session vocabulary.

## Part 2 — file manifest (wire-neutral relocations; same package, same field numbers)

| New file | Contents |
|---|---|
| `sidebar.proto` | WorkspaceRoster, RosterSection/RepoSection/TaskSection/RepositoryView/TaskView, RosterRow, all 24 RosterRowStatus* arms, PublishWorkspaceRosterCmd |
| `agent-emission.proto` **(round 2, new)** | AgentEmission — the agent-output vocabulary shared by the feed and the async bubbles |
| `response-bubble.proto` **(round 2)** | AgentResponse, AgentThinking, ResponseUsageStamp |
| `user-bubble.proto` | user-prompt arm message(s) — still declaration-free; the prompt is not agent output |
| `async-bubble.proto` **(round 2 rewrite, round 3 kind arms)** | AsyncBubble + 4 kind bodies (agent / workflow journal / shell / unclassified), AsyncOutputSpool, AsyncWorkflowJournalRow + 3 step arms, AsyncLiveness/Live/Settled + AsyncShellExit + 3 outcome arms, AsyncFold, AsyncBubbleUpdate + 5 update arms, AsyncBubbleDelta (24 declarations; AsyncNoticeItem withdrawn) |
| `tool-call.proto` **(round 2)** | AgentToolCall, AgentToolResult, AgentToolOutcome, TaskEntry, TaskCatalog |
| `permission-card.proto` | permission-request arm, PermissionAnswerCmd |
| `topbar.proto` | new TopbarView/TopbarConnectivity, relocated ModelOption, SetModelCmd, health views + cmds |
| `errors.proto` **(round 4, rewritten as a VOCABULARY file)** | FailureKind + 58 kind arms (50 daemon, 8 client-local), VendorFailureContext, QueryTerminationFailure, SessionResumeFailure + 5 arms. ErrorClass and SystemFailureItem RETIRED |
| `failure-card.proto` **(round 4, new)** | FailureCardView + 3 lifecycle arms, FailureCardRef |
| `footer.proto` **(round 4)** | ProgressView (+ `fence`, + `accounting`), ProgressWindow, RateLimitWindow, InterruptWindow, ContextCostAlert, FooterPhase/FooterMergeChip, new FooterFailureRow, FooterAccountingCell + 3 verdict arms |
| `tokens-menu.proto` | new TokenBreakdownView/Section/Row |
| `prompt-queue.proto` **(round 3, renamed from `composer.proto`)** | SubmitPromptCmd, InterruptCmd, QueueEntry (+ `oneof hold` over its 3 hold arms), QueueEntryShutdown/KeepAlive/RevivalHold, QueueView, QueueClassification, QueueForce/Accept/CancelCmd |
| `gate-revival.proto` **(round 4)** | HibernationDetail + 3 cause arms, ReviveSessionCmd + 2 mode arms, new WorkspaceGateView + 2 gate arms |
| `slash-menu.proto` | SessionCommand, SessionCommandItem, SkillBodyItem |
| `feed.proto` (container) **(round 2)** | ConversationItem RESTRUCTURED into packaging + payload, ConversationDelta, ConversationSource, TypingDelta, SessionInitView, new CompactionSummaryItem |
| `merge.proto` (shared fact) | MergeStatus + 8 phase arms |
| `state.proto` (plumbing) **(round 4)** | WorkspaceState (+ `fence`), SessionView (HOST SURFACE), RenderState, connectivity/status enums, RuntimeFault, BackfillState, DaemonView, HeartbeatView (fenced) |
| `frame.proto` (transport) **(round 2)** | FrontendFrame (+ async_bubble_delta = 20), StateSnapshot (+ async_bubbles = 11), FrontendCommand, CommandAck, InterruptConfirmRequired, ResyncCmd |
| `host.proto` | HostAction family, workspace create/open/close/merge cmds, WorkspaceAvailable, WorkspaceMaterializedCmd |
| `lifecycle.proto` | create/delete/restart/hibernate/shutdown cmds + schedule/hold messages, ClientLogCmd/Level |
| `durable.proto` (frozen evidence) **(round 4: PERSISTENCE ONLY)** | unchanged in every message, name and field number; header now documents that nothing here is ever pushed and no component may import it |
| `tokens.proto` (shared vocabulary) | canonical TokenUsage, TokenCacheHits, TokenCacheMisses |

## Part 3 — design notes / open decisions

1. FooterPhase is resolved display props (word/tone/breathing), deliberately
   NOT a oneof-of-states: the state authority stays in WorkspaceState; this
   message is its footer projection. Giving the footer arms would re-create a
   second state vocabulary.
2. FooterMergeChip: today's client-side mergeStatusChip() text composition
   moves daemon-side verbatim — the purest duplicate-don't-share example.
3. share_permille = -1 as "no share applies": keeps a real 0% distinct from
   "no percentage rendered" without a wrapper message.
4. OPEN (user): `tone` fields name color classes from the shared
   render-colors vocabulary table — the one shared authority this sketch
   keeps. Alternative: resolve literal colors per component (fields keep
   shape, change meaning).
5. OPEN (user): Emacs surfaces (tab-bar, mode-line, dots) are treated as
   HOST here — WorkspaceState keeps its multi-consumer role in state.proto.
   Alternative: their own component files with resolved views.
6. ~~The two new ConversationItem arms are sketched from the known inference
   suspects~~ — **resolved in round 2.** The inference sites were confirmed
   against the running webapp. `CompactionSummaryItem` stands. `AsyncNoticeItem`
   was wrong at the root and is withdrawn: the async surface is not a notice,
   it is a live sub-conversation, and Part 1b replaces it.

### Round-2 design notes

7. **The daemon takes over the whole async pipeline.** Today the webapp reads
   a detached agent's raw JSONL transcript over HTTP and parses it itself.
   Round 2 has the daemon parse and stream it, pushing typed
   `AgentEmission`s. Consequently `async-stream.ts` and `watcher-poll.ts`
   retire outright, and `subfeed.ts` / `stream-member.ts` lose their parsing
   and status-precedence halves. This is the single largest client deletion in
   the reorganization, and it is the reason `AgentEmission` is worth
   extracting at all.

8. **The identity ladder becomes daemon-side classification.** `watchers.ts`
   presently decides which bubble a tool call spawned by trying a structured
   sidecar, then a notification's task id, then **three regexes over English
   result prose**. That ladder cannot be replicated identically by a second
   frontend, so two frontends would eventually disagree about which bubble an
   update belongs to. The daemon runs it once and publishes only the answer,
   as `spawned_bubble_id` on `AgentToolCall` / `AgentToolOutcome` and as
   `AsyncBubble.id`. Frontends match ids; they never derive them.

9. **Nesting is pointers, not recursion.** `subfeed.ts` walks nested
   transcripts recursively, guarded by a depth cap of 5 and a visited-set —
   guards that exist only because the walk is recursive. `AsyncBubble` carries
   `parent_bubble_id` and lives in a flat map, so routing an update to a
   depth-4 bubble is one lookup, and no cap is needed to keep it terminating.

10. **The 200-item cap becomes a resolved fact.** `STREAM_ITEM_CAP = 200` is
    client policy today, and the "N earlier entries not shown" notice is
    computed from it. `AsyncFold { dropped_before, tail_cap }` states both
    daemon-side, so two frontends cannot silently disagree about what the user
    is being shown.

11. **No `skill` async kind.** The classifier routes only Task/Agent,
    Workflow and background Bash; `Skill` falls through to "not async" and a
    skill invocation renders as a synchronous card. No arm is pre-declared for
    a hypothetical detached skill — a speculative arm is a shape nobody has to
    honour. It is noted as reserved-for-future in a comment instead.

12. **OPEN (user): the thinking carve-out.** `AgentResponse.body` carries
    `ApiAssistantMessage` with its thinking blocks STRIPPED, and thinking is
    emitted as its own `AgentThinking` arm. This is the one place round 2
    carries a durable `data.v1` message non-verbatim. It is done because both
    frontends already split thinking out of `content` to draw the grey block,
    and because carrying it in both places would have a frontend render the
    reasoning twice. The alternative is to drop `AgentThinking` and leave
    every renderer splitting content blocks itself.

13. **OPEN (user): journal rows are append-only.** A step that starts and then
    completes emits a running row and then a done row; the daemon does not
    revise rows in place. This matches the current parser, which produces one
    row per journal line. The alternative — stable row ids with in-place
    revision — is a bigger contract and was not assumed.

### Round-3 design notes

14. **Four kind arms, each naming one producer.** Round 2's `spool` arm was
    the generic case: "bytes from something". Round 3 splits it, so that every
    arm names a producer the daemon can actually point at.
    - `agent` — unchanged.
    - `journal` → `AsyncWorkflowJournal`, renamed so the message says what it
      is: the Workflow tool's run journal, one row per `journal.jsonl` step.
      Nothing else ever produces this kind.
    - `shell` — a backgrounded shell command, carrying `command` (the command
      line) so the bubble header has a real face rather than a task id.
    - `unclassified` — a spawn whose TOOL the daemon does not recognize,
      carrying `tool_name`.

15. **`unclassified` is a contract arm, not a fallback.** The generic `spool`
    arm it replaces was a place unknown work could land silently, and the
    tool's name — the one fact that would let anyone fix the gap — was
    discarded on the way in. As its own arm it says three things a default
    cannot: that classification was attempted, that it failed, and what it
    failed on. A renderer must draw the bubble (it is real work the user
    started, and dropping it makes a running process invisible), and may not
    guess the kind.

16. **The shell exit code sits beside the outcome, not inside it.**
    `AsyncSettled` gains `AsyncShellExit shell_exit`, a sibling of the
    `oneof outcome`, present for work that is a process and absent for work
    that has no exit status (an agent, a workflow).
    - The exit status and the verdict are two facts about one ending. The
      daemon resolves `done`/`error` FROM the code — that mapping is not a
      client's to make — while the code stays on the wire so a card can show
      "exited 137" rather than an unexplained red dot.
    - Putting the code inside `done` and `error` would force every reader to
      check two arms for one field; making the exit status the outcome would
      leave exit-less work unable to say how it ended.
    - `settled`-without-an-outcome stays unrepresentable: the outcome oneof is
      untouched, and a killed process still reports its status.

17. **`QueueEntry`'s holds become a oneof.** `shutdown_hold` /
    `keep_alive_hold` / `revival_hold` fold into
    `oneof hold { shutdown = 7; keep_alive = 8; revival = 9; }`.
    - A drain lease, an in-flight keep-alive turn and a pending revival are
      mutually exclusive session conditions. As plain fields they could
      co-set, and an entry held by two of them would have two different sets
      of exits and two different bubbles to render — a no-co-set violation the
      shape permitted rather than prevented.
    - Field numbers are unchanged, so nothing needs reserving; only the names
      shorten, since `_hold` is redundant inside `hold`.
    - No arm set still means the ordinary case: a turn is running and the
      classifier decides delivery.

18. **`composer.proto` → `prompt-queue.proto`.** The primary deployment passes
    `?composer=0` and Emacs owns the input box, so the file's rendered
    component is the queued-prompt strip, not a composer. What the file covers
    is the prompt-intake lifecycle — submit → classify → hold → deliver /
    force / cancel — plus interrupt, and the new name says so. `frame.proto`
    was its only importer.

19. **`AsyncOutputAppend` is shared by `shell` and `unclassified`.** They are
    two arms of `AsyncBubbleUpdate` carrying the same message: the arm names
    the kind, the payload is one byte append at one offset in both cases. The
    two kinds differ in what they ARE, not in how their output arrives, so
    there is no axis along which duplicated append messages could evolve apart
    — only a gap-detection rule that would then have to be kept identical in
    two places. The same reasoning makes `AsyncOutputSpool` (text + delivery
    cursor) shared by both bubble bodies.

### Round-4 design notes

20. **The fence is opaque on purpose.** It is compared byte-wise and never
    parsed, so its composition stays the daemon's to change. A client that
    learned to decode it — to see a session id, a generation counter, a
    timestamp — would be depending on a fact this contract does not offer, and
    the next change to how generations are minted would break it.

21. **WorkspaceState keeps identity authority; the fence is its projection.**
    The daemon re-mints the fence exactly when the workspace's owning session
    or controller generation rotates, so "the fence changed" and "what I was
    talking to was replaced" are one event rather than two facts that can
    disagree. Emacs keeps reading `session_id` from the same message.

22. **Host and rendering surfaces are now stated per message.** Emacs
    legitimately manages sessions, so `SessionView`, `SessionHealthView` /
    `SessionHealthCmd` and all of `lifecycle.proto` keep session identity and
    are marked HOST SURFACE. Nothing was stripped from the host's view; the
    webapp's side of those messages became fenced component views instead.

23. **Transport addressing is an architecture item, not a proto item.** The
    `/sessions/{id}/…` route family and the `GET /sessions` existence probe
    need workspace-scoped endpoints with the daemon resolving workspace → live
    session server-side. The primary deployment already streams over
    `/workspace-stream?workspace=…`, so the session-scoped page address is what
    retires. Part 4b lists each route.

24. **A durable row and a view answer different questions.** A persistence
    record carries every dimension the vendor reported, in the vendor's own
    partitioning, with the redundancy an audit needs. Handing one to a renderer
    makes the renderer decide what it means — and two renderers then decide
    differently. The daemon already owns the digestion (`internal/tokenusage`,
    `AggregateTokenUtilization`, the turn-accounting reducer); round 4 moves
    only the last mile.

25. **The accounting verdict is arms, and its problems are prose.** The client
    used to build "INVALID ACCOUNTING: …" by joining oneof discriminator names
    and then appending a list of absent fragments. Neither is a renderer's job:
    the daemon composes the phrases and the client concatenates them.
    `incomplete` and `invalid` are separate arms because they are different
    facts — evidence missing versus evidence that contradicts itself — and the
    old shape rendered them identically.

26. **A failure's kind IS its class.** Verified against the registry before
    cutting: `SystemFailureItem` is constructed in exactly 13 places, all in
    `errclass.go`, and every one sets the class to a compile-time constant.
    The type→class mapping is a total function and **no kind is bi-class**, so
    the class carries no information the arm does not. Near-misses that are
    NOT bi-class: four `api.*` types arrive through two doors (HTTP status and
    stop reason) but both are vendor-side; `unexpected_query_termination`
    arrives with and without typed evidence but is machinery-side both times.

27. **Tone rendering keys on the kind.** Each arm's comment names the side it
    belongs to and the color that side resolves to (machinery BLUE, vendor
    PURPLE), so a renderer maps arm → vocabulary entry the way it maps every
    other tone today. This is pending open decision 4 (color-class names versus
    literal colors); whichever way that lands, the mapping input is the arm.

28. **The failure structure was derived from the surfaces, not the
    classifier.** The survey found that only ONE surface renders a failure
    whole; the footer row reads three fields; the roster notice reads a bare
    string and self-clears; three suspected banner surfaces (reconnect,
    resync-rejection, version-skew) render no failure content at all and were
    not given carriers. Shaping outward from the classifier would have given
    every surface the same over-large message and left four of them ignoring
    most of it.

29. **Client-local failures share the vocabulary, split by number.** The
    frontend mints 8 kinds the daemon cannot observe. They are arms 100+ of the
    same oneof, with the producer rule stated on `FailureKind`: the daemon owns
    everything below 100, a frontend owns 100 and up, and neither sets the
    other's. A separate local vocabulary would be a second shape for the same
    picture and a second renderer to keep in step.

## Part 5 — ROUND 5: pre-freeze orchestrator audit, and the FREEZE

A final pass over the whole materialized suite against the three landed
principles (no session awareness, oneof pattern, figma→idl), performed by the
orchestrator under the user's pre-freeze authorization. Eight findings, all
remediated in the draft files; the full change list is the "Round 5 changes"
section of `figma-idl-draft/README.md`. Headlines:

- **Transport gap (structural):** `TopbarView`, `TokenBreakdownView` and
  `WorkspaceGateView` — the round-4 webapp replacements for host-surface
  `SessionView` — had no `FrontendFrame` arm and no `StateSnapshot` field.
  Added arms 21/22/23 and snapshot fields 12/13/14; `TopbarView` also gains
  the standard `fence = 8`.
- **Oneof doctrine:** `QueueClassification` (enum verdict + two verdict-scoped
  plain fields) folded into `QueueEntry`'s `oneof classification`;
  `TaskEntry.kind`/`.status` free strings folded into oneofs mirroring the
  async-bubble vocabulary.
- **De-sessioning annotations:** `WorkspaceState.controller_generation_id`,
  `ShutdownHold.session_id` marked HOST SURFACE; the `SessionView` GUI-strip
  rule stated on the transport; `lifecycle.proto` header carves out the shared
  `ShutdownScheduleView` family; one stale `SessionView.hibernation` comment
  repointed at `WorkspaceGateView`.
- **Deliberate residue:** the legacy state enums (`RenderState`,
  `SessionConnectivity`, `SessionStatus`, `BackfillState`) stay — SSM
  state-log-coupled, host/daemon surface, webapp renderings already replaced
  by resolved views — deferred to a later approval-gated pass. `TaskCatalog`
  vs the async-bubble family flagged as an implementation-phase retirement
  question for the orchestrator's proto-change gate.

**THE CONTRACT IS FROZEN as of this round.** The frozen artifact is the byte
content of `figma-idl-draft/*.proto` at the master commit tagged
`figma-idl-base`'s working state (the draft is untracked; the tag marks the
baseline the implementation lands on). Every implementation agent receives
the draft files verbatim and may not deviate; a deviation or follow-on proto
change is surfaced to the ORCHESTRATOR, who alone approves or revises it.
