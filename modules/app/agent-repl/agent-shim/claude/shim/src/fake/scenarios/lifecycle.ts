/**
 * fake/scenarios/lifecycle.ts — turns that end the query, or refuse to end.
 *
 * These are the scenarios that exist for the SHIM's own machinery rather than
 * for the conversation: a turn that stays in flight so a test can act while it
 * runs, an interrupt that lands mid-tool, and the two ways a query can die.
 *
 * # The two query deaths are opposite facts
 *
 * `SessionQueryUnexpectedEof` is the iterable ENDING with no result — the CLI
 * went away cleanly and the turn simply never terminated.
 * `SessionQueryIteratorFailure` is the iterable REJECTING. A consumer that
 * treated both as EOF would silently erase every producer failure, which is
 * why the mock can produce each on demand.
 */
import { askPermission, conclude, scenario } from "./support.js";

const HOLD = scenario({
  name: "hold",
  prompt: "!hold",
  emits:
    "an assistant message frame and then NOTHING: the turn stays in flight until an interrupt lands, and ends " +
    "the way an interrupted turn does — no content, an error result",
  writes: "the opening assistant line, the prompt line and (at the interrupt) the turn record",
  arms: "AgentInterrupted.by_user, reached without any permission question",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "hold" }, "fake HOLD turn; it ends only on an interrupt");
    ctx.assistant([{ type: "text", text: "Working…" }], { stopReason: null });
    // The park is armed in the same synchronous run as the frame above, so
    // there is no window in which the turn is live and the stop has nothing to
    // resolve: a consumer only sees the frame after the resolver is in place.
    await ctx.awaitInterrupt();
    ctx.log.debug({ turn: ctx.turn }, "fake HOLD turn released by an interrupt");
    // No explicit result: the engine emits the interrupt terminal, which is the
    // ONE place that shape is spelled, so a scenario cannot get it wrong.
  },
});

const INTERRUPT_MID_TOOL = scenario({
  name: "interrupt",
  prompt: "!interrupt",
  emits:
    "a tool call the interrupt lands INSIDE: the assistant message is marked `aborted`, the tool result reports " +
    "`interrupted: true`, and the turn ends `error_during_execution` / `aborted_streaming`",
  writes: "the aborted assistant line, the interrupted tool_result line, the prompt line and the turn record",
  arms: "AgentBashInterrupted.cause=by_user and AgentInterrupted.by_user",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "interrupt" }, "fake mid-tool interrupt turn");
    const call = ctx.toolUse("Bash", { command: "sleep 600" }, { aborted: true });
    await ctx.awaitInterrupt();
    ctx.log.debug({ turn: ctx.turn }, "fake interrupt landed inside a tool call");
    ctx.toolResult(call, "", {
      stdout: "",
      stderr: "",
      // THE interrupted flag. Without it the run is indistinguishable from one
      // that simply produced nothing.
      interrupted: true,
      isImage: false,
      noOutputExpected: false,
    });
    // The engine emits the interrupt terminal.
  },
});

const QUERY_EOF = scenario({
  name: "query-eof",
  prompt: "!query-eof",
  emits: "NOTHING, and then the iterable ENDS — the turn never terminates. The CLI going away cleanly mid-turn",
  writes: "the prompt line only; there is no turn record because there was no turn end",
  arms: "SessionQueryDied.cause=unexpected_eof",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "query-eof" }, "the fake query ends with no result");
    ctx.endStream();
  },
});

const QUERY_FAIL = scenario({
  name: "query-fail",
  prompt: "!query-fail",
  emits: "NOTHING, and then the iterable REJECTS — the producer died rather than finished",
  writes: "the prompt line only",
  arms: "SessionQueryDied.cause=iterator_failure",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "query-fail" }, "the fake query fails its iterable");
    ctx.failStream(new Error("fake vendor query died mid-turn"));
  },
});

const QUERY_EOF_MID_ASK = scenario({
  name: "query-eof-mid-ask",
  prompt: "!query-eof-mid-ask",
  emits:
    "a gated `Bash` whose `canUseTool` ask is opened and then NEVER answered by the vendor: the iterable ENDS " +
    "with the callback still pending. THE ASK IS OPENED BEFORE THE DEATH, which is the whole point — an " +
    "unresolved `canUseTool` promise wedges the vendor process, so the query-death path owes every pending " +
    "callback a denial",
  writes: "the tool_use line and the prompt line; there is no turn record because there was no turn end",
  arms: "SessionQueryDied.cause=unexpected_eof with an AgentPermission settling denied",
  async run(ctx) {
    ctx.log.debug(
      { turn: ctx.turn, branch: "query-eof-mid-ask" },
      "the fake query ends with a permission ask still open",
    );
    const call = ctx.toolUse("Bash", { command: "git status" });
    // The gate OPENS and PERSISTS the ask synchronously before it hands back
    // the promise, so the stream below always dies with a genuinely open ask
    // rather than racing one into existence. Never awaited here: only the
    // shim's stand-down can resolve it.
    void askPermission(ctx, call);
    ctx.endStream();
  },
});

const KEEPALIVE_ECHO = scenario({
  name: "keepalive",
  prompt: "!keepalive",
  emits:
    "an ordinary short turn. It exists so a test can drive a keep-alive-shaped turn deterministically; the " +
    "`<!--agent-repl:keepalive-->` marker is the SHIM's, and the mock never adds or removes it",
  writes: "the assistant line, the prompt line (marker and all) and the turn record",
  arms: "AgentResponse.from_model, AgentSuccess.completed — classified keep-alive by the marker on the PROMPT",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "keepalive" }, "fake keep-alive-shaped turn");
    conclude(ctx, "ok");
  },
});

const QUEUE_VENDOR_TURN = scenario({
  name: "queue-vendor-turn",
  prompt: "!queue-vendor-turn",
  emits:
    "an ordinary short turn, and then — ahead of the NEXT send's own turn — a turn the vendor runs ON ITS OWN, the " +
    "way a background task's notification starts one: an assistant answer and a result with " +
    "`origin: {kind: \"task-notification\"}`, and NO `user_message_uuid` anywhere, because no send asked for it",
  writes: "the assistant line, the prompt line and the turn record, then the vendor turn's assistant line and record",
  arms:
    "AgentResponse.from_model, AgentSuccess.completed — twice, the second answering nobody. Grounded in the " +
    "2026-09-23 keep-alive leak, where such a turn's result closed the shim's keep-alive early",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "queue-vendor-turn" }, "fake turn that queues a vendor turn of its own");
    ctx.queueVendorTurn();
    conclude(ctx, "ok");
  },
});

const STOP_ON_REWIND = scenario({
  name: "stop-on-rewind",
  prompt: "!stop-on-rewind",
  emits:
    "an ordinary short turn, and it ARMS the session: from then on every truncating resume (a keep-alive " +
    "rewind's `resumeSessionAt`) reports, ahead of that query's first send, a background shell the previous " +
    "process left unfinished: `task_notification{status:\"stopped\"}` naming no task kind, then a turn the " +
    "vendor runs ON ITS OWN that answers nothing (a lone result with `origin: {kind: \"task-notification\"}`)",
  writes:
    "the assistant line, the prompt line and the turn record, a per-session mark under the account root, and on " +
    "each replay an enqueue/dequeue pair and a TRANSCRIPT-ONLY task-notification user record (`queueTranscriptOnly`, " +
    "`promptSource: system`) parented on the fork point, which the next send's prompt parents on; no reply, no turn record",
  arms:
    "AgentResponse.from_model, AgentSuccess.completed — and on each rewind a reply-less turn answering nobody. " +
    "Grounded in the ship-gns transcript and shim log of 2026-10-02 15:17:08Z (CLI 2.1.280), where every " +
    "keep-alive rewind replayed the stop",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "stop-on-rewind" }, "fake turn that arms the stop replay on every rewind");
    ctx.armStopOnRewind();
    conclude(ctx, "ok");
  },
});

export const LIFECYCLE_SCENARIOS = [
  HOLD,
  INTERRUPT_MID_TOOL,
  QUERY_EOF,
  QUERY_EOF_MID_ASK,
  QUERY_FAIL,
  KEEPALIVE_ECHO,
  QUEUE_VENDOR_TURN,
  STOP_ON_REWIND,
];
