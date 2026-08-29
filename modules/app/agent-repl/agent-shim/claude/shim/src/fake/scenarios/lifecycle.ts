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
import { conclude, scenario } from "./support.js";

export const HOLD = scenario({
  name: "hold",
  prompt: "!hold",
  emits:
    "an assistant message frame and then NOTHING: the turn stays in flight until an interrupt lands, and ends " +
    "the way an interrupted turn does — no content, an error result",
  writes: "the opening assistant line, the prompt line and (at the interrupt) the turn record",
  arms: "AgentInterrupted.by_user, reached without any permission question",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "hold" }, "fake HOLD turn; it ends only on an interrupt");
    ctx.assistant([{ type: "text", text: "Working…" }], { stopReason: null });
    // The park is armed in the same synchronous run as the frame above, so
    // there is no window in which the turn is live and the stop has nothing to
    // resolve: a consumer only sees the frame after the resolver is in place.
    await ctx.awaitInterrupt();
    ctx.log({ turn: ctx.turn }, "fake HOLD turn released by an interrupt");
    // No explicit result: the engine emits the interrupt terminal, which is the
    // ONE place that shape is spelled, so a scenario cannot get it wrong.
  },
});

export const INTERRUPT_MID_TOOL = scenario({
  name: "interrupt",
  prompt: "!interrupt",
  emits:
    "a tool call the interrupt lands INSIDE: the assistant message is marked `aborted`, the tool result reports " +
    "`interrupted: true`, and the turn ends `error_during_execution` / `aborted_streaming`",
  writes: "the aborted assistant line, the interrupted tool_result line, the prompt line and the turn record",
  arms: "AgentBashInterrupted.cause=by_user and AgentInterrupted.by_user",
  async run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "interrupt" }, "fake mid-tool interrupt turn");
    const call = ctx.toolUse("Bash", { command: "sleep 600" }, { aborted: true });
    await ctx.awaitInterrupt();
    ctx.log({ turn: ctx.turn }, "fake interrupt landed inside a tool call");
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

export const QUERY_EOF = scenario({
  name: "query-eof",
  prompt: "!query-eof",
  emits: "NOTHING, and then the iterable ENDS — the turn never terminates. The CLI going away cleanly mid-turn",
  writes: "the prompt line only; there is no turn record because there was no turn end",
  arms: "SessionQueryDied.cause=unexpected_eof",
  run(ctx) {
    ctx.log({ level: "warn", turn: ctx.turn, branch: "query-eof" }, "fake query ENDING with no result");
    ctx.endStream();
  },
});

export const QUERY_FAIL = scenario({
  name: "query-fail",
  prompt: "!query-fail",
  emits: "NOTHING, and then the iterable REJECTS — the producer died rather than finished",
  writes: "the prompt line only",
  arms: "SessionQueryDied.cause=iterator_failure",
  run(ctx) {
    ctx.log({ level: "warn", turn: ctx.turn, branch: "query-fail" }, "fake query FAILING its iterable");
    ctx.failStream(new Error("fake vendor query died mid-turn"));
  },
});

export const KEEPALIVE_ECHO = scenario({
  name: "keepalive",
  prompt: "!keepalive",
  emits:
    "an ordinary short turn. It exists so a test can drive a keep-alive-shaped turn deterministically; the " +
    "`<!--agent-repl:keepalive-->` marker is the SHIM's, and the mock never adds or removes it",
  writes: "the assistant line, the prompt line (marker and all) and the turn record",
  arms: "AgentResponse.from_model, AgentSuccess.completed — classified keep-alive by the marker on the PROMPT",
  run(ctx) {
    ctx.log({ turn: ctx.turn, branch: "keepalive" }, "fake keep-alive-shaped turn");
    conclude(ctx, "ok");
  },
});

export const LIFECYCLE_SCENARIOS = [HOLD, INTERRUPT_MID_TOOL, QUERY_EOF, QUERY_FAIL, KEEPALIVE_ECHO];
