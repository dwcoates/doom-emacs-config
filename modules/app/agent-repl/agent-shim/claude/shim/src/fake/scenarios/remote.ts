/**
 * fake/scenarios/remote.ts — a session reached from claude.ai through the
 * vendor's Remote Control bridge.
 *
 * # What the bridge does that no prompt of the shim's can
 *
 * With the bridge on (`Query.enableRemoteControl`), three things reach the
 * session from claude.ai, or its mobile app, past the shim entirely:
 *
 *   - A PROMPT typed there is queued by the vendor itself and runs as a turn
 *     the shim did not start; its echo (`--replay-user-messages`) is the only
 *     word of what was said (`!remote-prompt`).
 *   - A STOP pressed there aborts the running turn inside the vendor: the turn
 *     ends interrupted with no `interrupt()` from the shim (`!remote-interrupt`).
 *   - An ASK answered there is withdrawn from this host: the vendor cancels the
 *     `canUseTool` it posed, which aborts the signal it handed the callback, and
 *     the call goes on as claude.ai decided (`!perm-answered-remotely`). A stop
 *     pressed while an ask stands withdraws it the same way, and the turn then
 *     ends interrupted instead (`!perm-remote-interrupt`).
 *
 * # Declared, not capture-grounded
 *
 * No capture records a bridged session; the shapes are the CLI's as read from
 * the pinned binary (the bridge's `onInboundMessage`, `onInterrupt` and the
 * `control_cancel_request` it sends for a prompt answered elsewhere).
 */
import { askPermission, conclude, scenario, withheldThinking } from "./support.js";

/** What `!remote-prompt` types on claude.ai when it names nothing. */
const DEFAULT_REMOTE_PROMPT = "What changed since I left?";

const REMOTE_PROMPT = scenario({
  name: "remote-prompt",
  prompt: "!remote-prompt",
  emits:
    "this turn's own conclusion; then, with the Remote Control bridge on, a prompt typed on claude.ai (the args, " +
    "else a default) that the vendor queues itself and runs as a turn of its own, echoed under a uuid the shim never sent",
  writes: "the closing text line; the remote prompt's user line (origin human) and its turn's lines",
  arms: "PROMPT_ORIGIN_REMOTE_CONTROL with `said` (the adopted turn the remote prompt starts)",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "remote-prompt" }, "fake remote prompt turn");
    const text = ctx.args === "" ? DEFAULT_REMOTE_PROMPT : ctx.args;
    const uuid = ctx.injectRemotePrompt(text);
    conclude(ctx, uuid === undefined ? "Remote Control is off; nothing came from claude.ai." : "A prompt is on its way from claude.ai.");
  },
});

const REMOTE_INTERRUPT = scenario({
  name: "remote-interrupt",
  prompt: "!remote-interrupt",
  emits:
    "the start of an answer, then the turn aborted INSIDE the vendor (a stop pressed on claude.ai): the interrupt " +
    "terminal (`aborted_streaming`) with no `interrupt()` from the shim",
  writes: "the partial text line",
  arms: "AgentInterrupted.by_user with no command (a turn ended by a stop the shim did not make)",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "remote-interrupt" }, "fake turn stopped from claude.ai");
    ctx.assistant([withheldThinking(), { type: "text", text: "Let me look into" }], { stopReason: null });
    ctx.result({
      subtype: "error_during_execution",
      terminalReason: "aborted_streaming",
      errors: ["Interrupted by user"],
    });
  },
});

const PERM_ANSWERED_REMOTELY = scenario({
  name: "perm-answered-remotely",
  prompt: "!perm-answered-remotely",
  emits:
    "a gated `Bash` whose ask the vendor WITHDRAWS (its `canUseTool` signal aborted, as a `control_cancel_request` " +
    "does when claude.ai answered first); the call then runs, as claude.ai allowed it",
  writes: "the tool_use line, the tool_result line, the closing text line",
  arms: "AgentPermission.start + AgentPermissionSuccess.answered_remotely",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "perm-answered-remotely" }, "fake permission answered on claude.ai");
    const call = ctx.toolUse("Bash", { command: "git push" });
    const withdrawal = new AbortController();
    // NOT AWAITED: the vendor stopped waiting on this host the moment it
    // withdrew the ask, whatever the callback answers afterwards.
    void askPermission(ctx, call, { signal: withdrawal.signal });
    await ctx.tick();
    withdrawal.abort();
    ctx.log.debug({ tool_use_id: call.toolUseId }, "fake vendor withdrew the ask: claude.ai answered it");
    ctx.toolResult(call, "pushed\n", {
      stdout: "pushed\n",
      stderr: "",
      interrupted: false,
      isImage: false,
      noOutputExpected: false,
    });
    conclude(ctx, "Pushed, as allowed on claude.ai.");
  },
});

const PERM_REMOTE_INTERRUPT = scenario({
  name: "perm-remote-interrupt",
  prompt: "!perm-remote-interrupt",
  emits:
    "a gated `Bash` whose ask the vendor WITHDRAWS because a stop pressed on claude.ai aborted the turn: the call's " +
    "cancelled tool_result, then the interrupt terminal, with no `interrupt()` from the shim",
  writes: "the tool_use line, the cancelled tool_result line",
  arms: "AgentPermission.start + AgentPermissionDenied.by=user (the interrupt's), never answered_remotely",
  async run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "perm-remote-interrupt" }, "fake ask cut by a stop from claude.ai");
    const call = ctx.toolUse("Bash", { command: "git push --force" });
    const withdrawal = new AbortController();
    void askPermission(ctx, call, { signal: withdrawal.signal });
    await ctx.tick();
    withdrawal.abort();
    ctx.log.debug({ tool_use_id: call.toolUseId }, "fake vendor withdrew the ask: the turn was stopped");
    ctx.toolResult(call, "The user doesn't want to proceed with this tool use.", "Interrupted by user", { isError: true });
    ctx.result({
      subtype: "error_during_execution",
      terminalReason: "aborted_tools",
      errors: ["Interrupted by user"],
    });
  },
});

export const REMOTE_SCENARIOS = [REMOTE_PROMPT, REMOTE_INTERRUPT, PERM_ANSWERED_REMOTELY, PERM_REMOTE_INTERRUPT];
