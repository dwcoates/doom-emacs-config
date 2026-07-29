/**
 * A scripted, offline stand-in for the Agent SDK's `query()`. Used by
 * `--fake` mode so the full shim⇄daemon⇄webapp stack can run end-to-end
 * with no API key or network.
 *
 * Behavior per user turn:
 * - text starting with "!tool <command>" → one Bash tool round guarded by
 *   a `canUseTool` permission request, then a closing text block.
 * - "!md" → a streamed markdown showcase reply (webapp render demo).
 * - "!rotate" → a VENDOR SESSION UUID ROTATION mid-turn, then an ordinary
 *   text turn under the NEW identity (see runRotateTurn).
 * - anything else → a streamed text block echoing the input.
 * Every turn ends with a `result` message.
 */
import { AsyncQueue } from "./input-queue.js";
import { bindLog } from "./uds/log.js";
import { ModelInfo, PermissionMode, SlashCommand } from "./protocol.js";
import {
  CanUseToolLike,
  InterruptReceipt,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "./session.js";

const LOGGER = bindLog({ component: "claude-shim-fake-query", operation: "shim.fake-query.lifecycle" });

/**
 * The offline stand-in for `query.supportedModels()`. Two entries, not
 * one: a single-entry menu cannot express a SWITCH, and switching is the
 * whole thing the fake exists to let the webapp exercise.
 */
export const FAKE_MODELS: ModelInfo[] = [
  { value: "fake-model", displayName: "Fake Sonnet", description: "the offline default" },
  { value: "fake-model-fast", displayName: "Fake Haiku", description: "the offline fast one" },
];

/**
 * The offline stand-in for `query.supportedCommands()`. One entry takes an
 * argument and one does not, because the completion UI renders those two
 * cases differently and an all-or-nothing fake could not exercise both.
 */
export const FAKE_COMMANDS: SlashCommand[] = [
  {
    name: "fake-skill",
    description: "the offline skill",
    argumentHint: "<target>",
  },
  { name: "fake-bare", description: "the offline skill that takes no arguments", argumentHint: "" },
];

export interface FakeQueryOpts {
  sessionId: string;
  newUuid: () => string;
  /**
   * Mimic SDK resume: the init message reports this uuid as the
   * session_id (continuation of the resumed CLI session). Mirrors the
   * empirically verified real behavior — context restores but NO
   * history messages are re-emitted through the stream.
   *
   * Without resume, fake init reports the SHIM session id as its
   * session uuid (there is no CLI to mint one), so in fake mode the
   * daemon's captured claude_session_id equals the ephemeral daemon
   * id. That is a fake-mode-only artifact: real init always carries
   * the durable CLI uuid the spec describes.
   */
  resume?: string;
}

/** Canned reply for the "!md" turn — exercises every markdown construct. */
export const MARKDOWN_SHOWCASE = [
  "# Markdown showcase",
  "",
  "Rendered by the webapp's **markdown engine** — *streamed* over the wire like any other turn.",
  "",
  "## What works",
  "",
  "- **Bold**, *italic*, and `inline code`",
  "- [Links](https://example.com) with safe schemes only",
  "- Ordered lists too:",
  "",
  "1. first",
  "2. second",
  "",
  "> Blockquotes for the philosophical bits.",
  "",
  "```go",
  'func main() { fmt.Println("fenced code, escaped & highlighted-ish") }',
  "```",
  "",
  "---",
  "",
  "That is the whole demo.",
].join("\n");

export function createFakeQuery(
  prompt: AsyncIterable<SdkUserMessageLike>,
  canUseTool: CanUseToolLike,
  opts: FakeQueryOpts,
): QueryLike {
  LOGGER.log({ agent_repl_session_id: opts.sessionId, resumed: opts.resume !== undefined }, "creating offline fake SDK query");
  const out = new AsyncQueue<SdkMessageLike>();
  let interrupted = false;
  let permissionMode: PermissionMode = "default";
  // Mutable, and reported by init AND by every assistant message, so a
  // fake session exercises the real thing the topbar depends on: the
  // model the agent ANSWERS with is what moves, and the mirror follows it.
  let model = "fake-model";
  let turn = 0;
  // The session uuid every message currently reports.
  //
  // MUTABLE because the real vendor mutates it: a `/clear` retires the
  // transcript identity mid-stream and mints a new one, and every message
  // after that point carries the new uuid. `!rotate` is how an offline run
  // exercises that (see runRotateTurn) — it simulates the vendor's own
  // behavior, not a precondition of the code under test.
  //
  // On resume it starts at the RESUMED uuid, which is what the real CLI
  // reports on every message of a continued session, not only on its init.
  let sessionUuid = opts.resume ?? opts.sessionId;

  // Defaults first so a message may carry its own session_id; everything else
  // inherits the session uuid currently in force.
  const emit = (msg: Omit<SdkMessageLike, "uuid" | "session_id"> & { session_id?: string }): void => {
    if (out.isEnded) return;
    out.push({ uuid: opts.newUuid(), session_id: sessionUuid, ...msg } as SdkMessageLike);
  };

  const emitStream = (event: unknown): void => {
    emit({ type: "stream_event", event, parent_tool_use_id: null });
  };

  const usage = { input_tokens: 7, output_tokens: 11 };

  const emitTextBlock = (_messageId: string, index: number, text: string): void => {
    emitStream({ type: "content_block_start", index, content_block: { type: "text", text: "" } });
    const mid = Math.ceil(text.length / 2);
    for (const chunk of [text.slice(0, mid), text.slice(mid)]) {
      if (chunk !== "") {
        emitStream({ type: "content_block_delta", index, delta: { type: "text_delta", text: chunk } });
      }
    }
    emitStream({ type: "content_block_stop", index });
  };

  const emitResult = (subtype: string, resultText?: string): void => {
    emit({
      type: "result",
      subtype,
      duration_ms: 5,
      duration_api_ms: 3,
      num_turns: turn,
      total_cost_usd: 0.0001,
      usage,
      is_error: subtype !== "success",
      ...(subtype === "success" ? { result: resultText ?? "" } : {}),
      permission_denials: [],
    });
  };

  const runTextTurn = (messageId: string, text: string): void => {
    const reply =
      text.trim() === "!md"
        ? MARKDOWN_SHOWCASE
        : `echo: ${text} [mode=${permissionMode}]`;
    LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, prompt_length: text.length, permission_mode: permissionMode, branch: text.trim() === "!md" ? "markdown-showcase" : "echo" }, "running fake text turn");
    emitStream({
      type: "message_start",
      message: { id: messageId, role: "assistant", model, usage },
    });
    emitTextBlock(messageId, 0, reply);
    emitStream({ type: "message_delta", delta: { stop_reason: "end_turn" }, usage });
    emitStream({ type: "message_stop" });
    emit({
      type: "assistant",
      parent_tool_use_id: null,
      message: {
        id: messageId,
        role: "assistant",
        model,
        stop_reason: "end_turn",
        content: [{ type: "text", text: reply }],
        usage,
      },
    });
    emitResult("success", reply);
  };

  const runToolTurn = async (messageId: string, command: string): Promise<void> => {
    const toolUseId = `toolu_fake_${turn}`;
    emitStream({
      type: "message_start",
      message: { id: messageId, role: "assistant", model, usage },
    });
    emitStream({
      type: "content_block_start",
      index: 0,
      content_block: { type: "tool_use", id: toolUseId, name: "Bash", input: {} },
    });
    const inputJson = JSON.stringify({ command });
    emitStream({
      type: "content_block_delta",
      index: 0,
      delta: { type: "input_json_delta", partial_json: inputJson },
    });
    emitStream({ type: "content_block_stop", index: 0 });
    emitStream({ type: "message_stop" });

    LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, tool_name: "Bash", command_length: command.length }, "running fake tool turn");
    const decision = await canUseTool(
      "Bash",
      { command },
      { signal: new AbortController().signal, toolUseID: toolUseId, suggestions: [] },
    );
    if (interrupted) {
      LOGGER.log({ level: "warn", agent_repl_session_id: opts.sessionId, message_id: messageId }, "fake tool turn interrupted before permission completed");
      emitResult("error_during_execution");
      return;
    }
    const denied = decision.behavior === "deny";
    LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, tool_name: "Bash", decision: denied ? "deny" : "allow" }, "fake tool permission resolved");
    emit({
      type: "user",
      parent_tool_use_id: null,
      message: {
        role: "user",
        content: [
          {
            type: "tool_result",
            tool_use_id: toolUseId,
            content: denied
              ? `denied: ${(decision as { message: string }).message}`
              : `fake output of \`${command}\``,
            is_error: denied,
          },
        ],
      },
    });
    const closing = denied ? "The command was denied." : "Ran the command.";
    emitTextBlock(messageId, 1, closing);
    emit({
      type: "assistant",
      parent_tool_use_id: null,
      message: {
        id: messageId,
        role: "assistant",
        model,
        stop_reason: "end_turn",
        content: [{ type: "text", text: closing }],
        usage,
      },
    });
    emitResult("success", closing);
  };

  /**
   * A backgrounded tool turn: the spawn's progress heartbeat, the
   * announcing result (observed real shape), and the harness
   * task-notification that completes it — the whole detached-work frame
   * family, so e2e runs exercise tool_progress, task-notification, and
   * tailer plumbing instead of never seeing those frames at all.
   */
  const runBackgroundTurn = (messageId: string, command: string): void => {
    const toolUseId = `toolu_fake_${turn}`;
    const taskId = `fakebg${turn}`;
    emitStream({
      type: "message_start",
      message: { id: messageId, role: "assistant", model, usage },
    });
    emitStream({
      type: "content_block_start",
      index: 0,
      content_block: { type: "tool_use", id: toolUseId, name: "Bash", input: {} },
    });
    emitStream({
      type: "content_block_delta",
      index: 0,
      delta: {
        type: "input_json_delta",
        partial_json: JSON.stringify({ command, run_in_background: true }),
      },
    });
    emitStream({ type: "content_block_stop", index: 0 });
    emitStream({ type: "message_stop" });
    emit({
      type: "tool_progress",
      tool_use_id: toolUseId,
      tool_name: "Bash",
      parent_tool_use_id: null,
      elapsed_time_seconds: 1,
    });
    emit({
      type: "user",
      parent_tool_use_id: null,
      message: {
        role: "user",
        content: [
          {
            type: "tool_result",
            tool_use_id: toolUseId,
            content: `Command running in background with ID: ${taskId}. Output is being written to: /tmp/claude-0/fake/${taskId}/tasks/${taskId}.output.`,
            is_error: false,
          },
        ],
      },
    });
    // isReplay mirrors the real SDK: the CLI enqueues the injected
    // completion message onto the stream flagged as a replay, so the
    // fake must too — an unflagged notification here would green-light
    // a shim that drops the flagged shape production actually sends.
    emit({
      type: "user",
      isReplay: true,
      parent_tool_use_id: null,
      message: {
        role: "user",
        content: [
          {
            type: "text",
            text: `<task-notification>\n<task-id>${taskId}</task-id>\n<tool-use-id>${toolUseId}</tool-use-id>\n<status>completed</status>\n<summary>Background command completed</summary>\n</task-notification>`,
          },
        ],
      },
    });
    const closing = "Backgrounded the command.";
    emitTextBlock(messageId, 1, closing);
    emit({
      type: "assistant",
      parent_tool_use_id: null,
      message: {
        id: messageId,
        role: "assistant",
        model,
        stop_reason: "end_turn",
        content: [{ type: "text", text: closing }],
        usage,
      },
    });
    emitResult("success", closing);
  };

  /**
   * A turn that ROTATES the session uuid partway through, as a `/clear` does.
   *
   * The vendor mints a new transcript identity and announces it with a fresh
   * `system:init`; everything after that point — this turn's own `result`
   * included — belongs to the new identity. That split is the whole shape the
   * rotation handling exists for, so the fake reproduces it exactly: the turn
   * STARTED under one uuid and ENDS under another.
   */
  const runRotateTurn = (messageId: string, text: string): void => {
    sessionUuid = opts.newUuid();
    emit({
      type: "system",
      subtype: "init",
      cwd: process.cwd(),
      model,
      permissionMode,
      tools: ["Bash"],
    });
    runTextTurn(messageId, text);
  };

  const main = async (): Promise<void> => {
    emit({
      type: "system",
      subtype: "init",
      cwd: process.cwd(),
      model,
      permissionMode,
      tools: ["Bash"],
      // Resume continuation: init reports the RESUMED session uuid, as
      // the real SDK does. No history is re-emitted (verified real
      // behavior) — context restoration is implicit.
      ...(opts.resume !== undefined ? { session_id: opts.resume } : {}),
    });
    for await (const userMsg of prompt) {
      turn++;
      interrupted = false;
      const content = userMsg.message.content;
      const text =
        typeof content === "string"
          ? content
          : content
              .map((b) => (b.type === "text" ? b.text : ""))
              .join("");
      const messageId = `msg_fake_${turn}`;
      if (text.startsWith("!tool ")) {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "tool" }, "selected fake turn branch");
        await runToolTurn(messageId, text.slice("!tool ".length));
      } else if (text.trim() === "!rotate") {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "rotate" }, "selected fake turn branch");
        runRotateTurn(messageId, text);
      } else if (text.startsWith("!bg ")) {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "background" }, "selected fake turn branch");
        runBackgroundTurn(messageId, text.slice("!bg ".length));
      } else {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "text" }, "selected fake turn branch");
        runTextTurn(messageId, text);
      }
    }
    out.end();
    LOGGER.log({ agent_repl_session_id: opts.sessionId, turns: turn }, "fake SDK input ended");
  };

  void main().catch((err: unknown) => {
    // The fake is the producer boundary, so it owns the one causal error
    // record and fails the iterable. Ending cleanly here made callers see
    // ordinary SDK EOF and silently erased the actual producer failure.
    LOGGER.log({ level: "error", agent_repl_session_id: opts.sessionId, cause: err }, `fake SDK producer failed: ${err instanceof Error ? err.message : String(err)}`);
    out.fail(err);
  });

  const iterator = out[Symbol.asyncIterator]();
  return {
    [Symbol.asyncIterator]: () => iterator,
    // Resolves a REPRESENTATIVE receipt, not `undefined`. The shim depends on
    // SDK 0.3.220, whose interrupt() always answers with one — probing a real
    // session returns exactly `{"still_queued":[]}` — so returning undefined
    // would model a CLI we no longer ship against and leave every offline run
    // exercising a shape the real SDK never produces.
    //
    // Empty is the honest value: the offline query has no CLI queue behind it,
    // so nothing can survive an interrupt here.
    interrupt: async (): Promise<InterruptReceipt | undefined> => {
      interrupted = true;
      LOGGER.log({ agent_repl_session_id: opts.sessionId, turns: turn }, "fake SDK interrupt accepted");
      return { still_queued: [] };
    },
    setPermissionMode: async (mode: PermissionMode): Promise<void> => {
      LOGGER.log({ agent_repl_session_id: opts.sessionId, previous_permission_mode: permissionMode, permission_mode: mode }, "fake SDK permission mode changed");
      permissionMode = mode;
    },
    setModel: async (next: string): Promise<void> => {
      LOGGER.log({ agent_repl_session_id: opts.sessionId, previous_model: model, model: next }, "fake SDK model changed");
      model = next;
    },
    supportedModels: async (): Promise<ModelInfo[]> => FAKE_MODELS,
    supportedCommands: async (): Promise<SlashCommand[]> => FAKE_COMMANDS,
  };
}
