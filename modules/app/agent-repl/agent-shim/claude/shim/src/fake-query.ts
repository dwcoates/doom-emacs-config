/**
 * A scripted, offline stand-in for the Agent SDK's `query()`. Used by
 * `--fake` mode so the full shim⇄daemon⇄webapp stack can run end-to-end
 * with no API key or network.
 *
 * Behavior per user turn:
 * - text starting with "!tool <command>" → one Bash tool round guarded by
 *   a `canUseTool` permission request, then a closing text block.
 * - "!md" → a streamed markdown showcase reply (webapp render demo).
 * - anything else → a streamed text block echoing the input.
 * Every turn ends with a `result` message.
 */
import { AsyncQueue } from "./input-queue.js";
import { ModelInfo, PermissionMode, SlashCommand } from "./protocol.js";
import {
  CanUseToolLike,
  InterruptReceipt,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "./session.js";

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
  const out = new AsyncQueue<SdkMessageLike>();
  let interrupted = false;
  let permissionMode: PermissionMode = "default";
  // Mutable, and reported by init AND by every assistant message, so a
  // fake session exercises the real thing the topbar depends on: the
  // model the agent ANSWERS with is what moves, and the mirror follows it.
  let model = "fake-model";
  let turn = 0;

  // Defaults first so a message may carry its own session_id (the
  // resume-continuation init does); everything else inherits the
  // shim-assigned id.
  const emit = (msg: Omit<SdkMessageLike, "uuid" | "session_id"> & { session_id?: string }): void => {
    if (out.isEnded) return;
    out.push({ uuid: opts.newUuid(), session_id: opts.sessionId, ...msg } as SdkMessageLike);
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

    const decision = await canUseTool(
      "Bash",
      { command },
      { signal: new AbortController().signal, toolUseID: toolUseId, suggestions: [] },
    );
    if (interrupted) {
      emitResult("error_during_execution");
      return;
    }
    const denied = decision.behavior === "deny";
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
        await runToolTurn(messageId, text.slice("!tool ".length));
      } else if (text.startsWith("!bg ")) {
        runBackgroundTurn(messageId, text.slice("!bg ".length));
      } else {
        runTextTurn(messageId, text);
      }
    }
    out.end();
  };

  void main().catch((err: unknown) => {
    // Surface fake-engine bugs the same way a real SDK throw would land.
    out.end();
    throw err;
  });

  const iterator = out[Symbol.asyncIterator]();
  return {
    [Symbol.asyncIterator]: () => iterator,
    // The offline query has no CLI behind it to queue anything, so it models
    // a pre-`interrupt_receipt_v1` CLI and resolves without a receipt.
    interrupt: async (): Promise<InterruptReceipt | undefined> => {
      interrupted = true;
      return undefined;
    },
    setPermissionMode: async (mode: PermissionMode): Promise<void> => {
      permissionMode = mode;
    },
    setModel: async (next: string): Promise<void> => {
      model = next;
    },
    supportedModels: async (): Promise<ModelInfo[]> => FAKE_MODELS,
    supportedCommands: async (): Promise<SlashCommand[]> => FAKE_COMMANDS,
  };
}
