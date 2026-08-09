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
 * - text carrying FAIL_TURN_MARKER → a turn that ENDS IN ERROR.
 * - anything else → a streamed text block echoing the input.
 * Every turn ends with a `result` message.
 */
import { randomBytes } from "node:crypto";

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
 * The prompt marker that makes a fake turn END IN ERROR.
 *
 * IT EXISTS BECAUSE A TURN FAILURE IS OTHERWISE UNPROVOKABLE OFFLINE. The
 * daemon's merge pipeline classifies a before-action failure and an
 * after-action failure in opposite directions — the first fails the run, the
 * second rides on the terminal merged status as after_action_error — and
 * neither branch is reachable in `--fake` mode without a way to make a turn go
 * badly. The alternative is stubbing out the very prompt path under test.
 *
 * It is a MARKER WITHIN the prompt rather than an exact match, so a caller can
 * say which of its turns should fail and still send readable text. The daemon's
 * e2e acceptance gate spells it identically (mergeactions_e2e_test.go).
 */
export const FAIL_TURN_MARKER = "e2e-fail-this-turn";

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
  /** Ends the fake stream when its owning UDS session shuts down. */
  abortSignal?: AbortSignal;
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
  /**
   * Receives the fake query's deliberately test-only failure seam.  The UDS
   * bring-up gate invokes it after the daemon has handshaked and before it
   * asserts readiness, so E2E can exercise a resumed query dying during
   * restoration without changing fake resume semantics.
   */
  onBringUpFailureInjector?: (fail: () => void) => void;
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
  opts.onBringUpFailureInjector?.(() => {
    out.fail(new Error("injected resumed fake query failure during bring-up"));
  });
  opts.abortSignal?.addEventListener("abort", () => out.end(), { once: true });
  let interrupted = false;
  let permissionMode: PermissionMode = "default";
  // Mutable, and reported by init AND by every assistant message, so a
  // fake session exercises the real thing the topbar depends on: the
  // model the agent ANSWERS with is what moves, and the mirror follows it.
  let model = "fake-model";
  let turn = 0;
  // THE SPAWN TAG THAT MAKES A FAKE API MESSAGE ID GLOBALLY UNIQUE.
  //
  // The id used to be `msg_fake_<turn>` off this instance's own counter, which
  // made it unique per QUERY and nothing more. That is not the scope the
  // daemon keys it in: its token-utilization store is keyed by session plus
  // api_message_id, and a session outlives its shim — a revival, a rewind, or
  // any other respawn starts a fresh fake query whose first turn re-mints
  // `msg_fake_1` under the SAME session. The store correctly refuses the
  // divergent duplicate, and the refusal costs the whole shim link.
  //
  // The entropy is per CREATED QUERY rather than per process, because that is
  // the boundary the collision actually has: two queries in one process — a
  // respawn a test drives in-process, or any future multi-query host — must
  // not share it either.
  const spawnTag = randomBytes(4).toString("hex");
  /**
   * The api message id for one turn: `msg_fake_<spawn>_<turn>`.
   *
   * The shape stays recognizable on purpose. Every reader that had to know a
   * fake id when it saw one — logs, the daemon's e2e assertions — still can,
   * by the prefix and the turn suffix, rather than by an exact string that was
   * only ever unique by accident.
   */
  const messageIdFor = (n: number): string => `msg_fake_${spawnTag}_${n}`;
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
    const eventType = typeof event === "object" && event !== null && "type" in event
      ? (event as { type?: unknown }).type
      : undefined;
    emit({
      type: "stream_event",
      event,
      parent_tool_use_id: null,
      // A fake message_start models the same SDK timing contract as a live
      // one, so the real ephemeral correlation path remains exercised.
      ...(eventType === "message_start" ? { ttft_ms: 1 } : {}),
    });
  };

  const usage = {
    input_tokens: 7,
    output_tokens: 11,
    cache_read_input_tokens: 80,
    cache_creation_input_tokens: 4,
    cache_creation: { ephemeral_5m_input_tokens: 4 },
    server_tool_use: { web_search_requests: 0 },
    service_tier: "standard",
    speed: "standard",
    inference_geo: "us",
    fallback_credit: { status: "not_used" },
    output_tokens_details: { reasoning_tokens: 3 },
    unmodeled_usage: { fake_extension_tokens: 2 },
    cache_diagnostic: { status: "hit", reason: "stable_prefix" },
  };

  /** Lossless actor-distinct payload consumed by the usage-accounting E2E. */
  const accountingUsage = (actor: "main-agent" | "subagent", offset: number) => ({
    input_tokens: 11 + offset,
    output_tokens: 12 + offset,
    cache_read_input_tokens: 13 + offset,
    cache_creation_input_tokens: 14 + offset,
    cache_creation: {
      ephemeral_5m_input_tokens: 41 + offset,
      ephemeral_1h_input_tokens: 42 + offset,
    },
    server_tool_use: {
      web_search_requests: 51 + offset,
      web_fetch_requests: 52 + offset,
    },
    service_tier: "priority",
    speed: "fast",
    inference_geo: "us-east-1",
    iterations: [{
      // The vendor's discriminator for an ordinary sampling iteration is
      // `message` (BetaMessageIterationUsage), not the schema's arm name.
      type: "message",
      model: `${actor}-reasoning-model`,
      input_tokens: 61 + offset,
      output_tokens: 62 + offset,
      cache_read_input_tokens: 63 + offset,
      cache_creation_input_tokens: 64 + offset,
      cache_creation: {
        ephemeral_5m_input_tokens: 65 + offset,
        ephemeral_1h_input_tokens: 66 + offset,
      },
    }],
    output_tokens_details: {
      reasoning_tokens: 71 + offset,
      reasoning_model: `${actor}-reasoning-model`,
    },
    cache_diagnostic: {
      reason: "model_changed",
      cache_missed_input_tokens: 72 + offset,
    },
    fallback_credit: {
      status: { type: "redeemed", actor },
      credits: 73 + offset,
    },
    unmodeled_usage: {
      future_counter: 74 + offset,
      nested: { actor },
    },
  });
  const mainAccountingUsage = accountingUsage("main-agent", 0);
  const subagentAccountingUsage = accountingUsage("subagent", 100);

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

  const emitResult = (
    subtype: string,
    resultText?: string,
    modelUsage?: Record<string, unknown>,
    resultUsage: Record<string, unknown> = usage,
  ): void => {
    emit({
      type: "result",
      subtype,
      duration_ms: 5,
      duration_api_ms: 3,
      num_turns: turn,
      total_cost_usd: 0.0001,
      usage: resultUsage,
      ...(modelUsage === undefined ? {} : { model_usage: modelUsage }),
      is_error: subtype !== "success",
      ...(subtype === "success" ? { result: resultText ?? "" } : {}),
      permission_denials: [],
    });
  };

  const runTextTurn = (messageId: string, text: string): void => {
    const usageSubagent = text.trim() === "!usage-subagent";
    const responseUsage = usageSubagent ? mainAccountingUsage : usage;
    const reply =
      text.trim() === "!md"
        ? MARKDOWN_SHOWCASE
        : `echo: ${text} [mode=${permissionMode}]`;
    LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, prompt_length: text.length, permission_mode: permissionMode, branch: text.trim() === "!md" ? "markdown-showcase" : "echo" }, "running fake text turn");
    emitStream({
      type: "message_start",
      message: { id: messageId, role: "assistant", model, usage: responseUsage },
    });
    emitTextBlock(messageId, 0, reply);
    emitStream({ type: "message_delta", delta: { stop_reason: "end_turn" }, usage: responseUsage });
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
        usage: responseUsage,
      },
    });
    if (usageSubagent) {
      emit({
        type: "assistant",
        agent_id: "fake-subagent-agent-id",
        parent_tool_use_id: "toolu_fake_subagent",
        parent_agent_id: "fake-parent-agent-id",
        subagent_type: "general-purpose",
        task_description: "deterministic usage-accounting subagent",
        message: {
          id: `${messageId}_subagent`,
          role: "assistant",
          model: "fake-subagent-model",
          stop_reason: "end_turn",
          content: [{ type: "text", text: "subagent completed" }],
          usage: subagentAccountingUsage,
        },
      });
      emitResult("success", reply, {
        [model]: {
          input_tokens: mainAccountingUsage.input_tokens,
          output_tokens: mainAccountingUsage.output_tokens,
          cache_read_input_tokens: mainAccountingUsage.cache_read_input_tokens,
          cache_creation_input_tokens: mainAccountingUsage.cache_creation_input_tokens,
          web_search_requests: mainAccountingUsage.server_tool_use.web_search_requests,
          cost_usd: 0.001,
          context_window: 200000,
          max_output_tokens: 32000,
          canonical_model: model,
          provider: "anthropic",
        },
        "fake-subagent-model": {
          input_tokens: subagentAccountingUsage.input_tokens,
          output_tokens: subagentAccountingUsage.output_tokens,
          cache_read_input_tokens: subagentAccountingUsage.cache_read_input_tokens,
          cache_creation_input_tokens: subagentAccountingUsage.cache_creation_input_tokens,
          web_search_requests: subagentAccountingUsage.server_tool_use.web_search_requests,
          cost_usd: 0.002,
          context_window: 200000,
          max_output_tokens: 16000,
          canonical_model: "fake-subagent-model",
          provider: "anthropic",
        },
      }, mainAccountingUsage);
      return;
    }
    emitResult("success", reply, {
      [model]: {
        input_tokens: usage.input_tokens,
        output_tokens: usage.output_tokens,
        cache_read_input_tokens: usage.cache_read_input_tokens,
        cache_creation_input_tokens: usage.cache_creation_input_tokens,
        web_search_requests: 0,
        cost_usd: 0.0001,
        context_window: 200000,
        max_output_tokens: 32000,
        canonical_model: model,
        provider: "anthropic",
      },
    }, usage);
  };

  /**
   * A turn that ENDS IN ERROR: no assistant content, and a non-success result
   * whose `is_error` is true.
   *
   * It emits no message_start / message_stop pair on purpose. A turn that
   * failed produced no assistant message, and fabricating an empty one would
   * put a blank bubble on every frontend that renders the conversation.
   */
  const runFailingTurn = (messageId: string): void => {
    LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId }, "running fake FAILING turn");
    emitResult("error_during_execution");
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
      const messageId = messageIdFor(turn);
      if (text.startsWith("!tool ")) {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "tool" }, "selected fake turn branch");
        await runToolTurn(messageId, text.slice("!tool ".length));
      } else if (text.trim() === "!rotate") {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "rotate" }, "selected fake turn branch");
        runRotateTurn(messageId, text);
      } else if (text.trim() === "!query-eof") {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "query-eof" }, "selected fake turn branch");
        out.end();
        return;
      } else if (text.startsWith("!bg ")) {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "background" }, "selected fake turn branch");
        runBackgroundTurn(messageId, text.slice("!bg ".length));
      } else if (text.includes(FAIL_TURN_MARKER)) {
        LOGGER.log({ agent_repl_session_id: opts.sessionId, message_id: messageId, branch: "failing" }, "selected fake turn branch");
        runFailingTurn(messageId);
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
