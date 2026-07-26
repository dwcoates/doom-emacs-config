/**
 * ShimSession — the Layer-1 protocol engine.
 *
 * Owns the command→SDK and SDK→event mappings declared in
 * ./protocol.ts. The SDK itself is injected through
 * {@link SessionDeps.createQuery} so tests can drive the session with a
 * scripted fake query.
 */
import { AsyncQueue } from "./input-queue.js";
import {
  AssistantMessageError,
  ContentBlock,
  ContentBlockToolResult,
  ModelInfo,
  ModelUsage,
  PermissionDecisionCmd,
  PermissionMode,
  ProtocolError,
  RequestId,
  ResultEvt,
  ResultSubtype,
  ShimCommand,
  ShimEvent,
  SlashCommand,
  UserMessageCmd,
  decodeCommandLine,
} from "./protocol.js";

// ---------------------------------------------------------------------------
// SDK boundary types (structural, so tests can inject fakes)
// ---------------------------------------------------------------------------

/**
 * The SDK's `interrupt()` receipt (SDK >= 0.3.205, gated on the CLI
 * advertising `interrupt_receipt_v1` in system:init `capabilities`).
 *
 * `still_queued` names async user messages that SURVIVE the interrupt and
 * will still run. Our daemon holds its own prompt queue rather than
 * enqueuing into the CLI, so this is expected to be empty — a non-empty
 * list means work outlived an interrupt the daemon believes it cancelled,
 * which is exactly the kind of thing that must be loud, never swallowed.
 * Older CLIs resolve `undefined`.
 */
export interface InterruptReceipt {
  still_queued: string[];
  /** Present only when the request set `cancel_queued`. */
  cancelled?: string[];
}

/** Structural subset of the SDK `Query` interface the shim uses. */
export interface QueryLike extends AsyncIterable<SdkMessageLike> {
  interrupt(): Promise<InterruptReceipt | undefined>;
  setPermissionMode(mode: PermissionMode): Promise<void>;
  setModel(model: string): Promise<void>;
  supportedModels(): Promise<ModelInfo[]>;
  supportedCommands(): Promise<SlashCommand[]>;
}

/** Structural subset of the SDK message union the shim consumes. */
export type SdkMessageLike = {
  type: string;
  uuid?: string;
  session_id?: string;
  parent_tool_use_id?: string | null;
  [key: string]: unknown;
};

/** Structural form of the SDK's streaming-input user message. */
export interface SdkUserMessageLike {
  type: "user";
  message: { role: "user"; content: string | ContentBlock[] };
  parent_tool_use_id: string | null;
  session_id: string;
}

/** Structural form of the SDK `PermissionResult`. */
export type PermissionResultLike =
  | {
      behavior: "allow";
      updatedInput: Record<string, unknown>;
      updatedPermissions?: unknown[];
    }
  | { behavior: "deny"; message: string; interrupt?: boolean };

export type CanUseToolLike = (
  toolName: string,
  input: Record<string, unknown>,
  options: {
    signal: AbortSignal;
    suggestions?: unknown[];
    toolUseID?: string;
  },
) => Promise<PermissionResultLike>;

export interface SessionDeps {
  sessionId: string;
  shimVersion: string;
  sdkVersion: string;
  initialPermissionMode: PermissionMode;
  /** Construct the SDK query over the streaming input iterable. */
  createQuery: (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ) => QueryLike;
  /**
   * Re-resolve the slash-command list from a throwaway SDK handshake.
   *
   * Separate from {@link SessionDeps.createQuery} because the live query
   * cannot answer: the SDK memoizes `supportedCommands()` against the init
   * handshake it already performed, so only a NEW handshake sees a skill
   * added since. The implementation must reuse the live session's options
   * verbatim, or the list it resolves will not be the list the session can
   * actually invoke.
   */
  probeCommands: () => Promise<SlashCommand[]>;
  /**
   * Re-resolve the `/status` snapshot from a throwaway SDK handshake.
   *
   * Separate from {@link SessionDeps.createQuery} for the same reason
   * {@link SessionDeps.probeCommands} is: the fields a `/status` panel
   * reports are memoized against the live query's init handshake, so only a
   * NEW handshake sees a value changed since. Resolves to the SDK's
   * `system:init` message, whose shape is the SDK's own.
   */
  probeStatus: () => Promise<unknown>;
  /** Emit one Layer-1 event (the transport writes the NDJSON line). */
  emit: (evt: ShimEvent) => void;
  /** Terminate the shim process with the given exit code. */
  exit: (code: number) => void;
  /** Generate a fresh opaque request id for permission requests. */
  newRequestId: () => string;
}

interface PendingPermission {
  resolve: (result: PermissionResultLike) => void;
  input: Record<string, unknown>;
}

export class ShimSession {
  private readonly deps: SessionDeps;
  private readonly input = new AsyncQueue<SdkUserMessageLike>();
  private readonly pendingPermissions = new Map<RequestId, PendingPermission>();
  private query: QueryLike | null = null;
  private interruptRequested = false;
  /**
   * Outstanding-turn COUNTER (user-messages received minus results
   * seen), not a boolean: streaming-input mode queues turns, and a
   * boolean cleared by turn N's result would misreport turn N+1 as
   * idle, mislabeling its interrupted result.
   */
  private turnsInFlight = 0;
  private shutdownRequestId: RequestId | null = null;
  private closed = false;
  /** Uuids of task-notification messages already forwarded, so a live
   *  emission followed by the SDK's duplicate-acknowledgment replay of
   *  the same message forwards exactly once per shim process. */
  private readonly seenNotificationUuids = new Set<string>();

  constructor(deps: SessionDeps) {
    this.deps = deps;
  }

  /**
   * Construct the query and emit `ready`. Returns the pump promise; the
   * caller decides whether to await it.
   */
  start(): Promise<void> {
    this.query = this.deps.createQuery(this.input, this.canUseTool);
    this.deps.emit({
      type: "ready",
      session_id: this.deps.sessionId,
      shim_version: this.deps.shimVersion,
      sdk_version: this.deps.sdkVersion,
      permission_mode: this.deps.initialPermissionMode,
    });
    // Fire-and-forget on purpose: the model menu resolves off the SDK's
    // init handshake, and blocking `ready` (or the pump) on it would
    // delay every command behind a list nothing needs synchronously.
    this.publishSupportedModels();
    this.publishSupportedCommands();
    return this.pump();
  }

  /** Handle one raw NDJSON line from the daemon. */
  handleLine(line: string): void {
    if (line.trim() === "") return;
    let cmd: ShimCommand | null;
    try {
      cmd = decodeCommandLine(line);
    } catch (err) {
      if (err instanceof ProtocolError) {
        this.emitError("bad_command", err.message);
        return;
      }
      throw err;
    }
    if (cmd === null) return; // unknown type: ignored for forward compat
    this.handleCommand(cmd);
  }

  /**
   * Initiate the shutdown path used when the daemon's pipe goes away
   * without a `shutdown` command (e.g. the daemon died).
   */
  handleStdinEnd(): void {
    if (this.closed || this.shutdownRequestId !== null) return;
    this.shutdownRequestId = "";
    this.input.end();
  }

  private handleCommand(cmd: ShimCommand): void {
    if (this.shutdownRequestId !== null) {
      this.emitError(
        "shutdown_in_progress",
        `command ${cmd.type} rejected: shutdown in progress`,
        cmd.request_id,
      );
      return;
    }
    switch (cmd.type) {
      case "user-message":
        this.handleUserMessage(cmd);
        break;
      case "permission-decision":
        this.handlePermissionDecision(cmd);
        break;
      case "interrupt":
        // Only an in-flight turn can be aborted: an idle interrupt must
        // not poison the NEXT turn's result into `subtype: "aborted"`.
        // The SDK interrupt() is still forwarded (it is a no-op when
        // idle) and pending prompts are still cancelled.
        if (this.turnsInFlight > 0) {
          this.interruptRequested = true;
        }
        this.cancelPendingPermissions();
        void this.query!.interrupt()
          .then((receipt) => {
            this.reportInterruptSurvivors(receipt, cmd.request_id);
          })
          .catch((err: unknown) => {
            this.emitError("sdk_throw", `interrupt failed: ${errMessage(err)}`, cmd.request_id);
          });
        break;
      case "set-permission-mode":
        this.ackOnSettled(cmd.type, cmd.request_id, this.query!.setPermissionMode(cmd.mode));
        break;
      case "set-model":
        this.ackOnSettled(cmd.type, cmd.request_id, this.query!.setModel(cmd.model));
        break;
      case "refresh-commands":
        this.ackOnSettled(cmd.type, cmd.request_id, this.republishCommands());
        break;
      case "refresh-status":
        this.ackOnSettled(cmd.type, cmd.request_id, this.republishStatus());
        break;
      case "shutdown":
        this.shutdownRequestId = cmd.request_id;
        this.deps.emit({
          type: "ack",
          session_id: this.deps.sessionId,
          request_id: cmd.request_id,
        });
        this.cancelPendingPermissions();
        this.input.end();
        break;
    }
  }

  /**
   * The §1.2 contract for a command whose only success signal is an
   * `ack`: acknowledge when the SDK call settles, surface an
   * `sdk_throw` carrying the request_id when it rejects.
   *
   * The request_id on the failure path is load-bearing — the daemon
   * reads its presence as "this error is command-scoped and the session
   * is still usable" (an error without one precedes a shim death).
   */
  private ackOnSettled(type: string, requestId: RequestId, call: Promise<void>): void {
    void call
      .then(() => {
        this.deps.emit({
          type: "ack",
          session_id: this.deps.sessionId,
          request_id: requestId,
        });
      })
      .catch((err: unknown) => {
        this.emitError("sdk_throw", `${type} failed: ${errMessage(err)}`, requestId);
      });
  }

  /**
   * Publish the selectable-model menu (§1.2 `models`). Fired once at
   * startup: `supportedModels()` resolves off the SDK's init handshake,
   * so awaiting it here costs nothing and no command has to ask.
   *
   * A failure is NOT fatal — the session runs fine, only the picker goes
   * unpopulated — so it is surfaced as a recoverable, command-scoped
   * error rather than silently swallowed. The synthetic request_id is
   * what makes it recoverable: the daemon treats a request_id-less error
   * as the prelude to a shim death, which this very much is not.
   */
  private publishSupportedModels(): void {
    void this.query!.supportedModels()
      .then((models) => {
        this.deps.emit({
          type: "models",
          session_id: this.deps.sessionId,
          models,
        });
      })
      .catch((err: unknown) => {
        this.emitError(
          "sdk_throw",
          `supportedModels failed: ${errMessage(err)}`,
          this.deps.newRequestId(),
        );
      });
  }

  /**
   * Publish the slash-command menu (§1.2 `commands`). Fired once at
   * startup off the live query, for the same reason the model menu is:
   * `supportedCommands()` resolves off the init handshake the SDK has
   * already done, so it costs nothing and no command has to ask.
   *
   * Failure is recoverable exactly as it is for the model menu — only the
   * completion menu goes unpopulated — so it is surfaced with a synthetic
   * request_id, which is what tells the daemon this is a command-scoped
   * error rather than the prelude to a shim death.
   */
  private publishSupportedCommands(): void {
    void this.query!.supportedCommands()
      .then((commands) => {
        this.emitCommands(commands);
      })
      .catch((err: unknown) => {
        this.emitError(
          "sdk_throw",
          `supportedCommands failed: ${errMessage(err)}`,
          this.deps.newRequestId(),
        );
      });
  }

  /**
   * Re-resolve and republish the menu for a `refresh-commands`.
   *
   * Goes through {@link SessionDeps.probeCommands} rather than the live
   * query on purpose: the live query's `supportedCommands()` is memoized
   * against its init handshake, so asking it again would faithfully return
   * the very list we already know is stale.
   */
  private async republishCommands(): Promise<void> {
    this.emitCommands(await this.deps.probeCommands());
  }

  private emitCommands(commands: SlashCommand[]): void {
    this.deps.emit({
      type: "commands",
      session_id: this.deps.sessionId,
      commands,
    });
  }

  /**
   * Re-resolve and republish the `/status` snapshot for a `refresh-status`.
   *
   * Goes through {@link SessionDeps.probeStatus} rather than the live query
   * for the same reason {@link republishCommands} does: the fields a
   * `/status` panel reports are memoized against the live query's init
   * handshake, so re-reading them means running a fresh handshake on a
   * throwaway query.
   */
  private async republishStatus(): Promise<void> {
    this.emitStatus(await this.deps.probeStatus());
  }

  private emitStatus(status: unknown): void {
    this.deps.emit({
      type: "status",
      session_id: this.deps.sessionId,
      status,
    });
  }

  private handleUserMessage(cmd: UserMessageCmd): void {
    this.turnsInFlight++;
    const content: ContentBlock[] =
      typeof cmd.content === "string"
        ? [{ type: "text", text: cmd.content }]
        : cmd.content;
    this.input.push({
      type: "user",
      message: { role: "user", content },
      parent_tool_use_id: cmd.parent_tool_use_id ?? null,
      session_id: this.deps.sessionId,
    });
  }

  private handlePermissionDecision(cmd: PermissionDecisionCmd): void {
    const pending = this.pendingPermissions.get(cmd.request_id);
    if (!pending) {
      this.emitError(
        "bad_command",
        `permission-decision for unknown request_id ${cmd.request_id}`,
        cmd.request_id,
      );
      return;
    }
    this.pendingPermissions.delete(cmd.request_id);
    if (cmd.decision.behavior === "allow") {
      pending.resolve({
        behavior: "allow",
        updatedInput:
          (cmd.decision.updated_input as Record<string, unknown> | undefined) ??
          pending.input,
        updatedPermissions: cmd.decision.updated_permissions,
      });
    } else {
      pending.resolve({
        behavior: "deny",
        message: cmd.decision.message,
        interrupt: cmd.decision.interrupt,
      });
    }
  }

  /** SDK `canUseTool` callback: block the tool on a daemon decision. */
  private canUseTool: CanUseToolLike = (toolName, input, options) => {
    const requestId = this.deps.newRequestId();
    return new Promise<PermissionResultLike>((resolve) => {
      this.pendingPermissions.set(requestId, { resolve, input });
      options.signal.addEventListener("abort", () => {
        if (this.pendingPermissions.delete(requestId)) {
          resolve({ behavior: "deny", message: "permission request aborted" });
        }
      });
      this.deps.emit({
        type: "permission-request",
        session_id: this.deps.sessionId,
        request_id: requestId,
        tool_use_id: options.toolUseID ?? "",
        tool_name: toolName,
        input,
        suggestions: options.suggestions,
      });
    });
  };

  /**
   * Surface an interrupt receipt that reports surviving work.
   *
   * An empty (or absent) receipt is the expected case and stays silent. A
   * NON-empty `still_queued` contradicts the interrupt's whole contract, so
   * it is reported as a command-scoped error rather than logged and dropped
   * — the daemon must not believe it cancelled work that is still running.
   */
  private reportInterruptSurvivors(
    receipt: InterruptReceipt | undefined,
    requestId: RequestId,
  ): void {
    const survivors = receipt?.still_queued ?? [];
    if (survivors.length === 0) return;
    this.emitError(
      "sdk_throw",
      `interrupt left ${survivors.length} queued message(s) running: ${survivors.join(",")}`,
      requestId,
    );
  }

  /** Unblock any pending SDK permission waits (interrupt/shutdown). */
  private cancelPendingPermissions(): void {
    for (const [id, pending] of this.pendingPermissions) {
      this.pendingPermissions.delete(id);
      pending.resolve({ behavior: "deny", message: "permission request cancelled" });
    }
  }

  private async pump(): Promise<void> {
    try {
      for await (const msg of this.query!) {
        this.mapSdkMessage(msg);
      }
    } catch (err) {
      this.emitError("sdk_throw", errMessage(err), undefined, errStack(err));
      this.close("fatal_error", 1);
      return;
    }
    this.close(this.shutdownRequestId !== null ? "shutdown" : "sdk_end", 0);
  }

  private close(reason: "shutdown" | "sdk_end" | "fatal_error", exitCode: number): void {
    if (this.closed) return;
    this.closed = true;
    this.cancelPendingPermissions();
    this.deps.emit({
      type: "closed",
      session_id: this.deps.sessionId,
      ...(this.shutdownRequestId ? { request_id: this.shutdownRequestId } : {}),
      exit_code: exitCode,
      reason,
    });
    this.deps.exit(exitCode);
  }

  // -------------------------------------------------------------------------
  // SDK message → Layer-1 event mapping
  // -------------------------------------------------------------------------

  private mapSdkMessage(msg: SdkMessageLike): void {
    switch (msg.type) {
      case "stream_event":
        this.deps.emit({
          type: "stream-event",
          session_id: this.deps.sessionId,
          uuid: msg.uuid ?? "",
          ...parentField(msg),
          event: msg.event as never,
        });
        break;
      case "assistant": {
        const m = msg.message as {
          id: string;
          model: string;
          stop_reason: string | null;
          content: ContentBlock[];
          usage: ResultEvt["usage"];
        };
        // The SDK flags an API-level failure (a session/usage limit, a
        // billing or auth error) with `error` ALONGSIDE `message`; forward it
        // so downstream layers can render the message as an error rather than
        // mistake its text for an ordinary answer. Absent on normal messages.
        const error =
          typeof msg.error === "string" ? (msg.error as AssistantMessageError) : undefined;
        this.deps.emit({
          type: "assistant-message",
          session_id: this.deps.sessionId,
          uuid: msg.uuid ?? "",
          ...parentField(msg),
          ...(error !== undefined ? { error } : {}),
          message: {
            id: m.id,
            role: "assistant",
            model: m.model,
            stop_reason: m.stop_reason ?? null,
            content: m.content,
            usage: m.usage,
          },
        });
        break;
      }
      case "user":
        this.mapUserMessage(msg);
        break;
      case "result":
        this.mapResultMessage(msg);
        break;
      case "system":
        this.mapSystemMessage(msg);
        break;
      case "tool_progress":
        this.deps.emit({
          type: "system",
          session_id: this.deps.sessionId,
          uuid: msg.uuid ?? "",
          subtype: "tool_use_progress",
          data: msg,
        });
        break;
      default:
        // Unknown SDK message types are dropped: Layer 1 has no frame for
        // them and receivers must not see undefined shapes.
        break;
    }
  }

  /**
   * Tool results ride on SDK user-role messages as tool_result blocks.
   * So do harness task-notifications (the completion signal of detached
   * background work), as text blocks wrapping a <task-notification>
   * payload — those forward as system/task_notification events. Every
   * OTHER text block stays host-side noise (system reminders, local
   * command echoes) and is dropped as before.
   *
   * Task-notification texts are scanned BEFORE the replay guard: the
   * CLI enqueues the harness-injected completion message onto the SDK
   * stream flagged as a replay, so gating the scan on `isReplay` starves
   * the daemon of the ONLY signal that detached work concluded — no
   * badge ever settles and async_live never drops.  Re-observing a
   * notification on a genuine history replay (session resume) is
   * harmless downstream — the daemon's task settle and the webapp's
   * notification write are both idempotent — and the per-uuid dedup
   * below keeps one shim process from emitting the same notification
   * twice.  Tool results stay replay-gated: re-emitting one would
   * report a tool_use_id the daemon has already seen.
   *
   * The message's `tool_use_result` sibling rides along on each emitted
   * tool-result event (see ToolResultEvt.structured) — it is the SDK's
   * structured twin of the flattened `content`, and dropping it is what
   * forced every downstream layer to scrape ids out of English prose.
   */
  private mapUserMessage(msg: SdkMessageLike): void {
    const message = msg.message as { content?: unknown } | undefined;
    const content = message?.content;
    const blocks: ContentBlock[] =
      typeof content === "string"
        ? [{ type: "text", text: content } as ContentBlock]
        : Array.isArray(content)
          ? (content as ContentBlock[])
          : [];
    for (const block of blocks) {
      if (block.type === "text") {
        const text = String((block as { text?: unknown }).text ?? "");
        if (text.includes("<task-notification>")) {
          const uuid = String(msg.uuid ?? "");
          if (uuid !== "" && this.seenNotificationUuids.has(uuid)) continue;
          if (uuid !== "") this.seenNotificationUuids.add(uuid);
          this.deps.emit({
            type: "system",
            session_id: this.deps.sessionId,
            uuid,
            subtype: "task_notification",
            data: { text },
          });
        }
        continue;
      }
      if (block.type !== "tool_result") continue;
      // A replay is the SDK re-emitting a message already in its history,
      // marked only by `isReplay` — the `type` is plain "user", identical
      // to a real one.  Acting on it would emit a SECOND tool-result for
      // a tool_use_id already reported.
      if (msg.isReplay === true) continue;
      const tr = block as ContentBlockToolResult;
      this.deps.emit({
        type: "tool-result",
        session_id: this.deps.sessionId,
        uuid: msg.uuid ?? "",
        ...parentField(msg),
        tool_use_id: tr.tool_use_id,
        is_error: tr.is_error ?? false,
        content: tr.content,
        ...(msg.tool_use_result !== undefined
          ? { structured: msg.tool_use_result }
          : {}),
      });
    }
  }

  private mapResultMessage(msg: SdkMessageLike): void {
    const subtype: ResultSubtype = this.interruptRequested
      ? "aborted"
      : mapResultSubtype(String(msg.subtype));
    this.interruptRequested = false;
    if (this.turnsInFlight > 0) {
      this.turnsInFlight--;
    }
    const denials = msg.permission_denials as
      | Array<{ tool_use_id: string; tool_name: string; message?: string }>
      | undefined;
    const modelUsage = normalizeModelUsage(msg.modelUsage);
    const evt: ResultEvt = {
      type: "result",
      session_id: this.deps.sessionId,
      uuid: msg.uuid ?? "",
      subtype,
      duration_ms: (msg.duration_ms as number) ?? 0,
      duration_api_ms: (msg.duration_api_ms as number) ?? 0,
      num_turns: (msg.num_turns as number) ?? 0,
      total_cost_usd: (msg.total_cost_usd as number) ?? 0,
      usage: normalizeUsage(msg.usage),
      ...(modelUsage !== undefined ? { model_usage: modelUsage } : {}),
      is_error: (msg.is_error as boolean) ?? false,
      ...(denials && denials.length > 0
        ? {
            permission_denials: denials.map((d) => ({
              tool_use_id: d.tool_use_id,
              tool_name: d.tool_name,
              ...(d.message !== undefined ? { message: d.message } : {}),
            })),
          }
        : {}),
    };
    if (subtype === "success" && typeof msg.result === "string") {
      evt.result = msg.result;
    }
    this.deps.emit(evt);
  }

  private mapSystemMessage(msg: SdkMessageLike): void {
    const subtype = String(msg.subtype);
    // `status: "compacting"` announces the START of a context compaction
    // (the compact_boundary that ends it is the only other signal — there
    // is no progress percentage), forwarded so the GUI can show a
    // compaction-in-progress indicator. Every other SDK system subtype (a
    // non-`compacting` status, hook_response, ...) has no Layer-1
    // representation and is dropped.
    const forward =
      subtype === "init" ||
      subtype === "compact_boundary" ||
      subtype === "slash_command" ||
      (subtype === "status" && msg.status === "compacting");
    if (forward) {
      this.deps.emit({
        type: "system",
        session_id: this.deps.sessionId,
        uuid: msg.uuid ?? "",
        subtype,
        data: msg,
      });
    }
  }

  private emitError(
    code: "shim_internal" | "sdk_throw" | "bad_command" | "transport" | "shutdown_in_progress",
    message: string,
    requestId?: RequestId,
    stack?: string,
  ): void {
    this.deps.emit({
      type: "error",
      session_id: this.deps.sessionId,
      ...(requestId ? { request_id: requestId } : {}),
      code,
      message,
      ...(stack ? { stack } : {}),
    });
  }
}

function parentField(msg: SdkMessageLike): { parent_tool_use_id?: string } {
  return msg.parent_tool_use_id != null
    ? { parent_tool_use_id: msg.parent_tool_use_id }
    : {};
}

/**
 * Collapse SDK result subtypes onto the closed Layer-1 enum: `success`
 * and `error_max_turns` map through, every other error flavor becomes
 * `error_during_execution` (the SDK union grows over time; Layer 1's
 * does not).
 */
function mapResultSubtype(sdkSubtype: string): ResultSubtype {
  switch (sdkSubtype) {
    case "success":
      return "success";
    case "error_max_turns":
      return "error_max_turns";
    default:
      return "error_during_execution";
  }
}

function normalizeUsage(usage: unknown): ResultEvt["usage"] {
  const u = (usage ?? {}) as Record<string, number | undefined>;
  return {
    input_tokens: u.input_tokens ?? 0,
    output_tokens: u.output_tokens ?? 0,
    ...(u.cache_creation_input_tokens !== undefined
      ? { cache_creation_input_tokens: u.cache_creation_input_tokens }
      : {}),
    ...(u.cache_read_input_tokens !== undefined
      ? { cache_read_input_tokens: u.cache_read_input_tokens }
      : {}),
  };
}

/**
 * The SDK result's `modelUsage` map (camelCase, per-model, INCLUDING
 * subagent spend), normalized to the wire's snake_case `model_usage`.
 * Returns undefined when the SDK reports no map (or an empty one): the
 * result's plain `usage` covers only the top-level loop, so an absent
 * map means "no whole-tree figure", not "zero".
 */
function normalizeModelUsage(
  modelUsage: unknown,
): Record<string, ModelUsage> | undefined {
  if (typeof modelUsage !== "object" || modelUsage === null) return undefined;
  const entries = Object.entries(modelUsage as Record<string, unknown>);
  if (entries.length === 0) return undefined;
  const out: Record<string, ModelUsage> = {};
  for (const [model, raw] of entries) {
    const u = (raw ?? {}) as Record<string, number | undefined>;
    out[model] = {
      input_tokens: u.inputTokens ?? 0,
      output_tokens: u.outputTokens ?? 0,
      cache_creation_input_tokens: u.cacheCreationInputTokens ?? 0,
      cache_read_input_tokens: u.cacheReadInputTokens ?? 0,
      web_search_requests: u.webSearchRequests ?? 0,
      cost_usd: u.costUSD ?? 0,
      context_window: u.contextWindow ?? 0,
    };
  }
  return out;
}

function errMessage(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}

function errStack(err: unknown): string | undefined {
  return err instanceof Error ? err.stack : undefined;
}
