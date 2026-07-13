/**
 * Layer 1 wire protocol — Go daemon ⇄ TS shim (stdio NDJSON).
 *
 * Source of truth: ../../shared/protocol.md (§1). Field shapes here must
 * stay in lockstep with that document. Every frame carries a `type`
 * discriminator; unknown `type` values are ignored by receivers.
 */

// ---------------------------------------------------------------------------
// Common shared types (protocol.md "Common shared types")
// ---------------------------------------------------------------------------

export type SessionId = string;
export type RequestId = string;
export type ToolUseId = string;

export type PermissionMode =
  | "default"
  | "acceptEdits"
  | "bypassPermissions"
  | "plan"
  // CLI-era modes (claude >= 2.1); validated by the CLI itself.
  | "auto"
  | "manual"
  | "dontAsk"
  | "delegate";

/**
 * The PermissionMode enum, once. Both the command decoder and the
 * `--permission-mode` flag parser gate on THIS: the two used to carry
 * hand-written copies of the list, they drifted, and the decoder's copy
 * silently rejected every CLI-era mode the topbar offers.
 */
export const PERMISSION_MODES: readonly PermissionMode[] = [
  "default",
  "acceptEdits",
  "bypassPermissions",
  "plan",
  "auto",
  "manual",
  "dontAsk",
  "delegate",
];

export function isPermissionMode(v: unknown): v is PermissionMode {
  return typeof v === "string" && (PERMISSION_MODES as readonly string[]).includes(v);
}

export interface Usage {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens?: number;
  cache_read_input_tokens?: number;
}

/** One selectable model, from the SDK's `query.supportedModels()`. */
export interface ModelInfo {
  value: string;
  displayName: string;
  description: string;
}

/**
 * One invocable slash command, from the SDK's `query.supportedCommands()`.
 *
 * The SDK resolves this list itself — built-ins plus user, project, and
 * plugin skills — under the session's own `cwd`, `CLAUDE_CONFIG_DIR`, and
 * `settingSources`. So the set offered here is exactly the set the session
 * can actually invoke, and enumerating the skill directories from disk
 * would only re-derive it less faithfully.
 */
export interface SlashCommand {
  /** Command name WITHOUT the leading slash (e.g. `debug-logs`). */
  name: string;
  description: string;
  /** Hint for the command's arguments; empty when it takes none. */
  argumentHint: string;
}

export interface ContentBlockText {
  type: "text";
  text: string;
}
export interface ContentBlockThinking {
  type: "thinking";
  thinking: string;
  signature?: string;
}
export interface ContentBlockToolUse {
  type: "tool_use";
  id: ToolUseId;
  name: string;
  input: unknown;
}
export interface ContentBlockToolResult {
  type: "tool_result";
  tool_use_id: ToolUseId;
  content: string | Array<{ type: "text"; text: string }>;
  is_error?: boolean;
}
export type ContentBlock =
  | ContentBlockText
  | ContentBlockThinking
  | ContentBlockToolUse
  | ContentBlockToolResult;

// ---------------------------------------------------------------------------
// Commands (Go → shim), protocol.md §1.1
// ---------------------------------------------------------------------------

export interface UserMessageCmd {
  type: "user-message";
  request_id: RequestId;
  content: string | ContentBlock[];
  parent_tool_use_id?: ToolUseId;
}

export interface PermissionDecisionCmd {
  type: "permission-decision";
  request_id: RequestId;
  decision:
    | {
        behavior: "allow";
        updated_input?: unknown;
        updated_permissions?: unknown[];
      }
    | { behavior: "deny"; message: string; interrupt?: boolean };
}

export interface InterruptCmd {
  type: "interrupt";
  request_id: RequestId;
}

export interface SetPermissionModeCmd {
  type: "set-permission-mode";
  request_id: RequestId;
  mode: PermissionMode;
}

/**
 * Switch the model mid-flight (§1.1). `model` is always a concrete id:
 * the SDK's `setModel(undefined)` "back to the default" form is not
 * exposed, because the id it lands on is unknowable until the next
 * assistant message.
 */
export interface SetModelCmd {
  type: "set-model";
  request_id: RequestId;
  model: string;
}

export interface ShutdownCmd {
  type: "shutdown";
  request_id: RequestId;
  reason?: string;
}

/**
 * Re-resolve the slash-command list (§1.1).
 *
 * The SDK memoizes `supportedCommands()` against the init handshake, so a
 * skill added mid-session is invisible to the live query. Re-resolving
 * therefore means standing up a throwaway query purely to re-run that
 * handshake — which is what this command asks the shim to do.
 */
export interface RefreshCommandsCmd {
  type: "refresh-commands";
  request_id: RequestId;
}

export type ShimCommand =
  | UserMessageCmd
  | PermissionDecisionCmd
  | InterruptCmd
  | SetPermissionModeCmd
  | SetModelCmd
  | RefreshCommandsCmd
  | ShutdownCmd;

const COMMAND_TYPES: ReadonlySet<string> = new Set([
  "user-message",
  "permission-decision",
  "interrupt",
  "set-permission-mode",
  "set-model",
  "refresh-commands",
  "shutdown",
]);

// ---------------------------------------------------------------------------
// Events (shim → Go), protocol.md §1.2
// ---------------------------------------------------------------------------

export interface ReadyEvt {
  type: "ready";
  session_id: SessionId;
  shim_version: string;
  sdk_version: string;
  permission_mode: PermissionMode;
}

export interface AckEvt {
  type: "ack";
  session_id: SessionId;
  request_id: RequestId;
}

/**
 * The models this session may switch to (§1.2). Emitted once,
 * unsolicited, after the SDK's init handshake resolves: the list belongs
 * to the account and CLI, not to any command.
 */
export interface ModelsEvt {
  type: "models";
  session_id: SessionId;
  models: ModelInfo[];
}

/**
 * The slash commands this session may invoke (§1.2). Emitted once,
 * unsolicited, after the SDK's init handshake resolves, and again after
 * every `refresh-commands`: like the model menu, the list belongs to the
 * session rather than to any one command.
 */
export interface CommandsEvt {
  type: "commands";
  session_id: SessionId;
  commands: SlashCommand[];
}

/** Inlined subset of the Anthropic Messages streaming event union. */
export type RawMessageStreamEvent =
  | {
      type: "message_start";
      message: { id: string; role: "assistant"; model: string; usage: Usage };
    }
  | { type: "content_block_start"; index: number; content_block: ContentBlock }
  | {
      type: "content_block_delta";
      index: number;
      delta:
        | { type: "text_delta"; text: string }
        | { type: "thinking_delta"; thinking: string }
        | { type: "input_json_delta"; partial_json: string }
        | { type: "signature_delta"; signature: string };
    }
  | { type: "content_block_stop"; index: number }
  | {
      type: "message_delta";
      delta: { stop_reason?: string };
      usage?: Partial<Usage>;
    }
  | { type: "message_stop" }
  | { type: "ping" };

export interface StreamEventEvt {
  type: "stream-event";
  session_id: SessionId;
  uuid: string;
  parent_tool_use_id?: ToolUseId;
  event: RawMessageStreamEvent;
}

export interface AssistantMessageEvt {
  type: "assistant-message";
  session_id: SessionId;
  uuid: string;
  parent_tool_use_id?: ToolUseId;
  message: {
    id: string;
    role: "assistant";
    model: string;
    stop_reason: string | null;
    content: ContentBlock[];
    usage: Usage;
  };
}

export type ResultSubtype =
  | "success"
  | "error_max_turns"
  | "error_during_execution"
  | "aborted";

export interface ResultEvt {
  type: "result";
  session_id: SessionId;
  uuid: string;
  subtype: ResultSubtype;
  duration_ms: number;
  duration_api_ms: number;
  num_turns: number;
  total_cost_usd: number;
  usage: Usage;
  result?: string;
  is_error: boolean;
  permission_denials?: Array<{
    tool_use_id: ToolUseId;
    tool_name: string;
    message?: string;
  }>;
}

export interface PermissionRequestEvt {
  type: "permission-request";
  session_id: SessionId;
  request_id: RequestId;
  tool_use_id: ToolUseId;
  tool_name: string;
  input: unknown;
  suggestions?: unknown[];
}

/**
 * Tool results surface from the SDK as user-role messages carrying
 * `tool_result` content blocks; Layer 2's `tool-use-result` frame needs
 * them, so the shim decomposes each such block into one `tool-result`
 * event. Documented in protocol.md §1.2 as a forward-compatible
 * extension of the original event set.
 */
export interface ToolResultEvt {
  type: "tool-result";
  session_id: SessionId;
  uuid: string;
  parent_tool_use_id?: ToolUseId;
  tool_use_id: ToolUseId;
  is_error: boolean;
  content: string | Array<{ type: "text"; text: string }>;
}

export type SystemSubtype =
  | "init"
  | "compact_boundary"
  | "tool_use_progress"
  | "slash_command";

export interface SystemEvt {
  type: "system";
  session_id: SessionId;
  uuid: string;
  subtype: SystemSubtype;
  data: unknown;
}

export type ErrorCode =
  | "shim_internal"
  | "sdk_throw"
  | "bad_command"
  | "transport"
  | "shutdown_in_progress";

export interface ErrorEvt {
  type: "error";
  session_id: SessionId;
  request_id?: RequestId;
  code: ErrorCode;
  message: string;
  stack?: string;
}

export interface ClosedEvt {
  type: "closed";
  session_id: SessionId;
  request_id?: RequestId;
  exit_code: number;
  reason: "shutdown" | "sdk_end" | "fatal_error";
}

export type ShimEvent =
  | ReadyEvt
  | AckEvt
  | ModelsEvt
  | CommandsEvt
  | StreamEventEvt
  | AssistantMessageEvt
  | ResultEvt
  | PermissionRequestEvt
  | ToolResultEvt
  | SystemEvt
  | ErrorEvt
  | ClosedEvt;

// ---------------------------------------------------------------------------
// Codec
// ---------------------------------------------------------------------------

export class ProtocolError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ProtocolError";
  }
}

/**
 * Decode one NDJSON command line from the Go daemon.
 *
 * Returns `null` for frames whose `type` is unknown (forward
 * compatibility: unknown types are ignored, never an error). Throws
 * {@link ProtocolError} for lines that are not valid protocol frames at
 * all (bad JSON, non-object, missing discriminator/required fields).
 */
export function decodeCommandLine(line: string): ShimCommand | null {
  let parsed: unknown;
  try {
    parsed = JSON.parse(line);
  } catch (err) {
    throw new ProtocolError(`invalid JSON: ${(err as Error).message}`);
  }
  if (typeof parsed !== "object" || parsed === null || Array.isArray(parsed)) {
    throw new ProtocolError("frame is not a JSON object");
  }
  const frame = parsed as Record<string, unknown>;
  if (typeof frame.type !== "string") {
    throw new ProtocolError("frame has no string `type` discriminator");
  }
  if (!COMMAND_TYPES.has(frame.type)) {
    return null; // unknown type: ignore, forward compatibility
  }
  if (typeof frame.request_id !== "string" || frame.request_id === "") {
    throw new ProtocolError(`command ${frame.type} missing request_id`);
  }
  switch (frame.type) {
    case "user-message": {
      const content = frame.content;
      const validBlocks =
        Array.isArray(content) &&
        content.every(
          (b) => typeof b === "object" && b !== null && typeof (b as { type?: unknown }).type === "string",
        );
      if (typeof content !== "string" && !validBlocks) {
        throw new ProtocolError("user-message content must be a string or ContentBlock[]");
      }
      break;
    }
    case "permission-decision": {
      const d = frame.decision as { behavior?: unknown; message?: unknown } | undefined;
      if (
        typeof d !== "object" ||
        d === null ||
        (d.behavior !== "allow" && d.behavior !== "deny")
      ) {
        throw new ProtocolError("permission-decision decision.behavior must be allow|deny");
      }
      if (d.behavior === "deny" && typeof d.message !== "string") {
        throw new ProtocolError("permission-decision deny requires a message");
      }
      break;
    }
    case "set-permission-mode": {
      if (!isPermissionMode(frame.mode)) {
        throw new ProtocolError(`set-permission-mode invalid mode: ${String(frame.mode)}`);
      }
      break;
    }
    case "set-model": {
      // Empty is not "the default model" here, it is a caller who forgot
      // to say which model — and silently reading it as "default" would
      // switch the session to a model nobody asked for.
      if (typeof frame.model !== "string" || frame.model === "") {
        throw new ProtocolError("set-model requires a non-empty model id");
      }
      break;
    }
    // interrupt / shutdown carry nothing further to validate.
  }
  return frame as unknown as ShimCommand;
}

/** Encode one shim event as an NDJSON line (newline-terminated). */
export function encodeEvent(evt: ShimEvent): string {
  return JSON.stringify(evt) + "\n";
}
