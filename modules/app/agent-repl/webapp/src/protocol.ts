/**
 * Shared webapp data vocabulary + the webapp→daemon command shapes.
 *
 * The daemon→webapp PUSH channel is no longer this bespoke NDJSON frame
 * vocabulary: after the agent-shim cutover (design §11, §14.2, §16) the daemon
 * pushes `agentshim.frontend.v1` protojson frames, decoded by
 * `frontend-proto.ts` and mapped onto the store/render model by
 * `state-adapter.ts`. The old `parseFrame`/`WsEnvelope`/`*Frame`/`L2Frame`
 * vocabulary was DELETED in that one change — there is no dual path.
 *
 * What survives here:
 * - the plain DATA types the conversation-item model and its renderers share
 *   (`ContentBlock`, `Usage`, `ModelUsage`, `ModelInfo`, `ResultSubtype`,
 *   `AssistantMessageError`, `RenderHint`, `StreamRef`, `AsyncSource`,
 *   `PermissionPreview`, `PermissionMode`, `QueuedItem`);
 * - the webapp→daemon command shapes (`ClientCommand`), which are a separate
 *   plane from the push channel and are still how the composer, permission
 *   controls, model/mode pickers and diagnostics talk to the daemon.
 */

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

export interface Usage {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens?: number;
  cache_read_input_tokens?: number;
}

/**
 * One model's slice of a result's `model_usage` map. Unlike `Usage` —
 * which the SDK scopes to the top-level agent loop only — this
 * aggregation counts subagent requests too, so summing the map's
 * entries is the session's whole-tree spend.
 */
export interface ModelUsage {
  input_tokens: number;
  output_tokens: number;
  cache_creation_input_tokens: number;
  cache_read_input_tokens: number;
  web_search_requests: number;
  cost_usd: number;
  context_window: number;
}

/** One selectable model. */
export interface ModelInfo {
  value: string;
  displayName: string;
  description: string;
}

export interface ContentBlockText {
  type: "text";
  text: string;
}
export interface ContentBlockToolResult {
  type: "tool_result";
  tool_use_id: string;
  content: string | Array<{ type: "text"; text: string }>;
  is_error?: boolean;
}
export type ContentBlock =
  | ContentBlockText
  | ContentBlockToolResult
  | { type: string; [key: string]: unknown };

/**
 * One parked message in the in-flight queue, mirrored in the store's
 * `queued` slice. A queued message is NOT a conversation item: it renders as
 * a subdued "queued — waiting" affordance and only becomes a real `user-turn`
 * once drained.
 */
export interface QueuedItem {
  queue_id: string;
  request_id: string;
  content: ContentBlock[];
  status: "classifying" | "waiting" | "interrupt";
  verdict?: "wait" | "interrupt";
  reason?: string;
}

export type ResultSubtype =
  | "success"
  | "error_max_turns"
  | "error_during_execution"
  | "aborted";

/**
 * The SDK's structured assistant-message error discriminator — set when a
 * message IS an API-level failure rather than model output.
 */
export type AssistantMessageError =
  | "authentication_failed"
  | "billing_error"
  | "rate_limit"
  | "invalid_request"
  | "server_error"
  | "unknown";

export type RenderHint =
  | { kind: "bash"; stdout: string; stderr: string; exit_code?: number }
  | { kind: "diff"; file_path: string; unified_diff: string }
  | { kind: "grep"; matches: Array<{ file: string; line: number; text: string }> }
  // The launched skill's full SKILL.md body, sourced by the daemon so the
  // card can show the skill contents alongside its invocation.
  | { kind: "skill"; content: string }
  // One transition of a harness task, off the SDK's structured
  // statusChange. The TaskCreate card folds these in as its stream.
  | {
      kind: "task-update";
      task_id: string;
      status_from: string;
      status_to: string;
      fields?: string[];
    };

/**
 * Where an {@link AsyncSource}'s stream lives and how to read it.
 *
 * `format` selects THIS CLIENT'S RENDERER: a stream that is a conversation
 * renders as nested bubbles, one that is a record log renders as rows, and one
 * that is bytes renders as a `<pre>`.
 */
export interface StreamRef {
  transport: "ws" | "poll";
  format: "jsonl-transcript" | "jsonl-journal" | "text";
}

/**
 * One tool call OWNS A STREAM: work that outlives the call and keeps
 * producing after it settles. The seam the expanded-bubble view is built on;
 * every consumer reads the answer the daemon derived once from the structured
 * tool result.
 */
export interface AsyncSource {
  source_id: string;
  kind: "agent" | "shell" | "workflow";
  label?: string;
  /** A value outside this enum reads as `running` — see the store. */
  status: "running" | "done" | "error" | "killed";
  stream?: StreamRef;
  /**
   * The stream's file, when the structured result named a readable one.
   * Daemon-side correlation data (the poll route's path record); carried
   * here only for wire fidelity.
   */
  output_file?: string;
}

export type PermissionPreview =
  | { kind: "bash"; command: string }
  | { kind: "diff"; file_path: string; unified_diff: string }
  | { kind: "write"; file_path: string; bytes: number; preview: string }
  | { kind: "generic"; summary: string };

// --- webapp → daemon commands ------------------------------------------------

export interface UserMessageCmd {
  type: "user-message";
  request_id: string;
  content: string | ContentBlock[];
  parent_tool_use_id?: string;
}

export interface PermissionDecisionCmd {
  type: "permission-decision";
  request_id: string;
  decision:
    | { behavior: "allow"; updated_input?: unknown; updated_permissions?: unknown[] }
    | { behavior: "deny"; message: string; interrupt?: boolean };
}

export interface InterruptCmd {
  type: "interrupt";
  request_id: string;
}

export interface SetPermissionModeCmd {
  type: "set-permission-mode";
  request_id: string;
  mode: PermissionMode;
}

/**
 * Switch the model mid-flight. `model` is always a concrete id from the
 * model menu: the SDK's "back to the default" form is not exposed, because the
 * id it resolves to is unknowable until the next assistant message.
 */
export interface SetModelCmd {
  type: "set-model";
  request_id: string;
  model: string;
}

/**
 * Mirror a webapp-side diagnostic line into the daemon's log, where it reaches
 * disk. Fire-and-forget observation: no request_id, honored on hibernated and
 * terminal sessions, never revives one. This is how a wedged webview — whose
 * console nobody can see — leaves evidence.
 */
export interface ClientLogCmd {
  type: "client-log";
  level: "info" | "warn" | "error";
  message: string;
}

/**
 * Escalate a queued item to preempt the running turn. Daemon-handled, never
 * forwarded to the shim; a stale `queue_id` is a no-op ack.
 */
export interface QueueRunNowCmd {
  type: "queue-run-now";
  request_id: string;
  queue_id: string;
}

/**
 * Remove a queued item without ever sending it. Daemon-handled; a stale
 * `queue_id` is a no-op ack, not an error.
 */
export interface QueueCancelCmd {
  type: "queue-cancel";
  request_id: string;
  queue_id: string;
}

export type ClientCommand =
  | UserMessageCmd
  | PermissionDecisionCmd
  | InterruptCmd
  | SetPermissionModeCmd
  | SetModelCmd
  | ClientLogCmd
  | QueueRunNowCmd
  | QueueCancelCmd;
