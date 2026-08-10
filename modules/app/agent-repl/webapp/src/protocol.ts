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

import type {
  QueueClassification,
  QueueEntryKeepAliveHold,
  QueueEntryRevivalHold,
  QueueEntryShutdownHold,
  SessionCommand,
} from "./frontend-proto.js";

export type {
  QueueClassification,
  QueueEntryKeepAliveHold,
  QueueEntryRevivalHold,
  QueueEntryShutdownHold,
};

/**
 * Every mode the CLI accepts at session LAUNCH.
 *
 * `delegate` is absent because the CLI rejects it outright (verified against
 * both SDK 0.1.77 and 0.3.220), and SDK 0.3.220 dropped it from its own
 * PermissionMode type. `bypassPermissions` IS a valid launch mode but cannot
 * be switched into mid-session, which is why the topbar picker offers a
 * strict subset of this type rather than all of it.
 */
export type PermissionMode =
  | "default"
  | "acceptEdits"
  | "bypassPermissions"
  | "plan"
  // CLI-era modes (claude >= 2.1); validated by the CLI itself.
  | "auto"
  | "manual"
  | "dontAsk";

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

/** Additive timing measurements for exactly the responses covered by a total. */
export interface TokenTimingTotals {
  output_tokens_with_generation_duration: number;
  output_generation_duration_ms: number;
  responses_with_generation_duration: number;
  responses_without_generation_duration: number;
  total_time_to_first_token_ms: number;
  responses_with_time_to_first_token: number;
  responses_without_time_to_first_token: number;
}

/** Cumulative accounting for a defined set of API responses. */
export interface TokenUsageTotals extends Usage {
  cache_creation?: { ephemeral_5m_input_tokens: number; ephemeral_1h_input_tokens: number };
  server_tool_use?: { web_search_requests: number; web_fetch_requests: number };
  output_details?: { thinking_tokens: number };
  cache_rates?: { total_prompt_input_tokens: number; cache_hit_rate: number; cache_write_rate: number; uncached_input_rate: number };
  timing?: TokenTimingTotals;
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
 * One prompt the DAEMON is holding because a turn was already running when it
 * was submitted (E4). Mirrored in the store's `queued` slice, and sourced
 * wholesale from the pushed `QueueView`.
 *
 * A held prompt is NOT a conversation item: it renders as a subdued card, and
 * only becomes a real user turn once the daemon delivers it.
 */
export interface QueuedItem {
  id: string;
  text: string;
  queuedAtMs: number;
  classification: QueueClassification;
  /** The classifier's stated reason, or the failure detail on `error`. */
  rationale: string;
  /** The user has confirmed this entry's classification (view state only). */
  accepted: boolean;
  /**
   * Set ONLY while a scheduled shutdown's drain lease is holding this prompt.
   * Its presence is what selects the lease bubble over the classifier bubble:
   * the classifier never ran on such an entry, so `classification` above says
   * nothing about why it is waiting.
   */
  shutdownHold?: QueueEntryShutdownHold;
  /**
   * Set ONLY while an in-flight cache keep-alive turn is holding this prompt.
   * Its presence selects the keep-alive bubble over the classifier bubble, for
   * the same reason the lease hold above does: the classifier never ran, so
   * `classification` says nothing about why this entry is waiting.
   */
  keepAliveHold?: QueueEntryKeepAliveHold;
  /**
   * Set ONLY while a pending compact-first revival is holding this prompt. Its
   * presence selects the revival bubble over the classifier bubble, for the
   * same reason the two holds above do: the classifier never ran, so
   * `classification` says nothing about why this entry is waiting.
   */
  revivalHold?: QueueEntryRevivalHold;
  /**
   * The context cut running in front of this prompt, set ONLY with the
   * `uninterruptible` classification. It is the whole account of that verdict:
   * no classifier ran because a `/compact` or `/clear` is running and a cut is
   * never interrupted for a queued prompt.
   */
  uninterruptibleCommand?: SessionCommand;
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

// --- webapp diagnostic log ---------------------------------------------------
//
// The S8/S9 cutover deleted the legacy JSON `ClientCommand` command union: every
// frontend→daemon command is now an `agentshim.frontend.v1.FrontendCommand`
// protojson frame (see frontend-command.ts / command-dispatch.ts), including
// the webapp→daemon diagnostic forward, which rides the `client_log` arm (E4).
// `ClientLogCmd` below is the in-memory shape `wslog.ts` passes around; its
// wire encoding is `ClientLogBody` in frontend-command.ts.

/**
 * A webapp-side diagnostic line: ALWAYS written to the local console, and
 * mirrored into the daemon's log over the `client_log` FrontendCommand arm
 * (E4). This is the in-memory shape the ForwardingLogger passes around; the
 * wire encoding lives in frontend-command.ts (`ClientLogBody`).
 */
export interface ClientLogCmd {
  type: "client-log";
  level: "info" | "warn" | "error";
  message: string;
  /**
   * Optional structured payload (ids, counters, timings) accompanying the
   * message, encoded onto `ClientLogCmd.context` (a `google.protobuf.Struct`).
   *
   * Schemaless on purpose, matching the proto: the shape is the reporting call
   * site's business, so adding a diagnostic never becomes a proto change. Call
   * sites that HAVE structured facts pass them here instead of only string-
   * interpolating them into `message`, so the daemon's log carries the ids in a
   * form something can read back.
   */
  context?: ClientLogContext;
}

/** A ClientLogCmd's structured payload: JSON values, as a Struct carries. */
export type ClientLogContext = Record<string, unknown>;
