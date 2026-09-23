/**
 * convert/fold.ts — the seam every converter plugs into, and the dispatcher.
 *
 * # What the fold IS
 *
 * The vendor's stream is a FLAT LOG of records. `conversation.v1` is a model of
 * UNITS WITH IDENTITY that are upserted whole. The fold is the mapping between
 * them, and it is the shim's central act: one SDK message in, zero or more
 * self-describing rows out.
 *
 * It ACCUMULATES NOTHING beyond constant-size joins. That is not an efficiency
 * preference, it is the statelessness the whole architecture rests on: a shim
 * that grew state per turn would be a second, divergent copy of the record the
 * store already owns, and a bounce would lose it. The joins it is allowed are
 * each ONE remembered value or one bounded, self-emptying table:
 *
 *   - the BLOCK STATE OF EACH STREAM with a response open (so
 *     `<message.id>:<block_index>` is stable across the lines the vendor
 *     splits one message into) — ONE PER STREAM, keyed by the main stream or
 *     the spawning call a subagent's messages name, because the agents'
 *     streams interleave; each dropped at its `message_stop`, at the turn's
 *     end (main) or at its agent's end (a subagent);
 *   - the CALLS IN FLIGHT, so a tool result can restate its call's own facts —
 *     each entry dropped the moment its unit settles, and the table capped;
 *   - the HOOK FIRINGS in flight, for the same reason and on the same terms;
 *   - the KINDS OF THE TASKS in flight, on the same terms again: `task_started`
 *     is the only message that says whether a detached task is an agent run or
 *     a backgrounded shell command, and its `task_notification` must not settle
 *     a shell unit as a subagent;
 *   - ONE pending COMPACTION, because the vendor states the boundary one record
 *     before its summary — released by the vendor's own summary record (the
 *     main stream's synthetic `user` record the boundary's anchor names), and
 *     at the latest by the turn's terminal or by a second boundary, each of
 *     which records the cut without a summary and logs the gap at error;
 *   - ONE pending CLEAR, because a cut's upsert key must be the one spelling the
 *     FILE plane can also reach — the session the clear rotated to — and the
 *     `conversation_reset` does not name it; the init that follows does;
 *   - the LAST TOP-LEVEL RESPONSE unit, because `AgentCompleted.answer` names it
 *     and only the fold has seen which one it was — the main stream's alone;
 *   - the LAST VENDOR API ERROR of the turn — the main stream's alone, since
 *     the terminal it feeds is the main turn's — because the result record states
 *     only the HTTP status and the vendor's own error CLASS and retry delay ride
 *     records that arrive before it (`api_retry`, and an assistant message's
 *     `error`) — cleared by the terminal that consumes it.
 *
 * # Why the output is PersistEntry and not frames
 *
 * Every frame the fold produces is going to ONE place: a store row. Which book
 * it belongs to, which row it replaces and where in the vendor's record it came
 * from are facts only the fold has seen, so it mints them here rather than
 * making a second pass re-derive them from a frame that no longer says.
 *
 * # The rules every converter obeys
 *
 *   - Units upsert BY IDENTITY, and every frame of a unit is self-describing.
 *   - `update` frames are DELTAS, never cumulative; terminals carry wholes.
 *   - Usage and effort ride the FIRST content block's unit of each API response,
 *     and no other.
 *   - The EXEMPT SET is dropped silently and never becomes `AgentUnmodeled`;
 *     `AgentUnmodeled` means a genuinely unknown tool, and a recognizable
 *     built-in arriving there is a producer defect.
 *   - Anything unconvertible becomes RESIDUE rather than being dropped or
 *     crashing.
 *   - A record MISSING A FIELD the proto requires produces NO frame at all and a
 *     logged converter defect — never a partial message.
 *   - Every converter LOGS its branch.
 */
import { bindLog } from "../log.js";
import type { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { convertAttachment, type AttachmentRecord } from "./attachments.js";
import { convertDetached, createTaskKindRegistry, type TaskKindRegistry } from "./detached.js";
import { attachmentActivityId } from "./ids.js";
import { spawningCallOf, type FoldContext } from "./fold-context.js";
import {
  convertHookResponse,
  convertHookStarted,
  createHookRegistry,
  type HookRegistry,
} from "./hooks.js";
import { convertPermissionDenied } from "./permission.js";
import { residueEntry, residueForMessage } from "./residue.js";
import {
  clearedCutEntry,
  compactionEntry,
  convertSessionMessage,
  type PendingClear,
  type PendingCompaction,
} from "./session-updates.js";
import {
  convertAssistantMessage,
  convertStreamEvent,
  convertModelRefusal,
  convertThinkingTokens,
  StreamBlocks,
} from "./stream-events.js";
import {
  classifyVendorApiFailure,
  convertResult,
  isUserStop,
  type VendorApiError,
} from "./terminals.js";
import { convertToolProgressMessage, convertUserRecord } from "./tool-results.js";
import { createCallRegistry, cutOpenCalls, type CallRegistry } from "./tool-calls.js";
import { TOOL_CONVERTERS } from "./tools/registry.js";
import { isLaunchReceipt } from "./tools/subagent.js";

const LOGGER = bindLog({ component: "shim-convert-fold", operation: "shim.convert.fold" });

/**
 * Everything ONE SDK message produced.
 *
 * `turnEnded` is present EXACTLY when the message was the turn's `result` — the
 * only source of a turn terminal. Its frame is ALSO in `entries`: the terminal
 * is both a page line (the feed's stop notice has no other source) and the
 * engine's signal that the main thread can accept a prompt again, and making the
 * engine dig it back out of the list would be a second parse of a fact the fold
 * already resolved.
 */
export interface FoldOutput {
  /** The rows this message produced, in the order the vendor stated them. */
  readonly entries: readonly PersistEntry[];
  /** Set only for the turn's `result` message. */
  readonly turnEnded?: { readonly frame: conversationv1.AgentFrame };
}

/** An output that produced nothing — the exempt set's answer, and common. */
export const EMPTY_FOLD_OUTPUT: FoldOutput = { entries: [] };

/**
 * SDK message types that carry no conversation fact at all.
 *
 * DISTINCT FROM RESIDUE: residue means "we could not model this", and these are
 * transport bookkeeping we HAVE modelled, as meaning nothing. Recording them
 * would fill the unserved table with keep-alive frames.
 */
const SILENTLY_IGNORED_TYPES: ReadonlySet<string> = new Set(["keep_alive"]);

/**
 * The fold, as the engine drives it.
 *
 * ONE method, called once per SDK message in arrival order. Synchronous on
 * purpose: a fold that could await would be able to interleave two messages and
 * break the ordering every upsert depends on.
 */
interface Fold {
  /**
   * Convert one SDK message.
   *
   * NEVER THROWS: an unrecognized record is residue, and a malformed one is
   * residue plus a logged converter defect — a fold that threw would take the
   * session down over one bad vendor line.
   */
  onSdkMessage(message: SdkMessage, context: FoldContext): FoldOutput;
}

/** Everything the fold remembers. Each field is named in this file's header. */
interface FoldState {
  readonly streams: StreamBlocks;
  readonly calls: CallRegistry;
  readonly hooks: HookRegistry;
  readonly taskKinds: TaskKindRegistry;
  pendingCompaction?: PendingCompaction;
  pendingClear?: PendingClear;
  lastAnswer?: conversationv1.AgentActivityId;
  vendorApiError?: VendorApiError;
}

/**
 * Build a fold.
 *
 * The returned object holds ONLY the joins named in this file's header. Nothing
 * else survives a message.
 */
export function createFold(): Fold {
  const state: FoldState = {
    streams: new StreamBlocks(),
    calls: createCallRegistry(),
    hooks: createHookRegistry(),
    taskKinds: createTaskKindRegistry(),
  };

  return {
    onSdkMessage(message: SdkMessage, context: FoldContext): FoldOutput {
      try {
        return dispatch(message, context, state);
      } catch (error) {
        // A converter that throws is a DEFECT, and the honest answer to a defect
        // is residue plus a loud log — never a dead session, and never a
        // half-built message on the wire.
        const detail = error instanceof Error ? error.message : String(error);
        LOGGER.error(
          { sdk_message_type: (message as { type?: string }).type, detail },
          "converter defect: the message produced no frame and lands as residue",
        );
        // THE CONTROL PLANE IS TOLD TOO. Residue keeps the record honest; the
        // fault channel keeps the shim's own diagnostics honest, which is the
        // only place a consumer can see that the converter is degraded.
        context.reportFault?.("converter_defect", detail);
        return {
          entries: [
            residueEntry(
              context,
              message,
              residueForMessage(message, `converter defect: ${detail}`),
              "residue.unparsed",
            ),
          ],
        };
      }
    },
  };
}

/** The one switch over the vendor's message vocabulary. */
function dispatch(message: SdkMessage, context: FoldContext, state: FoldState): FoldOutput {
  const type = message.type;
  if (SILENTLY_IGNORED_TYPES.has(type)) {
    LOGGER.logVerbose({ sdk_message_type: type }, "message carries no conversation fact; ignored");
    return EMPTY_FOLD_OUTPUT;
  }

  // THE STREAM CARRIES ATTACHMENTS, though the SDK's declared union does not
  // name them — which is why this is a pre-switch guard on the raw `type`
  // rather than another `case` the compiler would reject. They are transcript
  // lines too, so the sidecar converts the SAME record from the file plane;
  // residue keyed `residue:<vendor record uuid>` (landing 5) is what makes the
  // two rows ONE row rather than a duplicate. Falling through to `default`
  // instead landed them under the `unknown` arm — "we did not model this" —
  // when the ruling says a tool-availability delta or an agent listing is
  // `vendor_specific`: understood, and deliberately not carried.
  if ((type as string) === "attachment") {
    const record = message as unknown as AttachmentRecord & { uuid?: string };
    const uuid = record.uuid ?? "";
    if (uuid === "") {
      // A CHAINED VENDOR RECORD WITHOUT A UUID IS MALFORMED, not merely
      // unmodelled: it has no identity for either plane to key on, so the two
      // planes could never collapse it into one row. That is a failure, and
      // `unparsed` is the arm that says so investigably.
      LOGGER.debug(
        { sdk_message_type: type },
        "an attachment record carried no uuid; it cannot be keyed and lands unparsed",
      );
      return {
        entries: [
          residueEntry(
            context,
            message,
            residueForMessage(message, "attachment record names no uuid"),
            "residue.unparsed",
          ),
        ],
      };
    }
    return { entries: convertAttachment(record, context, attachmentActivityId(uuid)) };
  }

  switch (type) {
    case "stream_event":
      return { entries: convertStreamEvent(message, context, state.streams) };

    case "assistant": {
      const entries = [
        ...convertAssistantMessage(message, context, state.streams, state.calls, TOOL_CONVERTERS),
      ];
      rememberAnswer(message, entries, state);
      // THE TERMINAL IT FEEDS IS THE MAIN TURN'S: a subagent's failed request
      // is that agent's own failure, never the class the turn ended on.
      if (spawningCallOf(message.parent_tool_use_id) === undefined) rememberVendorApiError(message, state);
      return { entries };
    }

    case "user": {
      const entries = [
        ...settleCompaction(message, context, state),
        ...convertUserRecord(message, context, state.calls, TOOL_CONVERTERS, state.taskKinds),
      ];
      endAgentsConcludedBy(message, state.streams);
      return { entries };
    }

    case "result": {
      // CONSUMED, NOT KEPT: the class belongs to the request that just failed,
      // and leaving it behind would let the next turn's terminal claim a class
      // this turn's vendor never stated.
      const vendorApiError = state.vendorApiError ?? {};
      state.vendorApiError = undefined;
      // THE MAIN STREAM ENDS WITH ITS TURN, `message_stop` or not.
      state.streams.endTurn();
      // A CUT NEVER OUTLIVES ITS TURN. One still held here is a summary the
      // vendor never stated, and holding it into the next turn is what left a
      // compaction undrawn for minutes. It lands before the terminal, which
      // stays the turn's last word.
      const released = releaseCompaction(context, state, "the turn ended");
      const output = convertResult(message, context, state.lastAnswer, vendorApiError);
      // A STOP CUTS WHAT WAS OPEN, and the calls it cut get no `tool_result` of
      // their own — so their terminals are owed here or nowhere, and a unit left
      // on its running arm draws a live tool inside a turn that has ended. The
      // cut frames come FIRST so the terminal is still the turn's last word.
      const cut = isUserStop(message)
        ? cutOpenCalls(TOOL_CONVERTERS, context, state.calls, { vendorUuid: message.uuid })
        : [];
      const before = [...released, ...cut];
      return before.length === 0 ? output : { ...output, entries: [...before, ...output.entries] };
    }

    case "tool_progress":
      return {
        entries: convertToolProgressMessage(message, context, state.calls, TOOL_CONVERTERS),
      };

    case "system":
      return { entries: convertSystemMessage(message, context, state) };

    case "rate_limit_event":
      return { entries: convertSessionMessage(message, context) };

    case "conversation_reset":
      // THE CLEAR'S CUT IS HELD HERE and released by the init that names the
      // session it rotated to — the only identity both planes can spell.
      return {
        entries: convertSessionMessage(message, context, undefined, (pending) => {
          state.pendingClear = pending;
        }),
      };

    default:
      LOGGER.debug(
        { sdk_message_type: type },
        "no converter owns this SDK message type; it lands as residue",
      );
      return {
        entries: [
          residueEntry(context, message, residueForMessage(message), `unknown.${String(type)}`),
        ],
      };
  }
}

/** `type: "system"` fans out by subtype across five converter families. */
function convertSystemMessage(
  message: Extract<SdkMessage, { type: "system" }>,
  context: FoldContext,
  state: FoldState,
): readonly PersistEntry[] {
  switch (message.subtype) {
    case "permission_denied":
      return convertPermissionDenied(message, context);
    case "task_notification":
      // A BACKGROUNDED AGENT'S END: its spawning call's stream is over.
      if (typeof message.tool_use_id === "string") state.streams.endAgent(message.tool_use_id);
      return convertDetached(message, context, state.taskKinds);
    case "task_started":
    case "task_updated":
    case "task_progress":
    case "background_tasks_changed":
      return convertDetached(message, context, state.taskKinds);
    case "hook_started":
      return convertHookStarted(message, context, state.hooks);
    case "hook_response":
      return convertHookResponse(message, context, state.hooks);
    case "thinking_tokens":
      return convertThinkingTokens(message, context, state.streams);
    case "model_refusal_no_fallback":
      return convertModelRefusal(message, context);
    case "api_retry":
      // NO ROW — `convertSessionMessage` states why — but the record IS the
      // vendor's own account of WHICH class failed and HOW LONG it said to
      // wait, and the terminal has no other source for either.
      rememberVendorApiError(message, state);
      return convertSessionMessage(message, context);
    default: {
      // A SECOND BOUNDARY NEVER DISCARDS THE FIRST: the first is released,
      // in order, before the second is held.
      const superseded: PersistEntry[] = [];
      const entries = convertSessionMessage(message, context, (pending) => {
        superseded.push(...releaseCompaction(context, state, "a later compaction boundary arrived"));
        state.pendingCompaction = pending;
      });
      const all = superseded.length === 0 ? entries : [...superseded, ...entries];
      return message.subtype === "init" ? [...all, ...settleClear(message, context, state)] : all;
    }
  }
}

/**
 * End the stream of every subagent whose spawning call this user record
 * CONCLUDES.
 *
 * A spawn's `tool_result` is the agent's end unless it is a LAUNCH RECEIPT
 * ({@link isLaunchReceipt}): the agent moved to the background and its end is
 * the `task_notification` that names the call. A call that spawned no agent
 * holds no stream, so ending it is a no-op.
 */
function endAgentsConcludedBy(
  message: Extract<SdkMessage, { type: "user" }>,
  streams: StreamBlocks,
): void {
  if (isLaunchReceipt((message as { tool_use_result?: unknown }).tool_use_result)) return;
  const content = (message.message as { content?: unknown } | undefined)?.content;
  if (!Array.isArray(content)) return;
  for (const block of content as { type?: unknown; tool_use_id?: unknown }[]) {
    if (block.type === "tool_result" && typeof block.tool_use_id === "string") {
      streams.endAgent(block.tool_use_id);
    }
  }
}

/**
 * The clear's cut row, once the init that names the rotated-to session arrives.
 *
 * ONE HELD RESET, RELEASED BY THE VERY NEXT INIT. The vendor states the reset
 * and then, milliseconds later, a `system:init` carrying the id the session
 * actually moved to — the same id the sidecar reads off the transcript file the
 * `/clear` envelope lands in, and therefore the one upsert key both planes can
 * mint for one cut.
 *
 * AN INIT THAT NAMES NO SESSION KEEPS THE CUT HELD rather than writing a row
 * keyed on nothing: an empty key would collide with every other unidentified
 * cut, and the file plane still writes this clear from the envelope on disk.
 */
function settleClear(
  message: Extract<SdkMessage, { type: "system" }>,
  context: FoldContext,
  state: FoldState,
): readonly PersistEntry[] {
  const pending = state.pendingClear;
  if (pending === undefined) return [];
  const sessionId = (message as { session_id?: unknown }).session_id;
  if (typeof sessionId !== "string" || sessionId === "") {
    // warn: a defect because a reset without its replacement session leaves the cut pending.
    LOGGER.warn(
      { uuid: pending.vendorUuid },
      "the init after a conversation reset named no session; the clear's cut is still held",
    );
    return [];
  }
  state.pendingClear = undefined;
  return [clearedCutEntry(context, pending, sessionId)];
}

/**
 * The compaction row, once the vendor's summary record arrives.
 *
 * THE SUMMARY IS A RECORD OF ITS OWN, not the prose that follows: the vendor
 * states the boundary and then, as the very next stream record, a MAIN-stream
 * `user` record marked `isSynthetic` whose text IS the summary and whose uuid
 * is the one the boundary's `anchor_uuid` names (see `PendingCompaction`). A
 * boundary that names an anchor is released only by the record carrying it; one
 * that names none by the first main-stream synthetic user record, which in
 * every captured compaction is the record right after the boundary.
 *
 * The text is read the way the file plane reads the transcript's
 * `isCompactSummary` line — a string verbatim, text blocks joined by newlines —
 * because both planes write this cut under one key and must agree on it.
 */
function settleCompaction(
  message: Extract<SdkMessage, { type: "user" }>,
  context: FoldContext,
  state: FoldState,
): readonly PersistEntry[] {
  const pending = state.pendingCompaction;
  if (pending === undefined) return [];
  // THE BOUNDARY IS THE SESSION'S, so only the MAIN stream carries its summary,
  // and only as a record the vendor synthesized: a tool result or a prompt is
  // never it.
  if (
    spawningCallOf(message.parent_tool_use_id) !== undefined ||
    (message as { isSynthetic?: unknown }).isSynthetic !== true
  ) {
    LOGGER.logVerbose(
      { uuid: message.uuid, boundary_uuid: pending.vendorUuid },
      "a user record that cannot be a compaction summary arrived while a compaction is held",
    );
    return [];
  }
  if (pending.summaryUuid !== undefined && message.uuid !== pending.summaryUuid) {
    LOGGER.debug(
      { uuid: message.uuid, summary_uuid: pending.summaryUuid, boundary_uuid: pending.vendorUuid },
      "a synthetic user record arrived while a compaction is held; it is not the summary the boundary names",
    );
    return [];
  }
  const content = (message.message as { content?: unknown } | undefined)?.content;
  const summary =
    typeof content === "string"
      ? content
      : Array.isArray(content)
        ? content
            .filter((block) => (block as { type?: unknown }).type === "text")
            .map((block) => (block as { text?: unknown }).text)
            .filter((text): text is string => typeof text === "string")
            .join("\n")
        : "";
  state.pendingCompaction = undefined;
  if (summary === "") {
    LOGGER.error(
      {
        stream: "main",
        uuid: pending.vendorUuid,
        summary_uuid: message.uuid,
        age_ms: context.nowMs() - pending.heldAtMs,
        detail: "the summary record named by the boundary stated no text",
      },
      "the compaction's summary record carried no text; recording the cut without a summary",
    );
    return [compactionEntry(context, pending, undefined)];
  }
  LOGGER.debug(
    { uuid: pending.vendorUuid, summary_uuid: message.uuid },
    "the compaction's summary record arrived; releasing the held cut with it",
  );
  return [compactionEntry(context, pending, summary)];
}

/**
 * Release a held compaction WITHOUT its summary, because the vendor's sequence
 * moved past the point where the summary could still arrive.
 *
 * Two edges call it: the turn's terminal (a cut never outlives its turn) and a
 * second boundary (which never discards the first). Either one means the
 * summary never came, which is a defect in the vendor's sequence or in the
 * reading of it, so it is recorded at error with the stream, the boundary's
 * vendor uuid and how long the cut was held. The cut itself still lands, with
 * every figure the boundary stated and no summary.
 */
function releaseCompaction(
  context: FoldContext,
  state: FoldState,
  why: string,
): readonly PersistEntry[] {
  const pending = state.pendingCompaction;
  if (pending === undefined) return [];
  state.pendingCompaction = undefined;
  LOGGER.error(
    {
      stream: "main",
      uuid: pending.vendorUuid,
      summary_uuid: pending.summaryUuid,
      age_ms: context.nowMs() - pending.heldAtMs,
      detail: `${why} before the vendor stated the compaction's summary record`,
    },
    "a compaction's summary never arrived; recording the held cut without a summary",
  );
  return [compactionEntry(context, pending, undefined)];
}

/**
 * Remember the vendor's own account of a failed API request.
 *
 * ONE REMEMBERED VALUE, the last of the turn: a run can fail several times
 * before it gives up, and the class that ended the turn is the last one stated.
 * `retry_after_ms` is only ever carried when the vendor stated a delay — an
 * absent one stays absent rather than becoming a zero wait.
 */
function rememberVendorApiError(
  message: { readonly error?: unknown; readonly retry_delay_ms?: unknown; readonly message?: unknown },
  state: FoldState,
): void {
  const errorClass = typeof message.error === "string" ? message.error : undefined;
  const retryAfterMs =
    typeof message.retry_delay_ms === "number" && Number.isFinite(message.retry_delay_ms)
      ? Math.round(message.retry_delay_ms)
      : undefined;
  if (errorClass === undefined && retryAfterMs === undefined) return;
  // THE SENTENCE RIDES THE SAME MESSAGE AS THE CLASS: an assistant message that
  // states an `error` is the CLI's notice for it, and its prose is the vendor's
  // own account of the failure. Only a message that states a class carries one.
  const sentence = errorClass === undefined ? undefined : noticeText(message.message);
  const previous = state.vendorApiError;
  const merged: { errorClass?: string; retryAfterMs?: number; sentence?: string } = {};
  const heldClass = errorClass ?? previous?.errorClass;
  if (heldClass !== undefined) merged.errorClass = heldClass;
  const heldWait = retryAfterMs ?? previous?.retryAfterMs;
  if (heldWait !== undefined) merged.retryAfterMs = heldWait;
  const heldSentence = sentence ?? previous?.sentence;
  if (heldSentence !== undefined) merged.sentence = heldSentence;
  state.vendorApiError = merged;
  // EARLY VISIBILITY FOR THE TWO CLASSES THAT MATTER. The full diagnostic record
  // is the terminal's (it alone reaches the status, the human sentence, the
  // config-dir and the model), but a credential rejection or a
  // model/resource-not-found is surfaced HERE the moment the vendor first names
  // the class, at INFO so it shows without verbose. Every other class stays at
  // the low-visibility verbose line so ordinary rate-limit retries do not flood
  // the log. Coverage is preserved either way: exactly one record is written.
  const kind = classifyVendorApiFailure(undefined, merged.errorClass, "");
  if (kind === "shim.vendor.auth_rejected" || kind === "shim.vendor.model_missing") {
    LOGGER.info(
      { operation: kind, vendor_error: merged.errorClass, retry_after_ms: merged.retryAfterMs },
      "the vendor stated an authentication or resource error class; held for the turn's terminal",
    );
    return;
  }
  LOGGER.logVerbose(
    { vendor_error: merged.errorClass, retry_after_ms: merged.retryAfterMs },
    "the vendor stated an API failure class; held for the turn's terminal",
  );
}

/**
 * The prose of a CLI error notice, or `undefined` when it has none.
 *
 * Read loosely: the notice's `message` is an ordinary assistant message whose
 * content is a string or a list of blocks, and only its text is the sentence.
 */
function noticeText(message: unknown): string | undefined {
  const content = (message as { content?: unknown } | undefined)?.content;
  const text =
    typeof content === "string"
      ? content
      : Array.isArray(content)
        ? content
            .filter((block) => (block as { type?: unknown }).type === "text")
            .map((block) => (block as { text?: unknown }).text)
            .filter((part): part is string => typeof part === "string")
            .join("")
        : "";
  const trimmed = text.trim();
  return trimmed === "" ? undefined : trimmed;
}

/** Whether a settled response block is prose the agent produced. */
function answersWithProse(result: conversationv1.AgentResponse["result"]): boolean {
  if (result.case === "success") return true;
  return result.case === "failure" && (result.value.prose?.markdown ?? "") !== "";
}

/**
 * Remember which unit is the agent's ANSWER.
 *
 * `AgentCompleted.answer` names the LAST TOP-LEVEL prose the agent produced, so
 * a consumer marks it final without deriving finality from position. Only the
 * fold has seen which one that was, and only a TOP-LEVEL one qualifies: a
 * subagent's prose is that agent's answer, not this one's.
 *
 * A FAILED BLOCK THAT SAID SOMETHING IS STILL THE PROSE IT PRODUCED. The
 * contract leaves the answer unset only "when no prose was produced at all (a
 * refusal with empty content, a token ceiling hit before anything was said)",
 * so a block cut at the output ceiling after it had spoken is the answer, and
 * one that failed before saying anything is not. Accepting only settled
 * successes left a max-tokens turn naming no answer over the prose it drew,
 * which the daemon rightly raised as `final_answer_unresolved`.
 */
function rememberAnswer(
  message: Extract<SdkMessage, { type: "assistant" }>,
  entries: readonly PersistEntry[],
  state: FoldState,
): void {
  if (spawningCallOf(message.parent_tool_use_id) !== undefined) return;
  for (const entry of entries) {
    if (entry.item.kind !== "frame") continue;
    const result = entry.item.frame.result;
    const update = result.case === "update" ? result.value.update : undefined;
    if (update?.case !== "activity") continue;
    if (update.value.item.case !== "response") continue;
    if (!answersWithProse(update.value.item.value.result)) continue;
    state.lastAnswer = update.value.activityId;
  }
}
