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
 *   - the per-message BLOCK COUNTER (so `<message.id>:<block_index>` is stable
 *     across the lines the vendor splits one message into), cleared at
 *     `message_stop`;
 *   - the CALLS IN FLIGHT, so a tool result can restate its call's own facts —
 *     each entry dropped the moment its unit settles, and the table capped;
 *   - the HOOK FIRINGS in flight, for the same reason and on the same terms;
 *   - the KINDS OF THE TASKS in flight, on the same terms again: `task_started`
 *     is the only message that says whether a detached task is an agent run or
 *     a backgrounded shell command, and its `task_notification` must not settle
 *     a shell unit as a subagent;
 *   - ONE pending COMPACTION, because `ContextCompacted.summary` is not optional
 *     and the vendor states the boundary before the summary;
 *   - the LAST TOP-LEVEL RESPONSE unit, because `AgentCompleted.answer` names it
 *     and only the fold has seen which one it was.
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
import type { FoldContext } from "./fold-context.js";
import {
  convertHookResponse,
  convertHookStarted,
  createHookRegistry,
  type HookRegistry,
} from "./hooks.js";
import { convertPermissionDenied } from "./permission.js";
import { residueEntry, residueForMessage } from "./residue.js";
import {
  compactionEntry,
  convertSessionMessage,
  type PendingCompaction,
} from "./session-updates.js";
import {
  createBlockState,
  convertAssistantMessage,
  convertStreamEvent,
  convertThinkingTokens,
  type BlockState,
} from "./stream-events.js";
import { convertResult } from "./terminals.js";
import { convertToolProgressMessage, convertUserRecord } from "./tool-results.js";
import { createCallRegistry, type CallRegistry } from "./tool-calls.js";
import { TOOL_CONVERTERS } from "./tools/registry.js";

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
  readonly blocks: BlockState;
  readonly calls: CallRegistry;
  readonly hooks: HookRegistry;
  readonly taskKinds: TaskKindRegistry;
  pendingCompaction?: PendingCompaction;
  lastAnswer?: conversationv1.AgentActivityId;
}

/**
 * Build a fold.
 *
 * The returned object holds ONLY the joins named in this file's header. Nothing
 * else survives a message.
 */
export function createFold(): Fold {
  const state: FoldState = {
    blocks: createBlockState(),
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
        LOGGER.log(
          { level: "error", sdk_message_type: (message as { type?: string }).type, detail },
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
      LOGGER.log(
        { level: "error", sdk_message_type: type },
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
      return { entries: convertStreamEvent(message, context, state.blocks) };

    case "assistant": {
      const entries = [
        ...settleCompaction(message, context, state),
        ...convertAssistantMessage(message, context, state.blocks, state.calls, TOOL_CONVERTERS),
      ];
      rememberAnswer(message, entries, state);
      return { entries };
    }

    case "user":
      return {
        entries: convertUserRecord(
          message,
          context,
          state.calls,
          TOOL_CONVERTERS,
          state.taskKinds,
        ),
      };

    case "result":
      return convertResult(message, context, state.lastAnswer);

    case "tool_progress":
      return {
        entries: convertToolProgressMessage(message, context, state.calls, TOOL_CONVERTERS),
      };

    case "system":
      return { entries: convertSystemMessage(message, context, state) };

    case "rate_limit_event":
    case "conversation_reset":
      return { entries: convertSessionMessage(message, context) };

    default:
      LOGGER.log(
        { level: "warn", sdk_message_type: type },
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
    case "task_started":
    case "task_updated":
    case "task_notification":
    case "task_progress":
    case "background_tasks_changed":
      return convertDetached(message, context, state.taskKinds);
    case "hook_started":
      return convertHookStarted(message, context, state.hooks);
    case "hook_response":
      return convertHookResponse(message, context, state.hooks);
    case "thinking_tokens":
      return convertThinkingTokens(message, context, state.blocks);
    default:
      return convertSessionMessage(message, context, (pending) => {
        state.pendingCompaction = pending;
      });
  }
}

/**
 * The compaction row, once the assistant message carrying its summary arrives.
 *
 * `ContextCompacted.summary` is not optional — the feed shows the summary in the
 * cut's place, so the cut is not a hole — and the vendor states the boundary
 * FIRST. One boundary is held, and it is released by the very next assistant
 * prose, which is what that prose IS.
 */
function settleCompaction(
  message: Extract<SdkMessage, { type: "assistant" }>,
  context: FoldContext,
  state: FoldState,
): readonly PersistEntry[] {
  const pending = state.pendingCompaction;
  if (pending === undefined) return [];
  const content = (message.message as { content?: unknown } | undefined)?.content;
  const summary = Array.isArray(content)
    ? content
        .filter((block) => (block as { type?: unknown }).type === "text")
        .map((block) => (block as { text?: unknown }).text)
        .filter((text): text is string => typeof text === "string")
        .join("")
    : typeof content === "string"
      ? content
      : "";
  if (summary === "") {
    LOGGER.log(
      { level: "warn" },
      "the assistant message after a compaction boundary carried no prose; the cut is still held",
    );
    return [];
  }
  state.pendingCompaction = undefined;
  return [compactionEntry(context, pending, summary)];
}

/**
 * Remember which unit is the agent's ANSWER.
 *
 * `AgentCompleted.answer` names the LAST TOP-LEVEL prose the agent produced, so
 * a consumer marks it final without deriving finality from position. Only the
 * fold has seen which one that was, and only a TOP-LEVEL one qualifies: a
 * subagent's prose is that agent's answer, not this one's.
 */
function rememberAnswer(
  message: Extract<SdkMessage, { type: "assistant" }>,
  entries: readonly PersistEntry[],
  state: FoldState,
): void {
  if (message.parent_tool_use_id !== null && message.parent_tool_use_id !== "") return;
  for (const entry of entries) {
    if (entry.item.kind !== "frame") continue;
    const result = entry.item.frame.result;
    if (result.case !== "update") continue;
    const update = result.value.update;
    if (update.case !== "activity") continue;
    if (update.value.item.case !== "response") continue;
    if (update.value.item.value.result.case !== "success") continue;
    state.lastAnswer = update.value.activityId;
  }
}
