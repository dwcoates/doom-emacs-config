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
 * each ONE remembered value — the current API response's first-block unit (so
 * usage rides exactly one unit), the per-message block counter (so
 * `<message.id>:<block_index>` is stable across the lines the vendor splits one
 * message into), and the message id the counter belongs to. Both are cleared at
 * `message_stop`. The last write/edit unit is the ENGINE's remembered value and
 * arrives on the context.
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
 *   - Usage and effort ride the FIRST content block's unit of each API
 *     response, and no other.
 *   - The EXEMPT SET is dropped silently and never becomes `AgentUnmodeled`;
 *     `AgentUnmodeled` means a genuinely unknown tool, and a recognizable
 *     built-in arriving there is a producer defect.
 *   - Anything unconvertible becomes RESIDUE rather than being dropped or
 *     crashing.
 *   - A record MISSING A FIELD the proto requires produces NO frame at all and
 *     a logged converter defect — never a partial message.
 *   - Every converter LOGS its branch.
 */
import { bindLog } from "../log.js";
import type { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import type { PersistEntry } from "../store/persistence.js";
import { convertDetached } from "./detached.js";
import type { FoldContext } from "./fold-context.js";
import { convertPermissionDenied } from "./permission.js";
import { residueEntry, residueForMessage } from "./residue.js";
import { convertSessionMessage } from "./session-updates.js";
import {
  createBlockState,
  convertAssistantMessage,
  convertStreamEvent,
  type BlockState,
} from "./stream-events.js";
import { convertResult } from "./terminals.js";
import { convertToolProgress, convertUserMessage } from "./tool-calls.js";

const LOGGER = bindLog({ component: "shim-convert-fold", operation: "shim.convert.fold" });

/**
 * Everything ONE SDK message produced.
 *
 * `turnEnded` is present EXACTLY when the message was the turn's `result` — the
 * only source of a turn terminal. Its frame is ALSO in `entries`: the terminal
 * is both a page line (the feed's stop notice has no other source) and the
 * engine's signal that the main thread can accept a prompt again, and making
 * the engine dig it back out of the list would be a second parse of a fact the
 * fold already resolved.
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
 * transport bookkeeping we have modelled as meaning nothing. Recording them
 * would fill the unserved table with keep-alive frames.
 */
const SILENTLY_IGNORED_TYPES = new Set<string>(["keep_alive"]);

/**
 * The fold, as the engine drives it.
 *
 * ONE method, called once per SDK message in arrival order. Synchronous on
 * purpose: a fold that could await would be able to interleave two messages and
 * break the ordering every upsert depends on.
 */
export interface Fold {
  /**
   * Convert one SDK message.
   *
   * NEVER THROWS: an unrecognized record is residue, and a malformed one is
   * residue plus a logged converter defect — a fold that threw would take the
   * session down over one bad vendor line.
   */
  onSdkMessage(message: SdkMessage, context: FoldContext): FoldOutput;
}

/**
 * Build a fold.
 *
 * The returned object holds ONLY the constant-size joins named in this file's
 * header. Nothing else survives a message.
 */
export function createFold(): Fold {
  const blocks: BlockState = createBlockState();

  return {
    onSdkMessage(message: SdkMessage, context: FoldContext): FoldOutput {
      try {
        return dispatch(message, context, blocks);
      } catch (error) {
        // A converter that throws is a DEFECT, and the honest answer to a
        // defect is residue plus a loud log — never a dead session, and never a
        // half-built message on the wire.
        const detail = error instanceof Error ? error.message : String(error);
        LOGGER.log(
          { level: "error", sdk_message_type: (message as { type?: string }).type, detail },
          "converter defect: the message produced no frame and lands as residue",
        );
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
function dispatch(message: SdkMessage, context: FoldContext, blocks: BlockState): FoldOutput {
  const type = message.type;
  if (SILENTLY_IGNORED_TYPES.has(type)) {
    LOGGER.logVerbose({ sdk_message_type: type }, "message carries no conversation fact; ignored");
    return EMPTY_FOLD_OUTPUT;
  }

  switch (type) {
    case "stream_event":
      return { entries: convertStreamEvent(message, context, blocks) };
    case "assistant":
      return { entries: convertAssistantMessage(message, context, blocks) };
    case "user":
      return { entries: convertUserMessage(message, context) };
    case "result":
      return convertResult(message, context);
    case "tool_progress":
      return { entries: convertToolProgress(message, context) };
    case "system":
      return { entries: convertSystemMessage(message, context) };
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

/** `type: "system"` fans out by subtype across three converter families. */
function convertSystemMessage(
  message: Extract<SdkMessage, { type: "system" }>,
  context: FoldContext,
): readonly PersistEntry[] {
  switch (message.subtype) {
    case "permission_denied":
      return convertPermissionDenied(message, context);
    case "task_started":
    case "task_updated":
    case "task_notification":
    case "task_progress":
    case "background_tasks_changed":
      return convertDetached(message, context);
    default:
      return convertSessionMessage(message, context);
  }
}
