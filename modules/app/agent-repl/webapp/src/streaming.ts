/**
 * streaming — the ONE place that decides what a streamed block IS and how it
 * moves from a live preview to a settled record.
 *
 * WHY THIS MODULE EXISTS. A block's identity used to be re-derived at every hop
 * that touched it: the adapter built one id for a preview and a different id
 * for the finished record, and the store then re-implemented "which item does
 * this delta belong to" once per content kind. Those derivations drifted apart
 * twice. Most recently the preview keyed the TRUE API block index while the
 * finished record keyed its index within a one-element array — so a multi-block
 * message left a stalled half-typed card beside its final one, and two thinking
 * blocks of one message silently overwrote each other. The rule was never
 * written down anywhere, so nothing could notice the two halves diverging.
 *
 * So identity is named ONCE, here, and the lifecycle is a single state machine
 * with one vocabulary that every streaming kind feeds.
 *
 * THE TWO IDS, and why a block needs both:
 * - The FEED PLACE (`blockId`) is where the block lives on screen. `smooth.ts`
 *   tracks its reveal cursor by it and `render.ts` keys its DOM node on it, so
 *   it is pinned by whoever opened the block and never moves again. Moving it
 *   would re-type prose already on screen and remount the node mid-stream.
 * - The RECORD IDENTITY (`uuid`) is what the finished record calls itself. It
 *   does not exist until the message has finished being emitted, and it is the
 *   only thing a redelivered record can be recognized by.
 *
 * They cannot be collapsed into one id, because the live stream and the
 * finished record genuinely share no identifier — which is exactly why pairing
 * them is a MATCH and not a lookup.
 *
 * THE LIFECYCLE. Every streamed block is in one of two phases, and there are
 * exactly three transitions:
 *
 *     (none) --open--> previewing --grow--> previewing --settle--> final
 *
 * `settle` also lands on no preview at all (a replayed or gap-filled record
 * whose deltas this webapp never saw) and on an already-final block (the same
 * record delivered twice); both are the same transition finding a different
 * home, which is why one function owns all three outcomes.
 *
 * WHAT THIS MODULE DOES NOT OWN. Transport: the EPHEMERAL delta relay and the
 * PERSISTENT record plane stay exactly as they are, and this module never
 * learns which one a fact arrived on. It is told "a block grew" or "a block
 * finished" and reconciles the feed accordingly.
 */
import type { ConversationItem, TextItem, ThinkingItem, ToolItem } from "./store.js";

/** The content kinds that stream. */
export type StreamKind = "text" | "thinking" | "input_json";

/** Where a streamed block is in its lifecycle. */
export type StreamPhase = "previewing" | "final";

/**
 * The prose blocks the feed holds as items of their own. `input_json` is
 * absent on purpose: a tool-input stream grows the `inputJson` of the ToolItem
 * that owns the call rather than opening a block of its own.
 */
export type StreamedBlock = TextItem | ThinkingItem;

/** One live delta, as the ephemeral relay hands it over. */
interface StreamDeltaBase {
  /**
   * The ANTHROPIC message id every delta of one message shares. It is stamped
   * ONCE, at the source (`agent-shim/.../proto/delta.ts`, from the
   * `message_start` that opened the message), and carried opaquely by every hop
   * after it. No consumer re-derives it from anything else.
   */
  messageId: string;
  /** The block's ordinal within that message, as the API numbered it. */
  blockIndex: number;
  delta: string;
}

export type StreamDelta =
  | (StreamDeltaBase & { kind: "text" | "thinking" })
  | (StreamDeltaBase & { kind: "input_json"; toolUseId: string });

/**
 * What a delta did. A delta that changed nothing says WHY, so the caller can
 * report it — a silently dropped delta is prose the user never sees.
 */
export type DeltaOutcome =
  | { changed: true }
  | {
      changed: true;
      toolInput: {
        toolUseId: string;
        phase: "absent" | "preview";
        branch: "preview-created" | "preview-appended";
      };
    }
  | {
      changed: false;
      reason: "superseded-authoritative-tool";
      toolInput: { toolUseId: string; phase: "final"; branch: "superseded-authoritative-final" };
    };

// --- identity ---------------------------------------------------------------

/**
 * The feed place a live preview opens.
 *
 * Built from the only two facts the live stream states about a block — which
 * message it belongs to, and which API block it is — because they are the only
 * two it CAN state. The record's own identity does not exist yet.
 */
export function previewBlockId(messageId: string, blockIndex: number): string {
  return `${messageId}:${blockIndex}`;
}

/**
 * The identity a FINISHED block carries, taken from the envelope of the record
 * that delivered it.
 *
 * From the ENVELOPE, never from the message id plus a content index. The SDK
 * emits one assistant record PER CONTENT BLOCK — every record of one API
 * message repeating the same `message.id` over a `content` array of length ONE
 * — so that index is always 0 and identifies nothing. Keying on it collapsed
 * every block of a message onto a single key.
 *
 * `uuid` is left absent when the frame carried no envelope identity at all,
 * which leaves the block keyed on its feed place: degraded, but far better than
 * keying every id-less block in the session onto one shared id.
 */
export function recordBlockIdentity(
  envelopeUuid: string,
  messageId: string,
  index: number,
): { blockId: string; uuid?: string } {
  const identity = envelopeUuid || messageId;
  const blockId = `${identity}:${index}`;
  return identity === "" ? { blockId } : { blockId, uuid: blockId };
}

/** Which phase a streamed block is in. */
export function phaseOf(block: StreamedBlock): StreamPhase {
  return block.uuid === undefined ? "previewing" : "final";
}

/**
 * A streamed block's reconciliation key: its record identity once settled, else
 * the feed place its preview opened.
 *
 * A settled block cannot stay keyed on its feed place, because it holds
 * whatever place its PREVIEW opened and a redelivered record has no way to
 * reproduce that. Keyed on the record identity instead, a resync recognizes the
 * block it already produced rather than appending a second copy of prose that
 * is already on screen.
 */
export function blockKey(block: StreamedBlock): string {
  return `${block.kind}:${block.uuid === undefined ? block.blockId : `u:${block.uuid}`}`;
}

// --- feed order ---------------------------------------------------------------

/**
 * Insert `item` into the feed at its seq rank: after every item ranked at or
 * below `seq`, before the first ranked above it.
 *
 * This — not appending — is what keeps the feed in conversation order. The
 * connect resync replays history (low seqs) on the same socket as live pushes
 * (high seqs), so arrival order interleaves the two arbitrarily; a live prompt
 * echo used to land wherever the replay happened to be and stay there. Ties
 * keep arrival order (items of one delta share its through-seq and must not
 * reorder), and an unranked item — minted before ranking existed — ranks as
 * oldest, so it never blocks a ranked insert from passing it.
 *
 * The scan runs from the tail because the live steady state IS an append:
 * each new item ranks at or above everything mounted, and the loop exits
 * immediately.
 */
export function insertBySeq(
  items: ConversationItem[],
  item: ConversationItem,
  seq: number,
): void {
  item.seq = seq;
  let idx = items.length;
  while (idx > 0 && (items[idx - 1].seq ?? Number.MIN_SAFE_INTEGER) > seq) idx--;
  items.splice(idx, 0, item);
}

// --- transitions ------------------------------------------------------------

/**
 * OPEN or GROW: fold one live delta onto the feed.
 *
 * Prose finds its block by feed place, opening one on first sight. A delta for
 * a block that has already settled still grows it (and reopens it), so a late
 * chunk is never dropped on the floor for arriving after its record.
 *
 * TOOL INPUT resolves by its required stable `toolUseId`. An early ephemeral
 * chunk opens a preview ToolItem when its durable call has not arrived; an
 * authoritative final makes a later chunk superseded. No ambient open state
 * or feed position participates in tool reconciliation.
 */
export function applyStreamDelta(
  items: ConversationItem[],
  delta: StreamDelta,
  nowIso: string,
  orderSeq: number,
): DeltaOutcome {
  if (delta.kind === "input_json") return growToolInput(items, delta, nowIso, orderSeq);

  const blockId = previewBlockId(delta.messageId, delta.blockIndex);
  const open = items.find(
    (i): i is StreamedBlock => (i.kind === "text" || i.kind === "thinking") && i.blockId === blockId,
  );
  if (open) {
    open.text += delta.delta;
    open.done = false;
    return { changed: true };
  }
  // ORDERSEQ is the store's high-water mark, not a delta seq: the typing relay
  // is ephemeral and carries none. A preview is live by definition, so the
  // high-water mark ranks it after everything known — while a concurrent
  // history replay (lower seqs) still slots above it.
  insertBySeq(
    items,
    delta.kind === "thinking"
      ? { kind: "thinking", blockId, messageId: delta.messageId, text: delta.delta, done: false }
      : {
          kind: "text",
          blockId,
          messageId: delta.messageId,
          text: delta.delta,
          done: false,
          ts: nowIso,
        },
    orderSeq,
  );
  return { changed: true };
}

/**
 * SETTLE: fold one finished streamed block onto the feed.
 *
 * Three homes, in order of specificity:
 * 1. the still-open preview its deltas grew (see {@link previewIndexFor});
 * 2. the block this same record already produced, found by record identity —
 *    a resync or gap-fill redelivering it;
 * 3. nowhere yet, so it is appended — a replayed message whose deltas this
 *    webapp never saw must still render.
 */
export function settleStreamedBlock(
  items: ConversationItem[],
  block: StreamedBlock,
  orderSeq: number,
): void {
  const previewIdx = previewIndexFor(items, block);
  if (previewIdx !== -1) {
    items[previewIdx] = adopt(items[previewIdx] as StreamedBlock, block);
    return;
  }
  const key = blockKey(block);
  const idx = items.findIndex(
    (i) => (i.kind === "text" || i.kind === "thinking") && blockKey(i) === key,
  );
  if (idx !== -1) {
    items[idx] = adopt(items[idx] as StreamedBlock, block);
    return;
  }
  insertBySeq(items, block, orderSeq);
}

/**
 * The index of the still-streaming preview `block` settles onto, or -1.
 *
 * The pair share no key, so they are matched on what they genuinely do share:
 * the same message, the same kind, and the same owning agent.
 *
 * The match is deliberately narrow. Only a block that has actually FINISHED (it
 * carries a record identity) may claim a preview, and only a preview still in
 * the `previewing` phase and still open may be claimed — so a settled block is
 * never re-adopted by a later record.
 *
 * EARLIEST unclaimed preview wins, which is what keeps two same-kind blocks of
 * one message in block order: the records arrive in that order and each stamps
 * its identity as it claims, so the next record meets the next preview instead
 * of re-claiming the first.
 */
function previewIndexFor(items: readonly ConversationItem[], block: StreamedBlock): number {
  if (phaseOf(block) !== "final" || !block.done) return -1;
  return items.findIndex(
    (i) =>
      (i.kind === "text" || i.kind === "thinking") &&
      i.kind === block.kind &&
      phaseOf(i) === "previewing" &&
      !i.done &&
      i.messageId === block.messageId &&
      i.parentToolUseId === block.parentToolUseId,
  );
}

/**
 * Land a finished block onto the item already standing for it.
 *
 * The finished block wins on every field it carries — it is the authoritative
 * version of the prose — EXCEPT the feed place and the feed rank, which stay
 * as the standing item opened them: a settle replaces content, never position.
 */
function adopt<T extends StreamedBlock>(existing: T, incoming: T): T {
  return { ...incoming, blockId: existing.blockId, seq: existing.seq };
}

/** Reconcile one input chunk onto its exact tool identity; see {@link applyStreamDelta}. */
function growToolInput(
  items: ConversationItem[],
  delta: Extract<StreamDelta, { kind: "input_json" }>,
  nowIso: string,
  orderSeq: number,
): DeltaOutcome {
  const item = items.find((candidate): candidate is ToolItem =>
    candidate.kind === "tool" && candidate.toolUseId === delta.toolUseId,
  );
  if (item === undefined) {
    insertBySeq(items, {
      kind: "tool",
      toolUseId: delta.toolUseId,
      toolName: "",
      messageId: delta.messageId,
      ts: nowIso,
      inputJson: delta.delta,
      inputDone: false,
    }, orderSeq);
    return { changed: true, toolInput: { toolUseId: delta.toolUseId, phase: "absent", branch: "preview-created" } };
  }
  if (item.inputDone) {
    return {
      changed: false,
      reason: "superseded-authoritative-tool",
      toolInput: { toolUseId: delta.toolUseId, phase: "final", branch: "superseded-authoritative-final" },
    };
  }
  item.inputJson += delta.delta;
  return { changed: true, toolInput: { toolUseId: delta.toolUseId, phase: "preview", branch: "preview-appended" } };
}
