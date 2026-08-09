/**
 * async-routing — ID-ONLY ROUTING for detached work (MANDATED INVARIANT I2),
 * and SPOOL CONTINUITY for its byte streams (MANDATED INVARIANT I4).
 *
 * # I2 — an update lands by `bubble_id`, or it does not land
 *
 * `AsyncBubbleUpdate.bubble_id` is the whole addressing story. The daemon mints
 * the id when it classifies the spawning tool call and stamps the SAME string
 * on that call's `AgentToolCall.spawned_bubble_id`, so a frontend MATCHES an id
 * and never derives one. This module is where that promise is kept:
 *
 * - an update naming a bubble the registry does not hold is a GAP;
 * - an update whose arm does not match the named bubble's KIND is a GAP;
 * - a gap is reported loudly and routed to resync. It is never buffered "in
 *   case the bubble shows up", never coerced into the nearest bubble, and
 *   never applied partially.
 *
 * The predecessor of this module was a three-tier identity LADDER (a
 * classification, then a notification's id, then an id-shaped token in result
 * prose). A ladder is a staged-probabilistic identity: it is usually right, and
 * when it is wrong it is wrong silently and unreproducibly, because two
 * frontends walking it read different evidence. It is gone. There is one tier,
 * and it is the daemon's answer.
 *
 * # I4 — an append lands at the offset it claims, or it does not land
 *
 * `AsyncOutputAppend.from_offset` MUST equal the spool's current
 * `through_offset`. That check is the only thing that can tell a lost chunk
 * from a quiet one, so a mismatch is a gap on the same footing as an unknown
 * id — never applied, never "fixed up" by seeking into the text.
 *
 * # VALIDATE, THEN COMMIT
 *
 * `applyDelta` stages the whole push against a COPY and swaps it in only if
 * every update lands. A gap anywhere therefore leaves the registry byte-for-byte
 * as it was: "no partial mutation" is a property of the algorithm rather than a
 * discipline every caller has to remember.
 */

import {
  UPDATE_ARM_KIND,
  type AsyncBubble,
  type AsyncBubbleDelta,
  type AsyncBubbleKind,
  type AsyncBubbleKindCase,
  type AsyncBubbleUpdate,
  type AsyncBubbleUpdateCase,
  type AsyncOutputAppend,
  type AsyncOutputSpool,
} from "./async-bubble.js";
import { log } from "./wslog.js";

/** Offsets are BYTE counts on the wire, so they are measured in bytes here. */
const ENCODER = new TextEncoder();

/** WHY an update could not land. Each is a resync trigger, never a warning. */
export type AsyncGapKind =
  /** No bubble with this id is open. Never buffered against a future open. */
  | "unknown-bubble"
  /** The arm names a kind the bubble is not. Never coerced into its kind. */
  | "kind-mismatch"
  /** An append claimed an offset the spool is not at. Never seeked to. */
  | "offset-gap";

/** One rejected update, with the evidence that makes it diagnosable. */
export interface AsyncGap {
  kind: AsyncGapKind;
  /** The id the update named — the only thing routing was allowed to read. */
  bubbleId: string;
  /** The update arm that could not land. */
  arm: AsyncBubbleUpdateCase;
  /** The kind the named bubble actually is; absent when no such bubble. */
  bubbleKind?: AsyncBubbleKindCase;
  /** For an offset gap: where the spool is, and where the append claimed to be. */
  throughOffset?: number;
  fromOffset?: number;
  /** The resolved sentence for the log and the resync record. */
  detail: string;
}

/** What a push did, or the one gap that stopped it doing anything. */
export type AsyncApplyResult =
  | { ok: true; opened: number; updated: number }
  | { ok: false; gap: AsyncGap };

/** A bubble whose non-empty `parentBubbleId` names no bubble the registry holds. */
export interface AsyncOrphan {
  bubble: AsyncBubble;
  /** The parent id that resolved to nothing. */
  missingParentId: string;
}

/** Append BYTES onto a spool, at the offset the append claims. */
function appendedSpool(spool: AsyncOutputSpool, append: AsyncOutputAppend): AsyncOutputSpool {
  return {
    text: spool.text + append.text,
    throughOffset: append.fromOffset + ENCODER.encode(append.text).length,
  };
}

/**
 * The open bubbles of one session, and the ONLY thing that decides where an
 * update lands.
 *
 * Deliberately a plain map keyed by id: routing an update to a bubble deep in a
 * spawn tree is one lookup, not a recursive walk, which is exactly what
 * async-bubble.proto's parent-POINTER design buys. Nothing here recurses into a
 * payload, so nothing here needs a depth bound to terminate.
 */
export class AsyncBubbleRegistry {
  /** Insertion-ordered, which is the order bubbles opened. */
  private bubbles = new Map<string, AsyncBubble>();

  /** How many bubbles are open. */
  get size(): number {
    return this.bubbles.size;
  }

  /** The bubble with ID, or null. The one routing primitive. */
  get(id: string): AsyncBubble | null {
    return this.bubbles.get(id) ?? null;
  }

  /** Every open bubble, in the order they opened. */
  all(): AsyncBubble[] {
    return [...this.bubbles.values()];
  }

  /**
   * Adopt a reconnect snapshot: the daemon's COMPLETE statement of what is
   * still open, folded to date.
   *
   * It REPLACES rather than merges. `StateSnapshot.async_bubbles` is everything
   * the session holds, so a local bubble absent from it is a bubble the daemon
   * no longer holds — keeping it would show the user work that has been
   * reaped, and merging its fold with the snapshot's would produce a
   * transcript neither end vouches for.
   */
  adoptSnapshot(bubbles: readonly AsyncBubble[]): void {
    const before = this.bubbles.size;
    this.bubbles = new Map(bubbles.map((b) => [b.id, b]));
    log("info", `async-routing: adopted snapshot of ${bubbles.length} async bubble(s), replacing ${before}`, {
      operation: "async-routing.adopt-snapshot",
      context: { adopted: bubbles.length, replaced: before },
    });
  }

  /**
   * Apply one push. Every update lands, or NOTHING does and the caller resyncs.
   *
   * `opened` bubbles REPLACE any copy already held, per the contract: a
   * re-delivered bubble is the daemon restating it in full, not a second one.
   */
  applyDelta(delta: AsyncBubbleDelta): AsyncApplyResult {
    // Stage against a copy. The registry is not touched until the whole push
    // is known to land, so a gap cannot leave a half-applied bubble behind.
    const staged = new Map(this.bubbles);
    for (const bubble of delta.opened) staged.set(bubble.id, bubble);

    for (const update of delta.updates) {
      const routed = routeUpdate(staged, update);
      if (routed.gap !== null) {
        log("error", `async-routing: ${routed.gap.detail}`, {
          operation: "async-routing.gap",
          context: {
            workspace: delta.workspace,
            through_seq: delta.throughSeq,
            gap_kind: routed.gap.kind,
            bubble_id: routed.gap.bubbleId,
            update_arm: routed.gap.arm,
            bubble_kind: routed.gap.bubbleKind ?? null,
            through_offset: routed.gap.throughOffset ?? null,
            from_offset: routed.gap.fromOffset ?? null,
            opened_in_push: delta.opened.length,
            updates_in_push: delta.updates.length,
            decision: "reject-whole-push-and-resync",
          },
        });
        return { ok: false, gap: routed.gap };
      }
      staged.set(update.bubbleId, routed.bubble);
    }

    this.bubbles = staged;
    return { ok: true, opened: delta.opened.length, updated: delta.updates.length };
  }

  /**
   * The bubble a tool card's CLASSIFICATION VERDICT names, or null.
   *
   * SPAWNEDBUBBLEID is `AgentToolCall.spawned_bubble_id` /
   * `AgentToolOutcome.spawned_bubble_id` verbatim. Empty means "this call
   * detached nothing" and ONLY that — it is not a request to go looking, so it
   * returns null without a lookup. A non-empty id that names no open bubble
   * also returns null: the card simply has no bubble to draw yet, and
   * inventing one from the tool's name or its result prose is the derivation
   * this whole surface exists to forbid.
   */
  bubbleForSpawn(spawnedBubbleId: string): AsyncBubble | null {
    if (spawnedBubbleId === "") return null;
    return this.get(spawnedBubbleId);
  }

  /** Bubbles at the top of the tree: those with no parent pointer. */
  roots(): AsyncBubble[] {
    return this.all().filter((b) => b.parentBubbleId === "");
  }

  /** The bubbles spawned FROM parentId, in the order they opened. */
  children(parentId: string): AsyncBubble[] {
    return this.all().filter((b) => b.parentBubbleId === parentId);
  }

  /**
   * Bubbles whose parent pointer resolves to nothing.
   *
   * Reported rather than silently promoted to roots. A dangling pointer is a
   * real thing the user should be told about — the bubble is live work — but
   * drawing it as top-level would state a tree position the daemon never
   * claimed.
   */
  orphans(): AsyncOrphan[] {
    return this.all()
      .filter((b) => b.parentBubbleId !== "" && !this.bubbles.has(b.parentBubbleId))
      .map((b) => ({ bubble: b, missingParentId: b.parentBubbleId }));
  }
}

/** One update resolved against the staged map: the new bubble, or the gap. */
function routeUpdate(
  staged: ReadonlyMap<string, AsyncBubble>,
  update: AsyncBubbleUpdate,
): { bubble: AsyncBubble; gap: null } | { bubble: null; gap: AsyncGap } {
  const arm = update.update.case;
  const target = staged.get(update.bubbleId);
  if (target === undefined) {
    return {
      bubble: null,
      gap: {
        kind: "unknown-bubble",
        bubbleId: update.bubbleId,
        arm,
        detail:
          `update arm '${arm}' names bubble '${update.bubbleId}', which is not open — ` +
          `an update for an unknown id is a gap and is rejected, never buffered ` +
          `in the hope its bubble shows up`,
      },
    };
  }

  // `liveness` is the ONE kind-independent arm: every bubble is live or
  // settled regardless of what kind of work it is.
  if (update.update.case === "liveness") {
    return { bubble: { ...target, liveness: update.update.value }, gap: null };
  }

  const expected = UPDATE_ARM_KIND[update.update.case];
  if (target.kind.case !== expected) {
    return {
      bubble: null,
      gap: {
        kind: "kind-mismatch",
        bubbleId: update.bubbleId,
        arm,
        bubbleKind: target.kind.case,
        detail:
          `update arm '${arm}' addresses a '${target.kind.case}' bubble '${update.bubbleId}' — ` +
          `the arm must match the bubble's kind, so this is a daemon bug and is ` +
          `rejected, not coerced`,
      },
    };
  }

  switch (update.update.case) {
    case "agent": {
      // The bubble's kind was just proven to be `agent`, so this narrowing is
      // the check's own conclusion rather than an assumption.
      if (target.kind.case !== "agent") return unreachableKind(target, arm, update.bubbleId);
      const value = update.update.value;
      return {
        bubble: {
          ...target,
          kind: {
            case: "agent",
            value: {
              emissions: [...target.kind.value.emissions, ...value.emissions],
              // Restated by the producer, not deltaed: a dropped-count that
              // drifts is worse than one that is re-sent.
              fold: value.fold,
            },
          },
        },
        gap: null,
      };
    }
    case "journal": {
      if (target.kind.case !== "journal") return unreachableKind(target, arm, update.bubbleId);
      const value = update.update.value;
      return {
        bubble: {
          ...target,
          kind: {
            case: "journal",
            // Rows are APPEND-ONLY and never revised in place: a step that
            // starts running and later completes emits a running row and then
            // a done row, and this end does not rewrite that history.
            value: { rows: [...target.kind.value.rows, ...value.rows], fold: value.fold },
          },
        },
        gap: null,
      };
    }
    default: {
      // shell and unclassified: the SAME payload at the SAME offset rule. One
      // continuity check, written once, because there is no axis along which
      // the two could evolve apart.
      if (target.kind.case !== "shell" && target.kind.case !== "unclassified") {
        return unreachableKind(target, arm, update.bubbleId);
      }
      const spool = target.kind.value.output;
      const append = update.update.value;
      if (append.fromOffset !== spool.throughOffset) {
        return {
          bubble: null,
          gap: {
            kind: "offset-gap",
            bubbleId: update.bubbleId,
            arm,
            bubbleKind: target.kind.case,
            throughOffset: spool.throughOffset,
            fromOffset: append.fromOffset,
            detail:
              `append to bubble '${update.bubbleId}' claims offset ${append.fromOffset} but the ` +
              `spool is through ${spool.throughOffset} — a bare append cannot tell a lost chunk ` +
              `from a quiet one, so the mismatch is a gap and the bytes are rejected`,
          },
        };
      }
      const next = appendedSpool(spool, append);
      const kind: AsyncBubbleKind =
        target.kind.case === "shell"
          ? { case: "shell", value: { ...target.kind.value, output: next } }
          : { case: "unclassified", value: { ...target.kind.value, output: next } };
      return { bubble: { ...target, kind }, gap: null };
    }
  }
}

/**
 * The kind check above already proved this cannot happen. It is surfaced as a
 * gap rather than asserted away, because "cannot happen" is a claim about
 * today's code and a swallowed contradiction is how a routing bug becomes a
 * silently wrong transcript.
 */
function unreachableKind(
  target: AsyncBubble,
  arm: AsyncBubbleUpdateCase,
  bubbleId: string,
): { bubble: null; gap: AsyncGap } {
  return {
    bubble: null,
    gap: {
      kind: "kind-mismatch",
      bubbleId,
      arm,
      bubbleKind: target.kind.case,
      detail:
        `update arm '${arm}' passed the kind check against bubble '${bubbleId}' but its payload ` +
        `does not fit kind '${target.kind.case}' — the arm/kind table and the apply switch ` +
        `disagree, which is a defect in this module`,
    },
  };
}
