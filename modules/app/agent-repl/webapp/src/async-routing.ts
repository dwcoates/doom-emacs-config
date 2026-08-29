/**
 * async-routing — ID-ONLY ROUTING for detached work (MANDATED INVARIANT I2),
 * and SPOOL CONTINUITY for its byte streams (MANDATED INVARIANT I4).
 *
 * # I2 — an update lands by `message_id`, or it does not land
 *
 * `DetachedWorkUpdate.message_id` is the whole addressing story. The daemon
 * mints the MESSAGE when it classifies the spawning tool call and stamps that
 * message's uuid on the call's `AgentToolCall.spawned_message_id`, so a frontend
 * MATCHES an id and never derives one. There is exactly ONE id space now — the
 * message's — so the bridge between two of them is gone rather than kept in
 * agreement. This module is where that promise is kept:
 *
 * - an update naming a message the registry does not hold is a GAP;
 * - an update whose arm does not match the named message's KIND is a GAP;
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
 * `DetachedWorkOutputAppend.from_offset` MUST equal the spool's current
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
import { log } from "./log.js";

/** Offsets are BYTE counts on the wire, so they are measured in bytes here. */
const ENCODER = new TextEncoder();

/** WHY an update could not land. Each is a resync trigger, never a warning. */
export type AsyncGapKind =
  /** No message with this id is open. Never buffered against a future open. */
  | "unknown-bubble"
  /** The arm names a kind the message's work is not. Never coerced into it. */
  | "kind-mismatch"
  /** An append claimed an offset the spool is not at. Never seeked to. */
  | "offset-gap";

/** One rejected update, with the evidence that makes it diagnosable. */
export interface AsyncGap {
  kind: AsyncGapKind;
  /**
   * The MESSAGE id the update named — the only thing routing was allowed to
   * read, and now the only id space there is.
   */
  messageId: string;
  /** The update arm that could not land. */
  arm: AsyncBubbleUpdateCase;
  /** The kind the named message's work actually is; absent when no such message. */
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

/**
 * Work whose non-empty `parentMessageId` names no message the registry holds.
 *
 * THIS MATTERS MORE UNDER LINEAGE, NOT LESS. `topLevelMessageId` is
 * denormalized, so a parent that resolves to nothing means the root this work
 * claims cannot be checked against the chain that should produce it — exactly
 * the drift the contract calls corruption.
 */
export interface AsyncOrphan {
  bubble: AsyncBubble;
  /** The parent message id that resolved to nothing. */
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
 * Deliberately a plain map keyed by MESSAGE id: routing an update to work deep
 * in a spawn tree is one lookup, not a recursive walk, which is exactly what
 * `MessageLineage`'s parent-POINTER design buys. Nothing here recurses into a
 * payload, so nothing here needs a depth bound to terminate.
 */
/**
 * ONE bubble's live-typing preview: the ephemeral text the daemon relayed for a
 * record it is folding into that bubble.
 *
 * It is a PREVIEW, never content. Nothing here is ever persisted, ranked into
 * the feed, or counted as part of the bubble's transcript — it exists only
 * until the bubble's own authoritative record arrives and retires it.
 */
export interface AsyncBubbleTyping {
  /**
   * The ANTHROPIC API message id the previewed block belongs to.
   *
   * Named `apiMessageId` rather than `messageId` because there is now a FEED
   * message id in scope on every one of these calls, and two different id
   * spaces sharing one name is how a preview ends up keyed by the wrong one.
   */
  apiMessageId: string;
  /** The block's ordinal within that message. */
  blockIndex: number;
  /** The chunks relayed for that block so far, concatenated. */
  text: string;
}

export class AsyncBubbleRegistry {
  /** Insertion-ordered, which is the order bubbles opened. */
  private bubbles = new Map<string, AsyncBubble>();

  /**
   * THE BUBBLE-SCOPED LIVE TYPING PREVIEWS, keyed by bubble id.
   *
   * A preview is retired by the authoritative record of the same block landing
   * on the SAME SURFACE. While an async window is open the daemon folds the
   * session's records off the top-level feed and into a bubble, so a preview of
   * one of those records opened on the FEED could never be retired: it would
   * spin "streaming input..." with no body for the life of the page, which is
   * exactly what the user reported seeing six of in a row.
   *
   * So the daemon addresses those previews to the message
   * (`TypingDelta.parent_message_id`) and they live HERE instead, where that
   * message's
   * own next authoritative update retires them — see {@link applyDelta}. The
   * feed never sees them, and there is no timer anywhere: a preview whose
   * retirement is structurally guaranteed does not need one.
   */
  private typing = new Map<string, AsyncBubbleTyping>();

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
   * It REPLACES rather than merges. `StateSnapshot.detached_work` is everything
   * the session holds, so a local bubble absent from it is a bubble the daemon
   * no longer holds — keeping it would show the user work that has been
   * reaped, and merging its fold with the snapshot's would produce a
   * transcript neither end vouches for.
   */
  adoptSnapshot(bubbles: readonly AsyncBubble[]): void {
    const before = this.bubbles.size;
    this.bubbles = new Map(bubbles.map((b) => [b.id, b]));
    // Previews are ephemeral and the snapshot is authoritative about content,
    // so every standing preview is now either redundant or orphaned. Both are
    // dropped: a preview retained across a resync is one the snapshot that
    // superseded it will never come back to retire.
    this.typing.clear();
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
            message_id: routed.gap.messageId,
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
      staged.set(update.messageId, routed.bubble);
    }

    this.bubbles = staged;
    // THE RETIREMENT. Every bubble this push spoke for has now stated its own
    // authoritative content, which is precisely what a preview of that content
    // was standing in for. Dropping it here — in the same commit that lands the
    // record, and only once the whole push is known to land — is why a
    // bubble-scoped preview can never outlive the window it belongs to.
    for (const bubble of delta.opened) this.typing.delete(bubble.id);
    for (const update of delta.updates) this.typing.delete(update.messageId);
    return { ok: true, opened: delta.opened.length, updated: delta.updates.length };
  }

  /**
   * Open or extend BUBBLEID's live-typing preview with one relayed chunk.
   *
   * A chunk for a DIFFERENT block than the one being previewed REPLACES it
   * rather than appending: the previous block's preview is over, and running
   * two blocks' text together would draw prose the model never emitted.
   *
   * A chunk for a message the registry does not hold is DROPPED and reported.
   * The alternative — keeping it against the hope that its message shows up —
   * is a preview with nothing that can ever retire it, which is the whole
   * failure this routing exists to prevent. Returns whether anything changed.
   */
  applyTyping(parentMessageId: string, apiMessageId: string, blockIndex: number, chunk: string): boolean {
    if (!this.bubbles.has(parentMessageId)) {
      log("warn", `async-routing: live typing for unknown message ${parentMessageId} — dropped`, {
        operation: "async-routing.typing-orphan",
        context: {
          parent_message_id: parentMessageId,
          api_message_id: apiMessageId,
          block_index: blockIndex,
          chunk_length: chunk.length,
          decision: "drop-preview-nothing-could-retire-it",
        },
      });
      return false;
    }
    const held = this.typing.get(parentMessageId);
    if (held !== undefined && held.apiMessageId === apiMessageId && held.blockIndex === blockIndex) {
      this.typing.set(parentMessageId, { apiMessageId, blockIndex, text: held.text + chunk });
      return true;
    }
    this.typing.set(parentMessageId, { apiMessageId, blockIndex, text: chunk });
    return true;
  }

  /**
   * Retire PARENTMESSAGEID's live-typing preview because the daemon says nothing
   * will ever complete it.
   *
   * A preview inside detached work is retired by that message's next
   * authoritative update. When that update can never arrive — the session died,
   * the shim rolled, the query was torn down mid-block — the card would
   * otherwise spin "streaming input…" with no body for the life of the page.
   * This is the daemon stating that fact; it is never a deadline this store
   * guesses, which is why there is no timer anywhere near it.
   */
  cutTyping(parentMessageId: string): boolean {
    return this.typing.delete(parentMessageId);
  }

  /** PARENTMESSAGEID's live-typing preview, or null when it has none standing. */
  typingFor(parentMessageId: string): AsyncBubbleTyping | null {
    return this.typing.get(parentMessageId) ?? null;
  }

  /**
   * The work a tool card's CLASSIFICATION VERDICT names, or null.
   *
   * SPAWNEDMESSAGEID is `AgentToolCall.spawned_message_id` /
   * `AgentToolOutcome.spawned_message_id` verbatim. Empty means "this call
   * detached nothing" and ONLY that — it is not a request to go looking, so it
   * returns null without a lookup. A non-empty id that names no open message
   * also returns null: the card simply has no work to draw yet, and
   * inventing some from the tool's name or its result prose is the derivation
   * this whole surface exists to forbid.
   */
  bubbleForSpawn(spawnedMessageId: string): AsyncBubble | null {
    if (spawnedMessageId === "") return null;
    return this.get(spawnedMessageId);
  }

  /**
   * The work the daemon attributed to the tool call TOOLUSEID, by matching
   * `DetachedWork.origin_tool_use_id`.
   *
   * THE OTHER END OF THE SAME DAEMON FACT. The classification links one tool
   * call to one piece of detached work, and the daemon publishes that link
   * from both ends: `origin_tool_use_id` on the work, `spawned_message_id` on
   * the call's emission. Matching either is matching a daemon-published id by
   * exact string equality — nothing here is parsed, scored or inferred, so
   * this is not a rung of an identity ladder.
   *
   * PROVENANCE, NEVER CONTAINMENT: this says which call STARTED the work, which
   * is a different fact from what CONTAINS it (`parentMessageId`), and reading
   * one as the other would place a feed row inside the card that launched it.
   *
   * Empty means "no tool call spawned this work", so it never matches: a card
   * with no id cannot own work that names no card.
   *
   * Returns a LIST because the wire permits several pieces of work to name one
   * call and silently keeping the first would hide the rest. Callers that need
   * a single answer say so through {@link bubbleForCall}.
   */
  bubblesForToolUse(toolUseId: string): AsyncBubble[] {
    if (toolUseId === "") return [];
    return this.all().filter((b) => b.originToolUseId === toolUseId);
  }

  /**
   * The work a tool card owns, resolved from BOTH ends of the daemon's
   * classification — and the check that they agree.
   *
   * This is deliberately not a preference order. A ladder consults a weaker
   * tier when a stronger one is silent and lets the two disagree in silence;
   * here the two are the same fact written twice, so a DISAGREEMENT is a
   * daemon bug and is reported as one rather than resolved by whichever tier
   * is listed first. What each end supplies is only WHEN it is available:
   * `spawned_message_id` rides a tool emission and `origin_tool_use_id` rides
   * the work, and a wire that carries one but not the other still answers
   * the question exactly once.
   */
  bubbleForCall(toolUseId: string, spawnedMessageId?: string): AsyncBubble | null {
    const named = spawnedMessageId === undefined ? null : this.bubbleForSpawn(spawnedMessageId);
    const byOrigin = this.bubblesForToolUse(toolUseId);
    if (named !== null && byOrigin.length > 0 && !byOrigin.some((b) => b.id === named.id)) {
      log("error", `async-routing: call ${toolUseId} names message ${named.id} while message(s) ${byOrigin.map((b) => b.id).join(", ")} name that call — the daemon's two statements of one classification disagree`, {
        operation: "async-routing.classification-disagreement",
        dedupKey: `async-classification-disagreement:${toolUseId}`,
        context: {
          tool_use_id: toolUseId,
          spawned_message_id: named.id,
          origin_message_ids: byOrigin.map((b) => b.id),
          decision: "reject-both",
        },
      });
      // Neither statement is preferred, because preferring one would be the
      // staged-probabilistic choice this module exists to refuse. The card
      // draws nothing and the contradiction is on the record.
      return null;
    }
    return named ?? byOrigin[0] ?? null;
  }

  /**
   * Work at the top of the tree: whose `parentMessageId` is empty, which the
   * contract states is the fact "this message sits directly in the feed" rather
   * than a placeholder for an unresolved parent.
   */
  roots(): AsyncBubble[] {
    return this.all().filter((b) => b.parentMessageId === "");
  }

  /** The work CONTAINED BY parentMessageId, in the order it opened. */
  children(parentMessageId: string): AsyncBubble[] {
    return this.all().filter((b) => b.parentMessageId === parentMessageId);
  }

  /**
   * Work whose `parentMessageId` resolves to nothing.
   *
   * Reported rather than silently promoted to roots. A dangling pointer is a
   * real thing the user should be told about — it is live work — but drawing it
   * as top-level would state a containment the daemon never claimed, and under
   * lineage it would also contradict the `topLevelMessageId` the work itself
   * carries.
   */
  orphans(): AsyncOrphan[] {
    return this.all()
      .filter((b) => b.parentMessageId !== "" && !this.bubbles.has(b.parentMessageId))
      .map((b) => ({ bubble: b, missingParentId: b.parentMessageId }));
  }
}

/** One update resolved against the staged map: the new bubble, or the gap. */
function routeUpdate(
  staged: ReadonlyMap<string, AsyncBubble>,
  update: AsyncBubbleUpdate,
): { bubble: AsyncBubble; gap: null } | { bubble: null; gap: AsyncGap } {
  const arm = update.update.case;
  const target = staged.get(update.messageId);
  if (target === undefined) {
    return {
      bubble: null,
      gap: {
        kind: "unknown-bubble",
        messageId: update.messageId,
        arm,
        detail:
          `update arm '${arm}' names message '${update.messageId}', which is not open — ` +
          `an update for an unknown id is a gap and is rejected, never buffered ` +
          `in the hope its message shows up`,
      },
    };
  }

  // `liveness` is the ONE kind-independent arm: every piece of work is live or
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
        messageId: update.messageId,
        arm,
        bubbleKind: target.kind.case,
        detail:
          `update arm '${arm}' addresses '${target.kind.case}' work on message '${update.messageId}' — ` +
          `the arm must match the work's kind, so this is a daemon bug and is ` +
          `rejected, not coerced`,
      },
    };
  }

  switch (update.update.case) {
    case "agent": {
      // The bubble's kind was just proven to be `agent`, so this narrowing is
      // the check's own conclusion rather than an assumption.
      if (target.kind.case !== "agent") return unreachableKind(target, arm, update.messageId);
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
    case "merge": {
      // The SAME payload as `agent`, applied by the same rule: a merge run's
      // emissions arrive exactly as a detached agent's do.
      if (target.kind.case !== "merge") return unreachableKind(target, arm, update.messageId);
      const value = update.update.value;
      return {
        bubble: {
          ...target,
          kind: {
            case: "merge",
            value: {
              emissions: [...target.kind.value.emissions, ...value.emissions],
              fold: value.fold,
            },
          },
        },
        gap: null,
      };
    }
    case "skill": {
      if (target.kind.case !== "skill") return unreachableKind(target, arm, update.messageId);
      const inner = update.update.value;
      const current = target.kind.value;
      // Body resolution REPLACES the body whole; emissions append. Two arms,
      // two lifetimes, and neither one touches the other's field.
      const value =
        inner.case === "body"
          ? { ...current, body: inner.value }
          : {
              ...current,
              emissions: [...current.emissions, ...inner.value.emissions],
              fold: inner.value.fold,
            };
      return { bubble: { ...target, kind: { case: "skill", value } }, gap: null };
    }
    case "journal": {
      if (target.kind.case !== "journal") return unreachableKind(target, arm, update.messageId);
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
        return unreachableKind(target, arm, update.messageId);
      }
      const spool = target.kind.value.output;
      const append = update.update.value;
      if (append.fromOffset !== spool.throughOffset) {
        return {
          bubble: null,
          gap: {
            kind: "offset-gap",
            messageId: update.messageId,
            arm,
            bubbleKind: target.kind.case,
            throughOffset: spool.throughOffset,
            fromOffset: append.fromOffset,
            detail:
              `append to message '${update.messageId}' claims offset ${append.fromOffset} but the ` +
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
  messageId: string,
): { bubble: null; gap: AsyncGap } {
  return {
    bubble: null,
    gap: {
      kind: "kind-mismatch",
      messageId,
      arm,
      bubbleKind: target.kind.case,
      detail:
        `update arm '${arm}' passed the kind check against message '${messageId}' but its payload ` +
        `does not fit kind '${target.kind.case}' — the arm/kind table and the apply switch ` +
        `disagree, which is a defect in this module`,
    },
  };
}
