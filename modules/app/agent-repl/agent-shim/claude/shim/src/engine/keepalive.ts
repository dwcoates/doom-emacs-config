/**
 * engine/keepalive.ts — the keep-alive cadence, and the yield it owes a real
 * prompt.
 *
 * RESPONSIBILITY. The vendor's prompt cache lapses on a timer, and a lapsed
 * cache makes the next real turn cost full price. The shim submits its own
 * minimal turns to keep it warm — entirely shim-internal work that no consumer
 * asked for and none should see.
 *
 * THE MARKER IS THE CONTRACT (ruled 2026-08-29). Every keep-alive prompt BEGINS
 * with the literal `<!--agent-repl:keepalive-->`, mirroring the existing
 * `<!--agent-repl:meta-->` marker. The store and sidecar treat a turn opened by
 * such a prompt as keep-alive — its prompt and every frame land as
 * `unserved_item.keepalive` — until the next non-keep-alive prompt.
 *
 * THE INTERVAL. Four minutes, against the vendor's FIVE-minute ephemeral tier
 * (`cache_creation.ephemeral_5m_input_tokens`, engine/cold.ts). The tier is the
 * shorter of the two the vendor buys, so keeping it warm keeps the 1-hour tier
 * warm as well; the one-minute margin absorbs a slow turn without letting the
 * cache lapse between beats.
 *
 * THE YIELD OBLIGATION. A real prompt must never build on keep-alive context,
 * so before one is delivered the vendor context is ROLLED BACK to just after
 * the last real record. The mechanism is the SDK's own declared one: close the
 * query and open a new one with `resume: <vendor session id>` and
 * `resumeSessionAt: <uuid of the last real record>` — "when resuming, only
 * resume messages up to and including the message with this UUID". It is the
 * only declared surface that truncates a conversation without rewriting the
 * vendor's file, and rewriting the file is the thing the transcript backup
 * exists because we cannot safely do. The rewind is an explicit, tested step:
 * {@link KeepaliveRewind.obligation} answers whether one is owed and names the
 * uuid to resume at.
 *
 * THE ANCHOR IS AN ASSISTANT RECORD OF A REAL TURN (ruled 2026-09-14). The SDK
 * states the type of the uuid it will accept: "The message ID should be from
 * `SDKAssistantMessage.uuid`". Every OTHER message the vendor emits carries a
 * `uuid` too — `system:init` and `result` both declare one as a REQUIRED field —
 * and those uuids name no transcript record, so a query opened at one is
 * refused by the vendor with `No message found with message.uuid of: <uuid>`
 * and the session dies with the prompt it was carrying. It killed two of the
 * owner's real sessions on 2026-09-14. So {@link KeepaliveRewind.noteRecord}
 * takes the MESSAGE, not a uuid, and refuses everything that is not an
 * `assistant` message of a REAL (non-keep-alive) open turn — the filter cannot
 * be got wrong by a caller, because the caller never extracts the uuid.
 *
 * AND THE ANCHOR DOES NOT CROSS A BOUNDARY. A uuid from before a compaction, a
 * conversation reset, or a fresh query binding may no longer be resumable, so
 * every such event {@link KeepaliveRewind.clearAnchor}s it and the next real
 * prompt after keep-alives simply carries them instead of rewinding.
 */
import { bindLog } from "../log.js";
import type { SdkMessage } from "../sdk/types.js";

const LOGGER = bindLog({ component: "shim-engine-keepalive", operation: "shim.engine.keepalive" });

/** Every keep-alive prompt BEGINS with this literal. */
export const KEEPALIVE_PROMPT_MARKER = "<!--agent-repl:keepalive-->";

/** Four minutes: inside the vendor's five-minute ephemeral tier, with a margin. */
export const KEEPALIVE_INTERVAL_MS = 4 * 60 * 1000;

/**
 * The prompt itself.
 *
 * Minimal on purpose: it exists to make an API call that reads the cache back,
 * not to get an answer, so anything the model would have to think about is
 * wasted money.
 *
 * NUMBERED, AND WHY (ruled 2026-09-17). Successive keep-alives used to be
 * byte-for-byte identical. On 2026-09-17 a keep-alive turn on claude-opus-5
 * degenerated: instead of answering ".", the model echoed the prompt back into
 * a ~64,000-token block and hit max_tokens, and it did so on turn after turn.
 * A repeated-identical prompt is the pattern that reinforces such an echo loop,
 * so every keep-alive now carries an incrementing counter and no two are the
 * same. The counter trails the instruction, so the marker the store and
 * sidecar classify on is untouched and {@link isKeepalivePrompt} still matches.
 */
export function keepalivePromptText(counter: number): string {
  return `${KEEPALIVE_PROMPT_MARKER}\nRespond with the single character "." and nothing else. (${counter})`;
}

/** True when a prompt the shim is about to submit is one of its own keep-alives. */
export function isKeepalivePrompt(text: string): boolean {
  return text.startsWith(KEEPALIVE_PROMPT_MARKER);
}

/** What a rewind needs: the record to resume at, and why. */
export interface RewindObligation {
  /** The vendor record uuid the next query resumes THROUGH, inclusive. */
  readonly resumeSessionAt: string;
  /** The turn whose assistant record that uuid is — the anchor's provenance. */
  readonly anchorTurnId: string;
  /** How many keep-alive turns are being discarded. */
  readonly discardedKeepaliveTurns: number;
}

/** The turn a record arrived under, as the rewind needs to see it. */
export interface RecordTurn {
  /** The turn's id, kept so the anchor can say which turn it came from. */
  readonly turnId: string;
  /** True when the shim opened this turn for its own keep-alive. */
  readonly keepalive: boolean;
}

/**
 * The yield obligation's bookkeeping: one remembered uuid and one counter.
 *
 * Deliberately not a history. The rewind needs exactly one anchor — the last
 * record written while a REAL turn was in flight — and the count of keep-alive
 * turns since, which is what makes the obligation reportable.
 */
export class KeepaliveRewind {
  private anchor: { readonly uuid: string; readonly turnId: string } | undefined;
  private keepaliveTurns = 0;

  /**
   * A message arrived under `turn`; it becomes the anchor only if it qualifies.
   *
   * IT TAKES THE MESSAGE, NOT A UUID, on purpose: the one rule that matters —
   * only an `assistant` message of a real, open turn may anchor a rewind — is
   * then enforced here rather than trusted to every call site.
   */
  noteRecord(message: SdkMessage, turn: RecordTurn | undefined): void {
    // Only an assistant record. `system:init`, `result`, the user echo, stream
    // events, hooks and control messages all carry uuids the vendor will not
    // resume at.
    if (message.type !== "assistant") return;
    // Only a REAL turn, and only while one is open. A keep-alive's own answer
    // is exactly the material the rewind exists to discard, and a record with
    // no open turn belongs to no turn this shim asked for.
    if (turn === undefined || turn.keepalive) return;
    const uuid = (message as { uuid?: unknown }).uuid;
    if (typeof uuid !== "string" || uuid === "") return;
    this.anchor = { uuid, turnId: turn.turnId };
  }

  /**
   * A boundary the anchor cannot be trusted across: forget it.
   *
   * INFO, not debug: the next real prompt after keep-alives will then carry
   * them rather than rewind, and an operator reading the feed has to be able to
   * see why. Silent when there was no anchor — nothing happened.
   */
  clearAnchor(reason: string): void {
    const held = this.anchor;
    if (held === undefined) return;
    this.anchor = undefined;
    LOGGER.info(
      { reason, anchor_uuid: held.uuid, anchor_turn_id: held.turnId },
      "the keep-alive rewind anchor is CLEARED: a uuid from before this boundary may not be resumable",
    );
  }

  /** The anchor as it stands, for a caller that needs to name it. */
  anchorUuid(): string | undefined {
    return this.anchor?.uuid;
  }

  /** A keep-alive turn ended; the context now carries material a real prompt must not see. */
  noteKeepaliveTurn(): void {
    this.keepaliveTurns++;
  }

  /**
   * What the next REAL prompt owes, or absence when it owes nothing.
   *
   * Absence covers both ordinary cases: no keep-alive has run since the last
   * real record, or no real record exists yet (a first prompt on a fresh
   * session has nothing to roll back TO, and truncating to nothing would
   * discard the session's own opening).
   */
  obligation(): RewindObligation | undefined {
    if (this.keepaliveTurns === 0) return undefined;
    if (this.anchor === undefined) {
      // INFO: keep-alive material is about to be CARRIED into a real turn,
      // which is the thing the rewind exists to prevent. It is the right
      // answer -- there is nothing safe to resume at -- but it is never
      // invisible.
      LOGGER.info(
        { keepalive_turns: this.keepaliveTurns },
        "no rewind anchor exists (none taken yet, or cleared at a boundary): the next real prompt proceeds WITHOUT a rewind and carries the keep-alive turns",
      );
      return undefined;
    }
    return {
      resumeSessionAt: this.anchor.uuid,
      anchorTurnId: this.anchor.turnId,
      discardedKeepaliveTurns: this.keepaliveTurns,
    };
  }

  /** The rewind happened (or was found unnecessary); the debt is cleared. */
  settled(): void {
    this.keepaliveTurns = 0;
  }
}

/** How the cadence schedules itself; injected so a suite never waits on a clock. */
export interface KeepaliveScheduler {
  setInterval(handler: () => void, ms: number): unknown;
  clearInterval(handle: unknown): void;
}

/** The real one. */
export const REAL_SCHEDULER: KeepaliveScheduler = {
  setInterval: (handler, ms) => {
    const handle = setInterval(handler, ms);
    // A keep-alive must never be the reason a process stays alive: the listener
    // holds the shim open, and an unref'd timer keeps `--version`-style short
    // lives short.
    if (typeof (handle as { unref?: () => void }).unref === "function") {
      (handle as { unref: () => void }).unref();
    }
    return handle;
  },
  clearInterval: (handle) => clearInterval(handle as ReturnType<typeof setInterval>),
};

/**
 * The cadence.
 *
 * STARTED BEFORE `StartSession` RETURNS SUCCESS, so a session that is never
 * prompted still keeps its cache warm. PAUSED while a turn is in flight,
 * because a keep-alive submitted into an open turn would be a second submitter
 * — the one thing the one-submitter invariant forbids. STOPPED at kill.
 */
export class KeepaliveCadence {
  private handle: unknown;
  private paused = false;

  constructor(
    private readonly beat: () => void,
    private readonly intervalMs: number = KEEPALIVE_INTERVAL_MS,
    private readonly scheduler: KeepaliveScheduler = REAL_SCHEDULER,
  ) {}

  /** Begin beating. Idempotent: a second start does not double the cadence. */
  start(): void {
    if (this.handle !== undefined) return;
    this.handle = this.scheduler.setInterval(() => {
      if (this.paused) {
        LOGGER.logVerbose({ outcome: "skipped_turn_in_flight" }, "keep-alive beat skipped: a turn is in flight");
        return;
      }
      this.beat();
    }, this.intervalMs);
    LOGGER.debug({ interval_ms: this.intervalMs }, "keep-alive cadence started");
  }

  /** A turn is in flight; hold the beat. */
  pause(): void {
    this.paused = true;
  }

  /** The turn ended; beat again. */
  resume(): void {
    this.paused = false;
  }

  /** The session is ending. */
  stop(): void {
    if (this.handle === undefined) return;
    this.scheduler.clearInterval(this.handle);
    this.handle = undefined;
    LOGGER.debug({}, "keep-alive cadence stopped");
  }
}
