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
 */
import { bindLog } from "../log.js";

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
 */
export function keepalivePromptText(): string {
  return `${KEEPALIVE_PROMPT_MARKER}\nRespond with the single character "." and nothing else.`;
}

/** True when a prompt the shim is about to submit is one of its own keep-alives. */
export function isKeepalivePrompt(text: string): boolean {
  return text.startsWith(KEEPALIVE_PROMPT_MARKER);
}

/** What a rewind needs: the record to resume at, and why. */
interface RewindObligation {
  /** The vendor record uuid the next query resumes THROUGH, inclusive. */
  readonly resumeSessionAt: string;
  /** How many keep-alive turns are being discarded. */
  readonly discardedKeepaliveTurns: number;
}

/**
 * The yield obligation's bookkeeping: one remembered uuid and one counter.
 *
 * Deliberately not a history. The rewind needs exactly one anchor — the last
 * record written while a REAL turn was in flight — and the count of keep-alive
 * turns since, which is what makes the obligation reportable.
 */
export class KeepaliveRewind {
  private anchor: string | undefined;
  private keepaliveTurns = 0;

  /** A record arrived. `keepalive` says which kind of turn produced it. */
  noteRecord(uuid: string, keepalive: boolean): void {
    if (keepalive) return;
    this.anchor = uuid;
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
      LOGGER.warn(
        { keepalive_turns: this.keepaliveTurns },
        "keep-alive turns ran before any real record: no rewind anchor exists, so the next real prompt proceeds without a rewind",
      );
      return undefined;
    }
    return { resumeSessionAt: this.anchor, discardedKeepaliveTurns: this.keepaliveTurns };
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
