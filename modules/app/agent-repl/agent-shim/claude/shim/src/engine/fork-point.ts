/**
 * engine/fork-point.ts — where a fork of this shim's conversation would end.
 *
 * A FORK CARRIES ONLY FINISHED TURNS (owner ruling, 2026-10-11): a fork taken
 * while a turn is in flight ends just before that turn began. This shim is the
 * only process that has seen every main-thread record of every turn in order —
 * a turn the vendor started on its own included — so it names the point, and
 * the daemon relays it to the child's ForkSession unchanged
 * (proto/src/shim/v1/fork_point.proto).
 *
 * THE POINT IS THE FINAL RECORD OF THE NEWEST CONCLUDED TURN whenever this
 * process concluded one: everything the parent does after that record — a turn
 * in flight, a turn the vendor starts during the fork — lies outside the fork by
 * construction. Only a shim that has concluded nothing since it started falls
 * back to the opener of the turn in flight, and only an idle one with nothing
 * concluded forks the whole transcript.
 *
 * A ROTATION FORGETS EVERYTHING: a /clear moves the conversation to a new
 * transcript file, and a record of the old one names nothing in the new.
 */
import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-engine-fork-point", operation: "shim.engine.fork_point" });

/** A turn open at the moment a fork point is asked for. */
export interface OpenForkTurn {
  /** The turn's id. */
  readonly turnId: string;
  /** When the turn opened, epoch ms. */
  readonly startedAtMs: number;
  /**
   * The vendor record that opened it, when this shim knows it: a send's
   * prompt uuid, or the first record of a turn the vendor started.
   */
  readonly openerUuid: string | undefined;
}

/** The answer GetForkPoint is built from. THE KIND IS THE EXTENT. */
export type ForkPointAnswer =
  | {
      readonly kind: "whole";
      readonly throughMs: number;
      readonly uninherited: readonly string[];
    }
  | {
      readonly kind: "through_record";
      readonly recordUuid: string;
      readonly throughMs: number;
      readonly uninherited: readonly string[];
    }
  | {
      readonly kind: "before_record";
      readonly recordUuid: string;
      readonly throughMs: number;
      readonly uninherited: readonly string[];
    }
  | {
      readonly kind: "unresolved";
      readonly detail: string;
    };

/** The newest concluded turn whose final record this process saw. */
interface Concluded {
  readonly turnId: string;
  readonly recordUuid: string;
  readonly atMs: number;
}

/**
 * The bookkeeping behind GetForkPoint: the first and last main-thread record of
 * every real turn still open, and the newest concluded turn's final record.
 *
 * Deliberately not a history. Only the newest conclusion is ever asked for, so
 * a turn's records are dropped the moment it concludes.
 */
export class ForkPointTracker {
  private readonly records = new Map<string, { first: string; last: string }>();
  private concluded: Concluded | undefined;

  /**
   * A main-thread conversation record of a REAL turn: a send's prompt, or a
   * vendor record attributed to the turn. A keep-alive's records never come
   * here: they are housekeeping a fork must not end on.
   */
  noteRecord(turnId: string, uuid: string): void {
    if (turnId === "" || uuid === "") return;
    const held = this.records.get(turnId);
    if (held === undefined) {
      this.records.set(turnId, { first: uuid, last: uuid });
      return;
    }
    held.last = uuid;
  }

  /**
   * A real turn concluded at `atMs`. Its last record becomes the fork point. A
   * turn that wrote no record leaves the previous point standing, which is
   * still the final record of a concluded turn, but moves the instant forward
   * so the turn's own stored rows (its prompt row, its terminal) are inherited.
   */
  noteConcluded(turnId: string, atMs: number): void {
    const held = this.records.get(turnId);
    this.records.delete(turnId);
    if (held !== undefined) {
      this.concluded = { turnId, recordUuid: held.last, atMs };
      return;
    }
    if (this.concluded !== undefined) {
      this.concluded = { ...this.concluded, turnId, atMs };
      return;
    }
    LOGGER.debug(
      { turn_id: turnId, at_ms: atMs },
      "a turn concluded with no main-thread record and none concluded before it; no fork point is named yet",
    );
  }

  /** The first record a turn wrote, when it wrote any. */
  firstRecord(turnId: string): string | undefined {
    return this.records.get(turnId)?.first;
  }

  /** A /clear moved the conversation to a new transcript: nothing named before it is in the new one. */
  reset(reason: string): void {
    if (this.concluded === undefined && this.records.size === 0) return;
    LOGGER.info(
      { reason, forgotten_point: this.concluded?.recordUuid ?? "", open_turns: this.records.size },
      "the fork point is forgotten: the conversation moved to a new transcript",
    );
    this.concluded = undefined;
    this.records.clear();
  }

  /**
   * The point a fork taken now ends at.
   *
   * `open` is every real turn open now, in no meaningful order; `nowMs` bounds
   * the inherited rows of a whole-transcript fork.
   */
  point(open: readonly OpenForkTurn[], nowMs: number): ForkPointAnswer {
    const uninherited = open.map((turn) => turn.turnId);
    if (this.concluded !== undefined) {
      return {
        kind: "through_record",
        recordUuid: this.concluded.recordUuid,
        throughMs: this.concluded.atMs,
        uninherited,
      };
    }
    if (open.length === 0) return { kind: "whole", throughMs: nowMs, uninherited };
    const earliest = open.reduce((a, b) => (b.startedAtMs < a.startedAtMs ? b : a));
    if (earliest.openerUuid === undefined || earliest.openerUuid === "") {
      return {
        kind: "unresolved",
        detail: `turn ${earliest.turnId} is in flight, no turn concluded since this shim started, and the record that opened it is unknown`,
      };
    }
    return { kind: "before_record", recordUuid: earliest.openerUuid, throughMs: earliest.startedAtMs, uninherited };
  }
}
