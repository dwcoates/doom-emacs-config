/**
 * HistoryPager — the webapp's use of the TWO POSITIONLESS history verbs.
 *
 * # The principle, from which every decision here follows
 *
 * THE CLIENT CANNOT NAME A POSITION. Not a seq, not an offset, not a cursor it
 * authored or holds. This module may ask for the FIRST page or the NEXT page
 * and nothing else, and neither request carries a position — the daemon owns
 * it, per reader per workspace.
 *
 * That is why nothing here caches a cursor, and why there is no fence: a fence
 * was daemon state the client echoed back so the daemon could check the client
 * against itself, which is the same category error as `from_seq`. A page in
 * flight across a generation change is handled by `request_id` correlation
 * (the store discards a page it is no longer awaiting), and a position the
 * daemon dropped surfaces as a REFUSED next page.
 *
 * # A refused next page is answered with a FIRST page
 *
 * Never a retry of the next page — the position it needs is gone and asking
 * again cannot bring it back — and never a full replay. `FirstPageCmd` resets
 * the daemon-held position and starts from the bottom, which is the whole
 * recovery story: cold open, reconnect, rotated seq space, dropped position.
 *
 * # The bounds are the resync's bounds, deliberately
 *
 * A page is served from a store read of a conversation that may hold a quarter
 * of a million events, so an unanswered request must not provoke another
 * forever (an observed command queue 5,069 deep). The same three bounds apply:
 * ONE request in flight, exponential jittered backoff on failure, and a
 * ceiling that reports itself. The backoff is IMPORTED from the resync rather
 * than re-implemented, because "how hard may this page press a daemon that is
 * not answering" is one policy and two copies of it would drift.
 */

import {
  RESYNC_FAILURE_CEILING,
  resyncBackoffMs,
  type ConnectResyncLogLevel,
} from "./connect-resync.js";

/** How this module reports itself; the app's client-log levels. */
export type HistoryPagerLogLevel = ConnectResyncLogLevel;

/** WHICH verb a request went out as. There is no third. */
export type HistoryPageVerb = "first" | "next";

/** What the pager needs from the world, injected so every rule is testable. */
export interface HistoryPagerOptions {
  /**
   * Send ONE history command, returning the request id it went out under.
   *
   * The promise resolves on the daemon's ACCEPTANCE ack and rejects on a
   * refusal. The PAGE itself does not arrive through it; it arrives as a
   * pushed frame carrying this request id, which `observePage` settles.
   *
   * Note what is NOT a parameter: any position. The verb and the workspace are
   * the whole request.
   */
  send: (verb: HistoryPageVerb, workspace: string) => { requestId: string; ack: Promise<void> };
  /**
   * The workspace to ask about, read at the DISPATCH edge — never captured
   * earlier — so a deferred request names the conversation this page is
   * actually reading. Empty means "not knowable yet", and the request defers.
   */
  workspace: () => string;
  log?: (level: HistoryPagerLogLevel, message: string) => void;
  /** Monotonic-enough clock for the backoff gate; injectable for tests. */
  now?: () => number;
  /** Jitter source in [0,1); injectable for tests. */
  random?: () => number;
  /** The failure ceiling was reached: this page stops asking and says so. */
  onGiveUp?: (consecutiveFailures: number, cause: string) => void;
}

/** The pager's own view of what it is doing, for the load-more affordance. */
export interface HistoryPagerView {
  loading: boolean;
  givenUp: boolean;
}

export class HistoryPager {
  /** A request is SENT and has neither acked nor failed. */
  private inFlight: string | null = null;
  /** WHICH verb the in-flight request went out as. */
  private inFlightVerb: HistoryPageVerb = "first";
  /** Consecutive terminal failures; reset by any acceptance. */
  private failures = 0;
  /** Wall-clock before which no request may be dispatched. */
  private nextAllowedAtMs = 0;
  /** The ceiling was reached: asking is suspended until told otherwise. */
  private givenUp = false;

  constructor(private readonly opts: HistoryPagerOptions) {}

  get view(): HistoryPagerView {
    return { loading: this.inFlight !== null, givenUp: this.givenUp };
  }

  /** Whether a sent request has yet to settle. */
  get isInFlight(): boolean {
    return this.inFlight !== null;
  }

  private now(): number {
    return (this.opts.now ?? Date.now)();
  }

  /**
   * A socket opened, or this view is starting over: forget everything the dead
   * connection accumulated. A request still marked in flight belonged to that
   * connection and can never settle.
   */
  reset(): void {
    this.inFlight = null;
    this.inFlightVerb = "first";
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    this.givenUp = false;
  }

  /** The user asked to try again: forget the failure history. */
  retryNow(): void {
    this.givenUp = false;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
  }

  /**
   * Ask for the MOST RECENT page and RESET the daemon-held position to it.
   *
   * The cold open, the reconnect, and the answer to a refused next page.
   */
  openFirst(): Promise<void> {
    return this.dispatch("first", "first_page");
  }

  /**
   * Ask for the page immediately OLDER than the last one served to this reader.
   *
   * It carries NO position: the daemon holds it.
   */
  next(): Promise<void> {
    return this.dispatch("next", "next_page");
  }

  /**
   * SINGLE-FLIGHT, BACKOFF, CEILING — the three bounds, in the order that
   * keeps the queue finite.
   */
  private dispatch(verb: HistoryPageVerb, reason: string): Promise<void> {
    if (this.givenUp) {
      this.opts.log?.("info", `history: not asking reason=${reason} decision=given_up`);
      return Promise.reject(
        new Error("history-pager: this page has stopped asking after repeated failures"),
      );
    }
    if (this.inFlight !== null) {
      // NOT coalesced. A history request names a specific place in the reader's
      // walk, and a later one asks a different question; dropping it is the
      // honest outcome — the user's second click on a button already working.
      this.opts.log?.(
        "info",
        `history: request already in flight reason=${reason} request_id=${this.inFlight} decision=drop`,
      );
      return Promise.reject(new Error("history-pager: a history request is already in flight"));
    }
    const now = this.now();
    if (now < this.nextAllowedAtMs) {
      this.opts.log?.(
        "info",
        `history: backing off reason=${reason} failures=${this.failures} ` +
          `retry_in_ms=${this.nextAllowedAtMs - now} decision=defer`,
      );
      return Promise.reject(new Error("history-pager: backing off after a failed history request"));
    }
    const workspace = this.opts.workspace();
    if (workspace === "") {
      this.opts.log?.("warn", `history: no live workspace to ask about yet reason=${reason} decision=defer`);
      return Promise.reject(new Error("history-pager: no live workspace to ask about yet"));
    }
    return this.send(verb, workspace, reason);
  }

  private send(verb: HistoryPageVerb, workspace: string, reason: string): Promise<void> {
    const { requestId, ack } = this.opts.send(verb, workspace);
    this.inFlight = requestId;
    this.inFlightVerb = verb;
    this.opts.log?.(
      "info",
      `history: requesting page reason=${reason} verb=${verb} ws=${workspace} ` +
        `request_id=${requestId} decision=dispatch`,
    );
    ack.then(
      () => this.settleAccepted(requestId),
      (err: unknown) => this.settleRejected(requestId, String(err)),
    );
    return ack;
  }

  /**
   * The daemon ACCEPTED the request. That is not the page — the page arrives as
   * a pushed frame — so the request stays in flight; what the acceptance
   * discharges is the FAILURE history.
   */
  private settleAccepted(requestId: string): void {
    if (this.inFlight !== requestId) return;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
  }

  private settleRejected(requestId: string, cause: string): void {
    if (this.inFlight !== requestId) return;
    this.settleFailed(requestId, cause);
  }

  /**
   * The daemon REFUSED the request, or the read behind an accepted one failed.
   *
   * A refused NEXT page means the daemon dropped this reader's position — the
   * generation changed under it — and the ONLY correct answer is a FIRST page.
   * Not a retry of the next page, whose position no longer exists, and not a
   * full replay.
   */
  observeRefusal(requestId: string, cause: string): boolean {
    if (this.inFlight !== requestId) return false;
    this.settleFailed(requestId, cause);
    return true;
  }

  private settleFailed(requestId: string, cause: string): void {
    const verb = this.inFlightVerb;
    this.inFlight = null;
    if (verb === "next") {
      // A DROPPED POSITION IS NOT A FAILURE TO BACK OFF FROM. It is the daemon
      // telling this reader where it now stands, and the recovery is a first
      // page — so the failure history is discharged rather than charged, which
      // is what keeps a legitimate generation change from spending the ceiling.
      this.failures = 0;
      this.nextAllowedAtMs = 0;
      this.opts.log?.(
        "warn",
        `history: next page REFUSED request_id=${requestId} cause=${cause}; the daemon dropped this ` +
          `reader's position, so answering with a FIRST page`,
      );
      void this.openFirst().catch(() => {});
      return;
    }
    this.failures += 1;
    if (this.failures >= RESYNC_FAILURE_CEILING) {
      this.givenUp = true;
      this.opts.log?.(
        "error",
        `history: giving up after ${this.failures} consecutive failures cause=${cause}`,
      );
      this.opts.onGiveUp?.(this.failures, cause);
      return;
    }
    const delay = resyncBackoffMs(this.failures, (this.opts.random ?? Math.random)());
    this.nextAllowedAtMs = this.now() + delay;
    this.opts.log?.(
      "warn",
      `history: failed failures=${this.failures} retry_in_ms=${delay} cause=${cause}`,
    );
  }

  /** A page ARRIVED and was adopted: the request it answers is settled. */
  observePage(requestId: string): boolean {
    if (this.inFlight !== requestId) return false;
    this.inFlight = null;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    return true;
  }
}
