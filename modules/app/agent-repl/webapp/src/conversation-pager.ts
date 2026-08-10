/**
 * ConversationPager — the webapp's "ask for a page of history" trigger.
 *
 * # What it replaces
 *
 * A cold mount used to ask for its conversation with `ResyncCmd{from_seq: 0}`,
 * because a page with nothing applied has a high-water mark of zero and that is
 * what the resync rule turns into. The daemon answered honestly: every store
 * event the session ever produced. The worst workspace observed cost 259,000
 * events and 186MB to draw a screen whose visible tail is about ten items.
 *
 * The cold open now asks a bounded question — the newest ~10 top-level items —
 * and the feed grows a load-more affordance at its top for the rest.
 *
 * # RESYNC IS NOT REPLACED, and this is the seam between them
 *
 * A resync from the applied high-water mark is still how this page catches up,
 * and `ConnectResync` still owns every trigger for it. What changed is only
 * where the FIRST mark comes from: a tail page carries `live_join_seq`, the
 * store adopts it as `lastSeq`, and the resync that follows asks from there.
 * Because the daemon's replay is INCLUSIVE of that mark, an item the session
 * produced between the page's mint and the subscribe is above it and is
 * replayed — the splice is gap-free BY CONSTRUCTION rather than by how fast the
 * page happened to be.
 *
 * # The bounds are the resync's bounds, deliberately
 *
 * The failure this end has already had once is a history request that goes
 * unanswered, provoking another, forever: an observed command queue 5,069 deep
 * with entries settling 420-550 SECONDS after they were sent
 * (connect-resync.ts). A page is served from a store read of a conversation
 * that may hold a quarter of a million events, so it is exactly that shape.
 * The same three bounds therefore apply, stated once here for pages as
 * `ConnectResync` states them once for resyncs: ONE request in flight at a
 * time, exponential jittered backoff on failure, and a ceiling that reports
 * itself instead of spinning where nobody can see it.
 *
 * Nothing here re-implements the backoff. It imports the resync's own
 * `resyncBackoffMs` and its constants, because "how hard may this page press a
 * daemon that is not answering" is one policy and two copies of it would drift.
 */

import {
  RESYNC_FAILURE_CEILING,
  resyncBackoffMs,
  type ConnectResyncLogLevel,
} from "./connect-resync.js";

/** How this module reports itself; the app's client-log levels. */
export type PagerLogLevel = ConnectResyncLogLevel;

/**
 * One page request, fully built at its DECISION edge.
 *
 * The fence is captured here rather than read when the transport sends, for
 * the reason `ResyncSnapshot` captures it: a delayed request that re-read
 * current state would silently rebind itself to a newer controller generation
 * and page a conversation nobody asked about.
 */
export interface PageRequest {
  workspace: string;
  /** "" = the tail anchor; otherwise the daemon's opaque continuation token. */
  cursor: string;
  /** 0 = the daemon's default. The daemon owns the ceiling and clamps to it. */
  limit: number;
  fence: string;
}

/** What the pager needs from the world, injected so every rule is testable. */
export interface ConversationPagerOptions {
  /**
   * Send one ConversationPageCmd, returning the request id it went out under.
   *
   * The promise resolves on the daemon's ACCEPTANCE ack — which the daemon
   * sends at enqueue, before the read runs — and rejects on a refusal. The
   * PAGE itself does not arrive through it; it arrives as a pushed frame
   * carrying this request id, and `onPage` is what observes that.
   */
  send: (request: PageRequest) => { requestId: string; ack: Promise<void> };
  /**
   * Build the request this pager would send RIGHT NOW for the given cursor,
   * or null when this page cannot name a workspace and fence yet.
   *
   * Injected rather than computed here because the live identity lives in the
   * store, and reading it at the dispatch edge is the only way a request
   * cannot carry a fence the store has already replaced.
   */
  request: (cursor: string) => PageRequest | null;
  log?: (level: PagerLogLevel, message: string) => void;
  /** Monotonic-enough clock for the backoff gate; injectable for tests. */
  now?: () => number;
  /** Jitter source in [0,1); injectable for tests. */
  random?: () => number;
  /**
   * The failure ceiling was reached: the page stops asking and says so, rather
   * than leaving a load-more button that silently does nothing.
   */
  onGiveUp?: (consecutiveFailures: number, cause: string) => void;
}

/**
 * The pager's own view of what it is doing, for the load-more affordance.
 *
 * `canLoadMore` is deliberately not "there is a cursor": a request already in
 * flight, a spent failure ceiling, and a retired start all mean the button
 * must not be pressable, and folding them here is what keeps three separate
 * conditions out of the renderer.
 */
export interface PagerView {
  loading: boolean;
  givenUp: boolean;
}

export class ConversationPager {
  /** A page request is SENT and has neither acked nor failed. */
  private inFlight: string | null = null;
  /** Consecutive terminal failures; reset by any acceptance. */
  private failures = 0;
  /** Wall-clock before which no page request may be dispatched. */
  private nextAllowedAtMs = 0;
  /** The ceiling was reached: asking is suspended until told otherwise. */
  private givenUp = false;
  /**
   * The anchor the in-flight request asked for.
   *
   * Held because a re-request after a stale-fence discard must ask for the
   * SAME page. Re-deriving it from the discarded page is impossible — the page
   * carries no anchor — and defaulting to the tail would silently turn a
   * load-more into a cold open, which is the quiet wrong answer this whole
   * design refuses everywhere else.
   */
  private inFlightCursor = "";
  /** Whether the outstanding request is already the one retry. */
  private retrying = false;

  constructor(private readonly opts: ConversationPagerOptions) {}

  get view(): PagerView {
    return { loading: this.inFlight !== null, givenUp: this.givenUp };
  }

  /** Whether a sent page request has yet to settle. */
  get isInFlight(): boolean {
    return this.inFlight !== null;
  }

  private now(): number {
    return (this.opts.now ?? Date.now)();
  }

  /**
   * A socket opened, or this view is starting over: forget everything the dead
   * connection accumulated.
   *
   * A request still marked in flight belonged to that connection and can never
   * settle, so it is dropped rather than left blocking this one's cold open
   * forever — the same discharge `ConnectResync.onConnect` performs, for the
   * same reason.
   */
  reset(): void {
    this.inFlight = null;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    this.givenUp = false;
    this.inFlightCursor = "";
    this.retrying = false;
  }

  /**
   * The user asked to try again: forget the failure history and allow one
   * request immediately.
   */
  retryNow(): void {
    this.givenUp = false;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
  }

  /**
   * Ask for the COLD OPEN page: the newest items, with no cursor.
   *
   * It resolves on the daemon's ACCEPTANCE and rejects when the request was
   * not sent at all — a bound refused it, or this page cannot name a workspace
   * and fence yet. A rejection is what lets a caller that owns its own retry
   * discipline (`ConnectResync`, which drives the cold open) charge the
   * failure rather than believe a request went out.
   */
  openTail(limit = 0): Promise<void> {
    return this.dispatch("", limit, "cold_open");
  }

  /**
   * Ask for the page immediately older than the one this feed's top holds.
   *
   * `cursor` is the daemon's own opaque token from the last page's `more` arm,
   * passed back byte-for-byte. This never parses it.
   */
  loadMore(cursor: string, limit = 0): Promise<void> {
    if (cursor === "") {
      const cause = "load-more asked for with no cursor, which is a tail read wearing the wrong name";
      this.opts.log?.("warn", `page: ${cause}; refused`);
      return Promise.reject(new Error(`conversation-pager: ${cause}`));
    }
    return this.dispatch(cursor, limit, "load_more");
  }

  /**
   * SINGLE-FLIGHT, BACKOFF, CEILING — the three bounds, in the order that
   * keeps the queue finite.
   */
  private dispatch(cursor: string, limit: number, reason: string): Promise<void> {
    if (this.givenUp) {
      this.opts.log?.("info", `page: not asking reason=${reason} decision=given_up`);
      return Promise.reject(new Error("conversation-pager: this page has stopped asking after repeated failures"));
    }
    if (this.inFlight !== null) {
      // NOT coalesced into a dirty flag, unlike a resync. A resync means "make
      // me current as of now", so a newer one answers an older one; a page
      // request names a SPECIFIC place in the history and a later one answers
      // a different question. Dropping it is the honest outcome — the user's
      // second click on a button that is already working.
      this.opts.log?.(
        "info",
        `page: request already in flight reason=${reason} request_id=${this.inFlight} decision=drop`,
      );
      return Promise.reject(new Error("conversation-pager: a page request is already in flight"));
    }
    const now = this.now();
    if (now < this.nextAllowedAtMs) {
      this.opts.log?.(
        "info",
        `page: backing off reason=${reason} failures=${this.failures} retry_in_ms=${this.nextAllowedAtMs - now} decision=defer`,
      );
      return Promise.reject(new Error("conversation-pager: backing off after a failed page request"));
    }
    const request = this.opts.request(cursor);
    if (request === null || request.workspace === "") {
      this.opts.log?.(
        "warn",
        `page: no live workspace and fence to ask with yet reason=${reason} decision=defer`,
      );
      return Promise.reject(new Error("conversation-pager: no live workspace and fence to ask with yet"));
    }
    return this.send({ ...request, limit }, reason, false);
  }

  private send(request: PageRequest, reason: string, isRetry: boolean): Promise<void> {
    const { requestId, ack } = this.opts.send(request);
    this.inFlight = requestId;
    this.inFlightCursor = request.cursor;
    this.retrying = isRetry;
    this.opts.log?.(
      "info",
      `page: requesting history reason=${reason} ws=${request.workspace} ` +
        `anchor=${request.cursor === "" ? "tail" : "before"} limit=${request.limit} ` +
        `fence=${request.fence || "none"} request_id=${requestId} decision=dispatch`,
    );
    ack.then(
      () => this.settleAccepted(requestId),
      (err: unknown) => this.settleRejected(requestId, String(err)),
    );
    return ack;
  }

  /**
   * The daemon ACCEPTED the request onto the workspace's lane.
   *
   * It is not the page — the page arrives as a pushed frame — so the request
   * stays in flight here. What the acceptance discharges is the FAILURE
   * history: a daemon that took this request is a daemon that is answering.
   */
  private settleAccepted(requestId: string): void {
    if (this.inFlight !== requestId) return;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
  }

  private settleRejected(requestId: string, cause: string): void {
    if (this.inFlight !== requestId) return;
    this.settleFailed(cause);
  }

  /**
   * The daemon refused the request, or the read behind an accepted one failed
   * (the daemon answers that with a second, failing ack under the same request
   * id).
   *
   * Both are the same fact for this end: no page is coming for this request,
   * and continuing to ask at full speed is what the bound exists to prevent.
   */
  observeRefusal(requestId: string, cause: string): boolean {
    if (this.inFlight !== requestId) return false;
    this.settleFailed(cause);
    return true;
  }

  private settleFailed(cause: string): void {
    this.inFlight = null;
    this.retrying = false;
    this.failures += 1;
    if (this.failures >= RESYNC_FAILURE_CEILING) {
      this.givenUp = true;
      this.opts.log?.(
        "error",
        `page: giving up after ${this.failures} consecutive failures cause=${cause}`,
      );
      this.opts.onGiveUp?.(this.failures, cause);
      return;
    }
    const delay = resyncBackoffMs(this.failures, (this.opts.random ?? Math.random)());
    this.nextAllowedAtMs = this.now() + delay;
    this.opts.log?.(
      "warn",
      `page: failed failures=${this.failures} retry_in_ms=${delay} cause=${cause}`,
    );
  }

  /**
   * A page ARRIVED and was adopted: the request it answers is settled.
   */
  observePage(requestId: string): boolean {
    if (this.inFlight !== requestId) return false;
    this.inFlight = null;
    this.retrying = false;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    return true;
  }

  /**
   * A page was DISCARDED WHOLE for a stale fence: re-request it ONCE against
   * the fence this page now holds.
   *
   * EXACTLY ONCE, and the reason is the resync's: a re-request that is itself
   * answered stale means this end cannot name a current fence from what it
   * has, and a second attempt would be the same guess made twice. It stops and
   * says so instead, leaving the next state push to supply the identity.
   *
   * The re-request asks for the SAME anchor the discarded one did, which is
   * why {@link inFlightCursor} is held rather than re-derived: a page carries
   * no anchor, and defaulting to the tail would turn a load-more into a cold
   * open without saying so.
   */
  observeStaleFence(requestId: string): boolean {
    const cursor = this.inFlightCursor;
    if (this.inFlight === requestId) this.inFlight = null;
    if (this.retrying) {
      this.retrying = false;
      this.opts.log?.(
        "warn",
        `page: the re-requested page was ALSO stale request_id=${requestId}; not asking again ` +
          `until a fresh workspace state names a fence this end can use`,
      );
      return false;
    }
    const request = this.opts.request(cursor);
    if (request === null || request.workspace === "") {
      this.opts.log?.(
        "warn",
        `page: a stale-fence page was discarded and there is no live fence to re-ask with request_id=${requestId}`,
      );
      return false;
    }
    this.opts.log?.(
      "warn",
      `page: page discarded for a stale fence request_id=${requestId}; re-requesting once ` +
        `against fence=${request.fence || "none"}`,
    );
    // The re-request's own acceptance is settled through the same handlers the
    // first attempt used; nothing else is waiting on this promise.
    void this.send({ ...request, limit: 0 }, "stale_fence_retry", true).catch(() => {});
    return true;
  }
}
