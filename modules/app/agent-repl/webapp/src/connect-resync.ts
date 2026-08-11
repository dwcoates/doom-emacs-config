/**
 * ConnectResync — the webapp's "ask for the conversation history" trigger.
 *
 * `StateSnapshot` deliberately carries no conversation (it is workspaces,
 * sessions, inits, queues, progress). Conversation reaches a frontend ONLY as
 * pushed `ConversationDelta`s, which are fire-and-forget: whatever the daemon
 * pushed before this page existed is gone. A freshly-mounted GUI therefore
 * starts EMPTY even when the daemon has a full backfilled history retained —
 * the observed failure was a ~1,009-event backfill burst (seq 6108→7117)
 * landing while the xwidget was still mounting, with no subscriber to receive
 * it.
 *
 * `ResyncCmd` is the daemon's answer and nothing sent it. This is what sends
 * it: on every (re)connect, once the connect snapshot has landed, one
 * `resync(workspace, lastSeq)` per active workspace. The daemon replays its
 * retained ring (and, below the ring floor, re-pulls from the store) as
 * ordinary `ConversationDelta`s, which the store's per-uuid reconciliation
 * absorbs — a re-pushed item REPLACES rather than duplicates.
 *
 * # Fire ONCE per connection (this is a hard requirement, not a nicety)
 *
 * The daemon answers a `ResyncCmd` by re-sending a fresh `StateSnapshot` to the
 * requesting client (`frontend/server.go` readLoop). So "resync on every
 * snapshot" is an infinite loop: snapshot → resync → snapshot → … The trigger
 * arms on connect, fires at most once, and does not re-arm until the socket
 * drops. A snapshot that arrives because WE asked for it therefore fires
 * nothing.
 *
 * # And it is BOUNDED, because the triggers no longer are
 *
 * Recovery became visibility-independent (background-recovery.ts), which is
 * right — a hidden page must repair itself — but it removed the accidental
 * bound WebKit's throttling used to supply. A heartbeat that re-arms a resync
 * every few seconds, against a daemon that is slow to answer, produced a
 * command queue 5,069 deep whose entries settled 420-550 SECONDS after they
 * were sent, each unanswered one provoking the next. The bound is stated here
 * instead, in the one place every resync passes through: ONE in flight at a
 * time (a want-resync arriving mid-flight becomes a dirty flag spent on
 * settle), exponential jittered backoff on failure, and a ceiling that raises
 * the connection banner rather than spinning where nobody can see it. A fresh
 * socket discharges all three.
 *
 * # Why it waits for a workspace
 *
 * `Manager.Resync` looks the workspace up by exact key (the session cwd); an
 * empty one is a loud nack, not a defaulted match. A very fresh mount can
 * receive its first snapshot before any `SessionView` has taught the store a
 * cwd, so the trigger stays armed across frames and fires on the first one
 * that has both: a snapshot seen AND a workspace known.
 *
 * Emacs deliberately has no counterpart. It renders no conversation, so it has
 * no history to recover.
 */

// The snapshot LEASE is the clock the settle deadline below must fit inside,
// so the deadline is derived from it rather than from a second number that
// would drift out of relation with it. `ws.ts` imports nothing from here, so
// this direction carries no cycle.
import { DEFAULT_SNAPSHOT_TIMEOUT_MS } from "./ws.js";

/** How this module reports itself; mirrors the app's client-log levels. */
export type ConnectResyncLogLevel = "info" | "warn" | "error";

/** The complete revisioned WorkspaceState snapshot a replay is conditional on. */
export interface ResyncSnapshot {
  workspace: string;
  fromSeq: number;
  fence: string;
}

/**
 * First retry delay after an unacked or refused resync.
 *
 * THE FLOOD THIS BOUNDS: the recovery heartbeat is visibility-independent, so
 * a background page re-arms a resync every few seconds whether or not the last
 * one was ever answered. With the daemon slow, each tick added another
 * `ResyncCmd` to a queue that only grew — an observed depth of 5,069 with
 * resyncs settling 420-550 SECONDS after they were sent, and every unacked one
 * provoking the next. Bounding is the fix; re-gating on visibility is not,
 * because a hidden page still has to recover.
 */
export const RESYNC_BACKOFF_BASE_MS = 2_000;

/** Ceiling on the retry delay: a page far behind still rejoins within a minute. */
export const RESYNC_BACKOFF_MAX_MS = 60_000;

/**
 * How long ONE in-flight resync may go unsettled before it is discharged as a
 * failure.
 *
 * THE WEDGE THIS ENDS, observed live (workspace marcos-pr-remediation, daemon
 * pid 36279): a resync was dispatched over a healthy socket and its ack never
 * came back. `inFlight` is cleared only by a settle or by a socket event, so
 * with the socket staying up the flag stayed true forever; every subsequent
 * want-resync — the recovery heartbeat fires one every few seconds — coalesced
 * into a settle that was never going to happen. No resync went out, so no
 * `StateSnapshot` came back, so the ws snapshot lease renewed by adoption
 * (ws.ts) expired twice with nothing adopted between, and version-skew.ts read
 * that as a stale bundle and reloaded the page. The reloaded page repeated it.
 * The single-in-flight rule is right; a flight with no deadline is not.
 *
 * DERIVED from the lease it must fit inside, never restated as a literal. The
 * discharge is worth nothing unless the retry it re-arms can still get a
 * snapshot adopted before the lease expires, so the deadline plus one backoff
 * plus a round trip has to fit in `DEFAULT_SNAPSHOT_TIMEOUT_MS`. A third of the
 * lease leaves two thirds for exactly that.
 */
export const RESYNC_SETTLE_DEADLINE_MS = Math.floor(DEFAULT_SNAPSHOT_TIMEOUT_MS / 3);

/** Minimal timer surface, injectable so tests drive the deadline themselves. */
export interface ResyncTimerHost {
  setTimeout: (callback: () => void, ms: number) => number;
  clearTimeout: (handle: number) => void;
}

/** The ambient timers, used when a caller injects none. */
const AMBIENT_TIMERS: ResyncTimerHost = {
  setTimeout: (callback, ms) => setTimeout(callback, ms) as unknown as number,
  clearTimeout: (handle) => clearTimeout(handle),
};

/**
 * Consecutive failures after which this page stops asking and SAYS SO.
 *
 * Silent spinning is what made the flood invisible. At the ceiling the
 * connection-lost banner goes back up with a retry affordance, so the state is
 * a thing the user can see and act on rather than a queue nobody is watching.
 */
export const RESYNC_FAILURE_CEILING = 8;

/**
 * The delay owed before the nth consecutive retry: exponential from the base,
 * capped, then jittered down by up to half so a fleet of pages that lost the
 * same daemon does not re-ask in lockstep.
 */
export function resyncBackoffMs(consecutiveFailures: number, random: number): number {
  const exponential = RESYNC_BACKOFF_BASE_MS * Math.pow(2, Math.max(0, consecutiveFailures - 1));
  const capped = Math.min(exponential, RESYNC_BACKOFF_MAX_MS);
  return Math.round(capped * (0.5 + 0.5 * random));
}

export interface ConnectResyncOptions {
  /**
   * Sends one ResyncCmd. Resolves when the daemon ACKED it and rejects when it
   * nacked it — this promise is the settle signal the single-in-flight rule is
   * built on, so a dispatcher that resolves on ENQUEUE rather than on ack would
   * defeat the bound.
   */
  resync: (snapshot: ResyncSnapshot) => Promise<void>;
  log?: (level: ConnectResyncLogLevel, message: string) => void;
  /** Monotonic-enough clock for the backoff gate; injectable for tests. */
  now?: () => number;
  /** Jitter source in [0,1); injectable for tests. */
  random?: () => number;
  /**
   * The failure ceiling was reached: surface the connection-lost banner. The
   * page asks nothing more until `retryNow()` or a fresh socket says otherwise.
   */
  onGiveUp?: (consecutiveFailures: number, cause: string) => void;
  /**
   * The request a coalesced want-resync should carry when the in-flight one
   * settles. Read at the settle edge rather than remembered from the dispatch,
   * so the catch-up asks from the mark this page has actually applied by then.
   */
  latestSnapshot?: () => ResyncSnapshot | null;
  /**
   * Re-read the LIVE identity after the daemon refused a resync for naming a
   * superseded one, returning the request to retry with — or null when this
   * page cannot name a live identity yet.
   *
   * Injected rather than computed here because the live identity lives in the
   * store (the workspace's current fence and owning session), and reading it
   * at the retry edge is the only way the retry cannot carry the same stale
   * generation the refusal just rejected.
   */
  adoptIdentity?: (rejection: string) => ResyncSnapshot | null;
  /**
   * The daemon refused this page's replay mark as belonging to a RETIRED store
   * seq space: discard the conversation ranked in that space and re-anchor from
   * a fresh tail page. Returns whether the re-anchor was actually started.
   *
   * Injected rather than done here because a re-anchor is two things this
   * module does not own — dropping the store's items and mark, and asking the
   * PAGER for a tail — and doing either from here would give the page a second
   * route to its own history.
   *
   * A false return means the page could not re-anchor (no workspace yet, the
   * pager is spent), and the refusal is charged to the backoff like any other:
   * the alternative is a page that silently stops asking.
   */
  reanchor?: (rejection: string) => boolean;
  /**
   * Timers the settle deadline runs on. Injected so tests advance the clock
   * themselves rather than waiting out a real lease.
   */
  timers?: ResyncTimerHost;
  /** Deadline for one in-flight resync; defaults to RESYNC_SETTLE_DEADLINE_MS. */
  settleDeadlineMs?: number;
}

/**
 * Whether a refusal is the daemon saying "the identity you named is not the
 * live one" rather than an ordinary failure.
 *
 * The daemon's prose is `command superseded by the current workspace
 * generation ... rejection_cause=identity_mismatch`. The CAUSE token is the
 * stable half of that sentence, so it is what this matches.
 */
export function isIdentityMismatch(rejection: string): boolean {
  return rejection.includes("identity_mismatch");
}

/**
 * Whether a refusal is the daemon saying "the mark you asked from counts in a
 * store seq space that no longer exists".
 *
 * The daemon's prose is `the replay mark counts in a RETIRED store seq space
 * ... rejection_cause=retired_seq_space`, and the CAUSE token is the stable
 * half, exactly as it is for `isIdentityMismatch`.
 *
 * WHAT IT MEANS IS NOT "TRY AGAIN". A vendor session uuid rotation restarts the
 * conversation's store seq space at 1, so this page's mark, its ranks, and the
 * items it holds all describe a conversation that is gone. No retry of the same
 * request can be answered, and the answer the daemon WOULD have given before —
 * the whole conversation, floored to zero — is the full replay paging exists to
 * end. The page re-anchors from a tail page instead, and REPLACES rather than
 * appends.
 */
export function isRetiredReplayMark(rejection: string): boolean {
  return rejection.includes("retired_seq_space");
}

/**
 * How many re-anchors one connection may take before the refusal is treated as
 * an ordinary failure.
 *
 * A re-anchor cannot loop by construction — it drops this page's mark to zero,
 * and a resync is never sent from zero — but "cannot loop by construction" is
 * exactly what the fence-mismatch resync loop was also believed to be. The
 * ceiling is the cheap proof: a page that has re-anchored three times on one
 * connection is not converging, and charging the fourth refusal to the backoff
 * puts it in front of a human instead of leaving it to spin.
 */
export const RESYNC_REANCHOR_CEILING = 3;

export class ConnectResync {
  /** A live socket that still owes its resync. */
  private armed = false;
  /** Whether this connection's `StateSnapshot` has landed yet. */
  private snapshotSeen = false;
  /**
   * The daemon boot id this page's applied store belongs to; null before the
   * first snapshot it ever adopted.
   *
   * @see observeDaemonIdentity for why the page holds one at all.
   */
  private daemonBootId: string | null = null;
  /**
   * A resync is SENT and has neither acked nor failed. Exactly one may be in
   * flight at a time: the daemon answers a resync with a full StateSnapshot,
   * so a second one asks for work the first is already doing.
   */
  private inFlight = false;
  /**
   * Identifies the CURRENT flight, so a settle can be attributed to the flight
   * that produced it.
   *
   * A resync discharged by its deadline (or by a socket event) may still have
   * its promise resolve or reject later — nothing cancels a dispatch already
   * on the wire. Applying that late settle would clear an `inFlight` that
   * belongs to a DIFFERENT request, or charge a failure to it, so the token is
   * compared first and a stale settle is counted and dropped instead.
   */
  private flightToken = 0;
  /** Handle of the current flight's deadline timer, or null when none is armed. */
  private deadlineTimer: number | null = null;
  /** Settles that arrived for an already-discharged flight; reported once. */
  private lateSettles = 0;
  /** A want-resync arrived while one was in flight or while backing off. */
  private dirty = false;
  /**
   * How many want-resyncs the current suppression run has absorbed — whether
   * they were absorbed by an in-flight request or deferred by a backoff — and
   * the workspace they named.
   *
   * THIS COUNTER IS THE LOG RATE LIMIT. Every suppression used to write its own
   * line, and the suppressions are not rare: a want-resync fires on essentially
   * every ingested frame, so one boot window produced ~28,000 identical
   * `already in flight` lines — hundreds per millisecond — which drowned the
   * console AND flooded the client_log telemetry the daemon persists, making
   * the very stall they accompany harder to read. The information content of
   * the run is one fact plus one number: the first suppression says coalescing
   * began, the count says how big it got, and every line between them says
   * nothing the two do not.
   */
  private suppressed = 0;
  private suppressedWorkspace = "";
  /** Consecutive terminal failures; reset by any ack and by a fresh socket. */
  private failures = 0;
  /**
   * Re-anchors this connection has taken (RESYNC_REANCHOR_CEILING bounds them).
   *
   * Counted per SOCKET rather than per settle, and deliberately NOT reset by an
   * ack: the bound must survive the tail page that follows a re-anchor, or a
   * page alternating re-anchor and ack forever would never reach it. A fresh
   * socket clears it, because a new connection is new evidence.
   */
  private reanchors = 0;
  /** Wall-clock before which no resync may be dispatched. */
  private nextAllowedAtMs = 0;
  /** The ceiling was reached: asking is suspended until told otherwise. */
  private givenUp = false;

  constructor(private readonly opts: ConnectResyncOptions) {}

  /** Whether this page has stopped asking and raised the banner. */
  get isGivenUp(): boolean {
    return this.givenUp;
  }

  /** Whether a sent resync has yet to ack or fail. */
  get isInFlight(): boolean {
    return this.inFlight;
  }

  /** Settles that arrived after their flight was discharged by its deadline. */
  get lateSettleCount(): number {
    return this.lateSettles;
  }

  /**
   * Record one suppressed want-resync, writing a line only for the FIRST of a
   * run. The rest are counted and reported in one summary when the in-flight
   * request settles (flushSuppressed).
   */
  private noteSuppressed(workspace: string, first: string): void {
    this.suppressed += 1;
    this.suppressedWorkspace = workspace;
    if (this.suppressed > 1) return;
    this.opts.log?.("info", first);
  }

  /**
   * Close out a suppression run with its total. Silent when nothing was
   * suppressed, so a clean request adds no line at all, and it never counts a
   * run twice: the counter is cleared as it is reported.
   */
  private flushSuppressed(outcome: string): void {
    if (this.suppressed === 0) return;
    const suppressed = this.suppressed;
    const workspace = this.suppressedWorkspace;
    this.suppressed = 0;
    this.suppressedWorkspace = "";
    this.opts.log?.(
      "info",
      `resync: coalesced ${suppressed} want-resync(s) ws=${workspace} outcome=${outcome} ` +
        `decision=summary`,
    );
  }

  private now(): number {
    return (this.opts.now ?? Date.now)();
  }

  /**
   * The user asked to try again from the banner: forget the failure history
   * and owe one resync immediately.
   */
  retryNow(): void {
    this.flushSuppressed("retry_requested");
    this.givenUp = false;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    this.rearm();
    this.opts.log?.("info", "resync: retry requested from the connection banner");
  }

  /** The daemon boot id this page is currently synchronized against. */
  get bootId(): string | null {
    return this.daemonBootId;
  }

  /**
   * Rule on the daemon identity a just-adopted `StateSnapshot` announced.
   *
   * THE ZOMBIE PAGE IS WHAT THIS ENDS. Every trigger for a resync used to be a
   * SOCKET event — a connect arms one, a disconnect disarms it — so a page
   * whose socket never cycled was never told to catch up. A daemon can be
   * replaced under a page whose transport looks fine: the webview is hidden and
   * WebKit has frozen its timers, the close never gets dispatched, or the
   * reconnect lands and its snapshot arrives after this connection's one resync
   * has already fired. The page then holds a store belonging to a daemon that
   * no longer exists, and there is nothing left in its own lifecycle that will
   * ever ask again. Killing the webview and reopening it was the only recovery.
   *
   * A CHANGED BOOT ID IS THE PROOF, and it is proof of a fact no socket event
   * carries: this is a DIFFERENT daemon process from the one the applied store
   * was built against, whatever the transport did. So the page ADOPTS the live
   * identity and re-arms, and the ordinary observe() below sends one resync
   * from the applied high-water mark — the delta, never a full replay, because
   * a bounce that changed no build left every applied item still valid
   * (version-skew.ts makes the same distinction for the bundle).
   *
   * IT CANNOT LOOP. The identity is adopted BEFORE the re-arm, so the fresh
   * snapshot the resync provokes carries the id this page now holds and is
   * an ordinary match. Only a THIRD daemon can re-arm it again, which is
   * another real bounce and owed another resync.
   *
   * An empty boot id is an invariant violation rather than a tolerated
   * absence: the daemon stamps a DaemonView on every connect snapshot, so a
   * missing one means no identity decision can be taken from this frame.
   */
  observeDaemonIdentity(bootId: string): boolean {
    if (bootId === "") {
      this.opts.log?.(
        "error",
        "resync: adopted snapshot carried no daemon boot id, which the daemon stamps on every connect snapshot",
      );
      throw new Error("connect-resync: adopted snapshot carried an empty daemon boot id");
    }
    if (this.daemonBootId === null) {
      this.daemonBootId = bootId;
      this.opts.log?.("info", `resync: pinned daemon boot id ${bootId}`);
      return false;
    }
    if (this.daemonBootId === bootId) return false;
    const previous = this.daemonBootId;
    this.daemonBootId = bootId;
    this.rearm();
    this.opts.log?.(
      "warn",
      `resync: daemon identity changed ${previous} -> ${bootId}; adopting it and re-arming a resync ` +
        `from this page's applied high-water mark`,
    );
    return true;
  }

  /**
   * Re-arm a resync for a connection that is already current, naming why.
   *
   * The visibility trigger's half of the zombie repair: a webview WebKit
   * throttled may have missed frames without ever losing its socket, so
   * becoming visible is itself evidence that this page's store may be behind.
   * Re-arming rather than dispatching directly keeps ONE dispatch path — the
   * observe() below — so a forced check obeys the same "needs a workspace"
   * precondition every other resync does.
   */
  forceResync(reason: string): void {
    this.rearm();
    this.opts.log?.("info", `resync: re-armed on ${reason}`);
  }

  /**
   * The workspace's FENCE ROTATED: adopt the consequence and owe one resync.
   *
   * This is a re-arm of a different kind from `forceResync`, and the
   * difference is the whole point. A visibility wake is a GUESS that this page
   * might be behind, so it must obey the backoff and the ceiling — those are
   * the bound on the flood that motivated them. A fence rotation is PROOF of a
   * specific fact: every request this page has made under the previous fence
   * was made under an identity the daemon has since retired, so the refusals
   * they earned say nothing about whether a request under the NEW fence will
   * be answered. Holding a page at its ceiling on that history would leave it
   * frozen for exactly the rotation that would have repaired it.
   *
   * The observed shape: a daemon bounce republishes a workspace hibernated
   * with no controller generation; this page adopts that fence, resyncs with
   * it, and is refused (`rejection_cause=identity_mismatch`) 0.3s before the
   * real generation is published. The socket never cycled and the boot id
   * never changed, so nothing else here would ever ask again.
   *
   * So the failure history is discharged exactly as a fresh socket discharges
   * it — same reasoning, different evidence. An in-flight request is NOT
   * cleared: it was really sent and will really settle, and forgetting it
   * would break the single-in-flight bound. It becomes the coalesced dirty
   * request instead, which is spent at the settle edge against the fence this
   * page holds by then.
   */
  observeFenceRotation(reason: string): void {
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    this.givenUp = false;
    this.rearm();
    this.opts.log?.("warn", `resync: workspace fence rotated (${reason}); re-arming a resync under the new fence`);
  }

  /**
   * Owe one resync on the CURRENT connection, without waiting for a socket
   * event to supply the arming.
   *
   * Both out-of-band triggers — a changed daemon identity, and the page coming
   * back into view — mean the same thing: this page may be behind and no socket
   * event is going to say so. Each needs the snapshot precondition satisfied as
   * well as the arm, because there is no connect snapshot coming for a socket
   * that never cycled, and an arm without it would sit waiting for one forever.
   * Stating that pair once is what keeps a third trigger from setting only half
   * of it.
   */
  private rearm(): void {
    this.armed = true;
    this.snapshotSeen = true;
  }

  /**
   * A socket opened: this connection owes one resync, ONCE, and immediately.
   *
   * A fresh socket is new evidence, not a retry: whatever refused or swallowed
   * the last request belonged to the connection that is now gone, so the
   * backoff and the failure count it accumulated are discharged here. Any
   * request still marked in flight belonged to that dead socket and can never
   * settle, so it is dropped rather than left blocking this connection's one
   * resync forever.
   */
  onConnect(): void {
    this.armed = true;
    this.snapshotSeen = false;
    this.flushSuppressed("socket_reconnected");
    // The dead socket's flight is dropped, so its eventual settle must not be
    // applied to this connection's request: retire the token with it.
    this.clearDeadline();
    this.flightToken++;
    this.inFlight = false;
    this.dirty = false;
    this.failures = 0;
    this.reanchors = 0;
    this.nextAllowedAtMs = 0;
    this.givenUp = false;
  }

  /**
   * The socket dropped. Disarming here rather than only re-arming on connect
   * means a resync can never be attributed to a dead socket's snapshot.
   */
  onDisconnect(): void {
    this.armed = false;
    this.snapshotSeen = false;
    // A request whose socket died cannot be acked or nacked by anyone. Holding
    // it in flight would make the NEXT connection's resync coalesce into a
    // settle that never comes.
    this.flushSuppressed("socket_lost");
    this.clearDeadline();
    this.flightToken++;
    this.inFlight = false;
  }

  /**
   * Feed one ingested frame. `isSnapshot` says whether it was the connect
   * `StateSnapshot`; SNAPSHOT is read from the store AFTER ingest, so one that
   * supplies the cwd and controller identity fires on its own arrival.
   *
   * Returns whether the resync was dispatched on this call.
   */
  observe(isSnapshot: boolean, snapshot: ResyncSnapshot): boolean {
    if (isSnapshot) this.snapshotSeen = true;
    if (!this.armed || !this.snapshotSeen || snapshot.workspace === "") return false;
    // THE THREE BOUNDS, in the order that keeps the queue finite.
    //
    // Given up: the ceiling was reached and the banner says so. Asking anyway
    // would be the silent spinning the banner exists to replace.
    if (this.givenUp) return false;
    // In flight: exactly one outstanding request. A want-resync arriving now
    // is not dropped — it is remembered as dirty and spent when the in-flight
    // one settles, so the catch-up it wanted still happens, once.
    if (this.inFlight) {
      this.dirty = true;
      this.noteSuppressed(
        snapshot.workspace,
        `resync: request already in flight ws=${snapshot.workspace} decision=coalesce; ` +
          `further suppressions are counted, not logged`,
      );
      return false;
    }
    // Backing off: a failed resync owes a growing delay before the next one,
    // so a daemon that cannot answer is asked less often rather than more.
    const now = this.now();
    if (now < this.nextAllowedAtMs) {
      this.dirty = true;
      // THE SAME RATE LIMIT THE IN-FLIGHT BRANCH HAS, and for the same reason:
      // a want-resync fires on essentially every ingested frame, so a workspace
      // whose resync keeps failing wrote one line PER FRAME for the whole
      // backoff window. Production showed 3,887 identical `backing off` lines
      // for one workspace — hundreds sharing a single millisecond — which is
      // the ~25 records/second of client-log telemetry that flooded the daemon.
      // The run's information is the first line plus the count, exactly as for
      // coalescing, and the summary is flushed by whichever of the dispatch,
      // the settle, or the disconnect closes the run.
      this.noteSuppressed(
        snapshot.workspace,
        `resync: backing off ws=${snapshot.workspace} failures=${this.failures} ` +
          `retry_in_ms=${this.nextAllowedAtMs - now} decision=defer; ` +
          `further deferrals are counted, not logged`,
      );
      return false;
    }
    // A BACKOFF RUN ENDS WHERE THE BACKOFF DOES. The in-flight run is closed by
    // its settle, but a run of deferrals has no settle to wait for — the window
    // simply expires and this dispatch happens — so it is reported here.
    this.flushSuppressed("dispatched");
    // Disarm BEFORE dispatching: the resync's own snapshot reply comes back
    // through this same path, and a trigger still armed when it lands would
    // ask again, forever.
    this.armed = false;
    this.opts.log?.(
      "info",
      `resync: requesting snapshot-bound conversation history ws=${snapshot.workspace} ` +
        `fence=${snapshot.fence || "none"} from_seq=${snapshot.fromSeq} decision=dispatch`,
    );
    this.send(snapshot, false);
    return true;
  }

  /**
   * Send one resync and rule on its refusal.
   *
   * IDENTITY MISMATCH IS NOT A FAILURE TO REPORT AND FORGET. A page that
   * outlived a daemon bounce can hold a session identity the daemon has since
   * superseded (a phantom or retired session), and every resync it sends is
   * refused with `rejection_cause=identity_mismatch`. Repeating the request
   * with the same identity can only be refused again, so the retry ADOPTS the
   * live identity the store now holds and asks once more with that.
   *
   * EXACTLY ONE RETRY. An adopted identity that is itself refused means this
   * page cannot name a live one from what it has, and a second adoption would
   * be the same guess. It re-arms instead, so the NEXT snapshot — which
   * carries the daemon's own account of who is live — supplies the identity
   * rather than this end inventing one.
   */
  private send(snapshot: ResyncSnapshot, isRetry: boolean): void {
    this.inFlight = true;
    const token = ++this.flightToken;
    this.armDeadline(token, snapshot);
    this.opts.resync(snapshot).then(
      () => {
        if (!this.claimSettle(token, "acked")) return;
        this.settleAcked();
      },
      (err: unknown) => {
        if (!this.claimSettle(token, "rejected")) return;
        this.settleRejected(snapshot, isRetry, err);
      },
    );
  }

  /** The deadline this instance runs its flights on. */
  private settleDeadlineMs(): number {
    return this.opts.settleDeadlineMs ?? RESYNC_SETTLE_DEADLINE_MS;
  }

  /**
   * Arm the deadline for the flight TOKEN identifies.
   *
   * The timeout is the ONLY thing that can discharge a flight over a socket
   * that stays up, which is exactly the wedge observed in production: without
   * it, an ack that never comes blocks every later resync for the life of the
   * connection.
   */
  private armDeadline(token: number, snapshot: ResyncSnapshot): void {
    const timers = this.opts.timers ?? AMBIENT_TIMERS;
    const deadlineMs = this.settleDeadlineMs();
    this.clearDeadline();
    this.deadlineTimer = timers.setTimeout(() => {
      this.deadlineTimer = null;
      // Another flight has since taken over (a retry, a reconnect): that one
      // owns the bound now, and discharging it here would double-charge.
      if (token !== this.flightToken || !this.inFlight) return;
      // Retire the token so the original dispatch's eventual settle is
      // recognized as late rather than applied to whatever runs next.
      this.flightToken++;
      this.opts.log?.(
        "error",
        `resync: request unsettled after ${deadlineMs}ms ws=${snapshot.workspace} ` +
          `fence=${snapshot.fence || "none"} from_seq=${snapshot.fromSeq} decision=discharged; ` +
          `holding it in flight would block every later resync on this socket`,
      );
      this.settleFailed(`settle_deadline_exceeded_after_${deadlineMs}ms`);
    }, deadlineMs);
  }

  private clearDeadline(): void {
    if (this.deadlineTimer === null) return;
    (this.opts.timers ?? AMBIENT_TIMERS).clearTimeout(this.deadlineTimer);
    this.deadlineTimer = null;
  }

  /**
   * Whether a settle for TOKEN may be applied, clearing the deadline when it
   * may. A settle for a discharged flight is COUNTED, not silently dropped:
   * the count is the evidence that the daemon does answer eventually and the
   * deadline is what the page is really waiting past.
   */
  private claimSettle(token: number, outcome: string): boolean {
    if (token === this.flightToken) {
      this.clearDeadline();
      return true;
    }
    this.lateSettles += 1;
    this.opts.log?.(
      "warn",
      `resync: ${outcome} settle arrived for a flight already discharged by its deadline ` +
        `late_settles=${this.lateSettles} decision=ignored`,
    );
    return false;
  }

  /**
   * The daemon acked: the failure history is discharged, and a want-resync
   * that arrived mid-flight is spent NOW rather than waiting for the next
   * heartbeat, from the mark this page has applied by this moment.
   */
  private settleAcked(): void {
    this.flushSuppressed("acked");
    this.inFlight = false;
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    if (!this.dirty) return;
    this.dirty = false;
    const next = this.opts.latestSnapshot?.() ?? null;
    if (next === null || next.workspace === "") {
      // Nothing nameable to ask with: leave the arm to the ordinary triggers
      // rather than inventing a request.
      this.rearm();
      return;
    }
    this.opts.log?.(
      "info",
      `resync: spending coalesced request ws=${next.workspace} from_seq=${next.fromSeq} decision=dispatch`,
    );
    this.send(next, false);
  }

  /**
   * A terminal failure: charge it to the backoff, and at the ceiling stop and
   * raise the banner instead of continuing to ask.
   */
  private settleFailed(cause: string): void {
    this.flushSuppressed("failed");
    this.inFlight = false;
    this.failures += 1;
    if (this.failures >= RESYNC_FAILURE_CEILING) {
      this.givenUp = true;
      this.armed = false;
      this.dirty = false;
      this.opts.log?.(
        "error",
        `resync: giving up after ${this.failures} consecutive failures cause=${cause}; ` +
          `surfacing the connection banner`,
      );
      this.opts.onGiveUp?.(this.failures, cause);
      return;
    }
    const delay = resyncBackoffMs(this.failures, (this.opts.random ?? Math.random)());
    this.nextAllowedAtMs = this.now() + delay;
    // Re-arm so the next trigger CAN retry — the backoff gate above, not the
    // arm, is what decides when it actually goes out.
    this.rearm();
    this.opts.log?.(
      "warn",
      `resync: failed failures=${this.failures} retry_in_ms=${delay} cause=${cause}`,
    );
  }

  /**
   * The daemon refused this page's mark as RETIRED: hand the repair to the
   * pager and stop asking for a delta.
   *
   * IT DOES NOT RE-ARM, and that is the loop guard. A re-anchor drops this
   * page's mark to zero, and a page holding zero asks the PAGER for a tail
   * rather than the daemon for a delta — so re-arming here would only race the
   * pager to the same tail page. The pager owns exactly one in-flight request
   * and its own ceiling, so the repair is bounded by machinery that already
   * exists rather than by a second copy of it here.
   *
   * THE CEILING IS THE SECOND GUARD. A re-anchor that keeps being followed by
   * another refused mark is not converging on anything, and past
   * RESYNC_REANCHOR_CEILING the refusal is charged to the backoff and surfaces
   * like any other, rather than cycling where nobody can see it.
   */
  private settleReanchored(snapshot: ResyncSnapshot, cause: string): void {
    this.flushSuppressed("mark_retired");
    this.inFlight = false;
    this.dirty = false;
    if (this.reanchors >= RESYNC_REANCHOR_CEILING) {
      this.opts.log?.(
        "error",
        `resync: replay mark refused as retired ${this.reanchors + 1}x on this connection ws=${snapshot.workspace}; ` +
          `re-anchoring is not converging, so this refusal is charged to the backoff cause=${cause}`,
      );
      this.settleFailed(cause);
      return;
    }
    this.reanchors += 1;
    const started = this.opts.reanchor?.(cause) ?? false;
    if (!started) {
      this.opts.log?.(
        "warn",
        `resync: replay mark refused as retired ws=${snapshot.workspace} from_seq=${snapshot.fromSeq} ` +
          `but this page could not start a tail re-anchor; charging the refusal to the backoff`,
      );
      this.settleFailed(cause);
      return;
    }
    // The refusal history is discharged for the reason a fence rotation
    // discharges it: every request made against the retired space says nothing
    // about the live one.
    this.failures = 0;
    this.nextAllowedAtMs = 0;
    this.opts.log?.(
      "warn",
      `resync: replay mark ${snapshot.fromSeq} belongs to a RETIRED seq space ws=${snapshot.workspace} ` +
        `decision=re_anchor reanchors=${this.reanchors}; the conversation is being REPLACED from a tail page, ` +
        `not extended`,
    );
  }

  private settleRejected(snapshot: ResyncSnapshot, isRetry: boolean, err: unknown): void {
    const cause = String(err);
    // A refused resync means this mount keeps whatever history it already
    // had — say so loudly rather than leaving an empty feed unexplained.
    this.opts.log?.(
      "error",
      `resync request failed ws=${snapshot.workspace} fence=${snapshot.fence || "none"} ` +
        `from_seq=${snapshot.fromSeq} decision=rejected cause=${cause}`,
    );
    // A RETIRED MARK IS RULED ON FIRST, and it is not a failure at all: the
    // daemon answered definitively, and the answer is "ask a different
    // question". Charging it to the backoff would delay the tail page that
    // repairs the view, and retrying the same mark can only earn the same
    // refusal.
    if (isRetiredReplayMark(cause)) {
      this.settleReanchored(snapshot, cause);
      return;
    }
    // EVERY TERMINAL REFUSAL IS CHARGED TO THE BACKOFF, identity mismatch
    // included — a page that cannot name a live identity re-asks on the next
    // snapshot, and without the charge that pair is its own flood.
    if (!isIdentityMismatch(cause)) {
      this.settleFailed(cause);
      return;
    }
    if (isRetry) {
      this.settleFailed(cause);
      this.opts.log?.(
        "warn",
        `resync: adopted identity was itself superseded ws=${snapshot.workspace} ` +
          `fence=${snapshot.fence || "none"}; re-armed for the next snapshot's identity`,
      );
      return;
    }
    const adopted = this.opts.adoptIdentity?.(cause) ?? null;
    if (adopted === null || adopted.workspace === "") {
      this.settleFailed(cause);
      this.opts.log?.(
        "warn",
        "resync: identity mismatch with no live identity to adopt yet; re-armed for the next snapshot",
      );
      return;
    }
    this.opts.log?.(
      "warn",
      `resync: identity mismatch; adopting live identity ws=${adopted.workspace} ` +
        `fence=${adopted.fence || "none"} from_seq=${adopted.fromSeq} and retrying once`,
    );
    // The retry is the SAME in-flight request continuing, not a second one:
    // send() re-marks it, and only its own settle discharges the flight.
    this.send(adopted, true);
  }
}
