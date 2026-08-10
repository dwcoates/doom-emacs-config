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

/** How this module reports itself; mirrors the app's client-log levels. */
export type ConnectResyncLogLevel = "info" | "warn" | "error";

/** The complete revisioned WorkspaceState snapshot a replay is conditional on. */
export interface ResyncSnapshot {
  workspace: string;
  fromSeq: number;
  fence: string;
}

export interface ConnectResyncOptions {
  /** Sends one ResyncCmd; rejects when the daemon nacks it. */
  resync: (snapshot: ResyncSnapshot) => Promise<void>;
  log?: (level: ConnectResyncLogLevel, message: string) => void;
}

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

  constructor(private readonly opts: ConnectResyncOptions) {}

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
    this.armed = true;
    this.snapshotSeen = true;
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
    this.armed = true;
    this.snapshotSeen = true;
    this.opts.log?.("info", `resync: re-armed on ${reason}`);
  }

  /** A socket opened: this connection owes one resync. */
  onConnect(): void {
    this.armed = true;
    this.snapshotSeen = false;
  }

  /**
   * The socket dropped. Disarming here rather than only re-arming on connect
   * means a resync can never be attributed to a dead socket's snapshot.
   */
  onDisconnect(): void {
    this.armed = false;
    this.snapshotSeen = false;
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
    // Disarm BEFORE dispatching: the resync's own snapshot reply comes back
    // through this same path, and a trigger still armed when it lands would
    // ask again, forever.
    this.armed = false;
    this.opts.log?.(
      "info",
      `resync: requesting snapshot-bound conversation history ws=${snapshot.workspace} ` +
        `fence=${snapshot.fence || "none"} from_seq=${snapshot.fromSeq} decision=dispatch`,
    );
    this.opts.resync(snapshot).catch((err: unknown) => {
      // A refused resync means this mount keeps whatever history it already
      // had — say so loudly rather than leaving an empty feed unexplained.
      this.opts.log?.(
        "error",
        `resync request failed ws=${snapshot.workspace} fence=${snapshot.fence || "none"} ` +
          `from_seq=${snapshot.fromSeq} decision=rejected cause=${String(err)}`,
      );
    });
    return true;
  }
}
