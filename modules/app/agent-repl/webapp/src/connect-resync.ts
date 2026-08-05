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
  sessionId: string;
  controllerGenerationId: string;
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

  constructor(private readonly opts: ConnectResyncOptions) {}

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
        `session=${snapshot.sessionId || "none"} generation=${snapshot.controllerGenerationId || "none"} ` +
        `from_seq=${snapshot.fromSeq} decision=dispatch`,
    );
    this.opts.resync(snapshot).catch((err: unknown) => {
      // A refused resync means this mount keeps whatever history it already
      // had — say so loudly rather than leaving an empty feed unexplained.
      this.opts.log?.(
        "error",
        `resync request failed ws=${snapshot.workspace} session=${snapshot.sessionId || "none"} ` +
          `generation=${snapshot.controllerGenerationId || "none"} from_seq=${snapshot.fromSeq} ` +
          `decision=rejected cause=${String(err)}`,
      );
    });
    return true;
  }
}
