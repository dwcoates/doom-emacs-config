/**
 * The shim's connections to the shim-store.
 *
 * Two SINGLE-ROLE connections to the same socket — the store classifies every
 * connection by its FIRST frame, so the two jobs cannot share one conn:
 * 1. The PRODUCER connection (connect()): WRITE persistent event batches
 *    (`StoreWrite`) and reconcile each `StoreWriteAck` (accepted / deduped /
 *    error accounting).
 * 2. The SUBSCRIPTION connection (subscribe(), reopened per call): first
 *    frame `Subscribe{session_id, from_seq}`, then a continuous read loop
 *    handing every store-merged `Event` to an injected sink (wired to
 *    `SessionServer.sendEvent`, forwarding to the daemon verbatim).
 *
 * THE HONEST SAD PATH (design §4.4, metaprompt no-fallbacks rule): if the
 * store is unreachable or rejects a batch, every event in that batch is
 * loud-logged as dropped and a `DegradedState` is reported to the injected
 * reporter. There is NO spill buffer, NO retry of a rejected batch, NO
 * fallback — store downtime is honest downtime and the display goes stale
 * until it returns.
 *
 * "Until it returns" is load-bearing, and the LINK STATE MACHINE below is what
 * makes it true. The store is launchd-managed and restarts under a live shim,
 * which kills BOTH connections for good. Without recovery every later write
 * drops against a corpse, the merged tail never resumes, and the connectivity
 * fault the daemon opened for the outage never closes — every workspace stays
 * painted degraded until something bounces the shim. That is what a deploy's
 * store restart did on 2026-08-02, fleet-wide.
 *
 * SO THE LINK RECOVERS, AS A LINK, AND SAYS SO:
 *
 *   - A dropped producer connection or a dropped standing subscription arms a
 *     relink (bounded backoff, immediate first attempt, 100ms doubling to a 5s
 *     ceiling — the same discipline the sidecar's store link and the
 *     daemon<->shim link use).
 *   - A relink re-establishes the producer connection and, when the daemon has
 *     a standing subscription, REOPENS it at {@link resumeSeq} — the last seq
 *     actually forwarded to the sink, so the store replays the gap rather than
 *     leaving it silently missing.
 *   - Once both are live again the client reports `DegradedState{recovered:
 *     true}`, which is the close edge of the fault window the outage opened.
 *     The daemon's fault machinery closes the session_fault row on it and
 *     re-resolves the workspace, so the color leaves the blue band unaided.
 *
 * NONE OF THAT SOFTENS THE SAD PATH. There is still no spill buffer, a
 * rejected batch is still never retried, and a write landing during an outage
 * is still dropped loudly. Recovery restores the LINK; it never resurrects the
 * events that fell while the link was down. A deliberate close() is final and
 * never relinks.
 *
 * THE SUBSCRIPTION KEY is the VENDOR session id (Claude's uuid), not this
 * shim's `--session-id` — see `storeKey`. Writes were always keyed that way
 * (the envelope is read off the SDK message); the subscription was not, so it
 * listened on a channel nothing published to.
 */
import net from "node:net";
import { create } from "@bufbuild/protobuf";
import { MessageConn, envelopeType, unpackAs } from "./framing.js";
import type { Any } from "./framing.js";
import { bindLog } from "./log.js";
import {
  DegradedState,
  DegradedStateSchema,
  Event,
  EventBatchSchema,
  EventSchema,
  HealthCheckSchema,
  HealthStatusSchema,
  Heartbeat,
  HeartbeatSchema,
  StoreWriteAck,
  StoreWriteAckSchema,
  StoreWriteSchema,
  SubscribeSchema,
} from "./proto.js";

/** Receives every store-merged, seq-stamped Event for forwarding. */
export type StoreSink = (evt: Event) => void;

/** Receives an honest degraded report when the store cannot be served. */
export type DegradedReporter = (report: DegradedState) => void;

/**
 * Bounces the DAEMON-facing connection as the second half of a vendor session
 * ROTATION (see {@link StoreClient.adoptStoreKey}).
 *
 * The store client owns the store key; it does not own the daemon link, and
 * re-keying without bouncing that link would push the new key's seq=1 down a
 * connection whose last_seen sits in the retired key's space — the terminal
 * ErrSeqRegression the rotation exists to avoid. So the two halves are wired
 * together here: whoever owns the daemon connection injects the bounce.
 *
 * Called SYNCHRONOUSLY and BEFORE the re-key, so no event of the new seq space
 * can reach the old connection.
 */
export type RotationHandler = (previous: string, next: string) => void;

/** One bounded historical replay's request (see {@link StoreClient.replay}). */
export interface ReplayOptions {
  /** EXCLUSIVE lower bound, matching Subscribe.from_seq. */
  fromSeq: bigint;
  /** EXCLUSIVE upper bound; 0n = stream until the replay drains. */
  toSeq: bigint;
  /** Hard cap on events delivered by this replay; 0 = uncapped. */
  maxEvents: number;
  /** No frame within this window means the replay drained. */
  idleMs: number;
  /** Receives each replayed event, in store order. */
  onEvent: (evt: Event) => void;
}

/** How a bounded replay ended. `reason` is empty unless truncated. */
export interface ReplayOutcome {
  delivered: number;
  truncated: boolean;
  reason: string;
}

export interface StoreClientOptions {
  socketPath: string;
  sessionId: string;
  /** Producer identity in StoreWrite; e.g. `claude-shim:<session>`. */
  producer: string;
  /** Heartbeat cadence on the connection; 0 disables. Default 5000ms. */
  heartbeatIntervalMs?: number;
  /**
   * The VENDOR session id (the Claude CLI's uuid, which is its transcript
   * filename) to subscribe under, when it is already known — i.e. on
   * `--resume`. Omit for a fresh session, whose uuid only the SDK can reveal;
   * `adoptStoreKey` sets it on the first converted event. See storeKey.
   */
  storeSessionId?: string;
  /** First relink delay after a failed attempt; default 100ms. */
  relinkBackoffMinMs?: number;
  /** Relink backoff ceiling; default 5000ms. */
  relinkBackoffMaxMs?: number;
}

const COMPONENT = "shim-store-client";
const HEALTH_TIMEOUT_MS = 2000;
/**
 * Relink backoff bounds, matching the discipline the sidecar's store link and
 * the daemon<->shim link already use: an immediate first attempt, then 100ms
 * doubling to a 5s ceiling. Bounded rather than unbounded because a store that
 * is down stays down for a deploy's length, not forever.
 */
const RELINK_BACKOFF_MIN_MS = 100;
const RELINK_BACKOFF_MAX_MS = 5000;

export interface StoreHealth {
  healthy: boolean;
  reason: string;
}
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.store-client.connection" });

interface PendingWrite {
  resolve: (ack: StoreWriteAck) => void;
  reject: (err: Error) => void;
  events: Event[];
}

/** A subscription socket that has sent Subscribe but has not crossed readiness. */
interface OpeningSubscription {
  socket: net.Socket;
  conn: MessageConn | null;
  settled: boolean;
  reject: (err: Error) => void;
}

export class StoreClient {
  private conn: MessageConn | null = null;
  /** Producer socket while connect() has not yet established MessageConn ownership. */
  private connectingSocket: net.Socket | null = null;
  /** Subscription transport before the store confirms registration and replay. */
  private openingSub: OpeningSubscription | null = null;
  /** A standing tail only after the store's readiness barrier has arrived. */
  private subConn: MessageConn | null = null;
  private connected = false;
  private sink: StoreSink | null = null;
  private reporter: DegradedReporter | null = null;
  private readonly pendingWrites: PendingWrite[] = [];
  /**
   * The in-flight producer redial, shared by every write that finds the
   * connection down. Writes are fire-and-forget from routeSdkMessage, so
   * without this a burst arriving during an outage would open one socket per
   * message instead of one per outage.
   */
  private redialing: Promise<void> | null = null;
  /**
   * Set by close(). A deliberate teardown must STAY torn down: without this a
   * write racing shutdown would redial the store and resurrect a connection
   * the shim is in the middle of closing.
   */
  private closed = false;
  /**
   * Tail of the send queue. `pendingWrites` is matched to acks POSITIONALLY
   * (onAck shifts), so sends must be issued in call order. write() used to be
   * synchronous up to conn.send(), which guaranteed that for free; awaiting a
   * redial breaks it, so sends chain here instead. This resolves once a send
   * is ISSUED, never once its ack lands — chaining on acks would serialize
   * round-trips and throttle the stream.
   */
  private sendChain: Promise<void> = Promise.resolve();
  private heartbeatTimer: NodeJS.Timeout | null = null;
  private readonly heartbeatIntervalMs: number;
  /**
   * The session id events are actually STORED under, which is the vendor
   * (Claude CLI) uuid — NOT this shim's `--session-id`.
   *
   * The store keys everything by the `session_id` on the Event envelope, and
   * that envelope is filled from the SDK message (`readEnvelope`), so writes
   * land under the vendor uuid. It has to be the uuid: the shim-sidecar
   * writes the SAME conversation from the transcript file, and its only
   * available identity is the filename (`<uuid>.jsonl`) — it never talks to
   * the daemon and cannot know an `s_…` id. Both planes must agree or the
   * `(session_id, dedup_key)` dedup that merges them cannot fire.
   *
   * Subscribing under the daemon's `s_…` id therefore registered a subscriber
   * on a channel nothing is ever published to: writes worked, replay and
   * live-tail silently returned nothing, and only EPHEMERAL events (which
   * bypass the store) reached the daemon.
   *
   * Seeded from `--resume` when known, else from the shim's own session id —
   * which is the pre-fix behavior, and harmless as a starting point because
   * `adoptStoreKey` upgrades it (and reopens the subscription) the moment the
   * first converted event reveals the real uuid.
   */
  private storeKey: string;
  /**
   * The daemon's last requested `from_seq`, retained so adopting the store key
   * can REOPEN the subscription at the same position. Null when the daemon has
   * not subscribed yet, in which case adopting merely records the key.
   */
  private lastFromSeq: bigint | null = null;
  /**
   * Whether any merged event has been forwarded to the sink under the current
   * store key. Once true the key is settled: the daemon has begun tracking
   * that key's seq space, so switching would look like a seq regression.
   */
  private forwardedAny = false;
  /**
   * Whether `storeKey` is a genuine VENDOR uuid rather than the `--session-id`
   * placeholder the constructor seeds it with. True from the start on
   * `--resume` (the uuid was passed in), otherwise set by the first adoption.
   *
   * It is what {@link vendorSessionId} reports, and reporting the placeholder
   * there would be worse than reporting nothing: the daemon compares the
   * announced uuid with the one it has persisted, so a placeholder would look
   * to it like a rotation away from the real conversation.
   */
  private vendorKnown: boolean;
  /**
   * Whether an UNRECOVERED degraded window is open — i.e. this client has
   * reported a `DegradedState` the daemon turned into an open session_fault
   * row and has not yet reported the recovery that closes it.
   *
   * It is what makes the two edges a WINDOW rather than two unrelated
   * reports: exactly one `recovered: true` is sent per outage, and none at all
   * when nothing was ever degraded. The daemon rejects a close with no open
   * window (ErrFaultWindowNotOpen), so an unpaired recovery would be a loud
   * error in its log rather than a no-op.
   */
  private linkDegraded = false;
  /** The armed relink attempt, or null when none is pending. */
  private relinkTimer: NodeJS.Timeout | null = null;
  /** The delay the NEXT relink attempt waits; 0 means immediate. */
  private relinkDelayMs = 0;
  /** Guards against two relink attempts overlapping on one outage. */
  private relinking = false;
  private readonly relinkMinMs: number;
  private readonly relinkMaxMs: number;
  /**
   * The highest seq actually handed to the sink under the current store key,
   * which is where a reopened subscription must resume from.
   *
   * The daemon's `from_seq` is only its position at HANDSHAKE time; by the
   * time a store restart drops the tail the daemon has consumed everything
   * this client forwarded since. Resubscribing at the handshake position would
   * replay all of it, and resubscribing at 0 would replay the conversation —
   * but resubscribing ABOVE what was forwarded would silently lose the events
   * that fell in the gap, which is the failure that matters. `Subscribe.
   * from_seq` is EXCLUSIVE, so the last forwarded seq resumes exactly after
   * the last event the daemon saw.
   */
  private lastForwardedSeq: bigint | null = null;
  /** The daemon-link bounce for a rotation; see {@link RotationHandler}. */
  private rotator: RotationHandler | null = null;
  /**
   * The uuid an IN-FLIGHT rotation is moving to, while it waits for the
   * already-issued writes to reach the wire.
   *
   * `adoptStoreKey` runs on EVERY converted event, so without this every
   * message arriving during that wait would start a rotation of its own.
   */
  private rotatingTo: string | null = null;

  constructor(private readonly opts: StoreClientOptions) {
    this.heartbeatIntervalMs = opts.heartbeatIntervalMs ?? 5000;
    this.relinkMinMs = opts.relinkBackoffMinMs ?? RELINK_BACKOFF_MIN_MS;
    this.relinkMaxMs = opts.relinkBackoffMaxMs ?? RELINK_BACKOFF_MAX_MS;
    this.storeKey = opts.storeSessionId ?? opts.sessionId;
    this.vendorKnown = (opts.storeSessionId ?? "") !== "";
  }

  /** Route the rotation's daemon-link bounce to `handler`. */
  onRotation(handler: RotationHandler): void {
    this.rotator = handler;
  }

  /**
   * The VENDOR session id this client is filing events under, or "" while it
   * is still the `--session-id` placeholder. Announced to the daemon in
   * ShimHello.vendor_session_id.
   */
  vendorSessionId(): string {
    return this.vendorKnown ? this.storeKey : "";
  }

  /**
   * Adopt the vendor session id events are stored under, learned from a
   * converted event's envelope. Idempotent, and a no-op when unchanged.
   *
   * TWO DIFFERENT THINGS WEAR THIS ONE NAME, told apart by whether any merged
   * event has been forwarded yet:
   *
   * 1. FIRST ADOPTION (nothing forwarded). The placeholder `--session-id` the
   *    constructor seeded is replaced by the uuid the SDK just revealed, and
   *    an already-open subscription is reopened at the daemon's last
   *    `from_seq`. Nothing is lost by subscribing late: the store replays
   *    everything with `seq > from_seq` from disk.
   *
   * 2. ROTATION (events already forwarded under the old uuid). The VENDOR
   *    minted a new transcript identity mid-stream — a `/clear` does exactly
   *    this — which retires one store seq space and starts another at 1. See
   *    {@link rotateStoreKey}.
   *
   * A REPEAT of the CURRENT key is neither: it returns above, which is what
   * keeps the same-uuid seq guarantee intact. Nothing here can move a key to
   * itself, so nothing here can make one uuid's seqs run backwards.
   */
  adoptStoreKey(sessionId: string): void {
    if (!sessionId || sessionId === this.rotatingTo) return;
    if (sessionId === this.storeKey) {
      // Equality does not prove this is the constructor's placeholder. The
      // SDK may authoritatively report the same string as the daemon session
      // id (the offline SDK does), which makes it a genuine vendor identity.
      // Record that observation without reopening an unchanged subscription.
      if (!this.vendorKnown) {
        this.vendorKnown = true;
        LOGGER.log({ agent_repl_session_id: this.opts.sessionId, claude_session_id: sessionId, store_key: sessionId },
          "confirmed the placeholder key as the SDK-reported vendor session id");
      }
      return;
    }
    if (this.forwardedAny) {
      this.rotatingTo = sessionId;
      LOGGER.log({
        agent_repl_session_id: this.opts.sessionId,
        claude_session_id: sessionId,
        store_key: this.storeKey,
        rotating_to: sessionId,
      }, "vendor session rotation detected, beginning controlled store re-key");
      void this.rotateStoreKey(sessionId);
      return;
    }
    const previous = this.storeKey;
    this.storeKey = sessionId;
    this.vendorKnown = true;
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, claude_session_id: sessionId, store_key: sessionId },
      `adopted store session key (was ${previous})`);
    // Only reopen if a standing subscription position exists; otherwise
    // recording the key is enough and the next subscribe() picks it up. The
    // rejection is dropped deliberately: subscribe() already degraded, and
    // this reopen has no caller waiting on it.
    if (this.lastFromSeq !== null) void this.subscribe(this.lastFromSeq).catch(() => {});
  }

  /**
   * A CONTROLLED RE-HANDSHAKE for a vendor session uuid rotation.
   *
   * WHAT ROTATION BREAKS. Each session id is its own SEQ SPACE, and the daemon
   * tracks last_seen_seq per shim CONNECTION. Silently re-keying hands that
   * connection the new space's seq=1 against a much higher last_seen, which it
   * reads as ErrSeqRegression — terminal, never reconnected from. That is what
   * a mid-session `conversation_reset` did on 2026-07-25 (seq=1 against
   * last_seen=5990) and why this used to REFUSE the switch outright, with
   * "until the shim respawns" as its stated remedy.
   *
   * Refusing cost more than it saved: every post-rotation fact — the sidecar's
   * ContextCleared, the turn's own TurnEnded, every later turn — was filed
   * under a uuid the daemon never subscribed to, so the clear never rendered
   * and the footer sat in THINKING forever. This performs that same remedy
   * DELIBERATELY and in miniature: the connection is bounced rather than the
   * process.
   *
   * THE ORDER IS THE WHOLE MECHANISM:
   *
   * 1. WAIT for the writes already issued under the old key to reach the wire.
   *    They carry the old uuid on their own envelopes, so they file correctly
   *    wherever they land — but their acks are matched POSITIONALLY on the
   *    producer connection, so they must not be interleaved with anything.
   * 2. BOUNCE THE DAEMON LINK, before the key moves. The old connection's
   *    last_seen belongs to the retiring space; anything from the new space
   *    reaching it is the regression above.
   * 3. RE-KEY, and RETIRE the standing subscription rather than reopening it
   *    here.
   *
   * STEP 3 USED TO REOPEN AT from_seq=0 ITSELF, and no longer needs to: the
   * bounce's re-handshake IS the gate, and the DaemonHello opening the next
   * connection carries the daemon's own from_seq — zero, because the hello's
   * announcement of the new uuid is what makes the daemon reset the cursor
   * before it reads it (core.proto DaemonHello.from_seq). Reopening here as
   * well raced that gate with a second subscription it would immediately
   * replace. What the retirement keeps is the property the reopen provided:
   * no connection to the RETIRED seq space outlives the rotation, so the
   * re-handshaked daemon cannot be handed an event counted in a space that is
   * gone.
   *
   * Steps 2 and 3 are synchronous and adjacent, so nothing can land between
   * them: the reconnect needs I/O and this event loop is single threaded.
   */
  private async rotateStoreKey(next: string): Promise<void> {
    const previous = this.storeKey;
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, store_key: previous, rotating_to: next },
      `VENDOR SESSION ROTATION observed: ${previous} -> ${next} — the vendor minted a new transcript identity mid-stream; re-keying the store subscription and bouncing the daemon link`);
    // Every send is chained (see sendChain) and already reports its own
    // failures, so this only ever waits — it never absorbs an error.
    await this.sendChain;
    if (this.closed) {
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, store_key: previous, rotating_to: next },
        `rotation ABANDONED: the store client was closed while in-flight writes drained`);
      this.rotatingTo = null;
      return;
    }
    if (this.rotator) {
      this.rotator(previous, next);
    } else {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, store_key: previous, rotating_to: next },
        `rotation has no daemon-link bounce bound — the daemon will keep the retired key's last_seen and read the new key's seq as a regression`);
    }
    this.storeKey = next;
    this.vendorKnown = true;
    this.rotatingTo = null;
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, store_key: next },
      `store re-keyed to the rotated vendor session id (was ${previous})`);
    // The standing subscription belongs to the RETIRED seq space. Retire it
    // with the key; the bounce's re-handshake opens the new one at the
    // daemon's from_seq, which that handshake resets to zero.
    this.dropStandingSubscription();
    // ...and retire its POSITION with it. Both numbers count in the seq space
    // that just ended, so a later relink resuming from either would reopen the
    // new key's tail at a position belonging to the old one. Nulling
    // lastFromSeq is also what tells relink there is no standing subscription
    // to restore until the re-handshake asks for one.
    this.lastFromSeq = null;
    this.lastForwardedSeq = null;
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, store_key: next },
      `retired the standing subscription of the old seq space; the re-handshake gate opens the new one at the daemon's from_seq`);
  }

  /** The session id this client currently subscribes under. */
  storeSessionId(): string {
    return this.storeKey;
  }

  /** Route merged store events to `sink` (idempotent to set). */
  onMerged(sink: StoreSink): void {
    this.sink = sink;
  }

  /** Route degraded reports to `reporter`. */
  onDegraded(reporter: DegradedReporter): void {
    this.reporter = reporter;
  }

  /** Connect to the store socket. Rejects loudly if the connect fails. */
  connect(): Promise<void> {
    return new Promise((resolve, reject) => {
      const socket = net.connect(this.opts.socketPath);
      this.connectingSocket = socket;
      const forgetSocket = (): void => {
        if (this.connectingSocket === socket) this.connectingSocket = null;
      };
      const onError = (err: Error): void => {
        forgetSocket();
        reject(err);
      };
      const onClose = (): void => {
        forgetSocket();
        reject(new Error("store producer connection closed before it was established"));
      };
      socket.once("error", onError);
      socket.once("close", onClose);
      socket.once("connect", () => {
        socket.removeListener("error", onError);
        socket.removeListener("close", onClose);
        forgetSocket();
        if (this.closed) {
          socket.destroy();
          reject(new Error("store client closed while producer connection was starting"));
          return;
        }
        this.conn = new MessageConn(
          socket,
          {
            onMessage: (msg) => this.onMessage(msg),
            onClose: (err) => this.onConnClose(err),
          },
          COMPONENT,
        );
        this.connected = true;
        this.startHeartbeat();
        LOGGER.log({ agent_repl_session_id: this.opts.sessionId }, `connected to store at ${this.opts.socketPath}`);
        resolve();
      });
    });
  }

  /** True while the store connection is live. */
  isConnected(): boolean {
    return this.connected;
  }

  /** True only after the daemon has installed a standing merged-event tail. */
  isSubscribed(): boolean {
    return this.lastFromSeq !== null && this.subConn !== null;
  }

  /**
   * Assert all store dependencies needed to render this session.  The existing
   * producer and standing subscription must both be live, then a separate
   * correlated HealthCheck proves the store is parsing framed protocol traffic
   * right now without disturbing either of those role-bound connections.
   */
  health(requestId: string): Promise<StoreHealth> {
    if (requestId === "") return Promise.resolve({ healthy: false, reason: "health check requires request_id" });
    if (!this.connected || this.conn === null) return Promise.resolve({ healthy: false, reason: "producer connection is not live" });
    if (!this.isSubscribed()) return Promise.resolve({ healthy: false, reason: "standing store subscription is not live" });

    return new Promise((resolve) => {
      let settled = false;
      let probe: MessageConn | null = null;
      const timeout = setTimeout(() => finish(false, `store health timed out after ${HEALTH_TIMEOUT_MS}ms`), HEALTH_TIMEOUT_MS);
      timeout.unref?.();
      const finish = (healthy: boolean, reason: string): void => {
        if (settled) return;
        settled = true;
        clearTimeout(timeout);
        probe?.close();
        LOGGER.log({ agent_repl_session_id: this.opts.sessionId, request_id: requestId, healthy, reason },
          healthy ? "store health PASS" : "store health FAIL");
        resolve({ healthy, reason });
      };
      const socket = net.connect(this.opts.socketPath);
      const onDialError = (err: Error): void => finish(false, `cannot open store health probe: ${err.message}`);
      socket.once("error", onDialError);
      socket.once("connect", () => {
        socket.removeListener("error", onDialError);
        probe = new MessageConn(
          socket,
          {
            onMessage: (msg) => {
              const status = unpackAs(msg, HealthStatusSchema);
              if (!status) {
                finish(false, `expected HealthStatus, got ${envelopeType(msg)}`);
                return;
              }
              if (status.requestId !== requestId) {
                finish(false, `HealthStatus request_id=${JSON.stringify(status.requestId)}, want ${JSON.stringify(requestId)}`);
                return;
              }
              if (!status.healthy) {
                finish(false, `store reported unhealthy: ${status.reason}`);
                return;
              }
              finish(true, "");
            },
            // MessageConn already owns any causal framing/socket error.
            onClose: (err) => finish(false, err ? "store health transport closed after a framing failure" : "store closed health connection"),
          },
          COMPONENT,
        );
        probe.send(HealthCheckSchema, create(HealthCheckSchema, { requestId }));
      });
    });
  }

  /**
   * Open (or reopen) the merged-event subscription at `fromSeq` (EXCLUSIVE).
   * The store replays persisted events with seq > fromSeq then live-tails;
   * this is exactly the reattach replay path (§4.4).
   *
   * The subscription rides its OWN connection whose first frame is the
   * Subscribe: the store's single-role protocol rejects a Subscribe sent down
   * the producer connection.
   *
   * IT RESOLVES ONLY WHEN THE STORE'S READINESS HEARTBEAT ARRIVES after
   * registration and replay — and REJECTS when that barrier cannot be crossed.
   * That is what lets the bring-up gate (uds-session.ts) hold its readiness ack
   * until the standing tail genuinely exists, instead of acking on a frame
   * merely written to a socket the store has not classified yet.
   */
  subscribe(fromSeq: bigint): Promise<void> {
    // Retained so adoptStoreKey can reopen here once the vendor uuid is
    // known: a fresh session only learns it from the SDK, which can be AFTER
    // the daemon's hello. Nothing is lost by that late reopen — the store
    // replays every event with seq > from_seq from disk.
    this.lastFromSeq = fromSeq;
    // The DAEMON's position supersedes whatever this client last forwarded:
    // it is the authority on where its own tail resumes, and a stale forwarded
    // seq from a retired subscription must not outrank it on a later relink.
    this.lastForwardedSeq = null;
    this.dropStandingSubscription();
    return this.openSubscription(fromSeq).catch((err: unknown) => {
      // The dial failure IS the outage this client exists to report: the
      // daemon has no merged tail until it is fixed. A relink is NOT armed
      // here — an explicit subscribe belongs to its caller (the bring-up gate
      // withholds readiness on it), and arming one would race that caller's
      // own retry.
      this.degrade(errText(err), 0);
      throw new Error(`store-client: ${errText(err)}`);
    });
  }

  /**
   * Open the standing subscription connection at `fromSeq` and settle once the
   * store confirms registration and replay completion with a Heartbeat.
   *
   * It REPORTS NOTHING on failure — it is the shared mechanism under both the
   * daemon's explicit {@link subscribe} (which degrades) and a relink attempt
   * (which does not, because the window it would open is already open). Who
   * owns the account of a failure is the caller's decision, not this dial's.
   */
  private openSubscription(fromSeq: bigint): Promise<void> {
    return new Promise((resolve, reject) => {
      const socket = net.connect(this.opts.socketPath);
      const opening: OpeningSubscription = { socket, conn: null, settled: false, reject };
      this.openingSub = opening;
      const fail = (err: Error): void => {
        if (opening.settled) return;
        opening.settled = true;
        if (this.openingSub === opening) this.openingSub = null;
        reject(err);
      };
      const onDialError = (err: Error): void => fail(new Error(`cannot subscribe: ${err.message}`));
      socket.once("error", onDialError);
      socket.once("connect", () => {
        socket.removeListener("error", onDialError);
        if (this.openingSub !== opening) {
          socket.destroy();
          return;
        }
        const conn: MessageConn = new MessageConn(
          socket,
          {
            onMessage: (msg) => {
              if (this.openingSub === opening && unpackAs(msg, HeartbeatSchema)) {
                opening.settled = true;
                this.openingSub = null;
                this.subConn = conn;
                resolve();
                return;
              }
              this.onSubMessage(msg);
            },
            onClose: (err) => {
              if (this.openingSub === opening) {
                fail(new Error(`cannot subscribe: connection closed before the store readiness barrier${err === null ? "" : `: ${err.message}`}`));
                return;
              }
              this.onSubClose(conn, err);
            },
          },
          COMPONENT,
        );
        opening.conn = conn;
        // storeKey, NOT opts.sessionId: the store keys events by the vendor
        // uuid on the Event envelope, so subscribing under this shim's
        // `--session-id` registers on a channel nothing publishes to.
        try {
          conn.send(SubscribeSchema, create(SubscribeSchema, {
            sessionId: this.storeKey,
            fromSeq,
          }));
        } catch (err) {
          fail(new Error(`cannot subscribe: ${errText(err)}`));
          conn.close();
          return;
        }
        LOGGER.log({
          agent_repl_session_id: this.opts.sessionId,
          claude_session_id: this.storeKey,
          store_key: this.storeKey,
          from_seq: fromSeq,
        }, "subscribed to store");
      });
    });
  }

  /**
   * Where a REOPENED standing subscription resumes: after the last event this
   * client actually forwarded, else at the daemon's own handshake position.
   *
   * Only ever called with a standing subscription in existence — `relink`
   * checks `lastFromSeq` first — so the absence of both positions is a bug in
   * this state machine rather than a condition to default away.
   */
  private resumeSeq(): bigint {
    if (this.lastForwardedSeq !== null) return this.lastForwardedSeq;
    if (this.lastFromSeq !== null) return this.lastFromSeq;
    throw new Error("store-client: resume position requested with no standing subscription");
  }

  /**
   * Arm the next relink attempt. Every drop and every failed attempt funnels
   * through here, so "how often does a down link retry" is one decision in one
   * place. A deliberate close() disarms it permanently.
   */
  private armRelink(delayMs: number): void {
    if (this.closed) return;
    if (this.relinkTimer) clearTimeout(this.relinkTimer);
    this.relinkTimer = setTimeout(() => {
      this.relinkTimer = null;
      void this.relink();
    }, delayMs);
    this.relinkTimer.unref?.();
  }

  /**
   * Note a dropped connection and start recovering IMMEDIATELY (the store's
   * replacement is usually already listening — a launchd restart takes about
   * as long as one dial). Backoff only builds behind attempts that fail.
   */
  private linkLost(): void {
    this.relinkDelayMs = 0;
    this.armRelink(0);
  }

  /**
   * One recovery attempt: re-establish the producer connection, reopen the
   * standing subscription at {@link resumeSeq}, and — only once BOTH hold —
   * report the recovery that closes the daemon's fault window.
   *
   * A partial success is not a recovery and is never reported as one: the
   * attempt re-arms itself and the window stays open, which is the honest
   * account of a link that can write but has no merged tail.
   */
  private async relink(): Promise<void> {
    if (this.closed || this.relinking) return;
    this.relinking = true;
    try {
      if (!this.connected || this.conn === null) {
        await this.ensureProducerConn();
      }
      if (this.lastFromSeq !== null && this.subConn === null && this.openingSub === null) {
        await this.openSubscription(this.resumeSeq());
      }
    } catch (err) {
      this.relinkDelayMs = nextRelinkDelay(this.relinkDelayMs, this.relinkMinMs, this.relinkMaxMs);
      LOGGER.log({
        level: "error",
        agent_repl_session_id: this.opts.sessionId,
        store_key: this.storeKey,
        retry_in_ms: this.relinkDelayMs,
      }, `store relink attempt failed, retrying in ${this.relinkDelayMs}ms: ${errText(err)}`);
      this.armRelink(this.relinkDelayMs);
      return;
    } finally {
      this.relinking = false;
    }
    this.relinkDelayMs = 0;
    this.reportRecovered();
  }

  /**
   * Close the degraded window this client opened, as the `recovered: true`
   * edge of the SAME DegradedState report — the contract the daemon's fault
   * machinery closes a session_fault row on (sessioncontroller/sinks.go), and
   * the reason a recovered store link repaints the workspace without a bounce.
   *
   * Silent when no window is open, so a link that never dropped never claims
   * to have recovered.
   */
  private reportRecovered(): void {
    if (!this.linkDegraded) return;
    this.linkDegraded = false;
    const resumed = this.lastFromSeq !== null;
    const reason = resumed
      ? `store link recovered: producer connection re-established and the standing subscription resumed`
      : `store link recovered: producer connection re-established`;
    LOGGER.log({
      agent_repl_session_id: this.opts.sessionId,
      store_key: this.storeKey,
      subscribed: resumed,
      last_forwarded_seq: this.lastForwardedSeq,
    }, reason);
    const report = create(DegradedStateSchema, {
      component: COMPONENT,
      reason,
      droppedCount: 0n,
      recovered: true,
    });
    if (this.reporter) {
      this.reporter(report);
    } else {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, reason },
        `RECOVERED (no reporter bound): ${reason}`);
    }
  }

  /**
   * Close the standing subscription without reporting an outage.
   *
   * Nulling the field FIRST is what lets onSubClose tell this deliberate
   * retirement from a genuine drop — the guard there compares identity, so the
   * closed connection reports nothing. Used both by a reopen (subscribe) and
   * by a rotation, which retires the subscription of a seq space that no
   * longer exists and leaves the next handshake gate to open the new one.
   */
  private dropStandingSubscription(): void {
    if (this.openingSub !== null) {
      const opening = this.openingSub;
      this.openingSub = null;
      if (!opening.settled) {
        opening.settled = true;
        opening.reject(new Error("cannot subscribe: subscription superseded before the store readiness barrier"));
      }
      if (opening.conn !== null) opening.conn.close();
      else opening.socket.destroy();
    }
    if (this.subConn !== null) {
      const old = this.subConn;
      this.subConn = null;
      old.close();
    }
  }

  /**
   * Write one PERSISTENT batch and resolve with its StoreWriteAck. On a batch
   * the store rejects, or a connection that cannot be re-established, every
   * event is loud-logged as dropped, a DegradedState is reported, and the
   * promise rejects.
   *
   * A DOWN connection is redialed first (see ensureProducerConn), so a batch
   * arriving between a store restart and the next relink tick still reaches
   * the store rather than racing the timer. This does not soften the sad path:
   * there is still no spill buffer, a rejected batch is still never retried,
   * and a redial that fails drops the batch exactly as a down connection
   * always did.
   */
  write(events: Event[]): Promise<StoreWriteAck> {
    LOGGER.logVerbose({ agent_repl_session_id: this.opts.sessionId, store_key: this.storeKey, event_count: events.length, event_kinds: events.map(envelopeKind) }, "queueing persistent event batch for store write");
    // The ack promise is built HERE and handed to the send, because the two
    // settle on different clocks: the send chains (so batches reach the wire in
    // call order even when the first is waiting on a redial), while the ack
    // lands whenever the store gets to it. Returning the send's own promise
    // would chain the next batch behind this batch's round-trip.
    let resolve!: (ack: StoreWriteAck) => void;
    let reject!: (err: Error) => void;
    const acked = new Promise<StoreWriteAck>((res, rej) => {
      resolve = res;
      reject = rej;
    });
    const issued = this.sendChain.then(() => this.issueBatch({ resolve, reject, events }));
    // A batch that cannot be sent reports itself (loud per-event drop log,
    // DegradedState, and a rejected `acked`), so the chain carries ORDER only.
    // Recovering it here keeps one failed send from wedging every later write.
    this.sendChain = issued.catch(() => {});
    return acked;
  }

  /**
   * Redial-if-needed, then send one batch and enqueue it for its ack.
   *
   * Settles once the batch is ISSUED, never on its ack — this is what the send
   * chain waits on, so awaiting the ack here would serialize the stream on
   * store round-trips. Every failure is routed to `pending.reject` rather than
   * thrown, so a caller always learns about its own batch.
   */
  private async issueBatch(pending: PendingWrite): Promise<void> {
    try {
      await this.ensureProducerConn();
    } catch (err) {
      this.dropBatch(pending.events, `store connection is down (redial failed: ${errText(err)})`);
      pending.reject(new Error("store-client: write on a down connection"));
      return;
    }
    // A write that redialed successfully has restored HALF the link. Hand the
    // rest to the state machine rather than duplicating it here: it reopens
    // the standing subscription and reports the recovery, or re-arms if the
    // store is only partly back.
    if (this.linkDegraded) this.armRelink(0);
    // Send BEFORE enqueueing: a send that throws never reached the wire, so no
    // ack will ever match it, and an entry queued first would silently steal
    // the NEXT batch's ack. Nothing can interleave, since an ack is only ever
    // read on a later turn of the event loop.
    try {
      this.conn!.send(StoreWriteSchema, create(StoreWriteSchema, {
        producer: this.opts.producer,
        batch: create(EventBatchSchema, { events: pending.events }),
      }));
    } catch (err) {
      const why = err instanceof Error ? err.message : String(err);
      this.dropBatch(pending.events, `store write failed to send: ${why}`);
      pending.reject(new Error(`store-client: write failed to send: ${why}`));
      return;
    }
    LOGGER.logVerbose({ agent_repl_session_id: this.opts.sessionId, store_key: this.storeKey, event_count: pending.events.length, pending_acks: this.pendingWrites.length + 1 }, "persistent event batch issued to store");
    this.pendingWrites.push(pending);
  }

  /**
   * Resolve once the producer connection is live, redialing at most once per
   * outage. Concurrent writers share the one in-flight dial rather than each
   * opening a socket. NOT a retry loop: one attempt, and a failure propagates
   * to the caller, which drops the batch loudly.
   */
  private ensureProducerConn(): Promise<void> {
    if (this.connected && this.conn) return Promise.resolve();
    // A deliberate teardown is final: never redial out from under shutdown.
    // (An explicit connect() still works — only the IMPLICIT redial is gated.)
    if (this.closed) {
      return Promise.reject(new Error("store client is closed"));
    }
    if (!this.redialing) {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId }, `producer connection down — redialing ${this.opts.socketPath}`);
      this.redialing = this.connect().finally(() => {
        this.redialing = null;
      });
    }
    return this.redialing;
  }

  /** Close both connections deliberately (not an error path). */
  close(): void {
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId, connected: this.connected },
      "closing shim-store connection and disarming relink");
    this.closed = true;
    // Disarm recovery FIRST: a relink firing after teardown would redial the
    // store out from under a shim that is shutting down.
    if (this.relinkTimer) {
      clearTimeout(this.relinkTimer);
      this.relinkTimer = null;
    }
    this.stopHeartbeat();
    this.connected = false;
    this.dropStandingSubscription();
    if (this.conn) {
      this.conn.close();
      this.conn = null;
    }
    if (this.connectingSocket) {
      this.connectingSocket.destroy();
      this.connectingSocket = null;
    }
    LOGGER.log({ agent_repl_session_id: this.opts.sessionId }, "shim-store connection closed");
  }

  private onMessage(msg: Any): void {
    const ack = unpackAs(msg, StoreWriteAckSchema);
    if (ack) {
      this.onAck(ack);
      return;
    }
    if (unpackAs(msg, HeartbeatSchema)) return; // liveness only
    LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId }, `unhandled store message ${envelopeType(msg)}`);
  }

  private onSubMessage(msg: Any): void {
    const evt = unpackAs(msg, EventSchema);
    if (evt) {
      if (this.sink) {
        // Settles the store key: see adoptStoreKey.
        this.forwardedAny = true;
        // The resume position for a reopened subscription (see
        // lastForwardedSeq). Recorded BEFORE the sink so a sink that throws
        // cannot leave the client claiming an event it never counted.
        if (this.lastForwardedSeq === null || evt.seq > this.lastForwardedSeq) {
          this.lastForwardedSeq = evt.seq;
        }
        this.sink(evt);
      } else {
        LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, claude_session_id: evt.sessionId, seq: evt.seq }, `merged event dropped: no sink bound`);
      }
      return;
    }
    if (unpackAs(msg, HeartbeatSchema)) return; // liveness only
    LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId }, `unhandled store subscription message ${envelopeType(msg)}`);
  }

  private onSubClose(conn: MessageConn, err: Error | null): void {
    if (this.subConn !== conn) return; // superseded or deliberately closed
    this.subConn = null;
    // MessageConn already owns any causal framing/socket error. This reason
    // describes the semantic loss of the subscription without re-recording it.
    const reason = err ? "store subscription lost after a framing failure" : "store subscription closed";
    // The daemon's view goes stale without the tail; report honestly.
    this.degrade(reason, 0);
    // ...and then recover it. A dropped tail is never re-opened by anyone
    // else: the daemon only re-sends Subscribe when the daemon<->shim link
    // itself bounces, which a store restart does not touch.
    this.linkLost();
  }

  private onAck(ack: StoreWriteAck): void {
    const pending = this.pendingWrites.shift();
    if (!pending) {
      LOGGER.log({ level: "warn", agent_repl_session_id: this.opts.sessionId }, `received StoreWriteAck with no pending write`);
      return;
    }
    if (ack.error !== "") {
      // The store rejected the WHOLE batch (loudly, per StoreWriteAck.error).
      this.dropBatch(pending.events, `store rejected batch: ${ack.error}`);
      pending.reject(new Error(`store-client: batch rejected: ${ack.error}`));
      return;
    }
    if (ack.deduped > 0n) {
      // Expected overlap with the file plane; debug-level, not an anomaly.
      LOGGER.log({ agent_repl_session_id: this.opts.sessionId, accepted: ack.accepted, deduped: ack.deduped, last_seq: ack.lastSeq }, `batch acked`);
    }
    if (ack.deduped === 0n) {
      LOGGER.logVerbose({ agent_repl_session_id: this.opts.sessionId, accepted: ack.accepted, last_seq: ack.lastSeq, pending_acks: this.pendingWrites.length }, "store accepted persistent event batch");
    }
    pending.resolve(ack);
  }

  private onConnClose(err: Error | null): void {
    this.stopHeartbeat();
    this.connected = false;
    this.conn = null;
    // MessageConn already owns any causal framing/socket error.
    const reason = err ? "store producer connection lost after a framing failure" : "store connection closed";
    // Every in-flight write is now a dropped batch (no spill, no retry).
    const pending = this.pendingWrites.splice(0);
    for (const p of pending) {
      this.dropBatch(p.events, reason);
      p.reject(new Error(`store-client: ${reason}`));
    }
    // Report the outage even if nothing was in flight: the subscription is
    // dead, so the daemon's view is now stale until the store returns.
    this.degrade(reason, 0);
    // A write may never come — an idle session that lost its store link would
    // sit degraded forever waiting for one — so recovery is driven by the link
    // itself rather than by the next batch that happens to need it.
    this.linkLost();
  }

  /** Loud-log each dropped event and report a DegradedState. */
  private dropBatch(events: Event[], reason: string): void {
    for (const evt of events) {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, claude_session_id: evt.sessionId, seq: evt.seq, kind: envelopeKind(evt), reason }, `DROPPED event: ${reason}`);
    }
    this.degrade(reason, events.length);
  }

  /**
   * Serve one BOUNDED historical replay over a THROWAWAY store subscription.
   *
   * This is the shim's half of `ReplayRequest` (core.proto, design §5.4.1).
   * The daemon needs a range of history it no longer holds; the store already
   * serves `Subscribe{from_seq}` replay-then-live-tail, so the range comes off
   * a subscription opened for this call and closed when it ends.
   *
   * THE STANDING SUBSCRIPTION IS NEVER TOUCHED. `subConn` is the daemon's live
   * tail and its position belongs to the daemon (§4.4); serving history by
   * reopening it at a low `from_seq` would drag that position backwards and
   * re-deliver history down the live path. The whole reason this method exists
   * as a separate connection is that it cannot do that. `this.subConn` is not
   * read or written anywhere below.
   *
   * BOUNDED, and every bound that trips is REPORTED rather than absorbed:
   * - `toSeq` — the first seq the daemon's own window already covers. Reaching
   *   it is the only COMPLETE ending.
   * - `maxEvents` — a hard cap on one replay.
   * - `idleMs` — no frame within the window means the replay drained and the
   *   subscription is sitting in live-tail with nothing to say. With no
   *   `toSeq` asked for, a drained replay IS the whole answer and is complete;
   *   with one, it means the range was never reached.
   * - a closed/failed connection.
   *
   * NOT a retry path: a dial that fails is a truncated replay, reported. The
   * producer connection's redial exists because a dead producer means
   * PERMANENT event loss; a failed replay just means the frontend asks again.
   */
  replay(opts: ReplayOptions): Promise<ReplayOutcome> {
    return new Promise<ReplayOutcome>((resolve) => {
      const startedAt = Date.now();
      let delivered = 0;
      let firstSeq: bigint | undefined;
      let lastSeq: bigint | undefined;
      let settled = false;
      let idleTimer: NodeJS.Timeout | null = null;
      let conn: MessageConn | null = null;

      const finish = (truncated: boolean, reason: string): void => {
        if (settled) return;
        settled = true;
        if (idleTimer) clearTimeout(idleTimer);
        conn?.close();
        LOGGER.log({ operation: "shim.store-client.replay", level: truncated ? "error" : "info", agent_repl_session_id: this.opts.sessionId, claude_session_id: this.storeKey, store_key: this.storeKey, from_seq: opts.fromSeq, to_seq: opts.toSeq, delivered, first_seq: firstSeq, last_seq: lastSeq, elapsed_ms: Date.now() - startedAt, idle_ms: opts.idleMs, truncated, reason },
          truncated ? `replay TRUNCATED: ${reason}` : `replay complete`);
        resolve({ delivered, truncated, reason: truncated ? reason : "" });
      };

      const armIdle = (): void => {
        if (idleTimer) clearTimeout(idleTimer);
        idleTimer = setTimeout(() => {
          // No `toSeq` asked for means "stream until the replay drains", so a
          // quiet subscription is the honest complete answer, not a shortfall.
          if (opts.toSeq === 0n) finish(false, "");
          else finish(true, `store subscription idle for ${opts.idleMs}ms before reaching seq ${opts.toSeq}`);
        }, opts.idleMs);
        idleTimer.unref?.();
      };

      const socket = net.connect(this.opts.socketPath);
      const onDialError = (err: Error) => finish(true, `cannot open replay subscription: ${err.message}`);
      socket.once("error", onDialError);
      socket.once("connect", () => {
        socket.removeListener("error", onDialError);
        conn = new MessageConn(
          socket,
          {
            onMessage: (msg) => {
              const evt = unpackAs(msg, EventSchema);
              if (!evt) return; // heartbeats and anything else: not replay content
              armIdle();
              // The store replays then LIVE-TAILS; `toSeq` is where the
              // daemon's own window takes over, so that event and everything
              // after it are its business, not this replay's.
              if (opts.toSeq !== 0n && evt.seq >= opts.toSeq) {
                finish(false, "");
                return;
              }
              opts.onEvent(evt);
              delivered += 1;
              firstSeq ??= evt.seq;
              lastSeq = evt.seq;
              if (delivered === 1 || delivered % 512 === 0) {
                LOGGER.logVerbose({ operation: "shim.store-client.replay-progress", agent_repl_session_id: this.opts.sessionId, claude_session_id: this.storeKey, store_key: this.storeKey, from_seq: opts.fromSeq, to_seq: opts.toSeq, delivered, first_seq: firstSeq, last_seq: lastSeq, elapsed_ms: Date.now() - startedAt, idle_ms: opts.idleMs },
                  "replay stream made progress");
              }
              if (opts.maxEvents > 0 && delivered >= opts.maxEvents) {
                finish(true, `hit the ${opts.maxEvents}-event cap before reaching seq ${opts.toSeq}`);
              }
            },
            // MessageConn owns the causal error; replay owns only truncation.
            onClose: (err) => finish(true, err ? "replay subscription lost after a framing failure" : "store closed the replay subscription"),
          },
          COMPONENT,
        );
        // storeKey, not opts.sessionId: the store keys events by the vendor
        // uuid on the Event envelope (see storeKey), so any other id
        // subscribes to a channel nothing publishes to.
        conn.send(SubscribeSchema, create(SubscribeSchema, {
          sessionId: this.storeKey,
          fromSeq: opts.fromSeq,
        }));
        LOGGER.log({ operation: "shim.store-client.replay", agent_repl_session_id: this.opts.sessionId, claude_session_id: this.storeKey, store_key: this.storeKey, from_seq: opts.fromSeq, to_seq: opts.toSeq, max_events: opts.maxEvents, idle_ms: opts.idleMs },
          `opened throwaway replay subscription (standing subscription untouched)`);
        armIdle();
      });
    });
  }

  private degrade(reason: string, droppedCount: number): void {
    // Opens the window reportRecovered closes. Set on EVERY degrade, including
    // a store-rejected batch: the daemon holds one fault row per component, so
    // whatever opened it, the next genuine link recovery is what closes it.
    this.linkDegraded = true;
    const report = create(DegradedStateSchema, {
      component: COMPONENT,
      reason,
      droppedCount: BigInt(droppedCount),
      recovered: false,
    });
    if (this.reporter) {
      this.reporter(report);
    } else {
      LOGGER.log({ level: "error", agent_repl_session_id: this.opts.sessionId, reason }, `DEGRADED (no reporter bound): ${reason}`);
    }
  }

  private startHeartbeat(): void {
    if (this.heartbeatIntervalMs <= 0) return;
    this.stopHeartbeat();
    this.heartbeatTimer = setInterval(() => {
      const hb = create(HeartbeatSchema, { sentAtMs: BigInt(Date.now()) } as Heartbeat);
      this.conn?.send(HeartbeatSchema, hb);
      this.subConn?.send(HeartbeatSchema, hb);
    }, this.heartbeatIntervalMs);
    this.heartbeatTimer.unref?.();
  }

  private stopHeartbeat(): void {
    if (this.heartbeatTimer) {
      clearInterval(this.heartbeatTimer);
      this.heartbeatTimer = null;
    }
  }
}

/** Best-effort event-kind label for drop logs (the payload oneof case). */
function envelopeKind(evt: Event): string {
  return evt.payload.case ?? "unknown";
}

/** The message of a thrown value, whatever it was thrown as. */
function errText(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}

/** Double `delayMs` from `minMs` up to the `maxMs` ceiling. */
function nextRelinkDelay(delayMs: number, minMs: number, maxMs: number): number {
  if (delayMs <= 0) return minMs;
  return Math.min(delayMs * 2, maxMs);
}
