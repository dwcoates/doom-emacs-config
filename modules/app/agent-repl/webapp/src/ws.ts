/**
 * WsClient — thin reconnecting WebSocket transport. All protocol logic
 * (gap detection, replay) lives in the store; the client just shuttles raw
 * inbound frames to `onMessage` and sends pre-encoded outbound frames (the
 * caller encodes `FrontendCommand` protojson via command-dispatch.ts).
 */
import { log, logVerbose } from "./wslog.js";

export interface WsClientOptions {
  url: string;
  /** Raw inbound text frame; the caller decodes + routes it. */
  onMessage: (data: string) => void;
  onStatusChange?: (connected: boolean) => void;
  /** State freshness, distinct from the browser socket's open bit. */
  onFreshnessChange?: (freshness: WsStateFreshness) => void;
  /** WebSocket constructor (injectable for tests). */
  wsFactory?: (url: string) => WebSocket;
  /** Reconnect backoff schedule in ms; last entry repeats. */
  backoffMs?: number[];
  /**
   * Pre-reconnect probe: resolves false when the session is gone
   * (e.g. GET /sessions no longer lists it), which stops the reconnect
   * loop and fires onGone instead of retrying forever against a 404.
   * A probe failure (network error) counts as "unknown" and retries.
   */
  sessionExists?: () => Promise<boolean>;
  /** Fired once when sessionExists reports the session is gone. */
  onGone?: () => void;
  /**
   * The daemon became unreachable (F4): the socket closed for a reason the
   * user did not ask for. Carries the close CODE and REASON, which are the
   * only evidence separating a daemon restart from a network drop — this
   * handler used to read neither.
   */
  onUnreachable?: (code: number, reason: string) => void;
  /**
   * The daemon became reachable again, closing the window `onUnreachable`
   * opened. Fired only after an actual reconnect, so a first connection never
   * reports a recovery from nothing.
   */
  onReachable?: () => void;
  /** Maximum gap between authoritative StateSnapshots. */
  snapshotTimeoutMs?: number;
}

const DEFAULT_BACKOFF_MS = [250, 500, 1000, 2000, 5000];
export const DEFAULT_SNAPSHOT_TIMEOUT_MS = 45_000;

export type WsStateFreshness =
  | "connecting"
  | "awaiting_snapshot"
  | "current"
  | "disconnected"
  | "expired";

export class WsClient {
  private readonly opts: WsClientOptions;
  private readonly backoff: number[];
  private ws: WebSocket | null = null;
  private attempts = 0;
  private closedByUser = false;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
  private snapshotTimer: ReturnType<typeof setTimeout> | null = null;
  private freshness: WsStateFreshness = "disconnected";
  private pendingReachable = false;
  /**
   * Connection epoch: bumped by every connect() and close(). Scheduled
   * reconnects and in-flight existence probes capture the epoch they
   * were started under and abort when it has moved on — otherwise a
   * stale probe resuming after a close()+connect() pair would open a
   * SECOND socket alongside the fresh one.
   */
  private epoch = 0;

  constructor(opts: WsClientOptions) {
    this.opts = opts;
    this.backoff = opts.backoffMs ?? DEFAULT_BACKOFF_MS;
  }

  connect(): void {
    this.epoch++;
    const connectionEpoch = this.epoch;
    this.closedByUser = false;
    this.transitionFreshness("connecting", { epoch: connectionEpoch });
    log("info", "websocket connection attempt started", {
      operation: "ws.connect",
      context: { url: this.opts.url, epoch: connectionEpoch, reconnect_attempt_count: this.attempts },
    });
    const factory = this.opts.wsFactory ?? ((url: string) => new WebSocket(url));
    const ws = factory(this.opts.url);
    this.ws = ws;

    ws.onopen = () => {
      if (!this.isCurrentSocket(ws, connectionEpoch)) {
        this.logStaleCallback("open", connectionEpoch);
        return;
      }
      // Socket-open begins synchronization. Recovery is not reported until
      // adoptSnapshot proves that current authoritative state landed.
      const reconnected = this.attempts > 0;
      this.pendingReachable = reconnected;
      this.attempts = 0;
      log("info", "websocket connection opened", {
        operation: "ws.open",
        context: { url: this.opts.url, epoch: connectionEpoch, reconnected },
      });
      this.transitionFreshness("awaiting_snapshot", { epoch: connectionEpoch, reconnected });
      this.armSnapshotExpiry(connectionEpoch);
    };
    ws.onmessage = (event: MessageEvent) => {
      if (!this.isCurrentSocket(ws, connectionEpoch)) {
        this.logStaleCallback("message", connectionEpoch);
        return;
      }
      // WebSocket frames can arrive many times per second during replay.
      // localOnly is a STRUCTURAL NO-REENTRY BOUNDARY, not volume control: a
      // forwarded record here becomes a clientLog command, whose commandAck is
      // itself an inbound frame that re-enters this handler — a perpetual 1:1
      // ack/log loop (observed live at ~60 commands/s for two days).
      logVerbose("info", "websocket received frame", {
        operation: "ws.message",
        localOnly: true,
        context: { epoch: connectionEpoch, byte_length: String(event.data).length, freshness: this.freshness },
      });
      this.opts.onMessage(String(event.data));
    };
    ws.onclose = (event: CloseEvent) => {
      if (!this.isCurrentSocket(ws, connectionEpoch)) {
        this.logStaleCallback("close", connectionEpoch);
        return;
      }
      this.clearSnapshotExpiry();
      if (this.freshness !== "expired") this.opts.onStatusChange?.(false);
      this.ws = null;
      if (this.closedByUser) {
        this.transitionFreshness("disconnected", { epoch: connectionEpoch, code: event.code, reason: event.reason, owner: "user" });
        log("info", "websocket closed by user", {
          operation: "ws.close-user",
          context: { epoch: connectionEpoch, code: event.code, reason: event.reason },
        });
        return;
      }
      if (this.freshness !== "expired") {
        this.transitionFreshness("disconnected", { epoch: connectionEpoch, code: event.code, reason: event.reason, owner: "transport" });
      }
      // The close CODE and REASON are the only evidence separating a daemon
      // that restarted from a network drop, and this handler used to read
      // neither — which is how "reconnecting…" became the webapp's single
      // answer to every transport fault. A user-initiated close is not a
      // failure and is deliberately above this line.
      this.opts.onUnreachable?.(event.code, event.reason);
      log("warn", "websocket closed unexpectedly and will reconnect", {
        operation: "ws.close-unexpected",
        context: { epoch: connectionEpoch, code: event.code, reason: event.reason, reconnect_attempt_count: this.attempts },
      });
      this.scheduleReconnect();
    };
    ws.onerror = () => {
      // onclose always follows onerror; the classification happens there,
      // where the code and reason are available. An error handler that
      // classified on its own would double-report every drop.
    };
  }

  /**
   * Attest that the current connection's StateSnapshot decoded and fully
   * ingested. Socket-open alone never calls this and therefore never makes the
   * UI current. Every renewed snapshot extends the same bounded lease.
   */
  adoptSnapshot(revision: Record<string, unknown> = {}): void {
    if (
      this.ws === null ||
      this.ws.readyState !== WebSocket.OPEN ||
      (this.freshness !== "awaiting_snapshot" && this.freshness !== "current")
    ) {
      log("error", "websocket snapshot adoption rejected outside a live snapshot-bearing connection", {
        operation: "ws.snapshot-adoption-rejected",
        context: { epoch: this.epoch, freshness: this.freshness, ready_state: this.ws?.readyState ?? -1, ...revision },
      });
      throw new Error(`ws: cannot adopt snapshot while freshness=${this.freshness}`);
    }
    const renewed = this.freshness === "current";
    this.armSnapshotExpiry(this.epoch);
    if (renewed) {
      logVerbose("info", "websocket authoritative snapshot lease renewed", {
        operation: "ws.snapshot-renewed",
        context: { epoch: this.epoch, timeout_ms: this.snapshotTimeoutMs(), ...revision },
      });
      return;
    }
    this.transitionFreshness("current", { epoch: this.epoch, timeout_ms: this.snapshotTimeoutMs(), ...revision });
    this.opts.onStatusChange?.(true);
    if (this.pendingReachable) {
      this.pendingReachable = false;
      this.opts.onReachable?.();
    }
  }

  private isCurrentSocket(ws: WebSocket, epoch: number): boolean {
    return this.ws === ws && this.epoch === epoch;
  }

  private logStaleCallback(callback: string, callbackEpoch: number): void {
    logVerbose("warn", "websocket callback ignored after connection generation advanced", {
      operation: "ws.stale-callback",
      context: { callback, callback_epoch: callbackEpoch, current_epoch: this.epoch, freshness: this.freshness },
    });
  }

  private transitionFreshness(next: WsStateFreshness, context: Record<string, unknown>): void {
    const previous = this.freshness;
    if (previous === next) return;
    this.freshness = next;
    log(next === "expired" ? "error" : "info", "websocket state freshness changed", {
      operation: "ws.freshness-changed",
      context: { previous, next, ...context },
    });
    this.opts.onFreshnessChange?.(next);
  }

  private snapshotTimeoutMs(): number {
    return this.opts.snapshotTimeoutMs ?? DEFAULT_SNAPSHOT_TIMEOUT_MS;
  }

  private armSnapshotExpiry(epoch: number): void {
    this.clearSnapshotExpiry();
    const timeoutMs = this.snapshotTimeoutMs();
    if (timeoutMs <= 0) {
      log("error", "websocket snapshot timeout configuration rejected", {
        operation: "ws.snapshot-timeout-invalid",
        context: { epoch, timeout_ms: timeoutMs },
      });
      throw new Error(`ws: snapshotTimeoutMs must be positive, got ${timeoutMs}`);
    }
    this.snapshotTimer = setTimeout(() => {
      if (epoch !== this.epoch || this.closedByUser) return;
      if (this.freshness !== "awaiting_snapshot" && this.freshness !== "current") return;
      this.transitionFreshness("expired", { epoch, timeout_ms: timeoutMs });
      this.opts.onStatusChange?.(false);
      // Force a new connection because only its first successfully ingested
      // StateSnapshot may restore currentness.
      this.ws?.close();
    }, timeoutMs);
  }

  private clearSnapshotExpiry(): void {
    if (this.snapshotTimer === null) return;
    clearTimeout(this.snapshotTimer);
    this.snapshotTimer = null;
  }

  private scheduleReconnect(): void {
    const delay = this.backoff[Math.min(this.attempts, this.backoff.length - 1)];
    this.attempts++;
    const epoch = this.epoch;
    log("info", "websocket reconnect scheduled", {
      operation: "ws.reconnect-scheduled",
      context: { epoch, delay_ms: delay, reconnect_attempt_count: this.attempts },
    });
    this.reconnectTimer = setTimeout(() => {
      void this.reconnectIfSessionExists(epoch);
    }, delay);
  }

  private async reconnectIfSessionExists(epoch: number): Promise<void> {
    if (this.epoch !== epoch || this.closedByUser) {
      logVerbose("info", "websocket reconnect abandoned after connection state changed", {
        operation: "ws.reconnect-abandoned",
        context: { scheduled_epoch: epoch, current_epoch: this.epoch, closed_by_user: this.closedByUser },
      });
      return;
    }
    if (this.opts.sessionExists) {
      let exists = true;
      try {
        exists = await this.opts.sessionExists();
      } catch (err) {
        // Probe unreachable (daemon briefly down?): treat as unknown
        // and keep retrying — only a definitive "not listed" stops us.
        log("error", "websocket session-exists probe failed before reconnect", {
          operation: "ws.session-exists-probe-failed",
          context: { scheduled_epoch: epoch, cause: err },
        });
      }
      // Re-check after the await: a close()/connect() during the probe
      // moves the epoch, and acting on the stale result would open a
      // duplicate socket (or fire onGone against a fresh connection).
      if (this.epoch !== epoch || this.closedByUser) {
        logVerbose("info", "websocket reconnect probe result became stale", {
          operation: "ws.reconnect-probe-stale",
          context: { scheduled_epoch: epoch, current_epoch: this.epoch, closed_by_user: this.closedByUser, session_exists: exists },
        });
        return;
      }
      if (!exists) {
        log("warn", "websocket session no longer exists and reconnect stopped", {
          operation: "ws.session-gone",
          context: { scheduled_epoch: epoch },
        });
        this.opts.onGone?.();
        return;
      }
    }
    this.connect();
  }

  /** Send one pre-encoded frame; false when the socket is not open. */
  send(raw: string): boolean {
    // STRUCTURAL NO-REENTRY BOUNDARY. Canonical logs are themselves encoded
    // commands sent through this method. Logging either branch here would call
    // ForwardingLogger -> CommandDispatcher.clientLog -> WsClient.send again.
    // The command owner records an ordinary send refusal. The logger retains
    // its own refused frame in its bounded forwarding queue.
    if (this.ws === null || this.ws.readyState !== WebSocket.OPEN || this.freshness !== "current") return false;
    this.ws.send(raw);
    return true;
  }

  close(): void {
    const wasCurrent = this.freshness === "current";
    const ws = this.ws;
    this.epoch++;
    this.closedByUser = true;
    this.pendingReachable = false;
    this.clearSnapshotExpiry();
    log("info", "websocket close requested", {
      operation: "ws.close-requested",
      context: { epoch: this.epoch, had_socket: ws !== null, had_reconnect_timer: this.reconnectTimer !== null },
    });
    if (this.reconnectTimer !== null) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    this.ws = null;
    if (wasCurrent) this.opts.onStatusChange?.(false);
    this.transitionFreshness("disconnected", { epoch: this.epoch, owner: "user" });
    ws?.close();
  }
}

/**
 * Whether the page should show its own composer. `?composer=0` is set
 * by hosts that own input themselves (the Emacs hybrid UI, where the
 * input panel below the webview submits over HTTP) — the webview then
 * stays output-only, which also sidesteps webview focus capture.
 */
export function composerEnabled(params: URLSearchParams): boolean {
  return params.get("composer") !== "0";
}

/**
 * Build the standard sessionExists probe against GET /sessions.
 * Extracted from the boot path so the "session gone" detection is unit
 * testable; throws on a non-2xx response so transport failures count as
 * "unknown" (retry) rather than "gone".
 */
export function makeSessionExistsProbe(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): () => Promise<boolean> {
  return async () => {
    const resp = await fetchFn(`${httpBase}/sessions`);
    if (!resp.ok) throw new Error(`GET /sessions: ${resp.status}`);
    const body = (await resp.json()) as { sessions: Array<{ session_id: string }> };
    return body.sessions.some((s) => s.session_id === sessionId);
  };
}
