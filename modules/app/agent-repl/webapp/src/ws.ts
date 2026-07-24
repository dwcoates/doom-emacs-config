/**
 * WsClient — thin reconnecting WebSocket transport. All protocol logic
 * (gap detection, replay) lives in the store; the client just shuttles raw
 * inbound frames to `onMessage` and sends pre-encoded outbound frames (the
 * caller encodes `FrontendCommand` protojson via command-dispatch.ts).
 */

export interface WsClientOptions {
  url: string;
  /** Raw inbound text frame; the caller decodes + routes it. */
  onMessage: (data: string) => void;
  onStatusChange?: (connected: boolean) => void;
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
   * Diagnostic sink. The one caller today is the dropped-reply branch:
   * a store-generated command (typically a gap's replay-request) that
   * could not be sent because the socket was not OPEN previously
   * vanished without a trace — and an unsent replay-request is a feed
   * that never un-wedges.
   */
  log?: (message: string) => void;
}

const DEFAULT_BACKOFF_MS = [250, 500, 1000, 2000, 5000];

export class WsClient {
  private readonly opts: WsClientOptions;
  private readonly backoff: number[];
  private ws: WebSocket | null = null;
  private attempts = 0;
  private closedByUser = false;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;
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
    this.closedByUser = false;
    const factory = this.opts.wsFactory ?? ((url: string) => new WebSocket(url));
    const ws = factory(this.opts.url);
    this.ws = ws;

    ws.onopen = () => {
      this.attempts = 0;
      this.opts.onStatusChange?.(true);
    };
    ws.onmessage = (event: MessageEvent) => {
      this.opts.onMessage(String(event.data));
    };
    ws.onclose = () => {
      this.opts.onStatusChange?.(false);
      this.ws = null;
      if (!this.closedByUser) this.scheduleReconnect();
    };
    ws.onerror = () => {
      // onclose always follows onerror; reconnect is handled there.
    };
  }

  private scheduleReconnect(): void {
    const delay = this.backoff[Math.min(this.attempts, this.backoff.length - 1)];
    this.attempts++;
    const epoch = this.epoch;
    this.reconnectTimer = setTimeout(() => {
      void this.reconnectIfSessionExists(epoch);
    }, delay);
  }

  private async reconnectIfSessionExists(epoch: number): Promise<void> {
    if (this.epoch !== epoch || this.closedByUser) return;
    if (this.opts.sessionExists) {
      let exists = true;
      try {
        exists = await this.opts.sessionExists();
      } catch (err) {
        // Probe unreachable (daemon briefly down?): treat as unknown
        // and keep retrying — only a definitive "not listed" stops us.
        this.opts.log?.(
          `ws: session-exists probe failed: ${String(err)} — treating as unknown, will retry`,
        );
      }
      // Re-check after the await: a close()/connect() during the probe
      // moves the epoch, and acting on the stale result would open a
      // duplicate socket (or fire onGone against a fresh connection).
      if (this.epoch !== epoch || this.closedByUser) return;
      if (!exists) {
        this.opts.log?.("ws: session gone — stopping reconnect");
        this.opts.onGone?.();
        return;
      }
    }
    this.connect();
  }

  /** Send one pre-encoded frame; false when the socket is not open. */
  send(raw: string): boolean {
    if (this.ws === null || this.ws.readyState !== WebSocket.OPEN) {
      return false;
    }
    this.ws.send(raw);
    return true;
  }

  close(): void {
    this.epoch++;
    this.closedByUser = true;
    if (this.reconnectTimer !== null) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    this.ws?.close();
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
