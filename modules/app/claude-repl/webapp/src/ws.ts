/**
 * WsClient — thin reconnecting WebSocket transport. All protocol logic
 * (gap detection, replay) lives in the store; the client just shuttles
 * raw messages and outbound commands.
 */
import { ClientCommand } from "./protocol.js";

export interface WsClientOptions {
  url: string;
  /** Raw inbound text message. Return value is a command to send back. */
  onMessage: (data: string) => ClientCommand | undefined;
  onStatusChange?: (connected: boolean) => void;
  /** WebSocket constructor (injectable for tests). */
  wsFactory?: (url: string) => WebSocket;
  /** Reconnect backoff schedule in ms; last entry repeats. */
  backoffMs?: number[];
}

const DEFAULT_BACKOFF_MS = [250, 500, 1000, 2000, 5000];

export class WsClient {
  private readonly opts: WsClientOptions;
  private readonly backoff: number[];
  private ws: WebSocket | null = null;
  private attempts = 0;
  private closedByUser = false;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;

  constructor(opts: WsClientOptions) {
    this.opts = opts;
    this.backoff = opts.backoffMs ?? DEFAULT_BACKOFF_MS;
  }

  connect(): void {
    this.closedByUser = false;
    const factory = this.opts.wsFactory ?? ((url: string) => new WebSocket(url));
    const ws = factory(this.opts.url);
    this.ws = ws;

    ws.onopen = () => {
      this.attempts = 0;
      this.opts.onStatusChange?.(true);
    };
    ws.onmessage = (event: MessageEvent) => {
      const reply = this.opts.onMessage(String(event.data));
      if (reply) this.send(reply);
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
    this.reconnectTimer = setTimeout(() => this.connect(), delay);
  }

  send(cmd: ClientCommand): boolean {
    if (this.ws === null || this.ws.readyState !== WebSocket.OPEN) {
      return false;
    }
    this.ws.send(JSON.stringify(cmd));
    return true;
  }

  close(): void {
    this.closedByUser = true;
    if (this.reconnectTimer !== null) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }
    this.ws?.close();
  }
}
