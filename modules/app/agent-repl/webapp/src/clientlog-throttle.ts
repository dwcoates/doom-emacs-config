/**
 * Client→daemon log forwarding rate control.
 *
 * WHY THIS EXISTS: every forwarded record is its own `client_log` command, and
 * a `client_log` carries the workspace it came from, so it rides that
 * workspace's per-workspace command lane. Sixteen refreshed webviews echoing
 * their console records drove one connection's command queue to 2,355 entries,
 * and every interactive command for those workspaces — opens, prompts,
 * resyncs — waited the console noise out.
 *
 * The daemon end now serves client logs from a low-priority sublane, so a
 * burst can no longer delay an interactive command. This end stops producing
 * the burst in the first place, which is what keeps the daemon's queue, its
 * ack bookkeeping and the on-disk log proportional to what actually happened.
 *
 * WHY THROTTLING RATHER THAN BATCHING: `ClientLogRecord` carries ONE record. A
 * batch would need a repeated field. Changing the wire contract to save frames
 * is a worse trade than sending the same frames at a bounded rate, so complete
 * records are buffered and released at a cap instead. Keeping the generated
 * record whole also preserves the client's timestamp and verbosity across the
 * throttle window.
 *
 * NOTHING IS SILENTLY LOST. A record is dropped only when the buffer's hard
 * bound is already full, and every drop is counted and reported to the daemon
 * as its own record at the next flush.
 */
import type { ClientLogRecord } from "../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";

/** Pushes one record toward the daemon; false means the socket refused it. */
export type ClientLogSend = (record: ClientLogRecord) => boolean;

export interface ClientLogThrottleOptions {
  send: ClientLogSend;
  /** Build the canonical warning that accounts for records the buffer lost. */
  droppedRecord: (dropped: number, bufferBound: number) => ClientLogRecord;
  /** Longest a buffered record waits before its flush. */
  intervalMs?: number;
  /** Records released per flush, and the count that triggers an early one. */
  maxBatch?: number;
  /** Hard buffer bound; arrivals beyond it are dropped and counted. */
  maxBuffer?: number;
  /** Injected for tests; defaults to the DOM timer functions. */
  setTimer?: (fn: () => void, ms: number) => unknown;
  clearTimer?: (handle: unknown) => void;
}

/** At most 50 records every 2s, which is 25/s of steady-state ceiling. */
const DEFAULT_INTERVAL_MS = 2000;
const DEFAULT_MAX_BATCH = 50;
const DEFAULT_MAX_BUFFER = 500;

export class ClientLogThrottle {
  private readonly buffer: ClientLogRecord[] = [];
  private readonly intervalMs: number;
  private readonly maxBatch: number;
  private readonly maxBuffer: number;
  private readonly setTimer: (fn: () => void, ms: number) => unknown;
  private readonly clearTimer: (handle: unknown) => void;
  private timer: unknown = null;
  private dropped = 0;

  constructor(private readonly options: ClientLogThrottleOptions) {
    this.intervalMs = options.intervalMs ?? DEFAULT_INTERVAL_MS;
    this.maxBatch = options.maxBatch ?? DEFAULT_MAX_BATCH;
    this.maxBuffer = options.maxBuffer ?? DEFAULT_MAX_BUFFER;
    this.setTimer = options.setTimer ?? ((fn, ms) => setTimeout(fn, ms));
    this.clearTimer = options.clearTimer ?? ((handle) => clearTimeout(handle as ReturnType<typeof setTimeout>));
  }

  /**
   * Accept one record for forwarding. Returns whether the throttle took
   * responsibility for it — false is a genuine loss (the buffer was full), and
   * the caller's local console record is then the only copy.
   *
   * An ERROR flushes immediately, buffer included: an error is the record
   * someone is going to go looking for, and it must not sit behind a two second
   * window that a crashing page may never reach the end of.
   */
  write(record: ClientLogRecord): boolean {
    if (this.buffer.length >= this.maxBuffer) {
      this.dropped += 1;
      return false;
    }
    this.buffer.push(record);
    if (record.level.case === "error" || this.buffer.length >= this.maxBatch) {
      this.flush();
      return true;
    }
    this.arm();
    return true;
  }

  /**
   * Release up to maxBatch buffered records, oldest first, preceded by the
   * drop summary when this window lost anything. A record the send path
   * refuses stays at the head for the next flush: the transport failed, the
   * evidence did not stop being owed.
   */
  flush(): void {
    this.disarm();
    if (this.dropped > 0) {
      const dropped = this.dropped;
      this.dropped = 0;
      const reported = this.options.send(this.options.droppedRecord(dropped, this.maxBuffer));
      // A refused summary is a transport failure, not permission to forget how
      // much was lost: the count goes back and is reported by a later flush.
      if (!reported) this.dropped += dropped;
    }
    let released = 0;
    while (this.buffer.length > 0 && released < this.maxBatch) {
      const record = this.buffer[0];
      if (!this.options.send(record)) break;
      this.buffer.shift();
      released += 1;
    }
    // Records left over — a burst past one batch, or a refusal above — still
    // owe a flush, so the window re-arms rather than waiting on the next write.
    if (this.buffer.length > 0 || this.dropped > 0) this.arm();
  }

  /** Records waiting for their window. */
  bufferedCount(): number {
    return this.buffer.length;
  }

  /** Records lost to the buffer bound and not yet reported. */
  droppedCount(): number {
    return this.dropped;
  }

  private arm(): void {
    if (this.timer !== null) return;
    this.timer = this.setTimer(() => {
      this.timer = null;
      this.flush();
    }, this.intervalMs);
  }

  private disarm(): void {
    if (this.timer === null) return;
    this.clearTimer(this.timer);
    this.timer = null;
  }
}
