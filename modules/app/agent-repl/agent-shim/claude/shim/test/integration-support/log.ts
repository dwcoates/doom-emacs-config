/**
 * test/integration-support/log.ts — the shim's durable record, read as evidence.
 *
 * The shim writes one JSON object per line to the descriptor it was handed as
 * fd 3 (`src/log.ts`). The integration suite treats that record as part of the
 * contract: it is where a fatal startup refusal, a refused SIGINT and a
 * poisoned sink are OBSERVABLE from outside the process, and none of those has
 * an rpc.
 *
 * # Why fs.watch and never a poll
 *
 * A polling loop turns "the shim logged X" into "the shim logged X within the
 * poll budget", which is a different assertion and a flaky one. Every wait here
 * is an EVENT: the kernel tells us the file grew, we read the bytes that
 * appeared, and any predicate that now matches resolves. The initial drain
 * before installing the watcher closes the only race — a record written before
 * the watcher existed is already in the buffer by the time anyone waits on it.
 */
import { closeSync, openSync, readSync, statSync, watch, type FSWatcher } from "node:fs";

/** One record, in the shape `src/log.ts` writes. */
export interface LogRecord {
  readonly timestamp: string;
  readonly runtime: string;
  readonly level: "debug" | "info" | "warn" | "error";
  readonly operation: string;
  readonly message: string;
  readonly context: Record<string, unknown>;
  readonly pid: number;
  readonly agent_repl_session_id: string;
  readonly claude_session_id?: string;
}

/** What a caller waits on. Returning true settles the wait with that record. */
export type LogPredicate = (record: LogRecord) => boolean;

/**
 * A live view of one shim's records, however they reach us.
 *
 * Two implementations exist because fd 3 is a FILE in almost every test and a
 * PIPE in the one test whose subject is a poisoned sink; the suites read both
 * through this one interface so only that test knows the difference.
 */
export interface RecordSink {
  /** Every record seen so far, in the order the shim wrote them. */
  records(): LogRecord[];
  /** The first record matching `predicate`, awaiting one if none has arrived. */
  record(predicate: LogPredicate): Promise<LogRecord>;
  /** Stop reading. */
  close(): void;
}

interface Waiter {
  readonly predicate: LogPredicate;
  readonly resolve: (record: LogRecord) => void;
}

/**
 * A live view of one shim's durable log file.
 *
 * Records already written are readable synchronously ({@link records}); records
 * not written yet are awaited ({@link record}).
 */
export class LogTail implements RecordSink {
  private readonly parsed: LogRecord[] = [];
  private readonly waiters: Waiter[] = [];
  private readonly fd: number;
  private watcher: FSWatcher | null = null;
  private offset = 0;
  private carry = "";
  private closed = false;

  private constructor(readonly path: string) {
    this.fd = openSync(path, "r");
  }

  /** Begin tailing a log file that already exists (the harness creates it). */
  static open(path: string): LogTail {
    const tail = new LogTail(path);
    tail.drain();
    tail.watcher = watch(path, () => tail.drain());
    return tail;
  }

  /** Every record read so far, in the order the shim wrote them. */
  records(): LogRecord[] {
    this.drain();
    return [...this.parsed];
  }

  /**
   * The first record matching `predicate`, awaiting one if none has arrived.
   *
   * There is deliberately no timeout argument: the suite's own per-test timeout
   * is the only deadline, so a hang reports as the test that hung rather than
   * as a helper that gave up early with a message of its own.
   */
  async record(predicate: LogPredicate): Promise<LogRecord> {
    this.drain();
    const already = this.parsed.find(predicate);
    if (already !== undefined) return already;
    return new Promise<LogRecord>((resolve) => {
      this.waiters.push({ predicate, resolve });
    });
  }

  /** Stop watching and release the descriptor. */
  close(): void {
    if (this.closed) return;
    this.closed = true;
    this.watcher?.close();
    this.watcher = null;
    closeSync(this.fd);
  }

  /** Read whatever bytes appeared since the last read and parse whole lines. */
  private drain(): void {
    if (this.closed) return;
    const size = statSync(this.path).size;
    while (this.offset < size) {
      const buffer = Buffer.alloc(Math.min(64 * 1024, size - this.offset));
      const read = readSync(this.fd, buffer, 0, buffer.length, this.offset);
      if (read <= 0) break;
      this.offset += read;
      this.carry += buffer.subarray(0, read).toString("utf8");
    }
    const lines = this.carry.split("\n");
    // The last element is either "" (the file ends on a newline) or a partial
    // line the writer has not finished; either way it is carried, never parsed.
    this.carry = lines.pop() ?? "";
    for (const line of lines) {
      if (line.trim() === "") continue;
      let record: LogRecord;
      try {
        record = JSON.parse(line) as LogRecord;
      } catch {
        // A line the shim could not write whole is evidence of nothing; the
        // next drain will see the rest of it only if the writer completes it.
        continue;
      }
      this.parsed.push(record);
      this.settle(record);
    }
  }

  private settle(record: LogRecord): void {
    for (let index = this.waiters.length - 1; index >= 0; index--) {
      const waiter = this.waiters[index];
      if (waiter === undefined || !waiter.predicate(record)) continue;
      this.waiters.splice(index, 1);
      waiter.resolve(record);
    }
  }
}

/** A predicate matching a record by its stable operation label. */
export function operationIs(operation: string): LogPredicate {
  return (record) => record.operation === operation;
}

/** A predicate matching a record by one of its context fields. */
export function contextIs(field: string, value: unknown): LogPredicate {
  return (record) => record.context[field] === value;
}

/** A predicate requiring every one of the given predicates. */
export function allOf(...predicates: LogPredicate[]): LogPredicate {
  return (record) => predicates.every((predicate) => predicate(record));
}

/**
 * The same view, fed by a STREAM rather than a file.
 *
 * Used when fd 3 is a pipe: the parent holds the read end, so the records
 * arrive as data events. Nothing here waits on a clock either — a record is
 * parsed when its line arrives.
 */
export class StreamRecords implements RecordSink {
  private readonly parsed: LogRecord[] = [];
  private readonly waiters: Waiter[] = [];
  private carry = "";

  constructor(private readonly source: NodeJS.ReadableStream) {
    source.on("data", (chunk: Buffer | string) => {
      this.consume(typeof chunk === "string" ? chunk : chunk.toString("utf8"));
    });
  }

  records(): LogRecord[] {
    return [...this.parsed];
  }

  async record(predicate: LogPredicate): Promise<LogRecord> {
    const already = this.parsed.find(predicate);
    if (already !== undefined) return already;
    return new Promise<LogRecord>((resolve) => {
      this.waiters.push({ predicate, resolve });
    });
  }

  close(): void {
    // Destroying the READ end is what poisons the writer's sink, so this is the
    // one close with an observable effect on the shim; the tests that want that
    // effect call it deliberately.
    const maybe = this.source as unknown as { destroy?: () => void };
    if (typeof maybe.destroy === "function") maybe.destroy();
  }

  private consume(text: string): void {
    this.carry += text;
    const lines = this.carry.split("\n");
    this.carry = lines.pop() ?? "";
    for (const line of lines) {
      if (line.trim() === "") continue;
      let record: LogRecord;
      try {
        record = JSON.parse(line) as LogRecord;
      } catch {
        continue;
      }
      this.parsed.push(record);
      for (let index = this.waiters.length - 1; index >= 0; index--) {
        const waiter = this.waiters[index];
        if (waiter === undefined || !waiter.predicate(record)) continue;
        this.waiters.splice(index, 1);
        waiter.resolve(record);
      }
    }
  }
}

/**
 * Parse the JSON records out of a captured stderr stream.
 *
 * A startup refusal happens BEFORE the durable sink is configured, so its
 * record has nowhere to go but stderr (`log.ts`'s emergency path). That makes
 * stderr the only place the missing-variable refusals are observable, and this
 * is how they are read.
 */
export function parseRecords(text: string): LogRecord[] {
  return text
    .split("\n")
    .filter((line) => line.trim().startsWith("{"))
    .flatMap((line) => {
      try {
        return [JSON.parse(line) as LogRecord];
      } catch {
        return [];
      }
    });
}
