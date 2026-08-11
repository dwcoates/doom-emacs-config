/**
 * THE DURABLE SPILL: the on-disk record of persistent batches this shim has
 * accepted but the store has not yet acknowledged.
 *
 * WHY IT EXISTS. A store bounce takes the producer connection down under a
 * live shim. Batches that arrive during the outage, and batches that were on
 * the wire when it dropped, are both work this shim ACCEPTED from the SDK
 * stream and owes to the store. Holding them in memory covers a store bounce;
 * it does not cover this shim dying while it holds them, and it makes the size
 * of the outage a question about heap. Writing them here first makes the hold a
 * fact about the filesystem instead: whatever kills the shim, the batches are
 * still on disk and the next shim for this workspace replays them.
 *
 * WHY REPLAYING IT IS SAFE. Every event carries a stable `write_id` minted once
 * by the producer (core.proto Event.write_id), and the store enforces a unique
 * (session_id, write_id). A batch replayed from here after it already landed is
 * absorbed as a no-op rather than written a second time, so the journal never
 * has to know whether a record made it — it only has to not lose one. That is
 * the whole reason the aggressive replay this file enables is not a duplication
 * bug.
 *
 * THE FORMAT is a header (`ARSP` + a u32 version) followed by length-prefixed
 * serialized `EventBatch` records: a u32 big-endian byte count, then that many
 * bytes. Every append is fsync'd before it is reported as held, because a hold
 * that is only in the page cache is exactly the guarantee this file exists to
 * stop making.
 *
 * A TORN TAIL is expected, not corruption: a shim killed mid-append leaves a
 * partial record. It was never fsync'd, so it was never acknowledged as held
 * and nothing is owed for it — it is logged loudly and truncated away. A bad
 * HEADER is different: the file is not one of ours, so it is preserved beside
 * the journal for inspection rather than deleted, and reported.
 */
import fs from "node:fs";
import path from "node:path";
import { fromBinary, toBinary } from "@bufbuild/protobuf";
import { bindLog } from "./log.js";
import { Event, EventBatchSchema, EventSchema } from "./proto.js";

const COMPONENT = "shim-store-spill";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.store-spill.journal" });

/** File magic, so a foreign file is never parsed as a journal. */
const MAGIC = Buffer.from("ARSP", "ascii");
const FORMAT_VERSION = 1;
const HEADER_BYTES = MAGIC.length + 4;
const LENGTH_BYTES = 4;

/** One recovered batch and the offset just past the record it came from. */
interface ParsedRecord {
  events: Event[];
  end: number;
}

export interface SpillJournalOptions {
  /** Absolute path of the journal file. */
  path: string;
  /** The session this journal belongs to; logged with every record. */
  sessionId: string;
}

/**
 * An append-only journal of held batches, opened for the life of one shim.
 *
 * Every method reports its own failure by THROWING rather than by degrading
 * silently: the caller (store-client) owns whether a spill failure downgrades
 * the hold's durability or fails the write, and it cannot make that decision
 * about an error it never sees.
 */
export class SpillJournal {
  private fd: number;
  private readonly journalPath: string;
  private readonly sessionId: string;
  /** Bytes currently in the journal, tracked so `isEmpty` needs no stat. */
  private bytes: number;

  constructor(opts: SpillJournalOptions) {
    this.journalPath = opts.path;
    this.sessionId = opts.sessionId;
    fs.mkdirSync(path.dirname(this.journalPath), { recursive: true });
    this.fd = fs.openSync(this.journalPath, "a+");
    this.bytes = fs.fstatSync(this.fd).size;
    if (this.bytes === 0) {
      this.writeHeader();
    }
    LOGGER.log({
      agent_repl_session_id: this.sessionId,
      spill_path: this.journalPath,
      spill_bytes: this.bytes,
    }, `store write spill journal open with ${this.bytes} byte(s) carried over from a previous shim`);
  }

  /** The journal's path, for the caller's own records. */
  path(): string {
    return this.journalPath;
  }

  /** True once the descriptor has been released; the FILE still exists. */
  isClosed(): boolean {
    return this.fd < 0;
  }

  /** True when nothing is owed from disk. */
  isEmpty(): boolean {
    return this.bytes <= HEADER_BYTES;
  }

  /**
   * Append one batch and FSYNC it. Returns only once the record is on stable
   * storage, because the caller reports the batch as held on this return.
   */
  append(events: Event[]): void {
    const body = toBinary(EventBatchSchema, { $typeName: "agentshim.core.v1.EventBatch", events, cursorAdvance: undefined });
    const record = Buffer.alloc(LENGTH_BYTES + body.length);
    record.writeUInt32BE(body.length, 0);
    Buffer.from(body).copy(record, LENGTH_BYTES);
    this.writeAll(record);
    fs.fsyncSync(this.fd);
    this.bytes += record.length;
  }

  /**
   * Read every complete record, oldest first.
   *
   * A trailing partial record is truncated away and reported: it was never
   * fsync'd, so no caller was ever told it was held.
   */
  read(): Event[][] {
    const buf = fs.readFileSync(this.journalPath);
    if (buf.length === 0) {
      this.writeHeader();
      return [];
    }
    if (buf.length < HEADER_BYTES || !buf.subarray(0, MAGIC.length).equals(MAGIC)) {
      this.quarantineForeignFile(`header is not ${MAGIC.toString("ascii")}`);
      return [];
    }
    const version = buf.readUInt32BE(MAGIC.length);
    if (version !== FORMAT_VERSION) {
      this.quarantineForeignFile(`format version ${version} is not this shim's ${FORMAT_VERSION}`);
      return [];
    }
    const batches: Event[][] = [];
    let offset = HEADER_BYTES;
    while (offset < buf.length) {
      const parsed = this.parseRecord(buf, offset);
      if (parsed === null) break;
      batches.push(parsed.events);
      offset = parsed.end;
    }
    if (offset !== buf.length) {
      LOGGER.log({
        level: "error",
        agent_repl_session_id: this.sessionId,
        spill_path: this.journalPath,
        complete_batches: batches.length,
        torn_bytes: buf.length - offset,
      }, `store write spill journal has a TORN TAIL of ${buf.length - offset} byte(s) — a shim died mid-append; the partial record was never fsynced, so nothing was ever reported held for it, and it is truncated away`);
      this.truncateTo(offset);
    }
    LOGGER.log({
      agent_repl_session_id: this.sessionId,
      spill_path: this.journalPath,
      recovered_batches: batches.length,
    }, `recovered ${batches.length} held persistent batch(es) from the store write spill journal`);
    return batches;
  }

  /** Drop every record; the store has acknowledged all of them. */
  clear(): void {
    this.truncateTo(HEADER_BYTES);
    LOGGER.logVerbose({ agent_repl_session_id: this.sessionId, spill_path: this.journalPath },
      "store write spill journal cleared: every spilled batch is durably in the store");
  }

  /** Release the descriptor. The FILE is deliberately left in place. */
  close(): void {
    if (this.fd < 0) return;
    fs.closeSync(this.fd);
    this.fd = -1;
  }

  /**
   * Parse one record at `offset`, or null when the remaining bytes are a torn
   * tail rather than a whole record.
   */
  private parseRecord(buf: Buffer, offset: number): ParsedRecord | null {
    if (offset + LENGTH_BYTES > buf.length) return null;
    const length = buf.readUInt32BE(offset);
    const start = offset + LENGTH_BYTES;
    const end = start + length;
    if (end > buf.length) return null;
    // A record whose length prefix survived but whose body did not decode is
    // NOT a torn tail — the bytes are all there and they are wrong. Report it
    // and stop, so the records after it are not read through a bad offset.
    try {
      const batch = fromBinary(EventBatchSchema, buf.subarray(start, end));
      return { events: batch.events.map((e) => fromBinary(EventSchema, toBinary(EventSchema, e))), end };
    } catch (err) {
      LOGGER.log({
        level: "error",
        agent_repl_session_id: this.sessionId,
        spill_path: this.journalPath,
        record_offset: offset,
        record_bytes: length,
        cause: err instanceof Error ? err.message : String(err),
      }, `store write spill journal record at offset ${offset} did not decode; the journal is truncated there and everything after it is unreadable`);
      return null;
    }
  }

  private writeHeader(): void {
    const header = Buffer.alloc(HEADER_BYTES);
    MAGIC.copy(header, 0);
    header.writeUInt32BE(FORMAT_VERSION, MAGIC.length);
    this.writeAll(header);
    fs.fsyncSync(this.fd);
    this.bytes = HEADER_BYTES;
  }

  /** Write every byte, since a short write would silently tear a record. */
  private writeAll(buf: Buffer): void {
    let offset = 0;
    while (offset < buf.length) {
      const written = fs.writeSync(this.fd, buf, offset, buf.length - offset);
      if (written <= 0) throw new Error(`store spill journal made no progress after ${offset} of ${buf.length} bytes`);
      offset += written;
    }
  }

  private truncateTo(size: number): void {
    fs.ftruncateSync(this.fd, size);
    fs.fsyncSync(this.fd);
    this.bytes = size;
  }

  /**
   * Move a file that is not one of ours aside and start a fresh journal.
   *
   * Preserved rather than deleted: whatever wrote it, deleting the evidence of
   * a filename collision would make the next occurrence just as puzzling.
   */
  private quarantineForeignFile(why: string): void {
    const aside = `${this.journalPath}.foreign-${Date.now()}`;
    LOGGER.log({
      level: "error",
      agent_repl_session_id: this.sessionId,
      spill_path: this.journalPath,
      quarantine_path: aside,
      reason: why,
    }, `store write spill journal REFUSED and moved aside (${why}); a fresh journal starts in its place and nothing is read from the old file`);
    fs.closeSync(this.fd);
    fs.renameSync(this.journalPath, aside);
    this.fd = fs.openSync(this.journalPath, "a+");
    this.writeHeader();
  }
}
