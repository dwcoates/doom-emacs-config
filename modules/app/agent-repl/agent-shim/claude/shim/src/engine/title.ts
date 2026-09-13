/**
 * engine/title.ts — reading the vendor's own conversation summary off the
 * transcript, incrementally.
 *
 * WHY A TAIL AND NOT A READ. The vendor states its `ai-title` only on the file
 * plane, and it restates it as the conversation moves on, so the shim has to
 * look more than once — at session start, and at the end of every turn. A
 * transcript is megabytes long by the end of a working session, and re-reading
 * the whole of it per turn to find one line would make the cost of the title
 * grow with the conversation. So this keeps a BYTE CURSOR and reads only what
 * has been appended since the last look.
 *
 * THE CURSOR STOPS AT THE LAST NEWLINE. A transcript is appended to by a live
 * process, so the final bytes of any read can be half a record; advancing past
 * them would swallow the rest of that line on the next read. Everything after
 * the last newline is left for next time.
 *
 * A SHORTER FILE IS A DIFFERENT FILE. A compaction rewrites the transcript and
 * a clear rotates to a new one; either way a size below the cursor means the
 * bytes the cursor counted are gone, so the tail restarts from the beginning
 * rather than reading from an offset into a file that never had it.
 */
import { closeSync, openSync, readSync, statSync } from "node:fs";
import { bindLog } from "../log.js";
import { lastAiTitle } from "../convert/session-title.js";

const LOGGER = bindLog({ component: "shim-engine-title", operation: "shim.engine.title" });

/** One transcript's incremental scan for the vendor's title. */
export class TranscriptTitleTail {
  private cursor = 0;
  private stated: string | undefined;

  constructor(readonly file: string) {}

  /** The title the transcript now states, when it CHANGED since the last read. */
  read(): string | undefined {
    const chunk = this.appended();
    if (chunk === "") return undefined;
    const title = lastAiTitle(chunk);
    if (title === undefined || title === this.stated) return undefined;
    this.stated = title;
    LOGGER.info({ file: this.file, title }, "the vendor stated a title for this conversation");
    return title;
  }

  /** The complete lines appended since the last read, or "" when there are none. */
  private appended(): string {
    let size: number;
    try {
      size = statSync(this.file).size;
    } catch (err) {
      if ((err as NodeJS.ErrnoException).code === "ENOENT") {
        // ABSENCE IS AN ANSWER, not a fault: a fresh session's transcript does
        // not exist until the vendor writes its first record.
        LOGGER.debug({ file: this.file }, "no transcript to read a title from yet");
        return "";
      }
      throw err;
    }
    if (size < this.cursor) {
      LOGGER.info(
        { file: this.file, size, cursor: this.cursor },
        "the transcript is shorter than it was; rereading it from the beginning",
      );
      this.cursor = 0;
    }
    if (size === this.cursor) return "";

    const length = size - this.cursor;
    const buffer = Buffer.allocUnsafe(length);
    const fd = openSync(this.file, "r");
    let read: number;
    try {
      read = readSync(fd, buffer, 0, length, this.cursor);
    } finally {
      closeSync(fd);
    }
    const raw = buffer.subarray(0, read).toString("utf8");
    const lastNewline = raw.lastIndexOf("\n");
    if (lastNewline < 0) return "";
    this.cursor += Buffer.byteLength(raw.slice(0, lastNewline + 1), "utf8");
    return raw.slice(0, lastNewline);
  }
}
