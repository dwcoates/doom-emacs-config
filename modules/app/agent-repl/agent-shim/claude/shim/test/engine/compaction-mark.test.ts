/**
 * The compaction mark — the durable "this transcript is already compacted".
 *
 * WHAT THIS GUARDS: that a hibernation asked for twice buys ONE vendor summary
 * turn. The mark is the half of that guarantee which survives a process: it is
 * a file, so a daemon bounce or a restarted shim reads back what its
 * predecessor recorded rather than paying for the summary again. Every read
 * failure must answer "no mark" — a mark that cannot be read has to cost one
 * extra compaction, never a hibernation that silently never compacts.
 */
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import {
  compactionMarkPath,
  readCompactionMark,
  transcriptBytes,
  writeCompactionMark,
} from "../../src/engine/compaction-mark.js";

const KEY = "ws-key";
const SESSION = "11111111-1111-4111-8111-111111111111";

function scratch(): string {
  return mkdtempSync(path.join(os.tmpdir(), "shim-mark-"));
}

describe("the compaction mark", () => {
  it("reads back what a compaction recorded", () => {
    const stateDir = scratch();

    writeCompactionMark(stateDir, KEY, SESSION, { transcript_bytes: 4096, compacted_at_ms: 17 });

    expect(readCompactionMark(stateDir, KEY, SESSION)).toEqual({
      transcript_bytes: 4096,
      compacted_at_ms: 17,
    });
  });

  it("is read back by a FRESH reader over the same state dir", () => {
    // THE RESTART. Nothing in this module holds state between calls, which is
    // what makes a mark written before a daemon bounce readable after one: the
    // second read below is the restarted shim's first read.
    const stateDir = scratch();
    writeCompactionMark(stateDir, KEY, SESSION, { transcript_bytes: 512, compacted_at_ms: 9 });
    readCompactionMark(stateDir, KEY, SESSION);

    const afterARestart = readCompactionMark(stateDir, KEY, SESSION);

    expect(afterARestart?.transcript_bytes).toBe(512);
  });

  it("answers absence for a session that has never been compacted", () => {
    const stateDir = scratch();

    expect(readCompactionMark(stateDir, KEY, SESSION)).toBeUndefined();
  });

  it("keeps one session's mark clear of another's", () => {
    // A workspace's vendor session id ROTATES, and a mark for a conversation
    // that is no longer the live one must never be read as this one's.
    const stateDir = scratch();
    writeCompactionMark(stateDir, KEY, SESSION, { transcript_bytes: 512, compacted_at_ms: 9 });

    expect(readCompactionMark(stateDir, KEY, "22222222-2222-4222-8222-222222222222")).toBeUndefined();
  });

  it("answers absence for a mark that is not readable json", () => {
    const stateDir = scratch();
    const file = compactionMarkPath(stateDir, KEY, SESSION);
    mkdirSync(path.dirname(file), { recursive: true });
    writeFileSync(file, "{ this is not json", "utf8");

    expect(readCompactionMark(stateDir, KEY, SESSION)).toBeUndefined();
  });

  it("answers absence for a mark that states no transcript length", () => {
    const stateDir = scratch();
    const file = compactionMarkPath(stateDir, KEY, SESSION);
    mkdirSync(path.dirname(file), { recursive: true });
    writeFileSync(file, JSON.stringify({ compacted_at_ms: 9 }), "utf8");

    expect(readCompactionMark(stateDir, KEY, SESSION)).toBeUndefined();
  });

  it("answers absence for a mark that states no instant", () => {
    const stateDir = scratch();
    const file = compactionMarkPath(stateDir, KEY, SESSION);
    mkdirSync(path.dirname(file), { recursive: true });
    writeFileSync(file, JSON.stringify({ transcript_bytes: 512 }), "utf8");

    expect(readCompactionMark(stateDir, KEY, SESSION)).toBeUndefined();
  });

  it("keeps the mark under this workspace's own shim state", () => {
    const stateDir = "/state";

    expect(compactionMarkPath(stateDir, KEY, SESSION)).toBe(
      path.join("/state", "shim", KEY, "compaction", `${SESSION}.json`),
    );
  });
});

describe("measuring the transcript", () => {
  it("measures a transcript's byte length", () => {
    const file = path.join(scratch(), "t.jsonl");
    writeFileSync(file, "0123456789", "utf8");

    expect(transcriptBytes(file)).toBe(10);
  });

  it("answers absence for a transcript that is not there", () => {
    expect(transcriptBytes(path.join(scratch(), "absent.jsonl"))).toBeUndefined();
  });
});
