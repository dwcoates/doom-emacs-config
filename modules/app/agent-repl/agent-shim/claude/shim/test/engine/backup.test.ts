/**
 * The one-copy transcript backup.
 *
 * WHAT THIS GUARDS: that a copy of the one unregenerable artifact exists, that
 * it is ONE appended copy per conversation, that damage to the transcript
 * never reaches the copy, and that making it can never cost a turn.
 */
import { appendFileSync, existsSync, mkdirSync, mkdtempSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it, vi } from "vitest";
import {
  backupDir,
  backupName,
  backupTranscript,
  isLegacyCopy,
  mirrorTranscript,
  removeLegacyCopies,
} from "../../src/engine/backup.js";

function scratch(): string {
  return mkdtempSync(path.join(os.tmpdir(), "shim-backup-"));
}

function transcript(contents = "one line\n"): string {
  const dir = scratch();
  const file = path.join(dir, "session.jsonl");
  writeFileSync(file, contents, "utf8");
  return file;
}

const WORKSPACE = "abc12345";

interface Record {
  readonly level: string;
  readonly [field: string]: unknown;
}

/** The records this module wrote while `act` ran, read off the stderr mirror, each its level beside its context fields. */
function recordsOf(act: () => void): Record[] {
  const written: string[] = [];
  const terminal = vi.spyOn(process.stderr, "write").mockImplementation((chunk) => {
    written.push(String(chunk));
    return true;
  });
  try {
    act();
  } finally {
    terminal.mockRestore();
  }
  return written.flatMap((line) => {
    try {
      const parsed = JSON.parse(line) as { level: string; context?: { [field: string]: unknown } };
      return [{ ...parsed.context, level: parsed.level }];
    } catch {
      return [];
    }
  });
}

function backupOf(state: string, id = "v-1"): string {
  return path.join(backupDir(state, WORKSPACE), backupName(id));
}

function back(file: string, state: string, id = "v-1"): void {
  backupTranscript({ transcript: file, stateDir: state, workspaceKey: WORKSPACE, vendorSessionId: id });
}

describe("the backup name", () => {
  it("is the conversation alone, so a conversation has exactly one file", () => {
    expect(backupName("v-1")).toBe("v-1.jsonl");
  });
});

describe("mirroring a transcript", () => {
  it("copies the whole transcript when there is no backup yet", () => {
    const file = transcript("a\nb\n");
    const target = path.join(scratch(), "copy.jsonl");

    const outcome = mirrorTranscript(file, target);

    expect([outcome, readFileSync(target, "utf8")]).toEqual([{ kind: "mirrored", appended: 4, bytes: 4 }, "a\nb\n"]);
  });

  it("appends only what the transcript grew by", () => {
    const file = transcript("a\n");
    const target = path.join(scratch(), "copy.jsonl");
    mirrorTranscript(file, target);
    appendFileSync(file, "b\n");

    const outcome = mirrorTranscript(file, target);

    expect([outcome, readFileSync(target, "utf8")]).toEqual([{ kind: "mirrored", appended: 2, bytes: 4 }, "a\nb\n"]);
  });

  it("appends nothing when the transcript has not changed", () => {
    const file = transcript("a\n");
    const target = path.join(scratch(), "copy.jsonl");
    mirrorTranscript(file, target);

    expect(mirrorTranscript(file, target)).toEqual({ kind: "mirrored", appended: 0, bytes: 2 });
  });

  it("leaves the backup untouched when the transcript SHRANK", () => {
    const file = transcript("a\nb\n");
    const target = path.join(scratch(), "copy.jsonl");
    mirrorTranscript(file, target);
    writeFileSync(file, "a\n");

    const outcome = mirrorTranscript(file, target);

    expect([outcome, readFileSync(target, "utf8")]).toEqual([
      { kind: "diverged", at: 2, transcriptBytes: 2, backupBytes: 4 },
      "a\nb\n",
    ]);
  });

  it("leaves the backup untouched when bytes it holds CHANGED, naming the first changed offset", () => {
    const file = transcript("abc\n");
    const target = path.join(scratch(), "copy.jsonl");
    mirrorTranscript(file, target);
    writeFileSync(file, "abX\nmore\n");

    const outcome = mirrorTranscript(file, target);

    expect([outcome, readFileSync(target, "utf8")]).toEqual([
      { kind: "diverged", at: 2, transcriptBytes: 9, backupBytes: 4 },
      "abc\n",
    ]);
  });

  it("compares and appends across chunk boundaries byte for byte", () => {
    // Larger than one 1MiB step, so both loops run more than once.
    const head = "x".repeat((1 << 20) + 7);
    const file = transcript(head);
    const target = path.join(scratch(), "copy.jsonl");
    mirrorTranscript(file, target);
    appendFileSync(file, "y".repeat((1 << 20) + 3));

    mirrorTranscript(file, target);

    expect(readFileSync(target).equals(readFileSync(file))).toBe(true);
  });

  it("throws, naming the transcript, when the transcript does not exist", () => {
    const missing = path.join(scratch(), "missing.jsonl");

    let thrown: NodeJS.ErrnoException | undefined;
    try {
      mirrorTranscript(missing, path.join(scratch(), "copy.jsonl"));
    } catch (err) {
      thrown = err as NodeJS.ErrnoException;
    }

    expect([thrown?.code, thrown?.path]).toEqual(["ENOENT", missing]);
  });
});

describe("taking a backup", () => {
  it("writes the transcript's bytes into the workspace's backup directory", () => {
    const state = scratch();
    const file = transcript("hello\n");

    back(file, state);

    expect(readFileSync(backupOf(state), "utf8")).toBe("hello\n");
  });

  it("keeps ONE file per conversation however many turns back it up", () => {
    const state = scratch();
    const file = transcript("t0\n");

    for (let turn = 1; turn <= 5; turn++) {
      appendFileSync(file, `t${String(turn)}\n`);
      back(file, state);
    }

    expect([readdirSync(backupDir(state, WORKSPACE)), readFileSync(backupOf(state), "utf8")]).toEqual([
      ["v-1.jsonl"],
      "t0\nt1\nt2\nt3\nt4\nt5\n",
    ]);
  });

  it("records a backup at info with what it appended", () => {
    const state = scratch();
    const file = transcript("hello\n");

    const records = recordsOf(() => back(file, state));

    expect(records).toEqual([
      expect.objectContaining({ level: "info", appended_bytes: 6, backup_bytes: 6, vendor_session_id: "v-1" }),
    ]);
  });

  it("WARNS, and keeps the last good copy, when the transcript was damaged", () => {
    // Arrange: a backed-up transcript that then loses its last line.
    const state = scratch();
    const file = transcript("a\nb\n");
    back(file, state);
    writeFileSync(file, "a\n");

    // Act.
    const records = recordsOf(() => back(file, state));

    // Assert: the refusal is a warning carrying where and how the two differ,
    // and the copy still holds the bytes the transcript lost.
    expect([records, readFileSync(backupOf(state), "utf8")]).toEqual([
      [
        expect.objectContaining({
          level: "warn",
          backup: backupOf(state),
          diverged_at: 2,
          transcript_bytes: 2,
          backup_bytes: 4,
        }),
      ],
      "a\nb\n",
    ]);
  });

  it("calls a transcript that was never written nothing to copy, not a failed backup", () => {
    // Arrange: the id a session rotates away from at its very start, which the
    // vendor never wrote a file for.
    const state = scratch();

    // Act.
    const records = recordsOf(() => back(path.join(scratch(), "missing.jsonl"), state));

    // Assert.
    expect([records.map((r) => r.level), existsSync(backupOf(state))]).toEqual([["info"], false]);
  });

  it("still WARNS about a copy that could have been made and was not", () => {
    // Arrange: the source exists; the target's own directory is a file, so the
    // copy fails for a reason nothing here can explain away.
    const state = scratch();
    const file = transcript("hello\n");
    mkdirSync(path.dirname(backupDir(state, WORKSPACE)), { recursive: true });
    writeFileSync(backupDir(state, WORKSPACE), "not a directory", "utf8");

    // Act.
    const records = recordsOf(() => back(file, state));

    // Assert.
    expect([records.map((r) => [r.level, r.transcript]), typeof records[0]?.cause]).toEqual([[["warn", file]], "string"]);
  });

  it("does not throw when the transcript is missing", () => {
    // A copy is worth less than the conversation it copies.
    const state = scratch();

    expect(() => back(path.join(scratch(), "missing.jsonl"), state)).not.toThrow();
  });

  it("removes the conversation's old timestamped whole copies once its one copy exists", () => {
    // Arrange: a workspace that took whole copies before this backup existed,
    // for this conversation and for another.
    const state = scratch();
    const dir = backupDir(state, WORKSPACE);
    mkdirSync(dir, { recursive: true });
    for (const name of ["v-1-1790905009558.jsonl", "v-1-1790905579254.jsonl", "v-2-1790905009558.jsonl"]) {
      writeFileSync(path.join(dir, name), "old\n", "utf8");
    }

    // Act.
    back(transcript("new\n"), state);

    // Assert: only this conversation's old copies went.
    expect(readdirSync(dir).sort()).toEqual(["v-1.jsonl", "v-2-1790905009558.jsonl"]);
  });

  it("keeps the old whole copies when the transcript was damaged", () => {
    // The old copies may be the only bytes the damage did not reach.
    const state = scratch();
    const file = transcript("a\nb\n");
    back(file, state);
    const old = path.join(backupDir(state, WORKSPACE), "v-1-1790905009558.jsonl");
    writeFileSync(old, "a\nb\n", "utf8");
    writeFileSync(file, "a\n");

    recordsOf(() => back(file, state));

    expect(existsSync(old)).toBe(true);
  });
});

describe("which files are old whole copies", () => {
  it.each([
    ["v-1-1790905009558.jsonl", true],
    ["v-1.jsonl", false],
    ["v-1-abc.jsonl", false],
    ["v-2-1790905009558.jsonl", false],
    ["v-1-1790905009558.jsonl.tmp", false],
  ])("%s is an old copy of v-1: %s", (name, expected) => {
    expect(isLegacyCopy(name, "v-1")).toBe(expected);
  });
});

describe("removing old whole copies", () => {
  it("does not throw, and warns, when the directory cannot be listed", () => {
    const records = recordsOf(() => removeLegacyCopies(path.join(scratch(), "absent"), "v-1"));

    expect(records).toEqual([expect.objectContaining({ level: "warn", vendor_session_id: "v-1" })]);
  });

  it("keeps going past a copy it cannot remove, and warns about it", () => {
    // Arrange: one old copy is a non-empty directory, so the un-recursive
    // rmSync refuses it, the same shape a copy owned by another user has.
    const dir = scratch();
    mkdirSync(path.join(dir, "v-1-1.jsonl", "held"), { recursive: true });
    writeFileSync(path.join(dir, "v-1-2.jsonl"), "", "utf8");

    // Act.
    const records = recordsOf(() => removeLegacyCopies(dir, "v-1"));

    // Assert.
    expect([readdirSync(dir), records.filter((r) => r.level === "warn")]).toEqual([
      ["v-1-1.jsonl"],
      [expect.objectContaining({ removed: "v-1-1.jsonl" })],
    ]);
  });
});
