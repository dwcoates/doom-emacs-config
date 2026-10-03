/**
 * Reading every conversation filed under a working directory.
 *
 * WHAT THIS GUARDS: that a directory that was never created is told apart from
 * an empty one, that ONE unreadable transcript does not deny the choice
 * between the rest, that a file being appended to right now is flagged active
 * so the daemon can refuse a bind on it, that the shim's own conversation is
 * marked rather than dropped, and that the list is ordered by activity. The
 * per-file facts themselves are exercised in engine/cold.test.ts; here the
 * concern is the directory and the flags laid over it.
 */
import { chmodSync, mkdirSync, mkdtempSync, utimesSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it, onTestFinished } from "vitest";
import { cwdSlug } from "../../src/engine/cold.js";
import {
  TRANSCRIPT_QUIET_AFTER_MS,
  readTranscripts,
  type TranscriptSummary,
} from "../../src/engine/transcripts.js";

/** A pretend vendor config dir, and the working directory it files under. */
const CWD = "/Users/someone/workspace/thing";

/** The instant every case reads the directory at. */
const NOW_MS = 1_700_000_000_000;

/** A fresh config dir with the project directory for {@link CWD} created. */
function configDirWithProject(): string {
  const configDir = mkdtempSync(path.join(os.tmpdir(), "shim-transcripts-"));
  mkdirSync(path.join(configDir, "projects", cwdSlug(CWD)), { recursive: true });
  return configDir;
}

/** One transcript line, terminated as the vendor terminates it. */
function line(record: Record<string, unknown>): string {
  return `${JSON.stringify(record)}\n`;
}

/** A user prompt record. */
function prompt(text: string): string {
  return line({ type: "user", message: { role: "user", content: text } });
}

/** An assistant record stating a request's usage and instant. */
function assistant(atMs: number, tokens: number): string {
  return line({
    type: "assistant",
    timestamp: new Date(atMs).toISOString(),
    message: {
      model: "claude-opus-5",
      usage: { input_tokens: tokens, cache_read_input_tokens: 0, cache_creation_input_tokens: 0 },
    },
  });
}

/**
 * Write one transcript and set its mtime.
 *
 * THE MTIME IS SET EXPLICITLY, never slept for: activeness is a fact about the
 * file's timestamp, and a suite that waited on a clock would be testing the
 * clock.
 */
function transcript(
  configDir: string,
  vendorSessionId: string,
  contents: string,
  mtimeMs: number,
): string {
  const file = path.join(configDir, "projects", cwdSlug(CWD), `${vendorSessionId}.jsonl`);
  writeFileSync(file, contents, "utf8");
  utimesSync(file, mtimeMs / 1000, mtimeMs / 1000);
  return file;
}

/** The summary for one id, or undefined when it was left out. */
function found(
  transcripts: readonly TranscriptSummary[],
  vendorSessionId: string,
): TranscriptSummary | undefined {
  return transcripts.find((t) => t.vendorSessionId === vendorSessionId);
}

/** A read that is expected to have succeeded, narrowed. */
function ok(configDir: string, bound?: string): readonly TranscriptSummary[] {
  const read = readTranscripts(configDir, CWD, bound, NOW_MS);
  if (read.kind !== "ok") throw new Error(`expected a successful read, got ${read.kind}`);
  return read.transcripts;
}

/** An instant comfortably outside the quiet window. */
const QUIET_MS = NOW_MS - TRANSCRIPT_QUIET_AFTER_MS - 60_000;

describe("readTranscripts", () => {
  it("reports no_project_dir when nothing has ever run in the directory", () => {
    // Arrange: a config dir with no projects tree at all.
    const configDir = mkdtempSync(path.join(os.tmpdir(), "shim-transcripts-"));

    // Act.
    const read = readTranscripts(configDir, CWD, undefined, NOW_MS);

    // Assert.
    expect(read.kind).toBe("no_project_dir");
  });

  it("names the path it searched when the directory does not exist", () => {
    // Arrange.
    const configDir = mkdtempSync(path.join(os.tmpdir(), "shim-transcripts-"));

    // Act.
    const read = readTranscripts(configDir, CWD, undefined, NOW_MS);

    // Assert.
    expect(read.kind === "no_project_dir" ? read.searchedPath : undefined).toBe(
      path.join(configDir, "projects", cwdSlug(CWD)),
    );
  });

  it("answers an EMPTY list for a directory that exists and holds nothing", () => {
    // Arrange.
    const configDir = configDirWithProject();

    // Act.
    const read = readTranscripts(configDir, CWD, undefined, NOW_MS);

    // Assert: empty is a success, distinct from no_project_dir.
    expect(read).toEqual({ kind: "ok", transcripts: [] });
  });

  it("reports unreadable when the directory exists and cannot be listed", () => {
    // Arrange: strip every permission from the project directory.
    const configDir = configDirWithProject();
    const project = path.join(configDir, "projects", cwdSlug(CWD));
    chmodSync(project, 0o000);
    // Given back afterwards: a directory nobody can list cannot be removed,
    // and the run's temp root (test/run-tmp-root.ts) fails its teardown on it.
    onTestFinished(() => chmodSync(project, 0o700));

    // Act.
    const read = readTranscripts(configDir, CWD, undefined, NOW_MS);

    // Assert.
    expect(read.kind).toBe("unreadable");
  });

  it("answers one entry per transcript file", () => {
    // Arrange.
    const configDir = configDirWithProject();
    transcript(configDir, "aaa", prompt("first"), QUIET_MS);
    transcript(configDir, "bbb", prompt("second"), QUIET_MS);

    // Act.
    const transcripts = ok(configDir);

    // Assert.
    expect(transcripts.map((t) => t.vendorSessionId).sort()).toEqual(["aaa", "bbb"]);
  });

  it("ignores a file that is not a transcript", () => {
    // Arrange: the vendor drops non-transcript files in the same directory.
    const configDir = configDirWithProject();
    transcript(configDir, "aaa", prompt("first"), QUIET_MS);
    writeFileSync(path.join(configDir, "projects", cwdSlug(CWD), "notes.md"), "hi", "utf8");

    // Act.
    const transcripts = ok(configDir);

    // Assert.
    expect(transcripts.map((t) => t.vendorSessionId)).toEqual(["aaa"]);
  });

  it("LEAVES OUT a transcript it cannot read rather than failing the whole list", () => {
    // Arrange: one readable conversation beside one with no read permission.
    const configDir = configDirWithProject();
    transcript(configDir, "readable", prompt("keep me"), QUIET_MS);
    const denied = transcript(configDir, "denied", prompt("lost"), QUIET_MS);
    chmodSync(denied, 0o000);

    // Act.
    const transcripts = ok(configDir);

    // Assert: one corrupt file must not deny the choice between the rest.
    expect(transcripts.map((t) => t.vendorSessionId)).toEqual(["readable"]);
  });

  it("FLAGS a transcript appended to inside the quiet window as active", () => {
    // Arrange: written a second ago.
    const configDir = configDirWithProject();
    transcript(configDir, "live", prompt("in progress"), NOW_MS - 1_000);

    // Act.
    const transcripts = ok(configDir);

    // Assert.
    expect(found(transcripts, "live")?.activeAtMs).toBe(NOW_MS - 1_000);
  });

  it("reports a WHOLE number of milliseconds for a sub-millisecond mtime", () => {
    // Arrange: a filesystem records finer than a millisecond, and the wire
    // field is an int64 — a float fails the whole verb at encode time.
    const configDir = configDirWithProject();
    transcript(configDir, "fractional", prompt("mid-thought"), NOW_MS - 1_000.5);

    // Act.
    const transcripts = ok(configDir);

    // Assert.
    expect(Number.isInteger(found(transcripts, "fractional")?.activeAtMs)).toBe(true);
  });

  it("leaves a transcript quiet for longer than the window unflagged", () => {
    // Arrange.
    const configDir = configDirWithProject();
    transcript(configDir, "idle", prompt("abandoned"), QUIET_MS);

    // Act.
    const transcripts = ok(configDir);

    // Assert.
    expect(found(transcripts, "idle")?.activeAtMs).toBeUndefined();
  });

  it("MARKS this shim's own conversation rather than omitting it", () => {
    // Arrange.
    const configDir = configDirWithProject();
    transcript(configDir, "mine", prompt("the one I am in"), QUIET_MS);
    transcript(configDir, "theirs", prompt("another"), QUIET_MS);

    // Act.
    const transcripts = ok(configDir, "mine");

    // Assert: the point of the verb is to find the OTHERS, with this one flagged.
    expect([found(transcripts, "mine")?.bound, found(transcripts, "theirs")?.bound]).toEqual([
      true,
      false,
    ]);
  });

  it("marks nothing bound when the shim has no session yet", () => {
    // Arrange.
    const configDir = configDirWithProject();
    transcript(configDir, "aaa", prompt("first"), QUIET_MS);

    // Act.
    const transcripts = ok(configDir, undefined);

    // Assert.
    expect(found(transcripts, "aaa")?.bound).toBe(false);
  });

  it("orders the list newest activity first", () => {
    // Arrange: three conversations whose last requests are hours apart.
    const configDir = configDirWithProject();
    transcript(configDir, "old", prompt("a") + assistant(QUIET_MS - 3_600_000, 10), QUIET_MS);
    transcript(configDir, "new", prompt("b") + assistant(QUIET_MS - 60_000, 10), QUIET_MS);
    transcript(configDir, "mid", prompt("c") + assistant(QUIET_MS - 600_000, 10), QUIET_MS);

    // Act.
    const transcripts = ok(configDir);

    // Assert.
    expect(transcripts.map((t) => t.vendorSessionId)).toEqual(["new", "mid", "old"]);
  });

  it("orders a conversation that never reached the model by its file's own mtime", () => {
    // Arrange: no assistant line at all in the newer one.
    const configDir = configDirWithProject();
    transcript(configDir, "answered", prompt("a") + assistant(QUIET_MS - 3_600_000, 10), QUIET_MS);
    transcript(configDir, "unanswered", prompt("b"), QUIET_MS);

    // Act.
    const transcripts = ok(configDir);

    // Assert: an unanswered conversation must not sort as if it were from 1970.
    expect(transcripts.map((t) => t.vendorSessionId)).toEqual(["unanswered", "answered"]);
  });

  it("carries the transcript's own facts through unchanged", () => {
    // Arrange.
    const configDir = configDirWithProject();
    transcript(
      configDir,
      "aaa",
      prompt("explain hash tables") + prompt("now the collisions") + assistant(QUIET_MS, 4_242),
      QUIET_MS,
    );

    // Act.
    const facts = found(ok(configDir), "aaa")?.facts;

    // Assert.
    expect([facts?.opening, facts?.prompts, facts?.contextTokens, facts?.sawUsage]).toEqual([
      "explain hash tables",
      2,
      4_242,
      true,
    ]);
  });
});
