/**
 * Reading a transcript's records for its title digest.
 *
 * WHAT THIS GUARDS: that a missing transcript and an unreadable one are told
 * apart (the daemon acts on them differently), that a torn final line does not
 * cost the digest the prompts before it, and that the records that do parse
 * become the digest. The boundary logic itself is exercised in
 * convert/title-digest.test.ts; here the concern is the I/O half.
 */
import { chmodSync, mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { readTitleDigest } from "../../src/engine/title-digest.js";

/** A scratch transcript path, written or not. */
function scratch(): string {
  return path.join(mkdtempSync(path.join(os.tmpdir(), "shim-title-digest-")), "session.jsonl");
}

/** One transcript line, terminated as the vendor terminates it. */
function line(record: Record<string, unknown>): string {
  return `${JSON.stringify(record)}\n`;
}

describe("readTitleDigest", () => {
  it("reports no_transcript when the file does not exist", () => {
    // Act.
    const read = readTitleDigest(scratch());

    // Assert.
    expect(read.kind).toBe("no_transcript");
  });

  it("gathers the digest from the records the transcript holds", () => {
    // Arrange.
    const file = scratch();
    writeFileSync(file, line({ type: "user", message: { role: "user", content: "explain hash tables" }}), "utf8");

    // Act.
    const read = readTitleDigest(file);

    // Assert.
    expect(read).toEqual({ kind: "ok", digest: { boundary: "none", prompts: ["explain hash tables"] } });
  });

  it("skips a torn final line rather than losing the prompts before it", () => {
    // Arrange — the last line is a half-written record, as a live writer leaves it.
    const file = scratch();
    writeFileSync(file, line({ type: "user", message: { role: "user", content: "a real prompt" }}) + '{"type":"user","mess', "utf8");

    // Act.
    const read = readTitleDigest(file);

    // Assert.
    expect(read).toEqual({ kind: "ok", digest: { boundary: "none", prompts: ["a real prompt"] } });
  });

  it("reports unreadable when the file exists but cannot be read", () => {
    // Arrange — a file with no read permission is present but unreadable.
    const file = scratch();
    writeFileSync(file, line({ type: "user", message: { role: "user", content: "x" }}), "utf8");
    chmodSync(file, 0o000);

    // Act.
    const read = readTitleDigest(file);

    // Assert — restore access so the tmp dir can be cleaned regardless.
    chmodSync(file, 0o600);
    expect(read.kind).toBe("unreadable");
  });
});
