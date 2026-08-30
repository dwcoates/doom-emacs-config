/**
 * The bounded transcript backup.
 *
 * WHAT THIS GUARDS: that a copy of the one unregenerable artifact exists, and
 * that making it can never cost a turn. The failure modes being excluded are an
 * unbounded backup directory that eventually costs more than it protects, and a
 * failed copy that takes the conversation down with it.
 */
import { mkdirSync, mkdtempSync, readdirSync, readFileSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { backupDir, backupName, backupTranscript, pruneBackups } from "../../src/engine/backup.js";

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

describe("the backup name", () => {
  it("carries the conversation and the instant", () => {
    expect(backupName("v-1", 42)).toBe("v-1-42.jsonl");
  });
});

describe("taking a copy", () => {
  it("writes the transcript's bytes into the workspace's backup directory", () => {
    const state = scratch();
    const file = transcript("hello\n");

    backupTranscript({ transcript: file, stateDir: state, workspaceKey: WORKSPACE, vendorSessionId: "v-1", atMs: 1 });

    expect(readFileSync(path.join(backupDir(state, WORKSPACE), "v-1-1.jsonl"), "utf8")).toBe("hello\n");
  });

  it("does not throw when the transcript is missing", () => {
    // A copy is worth less than the conversation it copies.
    const state = scratch();

    expect(() =>
      backupTranscript({
        transcript: path.join(scratch(), "missing.jsonl"),
        stateDir: state,
        workspaceKey: WORKSPACE,
        vendorSessionId: "v-1",
        atMs: 1,
      }),
    ).not.toThrow();
  });

  it("prunes down to the keep count", () => {
    const state = scratch();
    const file = transcript();

    for (let at = 1; at <= 5; at++) {
      backupTranscript({
        transcript: file,
        stateDir: state,
        workspaceKey: WORKSPACE,
        vendorSessionId: "v-1",
        atMs: at,
        keep: 2,
      });
    }

    expect(readdirSync(backupDir(state, WORKSPACE)).sort()).toEqual(["v-1-4.jsonl", "v-1-5.jsonl"]);
  });
});

describe("pruning", () => {
  it("keeps the newest copies, which are the lexically last names", () => {
    const dir = scratch();
    mkdirSync(dir, { recursive: true });
    for (const name of ["v-1.jsonl", "v-2.jsonl", "v-3.jsonl"]) {
      writeFileSync(path.join(dir, name), "", "utf8");
    }

    pruneBackups(dir, 1);

    expect(readdirSync(dir)).toEqual(["v-3.jsonl"]);
  });

  it("does nothing when the directory is already within the bound", () => {
    const dir = scratch();
    writeFileSync(path.join(dir, "v-1.jsonl"), "", "utf8");

    pruneBackups(dir, 8);

    expect(readdirSync(dir)).toEqual(["v-1.jsonl"]);
  });

  it("does not throw when the directory does not exist", () => {
    expect(() => pruneBackups(path.join(scratch(), "absent"), 1)).not.toThrow();
  });
});
