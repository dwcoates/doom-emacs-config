import { create } from "@bufbuild/protobuf";
import { existsSync, mkdirSync, writeFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it, vi } from "vitest";
import { transcriptPath } from "../../src/engine/cold.js";
import {
  forkableRecordUuids,
  forkSession,
  resolveUpTo,
  type ForkVendorSession,
} from "../../src/engine/fork-session.js";
import { createAgentIdentityStore } from "../../src/engine/identity.js";
import { forkFakeSession } from "../../src/fake/fork.js";
import { workspaceLockKey } from "../../src/locks.js";
import { shimv1 } from "../../src/proto.js";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import { testTempDir } from "../temp-dir.js";

const PARENT_ID = "11111111-1111-4111-8111-111111111111";
const PARENT_DIR = "/repo/parent";
const CHILD_DIR = "/repo/child";

function whole(): shimv1.ForkPoint {
  return create(shimv1.ForkPointSchema, { extent: { case: "whole", value: create(shimv1.ForkPointWholeSchema, {}) } });
}

function throughRecord(uuid: string): shimv1.ForkPoint {
  return create(shimv1.ForkPointSchema, {
    extent: {
      case: "throughRecord",
      value: create(shimv1.ForkPointThroughRecordSchema, { record: create(shimv1.ForkRecordTokenSchema, { value: uuid }) }),
    },
  });
}

function beforeRecord(uuid: string): shimv1.ForkPoint {
  return create(shimv1.ForkPointSchema, {
    extent: {
      case: "beforeRecord",
      value: create(shimv1.ForkPointBeforeRecordSchema, { record: create(shimv1.ForkRecordTokenSchema, { value: uuid }) }),
    },
  });
}

/** A parent transcript of three chained records and one unchained metadata line. */
function writeParent(configDir: string): string {
  const file = transcriptPath(configDir, PARENT_DIR, PARENT_ID);
  mkdirSync(path.dirname(file), { recursive: true });
  const lines = [
    { type: "user", uuid: "u1", parentUuid: null, message: { role: "user", content: "hello" } },
    { type: "ai-title", aiTitle: "a title" },
    { type: "assistant", uuid: "a1", parentUuid: "u1", message: { role: "assistant", content: [{ type: "text", text: "hi" }] } },
    { type: "user", uuid: "u2", parentUuid: "a1", message: { role: "user", content: "again" } },
  ];
  writeFileSync(file, `${lines.map((line) => JSON.stringify(line)).join("\n")}\n`);
  return file;
}

/** The directories a fork needs, with the parent's book persisted unless asked otherwise. */
async function fixture(options: { book?: boolean } = {}): Promise<{ configDir: string; stateDir: string }> {
  const configDir = testTempDir("fork-config-");
  const stateDir = testTempDir("fork-state-");
  if (options.book !== false) await createAgentIdentityStore(stateDir, workspaceLockKey(PARENT_DIR)).write("book-original");
  return { configDir, stateDir };
}

describe("forkableRecordUuids", () => {
  it("answers the vendor's message records in file order, skipping metadata and torn lines", () => {
    // Arrange
    const dir = testTempDir("fork-uuids-");
    const file = path.join(dir, "t.jsonl");
    writeFileSync(
      file,
      [
        JSON.stringify({ type: "user", uuid: "u1" }),
        JSON.stringify({ type: "last-prompt", uuid: "meta" }),
        "{not json",
        JSON.stringify({ type: "progress", uuid: "p1" }),
        JSON.stringify({ type: "attachment", uuid: "" }),
        JSON.stringify({ type: "system", uuid: "s1" }),
        "",
      ].join("\n"),
    );

    // Act
    const uuids = forkableRecordUuids(file);

    // Assert
    expect(uuids).toEqual(["u1", "p1", "s1"]);
  });
});

describe("resolveUpTo", () => {
  const uuids = ["u1", "a1", "u2"];
  it.each([
    { name: "whole forks everything", point: whole(), expected: { kind: "ok", upTo: undefined } },
    { name: "through a present record ends on it", point: throughRecord("a1"), expected: { kind: "ok", upTo: "a1" } },
    { name: "before a present opener ends on the record before it", point: beforeRecord("u2"), expected: { kind: "ok", upTo: "a1" } },
  ])("$name", ({ point, expected }) => {
    // Act
    const resolved = resolveUpTo(uuids, point);

    // Assert
    expect(resolved).toEqual(expected);
  });

  it.each([
    { name: "through an absent record", point: throughRecord("missing"), detail: "missing" },
    { name: "before an absent opener", point: beforeRecord("missing"), detail: "missing" },
    { name: "before the very first record", point: beforeRecord("u1"), detail: "no finished turn precedes it" },
  ])("refuses $name", ({ point, detail }) => {
    // Act
    const resolved = resolveUpTo(uuids, point);

    // Assert
    expect(resolved.kind).toBe("absent");
    expect(resolved.kind === "absent" ? resolved.detail : "").toContain(detail);
  });

  it("throws on a point with no extent, which the validator never lets through", () => {
    // Act / Assert
    expect(() => resolveUpTo(uuids, create(shimv1.ForkPointSchema, {}))).toThrow(/no extent/);
  });
});

describe("forkSession", () => {
  it("forks through the vendor and files the fork under this shim's directory", async () => {
    // Arrange
    const { configDir, stateDir } = await fixture();
    writeParent(configDir);
    const fork: ForkVendorSession = (spec) => Promise.resolve(forkFakeSession(configDir, spec, sequentialIds()));
    const mark = logSinkMark();

    // Act
    const outcome = await forkSession(
      { configDir, stateDir, cwd: CHILD_DIR, forkVendorSession: fork },
      { parentVendorSessionId: PARENT_ID, parentWorkspaceDir: PARENT_DIR, point: throughRecord("a1") },
    );

    // Assert
    expect(outcome).toEqual({ kind: "forked", vendorSessionId: "id-1", parentBook: "book-original" });
    expect(existsSync(transcriptPath(configDir, CHILD_DIR, "id-1"))).toBe(true);
    expect(existsSync(transcriptPath(configDir, PARENT_DIR, "id-1"))).toBe(false);
    expect(forkRecordsSince(mark)).toMatchObject([
      { level: "info", context: { vendor_session_id: "id-1", up_to: "a1", parent_book: "book-original" } },
    ]);
  });

  it("asks the vendor for the whole transcript when the point is whole", async () => {
    // Arrange
    const { configDir, stateDir } = await fixture();
    writeParent(configDir);
    const fork = vi.fn<ForkVendorSession>(() => {
      writeFileSync(transcriptPath(configDir, PARENT_DIR, "forked"), "{}\n");
      return Promise.resolve("forked");
    });

    // Act
    await forkSession(
      { configDir, stateDir, cwd: CHILD_DIR, forkVendorSession: fork },
      { parentVendorSessionId: PARENT_ID, parentWorkspaceDir: PARENT_DIR, point: whole() },
    );

    // Assert
    expect(fork).toHaveBeenCalledWith({ parentSessionId: PARENT_ID, parentDir: PARENT_DIR });
  });

  it.each([
    {
      name: "no parent transcript under this account root",
      book: true,
      parent: false,
      point: whole(),
      kind: "unknown_session",
      message: "the fork was refused: no parent transcript under this account root",
    },
    {
      name: "the point's record is not in the parent transcript",
      book: true,
      parent: true,
      point: throughRecord("missing"),
      kind: "record_absent",
      message: "the fork was refused: the fork point is not in the parent transcript",
    },
    {
      name: "the parent workspace has no persisted book",
      book: false,
      parent: true,
      point: whole(),
      kind: "parent_book_unknown",
      message: "the fork was refused: the parent workspace has no persisted book identity",
    },
  ])("refuses when $name, never asking the vendor", async ({ book, parent, point, kind, message }) => {
    // Arrange
    const { configDir, stateDir } = await fixture({ book });
    if (parent) writeParent(configDir);
    const fork = vi.fn<ForkVendorSession>();
    const mark = logSinkMark();

    // Act
    const outcome = await forkSession(
      { configDir, stateDir, cwd: CHILD_DIR, forkVendorSession: fork },
      { parentVendorSessionId: PARENT_ID, parentWorkspaceDir: PARENT_DIR, point },
    );

    // Assert
    expect(outcome.kind).toBe(kind);
    expect(fork).not.toHaveBeenCalled();
    expect(forkRecordsSince(mark)).toMatchObject([
      { level: "error", message, context: { parent_vendor_session_id: PARENT_ID, parent_workspace_dir: PARENT_DIR } },
    ]);
  });

  it("answers vendor_failed with the vendor's words when its fork throws", async () => {
    // Arrange
    const { configDir, stateDir } = await fixture();
    writeParent(configDir);
    const fork: ForkVendorSession = () => Promise.reject(new Error("vendor said no"));
    const mark = logSinkMark();

    // Act
    const outcome = await forkSession(
      { configDir, stateDir, cwd: CHILD_DIR, forkVendorSession: fork },
      { parentVendorSessionId: PARENT_ID, parentWorkspaceDir: PARENT_DIR, point: whole() },
    );

    // Assert
    expect(outcome).toEqual({ kind: "vendor_failed", detail: "vendor said no" });
    expect(forkRecordsSince(mark)).toMatchObject([
      { level: "error", message: "the fork was refused: the vendor's fork failed", context: { cause: "vendor said no" } },
    ]);
  });

  it("fails hard when the vendor answers an id it wrote no transcript for", async () => {
    // Arrange
    const { configDir, stateDir } = await fixture();
    writeParent(configDir);
    const fork: ForkVendorSession = () => Promise.resolve("never-written");

    // Act / Assert
    await expect(
      forkSession(
        { configDir, stateDir, cwd: CHILD_DIR, forkVendorSession: fork },
        { parentVendorSessionId: PARENT_ID, parentWorkspaceDir: PARENT_DIR, point: whole() },
      ),
    ).rejects.toThrow(/wrote no/);
  });
});

/** Mint ids id-1, id-2, … so a fork's identities are predictable. */
function sequentialIds(): () => string {
  let next = 0;
  return () => `id-${String(++next)}`;
}

/** The fork's own records since `mark`: the identity store's reads log too. */
function forkRecordsSince(mark: number): ReturnType<typeof logRecordsSince> {
  return logRecordsSince(mark).filter((record) => record.operation === "shim.engine.fork_session");
}
