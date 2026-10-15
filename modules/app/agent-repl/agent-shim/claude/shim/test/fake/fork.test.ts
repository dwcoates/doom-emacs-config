import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { forkFakeSession } from "../../src/fake/fork.js";
import { forkableRecords } from "../../src/engine/fork-session.js";

import { testTempDir } from "../temp-dir.js";
import { sequentialIds, writeTranscript } from "../vendor-transcript.js";
import { transcriptPath } from "../../src/engine/vendor-paths.js";

const PARENT_ID = "parent-session";
const PARENT_DIR = "/repo/parent";

/** A parent transcript: a prompt, a progress line, a tool call, its result, and metadata. */
function writeParent(configDir: string): void {
  const lines = [
    { type: "user", uuid: "u1", parentUuid: null, sessionId: PARENT_ID, message: { role: "user", content: "hello" } },
    { type: "progress", uuid: "p1", parentUuid: "u1", sessionId: PARENT_ID },
    {
      type: "assistant",
      uuid: "a1",
      parentUuid: "u1",
      sessionId: PARENT_ID,
      message: { role: "assistant", content: [{ type: "tool_use", id: "toolu_1", name: "Edit", input: { replace_all: false, file_path: "x" } }] },
    },
    { type: "file-history-snapshot", messageId: "m1" },
    {
      type: "user",
      uuid: "u2",
      parentUuid: "a1",
      sessionId: PARENT_ID,
      message: { role: "user", content: [{ type: "tool_result", tool_use_id: "toolu_1", content: "ok" }] },
    },
  ];
  writeTranscript(configDir, PARENT_DIR, PARENT_ID, lines);
}

/** Mint ids id-1, id-2, … so a fork's identities are predictable. */

function readFork(configDir: string, id: string): Array<Record<string, unknown>> {
  return readFileSync(transcriptPath(configDir, PARENT_DIR, id), "utf8")
    .trim()
    .split("\n")
    .map((line) => JSON.parse(line) as Record<string, unknown>);
}

describe("forkFakeSession", () => {
  it("copies every message record whole under new identities, beside the parent", () => {
    // Arrange
    const configDir = testTempDir("fake-fork-");
    writeParent(configDir);

    // Act
    const forked = forkFakeSession(configDir, { parentSessionId: PARENT_ID, parentDir: PARENT_DIR }, sequentialIds());

    // Assert
    const records = readFork(configDir, forked);
    expect(forked).toBe("id-1");
    expect(records.map((record) => record.type)).toEqual(["user", "assistant", "user", "custom-title"]);
    expect(records[1]).toEqual({
      type: "assistant",
      uuid: "id-4",
      parentUuid: "id-2",
      sessionId: "id-1",
      isSidechain: false,
      forkedFrom: { sessionId: PARENT_ID, messageUuid: "a1" },
      message: { role: "assistant", content: [{ type: "tool_use", id: "toolu_1", name: "Edit", input: { replace_all: false, file_path: "x" } }] },
    });
  });

  it("keeps content and key order byte-identical, tool ids included", () => {
    // Arrange
    const configDir = testTempDir("fake-fork-");
    writeParent(configDir);

    // Act
    const forked = forkFakeSession(configDir, { parentSessionId: PARENT_ID, parentDir: PARENT_DIR }, sequentialIds());

    // Assert
    const raw = readFileSync(transcriptPath(configDir, PARENT_DIR, forked), "utf8");
    expect(raw).toContain('"input":{"replace_all":false,"file_path":"x"}');
    expect(raw).toContain('"tool_use_id":"toolu_1"');
  });

  it("cuts through the named record, inclusive", () => {
    // Arrange
    const configDir = testTempDir("fake-fork-");
    writeParent(configDir);

    // Act
    const forked = forkFakeSession(configDir, { parentSessionId: PARENT_ID, parentDir: PARENT_DIR, upToMessageId: "a1" }, sequentialIds());

    // Assert
    expect(readFork(configDir, forked).map((record) => record.forkedFrom)).toEqual([
      { sessionId: PARENT_ID, messageUuid: "u1" },
      { sessionId: PARENT_ID, messageUuid: "a1" },
      undefined,
    ]);
  });

  it.each([
    { name: "a parent with no transcript", parentSessionId: "absent", upTo: undefined, error: /not found in project directory/ },
    { name: "a cut record the parent lacks", parentSessionId: PARENT_ID, upTo: "missing", error: /Message missing not found/ },
  ])("throws, as the vendor does, for $name", ({ parentSessionId, upTo, error }) => {
    // Arrange
    const configDir = testTempDir("fake-fork-");
    writeParent(configDir);

    // Act / Assert
    expect(() =>
      forkFakeSession(configDir, { parentSessionId, parentDir: PARENT_DIR, ...(upTo === undefined ? {} : { upToMessageId: upTo }) }),
    ).toThrow(error);
  });

  it("cuts at exactly the records the fork point resolves against, so the two read one set of types", () => {
    // Arrange: one record of every type a transcript holds, forkable or not.
    const configDir = testTempDir("fake-fork-");
    const types = ["user", "assistant", "attachment", "system", "progress", "queue-operation", "file-history-snapshot", "custom-title"];
    writeTranscript(configDir, PARENT_DIR, PARENT_ID, types.map((type) => ({ type, uuid: `r-${type}`, parentUuid: null })));
    const readable = new Set(forkableRecords(transcriptPath(configDir, PARENT_DIR, PARENT_ID)).map((record) => record.uuid));

    // Act: ask the fake vendor to cut at each record in turn.
    const cuttable = types
      .map((type) => `r-${type}`)
      .filter((uuid) => {
        try {
          forkFakeSession(configDir, { parentSessionId: PARENT_ID, parentDir: PARENT_DIR, upToMessageId: uuid }, sequentialIds());
          return true;
        } catch {
          return false;
        }
      });

    // Assert
    expect(new Set(cuttable)).toEqual(readable);
  });
});
