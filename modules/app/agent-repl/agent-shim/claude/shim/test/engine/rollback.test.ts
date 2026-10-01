/**
 * engine/rollback.ts — the transcript chain RollBackSession plans its cut on,
 * the cut's plan, and the one reading of the vendor refusing a cut.
 */
import { mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import {
  isPromptRecord,
  planCut,
  readTranscriptChain,
  RESUME_DROPS_TURN_REFUSAL_PREFIX,
  vendorCutRefusal,
  type ChainRecord,
  type TranscriptChain,
} from "../../src/engine/rollback.js";
import { KEEPALIVE_PROMPT_MARKER } from "../../src/engine/keepalive.js";

/** A transcript file holding `lines`, one JSON record (or raw text) per line. */
function transcript(lines: (Record<string, unknown> | string)[]): string {
  const file = path.join(mkdtempSync(path.join(os.tmpdir(), "shim-rollback-")), "session.jsonl");
  writeFileSync(file, `${lines.map((line) => (typeof line === "string" ? line : JSON.stringify(line))).join("\n")}\n`);
  return file;
}

const user = (uuid: string, parentUuid: string | null, content: unknown = `prompt ${uuid}`): Record<string, unknown> => ({
  type: "user",
  uuid,
  parentUuid,
  message: { role: "user", content },
});

const assistant = (uuid: string, parentUuid: string): Record<string, unknown> => ({
  type: "assistant",
  uuid,
  parentUuid,
  message: { role: "assistant", content: [{ type: "text", text: "ok" }] },
});

/** The chain the file holds, or a thrown failure naming why it holds none. */
function chainOf(file: string, head?: string): TranscriptChain {
  const read = readTranscriptChain(file, head);
  if (read.kind !== "ok") throw new Error(`no chain: ${read.kind}`);
  return read.transcript;
}

const uuids = (chain: TranscriptChain): string[] => chain.chain.map((record) => record.uuid);

describe("vendorCutRefusal", () => {
  it("answers the resume-drops-turn guard's refusal from its prefix on", () => {
    // Arrange.
    const evidence = `Error: ${RESUME_DROPS_TURN_REFUSAL_PREFIX} resuming at a1 would discard entries\nexit 1`;

    // Act.
    const refusal = vendorCutRefusal(evidence);

    // Assert.
    expect(refusal).toBe(`${RESUME_DROPS_TURN_REFUSAL_PREFIX} resuming at a1 would discard entries`);
  });

  it("answers the vendor's unknown-fork-point refusal", () => {
    // Arrange, Act.
    const refusal = vendorCutRefusal("No message found with message.uuid of: a9");

    // Assert.
    expect(refusal).toBe("No message found with message.uuid of: a9");
  });

  it("answers a line naming the anchor when no refusal sentence appears", () => {
    // Arrange, Act.
    const refusal = vendorCutRefusal("error_during_execution: bad anchor a9\nother", "a9");

    // Assert.
    expect(refusal).toBe("error_during_execution: bad anchor a9");
  });

  it("answers nothing when the evidence holds no refusal", () => {
    // Arrange, Act.
    const refusal = vendorCutRefusal("Claude Code process exited with code 1");

    // Assert.
    expect(refusal).toBeUndefined();
  });
});

describe("readTranscriptChain", () => {
  it("answers no_transcript when the vendor wrote none", () => {
    // Arrange, Act.
    const read = readTranscriptChain(path.join(os.tmpdir(), "shim-rollback-absent", "none.jsonl"));

    // Assert.
    expect(read.kind).toBe("no_transcript");
  });

  it("walks the chain from the file's last main-thread record, oldest first", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0")]);

    // Act.
    const chain = chainOf(file);

    // Assert.
    expect(uuids(chain)).toEqual(["p0", "a0", "p1"]);
  });

  it("skips a torn line", () => {
    // Arrange.
    const file = transcript([user("p0", null), "{\"type\":\"assis", assistant("a0", "p0")]);

    // Act.
    const chain = chainOf(file);

    // Assert.
    expect(uuids(chain)).toEqual(["p0", "a0"]);
  });

  it("leaves a branch an earlier rollback dropped out of the conversation", () => {
    // Arrange: p1's turn was dropped, and p2 was sent at the fork point a0.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1"), user("p2", "a0")]);

    // Act.
    const chain = chainOf(file);

    // Assert.
    expect(uuids(chain)).toEqual(["p0", "a0", "p2"]);
  });

  it("walks from a named head instead of the file's tail", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1")]);

    // Act.
    const chain = chainOf(file, "a0");

    // Assert.
    expect(uuids(chain)).toEqual(["p0", "a0"]);
  });

  it("ignores a sidechain record as the head", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), { ...assistant("s0", "a0"), isSidechain: true }]);

    // Act.
    const chain = chainOf(file);

    // Assert.
    expect(uuids(chain)).toEqual(["p0", "a0"]);
  });

  it("crosses a compaction through its logical parent", () => {
    // Arrange.
    const file = transcript([
      user("p0", null),
      assistant("a0", "p0"),
      { type: "system", subtype: "compact_boundary", uuid: "b0", parentUuid: null, logicalParentUuid: "a0" },
      user("p1", "b0"),
    ]);

    // Act.
    const chain = chainOf(file);

    // Assert.
    expect(uuids(chain)).toEqual(["p0", "a0", "b0", "p1"]);
  });

  it("answers unreadable when the chain names a record the file does not hold", () => {
    // Arrange.
    const file = transcript([user("p1", "missing")]);

    // Act.
    const read = readTranscriptChain(file);

    // Assert.
    expect(read.kind).toBe("unreadable");
  });

  it("answers unreadable when the chain loops", () => {
    // Arrange.
    const file = transcript([user("p0", "a0"), assistant("a0", "p0")]);

    // Act.
    const read = readTranscriptChain(file);

    // Assert.
    expect(read.kind).toBe("unreadable");
  });
});

describe("isPromptRecord", () => {
  const record = (overrides: Record<string, unknown>): ChainRecord =>
    ({ ...user("u", "a"), ...overrides }) as unknown as ChainRecord;

  it("counts a person's prompt", () => {
    // Arrange, Act, Assert.
    expect(isPromptRecord(record({}))).toBe(true);
  });

  it.each([
    ["a tool-result carrier", { message: { role: "user", content: [{ type: "tool_result", tool_use_id: "t", content: "x" }] } }],
    ["a harness meta record", { isMeta: true }],
    ["a compaction summary", { isCompactSummary: true }],
    ["the interrupt marker", { message: { role: "user", content: "[Request interrupted by user]" } }],
    ["a task notification", { message: { role: "user", content: "<task-notification>done</task-notification>" } }],
    ["a local command's stderr", { message: { role: "user", content: "<local-command-stderr>x</local-command-stderr>" } }],
    ["a record whose origin is not a person", { origin: { kind: "task-notification" } }],
    ["the shim's keep-alive", { message: { role: "user", content: `${KEEPALIVE_PROMPT_MARKER} .` } }],
    ["an assistant record", { type: "assistant" }],
  ])("does not count %s", (_name, overrides) => {
    // Arrange, Act, Assert.
    expect(isPromptRecord(record(overrides))).toBe(false);
  });
});

describe("planCut", () => {
  const conversation = (): TranscriptChain =>
    chainOf(
      transcript([
        user("p0", null),
        assistant("a0", "p0"),
        user("p1", "a0"),
        assistant("a1", "p1"),
        user("[interrupt]", "a1", "[Request interrupted by user]"),
        user("p2", "[interrupt]"),
        assistant("a2", "p2"),
      ]),
    );

  it("cuts at the prompt's parent when every later prompt is named", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p1", ["p1", "p2"]);

    // Assert.
    expect(plan).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0" });
  });

  it("refuses an unnamed later prompt, naming the first", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p1", ["p1"]);

    // Assert.
    expect(plan).toEqual({ kind: "unseenPrompt", vendorPromptUuid: "p2" });
  });

  it("refuses the conversation's first prompt", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p0", ["p0", "p1", "p2"]);

    // Assert.
    expect(plan).toEqual({ kind: "firstPrompt" });
  });

  it("refuses a prompt the transcript holds no record of", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p9", ["p9"]);

    // Assert.
    expect(plan.kind).toBe("promptNotRecorded");
  });

  it("refuses a record that is not a user record", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "a1", ["a1"]);

    // Assert.
    expect(plan.kind).toBe("promptNotRecorded");
  });

  it("refuses a prompt on a branch the conversation no longer holds", () => {
    // Arrange.
    const dropped = chainOf(transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), user("p2", "a0")]));

    // Act.
    const plan = planCut(dropped, "p1", ["p1"]);

    // Assert.
    expect(plan.kind).toBe("promptNotRecorded");
  });
});
