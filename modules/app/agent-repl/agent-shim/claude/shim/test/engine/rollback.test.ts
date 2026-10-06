/**
 * engine/rollback.ts — the transcript chain RollBackSession plans its cut on,
 * the cut's plan, and the one reading of the vendor refusing a cut.
 */
import { mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import {
  guardAttributes,
  isKeepaliveRecord,
  isPromptRecord,
  liveEnd,
  planCut,
  readLiveChain,
  readTranscriptChain,
  RESUME_DROPS_TURN_REFUSAL_PREFIX,
  vendorCutRefusal,
  type ChainRecord,
  type TranscriptChain,
} from "../../src/engine/rollback.js";
import { KEEPALIVE_PROMPT_MARKER } from "../../src/engine/keepalive.js";
import { logRecordsDuring } from "../log-records.js";

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
    const plan = planCut(conversation(), "p1", ["p1", "p2"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0", guard: { kind: "severalTurns" } });
  });

  it("refuses an unnamed later prompt, naming the first", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p1", ["p1"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "unseenPrompt", vendorPromptUuid: "p2", why: "unnamedPrompt" });
  });

  it("refuses the conversation's first prompt", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p0", ["p0", "p1", "p2"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "firstPrompt" });
  });

  it("refuses a prompt the transcript holds no record of", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "p9", ["p9"], "keep");

    // Assert.
    expect(plan.kind).toBe("promptNotRecorded");
  });

  it("refuses a record that is not a user record", () => {
    // Arrange, Act.
    const plan = planCut(conversation(), "a1", ["a1"], "keep");

    // Assert.
    expect(plan.kind).toBe("promptNotRecorded");
  });

  it("refuses a prompt on a branch the conversation no longer holds", () => {
    // Arrange.
    const dropped = chainOf(transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), user("p2", "a0")]));

    // Act.
    const plan = planCut(dropped, "p1", ["p1"], "keep");

    // Assert.
    expect(plan.kind).toBe("promptNotRecorded");
  });
});

describe("isKeepaliveRecord", () => {
  it("recognizes the shim's own keep-alive by its marker", () => {
    // Arrange.
    const record = user("k", "a", `${KEEPALIVE_PROMPT_MARKER}\nRespond with "." (3)`) as unknown as ChainRecord;

    // Act, Assert.
    expect(isKeepaliveRecord(record)).toBe(true);
  });

  it("does not take a person's prompt for one", () => {
    // Arrange.
    const record = user("p", "a") as unknown as ChainRecord;

    // Act, Assert.
    expect(isKeepaliveRecord(record)).toBe(false);
  });
});

describe("guardAttributes", () => {
  const record = (overrides: Record<string, unknown>): ChainRecord =>
    ({ ...user("u", "a"), ...overrides }) as unknown as ChainRecord;

  it.each([
    ["a dropped turn's prompt", { uuid: "p1" }],
    ["an assistant record", { type: "assistant" }],
    ["a tool-result carrier", { message: { role: "user", content: [{ type: "tool_result", tool_use_id: "t", content: "x" }] } }],
    ["a harness meta record", { isMeta: true }],
    ["a compaction summary", { isCompactSummary: true }],
    ["the interrupt marker", { message: { role: "user", content: "[Request interrupted by user]" } }],
  ])("lets %s go", (_name, overrides) => {
    // Arrange, Act, Assert.
    expect(guardAttributes(record(overrides), new Set(["p1"]))).toBe(true);
  });

  it.each([
    ["an unnamed prompt", {}],
    ["a task notification", { message: { role: "user", content: "<task-notification>done</task-notification>" } }],
    ["the shim's keep-alive", { message: { role: "user", content: `${KEEPALIVE_PROMPT_MARKER} .` } }],
  ])("refuses %s", (_name, overrides) => {
    // Arrange, Act, Assert.
    expect(guardAttributes(record(overrides), new Set(["p1"]))).toBe(false);
  });
});

describe("planCut, the vendor's guard and the files", () => {
  /** p0 answered, then p1's turn: its answer and one extra user record after it. */
  const withAfter = (extra: Record<string, unknown>): TranscriptChain =>
    chainOf(transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1"), extra]));

  const keepalive = user("k1", "a1", `${KEEPALIVE_PROMPT_MARKER}\nRespond with "." (1)`);
  const notification = user("n1", "a1", "<task-notification>done</task-notification>");

  it("arms the guard for one dropped turn whose entries are all its own", () => {
    // Arrange.
    const chain = chainOf(transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1")]));

    // Act.
    const plan = planCut(chain, "p1", ["p1"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0", guard: { kind: "armed", resumeDropsTurn: "p1" } });
  });

  it("does not arm the guard, and does not refuse, when a keep-alive sits past the fork point", () => {
    // Arrange, Act.
    const plan = planCut(withAfter(keepalive), "p1", ["p1"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0", guard: { kind: "unattributable", recordUuid: "k1" } });
  });

  it("does not refuse a restore over a keep-alive past the fork point", () => {
    // Arrange, Act.
    const plan = planCut(withAfter(keepalive), "p1", ["p1"], "restore");

    // Assert.
    expect(plan.kind).toBe("cut");
  });

  it("refuses a restore unseen_prompt for a task notification past the fork point", () => {
    // Arrange, Act.
    const plan = planCut(withAfter(notification), "p1", ["p1"], "restore");

    // Assert.
    expect(plan).toEqual({ kind: "unseenPrompt", vendorPromptUuid: "n1", why: "guardWouldRefuse" });
  });

  it("cuts a keep-files rollback over a task notification, with the guard unarmed", () => {
    // Arrange, Act.
    const plan = planCut(withAfter(notification), "p1", ["p1"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0", guard: { kind: "unattributable", recordUuid: "n1" } });
  });
});

describe("liveEnd", () => {
  const rolledBack = (...uuids: string[]): ReadonlySet<string> => new Set(uuids);

  it("answers whole when no rolled-back prompt is on the chain", () => {
    // Arrange.
    const chain = chainOf(transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0")]));

    // Act, Assert.
    expect(liveEnd(chain, rolledBack("p9"))).toEqual({ kind: "whole" });
  });

  it("ends just before the earliest rolled-back prompt on the chain", () => {
    // Arrange.
    const chain = chainOf(
      transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1"), user("p2", "a1"), assistant("a2", "p2")]),
    );

    // Act, Assert.
    expect(liveEnd(chain, rolledBack("p2", "p1"))).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0" });
  });

  it("answers whole once the next prompt started a branch at the fork point", () => {
    // Arrange: p1 was rolled back, and p2 was sent at a0.
    const chain = chainOf(transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1"), user("p2", "a0")]));

    // Act, Assert.
    expect(liveEnd(chain, rolledBack("p1"))).toEqual({ kind: "whole" });
  });

  it("crosses a compaction to the rolled-back prompt before it", () => {
    // Arrange.
    const chain = chainOf(
      transcript([
        user("p0", null),
        assistant("a0", "p0"),
        user("p1", "a0"),
        assistant("a1", "p1"),
        { type: "system", subtype: "compact_boundary", uuid: "b0", parentUuid: null, logicalParentUuid: "a1" },
        { ...user("s0", "b0", "summary"), isCompactSummary: true },
      ]),
    );

    // Act, Assert.
    expect(liveEnd(chain, rolledBack("p1"))).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0" });
  });

  it("answers firstPrompt when a rolled-back prompt opens the chain", () => {
    // Arrange.
    const chain = chainOf(transcript([user("p0", null), assistant("a0", "p0")]));

    // Act, Assert.
    expect(liveEnd(chain, rolledBack("p0"))).toEqual({ kind: "firstPrompt", promptUuid: "p0" });
  });
});

describe("readLiveChain", () => {
  it("ends the live chain at the fork point before a rolled-back prompt", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1")]);

    // Act.
    const read = readLiveChain(file, new Set(["p1"]));

    // Assert.
    expect(read.kind === "ok" ? uuids(read.transcript) : read.kind).toEqual(["p0", "a0"]);
  });

  it("answers the newest record's whole chain when nothing on it was rolled back", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0")]);

    // Act.
    const read = readLiveChain(file, new Set(["p9"]));

    // Assert.
    expect(read.kind === "ok" ? uuids(read.transcript) : read.kind).toEqual(["p0", "a0", "p1"]);
  });

  it("answers no_transcript when the vendor wrote none", () => {
    // Arrange, Act.
    const read = readLiveChain(path.join(os.tmpdir(), "shim-rollback-absent", "none.jsonl"), new Set(["p1"]));

    // Assert.
    expect(read.kind).toBe("no_transcript");
  });

  it("answers unreadable, at ERROR, when a rolled-back prompt opens the conversation", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0")]);
    let kind = "";

    // Act.
    const records = logRecordsDuring(() => {
      kind = readLiveChain(file, new Set(["p0"])).kind;
    });

    // Assert.
    expect({ kind, record: records.find((record) => record.message.includes("live end cannot be found")) }).toMatchObject({
      kind: "unreadable",
      record: { level: "error", context: { prompt_uuid: "p0" } },
    });
  });
});

describe("planCut, a turn the vendor refused for a usage limit", () => {
  // THE SHAPE A RATE-LIMITED TURN LEAVES (the owner's own transcripts,
  // 2026-10-06): the prompt's user record, its attachments, then a
  // `<synthetic>` assistant record carrying the 429, then the turn's system
  // records. The prompt IS recorded, so the daemon cuts it out before it holds
  // the prompt again (daemon/internal/promptqueue/rehold.go).
  const rateLimited = (): TranscriptChain =>
    chainOf(
      transcript([
        user("p0", null),
        assistant("a0", "p0"),
        user("p1", "a0"),
        { type: "attachment", uuid: "att1", parentUuid: "p1", attachment: { type: "queued_command" } },
        {
          type: "assistant",
          uuid: "e1",
          parentUuid: "att1",
          isApiErrorMessage: true,
          error: "rate_limit",
          apiErrorStatus: 429,
          message: { role: "assistant", model: "<synthetic>", content: [{ type: "text", text: "You've hit your session limit" }] },
        },
        { type: "system", subtype: "turn_duration", uuid: "s1", parentUuid: "e1" },
      ]),
    );

  it("holds the refused prompt on the live chain", () => {
    // Arrange, Act, Assert.
    expect(uuids(rateLimited())).toContain("p1");
  });

  it("cuts just before the refused prompt", () => {
    // Arrange, Act.
    const plan = planCut(rateLimited(), "p1", ["p1"], "keep");

    // Assert.
    expect(plan).toMatchObject({ kind: "cut", promptUuid: "p1", forkPoint: "a0" });
  });
});
