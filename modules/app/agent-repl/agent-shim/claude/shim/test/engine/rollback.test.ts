/**
 * engine/rollback.ts — the transcript chain RollBackSession plans its cut on,
 * the cut's plan, and the one reading of the vendor refusing a cut.
 */
import { appendFileSync, mkdtempSync, mkdirSync, renameSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";
import {
  guardAttributes,
  isKeepaliveRecord,
  isPromptRecord,
  liveEnd,
  planCut,
  chainRecordOf,
  createTranscriptIndex,
  readLiveChain,
  readTranscriptChain,
  RESUME_DROPS_TURN_REFUSAL_PREFIX,
  vendorCutRefusal,
  type ChainRecord,
  type TranscriptChain,
} from "../../src/engine/rollback.js";
import { KEEPALIVE_PROMPT_MARKER } from "../../src/engine/keepalive.js";
import { judgeTruncatingResume } from "../../src/fake/rollback.js";
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

  it("starts the chain at a compaction's boundary", () => {
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
    expect(uuids(chain)).toEqual(["b0", "p1"]);
  });

  it("reads a boundary whose logical parent the vendor wrote after the summary", () => {
    // Arrange: Claude Code 2.1.296's layout on the owner's doom transcript
    // (2026-10-13). The last preserved record t0 is written under the summary
    // s0, and the boundary names it as its logical parent.
    const file = transcript([
      user("p0", null),
      assistant("a0", "p0"),
      { type: "system", subtype: "compact_boundary", uuid: "b0", parentUuid: null, logicalParentUuid: "t0" },
      { ...user("s0", "b0", "summary"), isCompactSummary: true },
      { type: "attachment", uuid: "t0", parentUuid: "s0" },
      user("p1", "t0"),
    ]);

    // Act.
    const chain = chainOf(file);

    // Assert.
    expect(uuids(chain)).toEqual(["b0", "s0", "t0", "p1"]);
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

  it("refuses a prompt from before the newest compaction", () => {
    // Arrange.
    const compacted = chainOf(
      transcript([
        user("p0", null),
        assistant("a0", "p0"),
        user("p1", "a0"),
        assistant("a1", "p1"),
        { type: "system", subtype: "compact_boundary", uuid: "b0", parentUuid: null, logicalParentUuid: "a1" },
        { ...user("s0", "b0", "summary"), isCompactSummary: true },
        user("p2", "s0"),
      ]),
    );

    // Act.
    const plan = planCut(compacted, "p1", ["p1", "p2"], "keep");

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

  /** p0, p1 and p2 answered, then p2 rolled back: the file's newest record is still p2's answer. */
  const afterEarlierRollback = (): string =>
    transcript([
      user("p0", null),
      assistant("a0", "p0"),
      user("p1", "a0"),
      assistant("a1", "p1"),
      user("p2", "a1"),
      assistant("a2", "p2"),
    ]);
  const liveAfterEarlierRollback = (file: string): TranscriptChain => {
    const read = readLiveChain(createTranscriptIndex(file), new Set(["p2"]));
    if (read.kind !== "ok") throw new Error(`no live chain: ${read.kind}`);
    return read.transcript;
  };

  it("does not arm the guard when an earlier rollback's dropped prompt is still on the chain the vendor loads", () => {
    // Arrange.
    const live = liveAfterEarlierRollback(afterEarlierRollback());

    // Act.
    const plan = planCut(live, "p1", ["p1"], "keep");

    // Assert.
    expect(plan).toEqual({ kind: "cut", promptUuid: "p1", forkPoint: "a0", guard: { kind: "unattributable", recordUuid: "p2" } });
  });

  it("plans a cut past an earlier rollback that the mocked vendor boots", () => {
    // Arrange.
    const file = afterEarlierRollback();
    const plan = planCut(liveAfterEarlierRollback(file), "p1", ["p1"], "keep");
    if (plan.kind !== "cut") throw new Error(`no cut: ${plan.kind}`);
    const guard = plan.guard.kind === "armed" ? plan.guard.resumeDropsTurn : undefined;

    // Act.
    const boot = judgeTruncatingResume(file, plan.forkPoint, guard);

    // Assert.
    expect(boot).toEqual({ kind: "booted" });
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

  it("answers whole for a rolled-back prompt from before the newest compaction", () => {
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
    expect(liveEnd(chain, rolledBack("p1"))).toEqual({ kind: "whole" });
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
    const read = readLiveChain(createTranscriptIndex(file), new Set(["p1"]));

    // Assert.
    expect(read.kind === "ok" ? uuids(read.transcript) : read.kind).toEqual(["p0", "a0"]);
  });

  it("keeps the whole walked chain past a cut live end, through the dropped branch", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1")]);

    // Act.
    const read = readLiveChain(createTranscriptIndex(file), new Set(["p1"]));

    // Assert.
    expect(read.kind === "ok" ? read.transcript.walked.map((record) => record.uuid) : read.kind).toEqual(["p0", "a0", "p1", "a1"]);
  });

  it("answers the newest record's whole chain when nothing on it was rolled back", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0")]);

    // Act.
    const read = readLiveChain(createTranscriptIndex(file), new Set(["p9"]));

    // Assert.
    expect(read.kind === "ok" ? uuids(read.transcript) : read.kind).toEqual(["p0", "a0", "p1"]);
  });

  it("answers no_transcript when the vendor wrote none", () => {
    // Arrange, Act.
    const read = readLiveChain(
      createTranscriptIndex(path.join(os.tmpdir(), "shim-rollback-absent", "none.jsonl")),
      new Set(["p1"]),
    );

    // Assert.
    expect(read.kind).toBe("no_transcript");
  });

  it("answers unreadable, at ERROR, when a rolled-back prompt opens the conversation", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0")]);
    let kind = "";

    // Act.
    const records = logRecordsDuring(() => {
      kind = readLiveChain(createTranscriptIndex(file), new Set(["p0"])).kind;
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

/** LINES as transcript text, one JSON record per line. */
const linesOf = (lines: Record<string, unknown>[]): string => lines.map((line) => `${JSON.stringify(line)}\n`).join("");

describe("createTranscriptIndex", () => {
  it("reads what the file gained since the last read", () => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0")]);
    const index = createTranscriptIndex(file);
    index.read();
    appendFileSync(file, linesOf([user("p1", "a0"), assistant("a1", "p1")]));

    // Act.
    const read = index.read();

    // Assert.
    expect(read.kind === "ok" ? uuids(read.transcript) : read.kind).toEqual(["p0", "a0", "p1", "a1"]);
  });

  it("takes in what the file gained on a refresh, for the next read", () => {
    // Arrange.
    const file = transcript([user("p0", null)]);
    const index = createTranscriptIndex(file);
    index.refresh();
    appendFileSync(file, linesOf([assistant("a0", "p0")]));
    index.refresh();

    // Act.
    const read = index.read();

    // Assert.
    expect(read.kind === "ok" ? uuids(read.transcript) : read.kind).toEqual(["p0", "a0"]);
  });

  it.each([
    {
      name: "a replaced file",
      change: (file: string) => {
        const next = `${file}.next`;
        writeFileSync(next, linesOf([user("q0", null), assistant("q1", "q0")]));
        renameSync(next, file);
      },
    },
    { name: "a file that shrank", change: (file: string) => writeFileSync(file, linesOf([user("q0", null), assistant("q1", "q0")])) },
  ])("reads $name again from its start", ({ change }) => {
    // Arrange.
    const file = transcript([user("p0", null), assistant("a0", "p0"), user("p1", "a0"), assistant("a1", "p1")]);
    const index = createTranscriptIndex(file);
    index.read();
    change(file);

    // Act.
    const read = index.read();

    // Assert.
    expect(read.kind === "ok" ? [uuids(read.transcript), read.transcript.byUuid.has("p0")] : read.kind).toEqual([["q0", "q1"], false]);
  });

  it("reads an unterminated last line without keeping it, and reads it whole once its newline lands", () => {
    // Arrange.
    const file = transcript([user("p0", null)]);
    const index = createTranscriptIndex(file);
    const line = JSON.stringify(assistant("a0", "p0"));
    appendFileSync(file, line);
    const before = index.read();
    appendFileSync(file, `\n${JSON.stringify(user("p1", "a0"))}\n`);

    // Act.
    const after = index.read();

    // Assert.
    expect([before, after].map((read) => (read.kind === "ok" ? uuids(read.transcript) : read.kind))).toEqual([
      ["p0", "a0"],
      ["p0", "a0", "p1"],
    ]);
  });

  it("skips a torn last line, and reads it once it is written whole", () => {
    // Arrange.
    const file = transcript([user("p0", null)]);
    const index = createTranscriptIndex(file);
    const line = JSON.stringify(assistant("a0", "p0"));
    appendFileSync(file, line.slice(0, 20));
    const before = index.read();
    appendFileSync(file, `${line.slice(20)}\n`);

    // Act.
    const after = index.read();

    // Assert.
    expect([before, after].map((read) => (read.kind === "ok" ? uuids(read.transcript) : read.kind))).toEqual([["p0"], ["p0", "a0"]]);
  });

  it("answers no_transcript for a file that went away, and reads it afresh when it returns", () => {
    // Arrange.
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-rollback-"));
    const file = path.join(dir, "session.jsonl");
    const index = createTranscriptIndex(file);
    const absent = index.read();
    writeFileSync(file, linesOf([user("p0", null)]));

    // Act.
    const present = index.read();

    // Assert.
    expect([absent.kind, present.kind === "ok" ? uuids(present.transcript) : present.kind]).toEqual(["no_transcript", ["p0"]]);
  });

  it("answers unreadable, at ERROR, when the file cannot be read", () => {
    // Arrange: a directory where the transcript should be.
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-rollback-"));
    const file = path.join(dir, "session.jsonl");
    mkdirSync(file);
    let kind = "";

    // Act.
    const records = logRecordsDuring(() => {
      kind = createTranscriptIndex(file).read().kind;
    });

    // Assert.
    expect({ kind, record: records.find((record) => record.message.includes("could not be read for its chain")) }).toMatchObject({
      kind: "unreadable",
      record: { level: "error", context: { file } },
    });
  });

  it("records a refresh that cannot read the file at ERROR", () => {
    // Arrange.
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-rollback-"));
    const file = path.join(dir, "session.jsonl");
    mkdirSync(file);

    // Act.
    const records = logRecordsDuring(() => {
      createTranscriptIndex(file).refresh();
    });

    // Assert.
    expect(records.find((record) => record.message.includes("could not be read to refresh its index"))).toMatchObject({
      level: "error",
      context: { file },
    });
  });
});

describe("chainRecordOf", () => {
  it.each([
    {
      name: "a prompt keeps its text",
      record: user("p0", null, "hello"),
      want: { type: "user", uuid: "p0", parentUuid: null, message: { role: "user", content: "hello" } },
    },
    {
      name: "a tool result keeps its block types, not its output",
      record: user("r0", "a0", [{ type: "tool_result", tool_use_id: "t", content: "x".repeat(1000) }]),
      want: { type: "user", uuid: "r0", parentUuid: "a0", message: { role: "user", content: [{ type: "tool_result" }] } },
    },
    {
      name: "a text block keeps its text",
      record: user("p1", "a0", [{ type: "text", text: "hi", cache_control: {} }]),
      want: { type: "user", uuid: "p1", parentUuid: "a0", message: { role: "user", content: [{ type: "text", text: "hi" }] } },
    },
    {
      name: "the chain's own fields are kept",
      record: { type: "system", subtype: "compact_boundary", uuid: "b0", isMeta: true, isSidechain: false, isCompactSummary: true, origin: { kind: "x", extra: 1 } },
      want: { type: "system", subtype: "compact_boundary", uuid: "b0", isMeta: true, isSidechain: false, isCompactSummary: true, origin: { kind: "x" } },
    },
  ])("slims a record: $name", ({ record, want }) => {
    // Act.
    const slim = chainRecordOf(record);

    // Assert.
    expect(slim).toEqual(want);
  });
});

describe("what a read keeps of each record", () => {
  /** An answer holding a text block and a tool call. */
  const answerContent = [
    { type: "text", text: "ok" },
    { type: "tool_use", name: "Edit", input: { file_path: "/x" } },
  ];

  it.each([
    { reader: "a one-shot read keeps the whole record", read: (file: string) => readTranscriptChain(file), want: answerContent },
    {
      reader: "an index keeps only the chain's slice",
      read: (file: string) => createTranscriptIndex(file).read(),
      want: [{ type: "text", text: "ok" }, { type: "tool_use" }],
    },
  ])("$reader", ({ read, want }) => {
    // Arrange.
    const file = transcript([
      user("p0", null),
      { type: "assistant", uuid: "a0", parentUuid: "p0", message: { role: "assistant", content: answerContent } },
    ]);

    // Act.
    const got = read(file);

    // Assert.
    expect(got.kind === "ok" ? got.transcript.byUuid.get("a0")?.message?.content : got.kind).toEqual(want);
  });
});
