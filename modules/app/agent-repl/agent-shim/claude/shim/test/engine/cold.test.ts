/**
 * The cold gate: the four facts read off the transcript, and the judgement.
 *
 * WHAT THIS GUARDS: that a cold continuation is REFUSED with its cost before
 * any money is spent. The failure mode being excluded is a resume that quietly
 * pays full price for a lapsed cache — the user learns about it on the bill, at
 * which point the refusal this module exists to produce is worthless.
 */
import { mkdirSync, mkdtempSync, writeFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { writeSync } from "node:fs";
import { describe, expect, it, vi } from "vitest";
import { keepalivePromptText } from "../../src/engine/keepalive.js";
import {
  CACHE_TTL_1H_MS,
  CACHE_TTL_5M_MS,
  COLD_GATE_FLOOR_TOKENS,
  cwdSlug,
  judgeCold,
  readTranscriptFacts,
  sessionCold,
  TRANSCRIPT_OPENING_MAX_CHARS,
  transcriptPath,
  type TranscriptFacts,
} from "../../src/engine/cold.js";

function transcript(lines: unknown[]): string {
  const dir = mkdtempSync(path.join(os.tmpdir(), "shim-cold-"));
  const file = path.join(dir, "session.jsonl");
  writeFileSync(file, lines.map((line) => JSON.stringify(line)).join("\n"), "utf8");
  return file;
}

function assistant(overrides: Record<string, unknown> = {}): Record<string, unknown> {
  return {
    type: "assistant",
    timestamp: "2026-08-29T12:00:00.000Z",
    message: {
      model: "claude-opus-5",
      usage: {
        input_tokens: 2,
        cache_creation_input_tokens: 100,
        cache_read_input_tokens: 1000,
        cache_creation: { ephemeral_1h_input_tokens: 0, ephemeral_5m_input_tokens: 100 },
      },
    },
    ...overrides,
  };
}

describe("the project-directory slug", () => {
  it("replaces every byte outside [A-Za-z0-9] with a dash", () => {
    expect(cwdSlug("/Users/x/.config/y")).toBe("-Users-x--config-y");
  });

  it("replaces underscores too", () => {
    expect(cwdSlug("/private/var/folders/_m/x")).toBe("-private-var-folders--m-x");
  });

  it("preserves case", () => {
    expect(cwdSlug("/Users/DodgeC")).toBe("-Users-DodgeC");
  });
});

describe("the transcript path", () => {
  it("names the session file under the account's projects directory", () => {
    expect(transcriptPath("/account", "/ws", "s-1")).toBe("/account/projects/-ws/s-1.jsonl");
  });
});

describe("reading the transcript's facts", () => {
  it("answers absence when there is no transcript", () => {
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-cold-"));

    expect(readTranscriptFacts(path.join(dir, "missing.jsonl"))).toBeUndefined();
  });

  it("sums the last request's cache reads, cache writes and fresh input", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.contextTokens).toBe(1102);
  });

  it("takes the LAST assistant line's figures, not the first", () => {
    const file = transcript([
      assistant(),
      assistant({ message: { model: "m", usage: { input_tokens: 1, cache_read_input_tokens: 5 } } }),
    ]);

    expect(readTranscriptFacts(file)?.contextTokens).toBe(6);
  });

  it("reads the request instant from the line's timestamp", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.lastRequestAtMs).toBe(
      Date.parse("2026-08-29T12:00:00.000Z"),
    );
  });

  it("reads the 5-minute tier when only the 5m ephemeral bucket was written", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.cacheTtlMs).toBe(CACHE_TTL_5M_MS);
  });

  it("reads the 1-hour tier when the 1h ephemeral bucket was written", () => {
    const file = transcript([
      assistant({
        message: {
          model: "m",
          usage: { cache_creation: { ephemeral_1h_input_tokens: 14001, ephemeral_5m_input_tokens: 0 } },
        },
      }),
    ]);

    expect(readTranscriptFacts(file)?.cacheTtlMs).toBe(CACHE_TTL_1H_MS);
  });

  it("recovers the model the conversation was last answered by", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.lastModel).toBe("claude-opus-5");
  });

  it("never recovers the synthetic marker as the model", () => {
    // Arrange: the CLI's own notice for a refused request comes after a real answer.
    const file = transcript([
      assistant(),
      assistant({
        isApiErrorMessage: true,
        message: { model: "<synthetic>", usage: { input_tokens: 0, cache_read_input_tokens: 0 } },
      }),
    ]);

    // Act + Assert
    expect(readTranscriptFacts(file)?.lastModel).toBe("claude-opus-5");
  });

  it("never reads a CLI notice's zeroed usage as the conversation's size", () => {
    // Arrange
    const file = transcript([
      assistant(),
      assistant({
        isApiErrorMessage: true,
        message: { model: "<synthetic>", usage: { input_tokens: 0, cache_read_input_tokens: 0 } },
      }),
    ]);

    // Act + Assert
    expect(readTranscriptFacts(file)?.contextTokens).toBe(1102);
  });

  it("skips a synthetic record that is not flagged as an api error", () => {
    // Arrange: a session-limit or stop notice carries the marker without the flag.
    const file = transcript([
      assistant(),
      assistant({ message: { model: "<synthetic>", usage: { input_tokens: 0 } } }),
    ]);

    // Act + Assert
    expect(readTranscriptFacts(file)?.lastModel).toBe("claude-opus-5");
  });

  it("recovers no model from a transcript that holds only CLI notices", () => {
    // Arrange
    const file = transcript([
      assistant({ isApiErrorMessage: true, message: { model: "<synthetic>", usage: { input_tokens: 0 } } }),
    ]);

    // Act + Assert
    expect(readTranscriptFacts(file)?.lastModel).toBeUndefined();
  });

  it("recovers the permission mode from the last user record", () => {
    const file = transcript([
      { type: "user", permissionMode: "default" },
      { type: "user", permissionMode: "auto" },
      assistant(),
    ]);

    expect(readTranscriptFacts(file)?.lastPermissionMode).toBe("auto");
  });

  it("skips an unparsable trailing line rather than refusing to start", () => {
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-cold-"));
    mkdirSync(dir, { recursive: true });
    const file = path.join(dir, "torn.jsonl");
    writeFileSync(file, `${JSON.stringify(assistant())}\n{"type":"assist`, "utf8");

    expect(readTranscriptFacts(file)?.contextTokens).toBe(1102);
  });

  it("states that it saw usage when an assistant line carried some", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.sawUsage).toBe(true);
  });

  it("states that it saw NO usage when no assistant line carried any", () => {
    // A zero that cannot be told from an absence ranks an unread conversation
    // as the cheapest one.
    const file = transcript([{ type: "user", message: { role: "user", content: "just asked" } }]);

    expect(readTranscriptFacts(file)?.sawUsage).toBe(false);
  });

  it("takes the OPENING from the first user prompt, not the last", () => {
    const file = transcript([
      { type: "user", message: { role: "user", content: "explain hash tables" } },
      { type: "user", message: { role: "user", content: "now the collisions" } },
    ]);

    expect(readTranscriptFacts(file)?.opening).toBe("explain hash tables");
  });

  it("truncates the opening at the stated cap", () => {
    const file = transcript([
      { type: "user", message: { role: "user", content: "x".repeat(500) } },
    ]);

    expect(readTranscriptFacts(file)?.opening).toBe("x".repeat(TRANSCRIPT_OPENING_MAX_CHARS));
  });

  it("never opens a listing with the shim's own keep-alive prompt", () => {
    // A keep-alive is never served; a conversation that began idle opens with
    // the first thing a person said.
    const file = transcript([
      { type: "user", message: { role: "user", content: keepalivePromptText(1) } },
      { type: "user", message: { role: "user", content: "explain hash tables" } },
    ]);

    expect(readTranscriptFacts(file)?.opening).toBe("explain hash tables");
  });

  it("states no opening when the transcript holds no user prompt", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.opening).toBeUndefined();
  });

  it("counts the user prompts the transcript holds", () => {
    const file = transcript([
      { type: "user", message: { role: "user", content: "one" } },
      assistant(),
      { type: "user", message: { role: "user", content: "two" } },
    ]);

    expect(readTranscriptFacts(file)?.prompts).toBe(2);
  });

  it("does not count a tool result as a prompt", () => {
    const file = transcript([
      { type: "user", message: { role: "user", content: "one" } },
      { type: "user", message: { role: "user", content: [{ type: "tool_result", text: "out" }] } },
    ]);

    expect(readTranscriptFacts(file)?.prompts).toBe(1);
  });

  it("reports the clear's instant when the last boundary is a clear", () => {
    const file = transcript([
      {
        type: "user",
        timestamp: "2026-08-29T11:00:00.000Z",
        message: { role: "user", content: "<command-name>/clear</command-name>" },
      },
      { type: "user", message: { role: "user", content: "fresh start" } },
    ]);

    expect(readTranscriptFacts(file)?.clearedAtMs).toBe(Date.parse("2026-08-29T11:00:00.000Z"));
  });

  it("reports NO clear when a compaction came after it", () => {
    // Last-writer-wins: the conversation no longer resumes empty.
    const file = transcript([
      {
        type: "user",
        timestamp: "2026-08-29T11:00:00.000Z",
        message: { role: "user", content: "<command-name>/clear</command-name>" },
      },
      { type: "system", subtype: "compact_boundary" },
    ]);

    expect(readTranscriptFacts(file)?.clearedAtMs).toBeUndefined();
  });

  it("reports no clear for a conversation that was never cut", () => {
    expect(readTranscriptFacts(transcript([assistant()]))?.clearedAtMs).toBeUndefined();
  });
});

// ABOVE THE COLD-GATE FLOOR ON PURPOSE. The gate does not ask below 70,000
// tokens, so a fixture under it would make every judging case below read
// "warm" for the floor's reason rather than the one it is testing.
const FACTS: TranscriptFacts = {
  contextTokens: 100_000,
  sawUsage: true,
  lastRequestAtMs: 1_000_000,
  cacheTtlMs: CACHE_TTL_5M_MS,
  lastModel: "claude-opus-5",
  prompts: 1,
};

describe("judging the cache", () => {
  it("is warm inside the tier", () => {
    expect(judgeCold(FACTS, 1_000_000 + CACHE_TTL_5M_MS - 1, "claude-opus-5")).toBeUndefined();
  });

  it("is LAPSED past the tier", () => {
    expect(judgeCold(FACTS, 1_000_000 + CACHE_TTL_5M_MS + 1, "claude-opus-5")).toBe("lapsed");
  });

  it("is a MODEL SWITCH when the models differ, however recent the request", () => {
    // The prompt cache is per model, so recency buys nothing across a switch.
    expect(judgeCold(FACTS, 1_000_000 + 1, "claude-sonnet-5")).toBe("model_switch");
  });

  it("is warm when no model was requested", () => {
    expect(judgeCold(FACTS, 1_000_000 + 1, undefined)).toBeUndefined();
  });

  it("is warm when the transcript states no request instant", () => {
    expect(judgeCold({ ...FACTS, lastRequestAtMs: 0 }, 9_999_999, "claude-opus-5")).toBeUndefined();
  });
});

/** The durable log records written since `before`, parsed. */
function logRecordsSince(before: number): Array<{ level: string; message: string; context: Record<string, unknown> }> {
  const calls = vi.mocked(writeSync).mock.calls.slice(before) as unknown as Array<
    [number, Buffer, number, number]
  >;
  return calls.map(
    ([, bytes, offset, length]) =>
      JSON.parse(bytes.subarray(offset, offset + length).toString("utf8")) as {
        level: string;
        message: string;
        context: Record<string, unknown>;
      },
  );
}

/** The one record the floor writes when it lets a continuation through. */
function floorRecord(before: number): { level: string; context: Record<string, unknown> } | undefined {
  return logRecordsSince(before).find((record) => record.message.includes("under the cold-gate floor"));
}

describe("the cold-gate floor", () => {
  // OWNER RULING, 2026-09-13: no cold gate when the context a cold read would
  // re-read is under 70,000 tokens; at or above 70,000 the gate asks as
  // before. The floor is stated once here so a change to the constant has to
  // be a deliberate change to this table too.
  it("stands at the 70,000 tokens the owner ruled", () => {
    expect(COLD_GATE_FLOOR_TOKENS).toBe(70_000);
  });

  const LAPSED_AT = 1_000_000 + CACHE_TTL_5M_MS + 1;

  const cases: ReadonlyArray<{
    readonly name: string;
    readonly facts: TranscriptFacts;
    readonly nowMs: number;
    readonly requestedModel: string | undefined;
    readonly expected: string | undefined;
  }> = [
    {
      name: "a lapse holding 69,999 tokens continues WARM, one token under the floor",
      facts: { ...FACTS, contextTokens: 69_999 },
      nowMs: LAPSED_AT,
      requestedModel: "claude-opus-5",
      expected: undefined,
    },
    {
      name: "a lapse holding exactly 70,000 tokens GATES: the floor is a floor, not a ceiling",
      facts: { ...FACTS, contextTokens: 70_000 },
      nowMs: LAPSED_AT,
      requestedModel: "claude-opus-5",
      expected: "lapsed",
    },
    {
      name: "a lapse holding 0 tokens (a fresh transcript) continues WARM",
      facts: { ...FACTS, contextTokens: 0 },
      nowMs: LAPSED_AT,
      requestedModel: "claude-opus-5",
      expected: undefined,
    },
    {
      name: "a MODEL CHANGE holding 50,000 tokens continues WARM: the same floor applies to a switch",
      facts: { ...FACTS, contextTokens: 50_000 },
      nowMs: 1_000_000 + 1,
      requestedModel: "claude-sonnet-5",
      expected: undefined,
    },
    {
      name: "a lapse holding 200,000 tokens GATES as before",
      facts: { ...FACTS, contextTokens: 200_000 },
      nowMs: LAPSED_AT,
      requestedModel: "claude-opus-5",
      expected: "lapsed",
    },
  ];

  for (const testCase of cases) {
    it(testCase.name, () => {
      expect(judgeCold(testCase.facts, testCase.nowMs, testCase.requestedModel)).toBe(
        testCase.expected,
      );
    });
  }

  it("records ONE info line naming the context, the lapse and the rule when it lets a lapse through", () => {
    // Arrange: a lapsed conversation one token under the floor.
    const before = vi.mocked(writeSync).mock.calls.length;

    // Act.
    judgeCold({ ...FACTS, contextTokens: 69_999 }, LAPSED_AT, "claude-opus-5");

    // Assert: the log can answer "why was I not asked".
    const record = floorRecord(before);
    expect([record?.level, record?.context.context_tokens, record?.context.floor_tokens, record?.context.lapse_ms, record?.context.reason]).toEqual([
      "info",
      69_999,
      70_000,
      CACHE_TTL_5M_MS + 1,
      "lapsed",
    ]);
  });

  it("records NOTHING when the conversation was warm anyway, so the floor is not credited for it", () => {
    // Arrange: inside the tier, and far under the floor.
    const before = vi.mocked(writeSync).mock.calls.length;

    // Act.
    judgeCold({ ...FACTS, contextTokens: 1_000 }, 1_000_000 + 1, "claude-opus-5");

    // Assert.
    expect(floorRecord(before)).toBeUndefined();
  });
});

describe("the refusal message", () => {
  it("carries the cost and the lapsed tier", () => {
    const cold = sessionCold(FACTS, "lapsed", "claude-opus-5");

    expect({
      contextTokens: cold.contextTokens,
      lastRequestAtMs: cold.lastRequestAtMs,
      reason: cold.reason.case,
      ttl: cold.reason.case === "lapsed" ? cold.reason.value.cacheTtlMs : undefined,
    }).toEqual({
      contextTokens: 100_000n,
      lastRequestAtMs: 1_000_000n,
      reason: "lapsed",
      ttl: BigInt(CACHE_TTL_5M_MS),
    });
  });

  it("names the requested model on a model switch", () => {
    const cold = sessionCold(FACTS, "model_switch", "claude-sonnet-5");

    expect([cold.reason.case, cold.requestedModel?.name]).toEqual(["modelSwitch", "claude-sonnet-5"]);
  });
});

describe("reading facts off a transcript that cannot be read", () => {
  it("RAISES a read failure that is not a missing file, rather than judging the cache from nothing", () => {
    // Arrange: a directory where a transcript file is expected.
    const dir = mkdtempSync(path.join(os.tmpdir(), "shim-cold-eisdir-"));

    // Act + Assert: EISDIR is a broken state dir, not an absent conversation.
    expect(() => readTranscriptFacts(dir)).toThrow(/EISDIR|illegal operation/);
  });

  it("skips an assistant record that states no usage, because it says nothing about the cache", () => {
    // Arrange: the usage-bearing record comes FIRST, the usageless one after.
    const file = transcript([
      assistant(),
      { type: "assistant", timestamp: "2026-08-29T13:00:00.000Z", message: { model: "other" } },
    ]);

    // Act.
    const facts = readTranscriptFacts(file);

    // Assert: the earlier record's numbers survive untouched.
    expect([facts?.contextTokens, facts?.lastModel]).toEqual([1102, "claude-opus-5"]);
  });

  it("leaves the model unstated when no assistant record named one", () => {
    const file = transcript([
      { type: "assistant", timestamp: "2026-08-29T12:00:00.000Z", message: { usage: { input_tokens: 7 } } },
    ]);

    expect(readTranscriptFacts(file)?.lastModel).toBeUndefined();
  });
});
