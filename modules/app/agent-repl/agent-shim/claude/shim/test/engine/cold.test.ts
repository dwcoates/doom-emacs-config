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
import { describe, expect, it } from "vitest";
import {
  CACHE_TTL_1H_MS,
  CACHE_TTL_5M_MS,
  cwdSlug,
  judgeCold,
  readTranscriptFacts,
  sessionCold,
  transcriptPath,
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
});

const FACTS = {
  contextTokens: 1000,
  lastRequestAtMs: 1_000_000,
  cacheTtlMs: CACHE_TTL_5M_MS,
  lastModel: "claude-opus-5",
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

describe("the refusal message", () => {
  it("carries the cost and the lapsed tier", () => {
    const cold = sessionCold(FACTS, "lapsed", "claude-opus-5");

    expect({
      contextTokens: cold.contextTokens,
      lastRequestAtMs: cold.lastRequestAtMs,
      reason: cold.reason.case,
      ttl: cold.reason.case === "lapsed" ? cold.reason.value.cacheTtlMs : undefined,
    }).toEqual({
      contextTokens: 1000n,
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
