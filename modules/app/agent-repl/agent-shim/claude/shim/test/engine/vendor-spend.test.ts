/**
 * What the session's vendor calls spent (src/engine/vendor-spend.ts): the
 * cache-outcome rule and the context buckets against the shared vocabulary,
 * every origin's declared cause, the cold reasons, and the ledger's one count
 * per call.
 */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  CONTEXT_BUCKETS_TOKENS,
  MAIN_AGENT,
  SIDE_QUERY_AGENT,
  UNSTATED_SUBAGENT_TYPE,
  VendorSpendLedger,
  cacheOutcomeOf,
  causeOfOrigin,
  coldReasonOf,
  contextBucketOf,
  type ChainCall,
  type ReasonFacts,
} from "../../src/engine/vendor-spend.js";
import { CACHE_TTL_1H_MS, CACHE_TTL_5M_MS, type TranscriptFacts } from "../../src/engine/cold.js";
import { conversationv1 } from "../../src/proto.js";

const VOCAB = JSON.parse(readFileSync(new URL("../../../../../proto/vocab/vendor-spend.json", import.meta.url), "utf8")) as {
  context_buckets_tokens: number[];
  cache_outcomes: { cache_read: number; cache_write: number; uncached_input: number; outcome: string }[];
};

const AT = 1_700_000_000_000;
const MODEL = "claude-opus-5-5";
const C = conversationv1.VendorCallCause;
const O = conversationv1.VendorCacheOutcome;
const R = conversationv1.VendorColdReason;

/** An SDK assistant message carrying USAGE under message id ID. */
function assistant(id: string, usage: Record<string, unknown>, model = MODEL): { message: unknown } {
  return { message: { id, model, usage } };
}

/** A usage block: READ served, WRITE1H written at the hour tier, UNCACHED fresh, OUTPUT generated. */
function usage(read: number, write1h: number, uncached: number, output = 10): Record<string, unknown> {
  return {
    input_tokens: uncached,
    output_tokens: output,
    cache_read_input_tokens: read,
    cache_creation_input_tokens: write1h,
    cache_creation: { ephemeral_5m_input_tokens: 0, ephemeral_1h_input_tokens: write1h },
  };
}

/** The ledger's one entry, which a test that counted one kind of call reads. */
function only(ledger: VendorSpendLedger): conversationv1.VendorSpend {
  const entries = ledger.snapshot();
  expect(entries).toHaveLength(1);
  return entries[0]!;
}

describe("contextBucketOf", () => {
  it("holds the context buckets exactly as the vocabulary states them", () => {
    expect(CONTEXT_BUCKETS_TOKENS).toEqual(VOCAB.context_buckets_tokens);
  });

  it("files a context at a bound in that bound's bucket", () => {
    expect(contextBucketOf(300_000)).toBe(300_000);
  });

  it("files a context just above a bound in the next bucket", () => {
    expect(contextBucketOf(300_001)).toBe(400_000);
  });

  it("files a context above every bound in no bucket", () => {
    expect(contextBucketOf(1_000_001)).toBeUndefined();
  });
});

describe("cacheOutcomeOf", () => {
  it.each(VOCAB.cache_outcomes)("judges read $cache_read, write $cache_write, uncached $uncached_input as $outcome", (c) => {
    // Act
    const outcome = cacheOutcomeOf({ cacheRead: c.cache_read, cacheWrite: c.cache_write, uncached: c.uncached_input });
    // Assert
    expect(conversationv1.VendorCacheOutcome[outcome]).toBe(c.outcome);
  });
});

describe("causeOfOrigin", () => {
  const origins = conversationv1.PromptOriginSchema.values.filter((v) => v.number !== conversationv1.PromptOrigin.UNSPECIFIED);

  it.each(origins.map((v) => [v.name, v.number] as const))("finds a declared spend cause on %s", (_name, origin) => {
    // Act
    const cause = causeOfOrigin(origin as conversationv1.PromptOrigin);
    // Assert
    expect(cause).not.toBe(C.UNSPECIFIED);
    expect(cause).not.toBe(C.UNSTATED);
  });

  it("answers the unstated cause for the unspecified origin", () => {
    expect(causeOfOrigin(conversationv1.PromptOrigin.UNSPECIFIED)).toBe(C.UNSTATED);
  });

  it.each([
    [conversationv1.PromptOrigin.USER_SENT, C.USER_PROMPT],
    [conversationv1.PromptOrigin.MERGE_TEST_REPAIR, C.MERGE],
    [conversationv1.PromptOrigin.AGENT_SENT, C.WORKSPACE_NOTICE],
    [conversationv1.PromptOrigin.WORKSPACE_COMPACT, C.COMPACTION],
    [conversationv1.PromptOrigin.WORKSPACE_CREATED, C.WORKSPACE_TASK],
    [conversationv1.PromptOrigin.DEPLOY_HOOK, C.AUTOMATED],
    [conversationv1.PromptOrigin.VENDOR_STARTED, C.VENDOR_STARTED],
  ])("files origin %s under cause %s", (origin, cause) => {
    expect(causeOfOrigin(origin)).toBe(cause);
  });
});

describe("coldReasonOf", () => {
  const previous: ChainCall = { atMs: AT, model: MODEL, ttlMs: CACHE_TTL_1H_MS, cachedTokens: 100_000 };
  const call = (over: Partial<ReasonFacts> = {}): ReasonFacts => ({
    atMs: AT + 1000,
    model: MODEL,
    cacheRead: 100_000,
    afterCompaction: false,
    retriesVendorError: false,
    ...over,
  });

  it("names a first call when the agent made none before", () => {
    expect(coldReasonOf(undefined, call({ afterCompaction: true }))).toBe(R.FIRST_CALL);
  });

  it("names a compaction ahead of a model switch", () => {
    expect(coldReasonOf(previous, call({ afterCompaction: true, model: "claude-sonnet-5-5" }))).toBe(R.COMPACTION);
  });

  it("names a model switch ahead of a lapse", () => {
    expect(coldReasonOf(previous, call({ model: "claude-sonnet-5-5", atMs: AT + 2 * CACHE_TTL_1H_MS }))).toBe(R.MODEL_SWITCH);
  });

  it("names the hour tier's lapse past an hour", () => {
    expect(coldReasonOf(previous, call({ atMs: AT + CACHE_TTL_1H_MS + 1 }))).toBe(R.LAPSE_1H);
  });

  it("names the five-minute tier's lapse past five minutes", () => {
    expect(coldReasonOf({ ...previous, ttlMs: CACHE_TTL_5M_MS }, call({ atMs: AT + CACHE_TTL_5M_MS + 1 }))).toBe(R.LAPSE_5M);
  });

  it("names no lapse exactly at the tier", () => {
    expect(coldReasonOf(previous, call({ atMs: AT + CACHE_TTL_1H_MS }))).toBe(R.UNSTATED);
  });

  it("names a vendor error's retry ahead of a prefix change", () => {
    expect(coldReasonOf(previous, call({ retriesVendorError: true, cacheRead: 0 }))).toBe(R.VENDOR_ERROR_RETRY);
  });

  it("names a prefix change when the cache served less than the agent's last call left", () => {
    expect(coldReasonOf(previous, call({ cacheRead: 99_999 }))).toBe(R.PREFIX_CHANGE);
  });

  it("names nothing when none of the reasons holds", () => {
    expect(coldReasonOf(previous, call())).toBe(R.UNSTATED);
  });
});

describe("VendorSpendLedger", () => {
  it("counts a call once however many messages restate it, adding what its output grew by", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", usage(1000, 50, 3, 10)), MAIN_AGENT, C.USER_PROMPT, AT);
    ledger.countMessage(assistant("m1", usage(1000, 50, 3, 40)), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    const entry = only(ledger);
    expect(entry.calls).toBe(1n);
    expect(entry.tokens).toMatchObject({ cacheRead: 1000n, cacheWrite1h: 50n, cacheWrite5m: 0n, uncachedInput: 3n, output: 40n });
  });

  it("tags a call by its model, agent, cause, outcome and context bucket", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", usage(1000, 10, 3)), MAIN_AGENT, C.MERGE, AT);
    // Assert
    expect(only(ledger)).toMatchObject({
      model: MODEL,
      agent: conversationv1.VendorCallAgent.MAIN,
      subagentType: "",
      cause: C.MERGE,
      cacheOutcome: O.WARM,
      coldReason: R.UNSPECIFIED,
      contextLeTokens: 25_000n,
    });
  });

  it("files a write with no tier split at the five-minute tier", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", { input_tokens: 1, output_tokens: 1, cache_read_input_tokens: 0, cache_creation_input_tokens: 70 }), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    expect(only(ledger).tokens).toMatchObject({ cacheWrite5m: 70n, cacheWrite1h: 0n });
  });

  it("counts nothing for a message the CLI wrote itself", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", usage(0, 0, 0), "<synthetic>"), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    expect(ledger.snapshot()).toEqual([]);
    expect(ledger.version).toBe(0);
  });

  it("counts nothing for a message with no message id", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage({ message: { model: MODEL, usage: usage(1, 1, 1) } }, MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    expect(ledger.snapshot()).toEqual([]);
  });

  it("counts nothing for a usage that does not validate", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", { input_tokens: -1, output_tokens: 1 }), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    expect(ledger.snapshot()).toEqual([]);
  });

  it("records an unstated cause at ERROR and counts the call under it", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    const mark = logSinkMark();
    // Act
    ledger.countMessage(assistant("m1", usage(1000, 50, 3)), MAIN_AGENT, C.UNSTATED, AT);
    // Assert
    expect(only(ledger).cause).toBe(C.UNSTATED);
    const records = logRecordsSince(mark).filter((r) => r.level === "error");
    expect(records).toHaveLength(1);
    expect(records[0]!.context).toMatchObject({ message_id: "m1", model: MODEL, chain: "main", agent: "MAIN" });
  });

  it("records a call that names no model at ERROR", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    const mark = logSinkMark();
    // Act
    ledger.countMessage(assistant("m1", usage(1000, 50, 3), ""), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    const records = logRecordsSince(mark).filter((r) => r.level === "error");
    expect(records.map((r) => r.context)).toEqual([expect.objectContaining({ message_id: "m1", chain: "main" })]);
  });

  it("judges an agent's first cold call a first call, and its next a lapse past the tier its first wrote", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", usage(0, 90_000, 3)), MAIN_AGENT, C.USER_PROMPT, AT);
    ledger.countMessage(assistant("m2", usage(0, 90_000, 3)), MAIN_AGENT, C.USER_PROMPT, AT + CACHE_TTL_1H_MS + 1);
    // Assert
    const reasons = ledger.snapshot().map((e) => e.coldReason);
    expect(reasons).toEqual([R.FIRST_CALL, R.LAPSE_1H]);
  });

  it("judges the session's first call against the transcript it resumed", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    const facts = { sawUsage: true, lastRequestAtMs: AT, lastModel: MODEL, cacheTtlMs: CACHE_TTL_1H_MS, cachedTokens: 90_000 } as TranscriptFacts;
    ledger.seedMain(facts);
    // Act
    ledger.countMessage(assistant("m1", usage(0, 90_000, 3)), MAIN_AGENT, C.USER_PROMPT, AT + 2 * CACHE_TTL_1H_MS);
    // Assert
    expect(only(ledger).coldReason).toBe(R.LAPSE_1H);
  });

  it("judges the call after a compaction a compaction, once", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    ledger.countMessage(assistant("m1", usage(90_000, 10, 3)), MAIN_AGENT, C.USER_PROMPT, AT);
    ledger.noteCompaction();
    // Act
    ledger.countMessage(assistant("m2", usage(0, 30_000, 3)), MAIN_AGENT, C.USER_PROMPT, AT + 1000);
    ledger.countMessage(assistant("m3", usage(0, 30_000, 3)), MAIN_AGENT, C.USER_PROMPT, AT + 2000);
    // Assert
    expect(ledger.snapshot().map((e) => e.coldReason)).toEqual([R.UNSPECIFIED, R.COMPACTION, R.PREFIX_CHANGE]);
  });

  it("judges an agent along its own chain, so a subagent's first call does not see the main loop's", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    ledger.countMessage(assistant("m1", usage(90_000, 10, 3)), MAIN_AGENT, C.USER_PROMPT, AT);
    const sub = { chain: "toolu_1", agent: conversationv1.VendorCallAgent.SUBAGENT, subagentType: "Explore" };
    // Act
    ledger.countMessage(assistant("s1", usage(0, 20_000, 3)), sub, C.USER_PROMPT, AT + 1000);
    // Assert
    const subEntry = ledger.snapshot().find((e) => e.agent === conversationv1.VendorCallAgent.SUBAGENT);
    expect(subEntry).toMatchObject({ subagentType: "Explore", coldReason: R.FIRST_CALL, cacheOutcome: O.COLD_REWRITE });
  });

  it("states the calls of one kind as one entry and of another as another", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", usage(1000, 10, 3)), MAIN_AGENT, C.USER_PROMPT, AT);
    ledger.countMessage(assistant("m2", usage(1000, 10, 3)), MAIN_AGENT, C.USER_PROMPT, AT + 1);
    ledger.countMessage(assistant("m3", usage(1000, 10, 3)), MAIN_AGENT, C.KEEPALIVE, AT + 2);
    // Assert
    expect(ledger.snapshot().map((e) => [e.cause, e.calls])).toEqual([
      [C.USER_PROMPT, 2n],
      [C.KEEPALIVE, 1n],
    ]);
  });

  it("still knows its newest call after forgetting its oldest", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    for (let i = 0; i < 4097; i += 1) ledger.countMessage(assistant(`m${i}`, usage(1000, 10, 3)), MAIN_AGENT, C.USER_PROMPT, AT + i);
    // Act
    ledger.countMessage(assistant("m4096", usage(1000, 10, 3, 99)), MAIN_AGENT, C.USER_PROMPT, AT + 5000);
    // Assert: the last id is remembered, so its restatement only grew its output.
    expect(only(ledger).calls).toBe(4097n);
  });

  it("moves its version on a new call and on growth, and not on a restatement that grew nothing", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    ledger.countMessage(assistant("m1", usage(1000, 10, 3, 10)), MAIN_AGENT, C.USER_PROMPT, AT);
    const counted = ledger.version;
    // Act
    ledger.countMessage(assistant("m1", usage(1000, 10, 3, 10)), MAIN_AGENT, C.USER_PROMPT, AT);
    const restated = ledger.version;
    ledger.countMessage(assistant("m1", usage(1000, 10, 3, 11)), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    expect([counted, restated, ledger.version]).toEqual([1, 1, 2]);
  });

  it("states a context above every bound with no bucket", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.countMessage(assistant("m1", usage(1_000_000, 10, 3)), MAIN_AGENT, C.USER_PROMPT, AT);
    // Assert
    expect(only(ledger).contextLeTokens).toBeUndefined();
  });
});

describe("the ledger's turns", () => {
  it("answers the cause a turn was noted with", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.noteTurn("t1", C.MERGE);
    // Assert
    expect(ledger.causeOfTurn("t1")).toBe(C.MERGE);
  });

  it("keeps the cause a turn was first noted with", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    ledger.noteTurn("t1", C.MERGE);
    // Act
    ledger.noteTurn("t1", C.USER_PROMPT);
    // Assert
    expect(ledger.causeOfTurn("t1")).toBe(C.MERGE);
  });

  it("files the first real turn after a paid cold gate as the gate's answer, past a keep-alive", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    ledger.noteColdGatePaid();
    // Act
    ledger.noteTurn("k1", C.KEEPALIVE);
    ledger.noteTurn("t1", C.USER_PROMPT);
    ledger.noteTurn("t2", C.USER_PROMPT);
    // Assert
    expect([ledger.causeOfTurn("k1"), ledger.causeOfTurn("t1"), ledger.causeOfTurn("t2")]).toEqual([C.KEEPALIVE, C.COLD_GATE_ANSWER, C.USER_PROMPT]);
  });

  it("judges a vendor error retry's turn a retry, and no other turn", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.noteTurn("r1", C.VENDOR_ERROR_RETRY);
    ledger.noteTurn("t1", C.USER_PROMPT);
    // Assert
    expect([ledger.retries("r1"), ledger.retries("t1"), ledger.retries(undefined)]).toEqual([true, false, false]);
  });

  it("answers a seen spawn's type and cause, and an unseen spawn's as unstated", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    ledger.noteSpawn("toolu_1", "Explore", C.MERGE);
    ledger.noteSpawn("toolu_2", undefined, C.USER_PROMPT);
    // Assert
    expect(ledger.spawnOf("toolu_1")).toEqual({ subagentType: "Explore", cause: C.MERGE });
    expect(ledger.spawnOf("toolu_2")).toEqual({ subagentType: UNSTATED_SUBAGENT_TYPE, cause: C.USER_PROMPT });
    expect(ledger.spawnOf("toolu_3")).toBeUndefined();
  });

  it("gives every side query a chain of its own", () => {
    // Arrange
    const ledger = new VendorSpendLedger();
    // Act
    const first = SIDE_QUERY_AGENT(ledger);
    const second = SIDE_QUERY_AGENT(ledger);
    // Assert
    expect(first.chain).not.toBe(second.chain);
    expect(first.agent).toBe(conversationv1.VendorCallAgent.SIDE_QUERY);
  });
});
