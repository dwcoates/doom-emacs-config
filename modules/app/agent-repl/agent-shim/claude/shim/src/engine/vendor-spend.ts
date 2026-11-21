/**
 * engine/vendor-spend.ts — what this session's vendor calls spent, counted
 * from the vendor's own usage, for the daemon's always-on spend metrics
 * (conversation.v1 vendor_spend.proto, shim.v1.ShimMeasurements
 * `vendor_spend`, skills/agent-repl-metrics/CATALOG.md `agentrepl_vendor_*`).
 *
 * ONE CALL IS ONE MESSAGE ID. The SDK streams one API response as several
 * assistant messages, each restating the response's usage, the output count
 * growing as it streams. A call is tagged once, when its id is first seen (its
 * input counters are whole from the first message), and every later message
 * of it adds only what its counters grew by, so no response is counted twice
 * and none is missed. A message the CLI wrote itself (`<synthetic>`) is no
 * vendor call and is not counted.
 *
 * THE TAGS ARE STATED, NEVER GUESSED. A call's cause is the cause of the turn
 * it belongs to, as the turn was opened (its PromptOrigin's spend cause, the
 * shim's own keep-alive or prompt); a subagent's calls carry the cause of the
 * turn that spawned it. A call no stated turn explains is UNSTATED and
 * recorded at ERROR, never folded into another cause.
 */
import { create, getOption } from "@bufbuild/protobuf";
import type { NormalizedApiUsage } from "../api-usage.js";
import { normalizeApiUsage } from "../api-usage.js";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import { transcriptCacheAnchor } from "./keepalive.js";
import { CACHE_READ_PRICE, CACHE_TTL_1H_MS, CACHE_TTL_5M_MS, CACHE_WRITE_PRICE, isCliSynthesized, type TranscriptFacts } from "./cold.js";

const LOGGER = bindLog("shim.engine.vendor-spend", {
  component: "shim-vendor-spend",
  operation: "shim.engine.vendor_spend",
});

/**
 * The context buckets' inclusive upper bounds, in tokens, ascending.
 * proto/vocab/vendor-spend.json holds the same list; the daemon refuses a
 * frame naming any other bound, and test/engine/vendor-spend.test.ts compares
 * the two.
 */
export const CONTEXT_BUCKETS_TOKENS: readonly number[] = [
  25_000, 50_000, 100_000, 200_000, 300_000, 400_000, 600_000, 800_000, 1_000_000,
];

/** The bucket a context of `tokens` falls in: its upper bound, or absence above every bound. */
export function contextBucketOf(tokens: number): number | undefined {
  return CONTEXT_BUCKETS_TOKENS.find((bound) => tokens <= bound);
}

/** A call's input, by what the cache did with it. */
export interface CallInput {
  readonly cacheRead: number;
  /** Both tiers' writes. */
  readonly cacheWrite: number;
  readonly uncached: number;
}


/** THE ONE CACHE-OUTCOME RULE of conversation.v1.VendorCacheOutcome, cased in proto/vocab/vendor-spend.json. */
export function cacheOutcomeOf(input: CallInput): conversationv1.VendorCacheOutcome {
  const context = input.cacheRead + input.cacheWrite + input.uncached;
  const fresh = input.cacheWrite + input.uncached;
  if (fresh * CACHE_WRITE_PRICE <= context * CACHE_READ_PRICE) return conversationv1.VendorCacheOutcome.WARM;
  if (input.cacheWrite === 0) return conversationv1.VendorCacheOutcome.UNCACHED_NO_WRITE;
  if (fresh > input.cacheRead) return conversationv1.VendorCacheOutcome.COLD_REWRITE;
  return conversationv1.VendorCacheOutcome.PARTIAL;
}

/**
 * The spend cause a turn of `origin` is spent under: the origin's own
 * declaration (conversation.v1.spend_cause). An origin that declares none is
 * UNSPECIFIED, which no turn is ever opened with; it answers UNSTATED, and
 * the caller records it.
 */
export function causeOfOrigin(origin: conversationv1.PromptOrigin): conversationv1.VendorCallCause {
  const value = conversationv1.PromptOriginSchema.value[origin];
  const cause = value === undefined ? conversationv1.VendorCallCause.UNSPECIFIED : getOption(value, conversationv1.spend_cause);
  return cause === conversationv1.VendorCallCause.UNSPECIFIED ? conversationv1.VendorCallCause.UNSTATED : cause;
}

/** What an agent's previous call left, which its next call's cold reason is judged against. */
export interface ChainCall {
  readonly atMs: number;
  readonly model: string;
  /** The tier its cache stands at: the last tier this agent wrote. */
  readonly ttlMs: number;
  /** What it left cached: its read plus its write. */
  readonly cachedTokens: number;
}

/** What a call states that its cold reason is judged from. */
export interface ReasonFacts {
  readonly atMs: number;
  readonly model: string;
  readonly cacheRead: number;
  /** The call follows a compaction of its agent's conversation. */
  readonly afterCompaction: boolean;
  /** The call's turn retries a turn a vendor error ended. */
  readonly retriesVendorError: boolean;
}

/** Why the cache did not serve a call that was not WARM (conversation.v1.VendorColdReason's order). */
export function coldReasonOf(previous: ChainCall | undefined, call: ReasonFacts): conversationv1.VendorColdReason {
  if (previous === undefined) return conversationv1.VendorColdReason.FIRST_CALL;
  if (call.afterCompaction) return conversationv1.VendorColdReason.COMPACTION;
  if (previous.model !== call.model) return conversationv1.VendorColdReason.MODEL_SWITCH;
  if (call.atMs - previous.atMs > previous.ttlMs) {
    return previous.ttlMs >= CACHE_TTL_1H_MS ? conversationv1.VendorColdReason.LAPSE_1H : conversationv1.VendorColdReason.LAPSE_5M;
  }
  if (call.retriesVendorError) return conversationv1.VendorColdReason.VENDOR_ERROR_RETRY;
  if (call.cacheRead < previous.cachedTokens) return conversationv1.VendorColdReason.PREFIX_CHANGE;
  return conversationv1.VendorColdReason.UNSTATED;
}

/** One call's counters, as its usage states them. */
interface Counters {
  uncached: number;
  cacheRead: number;
  write5m: number;
  write1h: number;
  output: number;
}

function countersOf(usage: NormalizedApiUsage): Counters {
  // A USAGE WITH NO TIER SPLIT IS THE 5-MINUTE TIER, the API's only tier
  // before the split existed (engine/cold.ts writtenCacheTtlMs).
  const write1h = usage.cacheCreation1hInputTokens;
  return {
    uncached: usage.inputTokens,
    cacheRead: usage.cacheReadInputTokens,
    write5m: Math.max(0, usage.cacheCreationInputTokens - write1h),
    write1h,
    output: usage.outputTokens,
  };
}

const COUNTER_KEYS = ["uncached", "cacheRead", "write5m", "write1h", "output"] as const;

/** What tags one kind of call. */
export interface CallTags {
  readonly model: string;
  readonly agent: conversationv1.VendorCallAgent;
  readonly subagentType: string;
  readonly cause: conversationv1.VendorCallCause;
}

/** One kind of call's cumulative spend. */
interface Entry {
  readonly tags: CallTags;
  readonly outcome: conversationv1.VendorCacheOutcome;
  readonly reason: conversationv1.VendorColdReason;
  readonly contextLe: number | undefined;
  calls: number;
  readonly tokens: Counters;
}

/** A counted call: the entry it was filed under and the counters counted so far. */
interface Counted {
  readonly entry: Entry;
  readonly counters: Counters;
}

/**
 * HOW MANY CALLS ARE REMEMBERED BY ID, so a later message of the same
 * response adds only its growth. An id repeats only while its response
 * streams, and each agent streams one response at a time, so the remembered
 * ids must cover the responses streaming at once: 4,096 covers any number of
 * agents the vendor runs side by side many times over.
 */
const REMEMBERED_CALLS = 4096;

/** One agent of the session, as the ledger knows it. */
export interface Agent {
  /** The chain its cold reasons are judged along: `main`, a spawn's call id, a side query's own key. */
  readonly chain: string;
  readonly agent: conversationv1.VendorCallAgent;
  readonly subagentType: string;
}

/** The session's main loop. */
export const MAIN_AGENT: Agent = { chain: "main", agent: conversationv1.VendorCallAgent.MAIN, subagentType: "" };

/** A side query's agent, on a chain of its own. */
export function SIDE_QUERY_AGENT(ledger: VendorSpendLedger): Agent {
  return { chain: ledger.nextSideChain(), agent: conversationv1.VendorCallAgent.SIDE_QUERY, subagentType: "" };
}

/** What a side query's calls are counted under. */
export interface SideQuerySpend {
  readonly ledger: VendorSpendLedger;
  readonly cause: conversationv1.VendorCallCause;
  readonly nowMs: () => number;
  /** The transcript the query resumes, which its first call is judged against. */
  readonly seed?: TranscriptFacts;
}

/** The spend type a subagent whose spawn this process never saw is filed under. */
export const UNSTATED_SUBAGENT_TYPE = "unstated";

/**
 * The session's vendor spend: every call counted once, by what it was, since
 * the counting run began.
 */
export class VendorSpendLedger {
  private readonly entries = new Map<string, Entry>();
  private readonly counted = new Map<string, Counted>();
  private readonly chains = new Map<string, ChainCall>();
  private readonly compacted = new Set<string>();
  private readonly turnCauses = new Map<string, conversationv1.VendorCallCause>();
  private readonly retryTurns = new Set<string>();
  private readonly spawns = new Map<string, { readonly subagentType: string; readonly cause: conversationv1.VendorCallCause }>();
  private coldGatePaid = false;
  private sideQueries = 0;
  private changes = 0;

  /** How many times the spend changed: a watch owes a frame when this moved. */
  get version(): number {
    return this.changes;
  }

  /**
   * Seed the main loop's previous call from the transcript the session
   * resumed: its last request, so the session's first call is judged against
   * it rather than as a first call.
   */
  seedMain(facts: TranscriptFacts | undefined): void {
    this.seedChain(MAIN_AGENT.chain, facts);
  }

  /** Seed CHAIN's previous call from the transcript it resumes, as {@link seedMain} does the main loop's. */
  seedChain(chain: string, facts: TranscriptFacts | undefined): void {
    if (facts === undefined || !facts.sawUsage || facts.lastRequestAtMs <= 0) return;
    const anchor = transcriptCacheAnchor(facts);
    this.chains.set(chain, {
      atMs: anchor.atMs,
      model: facts.lastModel ?? "",
      ttlMs: anchor.ttlMs !== undefined && anchor.ttlMs > 0 ? anchor.ttlMs : CACHE_TTL_5M_MS,
      cachedTokens: anchor.cachedTokens,
    });
  }

  /** A fresh chain for a side query: none of its calls is judged against another query's. */
  nextSideChain(): string {
    this.sideQueries += 1;
    return `side-${this.sideQueries}`;
  }

  /** The main loop's conversation was compacted: its next call follows a compaction. */
  noteCompaction(): void {
    this.compacted.add(MAIN_AGENT.chain);
  }

  /** The session was brought up by a cold gate's pay answer: its next real turn pays the read. */
  noteColdGatePaid(): void {
    this.coldGatePaid = true;
  }

  /**
   * TURN opens with spend cause CAUSE: every call answering its send counts
   * under it. The first real turn after a paid cold gate counts as the
   * gate's answer. A VENDOR_ERROR_RETRY turn's calls are judged retries.
   */
  noteTurn(turnId: string, cause: conversationv1.VendorCallCause): void {
    // A TURN IS NOTED ONCE: a join that later takes the slot as its own turn
    // is the same turn, and keeps the cause it was first noted with.
    if (this.turnCauses.has(turnId)) return;
    let stated = cause;
    if (this.coldGatePaid && cause !== conversationv1.VendorCallCause.KEEPALIVE) {
      this.coldGatePaid = false;
      stated = conversationv1.VendorCallCause.COLD_GATE_ANSWER;
    }
    this.turnCauses.set(turnId, stated);
    if (cause === conversationv1.VendorCallCause.VENDOR_ERROR_RETRY) this.retryTurns.add(turnId);
  }

  /** The cause TURN was opened with, or absence for a turn no one stated. */
  causeOfTurn(turnId: string): conversationv1.VendorCallCause | undefined {
    return this.turnCauses.get(turnId);
  }

  /** Whether TURN retries a vendor error. */
  retries(turnId: string | undefined): boolean {
    return turnId !== undefined && this.retryTurns.has(turnId);
  }

  /**
   * A subagent spawn was seen: the call TOOLUSEID spawns a subagent of
   * SUBAGENTTYPE, inside a turn of CAUSE, which its calls are spent under.
   */
  noteSpawn(toolUseId: string, subagentType: string | undefined, cause: conversationv1.VendorCallCause): void {
    this.spawns.set(toolUseId, { subagentType: subagentType ?? UNSTATED_SUBAGENT_TYPE, cause });
  }

  /** The subagent the call TOOLUSEID spawned, with the cause its calls are spent under, when its spawn was seen. */
  spawnOf(toolUseId: string): { readonly subagentType: string; readonly cause: conversationv1.VendorCallCause } | undefined {
    return this.spawns.get(toolUseId);
  }

  /**
   * Count one assistant message of AGENT under CAUSE, at ATMS. A message
   * with no message id, no usage or a usage that does not validate is no
   * countable call (the converter records an invalid usage), and the CLI's
   * own messages are no vendor calls.
   */
  countMessage(
    message: { readonly message?: unknown; readonly isApiErrorMessage?: unknown },
    agent: Agent,
    cause: conversationv1.VendorCallCause,
    atMs: number,
    retriesVendorError = false,
  ): void {
    const body = message.message as { id?: unknown; model?: unknown; usage?: unknown } | undefined;
    if (body === undefined || typeof body.id !== "string" || body.id === "" || body.usage === undefined) return;
    const model = typeof body.model === "string" ? body.model : "";
    if (isCliSynthesized({ isApiErrorMessage: message.isApiErrorMessage === true, message: { model } })) return;
    let usage: NormalizedApiUsage;
    try {
      usage = normalizeApiUsage(body.usage);
    } catch {
      // Recorded at ERROR where the converter reads the same usage
      // (convert/stream-events.ts tokenUsage).
      return;
    }
    this.count(body.id, model, agent, cause, countersOf(usage), atMs, retriesVendorError);
  }

  /**
   * Count the usage USAGE of a call MESSAGEID on MODEL by AGENT under CAUSE.
   * The first sight of an id tags it; a later one adds what it grew by.
   */
  count(
    messageId: string,
    model: string,
    agent: Agent,
    cause: conversationv1.VendorCallCause,
    counters: Counters,
    atMs: number,
    retriesVendorError = false,
  ): void {
    if (model === "") {
      LOGGER.error({ message_id: messageId, chain: agent.chain, cause: "the vendor stated no model" }, "a vendor call names no model; it is counted under an empty model");
    }
    const seen = this.counted.get(messageId);
    if (seen !== undefined) {
      if (this.grow(seen, counters)) this.changes += 1;
      // Refreshed, so the response streaming now is the last to be forgotten.
      this.counted.delete(messageId);
      this.counted.set(messageId, seen);
      return;
    }
    if (cause === conversationv1.VendorCallCause.UNSTATED) {
      LOGGER.error(
        { message_id: messageId, model, chain: agent.chain, agent: conversationv1.VendorCallAgent[agent.agent], cause: "no turn or spawn stated a spend cause" },
        "a vendor call belongs to no turn whose cause was stated; it is counted under the unstated cause",
      );
    }
    const input = { cacheRead: counters.cacheRead, cacheWrite: counters.write5m + counters.write1h, uncached: counters.uncached };
    const outcome = cacheOutcomeOf(input);
    const previous = this.chains.get(agent.chain);
    const afterCompaction = this.compacted.delete(agent.chain);
    const reason =
      outcome === conversationv1.VendorCacheOutcome.WARM
        ? conversationv1.VendorColdReason.UNSPECIFIED
        : coldReasonOf(previous, { atMs, model, cacheRead: counters.cacheRead, afterCompaction, retriesVendorError });
    const context = input.cacheRead + input.cacheWrite + input.uncached;
    const ttlMs = counters.write1h > 0 ? CACHE_TTL_1H_MS : counters.write5m > 0 ? CACHE_TTL_5M_MS : (previous?.ttlMs ?? CACHE_TTL_5M_MS);
    this.chains.set(agent.chain, { atMs, model, ttlMs, cachedTokens: input.cacheRead + input.cacheWrite });
    const tags: CallTags = { model, agent: agent.agent, subagentType: agent.subagentType, cause };
    const entry = this.entryFor(tags, outcome, reason, contextBucketOf(context));
    entry.calls += 1;
    const filed: Counted = { entry, counters: { uncached: 0, cacheRead: 0, write5m: 0, write1h: 0, output: 0 } };
    this.grow(filed, counters);
    this.changes += 1;
    this.counted.set(messageId, filed);
    if (this.counted.size > REMEMBERED_CALLS) {
      const oldest = this.counted.keys().next().value;
      if (oldest !== undefined) this.counted.delete(oldest);
    }
    if (outcome !== conversationv1.VendorCacheOutcome.WARM) {
      LOGGER.debug(
        {
          message_id: messageId,
          model,
          chain: agent.chain,
          cause: conversationv1.VendorCallCause[cause],
          outcome: conversationv1.VendorCacheOutcome[outcome],
          reason: conversationv1.VendorColdReason[reason],
          context_tokens: context,
          cache_read: input.cacheRead,
          cache_write: input.cacheWrite,
          uncached: input.uncached,
        },
        "a vendor call the cache did not serve",
      );
    }
  }

  /** Add what a call's counters grew by since they were last counted; whether any grew. */
  private grow(seen: Counted, counters: Counters): boolean {
    let grew = false;
    for (const key of COUNTER_KEYS) {
      const by = counters[key] - seen.counters[key];
      if (by <= 0) continue;
      seen.entry.tokens[key] += by;
      seen.counters[key] = counters[key];
      grew = true;
    }
    return grew;
  }

  private entryFor(
    tags: CallTags,
    outcome: conversationv1.VendorCacheOutcome,
    reason: conversationv1.VendorColdReason,
    contextLe: number | undefined,
  ): Entry {
    const key = JSON.stringify([tags.model, tags.agent, tags.subagentType, tags.cause, outcome, reason, contextLe ?? null]);
    let entry = this.entries.get(key);
    if (entry === undefined) {
      entry = { tags, outcome, reason, contextLe, calls: 0, tokens: { uncached: 0, cacheRead: 0, write5m: 0, write1h: 0, output: 0 } };
      this.entries.set(key, entry);
    }
    return entry;
  }

  /** Every kind of call counted so far, as the measurements arm states it. */
  snapshot(): conversationv1.VendorSpend[] {
    return [...this.entries.values()].map((entry) =>
      create(conversationv1.VendorSpendSchema, {
        model: entry.tags.model,
        agent: entry.tags.agent,
        subagentType: entry.tags.subagentType,
        cause: entry.tags.cause,
        cacheOutcome: entry.outcome,
        coldReason: entry.reason,
        ...(entry.contextLe === undefined ? {} : { contextLeTokens: BigInt(entry.contextLe) }),
        calls: BigInt(entry.calls),
        tokens: create(conversationv1.VendorTokensSchema, {
          uncachedInput: BigInt(entry.tokens.uncached),
          cacheRead: BigInt(entry.tokens.cacheRead),
          cacheWrite5m: BigInt(entry.tokens.write5m),
          cacheWrite1h: BigInt(entry.tokens.write1h),
          output: BigInt(entry.tokens.output),
        }),
      }),
    );
  }
}
