/** Compact projections of durable terminal-turn accounting for chrome surfaces. */
import type { InvalidTurnAccounting, TurnAccounting } from "./frontend-proto.js";
import type { ConversationItem } from "./store.js";
import { formatTokens } from "./tokens.js";

export function latestTurnAccounting(items: readonly ConversationItem[]): TurnAccounting | null {
  for (let index = items.length - 1; index >= 0; index -= 1) {
    const item = items[index];
    if (item.kind === "result" && item.turnAccounting !== undefined) return item.turnAccounting;
  }
  return null;
}

export function accountingSummary(accounting: TurnAccounting): string {
  if (isInvalidAccounting(accounting)) {
    return `INVALID ACCOUNTING: ${accounting.verdict.problems.map((problem) => problem.kind).join(", ")}${invalidEvidenceSummary(accounting)}`;
  }
  if (accounting.reconciliation === undefined || accounting.reconciliation.responseAllAgents === undefined || accounting.timing === undefined || accounting.usageAtStart?.outcome === undefined || accounting.usageAtEnd?.outcome === undefined) {
    return `INCOMPLETE ACCOUNTING${completeEvidenceSummary(accounting)}`;
  }
  const total = accounting.reconciliation.responseAllAgents;
  const duration = accounting.timing.promptToResultMs;
  const tps = duration > 0 ? (1000 * total.outputTokens) / duration : null;
  const quota = accounting.usageAtStart.outcome.kind === "available" && accounting.usageAtEnd.outcome.kind === "available"
    ? `${accounting.usageAtStart.outcome.utilizationPercent.toFixed(1)}%→${accounting.usageAtEnd.outcome.utilizationPercent.toFixed(1)}% (${(accounting.usageAtEnd.outcome.utilizationPercent - accounting.usageAtStart.outcome.utilizationPercent).toFixed(1)}pp)`
    : "unavailable";
  const subagents = accounting.responses.filter((response) => response.actor === "subagent").length;
  return `5h ${quota} · ${Math.round(duration / 1000)}s · in ${formatTokens(total.inputTokens)} · out ${formatTokens(total.outputTokens)} · read ${formatTokens(total.cacheReadInputTokens)} · write ${formatTokens(total.cacheCreationInputTokens)} · ${total.cacheRates === undefined ? "hit unavailable" : rate(total.cacheRates.cacheHitRate)} · ${subagents} subagent${subagents === 1 ? "" : "s"} · ${tps === null ? "generation unavailable" : `${tps.toFixed(1)} tok/s`}`;
}

function isInvalidAccounting(accounting: TurnAccounting): accounting is InvalidTurnAccounting {
  return accounting.verdict.kind === "invalid";
}

function rate(cacheHitRate: number): string {
  return `hit ${(100 * cacheHitRate).toFixed(1)}%`;
}

function invalidEvidenceSummary(accounting: InvalidTurnAccounting): string {
  const evidence = [
    accounting.runtime === undefined ? "runtime absent" : undefined,
    accounting.timing === undefined ? "timing absent" : undefined,
    accounting.usageAtStart === undefined ? "usage start absent" : undefined,
    accounting.usageAtEnd === undefined ? "usage end absent" : undefined,
    accounting.reconciliation === undefined ? "reconciliation absent" : undefined,
  ].filter((entry): entry is string => entry !== undefined);
  return evidence.length === 0 ? "" : ` · ${evidence.join(", ")}`;
}

function completeEvidenceSummary(accounting: TurnAccounting): string {
  const evidence = [
    accounting.runtime === undefined ? "runtime absent" : undefined,
    accounting.timing === undefined ? "timing absent" : undefined,
    accounting.usageAtStart?.outcome === undefined ? "usage start absent" : undefined,
    accounting.usageAtEnd?.outcome === undefined ? "usage end absent" : undefined,
    accounting.reconciliation === undefined ? "reconciliation absent" : undefined,
  ].filter((entry): entry is string => entry !== undefined);
  return evidence.length === 0 ? "" : ` · ${evidence.join(", ")}`;
}
