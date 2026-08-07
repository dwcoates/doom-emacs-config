/** Compact projections of durable terminal-turn accounting for chrome surfaces. */
import type { InvalidTurnAccounting, TurnAccounting } from "./frontend-proto.js";
import type { ConversationItem } from "./store.js";
import { formatTokens } from "./tokens.js";
import { log } from "./wslog.js";

export function latestTurnAccounting(items: readonly ConversationItem[]): TurnAccounting | null {
  for (let index = items.length - 1; index >= 0; index -= 1) {
    const item = items[index];
    if (item.kind === "result" && item.turnAccounting !== undefined) return item.turnAccounting;
  }
  return null;
}

export function accountingSummary(accounting: TurnAccounting): string {
  if (isInvalidAccounting(accounting)) {
    const summary = `INVALID ACCOUNTING: ${accounting.verdict.problems.map((problem) => problem.kind).join(", ")}${invalidEvidenceSummary(accounting)}`;
    reportDegradedAccounting(accounting, "invalid", summary, {
      problems: accounting.verdict.problems.map((problem) => problem.kind),
      missing_evidence: missingEvidence(accounting, invalidEvidencePresence),
    });
    return summary;
  }
  if (accounting.reconciliation === undefined || accounting.reconciliation.responseAllAgents === undefined || accounting.timing === undefined || accounting.usageAtStart?.outcome === undefined || accounting.usageAtEnd?.outcome === undefined) {
    const summary = `INCOMPLETE ACCOUNTING${completeEvidenceSummary(accounting)}`;
    reportDegradedAccounting(accounting, "incomplete", summary, {
      missing_evidence: missingEvidence(accounting, completeEvidencePresence),
    });
    return summary;
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

/**
 * Which evidence fragments an INVALID verdict retained. An invalid turn is
 * already condemned by its problems, so presence is judged on the fragment
 * itself rather than on the readable interior a complete turn needs.
 */
function invalidEvidencePresence(accounting: TurnAccounting): readonly [string, boolean][] {
  return [
    ["runtime", accounting.runtime !== undefined],
    ["timing", accounting.timing !== undefined],
    ["usage start", accounting.usageAtStart !== undefined],
    ["usage end", accounting.usageAtEnd !== undefined],
    ["reconciliation", accounting.reconciliation !== undefined],
  ];
}

/** Which evidence fragments a turn needs before the full summary can be drawn. */
function completeEvidencePresence(accounting: TurnAccounting): readonly [string, boolean][] {
  return [
    ["runtime", accounting.runtime !== undefined],
    ["timing", accounting.timing !== undefined],
    ["usage start", accounting.usageAtStart?.outcome !== undefined],
    ["usage end", accounting.usageAtEnd?.outcome !== undefined],
    ["reconciliation", accounting.reconciliation !== undefined],
  ];
}

type EvidencePresence = (accounting: TurnAccounting) => readonly [string, boolean][];

function missingEvidence(accounting: TurnAccounting, presence: EvidencePresence): string[] {
  return presence(accounting).filter(([, present]) => !present).map(([name]) => name);
}

function evidenceSummary(accounting: TurnAccounting, presence: EvidencePresence): string {
  const evidence = missingEvidence(accounting, presence).map((name) => `${name} absent`);
  return evidence.length === 0 ? "" : ` · ${evidence.join(", ")}`;
}

function invalidEvidenceSummary(accounting: InvalidTurnAccounting): string {
  return evidenceSummary(accounting, invalidEvidencePresence);
}

function completeEvidenceSummary(accounting: TurnAccounting): string {
  return evidenceSummary(accounting, completeEvidencePresence);
}

/**
 * A degraded accounting verdict is DRAWN — the user reads "INVALID ACCOUNTING"
 * or "INCOMPLETE ACCOUNTING" in the footer and the topbar — so it must also
 * leave a record a warning sweep can find. Both chrome surfaces re-render the
 * latest turn on every frame, so the record is deduped per turn on `turnId`:
 * wslog suppresses a repeat of the SAME message under the key, and a verdict
 * that CHANGES for that turn (evidence landing late, incomplete hardening into
 * invalid) changes the message and logs again.
 */
function reportDegradedAccounting(
  accounting: TurnAccounting,
  verdict: "invalid" | "incomplete",
  summary: string,
  evidence: Record<string, unknown>,
): void {
  log("warn", `turn accounting verdict is ${verdict}: ${summary}`, {
    operation: "turn-accounting.verdict-degraded",
    dedupKey: `turn-accounting:${accounting.turnId}`,
    context: {
      verdict,
      turn_id: accounting.turnId,
      query_instance_id: accounting.queryInstanceId,
      ...evidence,
    },
  });
}
