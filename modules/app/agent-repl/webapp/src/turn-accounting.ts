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

/**
 * One labelled figure out of a turn's accounting.
 *
 * The pairs are the SINGLE owner of what a turn's accounting says. Every
 * surface that reports the accounting reads these — the footer's token peek
 * draws them as a grid, and the degraded-verdict warning joins them into its
 * log line — so a figure can never read one way on screen and another in the
 * log.
 */
export interface AccountingFact {
  /** What the figure is, in the reader's words. */
  label: string;
  /** The figure itself, already formatted. */
  value: string;
}

/**
 * A turn's accounting as labelled figures, and the one place a degraded verdict
 * is recorded.
 *
 * The three verdicts return DIFFERENT pairs rather than a complete shape with
 * holes in it: an invalid turn's problems and an incomplete turn's absent
 * evidence are the only facts those turns have, and padding them out with
 * zeroed token figures would report costs nothing measured.
 */
export function accountingFacts(accounting: TurnAccounting): readonly AccountingFact[] {
  if (isInvalidAccounting(accounting)) {
    const facts: AccountingFact[] = [
      { label: "verdict", value: "INVALID ACCOUNTING" },
      { label: "problems", value: accounting.verdict.problems.map((problem) => problem.kind).join(", ") },
    ];
    const absent = missingEvidence(accounting, invalidEvidencePresence);
    if (absent.length > 0) facts.push({ label: "missing evidence", value: absent.join(", ") });
    reportDegradedAccounting(accounting, "invalid", accountingFactsLine(facts), {
      problems: accounting.verdict.problems.map((problem) => problem.kind),
      missing_evidence: absent,
    });
    return facts;
  }
  if (accounting.reconciliation === undefined || accounting.reconciliation.responseAllAgents === undefined || accounting.timing === undefined || accounting.usageAtStart?.outcome === undefined || accounting.usageAtEnd?.outcome === undefined) {
    const facts: AccountingFact[] = [{ label: "verdict", value: "INCOMPLETE ACCOUNTING" }];
    const absent = missingEvidence(accounting, completeEvidencePresence);
    if (absent.length > 0) facts.push({ label: "missing evidence", value: absent.join(", ") });
    reportDegradedAccounting(accounting, "incomplete", accountingFactsLine(facts), {
      missing_evidence: absent,
    });
    return facts;
  }
  const total = accounting.reconciliation.responseAllAgents;
  const duration = accounting.timing.promptToResultMs;
  const tps = duration > 0 ? (1000 * total.outputTokens) / duration : null;
  const quota = accounting.usageAtStart.outcome.kind === "available" && accounting.usageAtEnd.outcome.kind === "available"
    ? `${accounting.usageAtStart.outcome.utilizationPercent.toFixed(1)}% → ${accounting.usageAtEnd.outcome.utilizationPercent.toFixed(1)}% (${(accounting.usageAtEnd.outcome.utilizationPercent - accounting.usageAtStart.outcome.utilizationPercent).toFixed(1)}pp)`
    : "unavailable";
  const subagents = accounting.responses.filter((response) => response.actor === "subagent").length;
  return [
    { label: "5h quota", value: quota },
    { label: "duration", value: `${Math.round(duration / 1000)}s` },
    { label: "input", value: formatTokens(total.inputTokens) },
    { label: "output", value: formatTokens(total.outputTokens) },
    { label: "cache read", value: formatTokens(total.cacheReadInputTokens) },
    { label: "cache write", value: formatTokens(total.cacheCreationInputTokens) },
    { label: "cache hit", value: total.cacheRates === undefined ? "unavailable" : rate(total.cacheRates.cacheHitRate) },
    { label: "subagents", value: String(subagents) },
    { label: "generation", value: tps === null ? "unavailable" : `${tps.toFixed(1)} tok/s` },
  ];
}

/** The facts as ONE line, for a log record that cannot carry a grid. */
export function accountingFactsLine(facts: readonly AccountingFact[]): string {
  return facts.map((fact) => `${fact.label} ${fact.value}`).join(" · ");
}

function isInvalidAccounting(accounting: TurnAccounting): accounting is InvalidTurnAccounting {
  return accounting.verdict.kind === "invalid";
}

function rate(cacheHitRate: number): string {
  return `${(100 * cacheHitRate).toFixed(1)}%`;
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

/**
 * A degraded accounting verdict is DRAWN — the user reads "INVALID ACCOUNTING"
 * or "INCOMPLETE ACCOUNTING" in the footer's token peek — so it must also
 * leave a record a warning sweep can find. The chrome re-renders the
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
