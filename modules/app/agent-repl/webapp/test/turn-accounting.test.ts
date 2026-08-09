import { beforeEach, describe, expect, it } from "vitest";
import { accountingFacts, accountingFactsLine, latestTurnAccounting } from "../src/turn-accounting.js";
import type { TurnAccounting } from "../src/frontend-proto.js";
import type { ClientLogCmd } from "../src/protocol.js";
import { ForwardingLogger, bindLogContext, setLogger } from "../src/wslog.js";

/** Every record the accounting projection forwarded toward the daemon. */
let forwarded: ClientLogCmd[] = [];

interface ForwardedRecord {
  level: string;
  operation: string;
  turn_id: string;
  query_instance_id: string;
  verdict: string;
  missing_evidence: string[];
  problems?: string[];
  agent_repl_session_id?: string;
}

function records(): ForwardedRecord[] {
  return forwarded.map((cmd) => {
    const record = cmd.context as unknown as {
      level: string;
      operation: string;
      agent_repl_session_id?: string;
      context: { turn_id: string; query_instance_id: string; verdict: string; missing_evidence: string[]; problems?: string[] };
    };
    return {
      level: record.level,
      operation: record.operation,
      agent_repl_session_id: record.agent_repl_session_id,
      ...record.context,
    };
  });
}

const invalidAccounting: TurnAccounting = {
  turnId: "turn", queryInstanceId: "query",
  verdict: { kind: "invalid", problems: [{ kind: "tokenLedgerMismatch", differingFieldPaths: ["usage.inputTokens"] }] },
};

const accounting: TurnAccounting = {
  turnId: "turn", queryInstanceId: "query",
  runtime: { vendorSessionId: "vendor", effectiveModel: "model", sdkVersion: "sdk", claudeCodeVersion: "cli", shimBuildSha: "sha", authSource: "oauth", subscriptionType: "pro", fastModeState: "off", fastModeReason: "", effectiveOptions: { kind: "sha256", value: "a" }, settings: { kind: "sha256", value: "b" }, tools: { kind: "sha256", value: "c" }, mcp: { kind: "sha256", value: "d" }, contextPrefix: { kind: "sha256", value: "e" } },
  timing: { promptAdmittedAtMs: 1, resultReceivedAtMs: 2001, accountingSettledAtMs: 2100, promptToResultMs: 2000, resultToSettlementMs: 99 },
  usageAtStart: { queryInstanceId: "query", turnId: "turn", boundaryAtMs: 1, observedAtMs: 2, sampleLatencyMs: 1, subscriptionType: "pro", boundary: "turnStart", outcome: { kind: "available", utilizationPercent: 10, resetsAtMs: 10_000 } },
  usageAtEnd: { queryInstanceId: "query", turnId: "turn", boundaryAtMs: 2001, observedAtMs: 2002, sampleLatencyMs: 1, subscriptionType: "pro", boundary: "turnEnd", outcome: { kind: "available", utilizationPercent: 12.5, resetsAtMs: 10_000 } },
  responses: [{ agentReplSessionId: "session", claudeSessionId: "claude", rootTurnId: "turn", apiRequestId: "request-main", apiMessageId: "main", model: "model", actor: "mainAgent", usage: { inputTokens: 10, outputTokens: 20, cacheReadInputTokens: 70, cacheCreationInputTokens: 5, cacheCreation: { ephemeral5mInputTokens: 3, ephemeral1hInputTokens: 2 }, serviceTier: "standard", speed: "", inferenceGeo: "", iterations: [], rawUsage: {} } }, { agentReplSessionId: "session", claudeSessionId: "claude", rootTurnId: "turn", apiRequestId: "request-sub", apiMessageId: "sub", model: "model", actor: "subagent", subagent: { agentId: "a", parentToolUseId: "tool-parent", parentAgentId: "agent-parent", subagentType: "research", taskDescription: "task" }, usage: { inputTokens: 1, outputTokens: 2, cacheReadInputTokens: 3, cacheCreationInputTokens: 4, cacheCreation: { ephemeral5mInputTokens: 4, ephemeral1hInputTokens: 0 }, serviceTier: "standard", speed: "", inferenceGeo: "", iterations: [], rawUsage: {} } }],
  reconciliation: { responseRecordCount: 2, responseAllAgents: { inputTokens: 11, outputTokens: 22, cacheReadInputTokens: 73, cacheCreationInputTokens: 9, cacheCreation5m: 7, cacheCreation1h: 2, cacheRates: { cacheHitRate: 0.25 } } as any, responseMainAgent: { inputTokens: 10, outputTokens: 20, cacheReadInputTokens: 70, cacheCreationInputTokens: 5, cacheCreation5m: 3, cacheCreation1h: 2 } as any, resultMainAgent: { inputTokens: 10, outputTokens: 20, cacheReadInputTokens: 70, cacheCreationInputTokens: 5, cacheCreation5m: 3, cacheCreation1h: 2 } as any, responseModels: [], resultModels: [], apiMessageIds: ["main", "sub"] },
  verdict: { kind: "complete" },
};

beforeEach(() => {
  forwarded = [];
  setLogger(new ForwardingLogger((cmd) => {
    forwarded.push(cmd);
    return true;
  }, () => {}));
  bindLogContext({ connection_id: "test-connection", agent_repl_session_id: "agent-session" });
});

describe("turn accounting projection", () => {
  it("renders the carried aggregate cache rate rather than recomputing response data", () => {
    expect(accountingFacts(accounting)).toEqual([
      { label: "5h quota", value: "10.0% → 12.5% (2.5pp)" },
      { label: "duration", value: "2s" },
      { label: "input", value: "11" },
      { label: "output", value: "22" },
      { label: "cache read", value: "73" },
      { label: "cache write", value: "9" },
      { label: "cache hit", value: "25.0%" },
      { label: "subagents", value: "1" },
      { label: "generation", value: "11.0 tok/s" },
    ]);
  });
  it("reports the quota unavailable when either boundary sample carries no outcome", () => {
    expect(accountingFacts({ ...accounting, usageAtEnd: { ...accounting.usageAtEnd!, outcome: { kind: "unavailable", reason: "no sample" } } })[0])
      .toEqual({ label: "5h quota", value: "unavailable" });
  });
  it("reports the generation rate unavailable when the turn measured no duration", () => {
    const facts = accountingFacts({ ...accounting, timing: { ...accounting.timing!, promptToResultMs: 0 } });
    expect(facts.find((fact) => fact.label === "generation")).toEqual({ label: "generation", value: "unavailable" });
  });
  it("reports the cache hit rate unavailable when the reconciliation carries none", () => {
    const reconciliation = { ...accounting.reconciliation!, responseAllAgents: { ...accounting.reconciliation!.responseAllAgents!, cacheRates: undefined } };
    const facts = accountingFacts({ ...accounting, reconciliation });
    expect(facts.find((fact) => fact.label === "cache hit")).toEqual({ label: "cache hit", value: "unavailable" });
  });
  it("makes invalid accounting loud without requiring absent partial evidence", () => {
    expect(accountingFacts(invalidAccounting)).toEqual([
      { label: "verdict", value: "INVALID ACCOUNTING" },
      { label: "problems", value: "tokenLedgerMismatch" },
      { label: "missing evidence", value: "runtime, timing, usage start, usage end, reconciliation" },
    ]);
    expect(latestTurnAccounting([{ kind: "result", subtype: "success", durationMs: 0, numTurns: 0, totalCostUsd: 0, usage: { input_tokens: 0, output_tokens: 0 }, isError: false, context: null, turnAccounting: accounting }])).toBe(accounting);
  });
  it("names only the evidence an incomplete turn is actually missing", () => {
    expect(accountingFacts({ ...accounting, reconciliation: undefined })).toEqual([
      { label: "verdict", value: "INCOMPLETE ACCOUNTING" },
      { label: "missing evidence", value: "reconciliation" },
    ]);
  });
  it("joins the facts into one line for a record that cannot carry a grid", () => {
    expect(accountingFactsLine([{ label: "input", value: "11" }, { label: "output", value: "22" }]))
      .toBe("input 11 · output 22");
  });
});

describe("the degraded-accounting log record", () => {
  it("warns once with the turn identity when an invalid verdict is drawn", () => {
    accountingFacts(invalidAccounting);

    expect(records()).toEqual([{
      level: "warn",
      operation: "turn-accounting.verdict-degraded",
      agent_repl_session_id: "agent-session",
      verdict: "invalid",
      turn_id: "turn",
      query_instance_id: "query",
      problems: ["tokenLedgerMismatch"],
      missing_evidence: ["runtime", "timing", "usage start", "usage end", "reconciliation"],
    }]);
  });

  it("names the absent evidence when an incomplete verdict is drawn", () => {
    accountingFacts({ ...accounting, reconciliation: undefined });

    expect(records()).toEqual([{
      level: "warn",
      operation: "turn-accounting.verdict-degraded",
      agent_repl_session_id: "agent-session",
      verdict: "incomplete",
      turn_id: "turn",
      query_instance_id: "query",
      missing_evidence: ["reconciliation"],
    }]);
  });

  it("does not repeat the record when the same verdict re-renders", () => {
    accountingFacts(invalidAccounting);
    accountingFacts(invalidAccounting);
    accountingFacts(invalidAccounting);

    expect(records()).toHaveLength(1);
  });

  it("records again when the same turn's verdict changes", () => {
    accountingFacts({ ...accounting, reconciliation: undefined });
    accountingFacts(invalidAccounting);

    expect(records().map((record) => record.verdict)).toEqual(["incomplete", "invalid"]);
  });

  it("keeps a complete verdict off the warning channel", () => {
    accountingFacts(accounting);

    expect(records()).toEqual([]);
  });
});
