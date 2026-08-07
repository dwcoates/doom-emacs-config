import { beforeEach, describe, expect, it } from "vitest";
import { accountingSummary, latestTurnAccounting } from "../src/turn-accounting.js";
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
    expect(accountingSummary(accounting)).toContain("5h 10.0%→12.5% (2.5pp) · 2s · in 11 · out 22 · read 73 · write 9 · hit 25.0% · 1 subagent · 11.0 tok/s");
  });
  it("makes invalid accounting loud without requiring absent partial evidence", () => {
    expect(accountingSummary(invalidAccounting)).toContain("INVALID ACCOUNTING: tokenLedgerMismatch · runtime absent");
    expect(latestTurnAccounting([{ kind: "result", subtype: "success", durationMs: 0, numTurns: 0, totalCostUsd: 0, usage: { input_tokens: 0, output_tokens: 0 }, isError: false, context: null, turnAccounting: accounting }])).toBe(accounting);
  });
});

describe("the degraded-accounting log record", () => {
  it("warns once with the turn identity when an invalid verdict is drawn", () => {
    accountingSummary(invalidAccounting);

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
    accountingSummary({ ...accounting, reconciliation: undefined });

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
    accountingSummary(invalidAccounting);
    accountingSummary(invalidAccounting);
    accountingSummary(invalidAccounting);

    expect(records()).toHaveLength(1);
  });

  it("records again when the same turn's verdict changes", () => {
    accountingSummary({ ...accounting, reconciliation: undefined });
    accountingSummary(invalidAccounting);

    expect(records().map((record) => record.verdict)).toEqual(["incomplete", "invalid"]);
  });

  it("keeps a complete verdict off the warning channel", () => {
    accountingSummary(accounting);

    expect(records()).toEqual([]);
  });
});
