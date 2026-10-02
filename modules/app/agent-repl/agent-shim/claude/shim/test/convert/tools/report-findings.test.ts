/**
 * A code review's typed defect report. Every vocabulary the tool uses is open at
 * one end (category) and closed at the other (verdict, outcome, level), so each
 * closed arm gets its own assertion and each unrecognized value is asserted to
 * leave the arm unset rather than to guess.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { reportFindingsConverter } from "../../../src/convert/tools/report-findings.js";
import { toolResultText } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";

const FINDING = {
  file: "src/convert/fold.ts",
  line: 42,
  summary: "the fold drops a tool result whose call was forgotten",
  short_summary: "dropped tool result",
  failure_scenario: "513 calls in flight, then the 1st returns → no terminal frame",
  category: "correctness",
};

function call(input: Record<string, unknown> = {}): PendingCall {
  return {
    toolUseId: "toolu_findings",
    toolName: "ReportFindings",
    input,
    startedAtMs: 5_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "reviewer" }),
  };
}

function outcome(structured: unknown, isError = false): ToolOutcome {
  return {
    content: isError ? toolResultText("the review crashed") : undefined,
    isError,
    structured,
    settledAtMs: 11_000,
  };
}

function armOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentReportFindings["state"] {
  expect(item?.case).toBe("reportFindings");
  return (item?.value as conversationv1.AgentReportFindings).state;
}

function successOf(
  item: conversationv1.AgentActivity["item"] | undefined,
): conversationv1.AgentReportFindingsSuccess {
  const arm = armOf(item);
  expect(arm.case).toBe("success");
  return arm.value as conversationv1.AgentReportFindingsSuccess;
}

function oneFinding(entry: Record<string, unknown>): conversationv1.AgentFinding {
  const success = successOf(
    reportFindingsConverter.settle(call(), outcome({ count: 1, findings: [entry] })),
  );
  return success.findings[0];
}

describe("reportFindingsConverter.start", () => {
  it("stamps the instant the call was issued", () => {
    // Arrange, Act.
    const arm = armOf(reportFindingsConverter.start(call()));

    // Assert.
    expect((arm.value as conversationv1.AgentReportFindingsStart).startedAt?.atMs).toBe(5_000n);
  });
});

describe("reportFindingsConverter.settle", () => {
  it("carries the findings the tool echoed, in the tool's own order", () => {
    // Arrange.
    const structured = { count: 2, findings: [FINDING, { ...FINDING, summary: "second" }] };

    // Act.
    const success = successOf(reportFindingsConverter.settle(call(), outcome(structured)));

    // Assert.
    expect(success.findings.map((finding) => finding.summary)).toEqual([FINDING.summary, "second"]);
  });

  it("carries an empty report, which is a review that found nothing", () => {
    // Arrange, Act.
    const success = successOf(
      reportFindingsConverter.settle(call(), outcome({ count: 0, findings: [] })),
    );

    // Assert.
    expect(success.findings).toEqual([]);
  });

  it("reads the findings back off the call when the tool echoed nothing", () => {
    // Arrange, Act.
    const success = successOf(
      reportFindingsConverter.settle(call({ findings: [FINDING] }), outcome({ count: 1 })),
    );

    // Assert.
    expect(success.findings.map((finding) => finding.file)).toEqual([FINDING.file]);
  });

  it("produces NO frame when neither the result nor the call listed any findings", () => {
    // Arrange, Act, Assert.
    expect(reportFindingsConverter.settle(call(), outcome({ count: 0 }))).toBeUndefined();
  });

  it("stamps the settle instant", () => {
    // Arrange, Act.
    const success = successOf(reportFindingsConverter.settle(call(), outcome({ findings: [] })));

    // Assert.
    expect(success.settledAt?.atMs).toBe(11_000n);
  });

  it("takes the effort level from the result", () => {
    // Arrange, Act.
    const success = successOf(
      reportFindingsConverter.settle(call(), outcome({ findings: [], level: "xhigh" })),
    );

    // Assert.
    expect(success.level).toBe(conversationv1.AgentEffortLevel.XHIGH);
  });

  it("falls back to the call's own level when the result stated none", () => {
    // Arrange, Act.
    const success = successOf(
      reportFindingsConverter.settle(call({ level: "max" }), outcome({ findings: [] })),
    );

    // Assert.
    expect(success.level).toBe(conversationv1.AgentEffortLevel.MAX);
  });

  it("drops an entry that is not a finding at all rather than half-building one", () => {
    // Arrange, Act.
    const success = successOf(
      reportFindingsConverter.settle(call(), outcome({ findings: [FINDING, "oops"] })),
    );

    // Assert.
    expect(success.findings).toHaveLength(1);
  });

  it("settles a vendor-marked error as the failure arm", () => {
    // Arrange, Act.
    const arm = armOf(reportFindingsConverter.settle(call(), outcome(undefined, true)));

    // Assert.
    expect(arm.case).toBe("failure");
  });

  it("carries what the call said when it failed", () => {
    // Arrange, Act.
    const arm = armOf(reportFindingsConverter.settle(call(), outcome(undefined, true)));

    // Assert.
    const failure = arm.value as conversationv1.AgentReportFindingsFailure;
    expect(failure.error?.content).toEqual(toolResultText("the review crashed"));
  });

  it("declares no progress arm, because AgentReportFindings has none", () => {
    // Arrange, Act, Assert.
    expect(reportFindingsConverter.carriesProgress).toBe(false);
  });
});

describe("one finding", () => {
  it("anchors to the file the defect is in", () => {
    // Arrange, Act, Assert.
    expect(oneFinding(FINDING).file).toBe("src/convert/fold.ts");
  });

  it("anchors to the 1-indexed line the tool gave", () => {
    // Arrange, Act, Assert.
    expect(oneFinding(FINDING).line).toBe(42);
  });

  it("leaves the line UNSET when the tool anchored to none", () => {
    // Arrange.
    const entry: Record<string, unknown> = { ...FINDING };
    delete entry["line"];

    // Act, Assert.
    expect(oneFinding(entry).line).toBeUndefined();
  });

  it("carries the compressed label for narrow surfaces", () => {
    // Arrange, Act, Assert.
    expect(oneFinding(FINDING).shortSummary).toBe("dropped tool result");
  });

  it("leaves the compressed label UNSET when the tool gave none", () => {
    // Arrange.
    const entry: Record<string, unknown> = { ...FINDING };
    delete entry["short_summary"];

    // Act, Assert.
    expect(oneFinding(entry).shortSummary).toBeUndefined();
  });

  it("carries the concrete failure scenario", () => {
    // Arrange, Act, Assert.
    expect(oneFinding(FINDING).failureScenario).toBe(FINDING.failure_scenario);
  });

  it("carries the finding type's slug verbatim, since the vocabulary is open", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, category: "test-coverage" }).category).toBe("test-coverage");
  });

  it("leaves the category UNSET when the tool gave none", () => {
    // Arrange.
    const entry: Record<string, unknown> = { ...FINDING };
    delete entry["category"];

    // Act, Assert.
    expect(oneFinding(entry).category).toBeUndefined();
  });

  it("maps a CONFIRMED verdict onto the confirmed arm", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, verdict: "CONFIRMED" }).verdict.case).toBe("confirmed");
  });

  it("maps a PLAUSIBLE verdict onto the plausible arm", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, verdict: "PLAUSIBLE" }).verdict.case).toBe("plausible");
  });

  it("leaves the verdict UNSET when no verify pass ran", () => {
    // Arrange, Act, Assert.
    expect(oneFinding(FINDING).verdict.case).toBeUndefined();
  });

  it("maps a fixed outcome onto the fixed arm", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, outcome: "fixed" }).outcome.case).toBe("fixed");
  });

  it("maps a skipped outcome onto the skipped arm", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, outcome: "skipped" }).outcome.case).toBe("skipped");
  });

  it("maps a no_change_needed outcome onto the noChangeNeeded arm", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, outcome: "no_change_needed" }).outcome.case).toBe("noChangeNeeded");
  });

  it("leaves the outcome UNSET on an ordinary report", () => {
    // Arrange, Act, Assert.
    expect(oneFinding(FINDING).outcome.case).toBeUndefined();
  });

  it("leaves the verdict UNSET for a verdict this contract has no arm for", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, verdict: "MAYBE" }).verdict.case).toBeUndefined();
  });

  it("leaves the outcome UNSET for an outcome this contract has no arm for", () => {
    // Arrange, Act, Assert.
    expect(oneFinding({ ...FINDING, outcome: "deferred" }).outcome.case).toBeUndefined();
  });

  it("carries an EMPTY file when the tool anchored the finding to none", () => {
    // Arrange.
    const { file: _file, ...rest } = FINDING;

    // Act, Assert.
    expect(oneFinding(rest).file).toBe("");
  });

  it("carries an EMPTY summary when the tool stated none", () => {
    // Arrange.
    const { summary: _summary, ...rest } = FINDING;

    // Act, Assert.
    expect(oneFinding(rest).summary).toBe("");
  });

  it("carries an EMPTY failure scenario when the tool stated none", () => {
    // Arrange.
    const { failure_scenario: _scenario, ...rest } = FINDING;

    // Act, Assert.
    expect(oneFinding(rest).failureScenario).toBe("");
  });
});
