/**
 * convert/tools/report-findings.ts — a code review's typed defect report.
 *
 * The tool's own Output ECHOES the findings it was given, so the terminal reads
 * the result rather than the call's input: what the review actually reported is
 * what the tool answered with. The input is the fallback for a vendor that
 * echoed nothing, since the report is otherwise lost.
 *
 * ORDER IS THE TOOL'S: most-severe first by its own contract, and nothing here
 * re-sorts. An empty list is a real report — a review that found nothing — and
 * is carried as one rather than suppressed.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { arr, asRecord, failureOf, str, uint } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-report-findings",
  operation: "shim.convert.report_findings",
});

/** One report-findings item, whatever arm it carries. */
function findingsItem(
  state: conversationv1.AgentReportFindings["state"],
): conversationv1.AgentActivity["item"] {
  return {
    case: "reportFindings",
    value: create(conversationv1.AgentReportFindingsSchema, { state }),
  };
}

/** The effort level the review ran at, in the one canonical vocabulary. */
export function effortLevelOf(level: string | undefined): conversationv1.AgentEffortLevel {
  switch (level) {
    case "low":
      return conversationv1.AgentEffortLevel.LOW;
    case "medium":
      return conversationv1.AgentEffortLevel.MEDIUM;
    case "high":
      return conversationv1.AgentEffortLevel.HIGH;
    case "xhigh":
      return conversationv1.AgentEffortLevel.XHIGH;
    case "max":
      return conversationv1.AgentEffortLevel.MAX;
    case undefined:
      return conversationv1.AgentEffortLevel.UNSPECIFIED;
    default:
      LOGGER.debug(
        { effort: level },
        "a review named an effort level this vocabulary has no value for; the level is left unspecified",
      );
      return conversationv1.AgentEffortLevel.UNSPECIFIED;
  }
}

/** The verify pass's verdict. UNSET when no verify pass ran. */
function verdictOf(verdict: string | undefined): conversationv1.AgentFinding["verdict"] {
  switch (verdict) {
    case "CONFIRMED":
      return { case: "confirmed", value: create(conversationv1.AgentFindingConfirmedSchema, {}) };
    case "PLAUSIBLE":
      return { case: "plausible", value: create(conversationv1.AgentFindingPlausibleSchema, {}) };
    case undefined:
      return { case: undefined };
    default:
      LOGGER.debug(
        { verdict },
        "a finding carries a verdict this contract has no arm for; the verdict is left unset",
      );
      return { case: undefined };
  }
}

/** What happened to a finding, set only on a RE-REPORT after fixes were applied. */
function outcomeOf(outcome: string | undefined): conversationv1.AgentFinding["outcome"] {
  switch (outcome) {
    case "fixed":
      return { case: "fixed", value: create(conversationv1.AgentFindingFixedSchema, {}) };
    case "skipped":
      return { case: "skipped", value: create(conversationv1.AgentFindingSkippedSchema, {}) };
    case "no_change_needed":
      return {
        case: "noChangeNeeded",
        value: create(conversationv1.AgentFindingNoChangeNeededSchema, {}),
      };
    case undefined:
      return { case: undefined };
    default:
      LOGGER.debug(
        { outcome },
        "a finding carries an outcome this contract has no arm for; the outcome is left unset",
      );
      return { case: undefined };
  }
}

/** One finding, or `undefined` when the entry is not one at all. */
function findingOf(entry: unknown): conversationv1.AgentFinding | undefined {
  const record = asRecord(entry);
  if (record === undefined) {
    LOGGER.debug(
      {},
      "a review reported a finding that is not an object; it is dropped rather than half-built",
    );
    return undefined;
  }
  return create(conversationv1.AgentFindingSchema, {
    file: str(record, "file") ?? "",
    line: uint(record, "line"),
    summary: str(record, "summary") ?? "",
    shortSummary: str(record, "short_summary"),
    failureScenario: str(record, "failure_scenario") ?? "",
    category: str(record, "category"),
    verdict: verdictOf(str(record, "verdict")),
    outcome: outcomeOf(str(record, "outcome")),
  });
}

/** The `ReportFindings` tool: a review stating what it found, typed. */
export const reportFindingsConverter: ToolConverter = {
  kind: "report_findings",
  // `AgentReportFindings` declares no progress arm.
  carriesProgress: false,

  start(call) {
    return findingsItem({
      case: "start",
      value: create(conversationv1.AgentReportFindingsStartSchema, {
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a findings report failed");
      return findingsItem({
        case: "failure",
        value: create(conversationv1.AgentReportFindingsFailureSchema, { error: failureOf(call, outcome) }),
      });
    }
    const structured = asRecord(outcome.structured);
    const echoed = arr(structured, "findings");
    const entries = echoed ?? arr(call.input, "findings");
    if (entries === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a findings report listed no findings on either the result or the call; no frame is produced",
      );
      return undefined;
    }
    if (echoed === undefined) {
      LOGGER.debug(
        { tool_use_id: call.toolUseId },
        "a findings report echoed nothing; the findings are read back off the call",
      );
    }
    const findings = entries
      .map(findingOf)
      .filter((finding): finding is conversationv1.AgentFinding => finding !== undefined);
    return findingsItem({
      case: "success",
      value: create(conversationv1.AgentReportFindingsSuccessSchema, {
        findings,
        level: effortLevelOf(str(structured, "level") ?? str(call.input, "level")),
        settledAt: settledAt(outcome.settledAtMs, call.startedAtMs),
      }),
    });
  },
};
