/**
 * convert/terminals.ts's FAILURE_ARMS table, direct.
 *
 * `convertResult` is exercised end to end by the golden corpus and by
 * `fake/scenarios/failures.ts`, but the fake vendor's failure scenarios all
 * answer through the result SUBTYPE fallback (`raw.subtype`), never through
 * `terminal_reason` naming these particular arms directly — a REAL vendor can
 * set `terminal_reason` to any of them (that is what the table is FOR: "the
 * arm is the vendor's, not a classification"), so nothing here is dead, it is
 * just a shape no scenario happens to produce. This pins each arm the
 * coverage run found at zero hits by calling `convertResult` with a synthetic
 * `result` message naming it via `terminal_reason` directly.
 */
import { describe, expect, it } from "vitest";

import { convertResult } from "../../src/convert/terminals.js";
import type { SdkMessage } from "../../src/sdk/types.js";
import { foldContext } from "./fold-harness.js";

function resultMessage(terminalReason: string): Extract<SdkMessage, { type: "result" }> {
  return {
    type: "result",
    uuid: "result-uuid-1",
    terminal_reason: terminalReason,
  } as unknown as Extract<SdkMessage, { type: "result" }>;
}

function failureCaseOf(terminalReason: string): string | undefined {
  const output = convertResult(resultMessage(terminalReason), foldContext(), undefined);
  const frame = output.turnEnded?.frame;
  const result = frame?.result;
  if (result?.case !== "failure") return undefined;
  return result.value.failure.case;
}

describe.each([
  ["blocking_limit", "blockingLimit"],
  ["rapid_refill_breaker", "rapidRefillBreaker"],
  ["prompt_too_long", "promptTooLong"],
  ["image_error", "imageError"],
  ["model_error", "modelError"],
  ["malformed_tool_use_exhausted", "malformedToolUseExhausted"],
  ["hook_stopped", "hookStopped"],
  ["tool_deferred", "toolDeferred"],
  ["tool_deferred_unavailable", "toolDeferredUnavailable"],
  ["turn_setup_failed", "turnSetupFailed"],
])("terminal_reason %s", (reason, expectedCase) => {
  it(`converts to failure.${expectedCase}`, () => {
    expect(failureCaseOf(reason)).toBe(expectedCase);
  });
});
