/**
 * DID THIS CAPTURE ACTUALLY CAPTURE ANYTHING?
 *
 * WHY THIS MODULE EXISTS: the first real capture run wrote an AUTHENTICATION
 * FAILURE as a successful golden. The script exited 0, `meta.json` carried
 * `errors: []`, and `captures/prose-streamed/` looked like a finished fixture —
 * while the actual content was:
 *
 *     assistant.message.content[0].text = "Not logged in · Please run /login"
 *     assistant.message.model           = "<synthetic>"
 *     result.is_error                   = true
 *     result.terminal_reason            = "api_error"
 *     result.duration_ms                = 33
 *     result.total_cost_usd             = 0
 *
 * THE TRAP THAT MADE IT LOOK FINE: `result.subtype` was **"success"**. The
 * vendor spells an authentication failure as a success-subtype result carrying
 * `is_error: true`. Any check that keys on `subtype` — the obvious check —
 * passes on this exact transcript. So nothing here reads `subtype`; the arms
 * below are the ones that actually moved.
 *
 * A capture is a GOLDEN: the converter suites are graded against it and the
 * mocked vendor's scripts are rebuilt FROM it. A poisoned golden does not fail
 * loudly later — it teaches the whole system a wrong shape, quietly. That is
 * why detection here is aggressive and the response is refusal rather than a
 * warning.
 *
 * Pure functions only, so every arm is unit-testable against the real recorded
 * evidence without spawning anything.
 */

/** The scenario that is SUPPOSED to capture API errors; nothing else may. */
export const API_ERROR_SCENARIO = "api-error-classes";

/** Whether a scenario is allowed to carry an API-error result. */
export function expectsApiError(scenario) {
  return scenario?.expects_api_error === true || scenario?.name === API_ERROR_SCENARIO;
}

/**
 * Whether `key` is set to `true` anywhere inside a value.
 *
 * `is_api_error_message` rides nested inside the assistant message envelope
 * rather than at the top level, and the vendor is free to move it; a depth-
 * agnostic search costs nothing and cannot be defeated by re-nesting.
 */
export function hasTruthyKeyAtAnyDepth(value, key) {
  if (value === null || typeof value !== "object") return false;
  if (Array.isArray(value)) {
    return value.some((item) => hasTruthyKeyAtAnyDepth(item, key));
  }
  for (const [childKey, childValue] of Object.entries(value)) {
    if (childKey === key && childValue === true) return true;
    if (hasTruthyKeyAtAnyDepth(childValue, key)) return true;
  }
  return false;
}

/** The recorded SDK messages of one capture, in order. */
export function sdkMessages(entries) {
  return entries.filter((entry) => entry.dir === "sdk").map((entry) => entry.msg);
}

/** The `system`/`init` message, if the session ever initialized. */
export function initMessage(entries) {
  return (
    sdkMessages(entries).find(
      (msg) => msg?.type === "system" && msg?.subtype === "init",
    ) ?? null
  );
}

/**
 * The vendor's report of WHERE its credential came from.
 *
 * Recorded in `meta.json` for the operator rather than treated as a failure:
 * a subscription (OAuth) session legitimately reports `"none"` here, so this
 * field alone cannot tell an unauthenticated run from a logged-in one. The
 * result-message arms below can, and do.
 */
export function apiKeySource(entries) {
  return initMessage(entries)?.apiKeySource ?? null;
}

/**
 * Classify one finished capture.
 *
 * Returns `{ ok, reasons }`. `reasons` is a list of human sentences, each
 * naming the exact field that condemned the capture, so the operator can act
 * on the report without opening the stream.
 */
/**
 * Result subtypes a scenario DECLARES as its intended terminal.
 *
 * Several scenarios exist to capture an error-shaped terminal (max_turns,
 * budget exhaustion, an interrupt's aborted stream). The vendor marks those
 * `is_error: true`, so without this allowance the golden the scenario exists
 * for would be quarantined as a failure. The allowance is per declared
 * subtype and never covers `api_error`: an api_error is still a real failure
 * unless the scenario `expects_api_error`.
 */
export function expectedErrorSubtypes(scenario) {
  const declared = scenario?.expects_error_subtypes;
  if (!Array.isArray(declared)) return new Set();
  for (const subtype of declared) {
    if (typeof subtype !== "string" || subtype === "") {
      throw new Error(`${scenario?.name ?? "?"}: expects_error_subtypes must be non-empty strings`);
    }
  }
  return new Set(declared);
}

export function classifyCapture(entries, scenario, report = { errors: [] }) {
  const reasons = [];
  const messages = sdkMessages(entries);
  const allowApiError = expectsApiError(scenario);
  const errorSubtypes = expectedErrorSubtypes(scenario);

  if (messages.length === 0) {
    reasons.push("the SDK produced no messages at all");
  }

  const results = messages.filter((msg) => msg?.type === "result");
  if (results.length === 0) {
    reasons.push("the turn never reached a result message (no terminal)");
  }

  for (const result of results) {
    // NOT `subtype`: the poisoned capture's subtype was "success".
    if (result.is_error === true && !allowApiError && !errorSubtypes.has(result.subtype)) {
      reasons.push("result.is_error is true");
    }
    if (result.terminal_reason === "api_error" && !allowApiError) {
      reasons.push('result.terminal_reason is "api_error"');
    }
    if (
      result.api_error_status !== null &&
      result.api_error_status !== undefined &&
      !allowApiError
    ) {
      reasons.push(
        `result.api_error_status is ${JSON.stringify(result.api_error_status)}`,
      );
    }
  }

  if (!allowApiError && messages.some((msg) => hasTruthyKeyAtAnyDepth(msg, "is_api_error_message"))) {
    reasons.push("a message carries is_api_error_message: true");
  }

  for (const error of report.errors ?? []) {
    reasons.push(`the run threw during ${error.stage}: ${error.error}`);
  }

  return { ok: reasons.length === 0, reasons };
}

/**
 * The one-line verdict printed per scenario during a run.
 *
 * Kept here so the run log and `meta.json` cannot drift apart in wording.
 */
export function verdictLine(name, outcome) {
  return outcome.ok
    ? `capture: ${name} OK`
    : `capture: ${name} FAILED — ${outcome.reasons.join("; ")}`;
}
