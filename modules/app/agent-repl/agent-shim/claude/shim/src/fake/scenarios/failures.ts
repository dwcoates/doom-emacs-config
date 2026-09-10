/**
 * fake/scenarios/failures.ts — every way a turn can stop badly.
 *
 * # The sixteen failure arms do NOT live in `result.subtype`
 *
 * `sdk.d.ts` declares exactly four error subtypes — `error_during_execution`,
 * `error_max_turns`, `error_max_budget_usd`,
 * `error_max_structured_output_retries`. Every finer stop is a
 * `TerminalReason`: `blocking_limit`, `rapid_refill_breaker`,
 * `prompt_too_long`, `image_error`, `model_error`, `api_error`,
 * `malformed_tool_use_exhausted`, `aborted_streaming`, `aborted_tools`,
 * `stop_hook_prevented`, `hook_stopped`, `tool_deferred`, `max_turns`,
 * `background_requested`, `completed`, `budget_exhausted`,
 * `structured_output_retry_exhausted`, `tool_deferred_unavailable`,
 * `turn_setup_failed`.
 *
 * So each scenario below pairs the RIGHT subtype with the RIGHT terminal
 * reason. A mock that only varied the subtype could reach four of the sixteen
 * conversation.v1 arms; a mock that only varied the reason would produce
 * results whose `is_error` contradicted their reason.
 *
 * ONE ARM HAS NO PRODUCER. `AgentContinuationPrevented` has no `TerminalReason`
 * of its own; the closest declared signals are
 * `SDKInformationalMessage.prevent_continuation` and the transcript's
 * `stop_hook_summary.preventedContinuation`, and `!fail-continuation-prevented`
 * emits BOTH beside a `stop_hook_prevented` terminal. That substitution is
 * named in the report as unsettled.
 *
 * # API errors are evidence twice over
 *
 * Mid-turn they are `system:api_error` transcript records and `api_retry`
 * messages (`AgentUpdate.api_error`); at the turn's end they are an
 * `error_during_execution` result with `terminal_reason: "api_error"` and an
 * `api_error_status` (`AgentFailure.api_request_failed`). Each `!api-*`
 * scenario produces the whole arc, because a converter that saw only one half
 * would classify the other wrongly.
 *
 * # The two `!fault-*` turns are about the SHIM, not the turn
 *
 * `!fault-converter` and `!fault-recover` are the only scenarios here whose
 * turns SUCCEED. What goes wrong in the first is not the conversation — it is
 * one vendor message the converter cannot convert, which the contract answers
 * with no frame, a logged converter defect and a residue record rather than a
 * dead session. They live in this family because the fact under test is a
 * failure; they end well because a defective vendor message does not stop a
 * turn.
 */
import { conclude, scenario, withheldThinking } from "./support.js";
import type { Scenario, ScenarioContext } from "../scenario.js";

/**
 * A turn that stops with a given subtype and terminal reason, having produced
 * NO assistant content.
 *
 * The absence is deliberate: a turn that failed produced no assistant message,
 * and fabricating an empty one would put a blank bubble on every frontend.
 */
function stopScenario(spec: {
  name: string;
  subtype:
    | "error_during_execution"
    | "error_max_turns"
    | "error_max_budget_usd"
    | "error_max_structured_output_retries";
  /**
   * The `TerminalReason` riding the result, or UNSET when the subtype alone is
   * the whole of the vendor's account.
   */
  terminalReason?: string;
  error: string;
  arms: string;
  before?: (ctx: ScenarioContext) => void;
}): Scenario {
  return scenario({
    name: spec.name,
    prompt: `!${spec.name}`,
    emits:
      `a reasoning block and a partial answer, then an error \`result\` with subtype \`${spec.subtype}\`` +
      (spec.terminalReason === undefined
        ? " and NO `terminal_reason` — the subtype alone is the vendor's whole account"
        : ` and \`terminal_reason: "${spec.terminalReason}"\``),
    writes: "the assistant lines for the work it did reach, the prompt line and the turn record",
    arms: `AgentThinking + AgentResponse, then ${spec.arms}`,
    run(ctx) {
      ctx.log.debug(
        { turn: ctx.turn, branch: spec.name, terminal_reason: spec.terminalReason },
        "fake failing turn",
      );
      // A TURN DOES NOT STOP BEFORE IT HAS DONE ANYTHING. Every turn-stop
      // capture carries work ahead of its terminal — `turn-stop-max-budget-usd`
      // is thinking + a response, `turn-stop-max-turns` is thinking + a bash
      // call — and the mock used to emit the bare result, which is a shape no
      // capture shows and which left the stop arms asserted over an empty turn.
      // `stopReason: null` because the API response did NOT end the turn.
      ctx.assistant([withheldThinking(), { type: "text", text: "Working on it…" }], {
        stopReason: null,
      });
      spec.before?.(ctx);
      ctx.result({
        subtype: spec.subtype,
        ...(spec.terminalReason === undefined ? {} : { terminalReason: spec.terminalReason }),
        errors: [spec.error],
      });
    },
  });
}

const FAIL_EXECUTION = stopScenario({
  name: "fail-execution",
  subtype: "error_during_execution",
  // NO terminal_reason: `execution_error` is the UNCLASSIFIED arm, so a row
  // that named one would reach whatever that reason spells instead —
  // `api_error` here reached `api_request_failed`, which grounded the wrong arm
  // and left `execution_error` untested.
  error: "the turn raised during execution",
  arms:
    "AgentFailure.execution_error — DECLARED-ONLY: no capture grounds this terminal (`turn-stop-error-during-execution` ended `success.interrupted` after an `aborted_streaming`), so the mock keeps the declared arm and the evidence gap is listed in testdata/captures/MANIFEST.md",
});

const FAIL_MAX_TURNS = stopScenario({
  name: "fail-max-turns",
  subtype: "error_max_turns",
  terminalReason: "max_turns",
  error: "the turn hit its max-turns ceiling",
  arms: "AgentFailure.max_turns",
});

const FAIL_BUDGET = stopScenario({
  name: "fail-budget",
  subtype: "error_max_budget_usd",
  terminalReason: "budget_exhausted",
  error: "the turn exhausted its usd budget",
  arms: "AgentFailure.budget_exhausted",
});

const FAIL_STRUCTURED_OUTPUT = stopScenario({
  name: "fail-structured-output",
  subtype: "error_max_structured_output_retries",
  terminalReason: "structured_output_retry_exhausted",
  error: "the structured-output retries were exhausted",
  arms:
    "AgentFailure.structured_output_retry_exhausted — DECLARED-ONLY: no capture grounds this terminal (`turn-stop-max-structured-output-retries` ended `success.completed`), so the mock keeps the declared arm and the evidence gap is listed in testdata/captures/MANIFEST.md",
});

const FAIL_BLOCKING_LIMIT = stopScenario({
  name: "fail-blocking-limit",
  subtype: "error_during_execution",
  terminalReason: "blocking_limit",
  error: "the account's blocking limit was reached",
  arms: "AgentFailure.blocking_limit",
});

const FAIL_RAPID_REFILL = stopScenario({
  name: "fail-rapid-refill",
  subtype: "error_during_execution",
  terminalReason: "rapid_refill_breaker",
  error: "the rapid-refill breaker tripped",
  arms: "AgentFailure.rapid_refill_breaker",
});

const FAIL_PROMPT_TOO_LONG = stopScenario({
  name: "fail-prompt-too-long",
  subtype: "error_during_execution",
  terminalReason: "prompt_too_long",
  error: "the prompt exceeded the context window",
  arms: "AgentFailure.prompt_too_long",
});

const FAIL_IMAGE = stopScenario({
  name: "fail-image",
  subtype: "error_during_execution",
  terminalReason: "image_error",
  error: "an attached image was rejected",
  arms: "AgentFailure.image_error",
});

const FAIL_MODEL = stopScenario({
  name: "fail-model",
  subtype: "error_during_execution",
  terminalReason: "model_error",
  error: "the requested model is unavailable",
  arms: "AgentFailure.model_error",
});

const FAIL_MALFORMED_TOOL_USE = stopScenario({
  name: "fail-malformed-tool-use",
  subtype: "error_during_execution",
  terminalReason: "malformed_tool_use_exhausted",
  error: "the model produced malformed tool input on every retry",
  arms: "AgentFailure.malformed_tool_use_exhausted",
});

const FAIL_TOOL_DEFERRED = stopScenario({
  name: "fail-tool-deferred",
  subtype: "error_during_execution",
  terminalReason: "tool_deferred",
  error: "the turn deferred a tool call to the host",
  arms: "AgentFailure.tool_deferred",
});

const FAIL_TOOL_DEFERRED_UNAVAILABLE = stopScenario({
  name: "fail-tool-deferred-unavailable",
  subtype: "error_during_execution",
  terminalReason: "tool_deferred_unavailable",
  error: "the deferred tool is not available to this host",
  arms: "AgentFailure.tool_deferred_unavailable",
});

const FAIL_TURN_SETUP = stopScenario({
  name: "fail-turn-setup",
  subtype: "error_during_execution",
  terminalReason: "turn_setup_failed",
  error: "the turn could not be set up",
  arms: "AgentFailure.turn_setup_failed",
});

const FAIL_ABORTED_TOOLS = stopScenario({
  name: "fail-aborted-tools",
  subtype: "error_during_execution",
  terminalReason: "aborted_tools",
  error: "the turn was aborted while its tools were running",
  arms: "AgentInterrupted.by_user, reached through the tools rather than the stream",
});

const FAIL_STOP_HOOK = stopScenario({
  name: "fail-stop-hook",
  subtype: "error_during_execution",
  terminalReason: "stop_hook_prevented",
  error: "a Stop hook prevented the turn from finishing",
  arms:
    "AgentFailure.stop_hook_prevented — DECLARED-ONLY: no capture grounds this terminal (`turn-stop-hook-stop` ended `success.completed`), so the mock keeps the declared arm and the evidence gap is listed in testdata/captures/MANIFEST.md",
  before(ctx) {
    // The vendor's own record of the stop-hook run. The shim NEVER synthesizes
    // a terminal from this — the terminal is the result's own reason — but a
    // consumer that renders which hooks ran needs the record.
    ctx.files.transcript.append({
      type: "system",
      subtype: "stop_hook_summary",
      hookCount: 1,
      hookInfos: [{ command: "/w/s/.claude/hooks/stop.sh", durationMs: 18 }],
      hookErrors: ["the stop hook refused"],
      hookAdditionalContext: [],
      preventedContinuation: true,
      stopReason: "the stop hook refused",
      hasOutput: true,
      level: "suggestion",
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
  },
});

const FAIL_HOOK_STOPPED = stopScenario({
  name: "fail-hook-stopped",
  subtype: "error_during_execution",
  terminalReason: "hook_stopped",
  error: "a hook stopped the turn",
  arms: "AgentFailure.hook_stopped",
});

const FAIL_CONTINUATION_PREVENTED = scenario({
  name: "fail-continuation-prevented",
  prompt: "!fail-continuation-prevented",
  emits:
    "an `informational` message with `prevent_continuation: true`, a `stop_hook_summary` record whose " +
    "`preventedContinuation` is true, then a `stop_hook_prevented` terminal",
  writes: "the `system:stop_hook_summary` line, the prompt line and the turn record",
  arms:
    "AgentFailure.stop_hook_prevented — the arm this pairing ACTUALLY reaches. AgentFailure.continuation_prevented " +
    "is UNSETTLED and UNGROUNDED: no `TerminalReason` names it, so the two declared prevent-continuation signals " +
    "ride the nearest terminal and nothing produces the continuation_prevented arm",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "fail-continuation-prevented" }, "fake continuation-prevented turn");
    ctx.systemMessage("informational", {
      content: "Execution stopped: continuation was prevented.",
      level: "warning",
      prevent_continuation: true,
    });
    ctx.files.transcript.append({
      type: "system",
      subtype: "stop_hook_summary",
      hookCount: 1,
      hookInfos: [{ command: "/w/s/.claude/hooks/stop.sh", durationMs: 22 }],
      hookErrors: [],
      hookAdditionalContext: [],
      preventedContinuation: true,
      stopReason: "continuation prevented",
      hasOutput: false,
      level: "suggestion",
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.result({
      subtype: "error_during_execution",
      terminalReason: "stop_hook_prevented",
      errors: ["continuation was prevented"],
    });
  },
});

/**
 * One API-error scenario: the mid-turn evidence AND the turn terminal.
 *
 * `apiRetry` says whether the vendor tried again before giving up, which is the
 * difference between a retriable class (429, 529, 500) and a terminal one
 * (401, 403, 400, 413, 404).
 */
function apiErrorScenario(spec: {
  name: string;
  status: number | null;
  errorClass: string;
  formatted: string;
  arm: string;
  retries: boolean;
  extra?: Record<string, unknown>;
}): Scenario {
  return scenario({
    name: spec.name,
    prompt: `!${spec.name}`,
    emits:
      `a \`system:api_error\` record${spec.retries ? " and an `api_retry` message" : ""}, a failed assistant message ` +
      `carrying the vendor's \`error\` class, then an ` +
      `\`error_during_execution\` result with \`terminal_reason: "api_error"\` and status ${String(spec.status)}`,
    writes: "a `system:api_error` line, the prompt line and the turn record",
    arms: `AgentUpdate.api_error mid-turn, then AgentFailure.api_request_failed → ${spec.arm}`,
    run(ctx) {
      ctx.log.debug(
        { turn: ctx.turn, branch: spec.name, api_error_status: spec.status ?? 0 },
        "fake api-error turn",
      );
      ctx.files.transcript.append({
        type: "system",
        subtype: "api_error",
        level: "error",
        error: {
          message: spec.formatted,
          formatted: spec.formatted,
          isNetworkDown: false,
          rateLimits: null,
          ...(spec.extra ?? {}),
        },
        ...(spec.retries ? { retryInMs: 549.38, retryAttempt: 1, maxRetries: 10 } : {}),
        isMeta: false,
        uuid: ctx.newUuid(),
        timestamp: ctx.nowIso(),
      });
      // THE VENDOR'S OWN ERROR CLASS, ON THE STREAM. The result record states
      // the HTTP status alone, and the status cannot separate `billing_error`
      // from an unmodelled 402, `oauth_org_not_allowed` from an ordinary 403,
      // or `max_output_tokens` from a status-less failure. `SDKAssistantMessage`
      // declares `error: SDKAssistantMessageError`, so the failed response is
      // where the class rides; the transcript keeps its own `system:api_error`
      // line and is left untouched.
      ctx.assistant([{ type: "text", text: spec.formatted }], {
        error: spec.errorClass,
        noReasoning: true,
        skipTranscript: true,
      });
      if (spec.retries) {
        ctx.systemMessage("api_retry", {
          attempt: 1,
          max_retries: 10,
          retry_delay_ms: 549,
          error_status: spec.status,
          error: spec.errorClass,
        });
      }
      ctx.result({
        subtype: "error_during_execution",
        terminalReason: "api_error",
        // THE STATUS IS THE WHOLE OF THE CLASSIFICATION: `api_error` names
        // twelve different failures and only the status tells them apart.
        ...(spec.status === null ? {} : { apiErrorStatus: spec.status }),
        errors: [spec.formatted],
      });
    },
  });
}

const API_RATE_LIMITED = apiErrorScenario({
  name: "api-429",
  status: 429,
  errorClass: "rate_limit",
  formatted: "Rate limited; retry after 30 seconds.",
  arm: "ApiRateLimited (with the retry-after)",
  retries: true,
  extra: { rateLimits: { retryAfterSeconds: 30 } },
});
const API_OVERLOADED = apiErrorScenario({
  name: "api-529",
  status: 529,
  errorClass: "overloaded",
  formatted: "The API is overloaded.",
  arm: "ApiOverloaded",
  retries: true,
});
const API_UNAUTHENTICATED = apiErrorScenario({
  name: "api-401",
  status: 401,
  errorClass: "authentication_failed",
  formatted: "Authentication failed.",
  arm: "ApiAuthenticationFailed",
  retries: false,
});
const API_FORBIDDEN = apiErrorScenario({
  name: "api-403",
  status: 403,
  errorClass: "invalid_request",
  formatted: "Permission denied for this request.",
  arm: "ApiPermissionDenied",
  retries: false,
});
const API_INVALID = apiErrorScenario({
  name: "api-400",
  status: 400,
  errorClass: "invalid_request",
  formatted: "The request was invalid.",
  arm: "ApiInvalidRequest",
  retries: false,
});
const API_TOO_LARGE = apiErrorScenario({
  name: "api-413",
  status: 413,
  errorClass: "invalid_request",
  formatted: "The request was too large.",
  arm: "ApiRequestTooLarge",
  retries: false,
});
const API_NOT_FOUND = apiErrorScenario({
  name: "api-404",
  status: 404,
  errorClass: "model_not_found",
  formatted: "The requested model was not found.",
  arm: "ApiNotFound",
  retries: false,
});
const API_INTERNAL = apiErrorScenario({
  name: "api-500",
  status: 500,
  errorClass: "server_error",
  formatted: "The service raised.",
  arm: "ApiInternal",
  retries: true,
});
const API_BILLING = apiErrorScenario({
  name: "api-billing",
  status: 402,
  errorClass: "billing_error",
  formatted: "The account has a billing problem.",
  arm: "ApiBillingError",
  retries: false,
});
const API_OAUTH_ORG = apiErrorScenario({
  name: "api-oauth-org",
  status: 403,
  errorClass: "oauth_org_not_allowed",
  formatted: "This organization is not permitted to use OAuth here.",
  arm: "ApiOauthOrgNotAllowed",
  retries: false,
});
const API_MAX_OUTPUT = apiErrorScenario({
  name: "api-max-output",
  status: null,
  errorClass: "max_output_tokens",
  formatted: "The response hit the max output tokens.",
  arm: "ApiMaxOutputTokens",
  retries: false,
});
const API_UNMODELED = apiErrorScenario({
  name: "api-unmodeled",
  status: 418,
  errorClass: "unknown",
  formatted: "An error class this build does not model.",
  arm: "ApiUnmodeledError",
  retries: false,
});

const MAX_TOKENS = scenario({
  name: "max-tokens",
  prompt: "!max-tokens",
  emits:
    "an assistant message truncated at the output-token ceiling: `stop_reason: \"max_tokens\"` on both the " +
    "message and the result, with the partial text kept",
  writes: "the truncated assistant line, the prompt line and the turn record",
  arms: "AgentResponseFailure.reason=max_tokens — the text is kept, the answer is incomplete",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "max-tokens" }, "fake max-output-tokens turn");
    ctx.assistant([{ type: "text", text: "The answer begins and then stops mid-" }], {
      stopReason: "max_tokens",
    });
    ctx.result({
      subtype: "success",
      result: "The answer begins and then stops mid-",
      stopReason: "max_tokens",
    });
  },
});

const REFUSAL_FALLBACK = scenario({
  name: "refusal-fallback",
  prompt: "!refusal-fallback",
  emits:
    "a refusal that FELL BACK to another model: a `fallback` content block naming both models, a " +
    "`model_refusal_fallback` message carrying the category and the retracted uuids, and the answer from the fallback leg",
  writes: "the fallback assistant line, a `system:model_refusal_fallback` line, the answer line, the turn record",
  arms: "AgentResponseFailure.reason=refused, then a fresh AgentResponse from the fallback model",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "refusal-fallback" }, "fake model-refusal (with fallback) turn");
    const refused = ctx.assistant([{ type: "fallback", from: { model: ctx.model }, to: { model: "fake-sonnet-5" } }], {
      stopReason: "refusal",
    });
    ctx.emit({
      type: "system",
      subtype: "model_refusal_fallback",
      trigger: "refusal",
      direction: "retry",
      original_model: ctx.model,
      fallback_model: "fake-sonnet-5",
      request_id: `req_fake_${refused.messageId}`,
      api_refusal_category: "reasoning_extraction",
      api_refusal_explanation: null,
      // The eviction signal: these uuids were delivered and are now retracted.
      retracted_message_uuids: [...refused.uuids],
      refused_user_message_uuid: null,
      content: "The first model's safeguards flagged this message. Switched to Fake Sonnet.",
    });
    ctx.files.transcript.append({
      type: "system",
      subtype: "model_refusal_fallback",
      direction: "retry",
      content: "The first model's safeguards flagged this message. Switched to Fake Sonnet.",
      level: "warning",
      trigger: "refusal",
      originalModel: ctx.model,
      fallbackModel: "fake-sonnet-5",
      requestId: `req_fake_${refused.messageId}`,
      apiRefusalCategory: "reasoning_extraction",
      apiRefusalExplanation: null,
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    const answer = "Here is the answer from the fallback model.";
    ctx.assistant([{ type: "text", text: answer }], { model: "fake-sonnet-5", stopReason: "end_turn" });
    ctx.result({
      subtype: "success",
      result: answer,
      extraModelUsage: {
        "fake-sonnet-5": {
          inputTokens: 2,
          outputTokens: 500,
          cacheReadInputTokens: 770_324,
          cacheCreationInputTokens: 0,
          webSearchRequests: 0,
          costUSD: 0.004,
          contextWindow: 200_000,
          maxOutputTokens: 32_000,
          canonicalModel: "fake-sonnet-5",
          provider: "firstParty",
        },
      },
    });
  },
});

const REFUSAL_NO_FALLBACK = scenario({
  name: "refusal-no-fallback",
  prompt: "!refusal-no-fallback",
  emits:
    "a refusal with NO fallback configured: a `model_refusal_no_fallback` message whose `content` is empty and " +
    "whose explanation points the integrator at the fallback docs, then an error terminal",
  writes: "a `system:model_refusal_no_fallback` line, the prompt line and the turn record",
  arms: "AgentResponseFailure.reason=refused with no recovery",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "refusal-no-fallback" }, "fake model-refusal (no fallback) turn");
    ctx.emit({
      type: "system",
      subtype: "model_refusal_no_fallback",
      original_model: ctx.model,
      request_id: "req_fake_refusal",
      api_refusal_category: "bio",
      api_refusal_explanation:
        "API integrators: you can reduce refusals for your users by configuring a fallback model.",
      refused_user_message_uuid: null,
      content: "",
    });
    ctx.files.transcript.append({
      type: "system",
      subtype: "model_refusal_no_fallback",
      content: "",
      level: "warning",
      originalModel: ctx.model,
      requestId: "req_fake_refusal",
      apiRefusalCategory: "bio",
      apiRefusalExplanation:
        "API integrators: you can reduce refusals for your users by configuring a fallback model.",
      refusedUserMessageUuid: null,
      isMeta: false,
      uuid: ctx.newUuid(),
      timestamp: ctx.nowIso(),
    });
    ctx.result({
      subtype: "error_during_execution",
      terminalReason: "model_error",
      errors: ["the model refused and no fallback is configured"],
    });
  },
});

/** The hook the two converter-fault scenarios announce, well-formed or not. */
const FAULT_HOOK = { hook_name: "PreToolUse:Read", hook_event: "PreToolUse" } as const;

const FAULT_CONVERTER = scenario({
  name: "fault-converter",
  prompt: "!fault-converter",
  emits:
    "ONE MALFORMED VENDOR MESSAGE and then an ordinary turn: a `hook_started` whose `hook_id` is the EMPTY " +
    "STRING — an identity the converter requires and refuses to invent — followed by prose and a success result. " +
    "The malformed message produces NO frame at all: the fold refuses it, logs a converter defect and records it " +
    "as residue, which is the contract's answer to a record missing a required field",
  writes: "the assistant line, the prompt line and the turn record; the malformed message is stream-only",
  arms:
    "SessionFault.converter_defect with an OPEN SessionDegradedWindow — the diagnostics arm, reached without any " +
    "rpc failing. The malformed message itself reaches NO conversation.v1 arm, which is the point",
  run(ctx) {
    ctx.log.warn(
      { turn: ctx.turn, branch: "fault-converter" },
      "fake turn carrying ONE malformed vendor message",
    );
    // `hook_id` is what `AgentHook`'s activity identity IS — the firing has no
    // other stable name across its two records — so an empty one is not a
    // degraded frame, it is no frame. Present-and-empty rather than absent: the
    // converter refuses an identity that is stated as nothing, which is the
    // shape a defective producer actually emits.
    ctx.systemMessage("hook_started", { hook_id: "", ...FAULT_HOOK });
    conclude(ctx, "One vendor message was unconvertible; the rest of the turn was ordinary.");
  },
});

const FAULT_RECOVER = scenario({
  name: "fault-recover",
  prompt: "!fault-recover",
  emits:
    "the SAME hook announcement, WELL-FORMED: a `hook_started`/`hook_response` pair carrying a real `hook_id`, " +
    "so the fold converts it and the frame the malformed turn could not produce appears. Nothing else changes — " +
    "the recovery is that an ordinary turn converted cleanly",
  writes: "the assistant line, the prompt line and the turn record",
  arms:
    "AgentHook.result=succeeded, and the diagnostics returning to HEALTHY with the degraded window CLOSED " +
    "carrying the dropped count the fault left behind",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "fault-recover" }, "fake recovery turn; every message converts");
    const hookId = ctx.newUuid();
    ctx.systemMessage("hook_started", { hook_id: hookId, ...FAULT_HOOK });
    ctx.systemMessage("hook_response", {
      hook_id: hookId,
      ...FAULT_HOOK,
      output: "",
      stdout: "{}\n",
      stderr: "",
      exit_code: 0,
      outcome: "success",
    });
    conclude(ctx, "Every message of this turn converted.");
  },
});

const CONTEXT_WINDOW_EXCEEDED = scenario({
  name: "context-window",
  prompt: "!context-window",
  emits: "a `prompt_too_long` terminal preceded by the vendor's informational notice naming the window",
  writes: "the prompt line and the turn record",
  arms: "AgentResponseFailure.reason=context_window_exceeded and AgentFailure.prompt_too_long",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "context-window" }, "fake context-window-exceeded turn");
    ctx.systemMessage("informational", {
      content: "The conversation exceeds this model's context window. Compact or clear it to continue.",
      level: "warning",
    });
    ctx.result({
      subtype: "error_during_execution",
      terminalReason: "prompt_too_long",
      errors: ["the conversation exceeds the model's context window"],
    });
  },
});

/**
 * The e2e marker turn.
 *
 * Selected by a MARKER inside the prompt rather than an `!name` prefix (see
 * `lifecycle.ts`'s selection note), because the daemon's acceptance gate sends
 * readable prose that merely CONTAINS the marker.
 */
export const FAIL_MARKER = scenario({
  name: "fail-marker",
  prompt: "(any prompt containing `e2e-fail-this-turn`)",
  emits: "an `error_during_execution` result and no assistant content — the daemon's merge-pipeline failure gate",
  writes: "the prompt line and the turn record",
  arms: "AgentFailure.execution_error",
  run(ctx) {
    ctx.log.debug({ turn: ctx.turn, branch: "fail-marker" }, "fake e2e failure-marker turn");
    ctx.result({
      subtype: "error_during_execution",
      terminalReason: "api_error",
      errors: ["e2e failure marker"],
    });
  },
});

export const FAILURE_SCENARIOS = [
  FAIL_EXECUTION,
  FAIL_MAX_TURNS,
  FAIL_BUDGET,
  FAIL_STRUCTURED_OUTPUT,
  FAIL_BLOCKING_LIMIT,
  FAIL_RAPID_REFILL,
  FAIL_PROMPT_TOO_LONG,
  FAIL_IMAGE,
  FAIL_MODEL,
  FAIL_MALFORMED_TOOL_USE,
  FAIL_TOOL_DEFERRED,
  FAIL_TOOL_DEFERRED_UNAVAILABLE,
  FAIL_TURN_SETUP,
  FAIL_ABORTED_TOOLS,
  FAIL_STOP_HOOK,
  FAIL_HOOK_STOPPED,
  FAIL_CONTINUATION_PREVENTED,
  API_RATE_LIMITED,
  API_OVERLOADED,
  API_UNAUTHENTICATED,
  API_FORBIDDEN,
  API_INVALID,
  API_TOO_LARGE,
  API_NOT_FOUND,
  API_INTERNAL,
  API_BILLING,
  API_OAUTH_ORG,
  API_MAX_OUTPUT,
  API_UNMODELED,
  MAX_TOKENS,
  REFUSAL_FALLBACK,
  REFUSAL_NO_FALLBACK,
  CONTEXT_WINDOW_EXCEEDED,
  FAULT_CONVERTER,
  FAULT_RECOVER,
  FAIL_MARKER,
];
