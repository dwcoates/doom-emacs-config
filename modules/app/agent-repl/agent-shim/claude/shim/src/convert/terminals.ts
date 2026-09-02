/**
 * convert/terminals.ts — HOW A TURN ENDED, and nothing else.
 *
 * # `result` is the only source
 *
 * A turn terminal comes from the vendor's own `result` record and from no other
 * evidence. In particular HOOK ACTIVITY NEVER SYNTHESIZES ONE: stop hooks fire
 * after a turn's stop and a hook's outcome is not the turn's, so a terminal
 * built from one would end a turn the vendor had not ended. The sixteen failure
 * arms include the vendor's own hook-stop terminals precisely so those can be
 * RELAYED rather than inferred.
 *
 * # The arm is the vendor's, not a classification
 *
 * `terminal_reason` is the vendor's declared closed set and it decides the arm.
 * The result SUBTYPE is the fallback for a vendor that stated no terminal
 * reason, and `errors` is the account either way — a list, because a run can
 * fail several times before it gives up and reporting only the last hides that
 * the earlier attempts failed differently.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";
import type { SdkMessage } from "../sdk/types.js";
import { terminalEntry } from "./entries.js";
import type { FoldContext } from "./fold-context.js";
import type { FoldOutput } from "./fold.js";

const LOGGER = bindLog({ component: "shim-convert-terminals", operation: "shim.convert.terminals" });

// ---------------------------------------------------------------------------
// The API's own failure taxonomy
// ---------------------------------------------------------------------------

/**
 * The vendor API's recorded failure, in its own declared taxonomy.
 *
 * THE KIND IS THE VENDOR'S, NOT A CLASSIFICATION: nothing here says whether
 * waiting helps — that judgement is the daemon's and reaches a client already
 * resolved. `retry_after_ms` is UNSET from a result record: the vendor states a
 * retry delay only on its `api_retry` message, which is a different record, and
 * carrying one across would be a join the fold does not make.
 */
function apiRequestFailed(
  message: string,
  httpStatus: number | undefined,
  vendorError: string | undefined,
): conversationv1.ApiRequestFailed {
  const kind = apiFailureKind(httpStatus, vendorError);
  return create(conversationv1.ApiRequestFailedSchema, { message, kind });
}

/** Which arm of the taxonomy a status or a vendor error string names. */
function apiFailureKind(
  httpStatus: number | undefined,
  vendorError: string | undefined,
): conversationv1.ApiRequestFailed["kind"] {
  const empty = <T>(schema: T): T => schema;
  void empty;
  switch (vendorError) {
    case "authentication_failed":
      return {
        case: "authenticationFailed",
        value: create(conversationv1.ApiAuthenticationFailedSchema, {}),
      };
    case "oauth_org_not_allowed":
      return {
        case: "oauthOrgNotAllowed",
        value: create(conversationv1.ApiOauthOrgNotAllowedSchema, {}),
      };
    case "billing_error":
      return { case: "billingError", value: create(conversationv1.ApiBillingErrorSchema, {}) };
    case "rate_limit":
      return { case: "rateLimited", value: create(conversationv1.ApiRateLimitedSchema, {}) };
    case "overloaded":
      return { case: "overloaded", value: create(conversationv1.ApiOverloadedSchema, {}) };
    case "invalid_request":
      return { case: "invalidRequest", value: create(conversationv1.ApiInvalidRequestSchema, {}) };
    case "model_not_found":
      return { case: "notFound", value: create(conversationv1.ApiNotFoundSchema, {}) };
    case "server_error":
      return { case: "internal", value: create(conversationv1.ApiInternalSchema, {}) };
    case "max_output_tokens":
      return { case: "maxOutputTokens", value: create(conversationv1.ApiMaxOutputTokensSchema, {}) };
    default:
      break;
  }
  switch (httpStatus) {
    case 400:
      return { case: "invalidRequest", value: create(conversationv1.ApiInvalidRequestSchema, {}) };
    case 401:
      return {
        case: "authenticationFailed",
        value: create(conversationv1.ApiAuthenticationFailedSchema, {}),
      };
    case 403:
      return { case: "permissionDenied", value: create(conversationv1.ApiPermissionDeniedSchema, {}) };
    case 404:
      return { case: "notFound", value: create(conversationv1.ApiNotFoundSchema, {}) };
    case 413:
      return { case: "requestTooLarge", value: create(conversationv1.ApiRequestTooLargeSchema, {}) };
    case 429:
      return { case: "rateLimited", value: create(conversationv1.ApiRateLimitedSchema, {}) };
    case 500:
      return { case: "internal", value: create(conversationv1.ApiInternalSchema, {}) };
    case 529:
      return { case: "overloaded", value: create(conversationv1.ApiOverloadedSchema, {}) };
    default:
      // A CLASS THE VENDOR ADDED LATER arrives here by name rather than as a
      // silently mishandled value.
      return {
        case: "unmodeled",
        value: create(conversationv1.ApiUnmodeledErrorSchema, {
          type: vendorError ?? (httpStatus === undefined ? "unknown" : `http_${httpStatus}`),
        }),
      };
  }
}

// ---------------------------------------------------------------------------
// terminal_reason → the arm
// ---------------------------------------------------------------------------

/** Every empty failure arm, by the vendor terminal reason that names it. */
const FAILURE_ARMS: Readonly<Record<string, () => conversationv1.AgentFailure["failure"]>> = {
  blocking_limit: () => ({
    case: "blockingLimit",
    value: create(conversationv1.AgentStoppedAtBlockingLimitSchema, {}),
  }),
  rapid_refill_breaker: () => ({
    case: "rapidRefillBreaker",
    value: create(conversationv1.AgentStoppedByRapidRefillBreakerSchema, {}),
  }),
  prompt_too_long: () => ({
    case: "promptTooLong",
    value: create(conversationv1.AgentPromptTooLongSchema, {}),
  }),
  image_error: () => ({
    case: "imageError",
    value: create(conversationv1.AgentImageRejectedSchema, {}),
  }),
  model_error: () => ({
    case: "modelError",
    value: create(conversationv1.AgentModelErrorSchema, {}),
  }),
  malformed_tool_use_exhausted: () => ({
    case: "malformedToolUseExhausted",
    value: create(conversationv1.AgentMalformedToolUseExhaustedSchema, {}),
  }),
  stop_hook_prevented: () => ({
    case: "stopHookPrevented",
    value: create(conversationv1.AgentStoppedByStopHookSchema, {}),
  }),
  hook_stopped: () => ({
    case: "hookStopped",
    value: create(conversationv1.AgentStoppedByHookSchema, {}),
  }),
  tool_deferred: () => ({
    case: "toolDeferred",
    value: create(conversationv1.AgentToolDeferredSchema, {}),
  }),
  tool_deferred_unavailable: () => ({
    case: "toolDeferredUnavailable",
    value: create(conversationv1.AgentToolDeferredUnavailableSchema, {}),
  }),
  max_turns: () => ({
    case: "maxTurns",
    value: create(conversationv1.AgentMaxTurnsReachedSchema, {}),
  }),
  budget_exhausted: () => ({
    case: "budgetExhausted",
    value: create(conversationv1.AgentBudgetExhaustedSchema, {}),
  }),
  structured_output_retry_exhausted: () => ({
    case: "structuredOutputRetryExhausted",
    value: create(conversationv1.AgentStructuredOutputRetriesExhaustedSchema, {}),
  }),
  turn_setup_failed: () => ({
    case: "turnSetupFailed",
    value: create(conversationv1.AgentTurnSetupFailedSchema, {}),
  }),
};

/** Every failure arm the result SUBTYPE names, for a vendor that stated no reason. */
const SUBTYPE_ARMS: Readonly<Record<string, () => conversationv1.AgentFailure["failure"]>> = {
  error_during_execution: () => ({
    case: "executionError",
    value: create(conversationv1.AgentExecutionErrorSchema, {}),
  }),
  error_max_turns: FAILURE_ARMS.max_turns as () => conversationv1.AgentFailure["failure"],
  error_max_budget_usd: FAILURE_ARMS.budget_exhausted as () => conversationv1.AgentFailure["failure"],
  error_max_structured_output_retries:
    FAILURE_ARMS.structured_output_retry_exhausted as () => conversationv1.AgentFailure["failure"],
};

/** The result record, read loosely for the fields the SDK declares optionally. */
interface RawResult {
  readonly subtype?: string;
  readonly is_error?: boolean;
  readonly terminal_reason?: string;
  readonly errors?: unknown;
  readonly api_error_status?: number | null;
  readonly result?: unknown;
  readonly permission_denials?: unknown;
}

/** The error strings the run accumulated, oldest first. */
function accumulatedErrors(raw: RawResult): string[] {
  const errors = raw.errors;
  if (!Array.isArray(errors)) return [];
  return errors.filter((entry): entry is string => typeof entry === "string");
}

/**
 * The turn's terminal frame.
 *
 * `turnEnded` is set on the output because the ENGINE needs the same fact the
 * feed does — the main thread can accept a prompt again — and making it dig the
 * frame back out of the row list would be a second parse of a resolved answer.
 */
export function convertResult(
  message: Extract<SdkMessage, { type: "result" }>,
  context: FoldContext,
  lastAnswer: conversationv1.AgentActivityId | undefined,
): FoldOutput {
  const raw = message as unknown as RawResult;
  const reason = raw.terminal_reason;
  const errors = accumulatedErrors(raw);
  const origin = {
    agentId: context.mainAgentId,
    vendorUuid: message.uuid,
    discriminator: "",
  };

  // ---- The two success shapes -------------------------------------------
  if (reason === "completed" || (reason === undefined && raw.subtype === "success" && raw.is_error !== true)) {
    LOGGER.log({ turn: context.turnId?.value }, "the turn completed");
    const result: conversationv1.AgentFrame["result"] = {
      case: "success",
      value: create(conversationv1.AgentSuccessSchema, {
        outcome: {
          case: "completed",
          value: create(conversationv1.AgentCompletedSchema, {
            // UNSET when no prose was produced at all — a refusal with empty
            // content, a ceiling hit before anything was said. Absence is not
            // an error, and naming a unit that does not exist would be one.
            answer: lastAnswer,
          }),
        },
      }),
    };
    return finish(context, origin, "agent_frame.success.completed", result);
  }

  if (reason === "aborted_streaming" || reason === "aborted_tools") {
    LOGGER.log({ turn: context.turnId?.value, reason }, "the turn was interrupted by a user stop");
    const result: conversationv1.AgentFrame["result"] = {
      case: "success",
      value: create(conversationv1.AgentSuccessSchema, {
        outcome: {
          case: "interrupted",
          value: create(conversationv1.AgentInterruptedSchema, {
            cause: {
              case: "byUser",
              value: create(conversationv1.AgentInterruptedByUserSchema, {}),
            },
          }),
        },
      }),
    };
    return finish(context, origin, "agent_frame.success.interrupted.by_user", result);
  }

  if (reason === "background_requested") {
    LOGGER.log({ turn: context.turnId?.value }, "the turn moved to the background rather than ending");
    const result: conversationv1.AgentFrame["result"] = {
      case: "success",
      value: create(conversationv1.AgentSuccessSchema, {
        outcome: {
          case: "backgrounded",
          value: create(conversationv1.AgentBackgroundedSchema, {}),
        },
      }),
    };
    return finish(context, origin, "agent_frame.success.backgrounded", result);
  }

  // ---- The failure arms --------------------------------------------------
  if (reason === "api_error") {
    const status = typeof raw.api_error_status === "number" ? raw.api_error_status : undefined;
    LOGGER.log(
      { level: "warn", turn: context.turnId?.value, http_status: status },
      "the turn ended on a recorded API failure",
    );
    const result: conversationv1.AgentFrame["result"] = {
      case: "failure",
      value: create(conversationv1.AgentFailureSchema, {
        errors,
        failure: {
          case: "apiRequestFailed",
          value: apiRequestFailed(errors[errors.length - 1] ?? "the vendor API failed the request", status, undefined),
        },
      }),
    };
    return finish(context, origin, "agent_frame.failure.api_request_failed", result);
  }

  const arm =
    (reason !== undefined ? FAILURE_ARMS[reason] : undefined) ??
    (raw.subtype !== undefined ? SUBTYPE_ARMS[raw.subtype] : undefined);
  if (arm === undefined) {
    // NO INVENTED ARM. A terminal reason nothing here spells is relayed as the
    // unclassified execution failure, with `errors` as the account — which is
    // exactly what that arm is for — and the site is logged so the gap is
    // reported rather than guessed at.
    LOGGER.log(
      { level: "warn", turn: context.turnId?.value, terminal_reason: reason, subtype: raw.subtype },
      "no failure arm spells this terminal reason; relayed as an unclassified execution failure",
    );
    const result: conversationv1.AgentFrame["result"] = {
      case: "failure",
      value: create(conversationv1.AgentFailureSchema, {
        errors,
        failure: {
          case: "executionError",
          value: create(conversationv1.AgentExecutionErrorSchema, {}),
        },
      }),
    };
    return finish(context, origin, "agent_frame.failure.execution_error", result);
  }

  const failure = arm();
  LOGGER.log(
    { level: "warn", turn: context.turnId?.value, terminal_reason: reason, arm: failure.case },
    "the turn ended on a vendor-stated failure",
  );
  const result: conversationv1.AgentFrame["result"] = {
    case: "failure",
    value: create(conversationv1.AgentFailureSchema, { errors, failure }),
  };
  return finish(context, origin, `agent_frame.failure.${String(failure.case)}`, result);
}

/** The one shape a terminal answers with: the row, and the engine's signal. */
function finish(
  context: FoldContext,
  origin: { agentId: conversationv1.AgentId; vendorUuid: string; discriminator: string },
  discriminator: string,
  result: conversationv1.AgentFrame["result"],
): FoldOutput {
  const entry = terminalEntry(context, { ...origin, discriminator }, result);
  const frame = entry.item.kind === "frame" ? entry.item.frame : undefined;
  if (frame === undefined) throw new Error("shim convert: a terminal entry must carry a frame");
  return { entries: [entry], turnEnded: { frame } };
}
