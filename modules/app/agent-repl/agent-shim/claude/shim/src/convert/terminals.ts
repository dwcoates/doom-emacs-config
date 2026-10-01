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
 * WHAT THE VENDOR SAID ABOUT THE FAILED REQUEST, as the fold remembered it.
 *
 * The result record carries the HTTP STATUS ALONE, and the status does not
 * separate `billing_error` from an unmodelled 402, `oauth_org_not_allowed` from
 * an ordinary 403, or `max_output_tokens` from a status-less failure. The CLASS
 * is stated on the vendor's own error records — `api_retry.error` and an
 * assistant message's `error`, both typed `SDKAssistantMessageError` — so the
 * fold remembers the last one of the turn and the terminal reads it here.
 *
 * The retry delay travels the same way: the vendor states it on `api_retry`
 * (`retry_delay_ms`), and `ApiRateLimited.retry_after_ms` is the field the
 * client counts down from, so the join IS made rather than dropped.
 */
export interface VendorApiError {
  /** The vendor's own error class, when one was stated this turn. */
  readonly errorClass?: string;
  /** The wait the vendor stated, in millis, when it stated one. */
  readonly retryAfterMs?: number;
  /**
   * The vendor's own sentence for the failure, from the error notice the CLI
   * wrote as an assistant message ("Claude Code 2.1.220 does not support this
   * model; version 2.1.251 or newer is required"). The `result` usually carries
   * no `errors` for an API failure, and without this the failure card said
   * "the model or resource does not exist" and nothing about WHICH or WHY.
   */
  readonly sentence?: string;
}

// ---------------------------------------------------------------------------
// Diagnostic classification for a vendor API failure
// ---------------------------------------------------------------------------

/**
 * A GREPPABLE operation name for what the vendor refused.
 *
 * DIAGNOSTIC, NOT A CLASSIFICATION THAT DECIDES AN ARM — {@link apiFailureKind}
 * is the one that maps to the proto and nothing here changes it. This exists so
 * the owner's two recurring failures ("the credential was rejected — sign in
 * again" and "the model or resource does not exist") land under a stable
 * operation a harvest can grep, without reading each free-text sentence:
 *
 *   - `shim.vendor.unreachable` — the request never reached the API (DNS,
 *     a refused or reset connection, a network timeout),
 *   - `shim.vendor.auth_rejected` — a credential/authentication refusal,
 *   - `shim.vendor.model_missing` — a model/resource-not-found refusal,
 *   - `shim.vendor.api_error` — every other recorded API failure.
 *
 * It reads the vendor's own class first, then the status, then the human
 * sentence, so a failure that named a class is not overruled by loose text.
 * UNREACHABLE IS READ FIRST: a network failure has no status, and its errno
 * (`ENOTFOUND`) would otherwise match the not-found pattern and read as a
 * missing model.
 */
export type VendorApiFailureKind =
  | "shim.vendor.unreachable"
  | "shim.vendor.auth_rejected"
  | "shim.vendor.model_missing"
  | "shim.vendor.api_error";

export function classifyVendorApiFailure(
  status: number | undefined,
  errorClass: string | undefined,
  message: string,
): VendorApiFailureKind {
  const haystack = `${errorClass ?? ""} ${message}`.toLowerCase();
  if (
    status === undefined &&
    /\b(enotfound|eai_again|econnrefused|econnreset|etimedout|enetunreach|ehostunreach)\b|can't reach the api|socket hang up/.test(
      haystack,
    )
  ) {
    return "shim.vendor.unreachable";
  }
  if (
    status === 401 ||
    status === 403 ||
    /\bauth|credential|unauthor|forbidden|sign[\s-]?in|oauth|api[\s-]?key/.test(haystack)
  ) {
    return "shim.vendor.auth_rejected";
  }
  if (
    status === 404 ||
    /not[\s-]?found|does not exist|no such|unknown model|resource/.test(haystack)
  ) {
    return "shim.vendor.model_missing";
  }
  return "shim.vendor.api_error";
}

/** The upper bound on the vendor sentence a diagnostic record carries. */
export const VENDOR_MESSAGE_MAX = 500;

/**
 * The vendor's human sentence, made SAFE to log.
 *
 * The message is a diagnostic prize — it is the sentence the owner sees — but a
 * credential must never ride it into the logs. The vendor should embed none,
 * yet a bearer token, an `sk-` key or a JWT that leaked into the text would be
 * logged verbatim otherwise, so any run of a credential SHAPE is masked before
 * the record is bounded. The mask is the only thing removed; the sentence is
 * otherwise the vendor's own words, truncated to {@link VENDOR_MESSAGE_MAX}.
 */
export function redactVendorMessage(message: string): string {
  const masked = message
    .replace(/\beyJ[A-Za-z0-9_-]{6,}\.[A-Za-z0-9._-]{6,}/g, "[redacted-jwt]")
    .replace(/\bsk-[A-Za-z0-9-]{8,}/gi, "[redacted-key]")
    .replace(/\bBearer\s+[A-Za-z0-9._-]{8,}/gi, "Bearer [redacted]");
  return masked.length > VENDOR_MESSAGE_MAX ? `${masked.slice(0, VENDOR_MESSAGE_MAX)}…` : masked;
}

/**
 * The vendor API's recorded failure, in its own declared taxonomy.
 *
 * THE KIND IS THE VENDOR'S, NOT A CLASSIFICATION: nothing here says whether
 * waiting helps — that judgement is the daemon's and reaches a client already
 * resolved.
 */
function apiRequestFailed(
  message: string,
  httpStatus: number | undefined,
  vendorError: VendorApiError,
): conversationv1.ApiRequestFailed {
  const kind = apiFailureKind(httpStatus, vendorError);
  return create(conversationv1.ApiRequestFailedSchema, { message, kind });
}

/**
 * Which arm of the taxonomy this failure is.
 *
 * THREE FACTS, IN THE ORDER OF WHAT EACH CAN SETTLE.
 *
 *  1. The three classes NO STATUS CAN NAME. `billing_error` shares 402 with
 *     anything else the vendor charges for, `oauth_org_not_allowed` is a 403
 *     like every other refused credential, and `max_output_tokens` arrives with
 *     no status at all. Each is its own arm in the proto precisely because the
 *     remedy differs, so the vendor's stated class wins over the status here.
 *  2. The HTTP STATUS, which is the finer fact for every remaining arm — the
 *     vendor spells both a 403 and a 413 `invalid_request`, and trusting the
 *     class there would collapse two arms into one.
 *  3. The CLASS AGAIN, as the fallback for a failure that stated no status.
 *
 * A CLASS THE VENDOR ADDED LATER falls out of all three and is kept BY NAME as
 * `unmodeled` rather than being silently mishandled.
 */
function apiFailureKind(
  httpStatus: number | undefined,
  vendor: VendorApiError,
): conversationv1.ApiRequestFailed["kind"] {
  const vendorError = vendor.errorClass;
  // UNSET IS NOT ZERO: the vendor said nothing about the wait unless it did,
  // and "retry now" is a different claim from silence.
  const wait =
    vendor.retryAfterMs === undefined ? {} : { retryAfterMs: BigInt(vendor.retryAfterMs) };

  switch (vendorError) {
    case "billing_error":
      return { case: "billingError", value: create(conversationv1.ApiBillingErrorSchema, {}) };
    case "oauth_org_not_allowed":
      return {
        case: "oauthOrgNotAllowed",
        value: create(conversationv1.ApiOauthOrgNotAllowedSchema, {}),
      };
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
    case 402:
      return { case: "billingError", value: create(conversationv1.ApiBillingErrorSchema, {}) };
    case 403:
      return {
        case: "permissionDenied",
        value: create(conversationv1.ApiPermissionDeniedSchema, {}),
      };
    case 404:
      return { case: "notFound", value: create(conversationv1.ApiNotFoundSchema, {}) };
    case 413:
      return { case: "requestTooLarge", value: create(conversationv1.ApiRequestTooLargeSchema, {}) };
    case 429:
      return { case: "rateLimited", value: create(conversationv1.ApiRateLimitedSchema, wait) };
    case 500:
      return { case: "internal", value: create(conversationv1.ApiInternalSchema, {}) };
    case 529:
      return { case: "overloaded", value: create(conversationv1.ApiOverloadedSchema, wait) };
    default:
      break;
  }

  switch (vendorError) {
    case "authentication_failed":
      return {
        case: "authenticationFailed",
        value: create(conversationv1.ApiAuthenticationFailedSchema, {}),
      };
    case "rate_limit":
      return { case: "rateLimited", value: create(conversationv1.ApiRateLimitedSchema, wait) };
    case "overloaded":
      return { case: "overloaded", value: create(conversationv1.ApiOverloadedSchema, wait) };
    case "invalid_request":
      return { case: "invalidRequest", value: create(conversationv1.ApiInvalidRequestSchema, {}) };
    case "model_not_found":
      return { case: "notFound", value: create(conversationv1.ApiNotFoundSchema, {}) };
    case "server_error":
      return { case: "internal", value: create(conversationv1.ApiInternalSchema, {}) };
    default:
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
  error_max_turns: FAILURE_ARMS.max_turns,
  error_max_budget_usd: FAILURE_ARMS.budget_exhausted,
  error_max_structured_output_retries:
    FAILURE_ARMS.structured_output_retry_exhausted,
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
 * Whether a result message is the USER'S OWN STOP.
 *
 * Exported because a stop has a consequence beyond the terminal itself: every
 * tool call still open when it lands was cut by it, and nothing else in the
 * fold can recognize the two reasons that mean "the user stopped this".
 */
export function isUserStop(message: Extract<SdkMessage, { type: "result" }>): boolean {
  const reason = (message as unknown as RawResult).terminal_reason;
  return reason === "aborted_streaming" || reason === "aborted_tools";
}

/**
 * The terminal of a turn the vendor ABSORBED into another turn.
 *
 * A turn the vendor started on its own (a hand-back, a task notification) can
 * take one of the shim's queued sends in between its tool rounds, and from
 * that frame on the vendor turn answers the send (engine/sends.ts, "the
 * fold"). The absorbed turn's own `result` therefore never comes, but it is a
 * turn every consumer is watching, so it is concluded here: COMPLETED, since
 * it ran to the point where the vendor moved on, naming the last top-level
 * prose it produced as its answer.
 */
export function absorbedTurnTerminal(
  context: FoldContext,
  coordinate: string,
  lastAnswer: conversationv1.AgentActivityId | undefined,
): FoldOutput {
  LOGGER.info(
    { turn: context.turnId?.value, coordinate, answer: lastAnswer?.value },
    "the turn was absorbed into another vendor turn; it is concluded as completed",
  );
  const result: conversationv1.AgentFrame["result"] = {
    case: "success",
    value: create(conversationv1.AgentSuccessSchema, {
      outcome: {
        case: "completed",
        value: create(conversationv1.AgentCompletedSchema, { answer: lastAnswer }),
      },
    }),
  };
  return finish(
    context,
    { agentId: context.mainAgentId, vendorUuid: coordinate, discriminator: "" },
    "agent_frame.success.completed.absorbed",
    result,
  );
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
  vendorApiError: VendorApiError = {},
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
    LOGGER.info({ turn: context.turnId?.value }, "the turn completed");
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

  if (isUserStop(message)) {
    // THE CALLER'S OWN STATEMENT OF HOW, VERBATIM. The vendor's result says
    // only that the turn was stopped; KillTurn's caller said how, and a stop
    // nobody stated a how for records `by_user` with no command.
    const byUser = context.stopCommand ?? create(conversationv1.AgentInterruptedByUserSchema, {});
    LOGGER.info(
      { turn: context.turnId?.value, reason, command: byUser.command.case ?? "unstated" },
      "the turn was interrupted by a user stop",
    );
    const result: conversationv1.AgentFrame["result"] = {
      case: "success",
      value: create(conversationv1.AgentSuccessSchema, {
        outcome: {
          case: "interrupted",
          value: create(conversationv1.AgentInterruptedSchema, {
            cause: {
              case: "byUser",
              value: byUser,
            },
          }),
        },
      }),
    };
    return finish(context, origin, "agent_frame.success.interrupted.by_user", result);
  }

  if (reason === "background_requested") {
    LOGGER.info({ turn: context.turnId?.value }, "the turn moved to the background rather than ending");
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
    LOGGER.info(
      {
        turn: context.turnId?.value,
        http_status: status,
        vendor_error: vendorApiError.errorClass,
        retry_after_ms: vendorApiError.retryAfterMs,
      },
      "the turn ended on a recorded API failure",
    );
    // THE ONE DIAGNOSTIC RECORD, at INFO so it shows without verbose. The
    // owner intermittently sees a credential rejection or a "model or resource
    // does not exist" and the cause — a clobbered token, an expired one, an
    // account/scope mismatch — is only recoverable after the fact with the
    // class, the status, the vendor's own sentence, the ACCOUNT (config-dir)
    // the failing token belonged to, and the model, all on one greppable line.
    // NO CREDENTIAL VALUE IS LOGGED: the config-dir is a path, and the sentence
    // is redacted of any token shape before it is bounded.
    const vendorMessage = errors[errors.length - 1] ?? vendorApiError.sentence ?? "";
    LOGGER.info(
      {
        operation: classifyVendorApiFailure(status, vendorApiError.errorClass, vendorMessage),
        turn: context.turnId?.value,
        vendor_error: vendorApiError.errorClass,
        http_status: status,
        vendor_message: redactVendorMessage(vendorMessage),
        claude_config_dir: context.claudeConfigDir,
        model: context.model,
        retry_after_ms: vendorApiError.retryAfterMs,
      },
      "the shim observed a vendor API failure; recording the class, status, message, account config-dir and model for diagnosis",
    );
    const result: conversationv1.AgentFrame["result"] = {
      case: "failure",
      value: create(conversationv1.AgentFailureSchema, {
        errors,
        failure: {
          case: "apiRequestFailed",
          value: apiRequestFailed(
            vendorMessage === "" ? "the vendor API failed the request" : vendorMessage,
            status,
            vendorApiError,
          ),
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
    // warn: a defect because an unknown terminal reason can only be relayed as unclassified failure.
    LOGGER.warn(
      { turn: context.turnId?.value, terminal_reason: reason, subtype: raw.subtype },
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
  LOGGER.debug(
    { turn: context.turnId?.value, terminal_reason: reason, arm: failure.case },
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
