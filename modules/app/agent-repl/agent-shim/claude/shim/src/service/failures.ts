/**
 * service/failures.ts — THE one spelling of every shim.v1 refusal.
 *
 * # Why a module instead of inline `create(...)` calls
 *
 * Each shim.v1 failure message is `detail` (a human string, never switched on)
 * plus a `kind`/`cause` ONEOF naming the machine-readable reason. Built inline,
 * a refusal is four lines of `create()` at every site, and the sites drift:
 * one handler answers `noSession`, another leaves the oneof unset and the
 * daemon receives an illegal message. Every refusal is minted here, by one
 * function per failure message (the base function of the proto→code mapping),
 * so the arm is chosen from a closed union the compiler checks and the oneof
 * can never be left unset.
 *
 * # The three refusal channels, and which one a verb uses
 *
 *   - A TYPED FAILURE in the response's own `result` oneof: the normal answer
 *     for a unary verb whose refusal is a fact about the session (no session,
 *     turn already open, model not in the catalog). It is a SUCCESSFUL rpc
 *     carrying a refusal, because the caller asked a legitimate question and
 *     got a legitimate answer.
 *   - A CONNECT ERROR: for refusals a response message cannot carry —
 *     `InvalidArgument` for a malformed request (there is no legal response to
 *     an illegal request), `NotFound` for a refused STREAM open (a stream has
 *     no failure message to put it in, so it closes at the transport), and
 *     `Unimplemented` for the workflow trio.
 *   - A SESSION FAULT on WatchSession: not a refusal at all, but the shim
 *     reporting its own degradation to whoever is listening.
 */
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { conversationv1, shimv1 } from "../proto.js";

// ---------------------------------------------------------------------------
// StartSession
// ---------------------------------------------------------------------------

/**
 * Why a session could not be started.
 *
 * `cold` is the one arm carrying evidence: the daemon needs the context cost
 * and the reason to offer the user a remediation, so a bare "it was cold" is
 * unactionable.
 */
export type StartSessionCause =
  | { readonly kind: "cold"; readonly cold: conversationv1.SessionCold }
  | { readonly kind: "vendorStartFailed" }
  | { readonly kind: "unknownSession" }
  | { readonly kind: "alreadyStarted" }
  | { readonly kind: "conversationOwned" };

/** The base constructor for `shim.v1.StartSessionFailure`. */
export function startSessionFailure(
  cause: StartSessionCause,
  detail: string,
): shimv1.StartSessionFailure {
  return create(shimv1.StartSessionFailureSchema, {
    detail,
    cause:
      cause.kind === "cold"
        ? { case: "cold", value: cause.cold }
        : cause.kind === "vendorStartFailed"
          ? { case: "vendorStartFailed", value: create(shimv1.StartSessionVendorStartFailedSchema, {}) }
          : cause.kind === "unknownSession"
            ? { case: "unknownSession", value: create(shimv1.StartSessionUnknownSessionSchema, {}) }
            : cause.kind === "alreadyStarted"
              ? { case: "alreadyStarted", value: create(shimv1.StartSessionAlreadyStartedSchema, {}) }
              : { case: "conversationOwned", value: create(shimv1.StartSessionConversationOwnedSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function startSessionRefused(
  cause: StartSessionCause,
  detail: string,
): shimv1.StartSessionResponse {
  return create(shimv1.StartSessionResponseSchema, {
    result: { case: "failure", value: startSessionFailure(cause, detail) },
  });
}

/** The accepted answer, carrying the session's opening facts. */
export function startSessionStarted(
  session: conversationv1.SessionStarted,
): shimv1.StartSessionResponse {
  return create(shimv1.StartSessionResponseSchema, {
    result: {
      case: "success",
      value: create(shimv1.StartSessionSuccessSchema, { session }),
    },
  });
}

// ---------------------------------------------------------------------------
// SetSessionModel
// ---------------------------------------------------------------------------

/** Why the model could not be changed. */
export type SetSessionModelCause =
  | { readonly kind: "cold"; readonly cold: conversationv1.SessionCold }
  | { readonly kind: "modelNotInCatalog" }
  | { readonly kind: "noSession" }
  | { readonly kind: "vendorRefused" };

/** The base constructor for `shim.v1.SetSessionModelFailure`. */
export function setSessionModelFailure(
  cause: SetSessionModelCause,
  detail: string,
): shimv1.SetSessionModelFailure {
  return create(shimv1.SetSessionModelFailureSchema, {
    detail,
    cause:
      cause.kind === "cold"
        ? { case: "cold", value: cause.cold }
        : cause.kind === "modelNotInCatalog"
          ? { case: "modelNotInCatalog", value: create(shimv1.SetSessionModelNotInCatalogSchema, {}) }
          : cause.kind === "noSession"
            ? { case: "noSession", value: create(shimv1.SetSessionModelNoSessionSchema, {}) }
            : { case: "vendorRefused", value: create(shimv1.SetSessionModelVendorRefusedSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function setSessionModelRefused(
  cause: SetSessionModelCause,
  detail: string,
): shimv1.SetSessionModelResponse {
  return create(shimv1.SetSessionModelResponseSchema, {
    result: { case: "failure", value: setSessionModelFailure(cause, detail) },
  });
}

// ---------------------------------------------------------------------------
// SetSessionPermissionMode
// ---------------------------------------------------------------------------

/** Why the permission mode could not be changed. */
export type SetSessionPermissionModeKind =
  | { readonly kind: "noSession" }
  | { readonly kind: "vendorRefused" };

/** The base constructor for `shim.v1.SetSessionPermissionModeFailure`. */
export function setSessionPermissionModeFailure(
  cause: SetSessionPermissionModeKind,
  detail: string,
): shimv1.SetSessionPermissionModeFailure {
  return create(shimv1.SetSessionPermissionModeFailureSchema, {
    detail,
    kind:
      cause.kind === "noSession"
        ? { case: "noSession", value: create(shimv1.SetSessionPermissionModeNoSessionSchema, {}) }
        : { case: "vendorRefused", value: create(shimv1.SetSessionPermissionModeVendorRefusedSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function setSessionPermissionModeRefused(
  cause: SetSessionPermissionModeKind,
  detail: string,
): shimv1.SetSessionPermissionModeResponse {
  return create(shimv1.SetSessionPermissionModeResponseSchema, {
    result: { case: "failure", value: setSessionPermissionModeFailure(cause, detail) },
  });
}

// ---------------------------------------------------------------------------
// Hibernate
// ---------------------------------------------------------------------------

/**
 * Why the pre-hibernation compaction could not be performed.
 *
 * `HibernateError` carries NO `detail` field — the arm is the whole answer,
 * except `compactionFailed`, which carries the vendor's own wording in
 * `error`. So this is the one refusal whose human string lives inside an arm.
 */
export type HibernateErrorKind =
  | { readonly kind: "turnInFlight" }
  | { readonly kind: "compactionFailed"; readonly error: string }
  | { readonly kind: "noSession" };

/** The base constructor for `shim.v1.HibernateError`. */
export function hibernateError(cause: HibernateErrorKind): shimv1.HibernateError {
  return create(shimv1.HibernateErrorSchema, {
    kind:
      cause.kind === "turnInFlight"
        ? { case: "turnInFlight", value: create(shimv1.HibernateTurnInFlightSchema, {}) }
        : cause.kind === "compactionFailed"
          ? {
              case: "compactionFailed",
              value: create(shimv1.HibernateCompactionFailedSchema, { error: cause.error }),
            }
          : { case: "noSession", value: create(shimv1.HibernateNoSessionSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function hibernateRefused(cause: HibernateErrorKind): shimv1.HibernateResponse {
  return create(shimv1.HibernateResponseSchema, {
    result: { case: "error", value: hibernateError(cause) },
  });
}

/** The ack the daemon waits for before standing the shim down. */
export function hibernateAcked(): shimv1.HibernateResponse {
  return create(shimv1.HibernateResponseSchema, {
    result: { case: "success", value: create(shimv1.HibernateSuccessSchema, {}) },
  });
}

// ---------------------------------------------------------------------------
// KillSession
// ---------------------------------------------------------------------------

/**
 * Why the session was not ended.
 *
 * `live` is the refusal an unforced kill answers with, and it NAMES what is
 * live so the daemon can tell the user what forcing would destroy.
 */
export type KillSessionCause =
  | { readonly kind: "live"; readonly live: conversationv1.SessionLive }
  | { readonly kind: "noSession" }
  | { readonly kind: "queryRefusedToEnd" };

/** The base constructor for `shim.v1.KillSessionFailure`. */
export function killSessionFailure(
  cause: KillSessionCause,
  detail: string,
): shimv1.KillSessionFailure {
  return create(shimv1.KillSessionFailureSchema, {
    detail,
    cause:
      cause.kind === "live"
        ? { case: "live", value: cause.live }
        : cause.kind === "noSession"
          ? { case: "noSession", value: create(shimv1.KillSessionNoSessionSchema, {}) }
          : { case: "queryRefusedToEnd", value: create(shimv1.KillSessionQueryRefusedToEndSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function killSessionRefused(
  cause: KillSessionCause,
  detail: string,
): shimv1.KillSessionResponse {
  return create(shimv1.KillSessionResponseSchema, {
    result: { case: "failure", value: killSessionFailure(cause, detail) },
  });
}

/** The session ended; `closed` says how, and names what died with it. */
export function killSessionClosed(
  closed: conversationv1.SessionKilled,
): shimv1.KillSessionResponse {
  return create(shimv1.KillSessionResponseSchema, {
    result: { case: "success", value: create(shimv1.KillSessionSuccessSchema, { closed }) },
  });
}

// ---------------------------------------------------------------------------
// StartTurn
// ---------------------------------------------------------------------------

/** Why a turn could not be started. */
export type StartTurnKind =
  | { readonly kind: "turnAlreadyOpen" }
  | { readonly kind: "noSession" }
  | { readonly kind: "vendorRefused" }
  | { readonly kind: "queryDead" };

/** The base constructor for `shim.v1.StartTurnFailure`. */
export function startTurnFailure(cause: StartTurnKind, detail: string): shimv1.StartTurnFailure {
  return create(shimv1.StartTurnFailureSchema, {
    detail,
    kind:
      cause.kind === "turnAlreadyOpen"
        ? { case: "turnAlreadyOpen", value: create(shimv1.StartTurnTurnAlreadyOpenSchema, {}) }
        : cause.kind === "noSession"
          ? { case: "noSession", value: create(shimv1.StartTurnNoSessionSchema, {}) }
          : cause.kind === "vendorRefused"
            ? { case: "vendorRefused", value: create(shimv1.StartTurnVendorRefusedSchema, {}) }
            : { case: "queryDead", value: create(shimv1.StartTurnQueryDeadSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function startTurnRefused(cause: StartTurnKind, detail: string): shimv1.StartTurnResponse {
  return create(shimv1.StartTurnResponseSchema, {
    result: { case: "failure", value: startTurnFailure(cause, detail) },
  });
}

/** The prompt was delivered; the record that comes back IS the turn's prompt. */
export function startTurnAccepted(prompt: conversationv1.AgentPrompt): shimv1.StartTurnResponse {
  return create(shimv1.StartTurnResponseSchema, {
    result: { case: "success", value: create(shimv1.StartTurnSuccessSchema, { prompt }) },
  });
}

// ---------------------------------------------------------------------------
// UpdateAgent
// ---------------------------------------------------------------------------

/**
 * Why the agent did not accept what was said to it.
 *
 * `answerMismatch` is the permission gate's own refusal: an answer whose
 * echoed values do not match the pending callback the shim already holds is
 * rejected rather than guessed at.
 */
export type UpdateAgentKind =
  | { readonly kind: "unknownAgent" }
  | { readonly kind: "noOpenAsk" }
  | { readonly kind: "answerMismatch" }
  | { readonly kind: "nothingRunning" }
  | { readonly kind: "noSession" };

/** The base constructor for `shim.v1.UpdateAgentFailure`. */
export function updateAgentFailure(
  cause: UpdateAgentKind,
  detail: string,
): shimv1.UpdateAgentFailure {
  return create(shimv1.UpdateAgentFailureSchema, {
    detail,
    kind:
      cause.kind === "unknownAgent"
        ? { case: "unknownAgent", value: create(shimv1.UpdateAgentUnknownAgentSchema, {}) }
        : cause.kind === "noOpenAsk"
          ? { case: "noOpenAsk", value: create(shimv1.UpdateAgentNoOpenAskSchema, {}) }
          : cause.kind === "answerMismatch"
            ? { case: "answerMismatch", value: create(shimv1.UpdateAgentAnswerMismatchSchema, {}) }
            : cause.kind === "nothingRunning"
              ? { case: "nothingRunning", value: create(shimv1.UpdateAgentNothingRunningSchema, {}) }
              : { case: "noSession", value: create(shimv1.UpdateAgentNoSessionSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function updateAgentRefused(
  cause: UpdateAgentKind,
  detail: string,
): shimv1.UpdateAgentResponse {
  return create(shimv1.UpdateAgentResponseSchema, {
    result: { case: "failure", value: updateAgentFailure(cause, detail) },
  });
}

/** Delivered. Everything it produces arrives on the agent's own stream. */
export function updateAgentDelivered(): shimv1.UpdateAgentResponse {
  return create(shimv1.UpdateAgentResponseSchema, {
    result: { case: "success", value: create(shimv1.UpdateAgentSuccessSchema, {}) },
  });
}

// ---------------------------------------------------------------------------
// KillTurn
// ---------------------------------------------------------------------------

/** Why the turn was not ended. `live` names the transitive refusal set. */
export type KillTurnCause =
  | { readonly kind: "live"; readonly live: conversationv1.TurnLive }
  | { readonly kind: "notTheOpenTurn" }
  | { readonly kind: "noTurnOpen" }
  | { readonly kind: "noSession" };

/** The base constructor for `shim.v1.KillTurnFailure`. */
export function killTurnFailure(cause: KillTurnCause, detail: string): shimv1.KillTurnFailure {
  return create(shimv1.KillTurnFailureSchema, {
    detail,
    cause:
      cause.kind === "live"
        ? { case: "live", value: cause.live }
        : cause.kind === "notTheOpenTurn"
          ? { case: "notTheOpenTurn", value: create(shimv1.KillTurnNotTheOpenTurnSchema, {}) }
          : cause.kind === "noTurnOpen"
            ? { case: "noTurnOpen", value: create(shimv1.KillTurnNoTurnOpenSchema, {}) }
            : { case: "noSession", value: create(shimv1.KillTurnNoSessionSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function killTurnRefused(cause: KillTurnCause, detail: string): shimv1.KillTurnResponse {
  return create(shimv1.KillTurnResponseSchema, {
    result: { case: "failure", value: killTurnFailure(cause, detail) },
  });
}

/** The turn ended; `killed` says whether anything died with it. */
export function killTurnKilled(killed: conversationv1.TurnKilled): shimv1.KillTurnResponse {
  return create(shimv1.KillTurnResponseSchema, {
    result: { case: "success", value: create(shimv1.KillTurnSuccessSchema, { killed }) },
  });
}

// ---------------------------------------------------------------------------
// StopBash
// ---------------------------------------------------------------------------

/** Why the backgrounded shell was not stopped. */
export type StopBashKind = { readonly kind: "unknownWork" } | { readonly kind: "alreadyEnded" };

/** The base constructor for `shim.v1.StopBashFailure`. */
export function stopBashFailure(cause: StopBashKind, detail: string): shimv1.StopBashFailure {
  return create(shimv1.StopBashFailureSchema, {
    detail,
    kind:
      cause.kind === "unknownWork"
        ? { case: "unknownWork", value: create(shimv1.StopBashUnknownWorkSchema, {}) }
        : { case: "alreadyEnded", value: create(shimv1.StopBashAlreadyEndedSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function stopBashRefused(cause: StopBashKind, detail: string): shimv1.StopBashResponse {
  return create(shimv1.StopBashResponseSchema, {
    result: { case: "failure", value: stopBashFailure(cause, detail) },
  });
}

/** Stopped. The run's own terminal arrives on its WatchBash stream. */
export function stopBashStopped(): shimv1.StopBashResponse {
  return create(shimv1.StopBashResponseSchema, {
    result: { case: "success", value: create(shimv1.StopBashSuccessSchema, {}) },
  });
}

// ---------------------------------------------------------------------------
// DetachForeground
// ---------------------------------------------------------------------------

/** Why the in-flight unit could not be moved onto its own stream. */
export type DetachForegroundKind =
  | { readonly kind: "unknownUnit" }
  | { readonly kind: "alreadyConcluded" }
  | { readonly kind: "notDetachable" }
  | { readonly kind: "noSession" };

/** The base constructor for `shim.v1.DetachForegroundFailure`. */
export function detachForegroundFailure(
  cause: DetachForegroundKind,
  detail: string,
): shimv1.DetachForegroundFailure {
  return create(shimv1.DetachForegroundFailureSchema, {
    detail,
    kind:
      cause.kind === "unknownUnit"
        ? { case: "unknownUnit", value: create(shimv1.DetachForegroundUnknownUnitSchema, {}) }
        : cause.kind === "alreadyConcluded"
          ? { case: "alreadyConcluded", value: create(shimv1.DetachForegroundAlreadyConcludedSchema, {}) }
          : cause.kind === "notDetachable"
            ? { case: "notDetachable", value: create(shimv1.DetachForegroundNotDetachableSchema, {}) }
            : { case: "noSession", value: create(shimv1.DetachForegroundNoSessionSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function detachForegroundRefused(
  cause: DetachForegroundKind,
  detail: string,
): shimv1.DetachForegroundResponse {
  return create(shimv1.DetachForegroundResponseSchema, {
    result: { case: "failure", value: detachForegroundFailure(cause, detail) },
  });
}

/** Detached. The turn now announces it and the consumer opens the Watch. */
export function detachForegroundDetached(): shimv1.DetachForegroundResponse {
  return create(shimv1.DetachForegroundResponseSchema, {
    result: { case: "success", value: create(shimv1.DetachForegroundSuccessSchema, {}) },
  });
}

// ---------------------------------------------------------------------------
// ReadHistory
// ---------------------------------------------------------------------------

/** Why a page of history could not be served. */
export type ReadHistoryKind =
  | { readonly kind: "unknownAgent" }
  | { readonly kind: "stalePointer" }
  | { readonly kind: "storeUnavailable" };

/** The base constructor for `shim.v1.ReadHistoryFailure`. */
export function readHistoryFailure(
  cause: ReadHistoryKind,
  detail: string,
): shimv1.ReadHistoryFailure {
  return create(shimv1.ReadHistoryFailureSchema, {
    detail,
    kind:
      cause.kind === "unknownAgent"
        ? { case: "unknownAgent", value: create(shimv1.ReadHistoryUnknownAgentSchema, {}) }
        : cause.kind === "stalePointer"
          ? { case: "stalePointer", value: create(shimv1.ReadHistoryStalePointerSchema, {}) }
          : { case: "storeUnavailable", value: create(shimv1.ReadHistoryStoreUnavailableSchema, {}) },
  });
}

/** The refusal as the whole response the handler returns. */
export function readHistoryRefused(
  cause: ReadHistoryKind,
  detail: string,
): shimv1.ReadHistoryResponse {
  return create(shimv1.ReadHistoryResponseSchema, {
    result: { case: "failure", value: readHistoryFailure(cause, detail) },
  });
}

/** One page of one agent's durable past. */
export function readHistoryPage(page: conversationv1.HistoryPage): shimv1.ReadHistoryResponse {
  return create(shimv1.ReadHistoryResponseSchema, {
    result: { case: "success", value: create(shimv1.ReadHistorySuccessSchema, { page }) },
  });
}

// ---------------------------------------------------------------------------
// SessionFault — the shim reporting its OWN degradation
// ---------------------------------------------------------------------------

/**
 * What broke inside the shim.
 *
 * NOT a refusal of anything: a fault rides WatchSession as a diagnostics
 * change, so the daemon learns the shim is degraded even when no rpc failed.
 */
export type SessionFaultKind =
  | { readonly kind: "storeUnreachable" }
  | { readonly kind: "converterDefect" }
  | { readonly kind: "logSinkPoisoned" }
  | { readonly kind: "keepaliveFailed" }
  | { readonly kind: "vendorQueryFailed" };

/** The base constructor for `conversation.v1.SessionFault`. */
export function sessionFault(
  cause: SessionFaultKind,
  component: string,
  detail: string,
): conversationv1.SessionFault {
  return create(conversationv1.SessionFaultSchema, {
    component,
    detail,
    kind:
      cause.kind === "storeUnreachable"
        ? { case: "storeUnreachable", value: create(conversationv1.SessionFaultStoreUnreachableSchema, {}) }
        : cause.kind === "converterDefect"
          ? { case: "converterDefect", value: create(conversationv1.SessionFaultConverterDefectSchema, {}) }
          : cause.kind === "logSinkPoisoned"
            ? { case: "logSinkPoisoned", value: create(conversationv1.SessionFaultLogSinkPoisonedSchema, {}) }
            : cause.kind === "keepaliveFailed"
              ? { case: "keepaliveFailed", value: create(conversationv1.SessionFaultKeepaliveFailedSchema, {}) }
              : { case: "vendorQueryFailed", value: create(conversationv1.SessionFaultVendorQueryFailedSchema, {}) },
  });
}

// ---------------------------------------------------------------------------
// Transport-level refusals
// ---------------------------------------------------------------------------

/**
 * A request the shim cannot answer because it is not a legal request.
 *
 * There is deliberately no typed failure for this: a response message would
 * have to be built from a request the shim could not read, and answering
 * "success: false" to a malformed request teaches the caller that its message
 * was understood.
 */
export function invalidArgument(detail: string): ConnectError {
  return new ConnectError(detail, Code.InvalidArgument);
}

/**
 * A refused STREAM open: an unknown watch target, a work id nobody knows, a
 * store token the store has forgotten.
 *
 * A stream has no failure message to carry a refusal — its response type is
 * the frame it streams — so the refusal closes the stream at the transport.
 */
export function notFound(detail: string): ConnectError {
  return new ConnectError(detail, Code.NotFound);
}

/**
 * WORKFLOW IS KICKED (ruled 2026-08-29). The three workflow verbs stay in the
 * contract and answer this until the wave that implements them.
 *
 * `Unimplemented` and not an empty success: a caller that receives an empty
 * workflow cannot tell "no workflows" from "this shim does not do workflows",
 * and would render the difference as an absence rather than as a gap.
 */
export function unimplemented(rpc: string): ConnectError {
  return new ConnectError(
    `shim.v1.${rpc} is not implemented in this wave (workflow is kicked); the verb exists in the contract and answers Unimplemented until it is`,
    Code.Unimplemented,
  );
}
