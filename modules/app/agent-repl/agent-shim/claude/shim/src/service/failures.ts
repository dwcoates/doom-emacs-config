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
import type { TitleDigest } from "../convert/title-digest.js";
import type { LockHolderHow } from "../locks.js";
import {
  TRANSCRIPT_QUIET_AFTER_MS,
  type TranscriptSummary,
  type TranscriptsRead,
} from "../engine/transcripts.js";

// ---------------------------------------------------------------------------
// StartSession
// ---------------------------------------------------------------------------

/**
 * Why a session could not be started.
 *
 * `cold` carries evidence because the daemon needs the context cost and the
 * reason to offer the user a remediation, so a bare "it was cold" is
 * unactionable. `lockHolderUnavailable` carries the binary and how it failed,
 * because "our own lock helper failed" is fixed by fixing THAT binary, and
 * the reader must not have to dig it out of prose.
 */
type StartSessionCause =
  | { readonly kind: "cold"; readonly cold: conversationv1.SessionCold }
  | { readonly kind: "vendorStartFailed" }
  | { readonly kind: "unknownSession" }
  | { readonly kind: "alreadyStarted" }
  | { readonly kind: "conversationOwned" }
  | { readonly kind: "lockHolderUnavailable"; readonly binary: string; readonly how: LockHolderHow };

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
              : cause.kind === "lockHolderUnavailable"
                ? {
                    case: "lockHolderUnavailable",
                    value: create(shimv1.StartSessionLockHolderUnavailableSchema, {
                      failure: lockHolderFailure(cause.binary, cause.how),
                    }),
                  }
                : { case: "conversationOwned", value: create(shimv1.StartSessionConversationOwnedSchema, {}) },
  });
}

/** The base constructor for `conversation.v1.LockHolderFailure`. */
export function lockHolderFailure(binary: string, how: LockHolderHow): conversationv1.LockHolderFailure {
  return create(conversationv1.LockHolderFailureSchema, { binary, how: lockHolderHowArm(how) });
}

/** The `how` arm for one failure. */
function lockHolderHowArm(how: LockHolderHow): conversationv1.LockHolderFailure["how"] {
  switch (how.kind) {
    case "spawnFailed":
      return { case: "spawnFailed", value: create(conversationv1.LockHolderSpawnFailedSchema, { osError: how.osError }) };
    case "exited":
      return { case: "exited", value: create(conversationv1.LockHolderExitedSchema, { code: how.code, stderr: how.stderr }) };
    case "signaled":
      return { case: "signaled", value: create(conversationv1.LockHolderSignaledSchema, { signal: how.signal, stderr: how.stderr }) };
    case "misanswered":
      return { case: "misanswered", value: create(conversationv1.LockHolderMisansweredSchema, { line: how.line }) };
    case "silent":
      return { case: "silent", value: create(conversationv1.LockHolderSilentSchema, { timeoutMs: how.timeoutMs }) };
  }
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
type SetSessionModelCause =
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
type SetSessionPermissionModeKind =
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
type HibernateErrorKind =
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

/**
 * `shim.v1.HibernateResponse` — the compaction has STARTED and outlives this
 * rpc.
 *
 * NEITHER AN ACK NOR A REFUSAL. The caller's deadline is seconds and a
 * compaction is a real vendor turn, so the answer cannot be the work's
 * completion: it is the fact that the work is under way. The caller defers this
 * pass and asks again, and the next ask acks at once because a transcript
 * already compacted is never compacted twice.
 */
export function hibernateCompacting(): shimv1.HibernateResponse {
  return create(shimv1.HibernateResponseSchema, {
    result: { case: "compacting", value: create(shimv1.HibernateCompactingSchema, {}) },
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
type KillSessionCause =
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
type StartTurnKind =
  // Always the daemon's own double-submit: a StartTurn arriving during the
  // shim's keep-alive waits for it inside the shim and never lands here.
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
        ? {
            case: "turnAlreadyOpen",
            value: create(shimv1.StartTurnTurnAlreadyOpenSchema, {}),
          }
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

/**
 * The prompt was delivered; the record that comes back IS the turn's prompt.
 *
 * ONE CALL SUBMITS AND PAINTS, so the opening page rides the answer. The page is
 * REQUIRED: an empty page is a page with no entries and a `floor`, never an
 * absent one — a consumer handed no page cannot tell "nothing to paint" from
 * "the shim forgot", and would draw a blank feed either way.
 */
export function startTurnAccepted(
  prompt: conversationv1.AgentPrompt,
  page: conversationv1.HistoryPage,
): shimv1.StartTurnResponse {
  return create(shimv1.StartTurnResponseSchema, {
    result: { case: "success", value: create(shimv1.StartTurnSuccessSchema, { prompt, page }) },
  });
}

/**
 * The page a StartTurn answers with when the record could not be read.
 *
 * A REFUSAL IS NOT AN OPTION HERE: the prompt is already durable and already
 * delivered by the time the page is read, so answering `failure` would tell the
 * daemon a turn did not start that is running. An empty page with a `floor` is
 * the honest shape — "there is nothing to paint from here" — and the caller's
 * own WatchAgent tail carries everything the turn produces.
 */
export function emptyOpeningPage(): conversationv1.HistoryPage {
  return create(conversationv1.HistoryPageSchema, {
    boundary: { case: "floor", value: create(conversationv1.HistoryFloorSchema, {}) },
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
type UpdateAgentKind =
  | { readonly kind: "unknownAgent" }
  | { readonly kind: "noOpenAsk" }
  | { readonly kind: "answerMismatch" }
  | { readonly kind: "nothingRunning" }
  | { readonly kind: "noSession" }
  /**
   * The input is well-formed and the agent is known, but the VENDOR OFFERS NO
   * ROUTE to deliver it to this agent kind.
   *
   * NOT `nothingRunning`, which is a claim about the agent's STATE and would
   * send a caller looking for a live agent that was live all along. The same
   * input to the main agent would deliver; the gap is the SDK's.
   */
  | { readonly kind: "notDeliverable" }
  /**
   * A PROMPT reached a subagent whose OWN TURN IS ALREADY RUNNING.
   *
   * A fact about the agent's state, distinct from `notDeliverable`: the route
   * question never arises, because the addressee is busy. The daemon relays it
   * as `SubmitPromptError.bubble_refused{agent_busy}` — it is the one producer
   * of that relay (landing 7, 2026-09-02).
   */
  | { readonly kind: "agentBusy" };

/** The base constructor for `shim.v1.UpdateAgentFailure`. */
export function updateAgentFailure(
  cause: UpdateAgentKind,
  detail: string,
): shimv1.UpdateAgentFailure {
  return create(shimv1.UpdateAgentFailureSchema, {
    detail,
    kind:
      cause.kind === "agentBusy"
        ? { case: "agentBusy", value: create(shimv1.UpdateAgentAgentBusySchema, {}) }
        : cause.kind === "unknownAgent"
        ? { case: "unknownAgent", value: create(shimv1.UpdateAgentUnknownAgentSchema, {}) }
        : cause.kind === "noOpenAsk"
          ? { case: "noOpenAsk", value: create(shimv1.UpdateAgentNoOpenAskSchema, {}) }
          : cause.kind === "answerMismatch"
            ? { case: "answerMismatch", value: create(shimv1.UpdateAgentAnswerMismatchSchema, {}) }
            : cause.kind === "nothingRunning"
              ? { case: "nothingRunning", value: create(shimv1.UpdateAgentNothingRunningSchema, {}) }
              : cause.kind === "noSession"
                ? { case: "noSession", value: create(shimv1.UpdateAgentNoSessionSchema, {}) }
                : {
                    case: "notDeliverable",
                    value: create(shimv1.UpdateAgentNotDeliverableSchema, {}),
                  },
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
type KillTurnCause =
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
type StopBashKind = { readonly kind: "unknownWork" } | { readonly kind: "alreadyEnded" };

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
type DetachForegroundKind =
  | { readonly kind: "unknownUnit" }
  | { readonly kind: "alreadyConcluded" }
  | { readonly kind: "notDetachable" }
  | { readonly kind: "noSession" }
  /**
   * The unit is detachable IN KIND and still in flight, but the pinned SDK
   * offers NO VERB to initiate a detachment.
   *
   * NOT `notDetachable`, which says the unit's KIND cannot detach — a claim
   * that is false for a shell or a subagent and would tell a caller to stop
   * offering the affordance for work that detaches on its own all the time.
   * The shim can only OBSERVE detachments the vendor made.
   */
  | { readonly kind: "unsupported" };

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
            : cause.kind === "noSession"
              ? { case: "noSession", value: create(shimv1.DetachForegroundNoSessionSchema, {}) }
              : {
                  case: "unsupported",
                  value: create(shimv1.DetachForegroundUnsupportedSchema, {}),
                },
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
type ReadHistoryKind =
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
// GatherTitleDigest
// ---------------------------------------------------------------------------

/** Why a title digest could not be gathered. */
type TitleDigestKind =
  | { readonly kind: "noTranscript" }
  | { readonly kind: "unreadable" };

/** The proto boundary enum the digest's boundary maps to. */
function titleDigestBoundary(boundary: TitleDigest["boundary"]): shimv1.TitleDigestBoundary {
  switch (boundary) {
    case "none":
      return shimv1.TitleDigestBoundary.NONE;
    case "clear":
      return shimv1.TitleDigestBoundary.CLEAR;
    case "compact":
      return shimv1.TitleDigestBoundary.COMPACT;
  }
}

/** The digest as the whole response the handler returns. */
export function titleDigestGathered(digest: TitleDigest): shimv1.GatherTitleDigestResponse {
  return create(shimv1.GatherTitleDigestResponseSchema, {
    result: {
      case: "success",
      value: create(shimv1.GatherTitleDigestSuccessSchema, {
        boundary: titleDigestBoundary(digest.boundary),
        // `lastCompactSummary` is optional-by-presence: it is set only for a
        // compaction boundary, matching the proto's "SET ONLY when COMPACT".
        lastCompactSummary: digest.lastCompactSummary,
        prompts: digest.prompts,
      }),
    },
  });
}

/** The refusal as the whole response the handler returns. */
export function titleDigestRefused(
  cause: TitleDigestKind,
  detail: string,
): shimv1.GatherTitleDigestResponse {
  return create(shimv1.GatherTitleDigestResponseSchema, {
    result: {
      case: "failure",
      value: create(shimv1.GatherTitleDigestFailureSchema, {
        detail,
        kind:
          cause.kind === "noTranscript"
            ? { case: "noTranscript", value: create(shimv1.GatherTitleDigestNoTranscriptSchema, {}) }
            : { case: "unreadable", value: create(shimv1.GatherTitleDigestUnreadableSchema, {}) },
      }),
    },
  });
}

// ---------------------------------------------------------------------------
// ReadTranscripts
// ---------------------------------------------------------------------------

/** One conversation as the wire states it, built from what its file stated. */
function transcriptMessage(summary: TranscriptSummary): shimv1.Transcript {
  const facts = summary.facts;
  return create(shimv1.TranscriptSchema, {
    vendorSessionId: summary.vendorSessionId,
    // EVERY OPTIONAL FIELD IS LEFT UNSET WHEN THE TRANSCRIPT STATES NOTHING.
    // A zero that cannot be told from an absence is how a chooser ends up
    // ranking an unreadable conversation as the smallest one.
    lastRequestAtMs: facts.lastRequestAtMs === 0 ? undefined : BigInt(facts.lastRequestAtMs),
    contextTokens: facts.sawUsage ? BigInt(facts.contextTokens) : undefined,
    lastModel:
      facts.lastModel === undefined
        ? undefined
        : create(conversationv1.AgentModelSchema, { name: facts.lastModel }),
    opening: facts.opening,
    prompts: facts.prompts,
    bound: summary.bound ? create(shimv1.TranscriptBoundSchema, {}) : undefined,
    cleared:
      facts.clearedAtMs === undefined
        ? undefined
        : create(shimv1.TranscriptClearedSchema, { atMs: BigInt(facts.clearedAtMs) }),
    active:
      summary.activeAtMs === undefined
        ? undefined
        : create(shimv1.TranscriptActiveSchema, {
            atMs: BigInt(summary.activeAtMs),
            quietAfterMs: BigInt(TRANSCRIPT_QUIET_AFTER_MS),
          }),
  });
}

/** The directory's conversations as the whole response the handler returns. */
export function transcriptsRead(
  transcripts: readonly TranscriptSummary[],
): shimv1.ReadTranscriptsResponse {
  return create(shimv1.ReadTranscriptsResponseSchema, {
    result: {
      case: "success",
      value: create(shimv1.ReadTranscriptsSuccessSchema, {
        transcripts: transcripts.map(transcriptMessage),
      }),
    },
  });
}

/** The refusal as the whole response the handler returns. */
export function transcriptsRefused(
  read: Extract<TranscriptsRead, { kind: "no_project_dir" | "unreadable" }>,
): shimv1.ReadTranscriptsResponse {
  return create(shimv1.ReadTranscriptsResponseSchema, {
    result: {
      case: "failure",
      value: create(shimv1.ReadTranscriptsFailureSchema, {
        cause:
          read.kind === "no_project_dir"
            ? {
                case: "noProjectDir",
                value: create(shimv1.ReadTranscriptsNoProjectDirSchema, {
                  searchedPath: read.searchedPath,
                }),
              }
            : {
                case: "unreadable",
                value: create(shimv1.ReadTranscriptsUnreadableSchema, {
                  searchedPath: read.searchedPath,
                  detail: read.detail,
                }),
              },
      }),
    },
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
type SessionFaultKind =
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

/**
 * An exception no handler anticipated.
 *
 * A `ConnectError` is a refusal the shim MEANT to make and passes through
 * unchanged; anything else is a defect, and answering a bare `internal error`
 * with no detail loses the one piece of evidence the caller could act on. So
 * the detail names the rpc and carries the exception's own message, and the
 * caller is never handed a silent stream close.
 */
export function internalFromUnknown(rpc: string, error: unknown): ConnectError {
  if (error instanceof ConnectError) return error;
  const detail = error instanceof Error ? error.message : String(error);
  return new ConnectError(`shim.v1.${rpc}: ${detail}`, Code.Internal);
}
