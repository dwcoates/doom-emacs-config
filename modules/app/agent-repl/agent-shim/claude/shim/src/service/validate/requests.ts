/**
 * service/validate/requests.ts — ONE base validate function per shim.v1 request
 * message.
 *
 * Each is called by its handler BEFORE the engine is touched, so an illegal
 * request never reaches the session. Every one delegates its message-typed
 * fields and oneof arms to `fields.ts`; nothing is validated twice and nothing
 * is validated nowhere.
 *
 * The workflow trio has no validator on purpose: `routes.ts` answers those
 * three `Unimplemented` before reading their requests, and validating a request
 * for a verb that does not exist would only pretend it does.
 */
import type { shimv1 } from "../../proto.js";
import { bindLog } from "../../log.js";
import { invalidArgument } from "../failures.js";
import {
  validateAgentActivityId,
  validateAgentId,
  validateAgentInput,
  validateAgentModel,
  validateAgentPermissionMode,
  validateDetachedWorkId,
  validateConversationThrough,
  validateHistoryPointer,
  validatePageSize,
  validatePromptOrigin,
  validateSessionColdRemediation,
  validateTurnId,
  validateUserSaid,
  unsetOneof,
} from "./fields.js";

const LOGGER = bindLog({ component: "shim-validate", operation: "shim.service.validate" });

/** Record a refusal once, at the boundary that made it, then rethrow. */
function refuse(rpc: string, err: unknown): never {
  LOGGER.debug(
    { rpc, cause: err },
    `refused a ${rpc} request that violates the validation invariant`,
  );
  throw err;
}

/** `shim.v1.StartSessionRequest` — fresh or resume, never neither. */
export function validateStartSessionRequest(request: shimv1.StartSessionRequest): void {
  try {
    switch (request.source.case) {
      case "fresh": {
        const fresh = request.source.value;
        // OPTIONAL SINCE LANDING 7: an UNSET model means "the SDK's own
        // default", and SessionStarted.effective_model states what took
        // effect. A model that IS named still has to be named properly — an
        // empty name is a caller that meant to say something and said nothing,
        // which is not the same request as saying nothing at all.
        if (fresh.model !== undefined) {
          validateAgentModel(fresh.model, "start_session.fresh.model");
        }
        validateAgentPermissionMode(fresh.permissionMode, "start_session.fresh.permission_mode");
        return;
      }
      case "resume": {
        const resume = request.source.value;
        if (resume.vendorSessionId === "") {
          throw unsetOneof("start_session.resume.vendor_session_id");
        }
        if (resume.coldRemediation !== undefined) {
          validateSessionColdRemediation(
            resume.coldRemediation,
            "start_session.resume.cold_remediation",
          );
        }
        return;
      }
      default:
        throw unsetOneof("start_session.source");
    }
  } catch (err) {
    refuse("StartSession", err);
  }
}

/**
 * `shim.v1.WatchSessionRequest` — empty by design.
 *
 * The function exists anyway: the mapping convention is one base function per
 * message, and an empty message that acquires a field later must acquire its
 * validation at a site that already exists rather than at a site someone has to
 * remember to create.
 */
export function validateWatchSessionRequest(_request: shimv1.WatchSessionRequest): void {
  // No fields. Nothing to refuse.
}

/** `shim.v1.SetSessionModelRequest` — the model, the cold threshold, the remedy. */
export function validateSetSessionModelRequest(request: shimv1.SetSessionModelRequest): void {
  try {
    validateAgentModel(request.model, "set_session_model.model");
    if (request.coldRemediation !== undefined) {
      validateSessionColdRemediation(
        request.coldRemediation,
        "set_session_model.cold_remediation",
      );
    }
  } catch (err) {
    refuse("SetSessionModel", err);
  }
}

/** `shim.v1.SetSessionPermissionModeRequest` — the mode, as a set oneof arm. */
export function validateSetSessionPermissionModeRequest(
  request: shimv1.SetSessionPermissionModeRequest,
): void {
  try {
    validateAgentPermissionMode(
      request.permissionMode,
      "set_session_permission_mode.permission_mode",
    );
  } catch (err) {
    refuse("SetSessionPermissionMode", err);
  }
}

/** `shim.v1.HibernateRequest` — empty by design; see validateWatchSessionRequest. */
export function validateHibernateRequest(_request: shimv1.HibernateRequest): void {
  // No fields. Nothing to refuse.
}

/** `shim.v1.KillSessionRequest` — `force` is a primitive with a legal false. */
export function validateKillSessionRequest(_request: shimv1.KillSessionRequest): void {
  // `force` is a bool: false is a legitimate value, not an absence.
}

/** `shim.v1.StartTurnRequest` — the turn, what was said, why, and the page budget. */
export function validateStartTurnRequest(request: shimv1.StartTurnRequest): void {
  try {
    validateTurnId(request.turn, "start_turn.turn");
    validateUserSaid(request.said, "start_turn.said");
    validatePromptOrigin(request.origin, "start_turn.origin");
    validatePageSize(request.pageSize, "start_turn.page_size");
    if (request.knownThrough !== undefined) {
      validateHistoryPointer(request.knownThrough, "start_turn.known_through");
    }
  } catch (err) {
    refuse("StartTurn", err);
  }
}

/**
 * `shim.v1.WatchAgentRequest` — an UNSET target is legal and means the
 * session's prompt thread, which the shim resolves.
 */
export function validateWatchAgentRequest(request: shimv1.WatchAgentRequest): void {
  try {
    if (request.target !== undefined) validateAgentId(request.target, "watch_agent.target");
    validatePageSize(request.pageSize, "watch_agent.page_size");
    if (request.knownThrough !== undefined) {
      validateHistoryPointer(request.knownThrough, "watch_agent.known_through");
    }
  } catch (err) {
    refuse("WatchAgent", err);
  }
}

/** `shim.v1.UpdateAgentRequest` — an unset target addresses the main agent. */
export function validateUpdateAgentRequest(request: shimv1.UpdateAgentRequest): void {
  try {
    if (request.target !== undefined) validateAgentId(request.target, "update_agent.target");
    validateAgentInput(request.input, "update_agent.input");
  } catch (err) {
    refuse("UpdateAgent", err);
  }
}

/** `shim.v1.KillTurnRequest` — which turn, and whether to force. */
export function validateKillTurnRequest(request: shimv1.KillTurnRequest): void {
  try {
    validateTurnId(request.turn, "kill_turn.turn");
  } catch (err) {
    refuse("KillTurn", err);
  }
}

/**
 * `shim.v1.RollBackSessionRequest` — the turn cut before, every dropped turn
 * (`to_before` among them), and keep-or-restore, never neither.
 */
export function validateRollBackSessionRequest(request: shimv1.RollBackSessionRequest): void {
  try {
    validateTurnId(request.toBefore, "roll_back_session.to_before");
    request.droppedTurns.forEach((turn, index) => {
      validateTurnId(turn, `roll_back_session.dropped_turns[${index}]`);
    });
    if (request.droppedTurns[0]?.value !== request.toBefore?.value) {
      throw invalidArgument(
        "roll_back_session.dropped_turns must begin with to_before: the turns are drawn from it onward, oldest first",
      );
    }
    if (request.files.case === undefined) throw unsetOneof("roll_back_session.files");
  } catch (err) {
    refuse("RollBackSession", err);
  }
}

/** `shim.v1.WatchBashRequest` — which backgrounded shell. */
export function validateWatchBashRequest(request: shimv1.WatchBashRequest): void {
  try {
    validateDetachedWorkId(request.work, "watch_bash.work");
  } catch (err) {
    refuse("WatchBash", err);
  }
}

/** `shim.v1.StopBashRequest` — which backgrounded shell. */
export function validateStopBashRequest(request: shimv1.StopBashRequest): void {
  try {
    validateDetachedWorkId(request.work, "stop_bash.work");
  } catch (err) {
    refuse("StopBash", err);
  }
}

/** `shim.v1.DetachForegroundRequest` — which in-flight unit leaves the turn. */
export function validateDetachForegroundRequest(request: shimv1.DetachForegroundRequest): void {
  try {
    validateAgentActivityId(request.unit, "detach_foreground.unit");
  } catch (err) {
    refuse("DetachForeground", err);
  }
}

/**
 * `shim.v1.GatherTitleDigestRequest` — the request carries no fields (the shim
 * resolves its own transcript), so there is nothing to reject. The validator
 * exists so `routes.ts` treats this verb exactly like every other: validate,
 * then delegate.
 */
export function validateGatherTitleDigestRequest(_request: shimv1.GatherTitleDigestRequest): void {
  // Intentionally empty: an empty message has no illegal shape.
}

/**
 * `shim.v1.ReadTranscriptsRequest` — the request carries no fields (the shim
 * resolves the directory from its own identity), so there is nothing to
 * reject. The validator exists so `routes.ts` treats this verb exactly like
 * every other: validate, then delegate.
 */
export function validateReadTranscriptsRequest(_request: shimv1.ReadTranscriptsRequest): void {
  // Intentionally empty: an empty message has no illegal shape.
}

/** `shim.v1.ReadHistoryRequest` — whose history, how much, and from where. */
export function validateReadHistoryRequest(request: shimv1.ReadHistoryRequest): void {
  try {
    if (request.target !== undefined) validateAgentId(request.target, "read_history.target");
    validatePageSize(request.pageSize, "read_history.page_size");
    switch (request.position.case) {
      case "first":
        return;
      case "after":
        validateHistoryPointer(request.position.value, "read_history.after");
        return;
      case "through":
        validateConversationThrough(request.position.value, "read_history.through");
        return;
      default:
        throw unsetOneof("read_history.position");
    }
  } catch (err) {
    refuse("ReadHistory", err);
  }
}
