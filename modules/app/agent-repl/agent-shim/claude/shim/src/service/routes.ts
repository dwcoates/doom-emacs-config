/**
 * service/routes.ts — the shim.v1 service implementation: one handler per rpc.
 *
 * A handler does exactly three things, in this order, and nothing else:
 *
 *   1. VALIDATE the request (`validate/`), which throws `InvalidArgument` on
 *      anything illegal, so the engine never sees a malformed message;
 *   2. DELEGATE to exactly one {@link Engine} method;
 *   3. YIELD what comes back, unchanged.
 *
 * No handler builds a response of its own, and no handler contains a branch on
 * session state — that lives in the engine, which is the only thing that knows
 * it. The single exception is the workflow trio, which has no engine method at
 * all (workflow is kicked) and answers `Unimplemented` from here.
 *
 * Streaming handlers yield the engine's iterable through. A refused OPEN
 * surfaces as the ConnectError the engine threw, which the transport turns into
 * the stream's error — the only way a stream can express a refusal, since its
 * response type is the frame it streams.
 */
import type { ConnectRouter } from "@connectrpc/connect";
import { bindLog } from "../log.js";
import type { Engine } from "../engine/engine.js";
import { shimv1 } from "../proto.js";
import { internalFromUnknown, unimplemented } from "./failures.js";
import {
  validateDetachForegroundRequest,
  validateHibernateRequest,
  validateKillSessionRequest,
  validateKillTurnRequest,
  validateReadHistoryRequest,
  validateSetSessionModelRequest,
  validateSetSessionPermissionModeRequest,
  validateStartSessionRequest,
  validateStartTurnRequest,
  validateStopBashRequest,
  validateUpdateAgentRequest,
  validateWatchAgentRequest,
  validateWatchBashRequest,
  validateWatchSessionRequest,
} from "./validate/requests.js";

const LOGGER = bindLog({ component: "shim-routes", operation: "shim.service.route" });

/** Record that a verb was entered, before anything can refuse it. */
function entered(rpc: string): void {
  LOGGER.debug({ rpc, boundary: "entered" }, `serving shim.v1.${rpc}`);
}

/**
 * Answer one unary rpc, mapping an UNANTICIPATED exception to a logged
 * `Internal` that carries its detail.
 *
 * A `ConnectError` is a refusal the shim meant to make and travels unchanged.
 * Anything else is a defect: without this the transport answers a bare
 * `[internal] internal error` and the evidence dies inside the process.
 */
async function answering<T>(rpc: string, act: () => Promise<T>): Promise<T> {
  try {
    const response = await act();
    LOGGER.debug({ rpc, boundary: "completed" }, `completed shim.v1.${rpc}`);
    return response;
  } catch (error) {
    throw reportUnhandled(rpc, error);
  }
}

/**
 * Serve one server-stream rpc under the same rule.
 *
 * A stream's response type is the frame it carries, so a refusal — anticipated
 * or not — can only reach the caller as the stream's error. Letting an
 * unanticipated one through unmapped closes the stream with no detail at all.
 */
async function* streaming<T>(rpc: string, frames: () => AsyncIterable<T>): AsyncIterable<T> {
  try {
    yield* frames();
    LOGGER.debug({ rpc, boundary: "completed" }, `completed shim.v1.${rpc} stream`);
  } catch (error) {
    throw reportUnhandled(rpc, error);
  }
}

/** Log an unanticipated exception once, at the boundary, and type it. */
function reportUnhandled(rpc: string, error: unknown): unknown {
  const mapped = internalFromUnknown(rpc, error);
  if (mapped !== error) {
    LOGGER.error(
      {
        rpc,
        detail: mapped.rawMessage,
        stack: error instanceof Error ? error.stack : undefined,
      },
      `shim.v1.${rpc} threw an exception no handler anticipated`,
    );
  }
  return mapped;
}

/**
 * Register every shim.v1 handler against one engine.
 *
 * Returns the router callback `connectNodeAdapter` consumes, so the wiring is
 * testable without a socket: a suite can build a router transport over this and
 * drive all seventeen verbs in-process.
 */
export function shimRoutes(engine: Engine): (router: ConnectRouter) => void {
  return (router: ConnectRouter): void => {
    router.service(shimv1.Shim, {
      // ---- Session ----

      async startSession(request) {
        entered("StartSession");
        validateStartSessionRequest(request);
        return answering("StartSession", () => engine.startSession(request));
      },

      async *watchSession(request) {
        entered("WatchSession");
        validateWatchSessionRequest(request);
        yield* streaming("WatchSession", () => engine.watchSession(request));
      },

      async setSessionModel(request) {
        entered("SetSessionModel");
        validateSetSessionModelRequest(request);
        return answering("SetSessionModel", () => engine.setSessionModel(request));
      },

      async setSessionPermissionMode(request) {
        entered("SetSessionPermissionMode");
        validateSetSessionPermissionModeRequest(request);
        return answering("SetSessionPermissionMode", () => engine.setSessionPermissionMode(request));
      },

      async hibernate(request) {
        entered("Hibernate");
        validateHibernateRequest(request);
        return answering("Hibernate", () => engine.hibernate(request));
      },

      async killSession(request) {
        entered("KillSession");
        validateKillSessionRequest(request);
        return answering("KillSession", () => engine.killSession(request));
      },

      // ---- Agent ----

      async startTurn(request) {
        entered("StartTurn");
        validateStartTurnRequest(request);
        return answering("StartTurn", () => engine.startTurn(request));
      },

      async *watchAgent(request) {
        entered("WatchAgent");
        validateWatchAgentRequest(request);
        yield* streaming("WatchAgent", () => engine.watchAgent(request));
      },

      async updateAgent(request) {
        entered("UpdateAgent");
        validateUpdateAgentRequest(request);
        return answering("UpdateAgent", () => engine.updateAgent(request));
      },

      async killTurn(request) {
        entered("KillTurn");
        validateKillTurnRequest(request);
        return answering("KillTurn", () => engine.killTurn(request));
      },

      // ---- Detached work ----

      async *watchBash(request) {
        entered("WatchBash");
        validateWatchBashRequest(request);
        yield* streaming("WatchBash", () => engine.watchBash(request));
      },

      async stopBash(request) {
        entered("StopBash");
        validateStopBashRequest(request);
        return answering("StopBash", () => engine.stopBash(request));
      },

      async detachForeground(request) {
        entered("DetachForeground");
        validateDetachForegroundRequest(request);
        return answering("DetachForeground", () => engine.detachForeground(request));
      },

      // ---- Workflow: IN the contract, NOT in this wave ----

      /**
       * WORKFLOW IS KICKED (ruled 2026-08-29). The three verbs answer
       * `Unimplemented` WITHOUT consulting an engine — there is no engine
       * method for them, so there is nothing to accidentally half-implement.
       * The request is not even validated: validating a verb that does not
       * exist would pretend it does.
       */
      getWorkflow() {
        entered("GetWorkflow");
        LOGGER.debug({ rpc: "GetWorkflow" }, "refused a workflow verb: workflow is not implemented in this wave");
        throw unimplemented("GetWorkflow");
      },

      // eslint-disable-next-line require-yield
      async *watchWorkflow() {
        entered("WatchWorkflow");
        LOGGER.debug({ rpc: "WatchWorkflow" }, "refused a workflow verb: workflow is not implemented in this wave");
        throw unimplemented("WatchWorkflow");
      },

      stopWorkflow() {
        entered("StopWorkflow");
        LOGGER.debug({ rpc: "StopWorkflow" }, "refused a workflow verb: workflow is not implemented in this wave");
        throw unimplemented("StopWorkflow");
      },

      // ---- History ----

      async readHistory(request) {
        entered("ReadHistory");
        validateReadHistoryRequest(request);
        return answering("ReadHistory", () => engine.readHistory(request));
      },
    });
  };
}
