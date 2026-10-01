/**
 * engine/engine.ts — THE seam between the transport and the session.
 *
 * `service/` owns the wire: it validates a request, calls exactly one method
 * here, and turns what comes back into a response. `engine/` owns the session:
 * the one vendor query, the turn, the store, the identities, the streams.
 * Neither side knows anything about the other's concerns — routes.ts never
 * touches the SDK, and the engine never constructs a Connect error.
 *
 * # Why the methods speak proto messages
 *
 * The response messages ARE the contract, and a refusal is one of their arms.
 * If the engine answered in some internal vocabulary, every handler would need
 * a mapping layer, and the two would drift until a refusal the engine can
 * express has no arm on the wire. So an engine method returns the endpoint's
 * own response message: a refusal is a legitimate return value, not an
 * exception, and the ONLY exceptions that cross this seam are transport-level
 * ones (a refused stream open, an unimplemented verb).
 *
 * # Streams
 *
 * A streaming method returns an `AsyncIterable` of its response message. The
 * handler just yields it through. A refused OPEN throws (a stream has no
 * failure message to carry a refusal); everything after a successful open is a
 * frame.
 *
 * # What is deliberately NOT here
 *
 * `GetWorkflow`, `WatchWorkflow` and `StopWorkflow`. WORKFLOW IS KICKED (ruled
 * 2026-08-29): the three verbs stay in the contract and `routes.ts` answers
 * them `Code.Unimplemented` WITHOUT consulting an engine. Giving the interface
 * three methods nobody may implement would invite exactly the implementation
 * the ruling forbids, so the kick is expressed as an absence.
 */
import { ConnectError } from "@connectrpc/connect";
import type { shimv1 } from "../proto.js";
import { unimplemented } from "../service/failures.js";

/**
 * The session behind the socket.
 *
 * ONE implementation is live at a time and it is process-wide: a shim serves
 * exactly one session, so there is no routing key on any method.
 */
export interface Engine {
  // ---- Session: spawn, attach, condition, end ----

  /** Bind to a vendor session and bring it to where a prompt can be accepted. */
  startSession(request: shimv1.StartSessionRequest): Promise<shimv1.StartSessionResponse>;

  /**
   * The standing stream of session-level facts.
   *
   * Never concludes on its own. Its FIRST push after StartSession is
   * `diagnostics{healthy}` — the daemon's readiness signal, and the reason a
   * consumer may open this before anything else exists.
   */
  watchSession(request: shimv1.WatchSessionRequest): AsyncIterable<shimv1.WatchSessionResponse>;

  /** Change the model from the next turn on; resolves after the current turn ends. */
  setSessionModel(request: shimv1.SetSessionModelRequest): Promise<shimv1.SetSessionModelResponse>;

  /** Change the mode every subsequent permission gate runs under. */
  setSessionPermissionMode(
    request: shimv1.SetSessionPermissionModeRequest,
  ): Promise<shimv1.SetSessionPermissionModeResponse>;

  /** Compact, then ack. The daemon stands the shim down only after the ack. */
  hibernate(request: shimv1.HibernateRequest): Promise<shimv1.HibernateResponse>;

  /** End the session and everything live in it; refuses unless forced. */
  killSession(request: shimv1.KillSessionRequest): Promise<shimv1.KillSessionResponse>;

  // ---- Agent: start a turn, watch any agent, speak to it, end the turn ----

  /**
   * Deliver the session's turn. Unary: returns once the prompt is accepted.
   *
   * `signal` is the caller's: while the start waits behind the shim's own
   * keep-alive, its abort ends the wait with the prompt undelivered.
   */
  startTurn(request: shimv1.StartTurnRequest, signal?: AbortSignal): Promise<shimv1.StartTurnResponse>;

  /**
   * One agent's page then its tail, served FROM THE STORE and never from
   * memory. Throws a Connect NotFound when the target names no agent.
   */
  watchAgent(request: shimv1.WatchAgentRequest): AsyncIterable<shimv1.WatchAgentResponse>;

  /** Speak to an existing agent: stop it, answer its open ask, or prompt it. */
  updateAgent(request: shimv1.UpdateAgentRequest): Promise<shimv1.UpdateAgentResponse>;

  /** End the turn and everything it spawned, transitively; refuses unless forced. */
  killTurn(request: shimv1.KillTurnRequest): Promise<shimv1.KillTurnResponse>;

  /**
   * Rewind the main agent's vendor conversation to just before one of its
   * prompts, optionally restoring the files; the arm is the outcome.
   */
  rollBackSession(request: shimv1.RollBackSessionRequest): Promise<shimv1.RollBackSessionResponse>;

  // ---- Detached work ----

  /**
   * Follow one backgrounded shell: its original start instant, then the
   * sidecar-fed deltas, then its terminal. Throws a Connect NotFound when the
   * work id names nothing.
   */
  watchBash(request: shimv1.WatchBashRequest): AsyncIterable<shimv1.WatchBashResponse>;

  /** Kill a backgrounded shell — the only input a process takes. */
  stopBash(request: shimv1.StopBashRequest): Promise<shimv1.StopBashResponse>;

  /** Move in-flight turn work onto its own stream (Ctrl-B). */
  detachForeground(
    request: shimv1.DetachForegroundRequest,
  ): Promise<shimv1.DetachForegroundResponse>;

  // ---- History ----

  /** One page of one agent's durable past, newest first. */
  readHistory(request: shimv1.ReadHistoryRequest): Promise<shimv1.ReadHistoryResponse>;

  /**
   * Every vendor conversation filed under this shim's working directory, read
   * from the transcripts' own lines. Starts no query and makes no model call.
   */
  readTranscripts(
    request: shimv1.ReadTranscriptsRequest,
  ): Promise<shimv1.ReadTranscriptsResponse>;

  // ---- Title digest ----

  /**
   * The material for a synthesized workspace title: the user's prompts since
   * the last context boundary, plus the compaction summary when that boundary
   * was a /compact. Read from the transcript the shim owns.
   */
  gatherTitleDigest(
    request: shimv1.GatherTitleDigestRequest,
  ): Promise<shimv1.GatherTitleDigestResponse>;

  // ---- Process lifecycle ----

  /**
   * The graceful stand-down SIGTERM takes.
   *
   * Identical to `KillSession{force:true}` followed by waiting for every store
   * ack, and it exists SEPARATELY because the daemon may already be dead: an
   * rpc cannot be the only teardown path for a process designed to outlive its
   * parent. Resolves only once every pending permission callback is resolved
   * (an unresolved `canUseTool` promise wedges the vendor process) and every
   * durable write is acknowledged.
   *
   * Resolves with the EXIT CODE the stand-down earned: 0 when everything the
   * record owed the store was acked, 1 when rows were dropped loudly and never
   * landed. Reporting 0 in that second case would tell the daemon the session
   * ended in good order when part of the conversation is gone (audit A23).
   */
  standDown(reason: string): Promise<number>;
}

/**
 * The engine before there is an engine.
 *
 * `main.ts` wires this so the process is a COMPLETE, dialable server from the
 * moment it binds: argv, env, locks, log sink, socket and routing are all
 * exercised, and every verb answers `Unimplemented` — the honest statement that
 * the transport is up and the session is not. Answering an empty success
 * instead would make the daemon believe it had a session.
 */
export class NotImplementedEngine implements Engine {
  startSession(): Promise<shimv1.StartSessionResponse> {
    return Promise.reject(unimplemented("StartSession"));
  }

  watchSession(): AsyncIterable<shimv1.WatchSessionResponse> {
    return refuseStream<shimv1.WatchSessionResponse>("WatchSession");
  }

  setSessionModel(): Promise<shimv1.SetSessionModelResponse> {
    return Promise.reject(unimplemented("SetSessionModel"));
  }

  setSessionPermissionMode(): Promise<shimv1.SetSessionPermissionModeResponse> {
    return Promise.reject(unimplemented("SetSessionPermissionMode"));
  }

  hibernate(): Promise<shimv1.HibernateResponse> {
    return Promise.reject(unimplemented("Hibernate"));
  }

  killSession(): Promise<shimv1.KillSessionResponse> {
    return Promise.reject(unimplemented("KillSession"));
  }

  startTurn(): Promise<shimv1.StartTurnResponse> {
    return Promise.reject(unimplemented("StartTurn"));
  }

  watchAgent(): AsyncIterable<shimv1.WatchAgentResponse> {
    return refuseStream<shimv1.WatchAgentResponse>("WatchAgent");
  }

  updateAgent(): Promise<shimv1.UpdateAgentResponse> {
    return Promise.reject(unimplemented("UpdateAgent"));
  }

  killTurn(): Promise<shimv1.KillTurnResponse> {
    return Promise.reject(unimplemented("KillTurn"));
  }

  rollBackSession(): Promise<shimv1.RollBackSessionResponse> {
    return Promise.reject(unimplemented("RollBackSession"));
  }

  watchBash(): AsyncIterable<shimv1.WatchBashResponse> {
    return refuseStream<shimv1.WatchBashResponse>("WatchBash");
  }

  stopBash(): Promise<shimv1.StopBashResponse> {
    return Promise.reject(unimplemented("StopBash"));
  }

  detachForeground(): Promise<shimv1.DetachForegroundResponse> {
    return Promise.reject(unimplemented("DetachForeground"));
  }

  readHistory(): Promise<shimv1.ReadHistoryResponse> {
    return Promise.reject(unimplemented("ReadHistory"));
  }

  readTranscripts(): Promise<shimv1.ReadTranscriptsResponse> {
    return Promise.reject(unimplemented("ReadTranscripts"));
  }

  gatherTitleDigest(): Promise<shimv1.GatherTitleDigestResponse> {
    return Promise.reject(unimplemented("GatherTitleDigest"));
  }

  /**
   * Nothing to stand down. NOT an error: SIGTERM must exit 0 whether or not a
   * session was ever started, or a shim spawned and immediately told to stop
   * would look like a crash.
   */
  standDown(): Promise<number> {
    return Promise.resolve(0);
  }
}

/**
 * A stream whose OPEN is refused, expressed as an iterable that throws on the
 * first pull.
 *
 * A Connect streaming handler is called for its iterable, so the refusal has to
 * live inside the iteration rather than at the call — the transport turns the
 * thrown ConnectError into the stream's error the same way either path would.
 */
function refuseStream<T>(rpc: string): AsyncIterable<T> {
  return {
    // eslint-disable-next-line require-yield
    async *[Symbol.asyncIterator](): AsyncIterator<T> {
      throw unimplemented(rpc) satisfies ConnectError;
    },
  };
}
