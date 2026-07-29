/**
 * UdsSession — the UDS-mode session engine (design §8, stitch task S5).
 *
 * This is the UDS-mode twin of the stdio {@link import("../session.js").ShimSession}.
 * Where ShimSession maps SDK messages onto the Layer-1 NDJSON protocol, this
 * class binds the SAME SDK-driving machinery (an {@link AsyncQueue} streaming
 * input, a `query()` pump, a `canUseTool` round-trip) onto the merged G4/G5
 * layers:
 *
 *   - CONTROL: a {@link SessionServer} listens on `session-<id>.sock`; the
 *     daemon connects. Inbound SubmitPrompt/Interrupt/PermissionResponse are
 *     dispatched through a {@link ControlDispatch} bound to this session's live
 *     SDK query (the {@link SdkControlTarget} below). A daemon disconnect tears
 *     nothing down — the turn keeps running and a restarted daemon reattaches
 *     (§4.4).
 *   - EVENTS: every SDK message is classified. `stream_event`/`tool_progress`
 *     are EPHEMERAL — mapped by {@link toEphemeralEvent} and sent STRAIGHT to
 *     the daemon, NEVER written to the store (the §4.3 delta bypass). Every
 *     other message is {@link convert}ed to `{ vendor, lifecycle }` and written
 *     to the store; the store merges/seq-stamps and feeds the merged stream
 *     back via `onMerged`, which forwards it to the daemon (the §4.2 round-trip).
 *   - SAD PATH: a store outage surfaces as an `Event(DegradedState)` forwarded
 *     to the daemon (StoreClient already loud-logs each dropped event).
 *
 * LIFETIME (§4.4): the UDS server owns lifetime. A daemon disconnect does NOT
 * end the session or the in-flight turn. There is no stdin in UDS mode, so
 * stdin-EOF is not a stop path; the explicit stop path is {@link shutdown}
 * (wired to SIGTERM by main.ts). `ShimHello.turnInFlight` is wired to the live
 * turn count so a reattaching daemon learns whether a turn is running.
 */
import { AsyncQueue } from "../input-queue.js";
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import {
  PERMISSION_MODES,
  isPermissionMode,
  isSwitchablePermissionMode,
  type ContentBlock,
  type PermissionMode,
} from "../protocol.js";
import { describeInterruptSurvivors } from "../session.js";
import type {
  CanUseToolLike,
  PermissionResultLike,
  QueryLike,
  SdkMessageLike,
  SdkUserMessageLike,
} from "../session.js";
import { SessionStartGate, convert, promptPreview } from "../proto/convert.js";
import { isEphemeral, toEphemeralEvent, StreamMessageTracker } from "../proto/delta.js";
import { ControlDispatch, type SdkControlTarget, type ToolPermissionResult } from "./control.js";
import { SessionServer, type SessionServerHandlers } from "./server.js";
import { StoreClient, type ReplayOutcome } from "./store-client.js";
import { envelopeIs, unpackAs, type Any } from "./framing.js";
import { ClaudeStreamMessageSchema } from "../../../../../proto/gen/ts/agentshim/data/v1/stream_pb.js";
import { TranscriptLineSchema } from "../../../../../proto/gen/ts/agentshim/data/v1/transcript_pb.js";
import type { ApiUserMessage } from "../../../../../proto/gen/ts/agentshim/data/v1/tools_pb.js";
import { bindLog, setClaudeSessionId } from "./log.js";
import {
  DaemonHello,
  DegradedState,
  DegradedStateSchema,
  Event,
  EventClass,
  EventSchema,
  HealthCheck,
  HealthStatus,
  HealthStatusSchema,
  Plane,
  ReplayDoneSchema,
  InterruptOutcome,
  ReplayRequest,
  SessionSource,
  SessionStartedSchema,
  ShimReadySchema,
  TurnStartedSchema,
} from "./proto.js";

const COMPONENT = "uds-session";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.uds-session.lifecycle" });

export interface UdsSessionDeps {
  sessionId: string;
  shimVersion: string;
  protocolVersion: string;
  /** Where the shim listens for the daemon (session-<id>.sock). */
  udsSocketPath: string;
  /** Where the shim connects to the shim-store (store.sock). */
  storeSocketPath: string;
  /**
   * SESSION_SOURCE_RESUME when the shim was spawned with `--resume`, FRESH
   * otherwise. Threaded into every convert() so `system:init`'s SessionStarted
   * twin reports the true origin (design §5.2 SessionSource).
   */
  sessionSource: SessionSource;
  /**
   * The vendor session id (Claude's uuid) the store keys events by, when
   * already known — i.e. `--resume`. Omit for a fresh session; the uuid is
   * then adopted from the first converted event.
   */
  storeSessionId?: string;
  /**
   * The `--permission-mode` argv value the query was CONSTRUCTED with.
   *
   * It is a spawn-time snapshot and the WEAKER of the two channels: the
   * daemon's DaemonHello.permission_mode is read off the session record at
   * handshake time and overrides this (see completeWiring). Kept anyway so the
   * override is a comparison rather than a guess, and so a daemon too old to
   * send the field leaves the session exactly where argv put it.
   *
   * Defaults to "default", which is what the argv parser itself defaults to.
   */
  permissionMode?: PermissionMode;
  /** Construct the SDK query over the streaming input iterable. */
  createQuery: (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ) => QueryLike;
  /** Request-id minter for permission round-trips; defaults to randomUUID. */
  newRequestId?: () => string;
  /** Heartbeat cadence on both UDS connections; 0 disables. Test injects 0. */
  heartbeatIntervalMs?: number;
  /** Wall-clock injection for deterministic tests. */
  nowMs?: () => number;
  /**
   * How long a bounded replay waits for the next store frame before deciding
   * its subscription drained. A FAILURE bound, not a pace: a store mid-replay
   * writes back to back. Default 5000ms; tests shorten it.
   */
  replayIdleMs?: number;
}

export class UdsSession {
  private readonly input = new AsyncQueue<SdkUserMessageLike>();
  private readonly control: ControlDispatch;
  private readonly store: StoreClient;
  private readonly server: SessionServer;
  private query: QueryLike | null = null;
  /**
   * Outstanding-turn COUNTER (prompts submitted minus results seen). A counter
   * not a boolean for the same reason ShimSession keeps one: streaming input
   * queues turns, so a boolean would misreport a queued turn. Read by the
   * server's ShimHello.turn_in_flight so a reattaching daemon knows.
   */
  private turnsInFlight = 0;
  private closed = false;
  /** Idle bound for a bounded replay's store subscription (see deps). */
  private readonly replayIdleMs: number;
  /**
   * Which assistant message the SDK is currently streaming. Deltas carry no
   * message identity of their own, so this supplies the one consumers
   * reconcile on — without it every chunk looked like a new message and the
   * frontend opened a bubble per chunk.
   */
  private readonly streamMessages = new StreamMessageTracker();
  /**
   * Admits only the FIRST `system:init` of this shim's lifetime to emit a
   * SessionStarted twin. Shim-lifetime state, so it is owned here and injected
   * into every convert() rather than living in the (otherwise stateless)
   * converter.
   */
  private readonly sessionGate = new SessionStartGate();
  /**
   * Whether the VENDOR session id — the key the store files this session's
   * events under — is known yet. Known up front on `--resume`; otherwise only
   * the SDK can reveal it, on the first converted message.
   */
  private storeKeyKnown: boolean;
  /**
   * TurnStarted events accepted before the vendor session id was known. See
   * emitTurnStarted: writing them under the placeholder key would file them in
   * a seq space nothing subscribes to, so they wait for the real key instead
   * of being lost.
   */
  private readonly deferredTurnStarts: Event[] = [];
  /**
   * The mode this session is ACTUALLY running under: argv until the bring-up
   * gate applies the daemon's, then the daemon's. Reported on the readiness
   * announcement so "what posture is this session in?" is answerable from the
   * log rather than by inference over two channels.
   *
   * A per-prompt SubmitPrompt.permission_mode override is deliberately NOT
   * folded in: this is where the session STARTS, and that is a per-turn
   * override applied on top of it.
   */
  private effectivePermissionMode: PermissionMode;

  constructor(private readonly deps: UdsSessionDeps) {
    this.storeKeyKnown = deps.storeSessionId !== undefined && deps.storeSessionId !== "";
    this.replayIdleMs = deps.replayIdleMs ?? 5000;
    this.effectivePermissionMode = deps.permissionMode ?? "default";
    const target: SdkControlTarget = {
      submitPrompt: ({ requestId, text, permissionMode }): void => {
        this.turnsInFlight++;
        const content: ContentBlock[] = [{ type: "text", text }];
        this.input.push({
          type: "user",
          message: { role: "user", content },
          parent_tool_use_id: null,
          session_id: this.deps.sessionId,
        });
        // The ACCEPT half of the prompt round-trip receipt. Its other half is
        // the forward log below (`user prompt event forwarded to daemon`), so
        // a prompt that reached the SDK but never came back as a bubble shows
        // up as a receipt with no matching forward, rather than as silence.
        // Only the FAILURE path used to say anything (control.ts).
        LOGGER.log({ agent_repl_session_id: this.deps.sessionId, request_id: requestId, len: text.length, turns_in_flight: this.turnsInFlight },
          `prompt accepted -> SDK input`);
        // Turn start is SHIM-AUTHORITATIVE (see convert.ts's header): the
        // vendor stream has no "a turn began" message, and the user-message
        // echo the converter used to derive one from never arrives for a live
        // submit. Accepting the prompt IS the turn starting, so say so here.
        this.emitTurnStarted(requestId, text);
        // A prompt-scoped permission-mode override rides on SubmitPrompt. Apply
        // it to the live query (fire-and-forget: the sync Ack does not wait on
        // the SDK).
        //
        // BOTH failure paths report DegradedState, not just a log line. The
        // user picked a mode; if it did not take, the session is running under
        // a DIFFERENT permission posture than the one they chose, and a log
        // nobody reads is not an acceptable way to tell them that. The gate is
        // `isSwitchablePermissionMode`, not `isPermissionMode`, because
        // bypassPermissions is launch-only and the CLI is guaranteed to reject
        // it here.
        if (permissionMode !== undefined && permissionMode !== "") {
          if (isSwitchablePermissionMode(permissionMode)) {
            void this.query?.setPermissionMode(permissionMode as PermissionMode).catch((err: unknown) => {
              this.reportDegraded(
                "claude-shim-permission-mode",
                `permission mode "${permissionMode}" was rejected, session still in the previous mode: ${errMsg(err)}`,
              );
            });
          } else {
            this.reportDegraded(
              "claude-shim-permission-mode",
              `permission mode "${permissionMode}" cannot be set on a running session, session still in the previous mode`,
            );
          }
        }
      },
      interrupt: (): InterruptOutcome => {
        // THE RACE, MADE STRUCTURALLY IMPOSSIBLE.
        //
        // The liveness read happens SYNCHRONOUSLY, here, before any await
        // and before any promise is created. This event loop is
        // single-threaded and this class owns the turn counter — it
        // increments on submit-accept and decrements when the result lands —
        // so no turn can start or end between this read and the outcome it
        // decides. A turn that ends a microsecond later cannot retroactively
        // turn INTERRUPTED into ALREADY_COMPLETE, and one that ended a
        // microsecond earlier is already counted out.
        //
        // Deciding it downstream is what made it ambiguous: an observer
        // watching for the turn's `aborted` result sees the stop and the
        // turn end as two unordered events.
        const wasLive = this.turnsInFlight > 0;
        // Interrupt cancels every blocked permission wait so no SDK callback
        // hangs, then forwards to the SDK (a no-op when idle).
        this.control.cancelAll("interrupt");
        if (!this.query) {
          // No SDK query to interrupt: the stop provably cannot be
          // delivered. Previously the optional-chain below swallowed this
          // whole — no forward, no report, and an Ack that read as success.
          this.reportDegraded("claude-shim-interrupt",
            "interrupt could not be delivered: no SDK query is constructed for this session");
          return InterruptOutcome.FAILED;
        }
        void this.query.interrupt()
          .then((receipt) => {
            // SDK >= 0.3.205 answers with an interrupt receipt. The anomaly
            // wording is shared with the stdio path (describeInterruptSurvivors)
            // so the two transports cannot describe the same broken assumption
            // two different ways. An empty receipt is the expected case and
            // says nothing; a non-empty one is honest downtime.
            const anomaly = describeInterruptSurvivors(receipt);
            if (anomaly === null) return;
            this.reportDegraded("claude-shim-interrupt", anomaly);
          })
          .catch((err: unknown) => {
            // An async rejection cannot revise an Ack already written, so it
            // keeps its DegradedState channel unchanged. That is the right
            // shape: the outcome is a statement about the TURN at the moment
            // the stop landed, while this is a later transport failure, and
            // collapsing the two would put a stale verdict on the wire.
            this.reportDegraded("claude-shim-interrupt", `interrupt failed: ${errMsg(err)}`);
          });
        return wasLive ? InterruptOutcome.INTERRUPTED : InterruptOutcome.ALREADY_COMPLETE;
      },
    };
    this.control = new ControlDispatch(
      target,
      (req) => this.server.sendPermissionRequest(req),
      this.deps.newRequestId !== undefined ? { newRequestId: this.deps.newRequestId } : {},
    );

    this.store = new StoreClient({
      socketPath: this.deps.storeSocketPath,
      sessionId: this.deps.sessionId,
      producer: `claude-shim:${this.deps.sessionId}`,
      ...(this.deps.heartbeatIntervalMs !== undefined ? { heartbeatIntervalMs: this.deps.heartbeatIntervalMs } : {}),
      ...(this.deps.storeSessionId !== undefined ? { storeSessionId: this.deps.storeSessionId } : {}),
    });

    const handlers: SessionServerHandlers = {
      onSubmitPrompt: (m) => this.control.handleSubmitPrompt(m),
      onInterrupt: (m) => this.control.handleInterrupt(m),
      onPermissionResponse: (m) => this.control.handlePermissionResponse(m),
      onReplayRequest: (m) => void this.serveReplay(m),
      onHealthCheck: (m) => this.health(m),
      // THE BRING-UP GATE'S WIRING STAGE. The DaemonHello carries the
      // from_seq this session's standing store subscription is to be opened
      // at; completeWiring opens it and only then acks with ShimReady.
      //
      // It runs on every REATTACH too, and that is the point rather than a
      // flaw: a restarted daemon has no memory of this session and must be
      // wired to it exactly as the first one was.
      onDaemonConnected: (hello) => void this.completeWiring(hello),
      onDaemonDisconnected: () => {
        // Reattach (§4.4): the turn keeps running and nothing is torn down.
        // Pending permission waits are NOT cancelled — a reattaching daemon can
        // still answer them; cancelAll fires only on interrupt/shutdown.
        LOGGER.log({ agent_repl_session_id: this.deps.sessionId, turn_in_flight: this.turnsInFlight > 0 }, `daemon detached; session and turn survive (awaiting reattach)`);
      },
    };
    this.server = new SessionServer(
      {
        socketPath: this.deps.udsSocketPath,
        sessionId: this.deps.sessionId,
        shimVersion: this.deps.shimVersion,
        protocolVersion: this.deps.protocolVersion,
        turnInFlight: () => this.turnsInFlight > 0,
        // The daemon resets its store cursor when this differs from the uuid
        // it has persisted, which is how a rotation's fresh seq space is
        // subscribed from zero instead of from a retired high-water mark.
        vendorSessionId: () => this.store.vendorSessionId(),
        ...(this.deps.heartbeatIntervalMs !== undefined ? { heartbeatIntervalMs: this.deps.heartbeatIntervalMs } : {}),
      },
      handlers,
    );
    // THE ROTATION'S OTHER HALF. The store client detects the vendor's new
    // transcript identity and owns the re-key; only this class can reach the
    // daemon link, so the bounce is injected here. It runs BEFORE the re-key
    // (store-client.ts rotateStoreKey), so no event of the new seq space can
    // reach a connection whose last_seen belongs to the retired one.
    this.store.onRotation((previous, next) => {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, store_key: previous, rotating_to: next },
        `vendor session rotated; bouncing the daemon link so the re-handshake announces ${next}`);
      this.server.bounce(`vendor session rotation ${previous} -> ${next}`);
    });
  }

  /**
   * Answer only after the active shim has proved its required store paths.
   * Receiving this request itself proves the daemon's UDS connection completed
   * the SessionServer handshake; StoreClient proves the producer, standing
   * subscription, and an independent store protocol round-trip.
   */
  private async health(check: HealthCheck): Promise<HealthStatus> {
    const result = await this.store.health(check.requestId);
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      request_id: check.requestId,
      healthy: result.healthy,
      store_connected: this.store.isConnected(),
      store_subscribed: this.store.isSubscribed(),
      reason: result.reason,
    }, result.healthy ? "session health PASS" : "session health FAIL");
    return create(HealthStatusSchema, {
      requestId: check.requestId,
      healthy: result.healthy,
      component: "claude-shim",
      reason: result.reason,
    });
  }

  /**
   * Serve one bounded historical replay for the daemon (core.proto
   * ReplayRequest, design §5.4.1).
   *
   * The events come off a THROWAWAY store subscription (StoreClient.replay)
   * and go back as ReplayEvent, which is a different wire type from the live
   * Event stream — so the daemon routes them to conversation translation and
   * nothing else, by frame type rather than by convention.
   *
   * Exactly one ReplayDone is sent, whatever happens. A replay that simply
   * stopped streaming would be indistinguishable from one still in flight, so
   * even an unexpected failure closes the request (loudly, as truncated).
   */
  private async serveReplay(req: ReplayRequest): Promise<void> {
    let outcome: ReplayOutcome;
    try {
      outcome = await this.store.replay({
        fromSeq: req.fromSeq,
        toSeq: req.toSeq,
        maxEvents: req.maxEvents,
        idleMs: this.replayIdleMs,
        onEvent: (evt) => this.server.sendReplayEvent(req.requestId, evt),
      });
    } catch (err) {
      outcome = { delivered: 0, truncated: true, reason: `replay failed: ${errMsg(err)}` };
      LOGGER.log({ level: "error", agent_repl_session_id: this.deps.sessionId, request_id: req.requestId }, outcome.reason);
    }
    this.server.sendReplayDone(create(ReplayDoneSchema, {
      requestId: req.requestId,
      truncated: outcome.truncated,
      reason: outcome.reason,
      delivered: BigInt(outcome.delivered),
    }));
  }

  /**
   * The SDK canUseTool callback: block the tool on a daemon decision via the
   * ControlDispatch PermissionRequest round-trip.
   */
  private readonly canUseTool: CanUseToolLike = (toolName, input) =>
    this.control.requestPermission(toolName, input as JsonObject).then(toPermissionResult);

  /**
   * Construct the query, connect the store, start listening, and pump the SDK
   * stream. Returns the pump promise (resolves when the SDK stream ends).
   */
  async start(): Promise<void> {
    this.query = this.deps.createQuery(this.input, this.canUseTool);
    // The store round-trip feed: merged, seq-stamped events go to the daemon.
    this.store.onMerged((evt) => {
      this.logUserPromptForward(evt);
      this.server.sendEvent(evt);
    });
    // Honest sad path: a store outage becomes an Event(DegradedState) forwarded
    // to the daemon (StoreClient has already loud-logged each dropped event).
    this.store.onDegraded((report) => this.server.sendEvent(this.degradedEvent(report)));
    await this.store.connect();
    // Readiness is asserted from the handshake hook wired in the
    // constructor, not here: connect() resolves on the DIAL, and an event
    // sent before the DaemonHello would be dropped.
    await this.server.connect();
    return this.pump();
  }

  /**
   * THE BRING-UP GATE, shim side (core.proto ShimHello). Finish wiring this
   * session at the from_seq the DaemonHello carried, then — and only then —
   * ack with ShimReady.
   *
   * WHAT IS ALREADY TRUE WHEN THIS RUNS, and why none of it needs doing here:
   *   - the SESSION LOCK is held, taken in main.ts before this object exists
   *     (a shim that could not claim its session refuses to start at all);
   *   - the SDK QUERY is constructed, and the STORE PRODUCER link is up, both
   *     by `start()` before it ever dials the daemon.
   * Each is therefore a structural precondition of reaching this code, not a
   * check performed by it.
   *
   * WHAT IS LEFT is the standing store subscription, which needs a from_seq
   * only the daemon knows. Awaiting it is the whole point: subscribe()
   * resolves when the Subscribe frame is on a live store connection, so the
   * ack that follows is a statement about a tail that EXISTS.
   *
   * A FAILURE WITHHOLDS THE ACK. No ShimReady is sent, the reason is logged
   * loudly, and a DegradedState carries it to the daemon (which the daemon can
   * surface even though this session is not ready). The daemon's readiness
   * wait then fails on its own deadline — the honest outcome, and far better
   * than acking a session whose store tail does not exist.
   */
  /**
   * Apply DaemonHello.permission_mode as this session's posture, INSIDE the
   * gate. Returns false when the gate must refuse (the caller then withholds
   * ShimReady exactly as it does for a failed store link).
   *
   * WHY IT IS A SWITCH AND NOT A CONSTRUCTION ARGUMENT. `start()` builds the
   * SDK query and only then dials the daemon, so the query already exists when
   * the hello lands — construction is strictly earlier, and there is no
   * ordering in which argv could be superseded at build time. Switching here,
   * before the ack, still gets the posture in place before the first prompt
   * can be delivered: nothing may be sent to a session that has not acked.
   *
   * PRECEDENCE: the handshake wins. argv is a spawn-time snapshot; this is the
   * record read at handshake time, and every reattach re-reads it. An override
   * of a DIFFERING argv value is logged, because a session silently running in
   * a mode other than the one its command line named is exactly the confusion
   * this whole change exists to end.
   *
   * REFUSALS ARE LOUD, NEVER FALLBACKS. An unknown mode, a missing query, a
   * launch-only mode the CLI cannot switch into, and a rejected switch all
   * withhold the ack and report a DegradedState. Falling back to "default"
   * would run the session under a posture nobody chose, which is the original
   * defect wearing a different hat.
   */
  private async applyHandshakePermissionMode(mode: string): Promise<boolean> {
    // Empty means a daemon too old to speak the field (core.proto): argv
    // stands, unchanged, for the rollout window.
    if (mode === "") return true;
    if (!isPermissionMode(mode)) {
      this.refuseBringUp(`DaemonHello carried permission_mode "${mode}", which is not one of ${PERMISSION_MODES.join(", ")}`);
      return false;
    }
    const argv = this.deps.permissionMode ?? "default";
    if (!isSwitchablePermissionMode(mode)) {
      // LAUNCH-ONLY (bypassPermissions): the CLI refuses to switch into it, so
      // the only way a session can be in it is to have been LAUNCHED in it.
      // Equal-to-argv is therefore the one acceptable case; anything else is a
      // posture this shim cannot deliver, and saying so is the honest answer.
      if (mode !== argv) {
        this.refuseBringUp(`the handshake asked for launch-only permission_mode "${mode}", which the CLI cannot switch a running session into (argv launched it as "${argv}")`);
        return false;
      }
      this.effectivePermissionMode = mode;
      return true;
    }
    if (this.query === null) {
      this.refuseBringUp(`the handshake asked for permission_mode "${mode}" but no SDK query is constructed for this session`);
      return false;
    }
    // ASSERTED EVEN WHEN IT EQUALS ARGV. "the flag must already have taken" is
    // the assumption this whole change exists to stop making — the offline fake
    // query, for one, ignores argv entirely — so the gate STATES the posture on
    // the live query rather than inferring it from the command line.
    try {
      await this.query.setPermissionMode(mode);
    } catch (err) {
      this.refuseBringUp(`the handshake's permission_mode "${mode}" was rejected by the CLI: ${errMsg(err)}`);
      return false;
    }
    this.effectivePermissionMode = mode;
    if (mode !== argv) {
      LOGGER.log({ agent_repl_session_id: this.deps.sessionId, permission_mode: mode, argv_permission_mode: argv },
        `handshake permission mode OVERRIDES the --permission-mode argv; this session runs as ${mode}`);
    }
    return true;
  }

  /**
   * Refuse the bring-up gate: loud log, DegradedState to the daemon, and NO
   * ShimReady. The daemon's readiness wait then fails on its own deadline,
   * which is the honest outcome for a session that is not wired as asked.
   */
  private refuseBringUp(reason: string): void {
    this.reportDegraded("claude-shim-bringup", `bring-up gate REFUSED: ${reason}`);
  }

  private async completeWiring(hello: DaemonHello): Promise<void> {
    const fromSeq = hello.fromSeq;
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, from_seq: fromSeq, store_key: this.store.storeSessionId() },
      `bring-up gate: wiring the standing store subscription before acking readiness`);
    if (!(await this.applyHandshakePermissionMode(hello.permissionMode))) return;
    try {
      await this.store.subscribe(fromSeq);
    } catch (err) {
      const reason = `bring-up gate REFUSED: the standing store subscription could not be opened at from_seq=${fromSeq}: ${errMsg(err)}`;
      // reportDegraded loud-logs AND sends the DegradedState. Readiness is
      // withheld deliberately: there is no ack to send for a session that is
      // not wired, and the daemon must not be told otherwise.
      this.reportDegraded("claude-shim-bringup", reason);
      return;
    }
    // Readiness first, ack second, in frame order on one connection: the
    // daemon has this session's SessionStarted in hand before the gate that
    // releases its callers closes.
    this.emitSessionStarted();
    this.server.sendReady(create(ShimReadySchema, {
      sessionId: this.deps.sessionId,
      fromSeq,
      vendorSessionId: this.store.storeSessionId(),
    }));
  }

  /**
   * Announce that this shim is ready, directly to the daemon.
   *
   * WHY NOT THE VENDOR'S system:init. SessionStarted used to ride as a twin
   * of the SDK's `system:init`, but the SDK does not emit that until the
   * FIRST PROMPT — so a session nobody had typed into yet never announced
   * itself, and the workspace sat in bring-up indefinitely waiting for a
   * message the user had no reason to send. Readiness cannot be conditional
   * on being used; that is the circularity this replaces.
   *
   * WHY DIRECT AND NOT THROUGH THE STORE. Every other lifecycle twin takes
   * the store round-trip so it lands seq-ordered against the conversation.
   * Readiness cannot: the store keys events by the VENDOR session id, which
   * on a fresh session is unknown until the SDK reveals it on the first
   * converted message — the first prompt again. A store write here would be
   * deferred exactly as `emitTurnStarted` defers its held turns, walking
   * straight back into the trap. Readiness is a fact about the SHIM rather
   * than about the vendor conversation, so it takes the same
   * SYNTHETIC/EPHEMERAL direct path DegradedState does.
   *
   * The gate is CLOSED afterwards so the vendor init's twin does not
   * re-announce a session already announced.
   */
  private emitSessionStarted(): void {
    this.sessionGate.close();
    this.server.sendEvent(create(EventSchema, {
      sessionId: this.deps.sessionId,
      seq: 0n,
      plane: Plane.SYNTHETIC,
      class: EventClass.EPHEMERAL,
      producedAtMs: BigInt(this.now()),
      payload: {
        case: "sessionStarted",
        value: create(SessionStartedSchema, { source: this.deps.sessionSource }),
      },
    }));
    // The EFFECTIVE mode, not the argv one: the readiness announcement is the
    // one line that says what posture this session actually came up in.
    LOGGER.log({
      agent_repl_session_id: this.deps.sessionId,
      permission_mode: this.effectivePermissionMode,
    },
      `readiness asserted (lock held, SDK query built, daemon handshaked)`);
  }

  /** True while a handshaked daemon connection is live (test/diagnostics). */
  isConnected(): boolean {
    return this.server.isConnected();
  }

  /** Outstanding-turn count (test/diagnostics). */
  turnCount(): number {
    return this.turnsInFlight;
  }

  /**
   * The explicit stop path (§4.4): SIGTERM (wired by main.ts) or a deliberate
   * teardown. Cancels blocked permissions, ends the SDK input iterable (which
   * ends the pump), and closes both UDS connections. Idempotent.
   */
  async shutdown(reason = "shutdown"): Promise<void> {
    if (this.closed) return;
    this.closed = true;
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId }, `shutdown (${reason})`);
    this.control.cancelAll(reason);
    this.input.end();
    await this.server.close();
    this.store.close();
  }

  private async pump(): Promise<void> {
    try {
      for await (const msg of this.query!) {
        this.routeSdkMessage(msg);
      }
    } catch (err) {
      LOGGER.log({ level: "error", agent_repl_session_id: this.deps.sessionId, cause: err }, `SDK stream failed: ${errMsg(err)}`);
      // A dead SDK turn is honest downtime: report it to the daemon, then tear
      // the session down (unlike a daemon disconnect, the turn itself is gone).
      this.server.sendEvent(this.degradedEvent(create(DegradedStateSchema, {
        component: "claude-shim-sdk",
        reason: `SDK stream failed: ${errMsg(err)}`,
        droppedCount: 0n,
        recovered: false,
      })));
      await this.shutdown("sdk_error");
      return;
    }
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId }, `SDK stream ended`);
    await this.shutdown("sdk_end");
  }

  /**
   * Route one SDK message per the G4 wiring: EPHEMERAL deltas straight to the
   * daemon (never the store); everything else converted and written to the
   * store (whose merged echo returns via onMerged → the daemon).
   */
  private routeSdkMessage(msg: SdkMessageLike): void {
    if (isEphemeral(msg)) {
      // Observe BEFORE converting: a message_start must make its own id
      // current for the deltas that follow it.
      this.streamMessages.observe(msg);
      const evt = toEphemeralEvent(msg, { ...this.convertOpts(), messageId: this.streamMessages.current() });
      if (evt) this.server.sendEvent(evt);
      return;
    }
    // A result closes the turn it belongs to.
    if (msg.type === "result" && this.turnsInFlight > 0) this.turnsInFlight--;
    const { vendor, lifecycle } = convert(msg, {
      sessionSource: this.deps.sessionSource,
      sessionGate: this.sessionGate,
      ...this.convertOpts(),
    });
    // The converted envelope carries the VENDOR session id (read off the SDK
    // message), which is the id the store files these events under. Adopt it
    // as the subscription key: a fresh session has no other way to learn it,
    // and subscribing under this shim's `--session-id` listens on a channel
    // nothing publishes to.
    this.store.adoptStoreKey(vendor.sessionId);
    setClaudeSessionId(vendor.sessionId);
    this.settleStoreKey(vendor.sessionId);
    void this.store.write([vendor, ...lifecycle]).catch(() => {
      // The honest sad path lives INSIDE StoreClient (loud-log per dropped
      // event + DegradedState to onDegraded). Nothing to add here; we only
      // keep the rejected promise from going unhandled.
    });
  }

  /**
   * Write the shim-authoritative TurnStarted for a just-accepted prompt.
   *
   * PERSISTENT and store-bound like every other lifecycle twin, so it takes
   * the same seq-stamped round-trip back to the daemon and lands in the SSM in
   * order with the TurnEnded that closes it.
   */
  private emitTurnStarted(requestId: string, text: string): void {
    const evt = create(EventSchema, {
      sessionId: this.store.storeSessionId(),
      seq: 0n,
      plane: Plane.STREAM,
      class: EventClass.PERSISTENT,
      requestId,
      producedAtMs: BigInt(this.now()),
      payload: {
        case: "turnStarted",
        value: create(TurnStartedSchema, { promptPreview: promptPreview(text) }),
      },
    });
    if (!this.storeKeyKnown) {
      // The vendor session id is genuinely unknown until the SDK reveals it
      // (a fresh session carries no `--resume` id), and the store keys its seq
      // space by it. Writing now would file this turn under the placeholder
      // `--session-id`, which nothing subscribes to, so the daemon would never
      // see the turn it just asked for. It waits for the real key instead.
      this.deferredTurnStarts.push(evt);
      LOGGER.log({ level: "warn", agent_repl_session_id: this.deps.sessionId, request_id: requestId },
        `turn accepted before the vendor session id was known; TurnStarted held until the store key settles`);
      return;
    }
    void this.store.write([evt]).catch(() => {
      // StoreClient owns the honest sad path (loud per-event drop log +
      // DegradedState). Only keeping the rejection handled here.
    });
  }

  /**
   * Record that the vendor session id is now known and flush any TurnStarted
   * held for it, restamped with the settled key.
   */
  private settleStoreKey(vendorSessionId: string): void {
    if (vendorSessionId === "") return;
    this.storeKeyKnown = true;
    if (this.deferredTurnStarts.length === 0) return;
    const key = this.store.storeSessionId();
    const held = this.deferredTurnStarts.splice(0);
    for (const evt of held) evt.sessionId = key;
    LOGGER.log({ agent_repl_session_id: this.deps.sessionId, claude_session_id: key, store_key: key },
      `store key settled; flushing ${held.length} held TurnStarted event(s)`);
    void this.store.write(held).catch(() => {
      // See emitTurnStarted: StoreClient reports the drop.
    });
  }

  private convertOpts(): { nowMs?: number } {
    return this.deps.nowMs !== undefined ? { nowMs: this.deps.nowMs() } : {};
  }

  private now(): number {
    return this.deps.nowMs !== undefined ? this.deps.nowMs() : Date.now();
  }

  /**
   * Report an SDK-side failure to the daemon as honest downtime.
   *
   * The one channel this session has for "something the user asked for did
   * not happen": a loud log PLUS a DegradedState the daemon can surface. A
   * bare `log` is not enough — nobody watching the UI ever sees it.
   */
  private reportDegraded(component: string, reason: string): void {
    LOGGER.log({ level: "error", agent_repl_session_id: this.deps.sessionId }, reason);
    this.server.sendEvent(this.degradedEvent(create(DegradedStateSchema, {
      component,
      reason,
      droppedCount: 0n,
      recovered: false,
    })));
  }

  /**
   * The RETURN half of the prompt round-trip receipt: one line when a merged
   * store event carrying the user's own prompt text goes back to the daemon.
   *
   * This is the leg nothing else covers. The live user-prompt echo does NOT
   * come back on the SDK stream (see submitPrompt) — the CLI writes a
   * transcript line, the sidecar files it, and it reaches the daemon only
   * through this forward. Without a line here, a prompt that was accepted but
   * never became a bubble looked identical to one that came back fine.
   */
  private logUserPromptForward(evt: Event): void {
    // Discriminator first: every lifecycle twin and every non-vendor payload
    // is out on a single string compare, before anything is unpacked.
    if (evt.payload.case !== "vendor") return;
    const prompt = userPromptText(evt.payload.value);
    if (prompt === null) return;
    LOGGER.log(
      { agent_repl_session_id: this.deps.sessionId, seq: evt.seq, len: prompt.len, arm: prompt.arm },
      `user prompt event forwarded to daemon`);
  }

  /** Wrap a DegradedState as a SYNTHETIC/EPHEMERAL Event for the daemon. */
  private degradedEvent(report: DegradedState): Event {
    return create(EventSchema, {
      sessionId: this.deps.sessionId,
      seq: 0n,
      plane: Plane.SYNTHETIC,
      class: EventClass.EPHEMERAL,
      producedAtMs: BigInt(this.now()),
      payload: { case: "degradedState", value: report },
    });
  }
}

/**
 * The user-prompt text a vendor Any carries, or null when it carries none.
 *
 * TWO CARRIERS, deliberately both: a `data.v1.TranscriptLine` whose line is a
 * UserLine is the FILE plane — the live echo the CLI writes and the sidecar
 * republishes, which is what becomes the GUI bubble — while a
 * `data.v1.UserMessage` inside a ClaudeStreamMessage is the STREAM plane, how
 * the same turn arrives on a resume replay. Watching only one leaves the
 * other's prompts invisible.
 *
 * Neither type url matching means this is some other vendor payload, and the
 * Any is never parsed: the check is a string compare on the type url.
 */
function userPromptText(vendor: Any): { len: number; arm: string } | null {
  if (envelopeIs(vendor, TranscriptLineSchema)) {
    const line = unpackAs(vendor, TranscriptLineSchema);
    if (line?.line.case !== "user") return null;
    const len = userTextLen(line.line.value.message);
    return len > 0 ? { len, arm: "transcript_user_line" } : null;
  }
  if (envelopeIs(vendor, ClaudeStreamMessageSchema)) {
    const csm = unpackAs(vendor, ClaudeStreamMessageSchema);
    if (csm?.msg.case !== "user") return null;
    const len = userTextLen(csm.msg.value.message);
    return len > 0 ? { len, arm: "user_message" } : null;
  }
  return null;
}

/**
 * Prompt-text length of a user message, counted exactly as the daemon's
 * userTurnReceipt counts it: the content string, or the sum of its TEXT
 * blocks.
 *
 * Tool-result blocks contribute nothing on purpose. Pure tool feedback rides
 * the user role too, and counting it would put a "prompt forwarded" line on
 * every tool call, burying the one receipt per prompt this exists for. Such a
 * message totals 0 and the caller drops it.
 */
function userTextLen(message: ApiUserMessage | undefined): number {
  const content = message?.content;
  if (content?.case === "contentString") return content.value.length;
  if (content?.case === "contentBlocks") {
    let len = 0;
    for (const block of content.value.blocks) {
      if (block.block.case === "text") len += block.block.value.text.length;
    }
    return len;
  }
  return 0;
}

/** Map a ControlDispatch resolution onto the SDK's PermissionResult shape. */
function toPermissionResult(r: ToolPermissionResult): PermissionResultLike {
  return r.behavior === "allow"
    ? { behavior: "allow", updatedInput: r.updatedInput as Record<string, unknown> }
    : { behavior: "deny", message: r.message };
}

function errMsg(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}
