/**
 * Control-plane glue between the daemon connection and the SDK session.
 *
 * Two responsibilities:
 * 1. Map inbound `SubmitPrompt` / `Interrupt` onto an injected SDK-session
 *    interface ({@link SdkControlTarget}) and return the synchronous
 *    `Ack`/`Nack` receipt the server writes back.
 * 2. Drive the `canUseTool` PermissionRequest round-trip: {@link requestPermission}
 *    emits a `PermissionRequest` to the daemon and BLOCKS (returns a Promise)
 *    on a pending-request map keyed by `request_id` until the matching
 *    `PermissionResponse` arrives via {@link handlePermissionResponse}.
 *    The shim is the DURABLE OWNER of that open question: while an answer is
 *    outstanding the request is re-sent on every reattach
 *    ({@link resendPending}), so neither an unattached daemon nor a daemon that
 *    died holding the ask can strand the turn.
 *
 * This deliberately does NOT import src/session.ts. {@link SdkControlTarget}
 * is the minimal seam the stitch phase binds to the real ShimSession.
 */
import { randomUUID } from "node:crypto";
import { create } from "@bufbuild/protobuf";
import type { JsonObject } from "@bufbuild/protobuf";
import { bindLog } from "./log.js";
import { normalizeModel } from "../model.js";
import {
  Ack,
  AckSchema,
  Interrupt,
  InterruptOutcome,
  Nack,
  NackSchema,
  PermissionDecision,
  PermissionRequest,
  PermissionRequestSchema,
  PermissionResponse,
  PromptOrigin,
  PromptOriginSchema,
  QuerySelectedModel,
  SetModel,
  SubmitPrompt,
} from "./proto.js";

/**
 * The minimal SDK-session surface control dispatch drives. The stitch phase
 * binds this to the real ShimSession (streaming-input push + interrupt).
 */
export interface SdkControlTarget {
  /** Push one user prompt into the SDK streaming-input turn. */
  submitPrompt(input: {
    requestId: string;
    text: string;
    origin: string;
    promptOrigin: PromptOrigin;
    permissionMode?: string;
  }): Promise<void>;
  /**
   * Interrupt the current turn via the SDK `interrupt()`.
   *
   * Returns the THREE-VALUED outcome, decided SYNCHRONOUSLY. The
   * implementation owns turn lifecycle, so only it can answer "was a turn
   * live when this landed?" without racing — see the emitter in
   * uds-session.ts for why that read must precede every await.
   */
  interrupt(input: { requestId: string }): InterruptOutcome;
  /** Set a model and return the SDK-confirmed selection. */
  setModel(input: { requestId: string; model: string }): Promise<string>;
  /**
   * The model this session is CURRENTLY running.
   *
   * A READ, not a change, and synchronous: the implementation already holds
   * the answer — it tracks every SDK `system:init` and every confirmed
   * `setModel` — so there is nothing to await and nothing to race. Empty means
   * the shim has never observed a real model, which is a refusal rather than
   * an answer (see handleQuerySelectedModel).
   */
  selectedModel(): string;
}

/**
 * A rejected model mutation can still report the SDK's current selection.
 * That value is authoritative state, not an optimistic echo of the request.
 */
export class ModelSelectionError extends Error {
  constructor(message: string, readonly selectedModel: string) {
    super(message);
    this.name = "ModelSelectionError";
  }
}

/** The shim's own resolution of a canUseTool request. */
export type ToolPermissionResult =
  | { behavior: "allow"; updatedInput: JsonObject }
  | { behavior: "deny"; message: string };

/**
 * Sends a PermissionRequest to the daemon (bound to SessionServer).
 *
 * Returns whether the frame reached a live daemon connection. `false` is NOT a
 * failure to be swallowed: the request stays pending here and is re-sent by
 * {@link ControlDispatch.resendPending} on the next reattach.
 */
export type PermissionRequestSender = (req: PermissionRequest) => boolean;

export interface ControlDispatchOptions {
  /** Request-id minter; defaults to randomUUID. Injectable for tests. */
  newRequestId?: () => string;
  /**
   * Whether an SDK turn is currently in flight. A pending permission belongs to
   * the turn whose tool call raised it, so a re-send with no live turn would be
   * re-asking a dead turn's question — {@link ControlDispatch.resendPending}
   * cancels those waits instead of putting them back in front of the user.
   *
   * Omitted, liveness is unknown and re-sends are unconditional (the old
   * behavior, and the right default for a dispatch with no turn owner bound).
   */
  isTurnLive?: () => boolean;
}

interface PendingPermission {
  resolve: (result: ToolPermissionResult) => void;
  input: JsonObject;
  /**
   * The EXACT frame first sent for this request. A re-send replays this value
   * rather than rebuilding one, so the identity the daemon rendezvouses on
   * (request_id) and everything it renders (tool name, input) are bit-identical
   * across every attempt.
   */
  request: PermissionRequest;
}

const COMPONENT = "shim-control";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.control.dispatch" });

export class ControlDispatch {
  private readonly newRequestId: () => string;
  private readonly isTurnLive: (() => boolean) | undefined;
  /**
   * Insertion-ordered by construction (Map iteration order), which is what
   * makes a multi-request re-send arrive in the order the user was originally
   * asked.
   */
  private readonly pending = new Map<string, PendingPermission>();

  constructor(
    private readonly target: SdkControlTarget,
    private readonly sendPermissionRequest: PermissionRequestSender,
    opts: ControlDispatchOptions = {},
  ) {
    this.newRequestId = opts.newRequestId ?? randomUUID;
    this.isTurnLive = opts.isTurnLive;
  }

  /** Handle a SubmitPrompt; push into the SDK turn and Ack (Nack on rejection). */
  async handleSubmitPrompt(msg: SubmitPrompt): Promise<Ack | Nack> {
    if (msg.requestId.trim() === "") {
      const reason = "SubmitPrompt requires a non-empty request_id";
      LOGGER.log({ level: "error" }, reason);
      return create(NackSchema, { requestId: msg.requestId, reason });
    }
    LOGGER.log({ request_id: msg.requestId, origin: msg.origin, prompt_origin: msg.promptOrigin, text_length: msg.text.length, permission_mode: msg.permissionMode || undefined }, "dispatching SubmitPrompt to SDK session");
    if (msg.promptOrigin === PromptOrigin.UNSPECIFIED || !PromptOriginSchema.values.some((value) => value.number === msg.promptOrigin)) {
      const reason = `SubmitPrompt ${JSON.stringify(msg.requestId)} has invalid prompt_origin ${msg.promptOrigin}`;
      LOGGER.log({ level: "error", request_id: msg.requestId, prompt_origin: msg.promptOrigin }, reason);
      return create(NackSchema, { requestId: msg.requestId, reason });
    }
    try {
      await this.target.submitPrompt({
        requestId: msg.requestId,
        text: msg.text,
        origin: msg.origin,
        promptOrigin: msg.promptOrigin,
        ...(msg.permissionMode !== "" ? { permissionMode: msg.permissionMode } : {}),
      });
      LOGGER.log({ request_id: msg.requestId }, "SubmitPrompt accepted by SDK session");
      return create(AckSchema, { requestId: msg.requestId });
    } catch (err) {
      LOGGER.log({ level: "error", request_id: msg.requestId, cause: err }, `submit-prompt failed: ${errMsg(err)}`);
      return create(NackSchema, { requestId: msg.requestId, reason: errMsg(err) });
    }
  }

  /**
   * Handle an Interrupt; forward to the SDK turn and Ack with the outcome
   * the target decided (Nack on throw).
   *
   * THE AMBIGUITY THIS REMOVES. A consumer watching for an `aborted` result
   * sees the turn end and the interrupt arrive as two unordered events, and
   * so cannot tell a stop that FAILED from a turn that had ALREADY ENDED —
   * it painted the second as the first. The shim owns turn lifecycle and its
   * event loop is single-threaded, so the answer is unambiguous HERE and
   * nowhere downstream. Carrying it on the Ack is what stops every consumer
   * from re-deriving it wrongly.
   *
   * A synchronous throw stays a NACK rather than becoming an Ack carrying
   * FAILED: a Nack is the stronger, already-established failure signal on
   * this wire, and downgrading it to a successful receipt with a sad field
   * would weaken error coverage the daemon already acts on. FAILED is for
   * the case the target can prove undeliverable while still answering.
   */
  handleInterrupt(msg: Interrupt): Ack | Nack {
    LOGGER.log({ request_id: msg.requestId }, "dispatching Interrupt to SDK session");
    try {
      const outcome = this.target.interrupt({ requestId: msg.requestId });
      LOGGER.log({ request_id: msg.requestId, interrupt_outcome: outcome }, "Interrupt resolved by SDK session");
      return create(AckSchema, { requestId: msg.requestId, interruptOutcome: outcome });
    } catch (err) {
      LOGGER.log({ level: "error", request_id: msg.requestId, cause: err }, `interrupt failed: ${errMsg(err)}`);
      return create(NackSchema, { requestId: msg.requestId, reason: errMsg(err) });
    }
  }

  /** Apply a deliberate model request only after the SDK settles it. */
  async handleSetModel(msg: SetModel): Promise<Ack | Nack> {
    const model = normalizeModel(msg.model);
    if (model === "") {
      const reason = "set-model requires a non-empty model id; <synthetic> is not selectable";
      LOGGER.log({ level: "error", request_id: msg.requestId, requested_model: msg.model }, reason);
      return create(NackSchema, { requestId: msg.requestId, reason });
    }
    try {
      const selectedModel = normalizeModel(await this.target.setModel({ requestId: msg.requestId, model }));
      if (selectedModel === "") {
        throw new Error("SDK acknowledged SetModel without a real selected model");
      }
      return create(AckSchema, { requestId: msg.requestId, selectedModel });
    } catch (err) {
      const reason = errMsg(err);
      const selectedModel = err instanceof ModelSelectionError ? normalizeModel(err.selectedModel) : "";
      LOGGER.log({ level: "error", request_id: msg.requestId, model, selected_model: selectedModel }, `set-model failed: ${reason}`);
      return create(NackSchema, { requestId: msg.requestId, reason, selectedModel });
    }
  }

  /**
   * Answer which model this session is currently running.
   *
   * WHY THE DAEMON ASKS. The bare argument-less `/model` opens the CLI's own
   * picker, which no layer above can intercept: without this read the daemon
   * learns the resulting model only when some later submit happens to
   * re-announce a `system:init`, so the topbar picker names the OLD model
   * until then and a hibernation before that respawns the session on it.
   *
   * AN UNKNOWN MODEL IS A NACK, NEVER AN EMPTY ACK. An Ack carrying no
   * selection would be indistinguishable from a confirmed absence, and the
   * daemon would have to guess which it was; the whole point of the read is
   * that the value it commits was verified. A refusal is the honest answer and
   * the daemon surfaces it as a failure.
   */
  handleQuerySelectedModel(msg: QuerySelectedModel): Ack | Nack {
    try {
      const selectedModel = normalizeModel(this.target.selectedModel());
      if (selectedModel === "") {
        const reason = "query-selected-model: this session has not observed a real model yet";
        LOGGER.log({ level: "error", request_id: msg.requestId }, reason);
        return create(NackSchema, { requestId: msg.requestId, reason });
      }
      LOGGER.log({ request_id: msg.requestId, selected_model: selectedModel }, "reported the live selected model");
      return create(AckSchema, { requestId: msg.requestId, selectedModel });
    } catch (err) {
      const reason = errMsg(err);
      LOGGER.log({ level: "error", request_id: msg.requestId, cause: err }, `query-selected-model failed: ${reason}`);
      return create(NackSchema, { requestId: msg.requestId, reason });
    }
  }

  /**
   * The SDK canUseTool callback. Emits a PermissionRequest to the daemon and
   * resolves only when the matching PermissionResponse arrives (or the
   * request is cancelled). One entry per request_id in the pending map.
   */
  requestPermission(toolName: string, input: JsonObject): Promise<ToolPermissionResult> {
    const requestId = this.newRequestId();
    const request = create(PermissionRequestSchema, { requestId, toolName, input });
    return new Promise<ToolPermissionResult>((resolve, reject) => {
      this.pending.set(requestId, { resolve, input, request });
      LOGGER.log({ request_id: requestId, tool_name: toolName, pending_count: this.pending.size, input_keys: Object.keys(input) }, "permission request opened for daemon decision");
      try {
        if (!this.sendPermissionRequest(request)) {
          // UNDELIVERED, NOT LOST. There is no daemon attached to answer, so
          // the ask is retained here and re-sent by resendPending the moment
          // one reattaches. Rejecting instead would fail the tool call for a
          // transport gap the user never chose.
          LOGGER.log({ level: "error", request_id: requestId, tool_name: toolName, pending_count: this.pending.size },
            "permission request could not be delivered (no daemon attached); it stays pending and is re-sent on reattach");
        }
      } catch (err) {
        this.pending.delete(requestId);
        LOGGER.log({ level: "error", request_id: requestId, tool_name: toolName, input_keys: Object.keys(input), pending_count: this.pending.size, cause: err }, `permission request send failed: ${errMsg(err)}`);
        reject(err);
      }
    });
  }

  /**
   * Re-send every still-unanswered permission request to the daemon.
   *
   * Called once per completed reattach (after ShimReady closes the bring-up
   * gate). It closes BOTH halves of the rendezvous gap: a request raised while
   * no daemon was attached was never delivered, and a request the previous
   * daemon received died with that daemon's in-memory rendezvous. Neither the
   * daemon nor the frontend can recover the question — only the side still
   * holding the blocked `canUseTool` promise can, and that is this map.
   *
   * WHY THE SAME FRAME AND THE SAME request_id. The re-send is not a new ask;
   * it is the same one, restated. Identity is what lets the daemon re-arm
   * idempotently: an id it has no waiter for re-establishes the wait, an id it
   * already answered replays the recorded answer instead of asking a human the
   * same question twice.
   *
   * NO INTERLEAVING WITH AN ANSWER. This is synchronous over the map on a
   * single-threaded event loop, so an answer that arrives just before it runs
   * has already removed its entry, and one that arrives just after resolves a
   * request whose re-send is merely redundant. Either way exactly one answer
   * resolves the wait.
   *
   * A DEAD TURN IS CANCELLED, NOT RE-ASKED. If no SDK turn is in flight the
   * tool call these requests block no longer exists, so re-asking would put a
   * question in front of the user that nothing is waiting on. Those waits are
   * force-denied through the same path an interrupt uses.
   */
  resendPending(reason: string): void {
    if (this.pending.size === 0) return;
    if (this.isTurnLive !== undefined && !this.isTurnLive()) {
      this.cancelAll(`${reason}: no turn is in flight to receive the answer`);
      return;
    }
    LOGGER.log({
      pending_count: this.pending.size,
      reason,
      request_ids: [...this.pending.keys()],
    }, "re-sending every unanswered permission request to the reattached daemon");
    for (const [requestId, pending] of this.pending) {
      try {
        if (!this.sendPermissionRequest(pending.request)) {
          LOGGER.log({ level: "error", request_id: requestId, tool_name: pending.request.toolName, reason },
            "permission request re-send found no daemon attached; it stays pending for the next reattach");
        }
      } catch (err) {
        // A throwing send is a transport fault, not an answer. The request
        // stays pending so the next reattach re-sends it; failing the tool
        // call here would decide on the user's behalf.
        LOGGER.log({ level: "error", request_id: requestId, tool_name: pending.request.toolName, reason, cause: err },
          `permission request re-send failed: ${errMsg(err)}`);
      }
    }
  }

  /** Resolve the blocked canUseTool round-trip a PermissionResponse targets. */
  handlePermissionResponse(msg: PermissionResponse): void {
    const pending = this.pending.get(msg.requestId);
    if (!pending) {
      LOGGER.log({ level: "warn", request_id: msg.requestId }, `permission-response for unknown request_id (ignored)`);
      return;
    }
    this.pending.delete(msg.requestId);
    if (msg.decision === PermissionDecision.ALLOW) {
      // Allow-with-edits: updated_input, when present, replaces the input.
      const updatedInput = msg.updatedInput ?? pending.input;
      LOGGER.log({ request_id: msg.requestId, decision: "allow", edited: msg.updatedInput !== undefined, pending_count: this.pending.size }, "permission request resolved");
      pending.resolve({ behavior: "allow", updatedInput });
    } else {
      LOGGER.log({ request_id: msg.requestId, decision: "deny", pending_count: this.pending.size }, "permission request resolved");
      pending.resolve({
        behavior: "deny",
        message: msg.denyMessage !== "" ? msg.denyMessage : "permission denied",
      });
    }
  }

  /**
   * Cancel every outstanding permission wait (interrupt / disconnect /
   * shutdown), resolving each as a deny so no SDK callback hangs forever.
   */
  cancelAll(reason: string): void {
    if (this.pending.size > 0) {
      // Each cancelled wait is a tool DENIED without the user ever being
      // asked, so this is a decision made on their behalf rather than a
      // lifecycle note. The request ids go with it: the bounded set is what
      // lets an operator tell which asks were answered for them.
      LOGGER.log({
        level: "warn",
        pending_count: this.pending.size,
        reason,
        request_ids: [...this.pending.keys()],
      }, "cancelling outstanding permission requests; each is force-denied without the user seeing the prompt");
    }
    for (const [id, pending] of this.pending) {
      this.pending.delete(id);
      pending.resolve({ behavior: "deny", message: reason });
    }
  }

  /** Count of in-flight permission round-trips (for tests / diagnostics). */
  pendingCount(): number {
    return this.pending.size;
  }
}

function errMsg(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}
