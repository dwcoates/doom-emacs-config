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

/** Sends a PermissionRequest to the daemon (bound to SessionServer). */
export type PermissionRequestSender = (req: PermissionRequest) => void;

export interface ControlDispatchOptions {
  /** Request-id minter; defaults to randomUUID. Injectable for tests. */
  newRequestId?: () => string;
}

interface PendingPermission {
  resolve: (result: ToolPermissionResult) => void;
  input: JsonObject;
}

const COMPONENT = "shim-control";
const LOGGER = bindLog({ component: COMPONENT, operation: "shim.control.dispatch" });

export class ControlDispatch {
  private readonly newRequestId: () => string;
  private readonly pending = new Map<string, PendingPermission>();

  constructor(
    private readonly target: SdkControlTarget,
    private readonly sendPermissionRequest: PermissionRequestSender,
    opts: ControlDispatchOptions = {},
  ) {
    this.newRequestId = opts.newRequestId ?? randomUUID;
  }

  /** Handle a SubmitPrompt; push into the SDK turn and Ack (Nack on rejection). */
  async handleSubmitPrompt(msg: SubmitPrompt): Promise<Ack | Nack> {
    if (msg.requestId.trim() === "") {
      const reason = "SubmitPrompt requires a non-empty request_id";
      LOGGER.log({ level: "error" }, reason);
      return create(NackSchema, { requestId: msg.requestId, reason });
    }
    LOGGER.log({ request_id: msg.requestId, origin: msg.origin, text_length: msg.text.length, permission_mode: msg.permissionMode || undefined }, "dispatching SubmitPrompt to SDK session");
    try {
      await this.target.submitPrompt({
        requestId: msg.requestId,
        text: msg.text,
        origin: msg.origin,
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
   * The SDK canUseTool callback. Emits a PermissionRequest to the daemon and
   * resolves only when the matching PermissionResponse arrives (or the
   * request is cancelled). One entry per request_id in the pending map.
   */
  requestPermission(toolName: string, input: JsonObject): Promise<ToolPermissionResult> {
    const requestId = this.newRequestId();
    return new Promise<ToolPermissionResult>((resolve, reject) => {
      this.pending.set(requestId, { resolve, input });
      LOGGER.log({ request_id: requestId, tool_name: toolName, pending_count: this.pending.size, input_keys: Object.keys(input) }, "permission request opened for daemon decision");
      try {
        this.sendPermissionRequest(create(PermissionRequestSchema, {
          requestId,
          toolName,
          input,
        }));
      } catch (err) {
        this.pending.delete(requestId);
        LOGGER.log({ level: "error", request_id: requestId, tool_name: toolName, input_keys: Object.keys(input), pending_count: this.pending.size, cause: err }, `permission request send failed: ${errMsg(err)}`);
        reject(err);
      }
    });
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
      LOGGER.log({ pending_count: this.pending.size, reason }, "cancelling outstanding permission requests");
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
