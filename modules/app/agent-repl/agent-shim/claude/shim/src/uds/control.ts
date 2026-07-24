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
import { shimLog } from "./log.js";
import {
  Ack,
  AckSchema,
  Interrupt,
  Nack,
  NackSchema,
  PermissionDecision,
  PermissionRequest,
  PermissionRequestSchema,
  PermissionResponse,
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
  }): void;
  /** Interrupt the current turn. `hard` maps to the SDK `interrupt()`. */
  interrupt(input: { requestId: string; hard: boolean }): void;
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

  /** Handle a SubmitPrompt; push into the SDK turn and Ack (Nack on throw). */
  handleSubmitPrompt(msg: SubmitPrompt): Ack | Nack {
    try {
      this.target.submitPrompt({
        requestId: msg.requestId,
        text: msg.text,
        origin: msg.origin,
        ...(msg.permissionMode !== "" ? { permissionMode: msg.permissionMode } : {}),
      });
      return create(AckSchema, { requestId: msg.requestId });
    } catch (err) {
      shimLog(COMPONENT, { request: msg.requestId }, `submit-prompt failed: ${errMsg(err)}`);
      return create(NackSchema, { requestId: msg.requestId, reason: errMsg(err) });
    }
  }

  /** Handle an Interrupt; forward to the SDK turn and Ack (Nack on throw). */
  handleInterrupt(msg: Interrupt): Ack | Nack {
    try {
      this.target.interrupt({ requestId: msg.requestId, hard: msg.hard });
      return create(AckSchema, { requestId: msg.requestId });
    } catch (err) {
      shimLog(COMPONENT, { request: msg.requestId }, `interrupt failed: ${errMsg(err)}`);
      return create(NackSchema, { requestId: msg.requestId, reason: errMsg(err) });
    }
  }

  /**
   * The SDK canUseTool callback. Emits a PermissionRequest to the daemon and
   * resolves only when the matching PermissionResponse arrives (or the
   * request is cancelled). One entry per request_id in the pending map.
   */
  requestPermission(toolName: string, input: JsonObject): Promise<ToolPermissionResult> {
    const requestId = this.newRequestId();
    return new Promise<ToolPermissionResult>((resolve) => {
      this.pending.set(requestId, { resolve, input });
      this.sendPermissionRequest(create(PermissionRequestSchema, {
        requestId,
        toolName,
        input,
      }));
    });
  }

  /** Resolve the blocked canUseTool round-trip a PermissionResponse targets. */
  handlePermissionResponse(msg: PermissionResponse): void {
    const pending = this.pending.get(msg.requestId);
    if (!pending) {
      shimLog(COMPONENT, { request: msg.requestId }, `permission-response for unknown request_id (ignored)`);
      return;
    }
    this.pending.delete(msg.requestId);
    if (msg.decision === PermissionDecision.ALLOW) {
      // Allow-with-edits: updated_input, when present, replaces the input.
      const updatedInput = msg.updatedInput ?? pending.input;
      pending.resolve({ behavior: "allow", updatedInput });
    } else {
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
