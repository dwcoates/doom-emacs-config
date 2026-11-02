/**
 * engine/parked-bridge.ts — a cold-parked conversation held reachable from
 * claude.ai (shim.v1.HoldParkedBridge).
 *
 * A workspace whose remote control is on keeps, while it is parked at its
 * cold gate, one idle vendor process resumed on the parked conversation with
 * the Remote Control bridge on (owner ruling, 2026-11-02). The session opens
 * it exactly as a `pay` answer would and sends no prompt; this module holds
 * what that opening produced and what has happened to it since:
 *
 *   - OPEN: the conversation's opening facts, kept to answer the `pay`
 *     answer's StartSession with (the answer PROMOTES the bridge rather than
 *     re-opening the conversation), and the bridge's claude.ai session.
 *   - THE REMOTE PROMPT, A LATCH. The first prompt typed on claude.ai that
 *     reaches the parked process is kept, and every watch, open now or opened
 *     later, is told it. A watch that was not open when it arrived (a daemon
 *     adopting this shim afterwards) can therefore never miss it.
 *   - PROMOTED: the answer took the session, and every watch ends.
 *
 * CONSTANT SIZE: the opening, one prompt, one flag and the open watches.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1, shimv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-engine-parked-bridge", operation: "shim.engine.parked-bridge" });

/** The prompt typed on claude.ai that reached the parked process. */
export interface ParkedRemotePrompt {
  readonly said: conversationv1.UserSaid;
  readonly clientUuid: string;
}

/** A refusal of HoldParkedBridge: the last frame of its stream. */
export type ParkedBridgeRefusal =
  | { readonly kind: "resumeFailed"; readonly detail: string }
  | { readonly kind: "vendorRefused"; readonly detail: string }
  | { readonly kind: "alreadyStarted"; readonly detail: string };

/** The one frame a refusal is. */
export function parkedBridgeRefused(refusal: ParkedBridgeRefusal): shimv1.HoldParkedBridgeResponse {
  const cause = ((): shimv1.HoldParkedBridgeRefused["cause"] => {
    switch (refusal.kind) {
      case "resumeFailed":
        return { case: "resumeFailed", value: create(shimv1.HoldParkedBridgeResumeFailedSchema, {}) };
      case "vendorRefused":
        return { case: "vendorRefused", value: create(shimv1.HoldParkedBridgeVendorRefusedSchema, {}) };
      case "alreadyStarted":
        return { case: "alreadyStarted", value: create(shimv1.HoldParkedBridgeAlreadyStartedSchema, {}) };
    }
  })();
  return create(shimv1.HoldParkedBridgeResponseSchema, {
    event: { case: "refused", value: create(shimv1.HoldParkedBridgeRefusedSchema, { cause, detail: refusal.detail }) },
  });
}

/** The parked conversation's bridge, open on an idle vendor process. */
export class ParkedBridge {
  private remote: ParkedRemotePrompt | undefined;
  private promoted = false;
  /** Woken on every change; replaced each time it fires. */
  private wake: () => void = () => undefined;
  private changed: Promise<void> = this.nextChange();

  constructor(
    /** The parked conversation, as the gate named it. */
    readonly vendorSessionId: string,
    /** The opening facts the `pay` answer's StartSession is answered with. */
    readonly started: conversationv1.SessionStarted,
    /** The bridge's claude.ai session. */
    readonly bridgeSessionId: string,
  ) {}

  /** Whether the answer has taken the session. */
  get isPromoted(): boolean {
    return this.promoted;
  }

  /** The prompt latched, when one has arrived. */
  get remotePrompt(): ParkedRemotePrompt | undefined {
    return this.remote;
  }

  /**
   * A prompt typed on claude.ai reached the parked process. Only the first is
   * latched: the gate is answered once, and every later prompt is the running
   * session's own.
   */
  remotePromptArrived(prompt: ParkedRemotePrompt): void {
    if (this.promoted) {
      throw new Error("shim parked bridge: a remote prompt was noted on a bridge the answer already promoted");
    }
    if (this.remote !== undefined) {
      LOGGER.info(
        { client_uuid: prompt.clientUuid, latched_uuid: this.remote.clientUuid },
        "another prompt typed on claude.ai reached the parked session; the gate is already being answered for the first",
      );
      return;
    }
    this.remote = prompt;
    LOGGER.info(
      { client_uuid: prompt.clientUuid, vendor_session_id: this.vendorSessionId },
      "a prompt typed on claude.ai reached the parked session; the vendor runs it now, spending the cold read, and the gate is answered pay",
    );
    this.fire();
  }

  /** The `pay` answer took the session: every watch ends. Promoted once. */
  promote(): void {
    if (this.promoted) {
      throw new Error("shim parked bridge: promoted twice");
    }
    this.promoted = true;
    this.fire();
  }

  /**
   * One watch's frames: `opened`, then the remote prompt once it has arrived
   * (at once when it already has), ending when the bridge is promoted or
   * `signal` aborts.
   */
  async *watch(alreadyOpen: boolean, signal: AbortSignal): AsyncIterable<shimv1.HoldParkedBridgeResponse> {
    yield create(shimv1.HoldParkedBridgeResponseSchema, {
      event: {
        case: "opened",
        value: create(shimv1.HoldParkedBridgeOpenedSchema, { bridgeSessionId: this.bridgeSessionId, alreadyOpen }),
      },
    });
    const aborted = new Promise<void>((resolve) => {
      if (signal.aborted) resolve();
      signal.addEventListener("abort", () => resolve(), { once: true });
    });
    let told = false;
    for (;;) {
      if (!told && this.remote !== undefined) {
        told = true;
        yield create(shimv1.HoldParkedBridgeResponseSchema, {
          event: {
            case: "remotePrompt",
            value: create(shimv1.HoldParkedBridgeRemotePromptSchema, {
              said: this.remote.said,
              clientUuid: this.remote.clientUuid,
            }),
          },
        });
      }
      if (this.promoted || signal.aborted) return;
      await Promise.race([this.changed, aborted]);
    }
  }

  private fire(): void {
    const wake = this.wake;
    this.changed = this.nextChange();
    wake();
  }

  private nextChange(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.wake = resolve;
    });
  }
}
