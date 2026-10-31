/**
 * THE ONE CLICK PATH of a topbar chip whose click turns a mode over (the
 * persistent-wifi chip and the remote-control chip).
 *
 * The daemon resolves the toggle from the mode it reads, so the click sends
 * one rpc and the chip redraws from the push the change causes. Between the
 * click and the answer the chip pulses (`data-settling`) and its button is
 * disabled; the answer is the one signal of "settled" this end has. A refusal
 * is drawn inside the chip's wrap beside its button, never inside the button
 * it answers.
 *
 * Each chip keeps its own click record and its own success record (what a
 * success carries differs); everything between them is here.
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";

import type { Control } from "../control.js";
import { whileInFlight } from "../feed/cards/controls.js";
import { guardMalformed } from "../rpc/guard.js";
import { isMalformedView } from "../rpc/malformed.js";
import {
  clearRefusals,
  drawTransportRefusal,
  drawTypedRefusal,
  drawUnreadableRefusal,
  type Cause,
  type SentenceTable,
} from "../rpc/refuse.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import type { AgentReplClient } from "../rpc/client.js";
import type { TopbarContext } from "./context.js";

/** A toggle response's result oneof: a success, or an error with a typed cause. */
export type ChipToggleResult =
  | { case: "success"; value: unknown }
  | { case: "error"; value: { cause: Cause } }
  | { case: undefined; value?: undefined };

/** A toggle response: a message whose result is {@link ChipToggleResult}. */
export type ChipToggleResponse = Message & { result: ChipToggleResult };

/** The success a toggle response RES carries. */
export type ChipToggleSuccess<Res extends ChipToggleResponse> = Extract<Res["result"], { case: "success" }>["value"];

/** One chip's toggle rpc, as {@link sendChipToggle} sends it. */
export interface ChipToggle<Res extends ChipToggleResponse> {
  /** The endpoint's name: the rpc sent, and the root of every path read. */
  readonly rpc: string;
  /** The chip's operation, e.g. `topbar.remote-control`; records add a suffix. */
  readonly operation: string;
  readonly send: (client: AgentReplClient) => Promise<Res>;
  readonly schema: DescMessage;
  /** The causes only this endpoint answers with. */
  readonly causes: SentenceTable;
  /** States a success; reads it strictly, so a malformed one throws. */
  readonly onSuccess: (success: ChipToggleSuccess<Res>) => void;
}

/**
 * Send CHIP's toggle and state what it answered at CHIP.
 *
 * CLEARED BEFORE THE CALL: a refusal standing beside the chip the reader just
 * clicked again reads as the answer to the NEW click. THE BUTTON COMES BACK ON
 * EVERY ANSWER: a toggle stays a legitimate click after it lands, and the
 * redraw that would replace the chip is the push of a CHANGED mode, which an
 * answer does not promise.
 */
export async function sendChipToggle<Res extends ChipToggleResponse>(
  tc: TopbarContext,
  chip: HTMLElement,
  button: Control,
  toggle: ChipToggle<Res>,
): Promise<void> {
  clearRefusals(chip);
  chip.setAttribute("data-settling", "");
  let answered: { value: Res } | { failed: unknown };
  try {
    answered = await whileInFlight([button], () => callUnary(tc.ctx, toggle.rpc, toggle.send, toggle.schema));
  } finally {
    chip.removeAttribute("data-settling");
  }
  if ("failed" in answered) {
    if (isMalformedView(answered.failed)) {
      await guardMalformed(tc.ctx, `${toggle.operation}-toggle`, Promise.reject(answered.failed));
      return;
    }
    drawTransportRefusal(chip, answered.failed);
    return;
  }
  button.disabled = false;
  try {
    const result = requireCase(answered.value.result, `${toggle.rpc}Response.result`);
    switch (result.case) {
      case "success":
        toggle.onSuccess(result.value);
        return;
      case "error":
        drawTypedRefusal(chip, `${toggle.rpc}Error.cause`, toggle.rpc, result.value.cause, toggle.causes);
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm(`${toggle.rpc}Response.result`, other.case);
      }
    }
  } catch (err) {
    if (!drawUnreadableRefusal(tc.ctx, chip, `${toggle.operation}-malformed-answer`, err)) {
      throw err;
    }
  }
}
