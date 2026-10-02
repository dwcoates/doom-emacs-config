/**
 * The ONE pick path every topbar selector sends through: the model selector,
 * the effort selector and the permission-mode picker.
 *
 * Each of the three echoes a served token to its own rpc and reads the same
 * answer shape — `success` (nothing drawn; the new state arrives on the topbar
 * stream) or `error{cause}` (a typed refusal drawn at the control). The
 * clearing, the in-flight disabling, the machinery-versus-refusal split and the
 * unreadable-refusal guard were three hand-kept copies of one sequence, and
 * three copies is how one comes to answer a refusal differently from the rest.
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";
import { release, whileInFlight } from "../feed/cards/controls.js";
import type { Control } from "../control.js";
import type { AgentReplClient } from "../rpc/client.js";
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
import type { TopbarContext } from "./context.js";

/** A selector rpc's answer: the arm is the outcome. */
export interface PickResponse extends Message {
  readonly result:
    | { readonly case: "success"; readonly value: unknown }
    | { readonly case: "error"; readonly value: { readonly cause: Cause } }
    | { readonly case: undefined; readonly value?: undefined };
}

/** One selector's pick: what to send, and how its refusals read. */
export interface Pick<Res extends PickResponse> {
  /** The rpc's name, as the log and the refusal record state it. */
  readonly rpc: string;
  /** The call itself, echoing the served token. */
  readonly send: (client: AgentReplClient) => Promise<Res>;
  /** The response's schema, for the strict decode. */
  readonly schema: DescMessage;
  /** The rpc's own causes; the cross-cutting four are read for every rpc. */
  readonly causes: SentenceTable;
  /** The operation an unreadable answer is filed under ("topbar.model-pick"). */
  readonly malformedOperation: string;
  /** The operation an unreadable refusal is filed under. */
  readonly unreadableOperation: string;
  /** What a refusal does beyond its sentence at the control, by arm. */
  readonly onRefused?: (arm: string) => void;
}

/** Send PICK from the control WRAP, whose BUTTON and ROW were clicked. */
export async function sendPick<Res extends PickResponse>(
  pick: Pick<Res>,
  tc: TopbarContext,
  wrap: HTMLElement,
  button: Control,
  row: Control,
): Promise<void> {
  // CLEARED BEFORE THE CALL, never after: a refusal from the previous pick
  // standing beside the control the reader just clicked again reads as the
  // answer to the NEW click.
  clearRefusals(wrap);
  const answered = await whileInFlight([row, button], () => callUnary(tc.ctx, pick.rpc, pick.send, pick.schema));
  if ("failed" in answered) {
    // AN ANSWER THIS BUILD CANNOT READ IS MACHINERY, not the daemon refusing:
    // it is filed as `frame_undecodable` through the one click guard and
    // nothing is drawn at the control, because there is no refusal to state.
    if (isMalformedView(answered.failed)) {
      await guardMalformed(tc.ctx, pick.malformedOperation, Promise.reject(answered.failed));
      return;
    }
    drawTransportRefusal(wrap);
    return;
  }
  try {
    const result = requireCase(answered.value.result, `${pick.rpc}Response.result`);
    switch (result.case) {
      case "success":
        // Nothing is drawn: the new state arrives on the topbar stream. The
        // reveal closes, because the reader's question has been answered.
        tc.reveals.close();
        return;
      case "error": {
        const arm = drawTypedRefusal(wrap, `${pick.rpc}Error.cause`, pick.rpc, result.value.cause, pick.causes);
        pick.onRefused?.(arm);
        release([row, button]);
        return;
      }
      default: {
        const other: { case: string } = result;
        return unreachableArm(`${pick.rpc}Response.result`, other.case);
      }
    }
  } catch (err) {
    release([row, button]);
    if (!drawUnreadableRefusal(tc.ctx, wrap, pick.unreadableOperation, err)) throw err;
  }
}
