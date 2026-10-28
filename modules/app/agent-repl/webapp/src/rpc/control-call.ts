/**
 * control-call — ONE CONTROL CALL a reader's gesture makes, whose only failure
 * surface is the warning chip.
 *
 * A gesture that changes something the daemon holds (the feed's selection, a
 * merge bubble's fold, an image overlay, a response highlight) calls one unary
 * rpc and draws nothing from its answer: the change arrives on the page's
 * streams. Every such call reads its answer the same way, so they read it
 * here:
 *
 *   - a transport failure files the chip (`callUnary` already logged it);
 *   - a refusal is worded through the rpc's own sentences (`refusalOf`, which
 *     also raises the page-wide move notice), recorded at ERROR with the arm,
 *     and filed on the chip;
 *   - a result arm this build cannot read is a `MalformedView`;
 *   - success answers the success arm's value, for the caller's own record.
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";
import { ConnectError } from "@connectrpc/connect";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AgentReplClient } from "./client.js";
import type { AppContext } from "./context.js";
import { callFailure, refusalOf, type Cause, type SentenceTable } from "./refuse.js";
import { requireCase, unreachableArm } from "./strict.js";
import { callUnary } from "./unary.js";

/** A response's result oneof, as protobuf-es hands it over. */
export type ControlResult<Success> =
  | { case: "success"; value: Success }
  | { case: "error"; value: { cause: Cause } }
  | { case: undefined; value?: undefined };

/** One control call. */
export interface ControlCall<Res extends Message, Success> {
  /** The rpc's name, as its records and paths spell it ("SelectFeedRow"). */
  readonly rpc: string;
  /** What the reader asked for, in their words: the chip's request and the refusal record's verb. */
  readonly what: string;
  /** Send the request. */
  readonly send: (client: AgentReplClient) => Promise<Res>;
  /** The response's schema. */
  readonly schema: DescMessage;
  /** The response's result oneof. */
  readonly result: (response: Res) => ControlResult<Success>;
  /** The rpc's own refusal arms, in the reader's words. */
  readonly refusals: SentenceTable;
  /** The refusal record's operation. */
  readonly refusedOperation: string;
  /** What the refusal record names besides the arm. */
  readonly context: Readonly<Record<string, string | number | boolean>>;
}

/**
 * Make CALL. Answers the success arm's value, or undefined once a transport
 * failure or a refusal has been filed on the chip.
 */
export async function controlCall<Res extends Message, Success>(
  ctx: AppContext,
  call: ControlCall<Res, Success>,
): Promise<Success | undefined> {
  let response: Res;
  try {
    response = await callUnary(ctx, call.rpc, call.send, call.schema);
  } catch (err) {
    // `callUnary` already logged the transport failure; the chip is told here.
    if (!(err instanceof ConnectError)) throw err;
    ctx.failures.report(controlPlaneFailed(call.what, callFailure(err).text));
    return undefined;
  }
  const resultPath = `${call.rpc}Response.result`;
  const result = requireCase(call.result(response), resultPath);
  switch (result.case) {
    case "success":
      return result.value;
    case "error": {
      const said = refusalOf(result.value.cause, call.refusals, `${call.rpc}Error.cause`);
      log.error(`the daemon refused to ${call.what}: ${said.text}`, {
        operation: call.refusedOperation,
        context: { ...call.context, arm: said.arm },
      });
      ctx.failures.report(controlPlaneFailed(call.what, said.text));
      return undefined;
    }
    default: {
      const other: { case: string } = result;
      return unreachableArm(resultPath, other.case);
    }
  }
}
