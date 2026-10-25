/**
 * gesture — a verb whose answer is the NEXT VIEW the daemon pushes: a click
 * that selects, switches or shows something, whose refusal draws nothing of
 * its own (the roster or the chat view that follows is the answer).
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";
import type { AgentReplClient } from "./client.js";
import { noteRefusedArm } from "./refusal.js";
import { requireCase } from "./strict.js";
import { callUnary, type UnaryContext } from "./unary.js";

/** A verb's response: its outcome oneof, as every response carries one. */
type GestureResponse = Message & { result: { case?: string | undefined; value?: unknown } };

/**
 * Send gesture RPC through FN: a refusal is recorded at INFO under OPERATION
 * (noteRefusedArm) and answered as its arm; a success answers null, as does a
 * transport failure, which callUnary records.
 */
export function sendGesture<Res extends GestureResponse>(
  ctx: UnaryContext,
  rpc: string,
  operation: string,
  fn: (client: AgentReplClient) => Promise<Res>,
  schema: DescMessage,
): Promise<string | null> {
  return callUnary(ctx, rpc, fn, schema).then(
    (response) => noteRefusedArm(rpc, operation, requireCase(response.result, `${rpc}Response.result`)),
    () => null, // the transport failure is callUnary's to record
  );
}
