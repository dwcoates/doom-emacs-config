/**
 * callUnary — every verb the webapp calls goes through here.
 *
 * EVERY CLICK IS AN RPC, and every response is `oneof result { success |
 * error }`. Three things are therefore true of every call and are done once,
 * here, rather than at forty call sites:
 *
 *   - the response passes the deep unknown-field check before anything reads
 *     it, so a newer daemon's extra facts are a loud refusal rather than a
 *     silently incomplete screen;
 *   - the `result` oneof must be SET — an unset one is a malformed view, not
 *     an implicit success and not an implicit failure;
 *   - the call and its outcome ARM are logged once, by this layer, with the
 *     rpc name; the caller then switches the arm and renders the refusal at
 *     its own call site.
 *
 * WHAT IS NOT DONE HERE: the error arm is NOT thrown. A `<Method>Error` is an
 * ANSWER — the daemon refused, and the refusal renders at the clicked control.
 * Only a TRANSPORT failure throws, as a ConnectError the caller may report
 * through `ctx.failures`.
 */
import { ConnectError } from "@connectrpc/connect";
import type { DescMessage, Message } from "@bufbuild/protobuf";
import { log } from "../log.js";
import { MalformedView } from "./malformed.js";
import { assertNoUnknownFields, requireCase } from "./strict.js";
import type { AgentReplClient } from "./client.js";

/** The slice of the context a unary call needs; the whole AppContext fits. */
export interface UnaryContext {
  readonly client: AgentReplClient;
}

/**
 * Issue one verb. NAME is the rpc's own name ("SubmitPrompt") and rides every
 * log record; SCHEMA is the RESPONSE schema the strict check walks.
 */
export async function callUnary<Res extends Message>(
  ctx: UnaryContext,
  name: string,
  fn: (client: AgentReplClient) => Promise<Res>,
  schema: DescMessage,
): Promise<Res> {
  log("debug", `calling ${name}`, { operation: "rpc.unary-call", context: { rpc: name } });
  let response: Res;
  try {
    response = await fn(ctx.client);
  } catch (err) {
    // The transport failed, so there is no answer to switch on. Logged here,
    // once, by the layer that owns the call; rethrown as ConnectError so every
    // caller has one type to catch regardless of what the fetch threw.
    const connectError = ConnectError.from(err);
    log("error", `${name} failed at the transport: ${connectError.message}`, {
      operation: "rpc.unary-transport-failure",
      context: { rpc: name, code: connectError.code, cause: connectError.rawMessage },
    });
    throw connectError;
  }
  assertNoUnknownFields(schema, response);
  const arm = outcomeArm(schema, response, name);
  log("debug", `${name} answered ${arm}`, {
    operation: "rpc.unary-answered",
    context: { rpc: name, outcome: arm },
  });
  return response;
}

/**
 * The `result` arm name, for the log record.
 *
 * A response WITHOUT a `result` oneof is legitimate (the duplex login stream's
 * messages, for instance), so the oneof is looked up on the SCHEMA rather than
 * assumed: present means it must be set, absent means there is no outcome arm
 * to name and the call is logged as `answered`.
 */
function outcomeArm(schema: DescMessage, response: Message, name: string): string {
  if (!schema.oneofs.some((oneof) => oneof.name === "result")) return "answered";
  const result = (response as { result?: { case?: string } }).result;
  if (result === undefined) {
    throw new MalformedView(`${name}Response.result`, "the result oneof is absent");
  }
  return requireCase(result, `${name}Response.result`).case;
}
