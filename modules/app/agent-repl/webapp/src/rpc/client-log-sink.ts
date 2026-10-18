/**
 * clientLogSink — the webapp logger's sink: one `ClientLog` call per record.
 *
 * Both of the webapp's documents log through it: a workspace page files its
 * records under its workspace, and the webhost, which owns no workspace (it
 * draws the sidebar for every one), files its records centrally. The caller
 * names which, as `ClientLogRequest.source`.
 */
import type { MessageInitShape } from "@bufbuild/protobuf";
import type { ClientLogRequestSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import type { ClientLogSink } from "../log.js";
import type { AgentReplClient } from "./client.js";
import { reportClientFailure } from "./link.js";
import { abandonedAtPageEnd, pageEnded } from "../page-teardown.js";

/** Whose records these are: `ClientLogRequest.source`, as a caller writes it. */
export type ClientLogSource = NonNullable<MessageInitShape<typeof ClientLogRequestSchema>["source"]>;

/**
 * The logger's sink: one `ClientLog` call per record.
 *
 * DELIBERATELY NOT THROUGH `callUnary`. That helper logs the call it is
 * making, so routing the log sink through it would log every log — and its
 * strict check would refuse a `ClientLogResponse` by throwing into the
 * logger's own failure path. This is the one rpc the app makes without it; the
 * rejection is what `ForwardingLogger` counts.
 */
export function clientLogSink(getClient: () => AgentReplClient, source: ClientLogSource): ClientLogSink {
  return async (record) => {
    let response;
    try {
      response = await getClient().clientLog({ source, record });
    } catch (err) {
      // THE PAGE ENDED with the record in flight (page-teardown.ts): no page is
      // left to say the sink is down to.
      if (pageEnded()) return abandonedAtPageEnd();
      // THE SINK ITSELF IS DOWN. `ForwardingLogger` counts the rejection and
      // says so on the console exactly once, and nothing else knew (the
      // audit's N2 row 10) -- so the footer is told, with a FIXED line. The
      // report logs, that log is forwarded through this same sink, and it
      // fails again: an identical repeat is dropped by `reportClientFailure`,
      // which is what makes that loop terminate. The rejection is rethrown
      // unchanged, because the logger's own count is still its to keep.
      reportClientFailure("client_log_failed", "ClientLog forwarding failed");
      throw err;
    }
    // `unknown_workspace` is TERMINAL, not a failure: this page's workspace has
    // been closed or forgotten and the daemon has nowhere to file the record.
    // The logger reads it as the cue to stop forwarding; every other arm is a
    // refusal of one record, which leaves forwarding alone.
    return response.result.case === "error" &&
      response.result.value.cause.case === "unknownWorkspace"
      ? "workspace_departed"
      : "accepted";
  };
}
