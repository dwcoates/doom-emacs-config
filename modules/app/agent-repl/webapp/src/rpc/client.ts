/**
 * The generated AgentRepl client, and the one place it is constructed.
 *
 * Every verb the webapp calls and every stream it watches goes through this
 * type. It is threaded on the AppContext rather than imported as a singleton
 * because the graceful-rollout path REPLACES it: WatchWebWorkspace's
 * `transferred` push means the page adopts a new daemon, and a module holding
 * its own client would keep talking to the old one.
 */
import { createClient, type Client, type Transport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";

/** Every AgentRepl verb and stream, typed from the generated service. */
export type AgentReplClient = Client<typeof AgentRepl>;

/** Bind the generated service to a transport. */
export function createAgentReplClient(transport: Transport): AgentReplClient {
  return createClient(AgentRepl, transport);
}
