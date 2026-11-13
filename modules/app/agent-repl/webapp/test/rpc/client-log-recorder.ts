/**
 * clientLogRecorder — a client whose `ClientLog` records every request it is
 * handed and answers ANSWER (success by default), and whose page stream stands
 * quiet until aborted, recording the request that opened it, for a suite that
 * reads what a document logged and stated.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";

import { ClientLogResponseSchema, type ClientLogRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import type { WatchPageRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_page_pb";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { createAgentReplClient, type AgentReplClient } from "../../src/rpc/client.js";

/** What the recorder answers each request with, or the error it throws. */
export type ClientLogAnswer = MessageInitShape<typeof ClientLogResponseSchema> | Error;

/** The client, and every ClientLog and WatchPage request it was handed, oldest first. */
export interface ClientLogRecorder {
  client: AgentReplClient;
  logged: ClientLogRequest[];
  pages: WatchPageRequest[];
  /** Settles with the first WatchPage request, the instant it is handed over. */
  firstPage: Promise<WatchPageRequest>;
}

export function clientLogRecorder(
  answer: ClientLogAnswer = { result: { case: "success", value: {} } },
): ClientLogRecorder {
  const logged: ClientLogRequest[] = [];
  const pages: WatchPageRequest[] = [];
  let pageOpened: (request: WatchPageRequest) => void = () => undefined;
  const firstPage = new Promise<WatchPageRequest>((resolve) => (pageOpened = resolve));
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      clientLog(request) {
        logged.push(request);
        if (answer instanceof Error) throw answer;
        return create(ClientLogResponseSchema, answer);
      },
      async *watchPage(request, context) {
        pages.push(request);
        pageOpened(request);
        await new Promise<void>((resolve) => context.signal.addEventListener("abort", () => resolve()));
      },
    });
  });
  return { client: createAgentReplClient(transport), logged, pages, firstPage };
}
