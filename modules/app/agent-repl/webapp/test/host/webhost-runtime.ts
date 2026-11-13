/**
 * A web view's runtime, as its boot makes it before its code loads
 * (src/host/document.ts `startWebhostRuntime`), over a quiet recording client
 * and keeping the suite's own page-wide logger, for a suite that starts a
 * document without dialing a daemon.
 */
import type { ClientLogRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import type { WatchPageRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_page_pb";
import { startWebhostRuntime, type WebhostDocument, type WebhostRuntime, type WebhostRuntimeDeps } from "../../src/host/document.js";
import { clientLogRecorder } from "../rpc/client-log-recorder.js";

/**
 * DOCUMENT's runtime and every ClientLog and WatchPage request its client was
 * handed.
 * INSTALL_LOGGER, when given, is handed the runtime's logger; none is
 * installed when absent.
 */
export function quietWebhostRuntime(
  document: WebhostDocument = "host",
  installLogger: WebhostRuntimeDeps["installLogger"] = () => undefined,
): {
  runtime: WebhostRuntime;
  logged: ClientLogRequest[];
  pages: WatchPageRequest[];
  firstPage: Promise<WatchPageRequest>;
} {
  const { client, logged, pages, firstPage } = clientLogRecorder();
  const runtime = startWebhostRuntime(document, window, { client, installLogger });
  return { runtime, logged, pages, firstPage };
}
