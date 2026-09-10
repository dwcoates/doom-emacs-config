/**
 * The webapp's one transport to the daemon.
 *
 * BINARY, NOT JSON. The library owns (de)serialization end to end — the
 * hand-rolled protojson decoder and its build-time anchoring tables are gone —
 * so the codec is a config knob rather than framing anyone maintains. Binary
 * is the smaller wire and the one whose unknown-field preservation the strict
 * layer above depends on.
 *
 * The daemon serves Connect over the same origin the page was loaded from, so
 * there is no cross-origin credential story here and none is invented.
 */
import { createConnectTransport } from "@connectrpc/connect-web";
import type { Transport } from "@connectrpc/connect";

export interface DaemonTransportOptions {
  /**
   * The fetch implementation, for a test that hosts the daemon in the SAME
   * process: jsdom's fetch cannot reach a node http server, so the suites hand
   * Node's own fetch (or connect-node's) in. Production leaves it unset and
   * gets the page's `globalThis.fetch`.
   */
  fetch?: typeof globalThis.fetch;
}

/** A Connect transport pointed at the daemon serving this page. */
export function createDaemonTransport(baseUrl: string, opts: DaemonTransportOptions = {}): Transport {
  return createConnectTransport({
    baseUrl,
    useBinaryFormat: true,
    ...(opts.fetch !== undefined ? { fetch: opts.fetch } : {}),
  });
}
