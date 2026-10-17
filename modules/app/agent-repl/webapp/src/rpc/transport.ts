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
  /**
   * Aborts every request the transport makes: the page's end
   * (page-teardown.ts `pageEnding`), so nothing the page asked for answers
   * into a page that is gone.
   */
  signal?: AbortSignal;
}

/** A Connect transport pointed at the daemon serving this page. */
export function createDaemonTransport(baseUrl: string, opts: DaemonTransportOptions = {}): Transport {
  // The page's fetch is read at each call, as the library itself reads it.
  const fetch: typeof globalThis.fetch = opts.fetch ?? ((input, init) => globalThis.fetch(input, init));
  const signal = opts.signal;
  return createConnectTransport({
    baseUrl,
    useBinaryFormat: true,
    fetch:
      signal === undefined
        ? fetch
        : (input, init) =>
            fetch(input, {
              ...init,
              signal: init?.signal == null ? signal : AbortSignal.any([init.signal, signal]),
            }),
  });
}
