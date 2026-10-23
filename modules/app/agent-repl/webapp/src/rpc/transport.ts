/**
 * The webapp's one transport to the daemon.
 *
 * BINARY, NOT JSON. The library owns (de)serialization end to end — the
 * hand-rolled protojson decoder and its build-time anchoring tables are gone —
 * so the codec is a config knob rather than framing anyone maintains. Binary
 * is the smaller wire and the one whose unknown-field preservation the strict
 * layer above depends on.
 *
 * A page dials the daemon over the same origin it was loaded from. The web
 * view host dials a daemon it is told (rpc/daemon-origin.ts), which the daemon
 * answers across origins for the host alone; no credentials ride either way.
 */
import { createConnectTransport } from "@connectrpc/connect-web";
import type { Transport } from "@connectrpc/connect";
import { createLinkedAbortController } from "@connectrpc/connect/protocol";
import type { DaemonOrigin } from "./daemon-origin.js";

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

/**
 * A Connect transport pointed at a daemon: BASE is a fixed origin (a page's
 * own), or a daemon origin read at every call (the web view host's).
 */
export function createDaemonTransport(base: string | DaemonOrigin, opts: DaemonTransportOptions = {}): Transport {
  // The page's fetch is read at each call, as the library itself reads it.
  const pageFetch: typeof globalThis.fetch = opts.fetch ?? ((input, init) => globalThis.fetch(input, init));
  const baseUrl = typeof base === "string" ? base : base.current();
  // A MOVABLE ORIGIN IS READ AT EVERY CALL: the library builds each call's
  // URL on the base it was made with, which is re-pointed here.
  const fetch: typeof globalThis.fetch =
    typeof base === "string"
      ? pageFetch
      : (input, init) => {
          const url = typeof input === "string" ? input : input instanceof URL ? input.href : input.url;
          // Connect builds every call's URL on the base it was given; one
          // that is not is a broken invariant, never a URL to guess at.
          if (!url.startsWith(baseUrl)) throw new Error(`a daemon call's URL ${url} is not on its base ${baseUrl}`);
          return pageFetch(base.current() + url.slice(baseUrl.length), init);
        };
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
              // LINKED, NOT `AbortSignal.any`, which WebKitGTK 2.40 (the e2e
              // sandbox's engine) lacks: every fetch threw there, and no page
              // reached its daemon. Connect aborts each call's own signal when
              // the call completes, and the link then drops its listeners, so
              // none collects on the page's long-lived signal.
              signal: init?.signal == null ? signal : createLinkedAbortController(init.signal, signal).signal,
            }),
  });
}
