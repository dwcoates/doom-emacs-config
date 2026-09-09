/**
 * directPageStreams — the page's stream, SHORT-CIRCUITED, for a unit test that
 * scripts one rpc.
 *
 * Production multiplexes every standing watch onto `WatchPage` because a
 * browser holds about six connections per host over HTTP/1.1 and a
 * server-streaming call pins one for its whole life (src/rpc/page-streams.ts).
 * That is a property of the SOCKET, and a unit test running against a router
 * transport in one process has no socket and no cap: scripting `WatchFooter`
 * and then reaching it through a page mux would test the mux, not the footer.
 *
 * So a component test hands its context this, which calls the rpc the
 * subscription stands for directly. The mux itself is covered where it belongs
 * — by `test/rpc/page-streams.test.ts` for its own behavior, and by the
 * webapp-layer suites, which run the real page against a real daemon.
 *
 * IT IS A TEST HELPER AND ONLY THAT. `createAppContext` opens the real stream
 * unless a caller passes a substitute, and nothing but a test passes one.
 */
import type {
  PageStreams,
  PageWatchKind,
  PageWatchRequest,
  PageWatchResponse,
} from "../../src/rpc/page-streams.js";
import type { AgentReplClient } from "../../src/rpc/client.js";

/** Which rpc each subscription arm stands for. */
const RPC_FOR_KIND = {
  roster: "watchWorkspaceRoster",
  webWorkspace: "watchWebWorkspace",
  daemon: "watchDaemon",
  topbar: "watchTopbar",
  footer: "watchFooter",
  holds: "watchDaemonHolds",
  feed: "watchFeed",
  loginTerminal: "watchLoginTerminal",
} as const satisfies Record<PageWatchKind, keyof AgentReplClient>;

/** A page-streams stand-in that calls each subscription's own rpc directly. */
export function directPageStreams(client: AgentReplClient, page = "test-page"): PageStreams {
  return {
    page,
    watch<K extends PageWatchKind>(
      kind: K,
      request: PageWatchRequest<K>,
      signal: AbortSignal,
    ): AsyncIterable<PageWatchResponse<K>> {
      const method = client[RPC_FOR_KIND[kind]] as (
        req: PageWatchRequest<K>,
        opts: { signal: AbortSignal },
      ) => AsyncIterable<PageWatchResponse<K>>;
      return method.call(client, request, { signal });
    },
  };
}
