/**
 * testAppContext — `createAppContext` for a unit test that scripts ONE rpc.
 *
 * The only difference from production is which page-stream the context gets:
 * this one calls each subscription's own rpc directly instead of multiplexing
 * it onto `WatchPage`. See `direct-page-streams.ts` for why that is the right
 * substitution in a test with no socket, and where the mux is covered instead.
 *
 * A test that wants the REAL page mux calls `createAppContext` itself — the
 * webapp-layer harness does exactly that, against a real daemon.
 */
import type { ServiceImpl } from "@connectrpc/connect";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { createTicker } from "../../src/clock.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { createAppContext, type AppContext, type AppContextInit } from "../../src/rpc/context.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { directPageStreams } from "./direct-page-streams.js";
import { fakePageSeen } from "../page-seen-fake.js";

/**
 * A test that is not about the page being seen leaves `seen` unset and gets a
 * page that is seen (page-seen-fake.ts).
 */
export function testAppContext(
  init: Omit<AppContextInit, "page" | "streams" | "seen"> & Partial<Pick<AppContextInit, "seen">>,
): AppContext {
  return createAppContext({
    seen: fakePageSeen(),
    ...init,
    page: "test-page",
    streams: directPageStreams(init.client),
  });
}

/** What a scripted context may say beyond its rpcs (`scriptedAppContext`). */
export interface ScriptedAppContextOptions {
  /** The workspace the page belongs to. */
  readonly workspace: AppContextInit["workspace"];
  /** The ticker's period in ms; 1000 unless stated. */
  readonly tickMs?: number;
  /** Where the page files its failure cards. */
  readonly failures: FailureSink;
}

/**
 * A context whose daemon answers exactly the rpcs RPCS implements, over an
 * in-memory router: what a unit test that scripts its daemon's answers uses,
 * so every such test builds its client the same way.
 */
export function scriptedAppContext(
  rpcs: Partial<ServiceImpl<typeof AgentRepl>>,
  opts: ScriptedAppContextOptions,
): AppContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, rpcs);
  });
  return testAppContext({
    client: createAgentReplClient(transport),
    workspace: opts.workspace,
    ticker: createTicker(opts.tickMs ?? 1000),
    failures: opts.failures,
    composerEnabled: false,
  });
}
