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
import { createAppContext, type AppContext, type AppContextInit } from "../../src/rpc/context.js";
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
