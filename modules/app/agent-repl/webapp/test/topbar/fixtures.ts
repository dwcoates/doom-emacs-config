// Shared arrangement for the topbar suites: a context whose rpcs a test
// scripts, and a reveal layer over a host with the anchors already present.
import { create } from "@bufbuild/protobuf";
import { createRouterTransport, type ServiceImpl } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { Ticker } from "../../src/clock.js";
import type { LocalFailure } from "../../src/failure/local.js";
import type { ClientFailureArm, FailureSink } from "../../src/failure/sink.js";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import type { TopbarContext } from "../../src/topbar/context.js";
import { mountRevealLayer, type RevealGeometry } from "../../src/topbar/reveal.js";

export const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
export const NOW = 1_700_000_000_000;

/** jsdom reports every rect as zero, so the geometry is supplied outright. */
export const GEOMETRY: RevealGeometry = {
  rectOf: () => ({ left: 0, top: 0, right: 0, bottom: 0, width: 0, height: 0 }),
  viewport: () => ({ width: 1000, height: 800 }),
};

export class RecordingSink implements FailureSink {
  readonly reported: FailureKind[] = [];
  readonly retracted: ClientFailureArm[] = [];
  report(kind: FailureKind): void {
    this.reported.push(kind);
  }
  retract(arm: ClientFailureArm): void {
    this.retracted.push(arm);
  }
}

/** A ticker a test steps by hand. */
export function fakeTicker(): Ticker & { set(nowMs: number): void } {
  let now = NOW;
  const listeners = new Set<(nowMs: number) => void>();
  return {
    now: () => now,
    subscribe(fn) {
      listeners.add(fn);
      return () => listeners.delete(fn);
    },
    set(nowMs: number) {
      now = nowMs;
      for (const fn of [...listeners]) fn(nowMs);
    },
  };
}

/** An AppContext whose service is exactly what IMPL scripts. */
export function appContext(
  impl: Partial<ServiceImpl<typeof AgentRepl>> = {},
  failures: FailureSink = new RecordingSink(),
  ticker: Ticker = fakeTicker(),
): AppContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, impl);
  });
  return testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker,
    failures,
    composerEnabled: false,
  });
}

/** A mounted host, its reveal layer, and the TopbarContext over both. */
export function topbarContext(
  ctx: AppContext = appContext(),
  openLogin: (control: HTMLElement) => void = () => undefined,
  localFailures: () => readonly LocalFailure[] = () => [],
): { host: HTMLElement; tc: TopbarContext } {
  const host = document.createElement("div");
  document.body.replaceChildren(host);
  const reveals = mountRevealLayer(host, GEOMETRY);
  return { host, tc: { ctx, reveals, openLogin, localFailures } };
}

/** The reveal currently drawn, or null. */
export function openPanel(host: HTMLElement): HTMLElement | null {
  return host.querySelector<HTMLElement>("[data-reveal]");
}
