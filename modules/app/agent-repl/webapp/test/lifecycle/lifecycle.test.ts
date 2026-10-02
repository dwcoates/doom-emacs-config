// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  AdoptWebWorkspaceErrorSchema,
  AdoptWebWorkspaceResponseSchema,
  type AdoptWebWorkspaceResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_adopt_web_workspace_pb";
import {
  DaemonShutdownAnnouncedSchema,
  WatchDaemonResponseSchema,
  type DaemonDrainScheduled,
  type DaemonShutdownAnnounced,
  type WatchDaemonResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import {
  DaemonDrainScheduledSchema,
  type NewsDigestStanding,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_daemon_pb";
import { WatchWebWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_web_workspace_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import type { Ticker } from "../../src/clock.js";
import type { ClientFailureArm, FailureSink } from "../../src/failure/sink.js";
import type { NewsDigestHandle } from "../../src/news-digest/news-digest.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "../rpc/app-context.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  AdoptionFailed,
  adoptAtBoot,
  announceShutdown,
  isDeployHandover,
  classifyAdoptionRefusal,
  drainReasonText,
  drawDrainNotice,
  drawMovedNotice,
  drawRestartingNotice,
  mountBanner,
  quietWindowMs,
  shutdownCauseText,
  bindSessionIdentity,
  startLifecycle,
  workspaceMoved,
} from "../../src/lifecycle/lifecycle.js";
import type { ClientLogRecord } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { WebWorkspaceSessionIdentitySchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_web_workspace_pb";
import { ForwardingLogger, bindLogContext, log, resetLoggingForTests, setLogger } from "../../src/log.js";
import { WebappBuildUnknown } from "../../src/webapp-build.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const NOW = 1_700_000_000_000;

/**
 * `startLifecycle` reads THIS page's own build (`readWebappBuild`) before it
 * opens anything, off the document's own built entry `<script>` tag — a tag
 * the real page never runs without, since Vite writes it. Every test in this
 * file gets the same stand-in so `startLifecycle` is called the way it is in
 * production, and each test starts and ends with a clean `<head>`.
 */
beforeEach(() => {
  const entry = document.createElement("script");
  entry.setAttribute("type", "module");
  entry.setAttribute("src", "/assets/index-testbuild.js");
  document.head.append(entry);
});
afterEach(() => {
  document.head.innerHTML = "";
});

/** Records every report/retract/suppress, so a test can assert the arms. */
class RecordingSink implements FailureSink {
  readonly reported: FailureKind[] = [];
  readonly retracted: ClientFailureArm[] = [];
  readonly suppressed: Array<[ClientFailureArm, number]> = [];
  report(kind: FailureKind): void {
    this.reported.push(kind);
  }
  retract(arm: ClientFailureArm): void {
    this.retracted.push(arm);
  }
  suppress(arm: ClientFailureArm, untilMs: number): void {
    this.suppressed.push([arm, untilMs]);
  }
}

/** A ticker whose instant a test moves by hand. */
function fakeTicker(): Ticker & { set(nowMs: number): void } {
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

const reason = (kind: "deploy" | "maintenance" | "operator", note = "the operator asked") => {
  if (kind === "deploy") return { kind: { case: "deploy" as const, value: {} } } as never;
  if (kind === "maintenance") return { kind: { case: "maintenance" as const, value: {} } } as never;
  return { kind: { case: "operator" as const, value: { note } } } as never;
};

const drain = (atMs: number, reasonInit: unknown): DaemonDrainScheduled =>
  create(DaemonDrainScheduledSchema, {
    atMs: BigInt(atMs),
    reason: reasonInit as never,
  });

const announced = (init: {
  address?: string;
  cause: unknown;
  outageMs: number;
  mintedAtMs: number;
}): DaemonShutdownAnnounced =>
  create(DaemonShutdownAnnouncedSchema, {
    address: init.address,
    cause: init.cause as never,
    expectedOutageMs: BigInt(init.outageMs),
    mintedAtMs: BigInt(init.mintedAtMs),
  });

// ---------------------------------------------------------------------------

describe("drainReasonText", () => {
  const table: ReadonlyArray<readonly [string, unknown, string]> = [
    ["deploy", { kind: { case: "deploy", value: {} } }, "deploy"],
    ["maintenance", { kind: { case: "maintenance", value: {} } }, "maintenance"],
    ["operator", { kind: { case: "operator", value: { note: "swapping disks" } } }, "swapping disks"],
  ];
  for (const [name, init, expected] of table) {
    it(`words the ${name} arm`, () => {
      expect(drainReasonText(init as never, "DrainReason")).toBe(expected);
    });
  }

  it("refuses a reason that names no arm", () => {
    expect(() => drainReasonText({ kind: {} } as never, "DrainReason")).toThrow(MalformedView);
  });
});

describe("shutdownCauseText", () => {
  it("words the rollout arm", () => {
    expect(shutdownCauseText({ kind: { case: "selfMergeRollout", value: {} } } as never)).toBe(
      "rollout",
    );
  });

  it("words a scheduled drain as its own reason", () => {
    expect(
      shutdownCauseText({
        kind: {
          case: "scheduledDrain",
          value: { reason: { kind: { case: "deploy", value: {} } } },
        },
      } as never),
    ).toBe("deploy");
  });

  it("words an immediate shutdown as its own reason", () => {
    expect(
      shutdownCauseText({
        kind: {
          case: "immediate",
          value: { reason: { kind: { case: "operator", value: { note: "now" } } } },
        },
      } as never),
    ).toBe("now");
  });

  it("refuses a cause that names no arm", () => {
    expect(() => shutdownCauseText({ kind: {} } as never)).toThrow(MalformedView);
  });
});

describe("quietWindowMs", () => {
  it("is the whole outage for a receiver that read it as it was minted", () => {
    const a = announced({
      cause: { kind: { case: "selfMergeRollout", value: {} } },
      outageMs: 8000,
      mintedAtMs: NOW,
    });
    expect(quietWindowMs(a, NOW)).toBe(8000);
  });

  it("shortens for a late receiver rather than restarting the window", () => {
    const a = announced({
      cause: { kind: { case: "selfMergeRollout", value: {} } },
      outageMs: 8000,
      mintedAtMs: NOW,
    });
    expect(quietWindowMs(a, NOW + 3000)).toBe(5000);
  });

  it("clamps a window that has already elapsed to zero", () => {
    const a = announced({
      cause: { kind: { case: "selfMergeRollout", value: {} } },
      outageMs: 8000,
      mintedAtMs: NOW,
    });
    expect(quietWindowMs(a, NOW + 20_000)).toBe(0);
  });
});

describe("drawMovedNotice", () => {
  it("names the successor's address, the one fact the reader may need", () => {
    expect(drawMovedNotice("127.0.0.1:8123").textContent).toBe(
      "workspace moved to 127.0.0.1:8123",
    );
  });

  it("carries the address as a hook", () => {
    expect(drawMovedNotice("127.0.0.1:8123").getAttribute("data-moved")).toBe("127.0.0.1:8123");
  });

  it("wears the standing-restart marker", () => {
    expect(drawMovedNotice("a:1").hasAttribute("data-restarting")).toBe(true);
  });
});

describe("drawDrainNotice", () => {
  it("counts down to the drain instant", () => {
    const { element, tick } = drawDrainNotice(drain(NOW + 252_000, reason("deploy")));
    tick(NOW);
    expect(element.textContent).toBe("daemon restart scheduled · deploy · in 4m 12s");
  });

  it("reads 'any moment now' once the instant has passed", () => {
    const { element, tick } = drawDrainNotice(drain(NOW, reason("deploy")));
    tick(NOW + 1000);
    expect(element.textContent).toBe("daemon restart scheduled · deploy · any moment now");
  });

  it("draws the reason the push carried", () => {
    const { element, tick } = drawDrainNotice(drain(NOW + 1000, reason("maintenance")));
    tick(NOW);
    expect(element.textContent).toContain("maintenance");
  });

  it("carries the reason's own arm beside the words it composed", () => {
    const { element } = drawDrainNotice(drain(NOW + 1000, reason("maintenance")));
    expect(element.querySelector("[data-arm]")?.getAttribute("data-arm")).toBe("maintenance");
  });

  it("carries the operator arm even though its words are the note", () => {
    const { element } = drawDrainNotice(drain(NOW + 1000, reason("operator")));
    expect(element.querySelector("[data-arm]")?.getAttribute("data-arm")).toBe("operator");
  });
});

describe("drawRestartingNotice", () => {
  it("counts down to the end of the announced outage", () => {
    const { element, tick } = drawRestartingNotice(
      announced({
        cause: { kind: { case: "selfMergeRollout", value: {} } },
        outageMs: 8000,
        mintedAtMs: NOW,
      }),
      NOW + 8000,
    );
    tick(NOW);
    expect(element.textContent).toBe("daemon restarting · rollout · expected back in 8s");
  });

  it("reads 'any moment now' past the window rather than counting up", () => {
    const { element, tick } = drawRestartingNotice(
      announced({
        cause: { kind: { case: "selfMergeRollout", value: {} } },
        outageMs: 8000,
        mintedAtMs: NOW,
      }),
      NOW + 8000,
    );
    tick(NOW + 9000);
    expect(element.textContent).toBe("daemon restarting · rollout · any moment now");
  });

  it("carries the cause arm as a hook", () => {
    const { element } = drawRestartingNotice(
      announced({
        cause: { kind: { case: "immediate", value: { reason: reason("deploy") } } },
        outageMs: 1,
        mintedAtMs: NOW,
      }),
      NOW,
    );
    expect(element.getAttribute("data-shutdown-cause")).toBe("immediate");
  });

  it("refuses an announcement carrying no cause", () => {
    expect(() =>
      drawRestartingNotice(
        create(DaemonShutdownAnnouncedSchema, { expectedOutageMs: 1n, mintedAtMs: 1n }),
        NOW,
      ),
    ).toThrow(MalformedView);
  });
});

// ---------------------------------------------------------------------------

function bannerContext(ticker: Ticker, failures: FailureSink = new RecordingSink()): AppContext {
  return testAppContext({
    client: createAgentReplClient(createRouterTransport(({ service }) => service(AgentRepl, {}))),
    workspace: WORKSPACE,
    ticker,
    failures,
    composerEnabled: false,
  });
}

describe("mountBanner", () => {
  let host: HTMLElement;
  beforeEach(() => {
    host = document.createElement("div");
  });

  it("ships empty, so a healthy page costs nothing", () => {
    mountBanner(host, bannerContext(fakeTicker()));
    expect(host.children.length).toBe(0);
  });

  it("draws the drain notice on a schedule", () => {
    const banner = mountBanner(host, bannerContext(fakeTicker()));
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    expect(host.textContent).toContain("daemon restart scheduled");
  });

  it("ticks the drain countdown from the shared ticker", () => {
    const ticker = fakeTicker();
    const banner = mountBanner(host, bannerContext(ticker));
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    ticker.set(NOW + 30_000);
    expect(host.textContent).toContain("in 30s");
  });

  it("takes the banner down when the schedule is cancelled", () => {
    const banner = mountBanner(host, bannerContext(fakeTicker()));
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    banner.clearDrain();
    expect(host.children.length).toBe(0);
  });

  it("lets an announced restart win over a schedule that has not fired", () => {
    const banner = mountBanner(host, bannerContext(fakeTicker()));
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    banner.showRestarting(
      announced({
        cause: { kind: { case: "selfMergeRollout", value: {} } },
        outageMs: 8000,
        mintedAtMs: NOW,
      }),
      NOW + 8000,
    );
    expect(host.textContent).toContain("daemon restarting");
  });

  it("falls back to the standing schedule when the restart notice clears", () => {
    const banner = mountBanner(host, bannerContext(fakeTicker()));
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    banner.showRestarting(
      announced({
        cause: { kind: { case: "selfMergeRollout", value: {} } },
        outageMs: 8000,
        mintedAtMs: NOW,
      }),
      NOW + 8000,
    );
    banner.clearRestarting();
    expect(host.textContent).toContain("daemon restart scheduled");
  });

  it("lets the moved notice win over everything, since nothing follows it", () => {
    const banner = mountBanner(host, bannerContext(fakeTicker()));
    banner.showMoved("127.0.0.1:9");
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    expect(host.textContent).toBe("workspace moved to 127.0.0.1:9");
  });

  it("unsubscribes the previous notice's clock on dispose", () => {
    const ticker = fakeTicker();
    const banner = mountBanner(host, bannerContext(ticker));
    banner.showDrain(drain(NOW + 60_000, reason("deploy")));
    banner.dispose();
    ticker.set(NOW + 30_000);
    expect(host.children.length).toBe(0);
  });
});

// ---------------------------------------------------------------------------

/** A daemon whose two lifecycle streams yield what the test scripts. */
function lifecycleClient(script: {
  web?: () => AsyncIterable<WatchWebWorkspaceResponseInit>;
  daemon?: () => AsyncIterable<WatchDaemonResponse>;
  adopt?: () => AdoptWebWorkspaceResponse;
}) {
  const state = {
    adoptCalls: 0,
    webRequests: [] as Array<{ webappBuild: string }>,
    daemonRequests: [] as Array<{ client: string | undefined }>,
  };
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      watchWebWorkspace: async function* (request) {
        state.webRequests.push({ webappBuild: request.webappBuild });
        for await (const push of script.web?.() ?? []) {
          yield create(WatchWebWorkspaceResponseSchema, push as never);
        }
        await new Promise<never>(() => undefined);
      },
      watchDaemon: async function* (request) {
        state.daemonRequests.push({ client: request.client.case });
        for await (const push of script.daemon?.() ?? []) yield push;
        await new Promise<never>(() => undefined);
      },
      adoptWebWorkspace: () => {
        state.adoptCalls += 1;
        return (
          script.adopt?.() ??
          create(AdoptWebWorkspaceResponseSchema, { result: { case: "success", value: {} } })
        );
      },
    });
  });
  return { client: createAgentReplClient(transport), state };
}

type WatchWebWorkspaceResponseInit =
  | { push: { case: "transferred"; value: { address: string } } }
  | {
      push: {
        case: "sessionIdentity";
        value: { agentReplSessionId: string; claudeSessionId: string };
      };
    };

function lifecycleContext(
  client: ReturnType<typeof lifecycleClient>["client"],
  failures: FailureSink,
  ticker: Ticker,
): AppContext {
  return testAppContext({ client, workspace: WORKSPACE, ticker, failures, composerEnabled: false });
}

/** A news digest overlay that draws nothing: these tests are not about it. */
const NO_DIGEST: Pick<NewsDigestHandle, "apply"> = { apply: () => undefined };

/** Let the transport's zero-delay frames land without moving the clock. */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

describe("startLifecycle: the handover", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  const transferred = async function* (): AsyncIterable<WatchWebWorkspaceResponseInit> {
    yield { push: { case: "transferred", value: { address: "127.0.0.1:8123" } } };
  };

  it("draws the moved notice naming the successor", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({ web: transferred });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    // ASSERT (before dispose: disposing takes the banner down by design)
    expect(host.textContent).toBe("workspace moved to 127.0.0.1:8123");
    handle.dispose();
  });

  it("carries this page's own webapp build on the WatchWebWorkspace request", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client, state } = lifecycleClient({ web: transferred });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT: read off the entry tag `beforeEach` above wrote into the document.
    expect(state.webRequests).toEqual([{ webappBuild: "testbuild" }]);
  });

  it("quiesces the page, so nothing more is sent on this client", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({ web: transferred });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT
    expect(ctx.isQuiesced()).toBe(true);
  });

  it("does not dial the successor, which is a different origin", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({ web: transferred });
    const first = client;
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT: the client the page holds is untouched — no transport was built.
    expect(ctx.client).toBe(first);
  });

  it("files no unreachable card for the streams the handover stopped", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const sink = new RecordingSink();
    const { client } = lifecycleClient({ web: transferred });
    const ctx = lifecycleContext(client, sink, fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    await vi.advanceTimersByTimeAsync(10_000);
    handle.dispose();
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).not.toContain("daemonUnreachable");
  });

  it("throws rather than opening WatchWebWorkspace when the page has no built entry tag", async () => {
    // ARRANGE: remove the entry tag `beforeEach` above wrote.
    document.head.innerHTML = "";
    const host = document.createElement("div");
    const { client, state } = lifecycleClient({ web: transferred });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT + ASSERT
    expect(() => startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST })).toThrow(WebappBuildUnknown);
    await settle();
    expect(state.webRequests).toEqual([]);
  });

  it("refuses a web-link push naming no arm", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const sink = new RecordingSink();
    const { client } = lifecycleClient({
      web: async function* () {
        yield {} as WatchWebWorkspaceResponseInit;
      },
    });
    const ctx = lifecycleContext(client, sink, fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT: the core files the unreadable frame rather than tearing down.
    expect(sink.reported.map((k) => k.kind.case)).toContain("frameUndecodable");
  });
});

describe("startLifecycle: the daemon stream", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("draws the standing drain banner", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: { case: "drainScheduled", value: drain(NOW + 60_000, reason("deploy")) },
        });
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    // ASSERT (before dispose: disposing takes the banner down by design)
    expect(host.textContent).toContain("daemon restart scheduled · deploy");
    handle.dispose();
  });

  it("hands the news digest standing to the overlay", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const applied: NewsDigestStanding[] = [];
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: { case: "newsDigest", value: { standing: { case: "none", value: {} } } },
        });
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, {
      drainBannerHost: host,
      newsDigest: { apply: (standing) => applied.push(standing) },
    });
    await settle();
    handle.dispose();
    // ASSERT
    expect(applied.map((standing) => standing.standing.case)).toEqual(["none"]);
  });

  it("names the webview client on the WatchDaemon request", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client, state } = lifecycleClient({});
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT: the daemon refuses a WatchDaemon naming no client (REQUIRED oneof).
    expect(state.daemonRequests).toEqual([{ client: "webview" }]);
  });

  it("suppresses the unreachable card for exactly the announced window", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const sink = new RecordingSink();
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: {
            case: "shutdownAnnounced",
            value: announced({
              cause: { kind: { case: "selfMergeRollout", value: {} } },
              outageMs: 8000,
              mintedAtMs: NOW,
            }),
          },
        });
      },
    });
    const ctx = lifecycleContext(client, sink, fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT
    expect(sink.suppressed).toEqual([["daemonUnreachable", NOW + 8000]]);
  });

  it("takes the restarting notice down when a stream that dropped reads again", async () => {
    // ARRANGE: the daemon answering is what ends an outage, not the countdown,
    // and a bounce takes every stream down together — so the FIRST stream to
    // read again is the daemon being back.
    const host = document.createElement("div");
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: {
            case: "shutdownAnnounced",
            value: announced({
              cause: { kind: { case: "selfMergeRollout", value: {} } },
              outageMs: 8000,
              mintedAtMs: NOW,
            }),
          },
        });
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    // ACT
    ctx.noteLinkRestored();
    // ASSERT
    expect(host.children.length).toBe(0);
    handle.dispose();
  });

  it("keeps the restarting notice while the announcing daemon's streams still push", async () => {
    // ARRANGE: the outgoing daemon goes on pushing on its standing streams
    // until it exits; a frame on a link that never dropped is not a new daemon.
    const host = document.createElement("div");
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: {
            case: "shutdownAnnounced",
            value: announced({
              cause: { kind: { case: "selfMergeRollout", value: {} } },
              outageMs: 8000,
              mintedAtMs: NOW,
            }),
          },
        });
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    // ACT
    ctx.notePush();
    // ASSERT
    expect(host.querySelector("[data-shutdown-cause]")).not.toBeNull();
    handle.dispose();
  });

  it("stops listening for frames once disposed", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({});
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ACT / ASSERT: the banner is gone with the mount, and a late frame or a
    // late reconnection reaches nothing that would draw into a host this page
    // no longer owns.
    expect(() => ctx.notePush()).not.toThrow();
    expect(() => ctx.noteLinkRestored()).not.toThrow();
  });

  it("draws the restarting notice from the announcement", async () => {
    // ARRANGE: a plain bounce (no successor address). A deploy's handover
    // draws no banner since 2026-09-27: its progress is the footer's.
    const host = document.createElement("div");
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: {
            case: "shutdownAnnounced",
            value: announced({
              cause: { kind: { case: "selfMergeRollout", value: {} } },
              outageMs: 8000,
              mintedAtMs: NOW,
            }),
          },
        });
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    // ASSERT (before dispose: disposing takes the banner down by design)
    expect(host.textContent).toContain("daemon restarting · rollout");
    handle.dispose();
  });

  it("takes the drain banner down on a cancellation", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {
          push: { case: "drainScheduled", value: drain(NOW + 60_000, reason("deploy")) },
        });
        yield create(WatchDaemonResponseSchema, {
          push: { case: "drainCancelled", value: {} },
        });
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT
    expect(host.children.length).toBe(0);
  });

  it("refuses a daemon push naming no arm", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const sink = new RecordingSink();
    const { client } = lifecycleClient({
      daemon: async function* () {
        yield create(WatchDaemonResponseSchema, {});
      },
    });
    const ctx = lifecycleContext(client, sink, fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    handle.dispose();
    // ASSERT
    expect(sink.reported.map((k) => k.kind.case)).toContain("frameUndecodable");
  });
});

describe("workspaceMoved", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("raises the same notice a transferred push does, for a refusal arm", async () => {
    // ARRANGE
    const host = document.createElement("div");
    const { client } = lifecycleClient({});
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    // ACT
    const handled = workspaceMoved("127.0.0.1:7");
    // ASSERT (before dispose: disposing takes the banner down by design)
    expect([handled, host.textContent]).toEqual([true, "workspace moved to 127.0.0.1:7"]);
    handle.dispose();
  });

  it("answers false when no lifecycle is mounted, rather than pretending it drew", () => {
    expect(workspaceMoved("127.0.0.1:7")).toBe(false);
  });
});

// ---------------------------------------------------------------------------

describe("classifyAdoptionRefusal", () => {
  const table: ReadonlyArray<readonly [string, unknown, string]> = [
    ["noTransferAnnounced", { case: "noTransferAnnounced", value: {} }, "no-transfer"],
    ["notYetAdopted", { case: "notYetAdopted", value: {} }, "retry"],
    ["unknownWorkspace", { case: "unknownWorkspace", value: {} }, "terminal"],
    [
      "workspaceRefMismatch",
      { case: "workspaceRefMismatch", value: { registryDir: "/x" } },
      "terminal",
    ],
    ["transferringAway", { case: "transferringAway", value: { address: "a:1" } }, "terminal"],
    ["participantNotExpected", { case: "participantNotExpected", value: {} }, "terminal"],
  ];
  for (const [name, cause, kind] of table) {
    it(`treats ${name} as ${kind}`, () => {
      expect(classifyAdoptionRefusal({ cause } as never).kind).toBe(kind);
    });
  }

  it("carries the registry's dir on a mismatch, so the reader can reconcile", () => {
    const outcome = classifyAdoptionRefusal({
      cause: { case: "workspaceRefMismatch", value: { registryDir: "/elsewhere" } },
    } as never);
    expect(outcome.kind === "terminal" && outcome.detail).toContain("/elsewhere");
  });

  it("refuses an error naming no cause", () => {
    expect(() =>
      classifyAdoptionRefusal(create(AdoptWebWorkspaceErrorSchema, {})),
    ).toThrow(MalformedView);
  });
});

describe("adoptAtBoot", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  const refusal = (cause: unknown): AdoptWebWorkspaceResponse =>
    create(AdoptWebWorkspaceResponseSchema, {
      result: { case: "error", value: { cause: cause as never } },
    });

  it("resolves adopted on success", async () => {
    const { client } = lifecycleClient({});
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    await expect(adoptAtBoot(ctx)).resolves.toBe("adopted");
  });

  it("resolves no-transfer on the ordinary non-handover boot", async () => {
    const { client } = lifecycleClient({
      adopt: () => refusal({ case: "noTransferAnnounced", value: {} }),
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    await expect(adoptAtBoot(ctx)).resolves.toBe("no-transfer");
  });

  it("calls the verb exactly once when it is answered at once", async () => {
    const { client, state } = lifecycleClient({});
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    await adoptAtBoot(ctx);
    expect(state.adoptCalls).toBe(1);
  });

  it("retries not_yet_adopted with backoff until the rendezvous completes", async () => {
    // ARRANGE
    let calls = 0;
    const { client } = lifecycleClient({
      adopt: () => {
        calls += 1;
        return calls < 3
          ? refusal({ case: "notYetAdopted", value: {} })
          : create(AdoptWebWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    const ticker = fakeTicker();
    const ctx = lifecycleContext(client, new RecordingSink(), ticker);
    // ACT
    const promise = adoptAtBoot(ctx, { initialMs: 250, maxMs: 5000, budgetMs: 60_000 });
    await vi.advanceTimersByTimeAsync(250);
    await vi.advanceTimersByTimeAsync(500);
    await vi.advanceTimersByTimeAsync(0);
    // ASSERT
    await expect(promise).resolves.toBe("adopted");
    expect(calls).toBe(3);
  });

  it("gives up once the budget is spent, so a boot cannot hang forever", async () => {
    // ARRANGE
    const { client } = lifecycleClient({
      adopt: () => refusal({ case: "notYetAdopted", value: {} }),
    });
    const ticker = fakeTicker();
    const ctx = lifecycleContext(client, new RecordingSink(), ticker);
    // ACT
    const promise = adoptAtBoot(ctx, { initialMs: 250, maxMs: 250, budgetMs: 500 });
    const settled = promise.catch((err: unknown) => err);
    for (let i = 0; i < 10; i += 1) {
      ticker.set(ticker.now() + 250);
      await vi.advanceTimersByTimeAsync(250);
    }
    // ASSERT
    await expect(settled).resolves.toBeInstanceOf(AdoptionFailed);
  });

  const terminal: ReadonlyArray<readonly [string, unknown]> = [
    ["unknownWorkspace", { case: "unknownWorkspace", value: {} }],
    ["workspaceRefMismatch", { case: "workspaceRefMismatch", value: { registryDir: "/x" } }],
    ["transferringAway", { case: "transferringAway", value: { address: "a:1" } }],
    ["participantNotExpected", { case: "participantNotExpected", value: {} }],
  ];
  for (const [name, cause] of terminal) {
    it(`fails the boot on ${name}, naming the arm`, async () => {
      const { client } = lifecycleClient({ adopt: () => refusal(cause) });
      const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
      const err = await adoptAtBoot(ctx).catch((e: unknown) => e);
      expect((err as AdoptionFailed).arm).toBe(name);
    });
  }

  it("reports the refusal through the failure sink exactly once", async () => {
    const sink = new RecordingSink();
    const { client } = lifecycleClient({
      adopt: () => refusal({ case: "unknownWorkspace", value: {} }),
    });
    const ctx = lifecycleContext(client, sink, fakeTicker());
    await adoptAtBoot(ctx).catch(() => undefined);
    expect(sink.reported.map((k) => k.kind.case)).toEqual(["controlPlaneFailed"]);
  });

  it("fails the boot on a transport failure rather than waiting on nothing", async () => {
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        adoptWebWorkspace: () => {
          throw new ConnectError("no route", Code.Unavailable);
        },
      });
    });
    const ctx = testAppContext({
      client: createAgentReplClient(transport),
      workspace: WORKSPACE,
      ticker: fakeTicker(),
      failures: new RecordingSink(),
      composerEnabled: false,
    });
    const err = await adoptAtBoot(ctx).catch((e: unknown) => e);
    expect((err as AdoptionFailed).arm).toBe("transport");
  });

  it("refuses a response whose result oneof is unset", async () => {
    const { client } = lifecycleClient({
      adopt: () => create(AdoptWebWorkspaceResponseSchema, {}),
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    await expect(adoptAtBoot(ctx)).rejects.toThrow(MalformedView);
  });
});

// ---------------------------------------------------------------------------
// THE ARMS A NEWER DAEMON COULD SET. The generated oneof cannot carry an arm
// this build has no descriptor for, so these reach the words directly with the
// shape a future schema would produce.
// ---------------------------------------------------------------------------

describe("the unknown arm", () => {
  it("refuses a shutdown cause this build cannot word", () => {
    // ARRANGE / ACT / ASSERT
    expect(() =>
      shutdownCauseText({ kind: { case: "quantumRollout", value: {} } } as never),
    ).toThrow(/arm 'quantumRollout' is not one this build can draw/);
  });

  it("refuses a drain reason this build cannot word", () => {
    expect(() =>
      drainReasonText({ kind: { case: "diskSwap", value: {} } } as never, "DrainReason"),
    ).toThrow(/arm 'diskSwap' is not one this build can draw/);
  });

  it("refuses an adoption refusal cause this build cannot classify", () => {
    expect(() =>
      classifyAdoptionRefusal({ cause: { case: "quarantined", value: {} } } as never),
    ).toThrow(/arm 'quarantined' is not one this build can draw/);
  });
});

// ---------------------------------------------------------------------------

describe("announceShutdown", () => {
  /** A sink that cannot mute a window, which the announcement must survive. */
  const sinkWithoutSuppress = (): FailureSink & { reported: FailureKind[] } => {
    const reported: FailureKind[] = [];
    return { reported, report: (kind) => reported.push(kind), retract: () => undefined };
  };

  it("still draws the restarting notice when the sink cannot mute the outage", () => {
    // ARRANGE
    const host = document.createElement("div");
    const ticker = fakeTicker();
    const ctx = bannerContext(ticker, sinkWithoutSuppress());
    const banner = mountBanner(host, ctx);
    // ACT
    announceShutdown(
      ctx,
      banner,
      announced({
        cause: { kind: { case: "selfMergeRollout", value: {} } },
        outageMs: 8000,
        mintedAtMs: NOW,
      }),
    );
    // ASSERT: the mute is a nicety; the notice is the contract.
    expect(host.textContent).toBe("daemon restarting · rollout · expected back in 8s");
    banner.dispose();
  });

  it("refuses an announcement carrying no cause rather than drawing a bare notice", () => {
    // ARRANGE
    const host = document.createElement("div");
    const ctx = bannerContext(fakeTicker(), new RecordingSink());
    const banner = mountBanner(host, ctx);
    const causeless = create(DaemonShutdownAnnouncedSchema, {
      expectedOutageMs: 8000n,
      mintedAtMs: BigInt(NOW),
    });
    // ACT / ASSERT
    expect(() => announceShutdown(ctx, banner, causeless)).toThrow(MalformedView);
    banner.dispose();
  });

  it("mutes the window before it tries to draw, so a causeless one still mutes", () => {
    // ARRANGE
    const host = document.createElement("div");
    const sink = new RecordingSink();
    const ctx = bannerContext(fakeTicker(), sink);
    const banner = mountBanner(host, ctx);
    const causeless = create(DaemonShutdownAnnouncedSchema, {
      expectedOutageMs: 8000n,
      mintedAtMs: BigInt(NOW),
    });
    // ACT
    expect(() => announceShutdown(ctx, banner, causeless)).toThrow(MalformedView);
    // ASSERT
    expect(sink.suppressed).toEqual([["daemonUnreachable", NOW + 8000]]);
    banner.dispose();
  });
});

describe("announceShutdown: a deploy's handover is the footer's to show", () => {
  const deployHandover = () =>
    announced({
      address: "127.0.0.1:9",
      cause: { kind: { case: "selfMergeRollout", value: {} } },
      outageMs: 8000,
      mintedAtMs: NOW,
    });

  it("draws no banner for a deploy's handover", () => {
    // ARRANGE
    const host = document.createElement("div");
    const ctx = bannerContext(fakeTicker(), new RecordingSink());
    const banner = mountBanner(host, ctx);
    // ACT
    announceShutdown(ctx, banner, deployHandover());
    // ASSERT
    expect(host.children.length).toBe(0);
    banner.dispose();
  });

  it("still mutes the expected unreachable failure for a deploy's handover", () => {
    // ARRANGE
    const host = document.createElement("div");
    const sink = new RecordingSink();
    const ctx = bannerContext(fakeTicker(), sink);
    const banner = mountBanner(host, ctx);
    // ACT
    announceShutdown(ctx, banner, deployHandover());
    // ASSERT
    expect(sink.suppressed).toEqual([["daemonUnreachable", NOW + 8000]]);
    banner.dispose();
  });

  it("still draws the banner for an unplanned restart", () => {
    // ARRANGE
    const host = document.createElement("div");
    const ctx = bannerContext(fakeTicker(), new RecordingSink());
    const banner = mountBanner(host, ctx);
    // ACT
    announceShutdown(
      ctx,
      banner,
      announced({
        cause: { kind: { case: "immediate", value: { reason: reason("maintenance") } } },
        outageMs: 8000,
        mintedAtMs: NOW,
      }),
    );
    // ASSERT
    expect(host.querySelector("[data-shutdown-cause='immediate']")).not.toBeNull();
    banner.dispose();
  });

  it("still draws the banner for a rollout with no successor address", () => {
    // ARRANGE: the layout restart's plain bounce.
    const host = document.createElement("div");
    const ctx = bannerContext(fakeTicker(), new RecordingSink());
    const banner = mountBanner(host, ctx);
    // ACT
    announceShutdown(
      ctx,
      banner,
      announced({ cause: { kind: { case: "selfMergeRollout", value: {} } }, outageMs: 8000, mintedAtMs: NOW }),
    );
    // ASSERT
    expect(host.querySelector("[data-shutdown-cause='selfMergeRollout']")).not.toBeNull();
    banner.dispose();
  });
});

describe("isDeployHandover", () => {
  it.each([
    ["the rollout with a successor", "selfMergeRollout", "127.0.0.1:9", true],
    ["the rollout with no successor", "selfMergeRollout", undefined, false],
  ])("answers %s", (_name, cause, address, want) => {
    expect(
      isDeployHandover(
        announced({ address, cause: { kind: { case: cause, value: {} } }, outageMs: 1, mintedAtMs: NOW }),
      ),
    ).toBe(want);
  });

  it("answers an immediate shutdown with an address as no deploy handover", () => {
    expect(
      isDeployHandover(
        announced({
          address: "127.0.0.1:9",
          cause: { kind: { case: "immediate", value: { reason: reason("deploy") } } },
          outageMs: 1,
          mintedAtMs: NOW,
        }),
      ),
    ).toBe(false);
  });
});

describe("mountBanner: cancelling what never stood", () => {
  it("leaves the terminal notice's own element in place when no drain is standing", () => {
    // ARRANGE
    const host = document.createElement("div");
    const banner = mountBanner(host, bannerContext(fakeTicker()));
    banner.showMoved("127.0.0.1:9");
    const drawn = host.firstElementChild;
    // ACT: no schedule was ever shown, so this must not redraw the host.
    banner.clearDrain();
    // ASSERT: a redraw would have replaced the element with an equal-looking one.
    expect(host.firstElementChild).toBe(drawn);
    banner.dispose();
  });
});

describe("startLifecycle: the daemon answering again", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("takes the restarting notice down when the bounced daemon stream reopens", async () => {
    // ARRANGE: run 1 announces the outage and then ends, which is the bounce;
    // run 2 is the successor answering, and that is what ends the outage.
    const host = document.createElement("div");
    let runs = 0;
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        watchWebWorkspace: async function* () {
          await new Promise<never>(() => undefined);
        },
        watchDaemon: async function* () {
          runs += 1;
          if (runs === 1) {
            yield create(WatchDaemonResponseSchema, {
              push: {
                case: "shutdownAnnounced",
                value: announced({
                  cause: { kind: { case: "selfMergeRollout", value: {} } },
                  outageMs: 8000,
                  mintedAtMs: NOW,
                }),
              },
            });
            return;
          }
          yield create(WatchDaemonResponseSchema, { push: { case: "drainCancelled", value: {} } });
          await new Promise<never>(() => undefined);
        },
      });
    });
    const ctx = lifecycleContext(
      createAgentReplClient(transport),
      new RecordingSink(),
      fakeTicker(),
    );
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    expect(host.textContent).toContain("daemon restarting");
    // ACT: wait out the reopen backoff.
    await vi.advanceTimersByTimeAsync(300);
    await settle();
    // ASSERT
    expect(host.children.length).toBe(0);
    handle.dispose();
  });
});

// ---------------------------------------------------------------------------
// THE PAGE'S LOG IDENTITY (landing 15). A browser has no durable sink, so the
// daemon writes this page's records — and a record that names no session can be
// joined to a workspace and no further.
// ---------------------------------------------------------------------------

interface LogHarness {
  logger: ForwardingLogger;
  sent: ClientLogRecord[];
}

/** A logger whose forwarded records the test reads, replacing setup.ts's. */
function captureLogger(): LogHarness {
  const sent: ClientLogRecord[] = [];
  const logger = new ForwardingLogger(
    async (record) => {
      sent.push(record);
      return "accepted";
    },
    () => undefined,
  );
  resetLoggingForTests();
  setLogger(logger);
  bindLogContext({ connection_id: "test-connection" });
  return { logger, sent };
}

/** Release the throttle's window and let the sink's promises settle. */
async function forwarded(h: LogHarness): Promise<ClientLogRecord[]> {
  h.logger.flush();
  await Promise.resolve();
  return h.sent;
}

/**
 * The one forwarded record for OPERATION. Absence throws rather than reading
 * as an absent field: "the record never went" and "the record went without the
 * identity" are different facts and only one of them is under test.
 */
function forwardedRecord(records: ClientLogRecord[], operation: string): ClientLogRecord {
  const found = records.find((r) => r.operation === operation);
  if (found === undefined) throw new Error(`no forwarded record for ${operation}`);
  return found;
}

function identityPush(agentReplSessionId: string, claudeSessionId = "") {
  return create(WebWorkspaceSessionIdentitySchema, { agentReplSessionId, claudeSessionId });
}

describe("bindSessionIdentity", () => {
  it("records the identity lifecycle edge at the default info threshold", async () => {
    // ARRANGE
    const h = captureLogger();
    // ACT
    bindSessionIdentity(identityPush("sess-1"));
    // ASSERT
    const record = forwardedRecord(await forwarded(h), "lifecycle.session_identity");
    expect(record.level.case).toBe("info");
  });

  it("stamps the session on a record logged after the frame", async () => {
    // ARRANGE
    const h = captureLogger();
    // ACT
    bindSessionIdentity(identityPush("sess-1"));
    log.info("after", { operation: "test.after" });
    // ASSERT
    const records = await forwarded(h);
    const after = forwardedRecord(records, "test.after");
    expect(after.context).toMatchObject({ agent_repl_session_id: "sess-1" });
  });

  it("stamps the vendor conversation the same frame named", async () => {
    // ARRANGE
    const h = captureLogger();
    // ACT
    bindSessionIdentity(identityPush("sess-1", "claude-1"));
    log.info("after", { operation: "test.after" });
    // ASSERT
    const records = await forwarded(h);
    const after = forwardedRecord(records, "test.after");
    expect(after.context).toMatchObject({ claude_session_id: "claude-1" });
  });

  it("leaves a record forwarded before the frame unattributed", async () => {
    // ARRANGE
    const h = captureLogger();
    // ACT: the record is forwarded while nothing is bound.
    log.info("before", { operation: "test.before" });
    const records = await forwarded(h);
    // ASSERT
    const before = forwardedRecord(records, "test.before");
    expect(before.context).not.toHaveProperty("agent_repl_session_id");
  });

  it("rebinds when a rotation mints a new session", async () => {
    // ARRANGE
    const h = captureLogger();
    bindSessionIdentity(identityPush("sess-1"));
    // ACT
    bindSessionIdentity(identityPush("sess-2"));
    log.info("after the rotation", { operation: "test.rotated" });
    // ASSERT
    const records = await forwarded(h);
    const rotated = forwardedRecord(records, "test.rotated");
    expect(rotated.context).toMatchObject({ agent_repl_session_id: "sess-2" });
  });

  it("drops the identity when the frame names none, rather than keeping the retired one", async () => {
    // ARRANGE
    const h = captureLogger();
    bindSessionIdentity(identityPush("sess-1"));
    // ACT: the workspace's session went away.
    bindSessionIdentity(identityPush(""));
    log.info("after the session went away", { operation: "test.sessionless" });
    // ASSERT
    const records = await forwarded(h);
    const sessionless = forwardedRecord(records, "test.sessionless");
    expect(sessionless.context).not.toHaveProperty("agent_repl_session_id");
  });
});

describe("startLifecycle: the session identity", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("binds the identity the link stream's opening push named", async () => {
    // ARRANGE
    const h = captureLogger();
    const host = document.createElement("div");
    const { client } = lifecycleClient({
      web: async function* () {
        yield {
          push: {
            case: "sessionIdentity",
            value: { agentReplSessionId: "sess-1", claudeSessionId: "claude-1" },
          },
        };
      },
    });
    const ctx = lifecycleContext(client, new RecordingSink(), fakeTicker());
    // ACT
    const handle = startLifecycle(ctx, { drainBannerHost: host, newsDigest: NO_DIGEST });
    await settle();
    log.info("after the push", { operation: "test.after-push" });
    // ASSERT
    h.logger.flush();
    await settle();
    const after = forwardedRecord(h.sent, "test.after-push");
    expect(after.context).toMatchObject({ agent_repl_session_id: "sess-1" });
    handle.dispose();
  });
});
