/**
 * The page's ONE standing stream.
 *
 * THE FIRST TEST HERE IS THE ONE THAT MATTERS. `a booted page opens exactly one
 * standing stream` counts the server-streaming calls a whole page makes, and it
 * FAILS on the code this module replaced: that page opened seven — six panel
 * watches plus the feed tail — against a browser cap of about six, so the
 * seventh queued forever with no request on the wire and the root feed never
 * received a live row. jsdom has no such cap, which is exactly why the count
 * has to be asserted rather than the symptom.
 */
import { readFileSync, readdirSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import type { MessageInitShape } from "@bufbuild/protobuf";
import {
  PageAttachedSchema,
  PageSubscriptionEndedSchema,
  WatchPageResponseSchema,
  type SubscribePageRequest,
  type UnsubscribePageRequest,
  type WatchPageResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_page_pb";
import { WatchFooterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import { WatchTopbarResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_topbar_pb";
import { FooterViewSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { TopbarViewSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import type { FailureSink } from "../../src/failure/sink.js";
import { createTicker } from "../../src/clock.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { createAppContext, type AppContext } from "../../src/rpc/context.js";
import {
  clearClientFailures,
  standingClientFailure,
} from "../../src/rpc/link.js";
import { startPageStreams, type PageStreamsHandle } from "../../src/rpc/page-streams.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** Records every failure arm a page files. */
class RecordingSink implements FailureSink {
  readonly reported: string[] = [];
  report(kind: FailureKind): void {
    this.reported.push(kind.kind.case ?? "unset");
  }
  retract(): void {}
}

/** Let the router transport's zero-delay frames land. */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** A frame carrying the attachment latch. */
function attached(): WatchPageResponse {
  return create(WatchPageResponseSchema, {
    frame: { case: "attached", value: create(PageAttachedSchema, {}) },
  });
}

/** A frame carrying one footer push addressed to SUBSCRIPTION. */
function footerPush(subscription: string, mark: bigint): WatchPageResponse {
  return create(WatchPageResponseSchema, {
    frame: {
      case: "push",
      value: {
        subscription,
        payload: {
          case: "footer",
          value: create(WatchFooterResponseSchema, {
            footer: create(FooterViewSchema, { strip: { clock: { turnStartedAtMs: mark } } }),
          }),
        },
      },
    },
  });
}

/** A frame carrying one topbar push addressed to SUBSCRIPTION. */
function topbarPush(subscription: string, title: string): WatchPageResponse {
  return create(WatchPageResponseSchema, {
    frame: {
      case: "push",
      value: {
        subscription,
        payload: {
          case: "topbar",
          value: create(WatchTopbarResponseSchema, {
            topbar: create(TopbarViewSchema, { title: { text: title } }),
          }),
        },
      },
    },
  });
}

/** A frame carrying a FOOTER payload addressed to SUBSCRIPTION. */
function misaddressedPush(subscription: string): WatchPageResponse {
  return footerPush(subscription, 7n);
}

/** A frame saying one subscription is over, and HOW. */
function ended(
  subscription: string,
  how: MessageInitShape<typeof PageSubscriptionEndedSchema>["how"] = {
    case: "sourceEnded",
    value: {},
  },
): WatchPageResponse {
  return create(WatchPageResponseSchema, {
    frame: {
      case: "ended",
      value: create(PageSubscriptionEndedSchema, { subscription, how }),
    },
  });
}

/** What one scripted daemon recorded and can be told to say next. */
interface FakeDaemon {
  /** Every server-streaming call the page opened, by rpc name. */
  readonly streamOpens: string[];
  /** Every SubscribePage the page issued. */
  readonly subscribes: SubscribePageRequest[];
  /** Every UnsubscribePage the page issued. */
  readonly unsubscribes: UnsubscribePageRequest[];
  /** Push one frame down the page's stream. */
  send(frame: WatchPageResponse): void;
  /** End the page's stream, as a dropped link would. */
  endPageStream(): void;
  /** Refuse the next SubscribePage with this error. */
  refuseSubscribeWith(err: ConnectError | null): void;
  /** Refuse every UnsubscribePage with this error. */
  refuseUnsubscribeWith(err: ConnectError | null): void;
}

/**
 * A daemon that serves the page mux and nothing else.
 *
 * It records which STREAMING rpcs were opened, which is the count the first
 * test asserts: a page that reached for a dedicated `Watch*` stream would
 * appear here as a second name.
 */
function fakeDaemon(): { client: ReturnType<typeof createAgentReplClient>; daemon: FakeDaemon } {
  const streamOpens: string[] = [];
  const subscribes: SubscribePageRequest[] = [];
  const unsubscribes: UnsubscribePageRequest[] = [];
  let outbound: WatchPageResponse[] = [];
  let wake: (() => void) | null = null;
  let open = true;
  let refusal: ConnectError | null = null;
  let unsubscribeRefusal: ConnectError | null = null;

  const state: FakeDaemon = {
    streamOpens,
    subscribes,
    unsubscribes,
    send(frame: WatchPageResponse): void {
      outbound.push(frame);
      wake?.();
    },
    endPageStream(): void {
      open = false;
      wake?.();
    },
    refuseSubscribeWith(err: ConnectError | null): void {
      refusal = err;
    },
    refuseUnsubscribeWith(err: ConnectError | null): void {
      unsubscribeRefusal = err;
    },
  };

  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      watchPage: async function* () {
        streamOpens.push("WatchPage");
        open = true;
        outbound = [];
        for (;;) {
          while (outbound.length > 0) yield outbound.shift() as WatchPageResponse;
          if (!open) return;
          await new Promise<void>((resolve) => {
            wake = () => {
              wake = null;
              resolve();
            };
          });
        }
      },
      subscribePage: (req) => {
        if (refusal !== null) throw refusal;
        subscribes.push(req);
        return {};
      },
      unsubscribePage: (req) => {
        unsubscribes.push(req);
        if (unsubscribeRefusal !== null) throw unsubscribeRefusal;
        return {};
      },
      // The dedicated streams a page must NOT open any more. Each records its
      // own name, so the count test names the offender rather than a number.
      watchFooter: async function* () {
        streamOpens.push("WatchFooter");
        await new Promise<void>(() => {});
      },
      watchTopbar: async function* () {
        streamOpens.push("WatchTopbar");
        await new Promise<void>(() => {});
      },
      watchWorkspaceRoster: async function* () {
        streamOpens.push("WatchWorkspaceRoster");
        await new Promise<void>(() => {});
      },
      watchDaemonHolds: async function* () {
        streamOpens.push("WatchDaemonHolds");
        await new Promise<void>(() => {});
      },
      watchDaemon: async function* () {
        streamOpens.push("WatchDaemon");
        await new Promise<void>(() => {});
      },
      watchWebWorkspace: async function* () {
        streamOpens.push("WatchWebWorkspace");
        await new Promise<void>(() => {});
      },
      watchFeed: async function* () {
        streamOpens.push("WatchFeed");
        await new Promise<void>(() => {});
      },
    });
  });
  return { client: createAgentReplClient(transport), daemon: state };
}

/** A context whose page stream runs against DAEMON. */
function contextFor(
  client: ReturnType<typeof createAgentReplClient>,
  failures: FailureSink,
): AppContext {
  return createAppContext({
    client,
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures,
    composerEnabled: false,
    page: "page-1",
  });
}

describe("the page's one standing stream", () => {
  it("opens exactly one standing stream for every watch it holds", async () => {
    // Arrange: a page holding SIX watches at once, which is what the webapp
    // mounts and is exactly the count that exhausted the browser's per-host
    // connection budget when each took a connection of its own.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    // Act.
    const signals = [
      new AbortController(),
      new AbortController(),
      new AbortController(),
      new AbortController(),
      new AbortController(),
      new AbortController(),
    ];
    const kinds = ["roster", "webWorkspace", "daemon", "topbar", "footer", "holds"] as const;
    for (const [index, kind] of kinds.entries()) {
      void (async () => {
        for await (const _ of ctx.streams.watch(kind, { workspace: WORKSPACE }, signals[index].signal)) {
          // Drained; this test asserts the connection count, not the pushes.
        }
      })();
    }
    await settle();

    // Assert: one stream, six subscriptions.
    expect(daemon.streamOpens).toEqual(["WatchPage"]);
    expect(daemon.subscribes).toHaveLength(6);
    for (const controller of signals) controller.abort();
    ctx.quiesce();
  });

  it("carries a subscription's pushes to the caller that asked for them", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const seen: bigint[] = [];
    const controller = new AbortController();
    void (async () => {
      for await (const response of ctx.streams.watch(
        "footer",
        { workspace: WORKSPACE },
        controller.signal,
      )) {
        seen.push(response.footer?.strip?.clock?.turnStartedAtMs ?? 0n);
      }
    })();
    await settle();

    // Act.
    daemon.send(footerPush(daemon.subscribes[0].subscription, 42n));
    await settle();

    // Assert.
    expect(seen).toEqual([42n]);
    controller.abort();
    ctx.quiesce();
  });

  it("ends a subscription the daemon says is over", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    let over = false;
    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw; this test is about the ending.
      }
      over = true;
    })();
    await settle();

    // Act.
    daemon.send(ended(daemon.subscribes[0].subscription));
    await settle();

    // Assert.
    expect(over).toBe(true);
    ctx.quiesce();
  });

  it("ends every subscription when the page's own stream ends", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    let over = false;
    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
      over = true;
    })();
    await settle();

    // Act: the link drops.
    daemon.endPageStream();
    await settle();

    // Assert: a subscription cannot outlive the stream that carried it, or a
    // component would sit on a watch the daemon no longer holds.
    expect(over).toBe(true);
    ctx.quiesce();
  });

  it("refuses a subscription whose page stream dies before it attaches", async () => {
    // Arrange: a subscription opened while the page's stream is still being
    // accepted, so it is waiting on the attachment latch.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    const controller = new AbortController();
    // The outcome is captured BEFORE the stream is cut, because a rejection
    // with no handler yet attached is an unhandled rejection in this runtime —
    // and this test is about a rejection that happens while nothing is looking.
    const outcome = (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Never reached.
      }
      return null;
    })().then(
      () => null,
      (err: unknown) => err,
    );

    // Act: that stream dies without ever attaching.
    daemon.endPageStream();
    await settle();

    // Assert: the wait RESOLVES INTO A REFUSAL rather than staying pending —
    // which is the silent hang this whole module exists to remove. The
    // caller's own stream loop files its card and retries.
    expect(String(await outcome)).toMatch(/holds no stream/);
    ctx.quiesce();
  });

  it("joins the page's next stream when it is opened during an outage", async () => {
    // Arrange: the page's stream has dropped and has not reopened yet.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();
    daemon.endPageStream();
    await settle();

    // Act: a component reopens its watch during the outage, then the page's
    // stream comes back and attaches.
    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await vi.advanceTimersByTimeAsync(60_000);
    await settle();
    daemon.send(attached());
    await settle();

    // Assert: it subscribed onto the NEW stream rather than onto the dead one.
    expect(daemon.subscribes.map((req) => req.page)).toEqual(["page-1"]);
    controller.abort();
    ctx.quiesce();
  });

  it("surfaces a refused subscribe to the caller", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();
    daemon.refuseSubscribeWith(new ConnectError("no such feed token", Code.NotFound));

    // Act.
    const controller = new AbortController();
    const opened = (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Never reached.
      }
    })();

    // Assert: the daemon's own refusal, not something this layer invented.
    await expect(opened).rejects.toThrow(/no such feed token/);
    ctx.quiesce();
  });

  it("ends the subscription on the daemon when its caller cancels", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();

    // Act.
    controller.abort();
    await settle();

    // Assert: a collapsed bubble stops costing the daemon work, rather than
    // leaving a subscription nothing will ever read.
    expect(daemon.unsubscribes.map((req) => req.subscription)).toEqual([
      daemon.subscribes[0].subscription,
    ]);
    ctx.quiesce();
  });

  it("cancels the page's stream when the page goes quiet", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const failures = new RecordingSink();
    const ctx = contextFor(client, failures);
    await settle();
    daemon.send(attached());
    await settle();

    // Act.
    ctx.quiesce();
    await settle();
    await vi.advanceTimersByTimeAsync(10_000);

    // Assert: a quiesced page does not reopen, so the workspace's departure
    // leaves no card behind for a link that is correctly gone.
    expect(daemon.streamOpens).toEqual(["WatchPage"]);
    expect(failures.reported).toEqual([]);
  });

  it("reopens the page's stream after it drops", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    // Act.
    daemon.endPageStream();
    await settle();
    await vi.advanceTimersByTimeAsync(60_000);
    await settle();

    // Assert: the ONE stream is standing again, so the subscriptions that
    // ended with it have something to reopen onto.
    expect(daemon.streamOpens.filter((name) => name === "WatchPage").length).toBeGreaterThan(1);
    ctx.quiesce();
  });

  it("is cancelled by its own handle", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const failures = new RecordingSink();
    const ctx = contextFor(client, failures);
    await settle();
    const handle: PageStreamsHandle = startPageStreams(ctx, "page-2");
    await settle();

    // Act.
    handle.cancel();
    await settle();
    await vi.advanceTimersByTimeAsync(60_000);

    // Assert: exactly the page's own stream plus the context's, and neither
    // reopened after the cancel.
    expect(daemon.streamOpens).toEqual(["WatchPage", "WatchPage"]);
    ctx.quiesce();
  });

  it("ends a subscription whose source finished, without an error", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const controller = new AbortController();
    const outcome = (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
      return "concluded";
    })().then(
      (value) => value,
      (err: unknown) => err,
    );
    await settle();

    // Act.
    daemon.send(ended(daemon.subscribes[0].subscription, { case: "sourceEnded", value: {} }));
    await settle();

    // Assert: NOTHING FAILED — there is simply nothing further to push, and
    // the caller reads the clean conclusion a dedicated stream would have given
    // it rather than an error it would draw a card for.
    expect(await outcome).toBe("concluded");
    ctx.quiesce();
  });

  it("raises a failed subscription as the Connect error the dedicated rpc would have", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const controller = new AbortController();
    const outcome = (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
      return null;
    })().then(
      () => null,
      (err: unknown) => err,
    );
    await settle();

    // Act.
    daemon.send(
      ended(daemon.subscribes[0].subscription, {
        case: "failed",
        value: { code: "not_found", message: "no such feed token" },
      }),
    );
    await settle();

    // Assert: THE SAME ERROR, KEYED ON THE SAME CODE. The view draws the card
    // its own rpc's failure would have drawn, because it is handed the same
    // ConnectError — a multiplexed subscription has no status of its own.
    const err = ConnectError.from(await outcome);
    expect(err.code).toBe(Code.NotFound);
    expect(err.rawMessage).toBe("no such feed token");
    ctx.quiesce();
  });

  it("delivers the frames a failed subscription had already sent before raising", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const seen: bigint[] = [];
    const controller = new AbortController();
    const outcome = (async () => {
      for await (const response of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        seen.push(response.footer?.strip?.clock?.turnStartedAtMs ?? 0n);
      }
    })().then(
      () => null,
      (err: unknown) => err,
    );
    await settle();

    // Act: a push, then the failure.
    daemon.send(footerPush(daemon.subscribes[0].subscription, 5n));
    daemon.send(
      ended(daemon.subscribes[0].subscription, {
        case: "failed",
        value: { code: "internal", message: "the resolver died" },
      }),
    );
    await settle();

    // Assert: the frame was TRUE WHEN IT WAS SENT, and nothing will resend it,
    // so the failure does not take the backlog down with it.
    expect(seen).toEqual([5n]);
    expect(ConnectError.from(await outcome).rawMessage).toBe("the resolver died");
    ctx.quiesce();
  });

  it("refuses a failure whose code is not a Connect code", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const failures = new RecordingSink();
    const ctx = contextFor(client, failures);
    await settle();
    daemon.send(attached());
    await settle();

    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();

    // Act.
    daemon.send(
      ended(daemon.subscribes[0].subscription, {
        case: "failed",
        value: { code: "not_a_real_code", message: "whatever this is" },
      }),
    );
    await settle();

    // Assert: a daemon that spelled its own contract wrong is a MALFORMED
    // FRAME, filed as such. Minting `Code.Unknown` from it would draw a
    // plausible card for a wire nobody can trust.
    expect(failures.reported).toContain("frameUndecodable");
    controller.abort();
    ctx.quiesce();
  });

  it("draws nothing for an ending it had already dropped the subscription for", async () => {
    // Arrange: a subscription the caller cancels, which drops the id from the
    // page's map BEFORE `UnsubscribePage` is sent — so the daemon's
    // `unsubscribed` ending always arrives for an id this page no longer holds.
    const { client, daemon } = fakeDaemon();
    const failures = new RecordingSink();
    const ctx = contextFor(client, failures);
    await settle();
    daemon.send(attached());
    await settle();

    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();
    const subscription = daemon.subscribes[0].subscription;
    controller.abort();
    await settle();

    // Act: the ending the daemon announces for the end this page asked for.
    daemon.send(ended(subscription, { case: "unsubscribed", value: {} }));
    await settle();

    // Assert: NOTHING HAPPENED THAT A READER DID NOT ASK FOR. No card, and the
    // record is debug — this is the ordinary shape of every unsubscribe, not
    // an event.
    expect(failures.reported).toEqual([]);
    ctx.quiesce();
  });

  it("refuses an ending that does not say how", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const failures = new RecordingSink();
    const ctx = contextFor(client, failures);
    await settle();
    daemon.send(attached());
    await settle();

    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();

    // Act: the oneof unset, which is a contract violation and not a fourth
    // kind of ending.
    daemon.send(
      create(WatchPageResponseSchema, {
        frame: {
          case: "ended",
          value: create(PageSubscriptionEndedSchema, {
            subscription: daemon.subscribes[0].subscription,
          }),
        },
      }),
    );
    await settle();

    // Assert: surfaced loudly through the unreachable-arm path, never guessed
    // at as one of the three.
    expect(failures.reported).toContain("frameUndecodable");
    controller.abort();
    ctx.quiesce();
  });

  it("applies two views' interleaved frames in the order they arrived", async () => {
    // Arrange: two subscriptions of DIFFERENT kinds on the one stream, which
    // is the arrangement a dedicated stream apiece never had to order.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const arrivals: string[] = [];
    const controller = new AbortController();
    void (async () => {
      for await (const response of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        arrivals.push(`footer:${response.footer?.strip?.clock?.turnStartedAtMs ?? 0n}`);
      }
    })();
    void (async () => {
      for await (const response of ctx.streams.watch("topbar", { workspace: WORKSPACE }, controller.signal)) {
        arrivals.push(`topbar:${response.topbar?.title?.text ?? ""}`);
      }
    })();
    await settle();
    const [footerSub, topbarSub] = daemon.subscribes.map((req) => req.subscription);

    // Act: alternate the two views on the single stream.
    daemon.send(footerPush(footerSub, 1n));
    daemon.send(topbarPush(topbarSub, "one"));
    daemon.send(footerPush(footerSub, 2n));
    daemon.send(topbarPush(topbarSub, "two"));
    await settle();

    // Assert: ARRIVAL ORDER, not per-view batching. One socket carries both,
    // so a frame the daemon wrote second must not be drawn first.
    expect(arrivals).toEqual(["footer:1", "topbar:one", "footer:2", "topbar:two"]);
    controller.abort();
    ctx.quiesce();
  });

  it("refuses a frame addressed to this subscription but carrying another view's payload", async () => {
    // Arrange: one topbar subscription.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const seen: unknown[] = [];
    const controller = new AbortController();
    const outcome = (async () => {
      for await (const response of ctx.streams.watch("topbar", { workspace: WORKSPACE }, controller.signal)) {
        seen.push(response);
      }
    })().then(
      () => null,
      (err: unknown) => err,
    );
    await settle();

    // Act: the daemon addresses it correctly but sends a FOOTER payload.
    daemon.send(misaddressedPush(daemon.subscribes[0].subscription));
    await settle();

    // Assert: ONE UNREADABLE FRAME, not another view's state drawn as this
    // one's. The topbar never sees a footer, and the caller's own stream loop
    // is handed the malformed frame to file.
    expect(seen).toEqual([]);
    expect(String(await outcome)).toMatch(/asked for topbar and was sent footer/);
    ctx.quiesce();
  });

  it("does not deliver one view's frame to another view's subscription", async () => {
    // Arrange: two subscriptions, so a frame has more than one place to go.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const footerSeen: bigint[] = [];
    const topbarSeen: string[] = [];
    const controller = new AbortController();
    void (async () => {
      for await (const response of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        footerSeen.push(response.footer?.strip?.clock?.turnStartedAtMs ?? 0n);
      }
    })();
    void (async () => {
      for await (const response of ctx.streams.watch("topbar", { workspace: WORKSPACE }, controller.signal)) {
        topbarSeen.push(response.topbar?.title?.text ?? "");
      }
    })();
    await settle();
    const [footerSub] = daemon.subscribes.map((req) => req.subscription);

    // Act: a push for the footer alone.
    daemon.send(footerPush(footerSub, 99n));
    await settle();

    // Assert: the id on the frame is what routes it, so a page holding many
    // views does not fan one view's push out to all of them.
    expect(footerSeen).toEqual([99n]);
    expect(topbarSeen).toEqual([]);
    controller.abort();
    ctx.quiesce();
  });

  it("leaves no subscription behind when its caller cancels", async () => {
    // Arrange: two subscriptions; one of them is a bubble's tail that will
    // collapse, which is the unbounded case the mux exists for.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();

    const kept = new AbortController();
    const collapsed = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, kept.signal)) {
        // Nothing to draw.
      }
    })();
    void (async () => {
      for await (const _ of ctx.streams.watch("topbar", { workspace: WORKSPACE }, collapsed.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();
    const [keptSub, collapsedSub] = daemon.subscribes.map((req) => req.subscription);

    // Act.
    collapsed.abort();
    await settle();

    // Act again: the daemon pushes to the id that was just retired.
    daemon.send(topbarPush(collapsedSub, "after"));
    await settle();

    // Assert: exactly the collapsed one was ended, the kept one was not, and
    // a push for the retired id reaches nobody rather than reviving it.
    expect(daemon.unsubscribes.map((req) => req.subscription)).toEqual([collapsedSub]);
    expect(daemon.subscribes.map((req) => req.subscription)).toEqual([keptSub, collapsedSub]);
    kept.abort();
    ctx.quiesce();
  });
});

/**
 * THE COUNT, ENFORCED AT ITS SOURCE.
 *
 * The one-connection guarantee is a property of the WHOLE page, and no single
 * component test can see it: each component is correct on its own while the
 * seventh stream still queues. So it is checked where it IS decidable — every
 * module but the page mux is forbidden to call a server-streaming rpc on the
 * client at all, which makes a second connection something the tree does not
 * contain rather than something a reviewer has to notice.
 *
 * THIS TEST FAILS ON THE CODE THE PAGE MUX REPLACED. That page called
 * `client.watchWorkspaceRoster`, `client.watchWebWorkspace`, `client.watchDaemon`,
 * `client.watchTopbar`, `client.watchFooter`, `client.watchDaemonHolds`,
 * `client.watchFeed` (from both the root feed and every bubble) and
 * `client.watchLoginTerminal` from eight different modules — against a browser
 * cap measured, in the e2e sandbox on this daemon, at exactly six.
 */
/**
 * THE FORBIDDEN LIST IS DERIVED, NEVER TYPED OUT.
 *
 * A hand-written list guards only the rpcs somebody remembered to add to it: a
 * NEW server-streaming rpc in the service would be a second connection this
 * check silently permits, which is the exact class of defect the mux exists to
 * make impossible. So it is read off the service descriptor — every
 * server-streaming method except the mux's own — and a streaming rpc landed
 * tomorrow is guarded the moment its bindings regenerate.
 */
/**
 * THE FINITE STREAMS: server-streaming rpcs that END on their own with a
 * terminal frame (`LoadFeedThrough`: pages, then `reached` or `error`). They
 * hold a connection for the length of one walk, as a slow unary call does, not
 * for the page's life, so they do not spend the standing-stream budget this
 * guard protects. Named here, one by one: a new streaming rpc is guarded until
 * someone states in this list that it is finite.
 */
const FINITE_STREAMING_RPCS: readonly string[] = ["loadFeedThrough"];

const STREAMING_RPCS: readonly string[] = Object.entries(AgentRepl.method)
  .filter(
    ([name, method]) =>
      method.methodKind === "server_streaming" &&
      name !== "watchPage" &&
      !FINITE_STREAMING_RPCS.includes(name),
  )
  .map(([name]) => name);

/** Every .ts file under src/, recursively. */
function sourceFiles(dir: string): string[] {
  const out: string[] = [];
  for (const entry of readdirSync(dir, { withFileTypes: true })) {
    const path = join(dir, entry.name);
    if (entry.isDirectory()) out.push(...sourceFiles(path));
    else if (entry.name.endsWith(".ts")) out.push(path);
  }
  return out;
}

describe("the page's connection budget", () => {
  it("opens a standing stream from nowhere but the page mux", () => {
    // Arrange.
    const srcRoot = join(fileURLToPath(new URL(".", import.meta.url)), "../../src");
    const mux = join(srcRoot, "rpc/page-streams.ts");

    // Act.
    const offenders: string[] = [];
    for (const file of sourceFiles(srcRoot)) {
      if (file === mux) continue;
      const body = readFileSync(file, "utf8");
      for (const rpc of STREAMING_RPCS) {
        if (body.includes(`.${rpc}(`)) offenders.push(`${file}: ${rpc}`);
      }
    }

    // Assert: a page holds ONE connection because there is nowhere else in the
    // tree that opens one, not because six components each remembered not to.
    expect(offenders).toEqual([]);
  });

  it("guards every server-streaming rpc the service declares but the mux's own", () => {
    // Arrange: the rpcs the page used to open a connection apiece for, plus
    // the host watch Emacs holds. Named here so a method DROPPED from the
    // service is noticed too, rather than quietly shrinking the guard.
    const expected = [
      "watchFeed",
      "watchWorkspaceRoster",
      "watchTopbar",
      "watchFooter",
      "watchDaemonHolds",
      "watchHostWorkspace",
      "watchDaemon",
      "watchWebWorkspace",
      "watchLoginTerminal",
    ];

    // Act.
    const guarded = [...STREAMING_RPCS].sort();

    // Assert: the derivation covers exactly those, and `watchPage` — the one
    // stream a page is allowed to hold — is not among them.
    expect(guarded).toEqual([...expected].sort());
    expect(guarded).not.toContain("watchPage");
  });

  it("exempts only rpcs the service still declares as server-streaming", () => {
    // Arrange / Act.
    const stale = FINITE_STREAMING_RPCS.filter(
      (name) => AgentRepl.method[name as keyof typeof AgentRepl.method]?.methodKind !== "server_streaming",
    );

    // Assert: an exemption for a dropped or reshaped rpc is noticed, not kept.
    expect(stale).toEqual([]);
  });
});

describe("the page mux and the client's link verdict", () => {
  afterEach(() => {
    clearClientFailures();
  });

  it("reports a subscription whose source finished, naming the subscription", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();
    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();

    // Act.
    const subscription = daemon.subscribes[0].subscription;
    daemon.send(ended(subscription, { case: "sourceEnded", value: {} }));
    await settle();

    // Assert.
    expect(standingClientFailure()).toEqual({
      kind: "subscription_source_ended",
      substatus: "daemon unreachable",
      activity: `the ${subscription} page subscription ended (source_ended)`,
    });
    ctx.quiesce();
  });

  it("reports an UnsubscribePage that could not be delivered", async () => {
    // Arrange.
    const { client, daemon } = fakeDaemon();
    const ctx = contextFor(client, new RecordingSink());
    await settle();
    daemon.send(attached());
    await settle();
    daemon.refuseUnsubscribeWith(new ConnectError("no route", Code.Unavailable));
    const controller = new AbortController();
    void (async () => {
      for await (const _ of ctx.streams.watch("footer", { workspace: WORKSPACE }, controller.signal)) {
        // Nothing to draw.
      }
    })();
    await settle();

    // Act: the page drops the subscription, which sends the unsubscribe.
    const subscription = daemon.subscribes[0].subscription;
    controller.abort();
    await settle();

    // Assert.
    expect(standingClientFailure()?.activity).toBe(
      `UnsubscribePage ${subscription} could not be delivered`,
    );
    ctx.quiesce();
  });
});
