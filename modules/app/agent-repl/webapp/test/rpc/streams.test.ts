import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  WatchFooterResponseSchema,
  type WatchFooterResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import { FooterViewSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { ForwardingLogger, setLogger } from "../../src/log.js";
import type { ClientFailureArm, FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient, type AgentReplClient } from "../../src/rpc/client.js";
import { type AppContext } from "../../src/rpc/context.js";
import { testAppContext } from "./app-context.js";
import { MalformedView, UnknownPushArm } from "../../src/rpc/malformed.js";
import { createTicker } from "../../src/clock.js";
import {
  clearClientFailures,
  onClientVerdict,
  reportClientFailure,
  standingClientFailure,
} from "../../src/rpc/link.js";
import { watchStream, type StreamEnd } from "../../src/rpc/streams.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const UNKNOWN = [{ no: 999, wireType: 0, data: new Uint8Array([1]) }];
const BACKOFF = { initialMs: 250, maxMs: 5000 };

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** Records every report/retract, so a test can assert the arms in order. */
class RecordingSink implements FailureSink {
  readonly reported: string[] = [];
  readonly retracted: ClientFailureArm[] = [];
  report(kind: FailureKind): void {
    this.reported.push(kind.kind.case ?? "unset");
  }
  retract(arm: ClientFailureArm): void {
    this.retracted.push(arm);
  }
}

/** A clean footer push. */
function push(): WatchFooterResponse {
  return create(WatchFooterResponseSchema, { footer: create(FooterViewSchema, {}) });
}

/** A push carrying a field this build has no descriptor for. */
function undecodablePush(): WatchFooterResponse {
  const response = push();
  response.$unknown = UNKNOWN;
  return response;
}

/**
 * A client whose WatchFooter yields SCRIPT[n] on its nth open. Each script
 * entry is the run's pushes; the run then ENDS, which a standing stream treats
 * as a transport failure. `openCount` reports how many times it was opened.
 */
function scriptedClient(script: ReadonlyArray<ReadonlyArray<WatchFooterResponse> | Error>) {
  const state = { openCount: 0 };
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      watchFooter: async function* () {
        const run = script[Math.min(state.openCount, script.length - 1)];
        state.openCount += 1;
        if (run instanceof Error) throw run;
        for (const response of run) yield response;
      },
    });
  });
  return { client: createAgentReplClient(transport), state };
}

function contextFor(client: AgentReplClient, failures: FailureSink): AppContext {
  return testAppContext({
    client,
    workspace: WORKSPACE,
    ticker: createTicker(1000),
    failures,
    composerEnabled: false,
  });
}

function open(
  ctx: AppContext,
  onPush: (r: WatchFooterResponse) => void,
  onEnd?: (e: StreamEnd) => void,
) {
  return watchStream(ctx, {
    name: "WatchFooter",
    schema: WatchFooterResponseSchema,
    open: (client, signal) => client.watchFooter({ workspace: WORKSPACE }, { signal }),
    onPush,
    onEnd,
    backoff: BACKOFF,
  });
}

/**
 * Let the stream's promise chain run WITHOUT advancing the clock.
 *
 * The router transport hands frames on through a zero-delay timer rather than
 * a bare microtask, so draining the microtask queue alone never delivers a
 * push. Advancing by 0 runs those without moving the clock the backoff
 * assertions read, and the loop covers a run that schedules another.
 */
async function settle(): Promise<void> {
  for (let i = 0; i < 20; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** Advance the fake clock and let the resulting continuations run. */
async function advance(ms: number): Promise<void> {
  await vi.advanceTimersByTimeAsync(ms);
  await settle();
}

describe("watchStream: pushes", () => {
  it("delivers a push to onPush", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const seen: WatchFooterResponse[] = [];
    // ACT
    const handle = open(contextFor(client, sink), (r) => seen.push(r));
    await settle();
    handle.cancel();
    // ASSERT
    expect(seen).toHaveLength(1);
  });

  it("delivers every push of a run, in order", async () => {
    const sink = new RecordingSink();
    const a = push();
    const b = push();
    const { client } = scriptedClient([[a, b]]);
    const seen: WatchFooterResponse[] = [];
    const handle = open(contextFor(client, sink), (r) => seen.push(r));
    await settle();
    handle.cancel();
    expect(seen).toHaveLength(2);
  });
});

describe("watchStream: an unreadable frame", () => {
  it("reports frameUndecodable for a push carrying an unknown field", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([[undecodablePush()]]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    expect(sink.reported).toContain("frameUndecodable");
  });

  it("SKIPS the bad frame rather than handing it to onPush", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([[undecodablePush()]]);
    const seen: WatchFooterResponse[] = [];
    const handle = open(contextFor(client, sink), (r) => seen.push(r));
    await settle();
    handle.cancel();
    expect(seen).toHaveLength(0);
  });

  it("KEEPS THE STREAM: the next frame is still delivered", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[undecodablePush(), push()]]);
    const seen: WatchFooterResponse[] = [];
    // ACT
    const handle = open(contextFor(client, sink), (r) => seen.push(r));
    await settle();
    handle.cancel();
    // ASSERT
    expect(seen).toHaveLength(1);
  });

  it("does not reopen the stream over one bad frame", async () => {
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[undecodablePush(), push()]]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    expect(state.openCount).toBe(1);
  });

  it("reports frameUndecodable when onPush itself refuses the view", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const handle = open(contextFor(client, sink), () => {
      throw new MalformedView("FooterView.status", "a oneof sets no arm");
    });
    await settle();
    handle.cancel();
    expect(sink.reported).toContain("frameUndecodable");
  });

  it("keeps the stream when onPush refuses one view", async () => {
    const sink = new RecordingSink();
    let calls = 0;
    const { client } = scriptedClient([[push(), push()]]);
    const handle = open(contextFor(client, sink), () => {
      calls += 1;
      if (calls === 1) throw new MalformedView("FooterView.status", "unset");
    });
    await settle();
    handle.cancel();
    expect(calls).toBe(2);
  });

  it("does not swallow a non-MalformedView throw from onPush", async () => {
    // ARRANGE: a genuine bug in a renderer must not be filed as a bad frame.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    // ACT
    const handle = open(contextFor(client, sink), () => {
      throw new TypeError("undefined is not a function");
    });
    await settle();
    handle.cancel();
    // ASSERT: it surfaced as the run's end, not as a skipped frame.
    expect(sink.reported).not.toContain("frameUndecodable");
  });
});

describe("watchStream: forward-compat skew (an unknown push arm)", () => {
  it("skips the frame quietly, filing NO frameUndecodable", async () => {
    // ARRANGE: a newer daemon set a top-level push arm this build cannot draw.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    // ACT
    const handle = open(contextFor(client, sink), () => {
      throw new UnknownPushArm("WatchFooterResponse.push", "mutationProgress");
    });
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).not.toContain("frameUndecodable");
  });

  it("KEEPS THE STREAM: a later frame this build CAN draw is still delivered", async () => {
    // ARRANGE: the first frame carries the unknown arm, the second is drawable.
    const sink = new RecordingSink();
    let calls = 0;
    const { client } = scriptedClient([[push(), push()]]);
    const drawn: WatchFooterResponse[] = [];
    // ACT
    const handle = open(contextFor(client, sink), (r) => {
      calls += 1;
      if (calls === 1) throw new UnknownPushArm("WatchFooterResponse.push", "mutationProgress");
      drawn.push(r);
    });
    await settle();
    handle.cancel();
    // ASSERT
    expect(drawn).toHaveLength(1);
  });

  it("does not reopen the stream over one skew frame", async () => {
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[push(), push()]]);
    let calls = 0;
    const handle = open(contextFor(client, sink), () => {
      calls += 1;
      if (calls === 1) throw new UnknownPushArm("WatchFooterResponse.push", "mutationProgress");
    });
    await settle();
    handle.cancel();
    expect(state.openCount).toBe(1);
  });

  it("logs the skew at info, naming the arm, so the skip is greppable", async () => {
    // ARRANGE
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { client } = scriptedClient([[push()]]);
    // ACT
    const handle = open(contextFor(client, new RecordingSink()), () => {
      throw new UnknownPushArm("WatchFooterResponse.push", "mutationProgress");
    });
    await settle();
    handle.cancel();
    // ASSERT
    expect(
      lines.some(
        ([level, line]) =>
          level === "info" &&
          line.includes("rpc.stream-frame-forward-skew") &&
          line.includes("mutationProgress"),
      ),
    ).toBe(true);
  });

  it("does NOT log the skew as an undecodable frame, keeping the loud channel clean", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { client } = scriptedClient([[push()]]);
    const handle = open(contextFor(client, new RecordingSink()), () => {
      throw new UnknownPushArm("WatchFooterResponse.push", "mutationProgress");
    });
    await settle();
    handle.cancel();
    expect(lines.some(([, line]) => line.includes("rpc.stream-frame-undecodable"))).toBe(false);
  });

  it("still files frameUndecodable for a NESTED unknown arm, which is a real malformation", async () => {
    // ARRANGE: a plain MalformedView (a nested arm), NOT an UnknownPushArm.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    // ACT
    const handle = open(contextFor(client, sink), () => {
      throw new MalformedView("FooterView.activity", "arm 'somethingNew' is not one this build can draw");
    });
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).toContain("frameUndecodable");
  });
});

describe("watchStream: a stream that ended", () => {
  it("reports daemonUnreachable when the producer simply concluded", async () => {
    // ARRANGE: a standing stream never concludes on its own.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    // ACT
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).toContain("daemonUnreachable");
  });

  it("reports daemonUnreachable when the stream threw", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([new ConnectError("gone", Code.Unavailable)]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    expect(sink.reported).toContain("daemonUnreachable");
  });

  it("tells onEnd a clean conclusion apart from a thrown one", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const ends: StreamEnd[] = [];
    const handle = open(contextFor(client, sink), () => {}, (e) => ends.push(e));
    await settle();
    handle.cancel();
    expect(ends[0]?.kind).toBe("producer_ended");
  });

  it("tells onEnd a thrown run was a transport failure", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([new ConnectError("gone", Code.Unavailable)]);
    const ends: StreamEnd[] = [];
    const handle = open(contextFor(client, sink), () => {}, (e) => ends.push(e));
    await settle();
    handle.cancel();
    expect(ends[0]?.kind).toBe("transport_failure");
  });

  it("reopens after the first backoff", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[push()], []]);
    // ACT
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    await advance(250);
    handle.cancel();
    // ASSERT
    expect(state.openCount).toBe(2);
  });

  it("does NOT reopen before the first backoff elapses", async () => {
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[push()], []]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    await advance(249);
    handle.cancel();
    expect(state.openCount).toBe(1);
  });

  it("doubles the backoff after a second failure", async () => {
    // ARRANGE: run 1 ends, wait 250; run 2 ends, wait 500.
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[], [], []]);
    // ACT
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    await advance(250);
    await advance(499);
    handle.cancel();
    // ASSERT: still on the second open, because 500 has not elapsed.
    expect(state.openCount).toBe(2);
  });

  it("opens the third time once the doubled backoff elapses", async () => {
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[], [], []]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    await advance(250);
    await advance(500);
    handle.cancel();
    expect(state.openCount).toBe(3);
  });

  it("caps the backoff at five seconds rather than doubling forever", async () => {
    // ARRANGE: 250, 500, 1000, 2000, 4000, then 5000 and 5000.
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[]]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    for (const wait of [250, 500, 1000, 2000, 4000, 5000]) await advance(wait);
    const opensBefore = state.openCount;
    // ACT: the next wait is the cap, not 8000.
    await advance(5000);
    handle.cancel();
    // ASSERT
    expect(state.openCount).toBe(opensBefore + 1);
  });

  it("RETRACTS daemonUnreachable on the first push after a failure", async () => {
    // ARRANGE: run 1 ends immediately, run 2 pushes.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[], [push()]]);
    // ACT
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    await advance(250);
    handle.cancel();
    // ASSERT
    expect(sink.retracted).toEqual(["daemonUnreachable"]);
  });

  it("does not retract before a failure has been reported", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    expect(sink.retracted).toEqual([]);
  });

  it("re-arms the backoff after a recovery, so a flapping link does not creep to the cap", async () => {
    // ARRANGE: fail, reopen and push, fail again -- the next wait is 250 again.
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[], [push()], []]);
    // ACT
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    await advance(250);
    await advance(250);
    handle.cancel();
    // ASSERT
    expect(state.openCount).toBe(3);
  });
});

/**
 * The stand-in planned-ending frame: a footer response with NO footer. The
 * footer stream carries no ending arm; the recognizer is a predicate, so
 * "no footer" stands in for `push.case === "ending"`.
 */
function endingPush(): WatchFooterResponse {
  return create(WatchFooterResponseSchema, {});
}

/** Open with a `plannedEnding` recognizer that names `endingPush()` frames. */
function openWithEnding(
  ctx: AppContext,
  onPush: (r: WatchFooterResponse) => void,
) {
  return watchStream(ctx, {
    name: "WatchFooter",
    schema: WatchFooterResponseSchema,
    open: (client, signal) => client.watchFooter({ workspace: WORKSPACE }, { signal }),
    plannedEnding: (r) => r.footer === undefined,
    onPush,
    backoff: BACKOFF,
  });
}

describe("watchStream: a planned ending", () => {
  it("files NO daemonUnreachable when the run ends cleanly after the ending", async () => {
    // ARRANGE: run 1 ends as planned; the reopened run 2 STANDS, as a live
    // daemon's stream does, so any card filed could only be run 1's.
    const sink = new RecordingSink();
    let opens = 0;
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        watchFooter: async function* () {
          opens += 1;
          if (opens === 1) {
            yield push();
            yield endingPush();
            return;
          }
          await new Promise<never>(() => undefined);
        },
      });
    });
    // ACT
    const handle = openWithEnding(contextFor(createAgentReplClient(transport), sink), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).not.toContain("daemonUnreachable");
  });

  it("reopens at once, without waiting out a backoff", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const ending = endingPush();
    const { client, state } = scriptedClient([[ending], [], []]);
    // ACT
    const handle = openWithEnding(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    // ASSERT: the planned end reopened with no clock advance.
    expect(state.openCount).toBe(2);
  });

  it("never hands the ending frame to onPush", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const ending = endingPush();
    const { client } = scriptedClient([[ending], []]);
    const seen: WatchFooterResponse[] = [];
    // ACT
    const handle = openWithEnding(contextFor(client, sink), (r) => seen.push(r));
    await settle();
    handle.cancel();
    // ASSERT
    expect(seen).toEqual([]);
  });

  it("still files daemonUnreachable for a clean end WITHOUT the ending", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    // ACT
    const handle = openWithEnding(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).toContain("daemonUnreachable");
  });

  it("still files daemonUnreachable for a run that THROWS after the ending", async () => {
    // ARRANGE: the run delivers the ending, then the transport fails.
    const sink = new RecordingSink();
    const ending = endingPush();
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        watchFooter: async function* () {
          yield ending;
          throw new ConnectError("gone", Code.Unavailable);
        },
      });
    });
    // ACT
    const handle = openWithEnding(contextFor(createAgentReplClient(transport), sink), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).toContain("daemonUnreachable");
  });

  it("does not carry the ending into the next run", async () => {
    // ARRANGE: run 1 ends as planned; run 2 ends cleanly with NO ending.
    const sink = new RecordingSink();
    const ending = endingPush();
    const { client } = scriptedClient([[ending], [push()], []]);
    // ACT
    const handle = openWithEnding(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(sink.reported).toContain("daemonUnreachable");
  });

  it("logs the planned end at info, never as a transport failure", async () => {
    // ARRANGE
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const ending = endingPush();
    const { client } = scriptedClient([[ending], [push()]]);
    // ACT
    const handle = openWithEnding(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(
      lines.some(([level, line]) => level === "info" && line.includes("rpc.stream-ended-planned")),
    ).toBe(true);
  });
});

describe("watchStream: cancel", () => {
  it("stops reopening", async () => {
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[]]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    await advance(5000);
    expect(state.openCount).toBe(1);
  });

  it("files no daemonUnreachable for the cancelled run", async () => {
    // ARRANGE: a stream that never ended on its own.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push(), push()]]);
    // ACT
    const handle = open(contextFor(client, sink), () => {});
    handle.cancel();
    await settle();
    // ASSERT
    expect(sink.reported).not.toContain("daemonUnreachable");
  });

  it("is idempotent", async () => {
    const sink = new RecordingSink();
    const { client } = scriptedClient([[]]);
    const handle = open(contextFor(client, sink), () => {});
    handle.cancel();
    expect(() => handle.cancel()).not.toThrow();
  });

  it("returns early from a backoff wait rather than idling it out", async () => {
    const sink = new RecordingSink();
    const { client, state } = scriptedClient([[]]);
    const handle = open(contextFor(client, sink), () => {});
    await settle();
    handle.cancel();
    await advance(10_000);
    expect(state.openCount).toBe(1);
  });
});

describe("watchStream: logging", () => {
  it("logs a skipped frame at error, so the evidence is not only a card", async () => {
    // ARRANGE
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { client } = scriptedClient([[undecodablePush()]]);
    // ACT
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(
      lines.some(([level, line]) => level === "error" && line.includes("rpc.stream-frame-undecodable")),
    ).toBe(true);
  });

  it("logs a stream that ended at error", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { client } = scriptedClient([[]]);
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    expect(
      lines.some(([level, line]) => level === "error" && line.includes("rpc.stream-transport-failure")),
    ).toBe(true);
  });

  it("logs the recovery, so a retraction is traceable", async () => {
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const { client } = scriptedClient([[], [push()]]);
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    await advance(250);
    handle.cancel();
    expect(lines.some(([, line]) => line.includes("rpc.stream-recovered"))).toBe(true);
  });
});

/**
 * A client whose WatchFooter yields one push and then STANDS — the shape a
 * component stream actually has, so nothing but a cancel can end it.
 */
function standingClient() {
  const state = { openCount: 0 };
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      watchFooter: async function* () {
        state.openCount += 1;
        yield push();
        await new Promise<never>(() => undefined);
      },
    });
  });
  return { client: createAgentReplClient(transport), state };
}

describe("watchStream: the page going quiet", () => {
  it("stops the stream, since the workspace moved to a daemon this page never dials", async () => {
    // ARRANGE
    const { client, state } = standingClient();
    const ctx = contextFor(client, new RecordingSink());
    open(ctx, () => undefined);
    await settle();
    // ACT
    ctx.quiesce();
    await advance(10_000);
    // ASSERT: one open only — no reopen, no backoff.
    expect(state.openCount).toBe(1);
  });

  it("files no unreachable card for the run the quiesce ended", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = standingClient();
    const ctx = contextFor(client, sink);
    open(ctx, () => undefined);
    await settle();
    // ACT
    ctx.quiesce();
    await advance(10_000);
    // ASSERT
    expect(sink.reported).not.toContain("daemonUnreachable");
  });

  it("never opens at all when the page was already quiet", async () => {
    // ARRANGE
    const { client, state } = standingClient();
    const ctx = contextFor(client, new RecordingSink());
    ctx.quiesce();
    // ACT
    open(ctx, () => undefined);
    await advance(10_000);
    // ASSERT
    expect(state.openCount).toBe(0);
  });
});

describe("watchStream: onReconnected", () => {
  it("fires on the first push after a run that ended without our cancel", async () => {
    // ARRANGE
    const { client } = scriptedClient([[], [push()]]);
    const ctx = contextFor(client, new RecordingSink());
    const reconnected = vi.fn();
    const handle = watchStream(ctx, {
      name: "WatchFooter",
      schema: WatchFooterResponseSchema,
      open: (c, signal) => c.watchFooter({ workspace: WORKSPACE }, { signal }),
      onPush: () => undefined,
      onReconnected: reconnected,
      backoff: BACKOFF,
    });
    // ACT
    await settle();
    await advance(250);
    handle.cancel();
    // ASSERT
    expect(reconnected).toHaveBeenCalledTimes(1);
  });

  it("does not fire on a first run that never dropped", async () => {
    // ARRANGE
    const { client } = scriptedClient([[push()]]);
    const ctx = contextFor(client, new RecordingSink());
    const reconnected = vi.fn();
    const handle = watchStream(ctx, {
      name: "WatchFooter",
      schema: WatchFooterResponseSchema,
      open: (c, signal) => c.watchFooter({ workspace: WORKSPACE }, { signal }),
      onPush: () => undefined,
      onReconnected: reconnected,
      backoff: BACKOFF,
    });
    // ACT
    await settle();
    handle.cancel();
    // ASSERT
    expect(reconnected).not.toHaveBeenCalled();
  });
});

describe("watchStream: the link's health, published page-wide", () => {
  it("notes a readable frame on the context", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const ctx = contextFor(client, sink);
    const noted = vi.fn();
    ctx.onPush(noted);
    // ACT
    const handle = open(ctx, () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(noted).toHaveBeenCalled();
  });

  it("notes the frame BEFORE it is drawn", async () => {
    // ARRANGE: a push that itself raises a notice (the shutdown announcement)
    // must not be taken back down by its own arrival, so the order is fixed.
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const ctx = contextFor(client, sink);
    const order: string[] = [];
    ctx.onPush(() => order.push("noted"));
    // ACT
    const handle = open(ctx, () => order.push("drawn"));
    await settle();
    handle.cancel();
    // ASSERT
    expect(order.slice(0, 2)).toEqual(["noted", "drawn"]);
  });

  it("notes nothing for a frame it could not read", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[undecodablePush()]]);
    const ctx = contextFor(client, sink);
    const noted = vi.fn();
    ctx.onPush(noted);
    // ACT
    const handle = open(ctx, () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(noted).not.toHaveBeenCalled();
  });
});

describe("watchStream: the link coming back, published page-wide", () => {
  it("notes the link restored on the first frame after a run that dropped", async () => {
    // ARRANGE
    const { client } = scriptedClient([[], [push()]]);
    const ctx = contextFor(client, new RecordingSink());
    const restored = vi.fn();
    ctx.onLinkRestored(restored);
    // ACT
    const handle = open(ctx, () => {});
    await settle();
    await advance(250);
    handle.cancel();
    // ASSERT
    expect(restored).toHaveBeenCalledTimes(1);
  });

  it("notes nothing for frames on a link that never dropped", async () => {
    // ARRANGE
    const { client } = scriptedClient([[push(), push()]]);
    const ctx = contextFor(client, new RecordingSink());
    const restored = vi.fn();
    ctx.onLinkRestored(restored);
    // ACT
    const handle = open(ctx, () => {});
    await settle();
    handle.cancel();
    // ASSERT
    expect(restored).not.toHaveBeenCalled();
  });

  it("notes the link restored BEFORE the frame is drawn", async () => {
    // ARRANGE: a reconnected stream whose first frame is itself a new
    // announcement must not have it taken down by its own arrival.
    const { client } = scriptedClient([[], [push()]]);
    const ctx = contextFor(client, new RecordingSink());
    const order: string[] = [];
    ctx.onLinkRestored(() => order.push("restored"));
    // ACT
    const handle = open(ctx, () => order.push("drawn"));
    await settle();
    await advance(250);
    handle.cancel();
    // ASSERT
    expect(order.slice(0, 2)).toEqual(["restored", "drawn"]);
  });
});

describe("watchStream: cancelling mid-run", () => {
  /**
   * A stream whose producer is a local generator rather than the transport, so
   * a frame is still waiting when the handle is cancelled. The signal is
   * deliberately ignored: the loop's own `cancelled` check is what this
   * exercises, not the abort.
   */
  function openLocal(
    ctx: AppContext,
    frames: readonly WatchFooterResponse[],
    onPush: (r: WatchFooterResponse) => void,
    onEnd?: (e: StreamEnd) => void,
  ) {
    return watchStream(ctx, {
      name: "WatchFooter",
      schema: WatchFooterResponseSchema,
      open: async function* () {
        for (const frame of frames) yield await Promise.resolve(frame);
      },
      onPush,
      onEnd,
      backoff: BACKOFF,
    });
  }

  it("stops drawing the frames still queued behind the cancel", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[]]);
    const seen: WatchFooterResponse[] = [];
    let handle: { cancel(): void } | undefined = undefined;
    // ACT
    handle = openLocal(contextFor(client, sink), [push(), push()], (r) => {
      seen.push(r);
      handle?.cancel();
    });
    await settle();
    // ASSERT
    expect(seen).toHaveLength(1);
  });

  it("reports the run as CANCELLED, not as a producer that ended on its own", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[]]);
    const ends: StreamEnd[] = [];
    let handle: { cancel(): void } | undefined = undefined;
    // ACT
    handle = openLocal(
      contextFor(client, sink),
      [push(), push()],
      () => handle?.cancel(),
      (end) => ends.push(end),
    );
    await settle();
    // ASSERT
    expect(ends.map((e) => e.kind)).toEqual(["cancelled"]);
  });

  it("files no unreachable card for a run its own cancel ended", async () => {
    // ARRANGE
    const sink = new RecordingSink();
    const { client } = scriptedClient([[]]);
    let handle: { cancel(): void } | undefined = undefined;
    // ACT
    handle = openLocal(contextFor(client, sink), [push(), push()], () => handle?.cancel());
    await settle();
    // ASSERT
    expect(sink.reported).toEqual([]);
  });
});

describe("watchStream: quiescing a stream that was already cancelled", () => {
  it("says nothing about the move, the stream having already stopped", async () => {
    // ARRANGE
    const lines: Array<[string, string]> = [];
    setLogger(new ForwardingLogger(async () => "accepted", (level, line) => lines.push([level, line])));
    const sink = new RecordingSink();
    const { client } = scriptedClient([[push()]]);
    const ctx = contextFor(client, sink);
    const handle = open(ctx, () => {});
    await settle();
    handle.cancel();
    lines.length = 0;
    // ACT
    ctx.quiesce();
    await settle();
    // ASSERT
    expect(lines.some(([, line]) => line.includes("rpc.stream-quiesced"))).toBe(false);
  });
});

describe("watchStream and the client's link verdict", () => {
  afterEach(() => {
    clearClientFailures();
  });

  it("reports a stream that concluded on its own, naming the stream and the ending", async () => {
    const { client } = scriptedClient([[]]);
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    expect(standingClientFailure()).toEqual({
      kind: "stream_ended",
      substatus: "daemon unreachable",
      activity: "WatchFooter stream ended (producer_ended)",
    });
  });

  it("names a thrown ending as the transport failure it was", async () => {
    const { client } = scriptedClient([new ConnectError("no route", Code.Unavailable)]);
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    expect(standingClientFailure()?.activity).toBe(
      "WatchFooter stream ended (transport_failure)",
    );
  });

  it("clears the verdict when the stream reads again", async () => {
    // The reopened run pushes and then ENDS, which reports afresh -- so the
    // clear is asserted on what was PUBLISHED, not on what stands at the end.
    const published: Array<string | null> = [];
    const stop = onClientVerdict((verdict) => published.push(verdict?.kind ?? null));
    const { client } = scriptedClient([[], [push()]]);
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    await advance(250);
    handle.cancel();
    stop();
    expect(published).toEqual([null, "stream_ended", null, "stream_ended"]);
  });

  it("clears an undecodable-frame verdict as soon as a frame decodes", async () => {
    // Arrange: the run's first frame cannot be read, the second can.
    reportClientFailure("frame_undecodable_card", "a frame could not be read");
    const { client } = scriptedClient([[undecodablePush(), push()]]);
    // Act
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    // Assert: the run ends after the good frame, so what stands is the
    // ENDING's verdict -- the undecodable one was dropped by the decode.
    expect(standingClientFailure()?.kind).toBe("stream_ended");
  });

  it("leaves a VERB's verdict standing when a frame decodes, which does not disprove it", async () => {
    // A STANDING stream: it pushes and never ends, so nothing but the decode
    // could have cleared the verdict.
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    const { client } = standingClient();
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    expect(standingClientFailure()?.kind).toBe("unary_transport");
  });

  it("leaves a verdict standing while the stream is still down", async () => {
    reportClientFailure("unary_transport", "SubmitPrompt: unavailable");
    const { client } = scriptedClient([[]]);
    const handle = open(contextFor(client, new RecordingSink()), () => {});
    await settle();
    handle.cancel();
    expect(standingClientFailure()).not.toBeNull();
  });
});
