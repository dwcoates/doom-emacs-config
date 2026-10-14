import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { testAppContext } from "./app-context.js";
import { createAppContext, createDaemonContext } from "../../src/rpc/context.js";
import { directPageStreams } from "./direct-page-streams.js";
import { fakePageSeen } from "../page-seen-fake.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };
const client = () => createAgentReplClient(createRouterTransport(() => {}));

function ctx(composerEnabled = false) {
  return testAppContext({
    client: client(),
    workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
    ticker: createTicker(1000),
    failures: SINK,
    composerEnabled,
  });
}

describe("createAppContext", () => {
  it("carries the workspace every request is addressed to", () => {
    expect(ctx().workspace.id).toBe("ws-1");
  });

  it("carries the composer flag off the page address", () => {
    expect(ctx(true).composerEnabled).toBe(true);
  });

  it("runs composer-less by default, as production does", () => {
    expect(ctx().composerEnabled).toBe(false);
  });
});

describe("quiesce", () => {
  it("is the ONE thing that changes what a page may send; the client itself never moves", () => {
    // The no-redial ruling: a successor daemon on another loopback port is a
    // different origin, so there is no client to swap to and no verb for it.
    const context = ctx();
    expect("replaceClient" in context).toBe(false);
  });

  it("reports the page as quiet", () => {
    const context = ctx();
    context.quiesce();
    expect(context.isQuiesced()).toBe(true);
  });

  it("starts talkative, since a fresh page has a daemon to talk to", () => {
    expect(ctx().isQuiesced()).toBe(false);
  });

  it("notifies every quiet subscriber, which is how streams cancel themselves", () => {
    const context = ctx();
    const fn = vi.fn();
    context.onQuiesced(fn);
    context.quiesce();
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("is idempotent, so a second transferred push renotifies nobody", () => {
    const context = ctx();
    const fn = vi.fn();
    context.onQuiesced(fn);
    context.quiesce();
    context.quiesce();
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("tells a subscriber that arrives after the fact at once", () => {
    const context = ctx();
    context.quiesce();
    const fn = vi.fn();
    context.onQuiesced(fn);
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("returns an unsubscriber that stops the notification", () => {
    const context = ctx();
    const fn = vi.fn();
    context.onQuiesced(fn)();
    context.quiesce();
    expect(fn).not.toHaveBeenCalled();
  });

  it("stops the page's stream-cost meter, once however often it is called", () => {
    // Arrange
    const dispose = vi.fn();
    const context = testAppContext({
      client: client(),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      composerEnabled: false,
      costs: { record: () => {}, flush: () => {}, dispose },
    });

    // Act
    context.quiesce();
    context.quiesce();

    // Assert
    expect(dispose).toHaveBeenCalledTimes(1);
  });

  it("quiets a page given no meter", () => {
    // Arrange / Act / Assert
    expect(() => ctx().quiesce()).not.toThrow();
  });
});

describe("notePush", () => {
  it("tells a subscriber that a frame arrived", () => {
    // Arrange
    const context = ctx();
    const fn = vi.fn();
    context.onPush(fn);
    // Act
    context.notePush();
    // Assert
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("tells every subscriber", () => {
    // Arrange
    const context = ctx();
    const first = vi.fn();
    const second = vi.fn();
    context.onPush(first);
    context.onPush(second);
    // Act
    context.notePush();
    // Assert
    expect(second).toHaveBeenCalledTimes(1);
  });

  it("tells one that unsubscribed nothing", () => {
    // Arrange
    const context = ctx();
    const fn = vi.fn();
    context.onPush(fn)();
    // Act
    context.notePush();
    // Assert
    expect(fn).not.toHaveBeenCalled();
  });

  it("tells a late subscriber nothing about frames already read", () => {
    // Arrange: unlike `onQuiesced`, a push is an EVENT and not a state, so
    // there is nothing for a subscriber arriving after one to be told.
    const context = ctx();
    context.notePush();
    const fn = vi.fn();
    // Act
    context.onPush(fn);
    // Assert
    expect(fn).not.toHaveBeenCalled();
  });
});

describe("noteLinkRestored", () => {
  it("tells a subscriber that the link came back", () => {
    // Arrange
    const context = ctx();
    const fn = vi.fn();
    context.onLinkRestored(fn);
    // Act
    context.noteLinkRestored();
    // Assert
    expect(fn).toHaveBeenCalledTimes(1);
  });

  it("tells one that unsubscribed nothing", () => {
    // Arrange
    const context = ctx();
    const fn = vi.fn();
    context.onLinkRestored(fn)();
    // Act
    context.noteLinkRestored();
    // Assert
    expect(fn).not.toHaveBeenCalled();
  });

  it("is not told by an ordinary frame", () => {
    // Arrange
    const context = ctx();
    const fn = vi.fn();
    context.onLinkRestored(fn);
    // Act
    context.notePush();
    // Assert
    expect(fn).not.toHaveBeenCalled();
  });
});

describe("onQuiesced after the page has already gone quiet", () => {
  it("runs a late subscriber AT ONCE, so a stream opened in the window is not stranded", () => {
    // ARRANGE
    const c = ctx();
    c.quiesce();
    const told = vi.fn();
    // ACT
    c.onQuiesced(told);
    // ASSERT
    expect(told).toHaveBeenCalledTimes(1);
  });

  it("hands a late subscriber an unsubscriber that is inert, never a second call", () => {
    // ARRANGE
    const c = ctx();
    c.quiesce();
    const told = vi.fn();
    // ACT
    const unsubscribe = c.onQuiesced(told);
    unsubscribe();
    c.quiesce();
    // ASSERT
    expect(told).toHaveBeenCalledTimes(1);
  });
});

describe("noteStreamCost", () => {
  it("records a frame's cost in the page's meter", () => {
    // Arrange
    const recorded: Array<[string, number]> = [];
    const c = testAppContext({
      client: client(),
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      ticker: createTicker(1000),
      failures: SINK,
      composerEnabled: false,
      costs: { record: (stream, ms) => recorded.push([stream, ms]), flush: () => {}, dispose: () => {} },
    });

    // Act
    c.noteStreamCost("WatchFooter", 3);

    // Assert
    expect(recorded).toEqual([["WatchFooter", 3]]);
  });

  it("records nothing for a page given no meter", () => {
    // Arrange / Act / Assert
    expect(() => ctx().noteStreamCost("WatchFooter", 3)).not.toThrow();
  });
});

describe("createDaemonContext", () => {
  it("talks to the daemon with no workspace in it", () => {
    // Arrange
    const c = client();

    // Act
    const daemon = createDaemonContext({
      client: c,
      ticker: createTicker(1000),
      failures: SINK,
      page: "webhost-page",
      streams: directPageStreams(c),
    });

    // Assert
    expect(["workspace" in daemon, daemon.client === c]).toEqual([false, true]);
  });

  it("goes quiet like a page's context", () => {
    // Arrange
    const c = client();
    const daemon = createDaemonContext({
      client: c,
      ticker: createTicker(1000),
      failures: SINK,
      page: "webhost-page",
      streams: directPageStreams(c),
    });

    // Act
    daemon.quiesce();

    // Assert
    expect(daemon.isQuiesced()).toBe(true);
  });

  it("is what a page's context is built on, with the page's workspace added", () => {
    // Arrange
    const c = client();
    const workspace = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });

    // Act
    const app = createAppContext({
      client: c,
      ticker: createTicker(1000),
      failures: SINK,
      page: "page",
      streams: directPageStreams(c),
      workspace,
      seen: fakePageSeen(),
      composerEnabled: false,
    });
    app.quiesce();

    // Assert
    expect([app.workspace.id, app.isQuiesced()]).toEqual(["ws-1", true]);
  });
});
