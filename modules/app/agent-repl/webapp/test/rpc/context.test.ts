import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { testAppContext } from "./app-context.js";

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
