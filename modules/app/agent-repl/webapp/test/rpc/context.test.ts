import { describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createTicker } from "../../src/clock.js";
import type { FailureSink } from "../../src/failure/sink.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { createAppContext } from "../../src/rpc/context.js";

const SINK: FailureSink = { report: () => {}, retract: () => {} };
const client = () => createAgentReplClient(createRouterTransport(() => {}));

function ctx(composerEnabled = false) {
  return createAppContext({
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

describe("replaceClient", () => {
  it("makes ctx.client the adopted one", () => {
    // ARRANGE
    const context = ctx();
    const next = client();
    // ACT
    context.replaceClient(next);
    // ASSERT
    expect(context.client).toBe(next);
  });

  it("notifies a listener so it can re-derive what it built from the old client", () => {
    const context = ctx();
    const fn = vi.fn();
    context.onClientReplaced(fn);
    context.replaceClient(client());
    expect(fn).toHaveBeenCalledOnce();
  });

  it("notifies every listener, not merely the first", () => {
    const context = ctx();
    const a = vi.fn();
    const b = vi.fn();
    context.onClientReplaced(a);
    context.onClientReplaced(b);
    context.replaceClient(client());
    expect([a.mock.calls.length, b.mock.calls.length]).toEqual([1, 1]);
  });

  it("hands the listener the NEW client, not the one being replaced", () => {
    const context = ctx();
    const next = client();
    let seen: unknown = null;
    context.onClientReplaced(() => {
      seen = context.client;
    });
    context.replaceClient(next);
    expect(seen).toBe(next);
  });

  it("survives a listener that unsubscribes itself mid-notification", () => {
    // ARRANGE: a stream handle does exactly this when it cancels on adoption.
    const context = ctx();
    const b = vi.fn();
    const unsubscribeA = context.onClientReplaced(() => unsubscribeA());
    context.onClientReplaced(b);
    // ACT / ASSERT
    expect(() => context.replaceClient(client())).not.toThrow();
    expect(b).toHaveBeenCalledOnce();
  });
});

describe("onClientReplaced", () => {
  it("returns an unsubscriber that stops later notifications", () => {
    const context = ctx();
    const fn = vi.fn();
    context.onClientReplaced(fn)();
    context.replaceClient(client());
    expect(fn).not.toHaveBeenCalled();
  });

  it("notifies once per replacement, so two adoptions notify twice", () => {
    const context = ctx();
    const fn = vi.fn();
    context.onClientReplaced(fn);
    context.replaceClient(client());
    context.replaceClient(client());
    expect(fn).toHaveBeenCalledTimes(2);
  });
});
