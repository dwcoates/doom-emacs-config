import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError, createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import {
  OpenExternalResponseSchema,
  type OpenExternalResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { ForwardingLogger, setLogger } from "../../src/log.js";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { QUIESCED_MESSAGE, callUnary, type UnaryContext } from "../../src/rpc/unary.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const UNKNOWN = [{ no: 999, wireType: 0, data: new Uint8Array([1]) }];

/** A context whose client answers OpenExternal with whatever ANSWER builds. */
function ctxAnswering(answer: () => OpenExternalResponse): UnaryContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, { openExternal: () => answer() });
  });
  return { client: createAgentReplClient(transport), isQuiesced: () => false };
}

/** A context whose client throws THROWN from OpenExternal. */
function ctxThrowing(thrown: unknown): UnaryContext {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openExternal: () => {
        throw thrown;
      },
    });
  });
  return { client: createAgentReplClient(transport), isQuiesced: () => false };
}

const call = (ctx: UnaryContext): Promise<OpenExternalResponse> =>
  callUnary(
    ctx,
    "OpenExternal",
    (client) => client.openExternal({ workspace: WORKSPACE, url: "https://example.test" }),
    OpenExternalResponseSchema,
  );

/** Capture the records the canonical logger emits during ACT. */
function captureLog(): Array<[string, string]> {
  const lines: Array<[string, string]> = [];
  setLogger(
    new ForwardingLogger(
      async () => "accepted",
      (level, line) => lines.push([level, line]),
      {},
      "debug",
    ),
  );
  return lines;
}

describe("callUnary", () => {
  it("returns the response on a success arm", async () => {
    const ctx = ctxAnswering(() =>
      create(OpenExternalResponseSchema, { result: { case: "success", value: {} } }),
    );
    const response = await call(ctx);
    expect(response.result.case).toBe("success");
  });

  it("RETURNS an error arm rather than throwing, because a refusal is an answer", async () => {
    const ctx = ctxAnswering(() =>
      create(OpenExternalResponseSchema, { result: { case: "error", value: {} } }),
    );
    const response = await call(ctx);
    expect(response.result.case).toBe("error");
  });

  it("refuses a response whose result oneof is unset", async () => {
    const ctx = ctxAnswering(() => create(OpenExternalResponseSchema, {}));
    await expect(call(ctx)).rejects.toBeInstanceOf(MalformedView);
  });

  it("names the unset oneof's path in the refusal", async () => {
    const ctx = ctxAnswering(() => create(OpenExternalResponseSchema, {}));
    await expect(call(ctx)).rejects.toMatchObject({ path: "OpenExternalResponse.result" });
  });

  it("refuses a response carrying a field this build has no descriptor for", async () => {
    const ctx = ctxAnswering(() => {
      const response = create(OpenExternalResponseSchema, {
        result: { case: "success", value: {} },
      });
      response.$unknown = UNKNOWN;
      return response;
    });
    await expect(call(ctx)).rejects.toBeInstanceOf(MalformedView);
  });

  it("rethrows a transport failure as a ConnectError", async () => {
    const ctx = ctxThrowing(new ConnectError("the daemon is down", Code.Unavailable));
    await expect(call(ctx)).rejects.toBeInstanceOf(ConnectError);
  });

  it("converts a plain throw into a ConnectError, so callers catch one type", async () => {
    const ctx = ctxThrowing(new Error("socket hang up"));
    await expect(call(ctx)).rejects.toBeInstanceOf(ConnectError);
  });

  it("logs the call at debug before issuing it", async () => {
    const lines = captureLog();
    const ctx = ctxAnswering(() =>
      create(OpenExternalResponseSchema, { result: { case: "success", value: {} } }),
    );
    await call(ctx);
    expect(lines.some(([, line]) => line.includes("rpc.unary-call"))).toBe(true);
  });

  it("logs the outcome ARM, not merely that the call returned", async () => {
    const lines = captureLog();
    const ctx = ctxAnswering(() =>
      create(OpenExternalResponseSchema, { result: { case: "error", value: {} } }),
    );
    await call(ctx);
    const answered = lines.find(([, line]) => line.includes("rpc.unary-answered"));
    expect(answered?.[1]).toContain('"outcome":"error"');
  });

  it("logs a transport failure at error, exactly once, by this layer", async () => {
    const lines = captureLog();
    const ctx = ctxThrowing(new ConnectError("gone", Code.Unavailable));
    await expect(call(ctx)).rejects.toBeInstanceOf(ConnectError);
    const errors = lines.filter(([level, line]) => level === "error" && line.includes("rpc.unary-transport-failure"));
    expect(errors).toHaveLength(1);
  });

  it("does not log an outcome for a call that never answered", async () => {
    const lines = captureLog();
    const ctx = ctxThrowing(new ConnectError("gone", Code.Unavailable));
    await expect(call(ctx)).rejects.toBeInstanceOf(ConnectError);
    expect(lines.some(([, line]) => line.includes("rpc.unary-answered"))).toBe(false);
  });
});

describe("the quiet window", () => {
  it("refuses the call locally rather than sending it", async () => {
    // ARRANGE
    let sent = 0;
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        openExternal: () => {
          sent += 1;
          return create(OpenExternalResponseSchema, { result: { case: "success", value: {} } });
        },
      });
    });
    const ctx: UnaryContext = {
      client: createAgentReplClient(transport),
      isQuiesced: () => true,
    };
    // ACT
    await expect(call(ctx)).rejects.toThrow(QUIESCED_MESSAGE);
    // ASSERT
    expect(sent).toBe(0);
  });

  it("refuses with Unavailable, so a call site draws its ordinary refusal", async () => {
    // ARRANGE
    const ctx: UnaryContext = { ...ctxAnswering(() => create(OpenExternalResponseSchema, {})), isQuiesced: () => true };
    // ACT
    const err = await call(ctx).catch((e: unknown) => e);
    // ASSERT
    expect(ConnectError.from(err).code).toBe(Code.Unavailable);
  });

  it("sends normally once the page is no longer quiet", async () => {
    // ARRANGE
    let quiet = true;
    const base = ctxAnswering(() =>
      create(OpenExternalResponseSchema, { result: { case: "success", value: {} } }),
    );
    const ctx: UnaryContext = { client: base.client, isQuiesced: () => quiet };
    // ACT
    quiet = false;
    const response = await call(ctx);
    // ASSERT
    expect(response.result.case).toBe("success");
  });
});

describe("the result oneof lookup", () => {
  it("logs a schema that has NO result oneof as answered, rather than refusing it", async () => {
    // ARRANGE: WorkspaceRef carries no `result` oneof, as the login duplex
    // stream's messages do not, so there is no outcome arm to name.
    const lines = captureLog();
    const ctx: UnaryContext = {
      client: createAgentReplClient(createRouterTransport(() => {})),
      isQuiesced: () => false,
    };
    // ACT
    await callUnary(ctx, "Echo", async () => WORKSPACE, WorkspaceRefSchema);
    // ASSERT
    const answered = lines.find(([, line]) => line.includes("rpc.unary-answered"));
    expect(answered?.[1]).toContain('"outcome":"answered"');
  });
});
