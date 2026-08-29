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
import { createAgentReplClient, type AgentReplClient } from "../../src/rpc/client.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { callUnary } from "../../src/rpc/unary.js";

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const UNKNOWN = [{ no: 999, wireType: 0, data: new Uint8Array([1]) }];

/** A context whose client answers OpenExternal with whatever ANSWER builds. */
function ctxAnswering(answer: () => OpenExternalResponse): { client: AgentReplClient } {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, { openExternal: () => answer() });
  });
  return { client: createAgentReplClient(transport) };
}

/** A context whose client throws THROWN from OpenExternal. */
function ctxThrowing(thrown: unknown): { client: AgentReplClient } {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      openExternal: () => {
        throw thrown;
      },
    });
  });
  return { client: createAgentReplClient(transport) };
}

const call = (ctx: { client: AgentReplClient }): Promise<OpenExternalResponse> =>
  callUnary(
    ctx,
    "OpenExternal",
    (client) => client.openExternal({ workspace: WORKSPACE, url: "https://example.test" }),
    OpenExternalResponseSchema,
  );

/** Capture the records the canonical logger emits during ACT. */
function captureLog(): Array<[string, string]> {
  const lines: Array<[string, string]> = [];
  setLogger(new ForwardingLogger(async () => {}, (level, line) => lines.push([level, line])));
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
