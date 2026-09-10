import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { OpenExternalResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_external_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";

describe("createAgentReplClient", () => {
  it("exposes a verb from the generated service", () => {
    const client = createAgentReplClient(createRouterTransport(() => {}));
    expect(typeof client.openExternal).toBe("function");
  });

  it("exposes a stream from the generated service", () => {
    const client = createAgentReplClient(createRouterTransport(() => {}));
    expect(typeof client.watchFooter).toBe("function");
  });

  it("carries a generated message through a call, which pins ONE protobuf runtime", async () => {
    // ARRANGE: a descriptor identity mismatch between the generated code's
    // @bufbuild/protobuf copy and the connect packages' would fail exactly
    // here, which is what this asserts.
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        openExternal: () =>
          create(OpenExternalResponseSchema, { result: { case: "success", value: {} } }),
      });
    });
    const client = createAgentReplClient(transport);
    // ACT
    const response = await client.openExternal({
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      url: "https://example.test",
    });
    // ASSERT
    expect(response.result.case).toBe("success");
  });

  it("round-trips a nested generated message the router echoed", async () => {
    const transport = createRouterTransport(({ service }) => {
      service(AgentRepl, {
        openExternal: (req) =>
          create(OpenExternalResponseSchema, {
            result: { case: req.url === "https://example.test" ? "success" : "error", value: {} },
          }),
      });
    });
    const client = createAgentReplClient(transport);
    const response = await client.openExternal({
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      url: "ftp://nope",
    });
    expect(response.result.case).toBe("error");
  });
});
