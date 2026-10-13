// The webapp logger's sink: one ClientLog call per record, filed under the
// source the caller names. See src/rpc/client-log-sink.ts.
import { create } from "@bufbuild/protobuf";
import { createRouterTransport } from "@connectrpc/connect";
import { afterEach, describe, expect, it } from "vitest";

import { ClientLogRecordSchema, ClientLogResponseSchema, type ClientLogRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { clearClientFailures, standingClientFailure } from "../../src/rpc/link.js";
import { clientLogSink } from "../../src/rpc/client-log-sink.js";

afterEach(() => {
  clearClientFailures();
});

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const RECORD = create(ClientLogRecordSchema, {
  level: { case: "info", value: {} },
  operation: "test.op",
  message: "m",
});

/** A client whose ClientLog records each request and answers ANSWER, or throws. */
function clientAnswering(answer: "success" | "unknownWorkspace" | "refused" | "throws") {
  const requests: ClientLogRequest[] = [];
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      clientLog(request) {
        requests.push(request);
        if (answer === "throws") throw new Error("the sink is down");
        if (answer === "success") return create(ClientLogResponseSchema, { result: { case: "success", value: {} } });
        // A refusal whose cause this build does not name arrives with the
        // cause unset.
        return create(ClientLogResponseSchema, {
          result: {
            case: "error",
            value: answer === "unknownWorkspace" ? { cause: { case: "unknownWorkspace", value: {} } } : {},
          },
        });
      },
    });
  });
  return { client: createAgentReplClient(transport), requests };
}

describe("clientLogSink", () => {
  it("files a workspace page's record under its workspace", async () => {
    // Arrange
    const { client, requests } = clientAnswering("success");
    const sink = clientLogSink(() => client, { case: "workspace", value: WORKSPACE });

    // Act
    await sink(RECORD);

    // Assert
    expect([requests[0]?.source.case, requests[0]?.source.value]).toEqual(["workspace", WORKSPACE]);
  });

  it("files the webhost's record as the webhost's", async () => {
    // Arrange
    const { client, requests } = clientAnswering("success");
    const sink = clientLogSink(() => client, { case: "webhost", value: {} });

    // Act
    await sink(RECORD);

    // Assert
    expect(requests[0]?.source.case).toBe("webhost");
  });

  it("answers accepted for a filed record", async () => {
    // Arrange
    const { client } = clientAnswering("success");

    // Act
    const outcome = await clientLogSink(() => client, { case: "webhost", value: {} })(RECORD);

    // Assert
    expect(outcome).toBe("accepted");
  });

  it("answers workspace_departed when the daemon no longer knows the workspace", async () => {
    // Arrange
    const { client } = clientAnswering("unknownWorkspace");

    // Act
    const outcome = await clientLogSink(() => client, { case: "workspace", value: WORKSPACE })(RECORD);

    // Assert
    expect(outcome).toBe("workspace_departed");
  });

  it("answers accepted for any other refusal of one record", async () => {
    // Arrange
    const { client } = clientAnswering("refused");

    // Act
    const outcome = await clientLogSink(() => client, { case: "workspace", value: WORKSPACE })(RECORD);

    // Assert
    expect(outcome).toBe("accepted");
  });

  it("reports the client failure and rethrows when the call fails", async () => {
    // Arrange
    const { client } = clientAnswering("throws");
    const sink = clientLogSink(() => client, { case: "webhost", value: {} });

    // Act
    const sent = sink(RECORD);

    // Assert
    await expect(sent).rejects.toThrow();
    expect(standingClientFailure()?.kind).toBe("client_log_failed");
  });
});
