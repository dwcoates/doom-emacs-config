// The webapp logger's sink: one ClientLog call per record, filed under the
// source the caller names. See src/rpc/client-log-sink.ts.
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, it } from "vitest";

import { ClientLogRecordSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_client_log_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { clearClientFailures, standingClientFailure } from "../../src/rpc/link.js";
import { clientLogSink } from "../../src/rpc/client-log-sink.js";
import { clientLogRecorder } from "./client-log-recorder.js";
import { beginPage, endPage } from "../../src/page-teardown.js";
import { settledByNextTask } from "../settled.js";

afterEach(() => {
  clearClientFailures();
  beginPage();
});

const WORKSPACE = create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" });
const RECORD = create(ClientLogRecordSchema, {
  level: { case: "info", value: {} },
  operation: "test.op",
  message: "m",
});

/** A client answering ANSWER: success, the unknown-workspace refusal, a refusal naming no cause, or a throw. */
function clientAnswering(answer: "success" | "unknownWorkspace" | "refused" | "throws") {
  if (answer === "throws") return clientLogRecorder(new Error("the sink is down"));
  if (answer === "success") return clientLogRecorder();
  // A refusal whose cause this build does not name arrives with the cause unset.
  return clientLogRecorder({
    result: {
      case: "error",
      value: answer === "unknownWorkspace" ? { cause: { case: "unknownWorkspace", value: {} } } : {},
    },
  });
}

describe("clientLogSink", () => {
  it("files a workspace page's record under its workspace", async () => {
    // Arrange
    const { client, logged: requests } = clientAnswering("success");
    const sink = clientLogSink(() => client, { case: "workspace", value: WORKSPACE });

    // Act
    await sink(RECORD);

    // Assert
    expect([requests[0]?.source.case, requests[0]?.source.value]).toEqual(["workspace", WORKSPACE]);
  });

  it("files the webhost's record as the webhost's", async () => {
    // Arrange
    const { client, logged: requests } = clientAnswering("success");
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

  it("never settles a record the page's end cut off, and tells the footer nothing", async () => {
    // Arrange
    const { client } = clientAnswering("throws");
    const sink = clientLogSink(() => client, { case: "webhost", value: {} });
    endPage();
    // Act
    const settled = await settledByNextTask(sink(RECORD));
    // Assert
    expect([settled, standingClientFailure()]).toEqual([false, null]);
  });
});
