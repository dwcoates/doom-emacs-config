// @vitest-environment jsdom
/**
 * control-call — one control call a gesture makes, its failures on the chip.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import {
  SelectFeedRowRequestSchema,
  SelectFeedRowResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { controlCall } from "../../src/rpc/control-call.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { scriptedAppContext } from "./app-context.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { codeUnder } from "../files-under.js";
import path from "node:path";
import { fileURLToPath } from "node:url";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src");

/** A page whose daemon answers SelectFeedRow through ANSWER. */
function page(answer: () => unknown): { ctx: ReturnType<typeof scriptedAppContext>; reported: FailureKind[] } {
  const reported: FailureKind[] = [];
  const ctx = scriptedAppContext(
    { selectFeedRow: () => answer() as never },
    {
      workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w" }),
      failures: { report: (kind) => reported.push(kind), retract: () => {} },
    },
  );
  return { ctx, reported };
}

/** One SelectFeedRow clear, made through controlCall. */
function clear(ctx: ReturnType<typeof scriptedAppContext>) {
  return controlCall(ctx, {
    rpc: "SelectFeedRow",
    what: "clear the selection",
    send: (client) =>
      client.selectFeedRow(create(SelectFeedRowRequestSchema, { workspace: ctx.workspace, move: { case: "clear", value: {} } })),
    schema: SelectFeedRowResponseSchema,
    result: (response) => response.result,
    refusals: { notSelectable: () => "that bubble cannot be selected" },
    refusedOperation: "test.control-call-refused",
    context: { move: "clear" },
  });
}

describe("controlCall", () => {
  it("answers the success arm's value", async () => {
    // Arrange
    const { ctx } = page(() =>
      create(SelectFeedRowResponseSchema, { result: { case: "success", value: { outcome: { case: "none", value: {} } } } }),
    );
    // Act
    const success = await clear(ctx);
    // Assert
    expect(success?.outcome.case).toBe("none");
  });

  it("files nothing on success", async () => {
    // Arrange
    const { ctx, reported } = page(() =>
      create(SelectFeedRowResponseSchema, { result: { case: "success", value: { outcome: { case: "none", value: {} } } } }),
    );
    // Act
    await clear(ctx);
    // Assert
    expect(reported).toEqual([]);
  });

  it("files a refusal on the chip and answers nothing", async () => {
    // Arrange
    const { ctx, reported } = page(() =>
      create(SelectFeedRowResponseSchema, { result: { case: "error", value: { cause: { case: "notSelectable", value: {} } } } }),
    );
    // Act
    const success = await clear(ctx);
    // Assert
    expect([success, reported.length]).toEqual([undefined, 1]);
  });

  it("records a refusal at error with the call's context and the arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { ctx } = page(() =>
      create(SelectFeedRowResponseSchema, { result: { case: "error", value: { cause: { case: "notSelectable", value: {} } } } }),
    );
    // Act
    await clear(ctx);
    // Assert
    const record = await forwardedRecord(capture, "test.control-call-refused");
    expect([record.level.case, record.context?.move, record.context?.arm]).toEqual(["error", "clear", "notSelectable"]);
  });

  it("files a daemon it cannot reach on the chip and answers nothing", async () => {
    // Arrange
    const { ctx, reported } = page(() => {
      throw new ConnectError("connection refused", Code.Unavailable);
    });
    // Act
    const success = await clear(ctx);
    // Assert
    expect([success, reported.length]).toEqual([undefined, 1]);
  });

  it("refuses a result that sets no arm as malformed", async () => {
    // Arrange
    const { ctx } = page(() => create(SelectFeedRowResponseSchema, {}));
    // Act, Assert
    await expect(clear(ctx)).rejects.toThrow(MalformedView);
  });
});

describe("the control call's call sites", () => {
  it("leave the transport-failure-to-chip shape to controlCall", () => {
    // Arrange
    const offenders: string[] = [];
    // Act
    for (const [file, code] of codeUnder(SRC)) {
      if (file === path.join("rpc", "control-call.ts")) continue;
      if (/controlPlaneFailed\([^)]*callFailure\(err\)\.text/.test(code)) offenders.push(file);
    }
    // Assert
    expect(offenders).toEqual([]);
  });
});
