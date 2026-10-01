// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { SelectFeedRowResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_feed_row_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { selectFeedRow } from "../../src/feed/select-feed-row.js";
import { isMalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { feedId, harness, type FeedScript } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

const WHAT = "do the move";

/** A scripted daemon and the chip's filings. */
function scripted(script: FeedScript = {}) {
  const h = harness(script);
  const reported: FailureKind[] = [];
  vi.spyOn(h.sink, "report").mockImplementation((kind) => {
    reported.push(kind);
  });
  return { h, reported };
}

const refused = (): FeedScript => ({
  selectFeedRow: () =>
    create(SelectFeedRowResponseSchema, {
      result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
    }),
});

const unreachable = (): FeedScript => ({
  selectFeedRow: () => {
    throw new ConnectError("connection refused", Code.Unavailable);
  },
});

describe("selectFeedRow", () => {
  it("sends the move for this page's workspace", async () => {
    // Arrange
    const { h } = scripted();
    // Act
    await selectFeedRow(h.ctx, { case: "leftView", value: { row: feedId("r1") } }, WHAT);
    // Assert
    const sent = h.calls.selectFeedRow[0];
    expect([sent?.workspace?.id, sent?.move.case]).toEqual(["ws-1", "leftView"]);
  });

  it("files nothing when the daemon applies the move", async () => {
    // Arrange
    const { h, reported } = scripted();
    // Act
    await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT);
    // Assert
    expect(reported).toEqual([]);
  });

  it("logs the applied move's outcome at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { h } = scripted();
    // Act
    await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT);
    // Assert
    const record = await forwardedRecord(capture, "feed.select-feed-row-answered");
    expect([record.context?.move, record.context?.outcome]).toEqual(["clear", "none"]);
  });

  it("files a refusal on the warning chip under the caller's request", async () => {
    // Arrange
    const { h, reported } = scripted(refused());
    // Act
    await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT);
    // Assert
    const kind = reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.what : kind?.case).toBe(WHAT);
  });

  it("logs a refusal at error with its arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { h } = scripted(refused());
    // Act
    await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT);
    // Assert
    const record = await forwardedRecord(capture, "feed.select-feed-row-refused");
    expect([record.level.case, record.context?.arm]).toEqual(["error", "notYetAdopted"]);
  });

  it("files a transport failure on the warning chip", async () => {
    // Arrange
    const { h, reported } = scripted(unreachable());
    // Act
    await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT);
    // Assert
    const kind = reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.cause : kind?.case).toBe(
      "the daemon could not be reached",
    );
  });

  it("logs a transport failure at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { h } = scripted(unreachable());
    // Act
    await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT);
    // Assert
    const record = await forwardedRecord(capture, "rpc.unary-transport-failure");
    expect([record.level.case, record.context?.rpc]).toEqual(["error", "SelectFeedRow"]);
  });

  it("refuses an error with no cause as a malformed view", async () => {
    // Arrange
    const { h } = scripted({
      selectFeedRow: () => create(SelectFeedRowResponseSchema, { result: { case: "error", value: {} } }),
    });
    // Act
    const outcome = await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT).catch((e: unknown) => e);
    // Assert
    expect(isMalformedView(outcome)).toBe(true);
  });

  it("refuses a success with no outcome as a malformed view", async () => {
    // Arrange
    const { h } = scripted({
      selectFeedRow: () =>
        create(SelectFeedRowResponseSchema, { result: { case: "success", value: {} } }),
    });
    // Act
    const outcome = await selectFeedRow(h.ctx, { case: "clear", value: {} }, WHAT).catch((e: unknown) => e);
    // Assert
    expect(isMalformedView(outcome)).toBe(true);
  });
});
