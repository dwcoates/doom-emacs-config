// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { FoldMergeBubbleResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_fold_merge_bubble_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import {
  CLOSE_MERGE_BUBBLE_REQUEST,
  foldMergeBubble,
  OPEN_MERGE_BUBBLE_REQUEST,
} from "../../src/feed/fold-merge-bubble.js";
import { isMalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { feedId, harness, type FeedScript } from "./harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

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
  foldMergeBubble: (req) =>
    create(FoldMergeBubbleResponseSchema, {
      result: { case: "error", value: { cause: { case: "notAMergeBubble", value: { row: req.row } } } },
    }),
});

const unreachable = (): FeedScript => ({
  foldMergeBubble: () => {
    throw new ConnectError("connection refused", Code.Unavailable);
  },
});

/** The `what` of the one controlPlaneFailed filing, or the arm filed instead. */
function filedWhat(reported: FailureKind[]): string | undefined {
  const kind = reported[0]?.kind;
  return kind?.case === "controlPlaneFailed" ? kind.value.what : kind?.case;
}

describe("foldMergeBubble", () => {
  it("sends the head row for this page's workspace", async () => {
    // Arrange
    const { h } = scripted();
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), false);
    // Assert
    const sent = h.calls.foldMergeBubble[0];
    expect([sent?.workspace?.id, sent?.row?.value]).toEqual(["ws-1", "m1"]);
  });

  it("sends the open arm for a reader's open", async () => {
    // Arrange
    const { h } = scripted();
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), false);
    // Assert
    expect(h.calls.foldMergeBubble[0]?.fold.case).toBe("open");
  });

  it("sends the close arm for a reader's close", async () => {
    // Arrange
    const { h } = scripted();
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    expect(h.calls.foldMergeBubble[0]?.fold.case).toBe("close");
  });

  it("files nothing when the daemon records the fold", async () => {
    // Arrange
    const { h, reported } = scripted();
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    expect(reported).toEqual([]);
  });

  it("logs the fold it sends at info", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { h } = scripted();
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    const record = await forwardedRecord(capture, "feed.merge-fold-sent");
    expect([record.level.case, record.context?.row, record.context?.folded]).toEqual(["info", "m1", true]);
  });

  it("files a refused open on the warning chip under the open request", async () => {
    // Arrange
    const { h, reported } = scripted(refused());
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), false);
    // Assert
    expect(filedWhat(reported)).toBe(OPEN_MERGE_BUBBLE_REQUEST);
  });

  it("files a refused close on the warning chip under the close request", async () => {
    // Arrange
    const { h, reported } = scripted(refused());
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    expect(filedWhat(reported)).toBe(CLOSE_MERGE_BUBBLE_REQUEST);
  });

  it("logs a refusal at error with its arm", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { h } = scripted(refused());
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    const record = await forwardedRecord(capture, "feed.merge-fold-refused");
    expect([record.level.case, record.context?.arm]).toEqual(["error", "notAMergeBubble"]);
  });

  it("words a cross-cutting refusal through the shared sentences", async () => {
    // Arrange
    const { h, reported } = scripted({
      foldMergeBubble: () =>
        create(FoldMergeBubbleResponseSchema, {
          result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
        }),
    });
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    const kind = reported[0]?.kind;
    expect(kind?.case === "controlPlaneFailed" ? kind.value.cause : kind?.case).toBe(
      "the daemon has not finished adopting this workspace yet",
    );
  });

  it("files a transport failure on the warning chip", async () => {
    // Arrange
    const { h, reported } = scripted(unreachable());
    // Act
    await foldMergeBubble(h.ctx, feedId("m1"), true);
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
    await foldMergeBubble(h.ctx, feedId("m1"), true);
    // Assert
    const record = await forwardedRecord(capture, "rpc.unary-transport-failure");
    expect([record.level.case, record.context?.rpc]).toEqual(["error", "FoldMergeBubble"]);
  });

  it("refuses an error with no cause as a malformed view", async () => {
    // Arrange
    const { h } = scripted({
      foldMergeBubble: () => create(FoldMergeBubbleResponseSchema, { result: { case: "error", value: {} } }),
    });
    // Act
    const outcome = await foldMergeBubble(h.ctx, feedId("m1"), true).catch((e: unknown) => e);
    // Assert
    expect(isMalformedView(outcome)).toBe(true);
  });

  it("refuses an answer with no result as a malformed view", async () => {
    // Arrange
    const { h } = scripted({ foldMergeBubble: () => create(FoldMergeBubbleResponseSchema, {}) });
    // Act
    const outcome = await foldMergeBubble(h.ctx, feedId("m1"), true).catch((e: unknown) => e);
    // Assert
    expect(isMalformedView(outcome)).toBe(true);
  });
});
