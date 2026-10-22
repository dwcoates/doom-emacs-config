// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { Code, ConnectError } from "@connectrpc/connect";
import { OpenFeedImageResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_image_pb";
import { CloseFeedImageResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_feed_image_pb";
import type { FailureKind } from "../../../proto/gen/ts/frontend/v1/failure_pb";
import { FeedImageBlockSchema } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { closeFeedImage, installImageOpen, openDrawnImage } from "../../src/feed/feed-image.js";
import { drawFeedImageBlock } from "../../src/feed/rows/blocks.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { harness, type FeedScript } from "./harness.js";

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

/** A scroll box holding one image the feed drew, and a click target beside it. */
function drawnBox(): { box: HTMLElement; image: HTMLElement; words: HTMLElement } {
  const box = document.createElement("div");
  const image = drawFeedImageBlock(create(FeedImageBlockSchema, { src: "/feed-images/abc", alt: "clip.png" }));
  const words = document.createElement("p");
  box.append(image, words);
  document.body.replaceChildren(box);
  return { box, image, words };
}

describe("openDrawnImage", () => {
  it("asks the daemon to open the drawn image, echoed as served, for this page's workspace", async () => {
    // Arrange
    const { h } = scripted();
    // Act
    await openDrawnImage(h.ctx, "/feed-images/abc", "clip.png");
    // Assert
    const sent = h.calls.openFeedImage[0];
    expect([sent?.workspace?.id, sent?.image.case, sent?.image.case === "drawn" ? sent.image.value.image?.src : ""]).toEqual([
      "ws-1",
      "drawn",
      "/feed-images/abc",
    ]);
  });

  it("files nothing when the daemon opens it", async () => {
    // Arrange
    const { h, reported } = scripted();
    // Act
    await openDrawnImage(h.ctx, "/feed-images/abc", "clip.png");
    // Assert
    expect(reported).toEqual([]);
  });

  it("files a refusal on the warning chip and logs it at error", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { h, reported } = scripted({
      openFeedImage: () =>
        create(OpenFeedImageResponseSchema, {
          result: { case: "error", value: { cause: { case: "notDrawn", value: { src: "/x" } } } },
        }),
    });
    // Act
    await openDrawnImage(h.ctx, "/x", "");
    // Assert
    const record = await forwardedRecord(capture, "feed.open-image-refused");
    expect([reported.length, record.level.case, record.context?.arm]).toEqual([1, "error", "notDrawn"]);
  });

  it("files a transport failure on the warning chip", async () => {
    // Arrange
    const { h, reported } = scripted({
      openFeedImage: () => {
        throw new ConnectError("connection refused", Code.Unavailable);
      },
    });
    // Act
    await openDrawnImage(h.ctx, "/feed-images/abc", "clip.png");
    // Assert
    expect(reported).toHaveLength(1);
  });
});

describe("closeFeedImage", () => {
  it("asks the daemon to close the overlay for this page's workspace", async () => {
    // Arrange
    const { h, reported } = scripted();
    // Act
    await closeFeedImage(h.ctx);
    // Assert
    expect([h.calls.closeFeedImage[0]?.workspace?.id, reported]).toEqual(["ws-1", []]);
  });

  it("files a refusal on the warning chip", async () => {
    // Arrange
    const { h, reported } = scripted({
      closeFeedImage: () =>
        create(CloseFeedImageResponseSchema, {
          result: { case: "error", value: { cause: { case: "notYetAdopted", value: {} } } },
        }),
    });
    // Act
    await closeFeedImage(h.ctx);
    // Assert
    expect(reported).toHaveLength(1);
  });

  it("files a transport failure on the warning chip", async () => {
    // Arrange
    const { h, reported } = scripted({
      closeFeedImage: () => {
        throw new ConnectError("connection refused", Code.Unavailable);
      },
    });
    // Act
    await closeFeedImage(h.ctx);
    // Assert
    expect(reported).toHaveLength(1);
  });
});

describe("installImageOpen", () => {
  it("opens a clicked image, by the source it was served", async () => {
    // Arrange
    const { h } = scripted();
    const { box, image } = drawnBox();
    const uninstall = installImageOpen(box, h.ctx);
    // Act
    image.click();
    await vi.runAllTimersAsync();
    // Assert
    const sent = h.calls.openFeedImage[0];
    expect(sent?.image.case === "drawn" ? [sent.image.value.image?.src, sent.image.value.image?.alt] : []).toEqual([
      "/feed-images/abc",
      "clip.png",
    ]);
    uninstall();
  });

  it("stops the click there, so the bubble the image sits in does not take it", () => {
    // Arrange
    const { h } = scripted();
    const { box, image } = drawnBox();
    const bubble = vi.fn();
    box.addEventListener("click", bubble);
    const uninstall = installImageOpen(box, h.ctx);
    // Act
    image.click();
    // Assert
    expect(bubble).not.toHaveBeenCalled();
    uninstall();
  });

  it("leaves a click on anything but an image alone", () => {
    // Arrange
    const { h } = scripted();
    const { box, words } = drawnBox();
    const uninstall = installImageOpen(box, h.ctx);
    // Act
    words.click();
    // Assert
    expect(h.calls.openFeedImage).toHaveLength(0);
    uninstall();
  });

  it("opens nothing once uninstalled", () => {
    // Arrange
    const { h } = scripted();
    const { box, image } = drawnBox();
    installImageOpen(box, h.ctx)();
    // Act
    image.click();
    // Assert
    expect(h.calls.openFeedImage).toHaveLength(0);
  });
});
