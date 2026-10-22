// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedImageOverlaySchema } from "../../../proto/gen/ts/frontend/v1/feed_image_overlay_pb";
import stylesheet from "../../src/styles.css?raw";
import { IMAGE_MISSING_TEXT } from "../../src/feed/rows/blocks.js";
import { IMAGE_OVERLAY_FRAME_CLASS, mountImageOverlay } from "../../src/image-overlay/image-overlay.js";
import { isMalformedView } from "../../src/rpc/malformed.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { harness } from "../feed/harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

const opened = (src = "/feed-images/abc", alt = "clip.png") =>
  create(FeedImageOverlaySchema, { state: { case: "open", value: { image: { src, alt } } } });
const closed = () => create(FeedImageOverlaySchema, { state: { case: "closed", value: {} } });

/** The overlay mounted over a feed box at a known rectangle. */
function mounted() {
  const h = harness();
  const host = document.createElement("div");
  const feedScroll = document.createElement("div");
  feedScroll.getBoundingClientRect = () => new DOMRect(240, 36, 800, 600);
  document.body.replaceChildren(feedScroll, host);
  const overlay = mountImageOverlay(host, h.ctx, { feedScroll });
  return { h, host, overlay };
}

describe("mountImageOverlay", () => {
  it("is hidden until the daemon opens an image", () => {
    const { host, overlay } = mounted();
    expect(host.hidden).toBe(true);
    overlay.dispose();
  });

  it("draws the open image in its frame, at its source as served", () => {
    // Arrange
    const { host, overlay } = mounted();
    // Act
    overlay.apply(opened());
    // Assert
    const image = host.querySelector(`.${IMAGE_OVERLAY_FRAME_CLASS} img`);
    expect([host.hidden, image?.getAttribute("src"), image?.getAttribute("alt")]).toEqual([false, "/feed-images/abc", "clip.png"]);
    overlay.dispose();
  });

  it("sits over the feed's box", () => {
    // Arrange
    const { host, overlay } = mounted();
    // Act
    overlay.apply(opened());
    // Assert
    expect([host.style.top, host.style.left, host.style.width, host.style.height]).toEqual(["36px", "240px", "800px", "600px"]);
    overlay.dispose();
  });

  it("replaces an open image with the next one opened", () => {
    // Arrange
    const { host, overlay } = mounted();
    overlay.apply(opened("/feed-images/one"));
    // Act
    overlay.apply(opened("/feed-images/two"));
    // Assert
    expect([...host.querySelectorAll("img")].map((img) => img.getAttribute("src"))).toEqual(["/feed-images/two"]);
    overlay.dispose();
  });

  it("takes the overlay down when the daemon closes it", () => {
    // Arrange
    const { host, overlay } = mounted();
    overlay.apply(opened());
    // Act
    overlay.apply(closed());
    // Assert
    expect([host.hidden, host.childElementCount]).toEqual([true, 0]);
    overlay.dispose();
  });

  it("says an image that cannot be loaded is no longer available, in its place", () => {
    // Arrange
    const { host, overlay } = mounted();
    overlay.apply(opened("/feed-images/gone"));
    // Act
    host.querySelector("img")?.dispatchEvent(new Event("error"));
    // Assert
    expect(host.querySelector(".image-overlay-missing")?.textContent).toBe(IMAGE_MISSING_TEXT);
    overlay.dispose();
  });

  it("asks the daemon to close on a click anywhere on it", async () => {
    // Arrange
    const { h, host, overlay } = mounted();
    overlay.apply(opened());
    // Act
    host.querySelector("img")?.click();
    await vi.runAllTimersAsync();
    // Assert
    expect(h.calls.closeFeedImage).toHaveLength(1);
    overlay.dispose();
  });

  it("asks the daemon to close on Escape", async () => {
    // Arrange
    const { h, overlay } = mounted();
    overlay.apply(opened());
    // Act
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await vi.runAllTimersAsync();
    // Assert
    expect(h.calls.closeFeedImage).toHaveLength(1);
    overlay.dispose();
  });

  it("asks nothing on Escape while no image is open", async () => {
    // Arrange
    const { h, overlay } = mounted();
    // Act
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await vi.runAllTimersAsync();
    // Assert
    expect(h.calls.closeFeedImage).toHaveLength(0);
    overlay.dispose();
  });

  it("records the open at info, naming its source", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { overlay } = mounted();
    // Act
    overlay.apply(opened());
    // Assert
    const record = await forwardedRecord(capture, "image-overlay.shown");
    expect([record.level.case, record.context?.src]).toEqual(["info", "/feed-images/abc"]);
    overlay.dispose();
  });

  it("refuses an overlay whose state is unset", () => {
    const { overlay } = mounted();
    let thrown: unknown;
    try {
      overlay.apply(create(FeedImageOverlaySchema, {}));
    } catch (err) {
      thrown = err;
    }
    expect(isMalformedView(thrown)).toBe(true);
    overlay.dispose();
  });

  it("stops listening once disposed", async () => {
    // Arrange
    const { h, overlay } = mounted();
    overlay.apply(opened());
    overlay.dispose();
    // Act
    document.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await vi.runAllTimersAsync();
    // Assert
    expect(h.calls.closeFeedImage).toHaveLength(0);
  });
});

describe("the image overlay's own rules in the stylesheet", () => {
  const rule = (selector: string): string =>
    stylesheet.match(new RegExp(`${selector.replace(/[[\]"=.-]/g, "\\$&")}\\s*\\{[^}]*\\}`))?.[0] ?? "";

  it("insets the frame inside the feed's box, so the rows show around it", () => {
    expect(rule('[data-component="image-overlay"]')).toContain("padding: 0.75rem 1rem");
  });

  it("draws the image as large as fits, aspect kept", () => {
    expect([rule(".image-overlay-image").includes("max-width: 100%"), rule(".image-overlay-image").includes("object-fit: contain")]).toEqual([
      true,
      true,
    ]);
  });
});
