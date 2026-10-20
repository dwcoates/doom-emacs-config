// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import {
  SEND_GLIDE_ATTRIBUTE,
  SEND_GLIDE_EASING,
  SEND_GLIDE_MS,
  glideSentPrompt,
} from "../../src/feed/send-glide.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { deferredAnimation } from "../deferred-animation.js";

/** A connected row HEIGHT px tall holding one bubble. */
function row(height: number) {
  const el = document.createElement("article");
  el.className = "feed-item";
  el.getBoundingClientRect = () => ({ height }) as DOMRect;
  const bubble = document.createElement("div");
  el.append(bubble);
  document.body.append(el);
  return { el, bubble };
}

const MOTION = { reducedMotion: () => false };

afterEach(() => {
  document.body.replaceChildren();
  vi.restoreAllMocks();
});

describe("glideSentPrompt", () => {
  it("animates the bubble up from one row-height below the row's bottom edge", () => {
    // Arrange
    const { el, bubble } = row(64);
    const animate = vi.spyOn(bubble, "animate");

    // Act
    glideSentPrompt(el, bubble, MOTION);

    // Assert
    expect(animate.mock.calls[0]).toEqual([
      [{ transform: "translateY(64px)" }, { transform: "none" }],
      { duration: SEND_GLIDE_MS, easing: SEND_GLIDE_EASING },
    ]);
  });

  it("starts the glide in the drawing task, before any frame paints", () => {
    // Arrange
    const { el, bubble } = row(64);
    const animate = vi.spyOn(bubble, "animate");

    // Act
    glideSentPrompt(el, bubble, MOTION);

    // Assert
    expect(animate).toHaveBeenCalledTimes(1);
  });

  it("clips the row while the bubble glides", () => {
    // Arrange
    const { el, bubble } = row(64);
    vi.spyOn(bubble, "animate").mockReturnValue(deferredAnimation().animation);

    // Act
    glideSentPrompt(el, bubble, MOTION);

    // Assert
    expect(el.hasAttribute(SEND_GLIDE_ATTRIBUTE)).toBe(true);
  });

  it("unclips the row once the glide finishes", async () => {
    // Arrange
    const { el, bubble } = row(64);
    const held = deferredAnimation();
    vi.spyOn(bubble, "animate").mockReturnValue(held.animation);
    glideSentPrompt(el, bubble, MOTION);

    // Act
    await held.finish();

    // Assert
    expect(el.hasAttribute(SEND_GLIDE_ATTRIBUTE)).toBe(false);
  });

  it("unclips the row when the glide is cut short", async () => {
    // Arrange
    const { el, bubble } = row(64);
    const held = deferredAnimation();
    vi.spyOn(bubble, "animate").mockReturnValue(held.animation);
    glideSentPrompt(el, bubble, MOTION);

    // Act
    await held.cancel();

    // Assert
    expect(el.hasAttribute(SEND_GLIDE_ATTRIBUTE)).toBe(false);
  });

  it("records the glide at DEBUG with its rise and duration", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { el, bubble } = row(64);

    // Act
    glideSentPrompt(el, bubble, MOTION);

    // Assert
    const record = await forwardedRecord(capture, "feed.send-glide");
    expect(record.context).toMatchObject({ rise_px: 64, duration_ms: SEND_GLIDE_MS });
  });

  it("shows the bubble at once, unclipped, for a reader who asked for reduced motion", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { el, bubble } = row(64);
    const animate = vi.spyOn(bubble, "animate");

    // Act
    glideSentPrompt(el, bubble, { reducedMotion: () => true });

    // Assert
    const record = await forwardedRecord(capture, "feed.send-glide-skipped");
    expect([animate.mock.calls.length, el.hasAttribute(SEND_GLIDE_ATTRIBUTE), record.context]).toMatchObject([
      0,
      false,
      { why: "reduced-motion" },
    ]);
  });
});
