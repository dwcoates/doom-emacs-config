// @vitest-environment jsdom
// A rail row drawn only once it comes into view. See src/sidebar/reveal.ts.
import { describe, expect, it } from "vitest";
import { createRowReveal, REVEAL_MARGIN } from "../../src/sidebar/reveal.js";
import { fireIntersection, intersectionObservers } from "../intersection-observer.js";
import { captureLogRecords } from "../log-capture.js";

/** A reveal counting its after-batch runs. */
function counted() {
  let revealed = 0;
  const reveal = createRowReveal(() => (revealed += 1));
  return { reveal, revealed: () => revealed };
}

describe("createRowReveal", () => {
  it("draws nothing before the row comes into view", () => {
    // Arrange
    const { reveal } = counted();
    let drew = 0;

    // Act
    reveal.whenShown(document.createElement("div"), () => (drew += 1));

    // Assert
    expect(drew).toBe(0);
    reveal.dispose();
  });

  it("draws the row the first time it comes into view", () => {
    // Arrange
    const { reveal } = counted();
    const target = document.createElement("div");
    let drew = 0;
    reveal.whenShown(target, () => (drew += 1));

    // Act
    fireIntersection(target, true);

    // Assert
    expect(drew).toBe(1);
    reveal.dispose();
  });

  it("stops watching a row once it is drawn", () => {
    // Arrange
    const { reveal } = counted();
    const target = document.createElement("div");
    reveal.whenShown(target, () => undefined);

    // Act
    fireIntersection(target, true);

    // Assert
    expect(() => fireIntersection(target, true)).toThrow(/no IntersectionObserver/);
    reveal.dispose();
  });

  it("draws nothing for a row reported leaving the view", () => {
    // Arrange
    const { reveal, revealed } = counted();
    const target = document.createElement("div");
    let drew = 0;
    reveal.whenShown(target, () => (drew += 1));

    // Act
    fireIntersection(target, false);

    // Assert
    expect([drew, revealed()]).toEqual([0, 0]);
    reveal.dispose();
  });

  it("runs the rail's after-reveal once a row is drawn", () => {
    // Arrange
    const { reveal, revealed } = counted();
    const target = document.createElement("div");
    reveal.whenShown(target, () => undefined);

    // Act
    fireIntersection(target, true);

    // Assert
    expect(revealed()).toBe(1);
    reveal.dispose();
  });

  it("draws nothing once the section's teardown stopped the wait", () => {
    // Arrange
    const { reveal } = counted();
    const target = document.createElement("div");
    const stop = reveal.whenShown(target, () => undefined);

    // Act
    stop();

    // Assert
    expect(() => fireIntersection(target, true)).toThrow(/no IntersectionObserver/);
    reveal.dispose();
  });

  it("watches from the page's viewport, a margin ahead of it", () => {
    // Act
    const { reveal } = counted();

    // Assert
    const observer = intersectionObservers().at(-1);
    expect([observer?.root, observer?.rootMargin]).toEqual([null, REVEAL_MARGIN]);
    reveal.dispose();
  });

  it("stops observing when the rail is disposed", () => {
    // Arrange
    const { reveal } = counted();
    const target = document.createElement("div");
    reveal.whenShown(target, () => undefined);

    // Act
    reveal.dispose();

    // Assert
    expect(() => fireIntersection(target, true)).toThrow(/no IntersectionObserver/);
  });

  it("refuses a page with no IntersectionObserver, logging why", async () => {
    // Arrange
    const capture = captureLogRecords("debug");

    // Act
    const create = (): unknown => createRowReveal(() => undefined, null);

    // Assert
    expect(create).toThrow(/no IntersectionObserver/);
    capture.logger.flush();
    await Promise.resolve();
    expect(capture.sent.find((r) => r.operation === "sidebar.reveal.unsupported")?.level.case).toBe("error");
  });
});
