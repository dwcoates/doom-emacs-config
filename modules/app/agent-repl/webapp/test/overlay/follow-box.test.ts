// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { followBox } from "../../src/overlay/follow-box.js";
import { fireResize } from "../resize-observer.js";

/** A shown host and a target at RECT. */
function arrange(rect: DOMRect): { host: HTMLElement; target: HTMLElement } {
  const host = document.createElement("div");
  const target = document.createElement("div");
  target.getBoundingClientRect = () => rect;
  document.body.replaceChildren(target, host);
  return { host, target };
}

/** The four coordinates the follower writes. */
const placed = (host: HTMLElement): string[] => [host.style.top, host.style.left, host.style.width, host.style.height];

describe("followBox", () => {
  it("copies the target's rectangle onto a shown host", () => {
    // Arrange
    const { host, target } = arrange(new DOMRect(240, 36, 800, 600));
    const box = followBox(host, target);

    // Act
    box.follow();

    // Assert
    expect(placed(host)).toEqual(["36px", "240px", "800px", "600px"]);
    box.dispose();
  });

  it("leaves a hidden host where it is", () => {
    // Arrange
    const { host, target } = arrange(new DOMRect(240, 36, 800, 600));
    host.hidden = true;
    const box = followBox(host, target);

    // Act
    box.follow();

    // Assert
    expect(placed(host)).toEqual(["", "", "", ""]);
    box.dispose();
  });

  it("follows the target when it resizes", () => {
    // Arrange
    const { host, target } = arrange(new DOMRect(0, 0, 100, 100));
    const box = followBox(host, target);
    target.getBoundingClientRect = () => new DOMRect(10, 20, 300, 400);

    // Act
    fireResize(target);

    // Assert
    expect(placed(host)).toEqual(["20px", "10px", "300px", "400px"]);
    box.dispose();
  });

  it("stops following once disposed", () => {
    // Arrange
    const { host, target } = arrange(new DOMRect(0, 0, 100, 100));
    followBox(host, target).dispose();

    // Act / Assert: nothing observes the target any more.
    expect(() => fireResize(target)).toThrow();
  });
});
