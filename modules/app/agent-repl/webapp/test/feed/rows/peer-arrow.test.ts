// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { PEER_ARROW_CLASS, drawPeerArrow } from "../../../src/feed/rows/peer-arrow.js";

describe("drawPeerArrow", () => {
  it.each([["in"], ["out"]] as const)("states its direction %s", (direction) => {
    expect(drawPeerArrow(direction).getAttribute("data-direction")).toBe(direction);
  });

  it("wears the class the stylesheet sizes it by", () => {
    expect(drawPeerArrow("in").getAttribute("class")).toBe(PEER_ARROW_CLASS);
  });

  it("is hidden from assistive technology, the name beside it being the header's words", () => {
    expect(drawPeerArrow("out").getAttribute("aria-hidden")).toBe("true");
  });

  it("points left for a message that came in", () => {
    const head = drawPeerArrow("in").querySelectorAll("path")[1]?.getAttribute("d");
    expect(head).toBe("M7 4 L3 8 L7 12");
  });

  it("points right for a message that went out", () => {
    const head = drawPeerArrow("out").querySelectorAll("path")[1]?.getAttribute("d");
    expect(head).toBe("M9 4 L13 8 L9 12");
  });
});
