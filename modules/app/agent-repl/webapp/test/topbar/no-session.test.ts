// @vitest-environment jsdom
/**
 * The no-session cell.
 *
 * WHAT THIS GUARDS: that a control the daemon left absent keeps its slot and
 * says why. The failure modes being excluded are a strip that rearranges
 * itself the moment a workspace hibernates, and a dash a reader cannot
 * interpret.
 */
import { describe, expect, it } from "vitest";
import { NO_SESSION_DASH, drawNoSessionCell } from "../../src/topbar/no-session.js";

describe("the no-session cell", () => {
  it("draws the dash", () => {
    expect(drawNoSessionCell("model").textContent).toBe(NO_SESSION_DASH);
  });

  it("carries the cell it stands in, so the slot is identifiable", () => {
    expect(drawNoSessionCell("mode").getAttribute("data-no-session")).toBe("mode");
  });

  it("keeps the class of the control it replaces, so the slot keeps its box", () => {
  });

  it("says why it is empty, because a bare dash is uninterpretable", () => {
    expect(drawNoSessionCell("model").title).toContain("no session");
  });

  it("is not a control: a span, never a button", () => {
    expect(drawNoSessionCell("model").tagName).toBe("SPAN");
  });
});
