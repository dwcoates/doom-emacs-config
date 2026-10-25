// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { coldGateFiguredLine, statusWords, textLine } from "../../src/footer/parts.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { coldGatePercentColor } from "../../src/pressure-color.js";
import { FOOTER_STATUS_CASES } from "../../src/footer/tones.js";
import { protoArmName } from "../../src/vocab.js";

describe("statusWords", () => {
  it.each(FOOTER_STATUS_CASES.map((arm) => [arm]))("words the %s arm lowercase", (arm) => {
    expect(statusWords(arm)).toBe(protoArmName(arm).replace(/_/g, " "));
  });

  it("puts spaces where the schema spelling has underscores", () => {
    expect(statusWords("networkResume")).toBe("network resume");
  });
});

describe("textLine", () => {
  it("draws the text verbatim", () => {
    expect(textLine("x", "a · b").textContent).toBe("a · b");
  });

  it("wears the class it was handed", () => {
    expect(textLine("footer-activity-hook", "fmt").className).toBe("footer-activity-hook");
  });
});

describe("coldGateFiguredLine", () => {
  const parts = { lead: "spent ", figure: { text: "104.3k", windowFill: 0.41 } as never, tail: " tokens" };

  /** The color PERCENT paints on the cold-gate gradient, as the DOM normalizes it. */
  function painted(percent: number): string {
    const probe = document.createElement("span");
    probe.style.color = coldGatePercentColor(percent);
    return probe.style.color;
  }

  it("reads exactly as its composed text", () => {
    expect(coldGateFiguredLine("x", "spent 104.3k tokens", parts, "p").textContent).toBe("spent 104.3k tokens");
  });

  it("wears the class it was handed", () => {
    expect(coldGateFiguredLine("footer-activity-compaction", "spent 104.3k tokens", parts, "p").className).toBe(
      "footer-activity-compaction",
    );
  });

  it("draws the figure in its own span", () => {
    const line = coldGateFiguredLine("x", "spent 104.3k tokens", parts, "p");
    expect(line.querySelector(".footer-cold-gate-figure")?.textContent).toBe("104.3k");
  });

  it("colors the figure by its window fill on the cold-gate gradient", () => {
    const line = coldGateFiguredLine("x", "spent 104.3k tokens", parts, "p");
    expect(line.querySelector<HTMLElement>(".footer-cold-gate-figure")?.style.color).toBe(painted(41));
  });

  it("refuses parts that do not make the composed text", () => {
    expect(() => coldGateFiguredLine("x", "spent 104.3k context tokens", parts, "p")).toThrow(MalformedView);
  });

  it("refuses parts with no figure", () => {
    expect(() => coldGateFiguredLine("x", "spent  tokens", { lead: "spent ", tail: " tokens" }, "p")).toThrow(
      MalformedView,
    );
  });

  it("refuses a window fill outside [0, 1]", () => {
    const outside = { ...parts, figure: { text: "104.3k", windowFill: 1.5 } as never };
    expect(() => coldGateFiguredLine("x", "spent 104.3k tokens", outside, "p")).toThrow(MalformedView);
  });
});
