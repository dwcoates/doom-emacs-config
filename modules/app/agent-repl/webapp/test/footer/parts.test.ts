// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { grabber, statusWords, textLine } from "../../src/footer/parts.js";
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

describe("grabber", () => {
  it("draws the notch hidden from assistive tech", () => {
    const notch = grabber();
    expect([notch.className, notch.getAttribute("aria-hidden")]).toEqual(["pfooter-grab", "true"]);
  });
});
