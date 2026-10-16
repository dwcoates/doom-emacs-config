// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { choiceNumeral, drawChoiceNumeral } from "../../../src/feed/asks/choice-number.js";

describe("a choice's numeral", () => {
  it("is its number and a dot", () => {
    expect([choiceNumeral(1), choiceNumeral(12)]).toEqual(["1.", "12."]);
  });

  it("is a span of the named class carrying its number on the named attribute", () => {
    const el = drawChoiceNumeral(3, "x-number", "data-x-number");
    expect([el.tagName, el.className, el.getAttribute("data-x-number"), el.textContent]).toEqual([
      "SPAN",
      "x-number",
      "3",
      "3.",
    ]);
  });

});
