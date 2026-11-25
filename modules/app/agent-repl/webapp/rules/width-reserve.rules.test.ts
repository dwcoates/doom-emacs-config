/** The response usage corner zeroes and quotes its reserve only through src/width-reserve.ts. */
import { describe, expect, it } from "vitest";
import responseSource from "../src/feed/cards/response.ts?raw";

describe("reserveLabelsCss", () => {
  it("is the one zeroing the response usage corner uses", () => {
    // Arrange -- a hand-rolled zeroing would repeat the digit pattern.
    const handRolled = responseSource.match(/replace\(\/\\d\/g, "0"\)/g) ?? [];
    // Act + Assert
    expect(handRolled).toEqual([]);
  });
});

describe("cssString", () => {
  it("is the one CSS string the response corner hands its ghost", () => {
    // Arrange -- a hand-rolled quoting would wrap a template in quotes.
    const handRolled = responseSource.match(/`"\$\{/g) ?? [];
    // Act + Assert
    expect(handRolled).toEqual([]);
  });
});
