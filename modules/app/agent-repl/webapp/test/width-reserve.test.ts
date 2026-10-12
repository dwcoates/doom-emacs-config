import { describe, expect, it } from "vitest";
import responseSource from "../src/feed/cards/response.ts?raw";
import { fitsReserve, reserveLabels, reserveLabelsCss, zeroFigures } from "../src/width-reserve.js";

describe("zeroFigures", () => {
  it("zeroes every digit and keeps every other character", () => {
    // Arrange / Act / Assert
    expect(zeroFigures("12m 37s ago")).toBe("00m 00s ago");
  });
});

describe("reserveLabels", () => {
  it("draws each widest instant through the formatter, figures zeroed", () => {
    // Arrange
    const label = (ms: number): string => `${String(ms / 1000)}s`;
    // Act
    const reserve = reserveLabels([59_000, 9_000], label);
    // Assert
    expect(reserve).toEqual(["00s", "0s"]);
  });
});

describe("fitsReserve", () => {
  const reserve = ["00h 00m", "00m 00s", "00s"];

  it.each([
    ["a reading of a reserved shape with fewer figures", "5m 3s", true],
    ["a reading of a reserved shape at its full figures", "59m 59s", true],
    ["a reading with more figures than its shape reserves", "100h 20m", false],
    ["a reading of a shape the reserve lacks", "3d 2h", false],
  ])("answers %s", (_name, label, fits) => {
    // Arrange / Act / Assert
    expect(fitsReserve(label, reserve)).toBe(fits);
  });
});

describe("reserveLabelsCss", () => {
  it("joins the labels into one quoted CSS string, a line each", () => {
    // Arrange / Act / Assert
    expect(reserveLabelsCss(["00s", "00m 00s"])).toBe('"00s\\A 00m 00s"');
  });

  it("is the one zeroing the response usage corner uses", () => {
    // Arrange -- a hand-rolled zeroing would repeat the digit pattern.
    const handRolled = responseSource.match(/replace\(\/\\d\/g, "0"\)/g) ?? [];
    // Act + Assert
    expect(handRolled).toEqual([]);
  });
});
