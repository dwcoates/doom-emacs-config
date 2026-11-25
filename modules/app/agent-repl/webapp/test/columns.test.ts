// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { columnHeader } from "../src/columns.js";

describe("columnHeader", () => {
  it("names its column with data-column", () => {
    expect(columnHeader("stage").getAttribute("data-column")).toBe("stage");
  });

  it("says its column's name", () => {
    expect(columnHeader("duration").textContent).toBe("duration");
  });

  it("wears the shared column header class", () => {
    expect(columnHeader("tokens").className).toBe("footer-column-header");
  });
});

