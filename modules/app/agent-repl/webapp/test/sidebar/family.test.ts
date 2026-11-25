// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { pathedFamilies } from "../../src/sidebar/family.js";
import { openRow, row } from "./harness.js";

describe("pathedFamilies", () => {
  it("gives each family its index under the region's path", () => {
    const got = pathedFamilies([openRow(row({ id: "a" })), openRow(row({ id: "b" }))], "R.rows");
    expect(got.map((e) => [e.family.row?.workspace?.workspace?.id, e.path])).toEqual([
      ["a", "R.rows[0]"],
      ["b", "R.rows[1]"],
    ]);
  });
});
