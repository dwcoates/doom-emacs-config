/** The test rosters that build families take them from test/roster-families.ts. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { testDir } from "./files-under.js";

describe("the test rosters that build families", () => {
  it.each(["sidebar/harness.ts", "integration/fixtures.ts"])("%s takes them from roster-families", (file) => {
    // A hand-rolled family or numbering would let the two kinds of test roster disagree.
    const source = readFileSync(path.join(testDir, file), "utf8");
    expect(source).toMatch(/from "\.\.\/roster-families\.js"/);
    expect(source).not.toMatch(/new WeakMap<RosterRow/);
    expect(source).not.toMatch(/RosterOpenRowTabSchema/);
  });

  it.each(["sidebar/harness.ts", "integration/fixtures.ts"])("%s numbers a section's tabs through sectionTabRegions", (file) => {
    // A hand-rolled walk of a section's entries could number them out of drawn order.
    const source = readFileSync(path.join(testDir, file), "utf8");
    expect(source).toMatch(/\.map\(sectionTabRegions\)/);
  });
});
