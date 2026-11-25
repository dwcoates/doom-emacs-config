// How the rail places a box's children: through dom.ts's one placeChildren,
// which moves only what is out of place. The second copy this file once
// tested (src/sidebar/children.ts) re-inserted every child on any change and
// was removed on 2026-11-23.
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder } from "../files-under.js";
import { codeOf } from "../../test/source-text.js";

describe("the rail's placement of its children", () => {
  it("is dom.ts's one placeChildren, and no sidebar source defines its own", () => {
    // Arrange
    const sidebarDir = join(__dirname, "..", "..", "src", "sidebar");
    const roster = codeOf(readFileSync(join(sidebarDir, "roster.ts"), "utf8"));
    const sidebar = codeOf(readFileSync(join(sidebarDir, "sidebar.ts"), "utf8"));
    const copies = [...codeUnder(sidebarDir)].filter(([, code]) => /function placeChildren\b/.test(code)).map(([path]) => path);

    // Act / Assert
    expect([
      roster.includes('from "../dom.js"') && roster.includes("placeChildren("),
      sidebar.includes('from "../dom.js"') && sidebar.includes("placeChildren(body"),
      copies,
    ]).toEqual([true, true, []]);
  });
});
