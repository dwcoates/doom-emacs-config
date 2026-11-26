/**
 * Every activity cell is built and replaced through src/footer/activity-cell.ts:
 * no footer source hand-rolls the cell's class list or its replacement.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { filesUnder, srcDir } from "../files-under.js";

const helper = path.join(srcDir, "footer", "activity-cell.ts");

describe("every activity cell goes through activity-cell", () => {
  it("hand-rolls the activity cell's class list in no src file", () => {
    const offenders = filesUnder(srcDir)
      .filter((file) => file !== helper && /pfooter-grow footer-activity\b/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(srcDir, file));
    expect(offenders).toEqual([]);
  });

  it("finds the activity cell by its class in no src file but the helper", () => {
    const offenders = filesUnder(srcDir)
      .filter((file) => file !== helper && /classList\.contains\((?:"footer-activity"|ACTIVITY_CELL_CLASS)\)/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(srcDir, file));
    expect(offenders).toEqual([]);
  });
});
