// @vitest-environment jsdom
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { columnHeader, COLUMNS_ROW_CLASS } from "../src/columns.js";

const here = path.dirname(fileURLToPath(import.meta.url));

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

describe("COLUMNS_ROW_CLASS", () => {
  it("is the class both column tables' shared rule takes their rows by", () => {
    const css = readFileSync(path.join(here, "..", "src", "styles.css"), "utf8");
    expect([
      css.includes(`.pfooter-sheet[data-panel="agents"] > .${COLUMNS_ROW_CLASS}`),
      css.includes(`.merge-queue .${COLUMNS_ROW_CLASS}`),
    ]).toEqual([true, true]);
  });
});

/** Every .ts file under DIR, recursively. */
function sources(dir: string): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = path.join(dir, name);
    if (statSync(full).isDirectory()) return sources(full);
    return name.endsWith(".ts") ? [full] : [];
  });
}

// EVERY COLUMN TABLE'S HEADERS AND ROWS COME FROM columns.ts: a header cell or
// a row class spelled by hand elsewhere is a second table that drifts.
describe("the column tables share one set of pieces", () => {
  it("spells the header and row classes nowhere outside columns.ts", () => {
    const src = path.join(here, "..", "src");
    const offenders = sources(src)
      .filter((file) => path.basename(file) !== "columns.ts")
      .filter((file) => /footer-column(?:-header|s)\b/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file));
    expect(offenders).toEqual([]);
  });
});
