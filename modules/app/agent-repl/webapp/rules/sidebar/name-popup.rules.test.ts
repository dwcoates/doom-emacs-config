/**
 * EVERY NAME THE SIDEBAR ASKS FOR IS ASKED THROUGH `openNamePopup`: the
 * sidebar's sources read for the popup's spelling and its openers.
 */
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("the sidebar's name popups", () => {
  // EVERY NAME THE SIDEBAR ASKS FOR IS ASKED THROUGH openNamePopup: the
  // popup's class, input and button are spelled in name-popup.ts alone, so a
  // hand-rolled second popup fails here rather than drifting from the first.
  it("are all drawn by openNamePopup", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar");
    const spelled = readdirSync(dir)
      .filter((file) => file.endsWith(".ts"))
      .filter((file) => /sb-subsection-popup|sb-subsection-name/.test(readFileSync(path.join(dir, file), "utf8")));
    const openers = readdirSync(dir)
      .filter((file) => file.endsWith(".ts") && file !== "name-popup.ts")
      .filter((file) => /openNamePopup\(/.test(readFileSync(path.join(dir, file), "utf8")));
    expect([spelled, openers.sort()]).toEqual([["name-popup.ts"], ["rename.ts", "subsections.ts"]]);
  });
});
