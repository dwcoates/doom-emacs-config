/**
 * THE TOPBAR'S TOGGLE CHIPS SHARE ONE CLICK PATH: no topbar source but
 * chip-toggle.ts marks a chip as settling, and every toggle chip sends
 * through `sendChipToggle`.
 */
import { readFileSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("the toggle chips' one click path", () => {
  const topbarDir = join(dirname(fileURLToPath(import.meta.url)), "../../src/topbar");
  const sources = readdirSync(topbarDir)
    .filter((f) => f.endsWith(".ts") && f !== "chip-toggle.ts")
    .map((f) => ({ file: f, text: readFileSync(join(topbarDir, f), "utf8") }));

  it("is the only topbar code that marks a chip as settling", () => {
    expect(sources.filter((s) => s.text.includes('"data-settling"')).map((s) => s.file)).toEqual([]);
  });

  it("is what every toggle chip sends through", () => {
    const callers = sources.filter((s) => s.text.includes("sendChipToggle(")).map((s) => s.file).sort();
    expect(callers).toEqual(["persistent-wifi.ts", "queue-bypass.ts", "remote-control.ts"]);
  });
});
