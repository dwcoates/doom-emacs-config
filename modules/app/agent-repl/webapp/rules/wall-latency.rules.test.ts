/** Every wall-clock latency site measures through recordWallLatency (src/wall-latency.ts). */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

describe("the wall-clock latency sites", () => {
  // EVERY latency that starts in another process ends through the one helper,
  // so none can drift on the clock-step rule or the unit.
  it.each(["src/switch-latency.ts", "src/prompt-echo.ts"])("%s measures through recordWallLatency", (file) => {
    const source = readFileSync(new URL(`../${file}`, import.meta.url), "utf8");
    expect(source).toContain("recordWallLatency(");
    expect(source).not.toMatch(/wallNow\(\)\s*-/);
  });
});
