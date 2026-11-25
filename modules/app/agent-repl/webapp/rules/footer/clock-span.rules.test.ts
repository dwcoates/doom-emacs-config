/**
 * Every footer clock span and activity age is built by the one builder
 * (src/footer/clock-span.ts): none is marked or drawn by hand elsewhere in
 * the footer.
 */
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

// EVERY FOOTER CLOCK IS BUILT BY footerClockSpan: a span marked by hand
// elsewhere is a second spelling of the same shape, which drifts.
describe("the footer's clock spans share one builder", () => {
  it("marks no clock span by hand outside clock-span.ts", () => {
    const dir = path.join(here, "..", "..", "src", "footer");
    const offenders = readdirSync(dir)
      .filter((file) => file.endsWith(".ts") && file !== "clock-span.ts")
      .filter((file) => /setAttribute\("data-(countdown|age)"/.test(readFileSync(path.join(dir, file), "utf8")));
    expect(offenders).toEqual([]);
  });
});

// EVERY ACTIVITY AGE IS footerAgeSpan's: a hand-drawn " · N ago" elsewhere in
// the footer is a second spelling of the same line ending, which drifts.
describe("the footer's activity ages share one builder", () => {
  it("draws no activity age by hand outside clock-span.ts", () => {
    const dir = path.join(here, "..", "..", "src", "footer");
    const offenders = readdirSync(dir)
      .filter((file) => file.endsWith(".ts") && file !== "clock-span.ts")
      .filter((file) => /"footer-activity-age"|\} ago`/.test(readFileSync(path.join(dir, file), "utf8")));
    expect(offenders).toEqual([]);
  });
});
