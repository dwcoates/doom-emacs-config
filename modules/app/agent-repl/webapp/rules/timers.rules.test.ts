/** The timer seam's call sites keep no timers of their own beside src/timers.ts. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

describe("the timer seam's call sites", () => {
  it("hold no map of timer handles of their own beside createKeyedTimers", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../src");
    const own = filesUnder(src, [".ts", ".tsx"]).filter((file) =>
      /new Map<[^>]*ReturnType<typeof setTimeout>>/.test(readFileSync(file, "utf8")),
    );
    expect(own.map((file) => path.relative(src, file))).toEqual([]);
  });

  it("declare no timer seam of their own beside src/timers.ts", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../src");
    const own = filesUnder(src, [".ts", ".tsx"]).filter(
      (file) => !file.endsWith(`${path.sep}timers.ts`) && /setTimeout\(fn: \(\) => void, ms: number\): number;/.test(readFileSync(file, "utf8")),
    );
    expect(own.map((file) => path.relative(src, file))).toEqual([]);
  });
});
