/** prefersReducedMotion (src/reduced-motion.ts) is how every source reads the reader's motion setting. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../src");

describe("prefersReducedMotion", () => {
  it.each(["feed/send-glide.ts", "bubble/collapse-motion.ts"])("is how %s reads the reader's setting", (file) => {
    const source = readFileSync(path.join(SRC, file), "utf8");
    expect([source.includes("prefersReducedMotion("), source.includes("prefers-reduced-motion: reduce")]).toEqual([true, false]);
  });
});
