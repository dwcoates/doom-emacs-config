/**
 * The hook-side rejection capture (rejection.ts), and the one shape every
 * webkit suite records a refusal through.
 */
import path from "node:path";
import { readFile, readdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { beforeAll, describe, expect, it } from "vitest";
import { rejectionOf } from "./rejection";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("rejectionOf", () => {
  const thrown = new Error("refused");
  let rejected: unknown;
  let resolved: unknown;
  let hand: string[];

  beforeAll(async () => {
    rejected = await rejectionOf(Promise.reject(thrown));
    resolved = await rejectionOf(Promise.resolve("fine"));
    const names = (await readdir(here)).filter((name) => name.endsWith(".ts") && name !== "rejection.ts");
    hand = [];
    for (const name of names) {
      const src = await readFile(path.join(here, name), "utf8");
      if (/\(\) => null,\s*\(err: unknown\) => err/.test(src)) hand.push(name);
    }
  });

  it("answers the error a rejected promise carried", () => {
    // Arrange + Act: the hook's rejected capture. Assert
    expect(rejected).toBe(thrown);
  });

  it("answers null for a promise that resolved", () => {
    // Arrange + Act: the hook's resolved capture. Assert
    expect(resolved).toBeNull();
  });

  it("is the only place a webkit suite turns a rejection into a value by hand", () => {
    // Arrange + Act: the hook's read of every webkit file. Assert
    expect(hand).toEqual([]);
  });
});
