import { describe, expect, it } from "vitest";

import {
  CACHE_LAPSE_GREEN_MS,
  COLD_CACHE_TEXT,
  cacheLapseHeat,
  cacheLapseText,
} from "../../src/sidebar/cache-lapse.js";

describe("cacheLapseText", () => {
  it.each([
    ["minutes and seconds out", 12 * 60_000 + 34_000, "12m 34s"],
    ["a fraction of a second out", 400, "1s"],
    ["just over a second out", 1_200, "2s"],
    ["the full hour out", CACHE_LAPSE_GREEN_MS, "1h"],
    ["at the lapse", 0, COLD_CACHE_TEXT],
    ["past the lapse", -5_000, COLD_CACHE_TEXT],
  ])("reads %s", (_name, remainingMs, want) => {
    // Act, Assert
    expect(cacheLapseText(remainingMs)).toBe(want);
  });
});

describe("cacheLapseHeat", () => {
  it.each([
    ["more than an hour out", 2 * CACHE_LAPSE_GREEN_MS, 0],
    ["exactly an hour out", CACHE_LAPSE_GREEN_MS, 0],
    ["half an hour out", CACHE_LAPSE_GREEN_MS / 2, 0.5],
    ["at the lapse", 0, 1],
    ["past the lapse", -60_000, 1],
  ])("sits %s", (_name, remainingMs, want) => {
    // Act, Assert
    expect(cacheLapseHeat(remainingMs)).toBe(want);
  });
});
