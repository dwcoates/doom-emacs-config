import { describe, expect, it } from "vitest";
import { formatTokens } from "../src/format.js";

describe("formatTokens", () => {
  const cases = [
    { n: 0, want: "0" },
    { n: 999, want: "999" },
    { n: 1_000, want: "1k" },
    { n: 1_200, want: "1.2k" },
    { n: 12_340, want: "12.3k" },
    { n: 182_000, want: "182k" },
    { n: 999_949, want: "999.9k" },
    { n: 999_950, want: "1M" },
    { n: 1_200_000, want: "1.2M" },
    { n: 1_049, want: "1k" },
    { n: 1_050, want: "1.1k" },
    { n: 10_113, want: "10.1k" },
    { n: 142_300, want: "142.3k" },
    { n: 1_000_000, want: "1M" },
  ] as const;

  for (const c of cases) {
    it(`formats ${c.n} as ${c.want}`, () => {
      expect(formatTokens(c.n)).toBe(c.want);
    });
  }
});
