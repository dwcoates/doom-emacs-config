/**
 * The RUNTIME half of the SDK boundary. The structural half — that the vendor's
 * `Query` still satisfies `QueryLike`, and that every message arm still exists
 * under its declared name — is asserted at COMPILE time by `sdk/types.ts`
 * itself (`_QueryStillSatisfiesQueryLike` and the `import type` list), so it is
 * `npm run typecheck` that fails on a vendor upgrade, not this file.
 */
import { describe, expect, it } from "vitest";
import { describeInterruptSurvivors } from "../../src/sdk/types.js";

describe("describeInterruptSurvivors", () => {
  it("stays silent on the expected empty receipt", () => {
    // Arrange, Act.
    const described = describeInterruptSurvivors({ still_queued: [] });

    // Assert.
    expect(described).toBeNull();
  });

  it("stays silent when the CLI answered with no receipt at all", () => {
    // Arrange, Act.
    const described = describeInterruptSurvivors(undefined);

    // Assert.
    expect(described).toBeNull();
  });

  it("names every surviving uuid verbatim rather than counting them", () => {
    // Arrange, Act.
    const described = describeInterruptSurvivors({ still_queued: ["u-1", "u-2"] });

    // Assert.
    expect(described).toBe(
      "interrupt receipt reports 2 message(s) STILL QUEUED CLI-side, which the daemon cannot " +
        "see or cancel: still_queued=[u-1 u-2]",
    );
  });

  it("names the cancelled uuids alongside the survivors", () => {
    // Arrange, Act.
    const described = describeInterruptSurvivors({ still_queued: ["u-1"], cancelled: ["u-9"] });

    // Assert.
    expect(described).toContain("; cancelled=[u-9]");
  });
});
