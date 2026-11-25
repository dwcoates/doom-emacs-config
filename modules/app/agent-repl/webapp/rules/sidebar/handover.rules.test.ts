/**
 * THE SIDEBAR'S HANDOVER CALL SITES go through `handover.ts`: no other sidebar
 * source dials another daemon or tests a handover arm by hand.
 */
import { describe, expect, it } from "vitest";
import { HANDOVER_ARMS } from "../../src/sidebar/handover.js";
import { codeOf } from "../../test/source-text.js";

describe("the sidebar's handover call sites", () => {
  // eslint-disable-next-line @typescript-eslint/no-unnecessary-type-assertion -- eslint resolves import.meta.glob through vite/client and reads the assertion as a no-op; tsc, whose program has no vite/client, does not, and rejects the raw result as `unknown` without it.
  const sources = import.meta.glob("../../src/sidebar/**/*.ts", { query: "?raw", import: "default", eager: true }) as Record<string, string>;
  const others = Object.entries(sources).filter(([path]) => !path.endsWith("/handover.ts"));

  it("dial another daemon only through contextAt", () => {
    // Act
    const handRolled = others.filter(([, source]) => /\.clientAt\(/.test(codeOf(source))).map(([path]) => path);

    // Assert
    expect(handRolled).toEqual([]);
  });

  it.each(HANDOVER_ARMS)("never test the %s arm by hand", (arm) => {
    // Act
    const handRolled = others.filter(([, source]) => codeOf(source).includes(`=== "${arm}"`)).map(([path]) => path);

    // Assert
    expect(handRolled).toEqual([]);
  });
});
