/**
 * EVERY SLOT PAINTER FINDS ITS SLOTS THROUGH `rowSlots`: their sources read
 * for the call.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("rowSlots", () => {
  it("is the read every slot painter finds its slots by", () => {
    // Arrange
    const source = (file: string): string => readFileSync(join(__dirname, "..", "..", "src", "sidebar", file), "utf8");

    // Act / Assert
    for (const file of ["tab-numbers.ts", "recency-gutters.ts"]) {
      expect(source(file)).toContain("rowSlots(");
      expect(source(file)).not.toContain('closest("[data-roster-row]")');
    }
  });
});
