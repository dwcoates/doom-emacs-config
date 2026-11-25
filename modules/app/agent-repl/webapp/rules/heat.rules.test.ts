/** Every token figure is colored through heatColor (src/heat.ts). */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("heatColor", () => {
  it.each(["src/footer/strip.ts", "src/feed/hover-stamp.ts"])(
    "%s colors its token figure through the shared helper",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Assert
      expect(source).toContain("heatColor(");
      expect(source).not.toContain("--token-heat-0");
    },
  );
});
