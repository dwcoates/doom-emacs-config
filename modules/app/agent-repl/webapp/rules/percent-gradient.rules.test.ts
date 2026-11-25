/** Every percent color goes through percentGradientColor (src/percent-gradient.ts), never an interpolation of its own. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("percentGradientColor", () => {
  it.each(["src/panels/context-colors.ts", "src/pressure-color.ts"])(
    "%s colors its percent through the shared helper, not its own interpolation",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Assert
      expect(source).toContain("percentGradientColor(");
      expect(source).not.toMatch(/function lerp\b/);
    },
  );
});
