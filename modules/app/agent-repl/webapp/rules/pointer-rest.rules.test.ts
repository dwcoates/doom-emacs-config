/** Every surface that suspends on leaving judges the return through createPointerRest (src/pointer-rest.ts). */
import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

describe("every surface that suspends on leaving", () => {
  it.each(["../src/sidebar/transients.ts", "../src/feed/hover-stamp.ts"])(
    "%s judges the user's return through the one latch, not its own",
    (path) => {
      // Arrange
      const source = readFileSync(fileURLToPath(new URL(path, import.meta.url)), "utf8");
      // Act / Assert
      expect([source.includes("createPointerRest()"), /suspendedAt|lastPoint/.test(source)]).toEqual([true, false]);
    },
  );
});
