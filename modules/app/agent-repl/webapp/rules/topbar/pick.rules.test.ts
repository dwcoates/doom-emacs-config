/**
 * THE TOPBAR'S SELECTORS SHARE THE ONE PICK PATH: each selector's source sends
 * through `sendPick` and never calls an rpc itself.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const here = path.dirname(fileURLToPath(import.meta.url));

describe("the selectors share the one pick path", () => {
  it.each(["src/topbar/model.ts", "src/topbar/effort.ts", "src/topbar/permission-mode.ts", "src/topbar/queue-bypass.ts"])(
    "%s sends through sendPick and never calls an rpc itself",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", "..", file), "utf8");
      // Act / Assert
      expect([source.includes("sendPick("), source.includes("callUnary(")]).toEqual([true, false]);
    },
  );
});
