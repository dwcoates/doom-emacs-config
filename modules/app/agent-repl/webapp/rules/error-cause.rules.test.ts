/** errorCause (src/error-cause.ts) is the only spelling of a thrown value's cause in the webapp's source. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "src");

describe("errorCause", () => {
  it("is the only spelling of the cause in the webapp's source", () => {
    // Arrange
    const handRolled = /instanceof Error \? `\$\{\w+\.name\}: \$\{\w+\.message\}` : String\(/;

    // Act
    const sites = filesUnder(SRC)
      .map((file) => path.relative(SRC, file))
      .filter((file) => file !== "error-cause.ts" && handRolled.test(readFileSync(path.join(SRC, file), "utf8")));

    // Assert
    expect(sites).toEqual([]);
  });
});
