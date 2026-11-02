// The cause of a thrown value, as records and failures carry it. See
// src/error-cause.ts.
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { errorCause } from "../src/error-cause.js";
import { filesUnder } from "./files-under.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "src");

describe("errorCause", () => {
  const cases: { name: string; err: unknown; want: string }[] = [
    { name: "an Error is its name and message", err: new Error("boom"), want: "Error: boom" },
    { name: "a subclass keeps its own name", err: new TypeError("no such page"), want: "TypeError: no such page" },
    { name: "a thrown string is itself", err: "plain", want: "plain" },
    { name: "any other value is its string", err: 42, want: "42" },
    { name: "undefined is its string", err: undefined, want: "undefined" },
  ];

  it.each(cases)("$name", ({ err, want }) => {
    // Act
    const got = errorCause(err);

    // Assert
    expect(got).toBe(want);
  });

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
