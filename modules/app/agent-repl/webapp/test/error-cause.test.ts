// The cause of a thrown value, as records and failures carry it. See
// src/error-cause.ts.
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

import { errorCause } from "../src/error-cause.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "..", "src");

/** Every TypeScript source under DIR, as paths relative to SRC. */
function sources(dir: string): string[] {
  return readdirSync(dir, { withFileTypes: true }).flatMap((entry) => {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) return sources(full);
    return entry.name.endsWith(".ts") ? [path.relative(SRC, full)] : [];
  });
}

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
    const sites = sources(SRC).filter(
      (file) => file !== "error-cause.ts" && handRolled.test(readFileSync(path.join(SRC, file), "utf8")),
    );

    // Assert
    expect(sites).toEqual([]);
  });
});
