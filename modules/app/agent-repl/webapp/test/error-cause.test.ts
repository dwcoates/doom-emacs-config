// The cause of a thrown value, as records and failures carry it. See
// src/error-cause.ts.
import { describe, expect, it } from "vitest";

import { errorCause } from "../src/error-cause.js";

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
});
