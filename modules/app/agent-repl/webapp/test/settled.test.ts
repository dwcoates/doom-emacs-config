// The helper tests ask whether a promise has settled. See test/settled.ts.
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import path from "node:path";

import { settledByNextTask } from "./settled.js";

describe("settledByNextTask", () => {
  it.each([
    { name: "resolved", promise: () => Promise.resolve(1), settled: true },
    { name: "rejected", promise: () => Promise.reject(new Error("no")), settled: true },
    { name: "resolved after a chain of microtasks", promise: () => Promise.resolve().then(() => undefined).then(() => 2), settled: true },
    { name: "never settling", promise: () => new Promise<never>(() => undefined), settled: false },
  ])("answers $settled for a promise $name", async ({ promise, settled }) => {
    // Act
    const answer = await settledByNextTask(promise());

    // Assert
    expect(answer).toBe(settled);
  });

  it("is how every never-settles test asks, so no suite hand-rolls the check", () => {
    // Arrange
    const users = ["page-teardown.test.ts", "rpc/unary.test.ts", "rpc/client-log-sink.test.ts"];

    // Act
    const handRolled = users.filter((file) => readFileSync(path.join(__dirname, file), "utf8").includes("let settled"));

    // Assert
    expect(handRolled).toEqual([]);
  });
});
