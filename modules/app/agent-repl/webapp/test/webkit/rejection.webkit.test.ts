/**
 * The hook-side rejection capture (rejection.ts). The one shape every webkit
 * suite records a refusal through is a rule: rules/webkit/rejection.rules.test.ts.
 */
import { beforeAll, describe, expect, it } from "vitest";
import { rejectionOf } from "./rejection";

describe("rejectionOf", () => {
  const thrown = new Error("refused");
  let rejected: unknown;
  let resolved: unknown;

  beforeAll(async () => {
    rejected = await rejectionOf(Promise.reject(thrown));
    resolved = await rejectionOf(Promise.resolve("fine"));
  });

  it("answers the error a rejected promise carried", () => {
    // Arrange + Act: the hook's rejected capture. Assert
    expect(rejected).toBe(thrown);
  });

  it("answers null for a promise that resolved", () => {
    // Arrange + Act: the hook's resolved capture. Assert
    expect(resolved).toBeNull();
  });
});
