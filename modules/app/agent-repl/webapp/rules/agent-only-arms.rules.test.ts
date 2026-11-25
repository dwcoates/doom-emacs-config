/**
 * The suites that word refusal arms off the schema take the agent-only arms
 * out through `userRefusalArms` (test/agent-only-arms.ts).
 */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

describe("the suites that word refusal arms off the schema", () => {
  it.each([
    "test/composer/composer.test.ts",
    "test/integration/refusals.integration.test.ts",
  ])("%s takes the agent-only arms out through userRefusalArms", (file) => {
    expect(readFileSync(new URL(`../${file}`, import.meta.url), "utf8")).toContain("userRefusalArms(");
  });
});
