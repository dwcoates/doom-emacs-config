/**
 * THE ARRANGEMENT'S RUN STORES SHARE THE KEYED STORE (src/feed/keyed-store.ts):
 * a rule over their sources.
 */
import { describe, expect, it } from "vitest";
import toolGroupSource from "../../src/feed/tool-group.ts?raw";
import responseRunSource from "../../src/feed/response-run.ts?raw";

describe("the arrangement's run stores share the keyed store", () => {
  it.each([
    ["tool-group.ts", toolGroupSource],
    ["response-run.ts", responseRunSource],
  ])("%s keeps its runs in createKeyedStore", (_name, source) => {
    expect(source).toContain("createKeyedStore<");
    expect(source).not.toMatch(/new Map<string, [A-Z]\w*>\(\);\s*let seen/);
  });
});
