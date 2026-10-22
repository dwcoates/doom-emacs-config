import { readFileSync, readdirSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { storev1 } from "../../src/proto.js";
import { stubClient } from "./stub-client.js";

describe("stubClient", () => {
  it("refuses every verb no override names", () => {
    // Arrange.
    const client = stubClient({});

    // Act / Assert.
    expect(() => client.getLiveWork(create(storev1.GetLiveWorkRequestSchema, {}))).toThrow(
      "this suite did not expect that call",
    );
  });

  it("answers a verb an override names", async () => {
    // Arrange.
    const answer = create(storev1.GetLiveWorkResponseSchema, {});
    const client = stubClient({ getLiveWork: () => Promise.resolve(answer) });

    // Act.
    const got = await client.getLiveWork(create(storev1.GetLiveWorkRequestSchema, {}));

    // Assert.
    expect(got).toBe(answer);
  });

  // EVERY STORE SUITE BUILDS ITS HAND-MADE CLIENT HERE: a suite that declares
  // its own stub, or spells every verb out by hand, has drifted from the one
  // shape and fails this case.
  it("is the only hand-built store client the store suites declare", () => {
    // Arrange.
    const here = dirname(fileURLToPath(import.meta.url));
    const suites = readdirSync(here).filter((name) => name.endsWith(".test.ts") && name !== "stub-client.test.ts");

    // Act.
    const drifted = suites.filter((name) => {
      const text = readFileSync(join(here, name), "utf8");
      return /function stubClient\(/.test(text) || /openAgentSession: async \(\) => \{\s*throw new Error\("unused"\)/.test(text);
    });

    // Assert.
    expect(drifted).toEqual([]);
  });
});
