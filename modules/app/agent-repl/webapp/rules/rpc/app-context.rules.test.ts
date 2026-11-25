/**
 * EVERY MERGE TEST SCRIPTS ITS DAEMON THROUGH `scriptedAppContext`
 * (`test/rpc/app-context.ts`): none builds the client by hand.
 */
import path from "node:path";
import { describe, expect, it } from "vitest";
import { codeUnder, testDir } from "../files-under.js";

describe("scriptedAppContext", () => {
  it("is how every merge test scripts its daemon, so none builds the client by hand", () => {
    const here = path.join(testDir, "feed", "merge");
    const handRolled = [...codeUnder(here)].filter(([, code]) => code.includes("createRouterTransport")).map(([file]) => file);
    expect(handRolled).toEqual([]);
  });
});
