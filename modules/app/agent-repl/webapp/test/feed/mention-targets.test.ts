import { readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";
import { readMentionTargets } from "../../src/feed/workspace-mentions.js";
import { filesUnder } from "../files-under.js";
import { closedTarget, mentionTargets, openTarget } from "./mention-targets.js";

describe("mention-targets", () => {
  it("builds an open target with its workspace id and directory derived from its name", () => {
    expect(openTarget("alpha")).toEqual({
      name: { text: "alpha" },
      workspace: { id: "ws-alpha", dir: "/w/alpha" },
      standing: { case: "open", value: {} },
    });
  });

  it("builds a closed target identical to the open one but for its standing", () => {
    expect(closedTarget("gone", "id", "/d")).toEqual({ ...openTarget("gone", "id", "/d"), standing: { case: "closed", value: {} } });
  });

  it("builds a set the page reads without refusing it", () => {
    // ACT
    const read = readMentionTargets(mentionTargets(openTarget("alpha"), closedTarget("gone")));
    // ASSERT
    expect([...read.byId.values()].map((t) => `${t.workspace.id}:${t.standing}`)).toEqual(["ws-alpha:open", "ws-gone:closed"]);
  });

  it("is the only way a test builds served targets", () => {
    // ARRANGE: the malformed table in workspace-mentions.test.ts builds raw
    // targets on purpose, since a malformed target is what it tests.
    const allowed = new Set(["feed/mention-targets.ts", "feed/workspace-mentions.test.ts"]);
    const root = join(__dirname, "..");
    // Spelled in two pieces so this file does not match its own search.
    const schema = ["WorkspaceMentionTargets", "Schema"].join("");
    // ACT
    const handRolled = filesUnder(root, [".ts", ".tsx"])
      .map((full) => relative(root, full))
      .filter((f) => !allowed.has(f) && readFileSync(join(root, f), "utf8").includes(schema));
    // ASSERT
    expect(handRolled).toEqual([]);
  });
});
