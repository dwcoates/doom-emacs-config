import { describe, expect, it } from "vitest";
import { readMentionTargets } from "../../src/feed/workspace-mentions.js";
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
});
