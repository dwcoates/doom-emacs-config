// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedMergeHookScriptExitSchema,
  FeedMergeTabHookScriptSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedMergeHookScriptExit,
  drawFeedMergeTabHookScript,
} from "../../../src/feed/merge/hook-tabs.js";
import { oneofArms } from "../../arms.js";

const PATH = "FeedMergeTab.kind.hook_script";

/** A script hook's tab message, with EDITS. */
const scriptTab = (edits: Record<string, unknown> = {}) =>
  create(FeedMergeTabHookScriptSchema, {
    path: { text: "bin/deploy-landing.sh" },
    log: { path: "/state/merge-logs/0011aabb-deploy-1.log" },
    tail: [],
    ...edits,
  });

describe("drawFeedMergeTabHookScript", () => {
  it("draws the script's path verbatim", () => {
    const el = drawFeedMergeTabHookScript(scriptTab(), PATH);
    expect(el.querySelector(".merge-hook-script-path")?.textContent).toBe("bin/deploy-landing.sh");
  });

  it("draws the tail lines oldest first, one per line", () => {
    const el = drawFeedMergeTabHookScript(scriptTab({ tail: [{ text: "building" }, { text: "installed" }] }), PATH);
    expect(el.querySelector(".merge-hook-script-tail")?.textContent).toBe("building\ninstalled");
  });

  it("draws no tail before the script writes", () => {
    const el = drawFeedMergeTabHookScript(scriptTab(), PATH);
    expect(el.querySelector(".merge-hook-script-tail")).toBeNull();
  });

  it("draws no exit while the script runs", () => {
    const el = drawFeedMergeTabHookScript(scriptTab(), PATH);
    expect(el.querySelector(".merge-hook-script-exit")).toBeNull();
  });

  it("draws the exit once the script ended", () => {
    const el = drawFeedMergeTabHookScript(
      scriptTab({ exit: { how: { case: "exited", value: { status: 0 } } } }),
      PATH,
    );
    expect(el.querySelector(".merge-hook-script-exit")?.textContent).toBe("exited 0");
  });

  it("draws the log's path", () => {
    const el = drawFeedMergeTabHookScript(scriptTab(), PATH);
    expect(el.querySelector(".merge-hook-script-log")?.textContent).toBe("log /state/merge-logs/0011aabb-deploy-1.log");
  });

  it("draws no log line when the daemon named none", () => {
    const el = drawFeedMergeTabHookScript(scriptTab({ log: undefined }), PATH);
    expect(el.querySelector(".merge-hook-script-log")).toBeNull();
  });
});

describe("drawFeedMergeHookScriptExit", () => {
  const ARM_VALUES: Readonly<Record<string, { value: object; words: string }>> = {
    exited: { value: { status: 3 }, words: "exited 3" },
    timedOut: { value: { timeoutSeconds: 60 }, words: "timed out after 60s" },
  };

  it("has words for every arm the contract declares", () => {
    expect(Object.keys(ARM_VALUES).sort()).toEqual([...oneofArms(FeedMergeHookScriptExitSchema, "how")].sort());
  });

  it.each(Object.entries(ARM_VALUES))("draws the %s arm and stamps it", (arm, { value, words }) => {
    const el = drawFeedMergeHookScriptExit(
      create(FeedMergeHookScriptExitSchema, { how: { case: arm, value } } as never),
      `${PATH}.exit`,
    );
    expect(el.textContent).toBe(words);
    expect(el.getAttribute("data-hook-exit")).toBe(arm);
  });

  it("refuses an exit with no arm", () => {
    expect(() => drawFeedMergeHookScriptExit(create(FeedMergeHookScriptExitSchema, {}), `${PATH}.exit`)).toThrow(MalformedView);
  });
});
