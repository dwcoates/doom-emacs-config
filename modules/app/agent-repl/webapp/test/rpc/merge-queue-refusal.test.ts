import { describe, expect, it } from "vitest";
import {
  discardMergeRefusal,
  requeueMergeRefusal,
  updateMergeQueueRefusal,
} from "../../src/rpc/merge-queue-refusal.js";
import { MalformedView } from "../../src/rpc/malformed.js";

describe("updateMergeQueueRefusal", () => {
  it.each([
    { arm: "noSuchQueuedMerge", want: "this merge is no longer waiting on the queue" },
    { arm: "notPaused", want: "the queue is no longer paused" },
    { arm: "unknownRepository", want: "the daemon no longer has that repository" },
  ])("words $arm", ({ arm, want }) => {
    expect(updateMergeQueueRefusal({ case: arm, value: {} })).toBe(want);
  });

  it("refuses an arm this build has no sentence for as a malformed view", () => {
    expect(() => updateMergeQueueRefusal({ case: "aNewerArm", value: {} })).toThrow(MalformedView);
  });
});

describe("requeueMergeRefusal", () => {
  it.each([
    { arm: "unknownMerge", value: {}, want: "this merge no longer holds a request" },
    { arm: "stillQueued", value: {}, want: "this merge is back in line already" },
    { arm: "branchGone", value: { branch: "fix" }, want: "its branch fix is gone; discard it" },
    { arm: "alreadyLanded", value: {}, want: "this merge has landed" },
    { arm: "versionIncrementRequired", value: {}, want: "this merge needs a version increment; requeue it from the command line with -version-part" },
    { arm: "versionIncrementUnexpected", value: {}, want: "this merge carries a version increment it does not need" },
    { arm: "versionAlreadyRecorded", value: {}, want: "this merge keeps the version increment it was asked for" },
    { arm: "placementBehindNotInLine", value: {}, want: "the merge it was placed behind is not in line" },
    { arm: "workspaceClosed", value: {}, want: "this merge's workspace is closed" },
    { arm: "notInCatalog", value: {}, want: "that model is not in this login's catalog" },
    { arm: "coldGated", value: {}, want: "this merge's workspace is parked at its cold gate; answer the gate first" },
  ])("words $arm", ({ arm, value, want }) => {
    expect(requeueMergeRefusal({ case: arm, value })).toBe(want);
  });

  it("refuses an arm this build has no sentence for as a malformed view", () => {
    expect(() => requeueMergeRefusal({ case: "aNewerArm", value: {} })).toThrow(MalformedView);
  });
});

describe("discardMergeRefusal", () => {
  it.each([
    { arm: "unknownMerge", want: "this merge no longer holds a request" },
    { arm: "stillQueued", want: "this merge is back in line; take it off the queue first" },
  ])("words $arm", ({ arm, want }) => {
    expect(discardMergeRefusal({ case: arm, value: {} })).toBe(want);
  });

  it("refuses an arm this build has no sentence for as a malformed view", () => {
    expect(() => discardMergeRefusal({ case: "aNewerArm", value: {} })).toThrow(MalformedView);
  });
});
