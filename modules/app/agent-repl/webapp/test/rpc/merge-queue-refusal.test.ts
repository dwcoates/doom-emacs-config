import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { UPDATE_MERGE_QUEUE_SENTENCES, updateMergeQueueRefusal } from "../../src/rpc/merge-queue-refusal.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { codeUnder } from "../files-under.js";

const SRC = fileURLToPath(new URL("../../src", import.meta.url));

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

  it("is the one table every source words an UpdateMergeQueue refusal from", () => {
    const code = codeUnder(SRC);
    const users = [...code].filter(([, text]) => /UPDATE_MERGE_QUEUE_SENTENCES|updateMergeQueueRefusal/.test(text)).map(([file]) => file).sort();
    const handRolled = [...code]
      .filter(([file, text]) => file !== "rpc/merge-queue-refusal.ts" && text.includes(UPDATE_MERGE_QUEUE_SENTENCES.noSuchQueuedMerge()))
      .map(([file]) => file);
    expect([users, handRolled]).toEqual([["feed/merge/queue.ts", "rpc/merge-queue-refusal.ts", "sidebar/merge-queue-view.ts"], []]);
  });
});
