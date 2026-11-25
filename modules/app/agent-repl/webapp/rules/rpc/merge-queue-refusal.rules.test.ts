/**
 * THE ONE TABLE EVERY SOURCE WORDS AN UpdateMergeQueue REFUSAL FROM
 * (`src/rpc/merge-queue-refusal.ts`): no source hand-rolls its sentences.
 */
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { UPDATE_MERGE_QUEUE_SENTENCES } from "../../src/rpc/merge-queue-refusal.js";
import { codeUnder } from "../files-under.js";

const SRC = fileURLToPath(new URL("../../src", import.meta.url));

describe("updateMergeQueueRefusal", () => {
  it("is the one table every source words an UpdateMergeQueue refusal from", () => {
    const code = codeUnder(SRC);
    const users = [...code].filter(([, text]) => /UPDATE_MERGE_QUEUE_SENTENCES|updateMergeQueueRefusal/.test(text)).map(([file]) => file).sort();
    const handRolled = [...code]
      .filter(([file, text]) => file !== "rpc/merge-queue-refusal.ts" && text.includes(UPDATE_MERGE_QUEUE_SENTENCES.noSuchQueuedMerge()))
      .map(([file]) => file);
    expect([users, handRolled]).toEqual([["feed/merge/queue.ts", "rpc/merge-queue-refusal.ts", "sidebar/merge-queue-view.ts"], []]);
  });
});
