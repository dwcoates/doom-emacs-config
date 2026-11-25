/** Every queue a reader drags places its drop through src/merge-queue-move.ts. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

describe("every queue a reader drags places its drop through this module", () => {
  const src = path.join(path.dirname(fileURLToPath(import.meta.url)), "../src");

  it("builds a moveTo action nowhere else", () => {
    // Act
    const builders = filesUnder(src).filter((file) => /case:\s*"moveTo"/.test(readFileSync(file, "utf8")));

    // Assert
    expect(builders.map((file) => path.relative(src, file))).toEqual(["merge-queue-move.ts"]);
  });

  it("is the drag of both the sidebar's Merge Queue and the feed's queue tab", () => {
    // Act
    const users = ["sidebar/merge-queue-view.ts", "feed/merge/queue.ts"].map((file) =>
      /import \{[^}]*\bwaitingMergeDrag\b[^}]*\} from "[./]+merge-queue-move\.js"/.test(readFileSync(path.join(src, file), "utf8")),
    );

    // Assert
    expect(users).toEqual([true, true]);
  });
});
