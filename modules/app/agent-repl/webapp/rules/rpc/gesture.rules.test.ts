/**
 * A REFUSED ARM IS NOTED ONLY THROUGH `sendGesture` (`src/rpc/gesture.ts`),
 * but for the chat search that reads its success.
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "../files-under.js";

describe("the gesture sites", () => {
  it("note a refused arm only through sendGesture, but for the chat search that reads its success", () => {
    const src = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "../../src");
    const noting = filesUnder(src, [".ts", ".tsx"])
      .filter((file) => /noteRefusedArm\(/.test(readFileSync(file, "utf8")))
      .map((file) => path.relative(src, file))
      .sort();
    expect(noting).toEqual(["rpc/gesture.ts", "rpc/refusal.ts", "sidebar/chat-search.ts"]);
  });
});
