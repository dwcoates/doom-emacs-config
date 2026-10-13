import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { ASK_WAITING_WORD } from "../../../src/feed/asks/ask-words.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src");

describe("the words both ask cards share", () => {
  it("says an ask still waiting on the user is waiting", () => {
    expect(ASK_WAITING_WORD).toBe("waiting");
  });

  it.each(["feed/asks/permission.ts", "feed/asks/question.ts"])("is how %s badges a waiting ask", (file) => {
    const source = readFileSync(path.join(SRC, file), "utf8");
    // No second spelling of the word handed to a badge.
    expect([source.includes("drawToolCardBadge(ASK_WAITING_WORD"), /drawToolCardBadge\(\s*["'`]waiting/.test(source)]).toEqual([
      true,
      false,
    ]);
  });
});
