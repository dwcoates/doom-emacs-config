/**
 * `userRequester` IS THE ONE PLACE A WEBAPP SOURCE NAMES THE USER AS ITS ASKER
 * (`src/rpc/requester.ts`).
 */
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { codeUnder } from "../files-under.js";

const SRC = fileURLToPath(new URL("../../src", import.meta.url));

describe("userRequester", () => {
  it("is the one place a webapp source names the user as its asker", () => {
    const handRolled = [...codeUnder(SRC)]
      .filter(([file, code]) => file !== "rpc/requester.ts" && code.includes('asker: { case: "user"'))
      .map(([file]) => file);
    expect(handRolled).toEqual([]);
  });
});
