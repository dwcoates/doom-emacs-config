import { describe, expect, it } from "vitest";
import { ASK_WAITING_WORD } from "../../../src/feed/asks/ask-words.js";

describe("the words both ask cards share", () => {
  it("says an ask still waiting on the user is waiting", () => {
    expect(ASK_WAITING_WORD).toBe("waiting");
  });
});
