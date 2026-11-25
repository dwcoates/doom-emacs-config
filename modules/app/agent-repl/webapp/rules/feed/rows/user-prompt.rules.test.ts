/**
 * Every prompt that draws a person's words does so through userPromptLook
 * (src/feed/rows/user-prompt.ts), never a hand-rolled copy control or cap.
 */
import { describe, expect, it } from "vitest";
import { codeOf } from "../../../test/source-text.js";
import userPromptSource from "../../../src/feed/rows/user-prompt.ts?raw";
import heldPromptSource from "../../../src/held/held-prompt.ts?raw";
import ingressPromptSource from "../../../src/held/ingress-prompt.ts?raw";

describe("userPromptLook: the one source of the user prompt's look", () => {
  it.each([
    ["the feed's user prompt", userPromptSource],
    ["a held prompt", heldPromptSource],
    ["an ingress prompt", ingressPromptSource],
  ])("is how %s draws a person's words, never a hand-rolled copy control or cap", (_name, source) => {
    // A held prompt an agent sent folds at the agent-to-agent cap, so that
    // one named constant is the only cap besides the user prompt's it may set.
    // Arrange / Act
    const code = codeOf(source);
    // Assert
    expect([
      /\buserPromptLook\(/.test(code),
      /\bdrawPromptCopyControl\(/.test(code.replace(/export function userPromptLook[\s\S]*?\n}\n/, "")),
      [...code.matchAll(/capLines:\s*(\w+)/g)].map((m) => m[1]).filter((cap) => cap !== "USER_PROMPT_CAP_LINES" && cap !== "AGENT_PROMPT_CAP_LINES" && cap !== "typeof"),
    ]).toEqual([true, false, []]);
  });
});
