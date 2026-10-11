// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ASK_HINT_ATTRIBUTE, drawAskHint } from "../../../src/feed/asks/hint.js";
import { drawFeedPermission } from "../../../src/feed/asks/permission.js";
import { FeedPermissionSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { askHarness } from "./harness.js";

describe("drawAskHint", () => {
  it("draws the hint line saying its text verbatim", () => {
    const hint = drawAskHint("type it in Emacs");
    expect([hint.localName, hint.className, hint.hasAttribute(ASK_HINT_ATTRIBUTE), hint.textContent]).toEqual([
      "div",
      "ask-hint",
      true,
      "type it in Emacs",
    ]);
  });
});

describe("the permission card draws the one hint", () => {
  it("draws the shared builder's element", () => {
    // A hand-rolled hint that drifted from the builder fails here.
    const card = drawFeedPermission(
      create(FeedPermissionSchema, {
        headline: { text: "Claude wants to run ls" },
        arguments: { lines: [] },
        state: { case: "open", value: {} },
      }),
      askHarness().rc,
    );
    const hint = card.querySelector(`[${ASK_HINT_ATTRIBUTE}]`);
    const expected = drawAskHint(hint?.textContent ?? "");
    expect(hint?.outerHTML).toBe(expected.outerHTML);
  });
});
