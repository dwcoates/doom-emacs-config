// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { ASK_HINT_ATTRIBUTE, drawAskHint } from "../../../src/feed/asks/hint.js";
import { drawFeedPermission } from "../../../src/feed/asks/permission.js";
import { drawFeedQuestion } from "../../../src/feed/asks/question.js";
import { FeedPermissionSchema, FeedQuestionSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
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

describe("both standing ask cards draw the one hint", () => {
  it.each([
    [
      "permission",
      () =>
        drawFeedPermission(
          create(FeedPermissionSchema, {
            headline: { text: "Claude wants to run ls" },
            arguments: { lines: [] },
            state: { case: "open", value: {} },
          }),
          askHarness().rc,
        ),
    ],
    [
      "question",
      () =>
        drawFeedQuestion(
          create(FeedQuestionSchema, {
            questions: [
              {
                header: { text: "h" },
                text: { text: "q?" },
                options: { case: "singleSelect", value: { options: [{ label: { text: "a" } }] } },
              },
            ],
            state: { case: "open", value: { active: 0, drafts: [{}] } },
          }),
          askHarness().rc,
        ),
    ],
  ] as const)("the %s card's hint is the shared builder's element", (_kind, draw) => {
    // A hand-rolled hint that drifted from the builder fails here.
    const hint = draw().querySelector(`[${ASK_HINT_ATTRIBUTE}]`);
    const expected = drawAskHint(hint?.textContent ?? "");
    expect(hint?.outerHTML).toBe(expected.outerHTML);
  });
});
