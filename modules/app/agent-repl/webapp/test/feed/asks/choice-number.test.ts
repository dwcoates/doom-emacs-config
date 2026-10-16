// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedPermissionSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { choiceNumeral, drawChoiceNumeral } from "../../../src/feed/asks/choice-number.js";
import { drawFeedPermission } from "../../../src/feed/asks/permission.js";
import { drawFeedQuestionOption } from "../../../src/feed/asks/question.js";
import { FeedQuestionOptionSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { askHarness } from "./harness.js";

describe("a choice's numeral", () => {
  it("is its number and a dot", () => {
    expect([choiceNumeral(1), choiceNumeral(12)]).toEqual(["1.", "12."]);
  });

  it("is a span of the named class carrying its number on the named attribute", () => {
    const el = drawChoiceNumeral(3, "x-number", "data-x-number");
    expect([el.tagName, el.className, el.getAttribute("data-x-number"), el.textContent]).toEqual([
      "SPAN",
      "x-number",
      "3",
      "3.",
    ]);
  });

  it("is drawn the same way by the question card and the permission card", () => {
    const option = drawFeedQuestionOption(
      create(FeedQuestionOptionSchema, { label: { text: "a" } }),
      1,
      new Set(),
      "FeedQuestionOption",
    );
    const card = drawFeedPermission(
      create(FeedPermissionSchema, {
        headline: { text: "Claude wants to run ls" },
        arguments: { lines: [] },
        state: { case: "open", value: {} },
      }),
      askHarness().rc,
    );
    const questionNumeral = option.querySelector(".q-opt-number");
    const permissionNumeral = card.querySelector('[data-permission="allowOnce"] .perm-number');
    expect([questionNumeral?.tagName, questionNumeral?.textContent]).toEqual(["SPAN", choiceNumeral(1)]);
    expect([permissionNumeral?.tagName, permissionNumeral?.textContent]).toEqual(["SPAN", choiceNumeral(1)]);
  });
});
