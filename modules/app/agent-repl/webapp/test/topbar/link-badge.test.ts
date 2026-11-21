// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarTitleLinkBadgeSchema,
  type TopbarTitleLinkBadge,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { drawLinkBadge, LINK_BADGE_CLASS } from "../../src/topbar/link-badge.js";

describe("drawLinkBadge", () => {
  it.each(["open", "draft", "merged", "closed"] as const)("draws the %s arm as its word, named by data-badge", (state) => {
    // Arrange
    const badge = create(TopbarTitleLinkBadgeSchema, { state: { case: state, value: {} } });
    // Act
    const el = drawLinkBadge(badge)!;
    // Assert
    expect([el.className, el.dataset.badge, el.textContent]).toEqual([LINK_BADGE_CLASS, state, state]);
  });

  it("draws nothing for a row served no badge", () => {
    expect(drawLinkBadge(undefined)).toBeNull();
  });

  it("draws nothing for a badge whose state this schema does not name", () => {
    expect(drawLinkBadge(create(TopbarTitleLinkBadgeSchema, {}))).toBeNull();
  });

  it("refuses a state arm this build cannot draw", () => {
    // Arrange
    const future = { state: { case: "locked", value: {} } } as unknown as TopbarTitleLinkBadge;
    // Act / Assert
    expect(() => drawLinkBadge(future)).toThrow(/TopbarTitleLinkBadge.state/);
  });
});
