// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  TopbarWorkspaceNameLinkBadgeSchema,
  type TopbarWorkspaceNameLinkBadge,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { drawLinkBadge, drawLinkMarks, LINK_BADGE_CLASS, LINK_MARK_CLASS } from "../../src/topbar/link-badge.js";

describe("drawLinkBadge", () => {
  it.each(["open", "draft", "merged", "closed"] as const)("draws the %s arm as its word, named by data-badge", (state) => {
    // Arrange
    const badge = create(TopbarWorkspaceNameLinkBadgeSchema, { state: { case: state, value: {} } });
    // Act
    const el = drawLinkBadge(badge)!;
    // Assert
    expect([el.className, el.dataset.badge, el.textContent]).toEqual([LINK_BADGE_CLASS, state, state]);
  });

  it("draws nothing for a row served no badge", () => {
    expect(drawLinkBadge(undefined)).toBeNull();
  });

  it("draws nothing for a badge whose state this schema does not name", () => {
    expect(drawLinkBadge(create(TopbarWorkspaceNameLinkBadgeSchema, {}))).toBeNull();
  });

  it("refuses a state arm this build cannot draw", () => {
    // Arrange
    const future = { state: { case: "locked", value: {} } } as unknown as TopbarWorkspaceNameLinkBadge;
    // Act / Assert
    expect(() => drawLinkBadge(future)).toThrow(/TopbarWorkspaceNameLinkBadge.state/);
  });
});

describe("drawLinkMarks", () => {
  const drawn = (item: { owned: boolean; subscribed: boolean }) =>
    drawLinkMarks(item).map((el) => [[...el.classList], el.dataset.mark, el.textContent]);

  it.each([
    [{ owned: false, subscribed: false }, []],
    [{ owned: true, subscribed: false }, [["owned"]]],
    [{ owned: false, subscribed: true }, [["subscribed"]]],
    [{ owned: true, subscribed: true }, [["owned"], ["subscribed"]]],
  ])("draws the marks of %o, owned before subscribed", (item, want) => {
    expect(drawn(item)).toEqual(want.map(([mark]) => [["badge", LINK_MARK_CLASS], mark, mark]));
  });
});
