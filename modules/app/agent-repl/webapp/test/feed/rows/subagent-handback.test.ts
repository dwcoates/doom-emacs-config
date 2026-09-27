// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedSubagentHandbackBadgeLabelSchema,
  FeedSubagentHandbackBadgeSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  SUBAGENT_HANDBACK_BADGE_CLASS,
  drawFeedSubagentHandback,
} from "../../../src/feed/rows/subagent-handback.js";
import { BUBBLE_ROLE_ATTRIBUTE } from "../../../src/bubble/draw.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { CAPPED_SELECTOR } from "../../../src/expand.js";

/** A hand-back badge carrying LABEL. */
function badge(label: string) {
  return create(FeedSubagentHandbackBadgeSchema, {
    label: create(FeedSubagentHandbackBadgeLabelSchema, { text: label }),
  });
}

describe("drawFeedSubagentHandback: its spec", () => {
  it("draws the daemon's label verbatim", () => {
    expect(drawFeedSubagentHandback(badge("agent Explore reported back")).textContent).toBe(
      "agent Explore reported back",
    );
  });

  it("wears the shared badge pill and its own hook", () => {
    const el = drawFeedSubagentHandback(badge("agent Explore reported back"));
    expect([el.classList.contains("badge"), el.classList.contains(SUBAGENT_HANDBACK_BADGE_CLASS)]).toEqual([
      true,
      true,
    ]);
  });

  it("has no body: the label is its only content", () => {
    expect(drawFeedSubagentHandback(badge("agent Explore reported back")).children.length).toBe(0);
  });

  it("is not a bubble", () => {
    expect(drawFeedSubagentHandback(badge("agent Explore reported back")).hasAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe(
      false,
    );
  });

  it("is not a card", () => {
    const el = drawFeedSubagentHandback(badge("agent Explore reported back"));
    expect(el.matches(".tool-card")).toBe(false);
  });

  it("is not expandable", () => {
    expect(drawFeedSubagentHandback(badge("agent Explore reported back")).matches(CAPPED_SELECTOR)).toBe(false);
  });

  it("refuses a badge with no label rather than drawing an empty pill", () => {
    expect(() => drawFeedSubagentHandback(create(FeedSubagentHandbackBadgeSchema, {}))).toThrow(MalformedView);
  });
});
