// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedPeerMessageSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  PEER_BODY_CLASS,
  PEER_BUBBLE_CLASS,
  PEER_LABEL_CLASS,
  drawFeedPeerMessage,
} from "../../../src/feed/rows/peer-message.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import { BUBBLE_SCROLL_CLASS } from "../../../src/feed/bubble-scroll.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANT_ATTRIBUTE,
} from "../../../src/bubble/draw.js";
import { FITTING_TREE, WIDE_TREE, stagedCols, treeLineWidths, useTreeLayout } from "../../tree-layout.js";

/** A peer message with the given sender and body. */
function peer(sender: string, body: string) {
  return create(FeedPeerMessageSchema, { sender, body });
}

/** EL attached under its own column inside a feed host armed as feed.ts arms it. */
function mounted(el: HTMLElement): HTMLElement {
  const host = document.createElement("div");
  installClickExpand(host, () => "");
  const column = document.createElement("div");
  column.append(el);
  host.append(column);
  document.body.append(host);
  return el;
}

describe("drawFeedPeerMessage: its spec", () => {
  it("is a prompt-role bubble", () => {
    expect(drawFeedPeerMessage(peer("agent Explore", "hi")).getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("prompt");
  });

  it("is the peer variant", () => {
    expect(drawFeedPeerMessage(peer("agent Explore", "hi")).getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("peer");
  });

  it("wears the peer hook, never the user prompt's", () => {
    const el = drawFeedPeerMessage(peer("agent Explore", "hi"));
    expect([el.classList.contains(PEER_BUBBLE_CLASS), el.classList.contains("user")]).toEqual([true, false]);
  });

  it("collapses to its header alone: a zero-line cap", () => {
    expect(drawFeedPeerMessage(peer("agent Explore", "hi")).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe("0");
  });

  it("draws the sender label verbatim in the header strip", () => {
    const label = drawFeedPeerMessage(peer("agent Explore", "hi")).querySelector(`.${PEER_LABEL_CLASS}`);
    expect([label?.textContent, label?.classList.contains(BUBBLE_STRIP_CLASS)]).toEqual(["agent Explore", true]);
  });

  it("carries no private toggle of its own", () => {
    const el = drawFeedPeerMessage(peer("agent Explore", "hi"));
    expect(el.querySelector("button, .peer-head, .peer-chevron")).toBeNull();
  });

  it("renders the markdown body through the one body pipeline", () => {
    const el = drawFeedPeerMessage(peer("agent Explore", "**bold**"));
    expect(el.querySelector(`.bubble-body > .${PEER_BODY_CLASS} strong`)?.textContent).toBe("bold");
  });

  it("starts collapsed", () => {
    const scroll = drawFeedPeerMessage(peer("agent Explore", "hi")).querySelector(`.${BUBBLE_SCROLL_CLASS}`);
    expect(scroll?.classList.contains(EXPANDED_CLASS)).toBe(false);
  });
});

describe("drawFeedPeerMessage: the toggle is expand.ts's", () => {
  it("expands the scroll box on a click on the label", () => {
    // Arrange
    const el = mounted(drawFeedPeerMessage(peer("agent Explore", "the body")));
    const label = el.querySelector<HTMLElement>(`.${PEER_LABEL_CLASS}`);
    // Act
    label?.click();
    // Assert
    expect(el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("collapses again on a second click on the label", () => {
    // Arrange
    const el = mounted(drawFeedPeerMessage(peer("agent Explore", "the body")));
    const label = el.querySelector<HTMLElement>(`.${PEER_LABEL_CLASS}`);
    // Act
    label?.click();
    label?.click();
    // Assert
    expect(el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("collapses on a click on the revealed body, as every bubble does", () => {
    // Arrange
    const el = mounted(drawFeedPeerMessage(peer("agent Explore", "the body")));
    el.querySelector<HTMLElement>(`.${PEER_LABEL_CLASS}`)?.click();
    // Act
    el.querySelector<HTMLElement>(`.${PEER_BODY_CLASS}`)?.click();
    // Assert
    expect(el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.contains(EXPANDED_CLASS)).toBe(false);
  });
});

describe("drawFeedPeerMessage: a tree in the body", () => {
  const staged = useTreeLayout();

  it("wraps at the peer bubble's own cap", () => {
    // Arrange / Act
    const el = mounted(drawFeedPeerMessage(peer("agent Explore", WIDE_TREE)));
    // Assert
    const widths = treeLineWidths(el);
    expect(widths.length).toBeGreaterThan(3);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("never wraps below its max width", () => {
    // Arrange / Act
    const el = mounted(drawFeedPeerMessage(peer("agent Explore", FITTING_TREE)));
    // Assert
    expect(treeLineWidths(el)).toHaveLength(3);
  });
});

describe("drawFeedPeerMessage: a re-push", () => {
  it("updates the previous draw in place", () => {
    // Arrange
    const first = drawFeedPeerMessage(peer("agent Explore", "hi"));
    // Act
    const again = drawFeedPeerMessage(peer("agent Explore", "hi again"), first);
    // Assert
    expect([again, again.querySelector(`.${PEER_BODY_CLASS}`)?.textContent?.trim()]).toEqual([first, "hi again"]);
  });
});
