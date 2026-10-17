// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedPeerMessageSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import {
  PEER_BODY_CLASS,
  PEER_BUBBLE_CLASS,
  PEER_LABEL_CLASS,
  PEER_NAME_CLASS,
  drawFeedPeerMessage,
} from "../../../src/feed/rows/peer-message.js";
import { PEER_ARROW_CLASS } from "../../../src/feed/rows/peer-arrow.js";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import { BUBBLE_SCROLL_CLASS } from "../../../src/feed/bubble-scroll.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANT_ATTRIBUTE,
} from "../../../src/bubble/draw.js";
import { FITTING_TREE, WIDE_TREE, stagedCols, treeLineWidths, useTreeLayout } from "../../tree-layout.js";

/** A peer message from the workspace SENDER, with the given body. */
function peer(sender: string, body: string) {
  return create(FeedPeerMessageSchema, {
    from: { name: { text: sender }, sender: { case: "workspace", value: {} } },
    body,
  });
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
    expect(drawFeedPeerMessage(peer("review-bot", "hi")).getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("prompt");
  });

  it("is the peer variant", () => {
    expect(drawFeedPeerMessage(peer("review-bot", "hi")).getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("peer");
  });

  it("wears the peer hook, never the user prompt's", () => {
    const el = drawFeedPeerMessage(peer("review-bot", "hi"));
    expect([el.classList.contains(PEER_BUBBLE_CLASS), el.classList.contains("user")]).toEqual([true, false]);
  });

  it("collapses to its header alone: a zero-line cap", () => {
    expect(drawFeedPeerMessage(peer("review-bot", "hi")).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe("0");
  });

  it("draws the sender's name verbatim in the header strip", () => {
    const label = drawFeedPeerMessage(peer("review-bot", "hi")).querySelector(`.${PEER_LABEL_CLASS}`);
    expect([label?.textContent, label?.classList.contains(BUBBLE_STRIP_CLASS)]).toEqual(["review-bot", true]);
  });

  it("draws a left arrow, then the sender's name", () => {
    const label = drawFeedPeerMessage(peer("review-bot", "hi")).querySelector(`.${PEER_LABEL_CLASS}`);
    expect(
      [...(label?.children ?? [])].map((child) =>
        child.classList.contains(PEER_ARROW_CLASS) ? `arrow-${child.getAttribute("data-direction")}` : child.textContent,
      ),
    ).toEqual(["arrow-in", "review-bot"]);
  });

  it.each([
    ["an unresolved sender", { case: "unresolved" as const, value: {} }],
    ["an unnamed sender", { case: "unnamed" as const, value: {} }],
    ["a named subagent", { case: "agent" as const, value: { identity: { case: "named" as const, value: {} } } }],
    ["an unnamed subagent", { case: "agent" as const, value: { identity: { case: "identified" as const, value: {} } } }],
  ])("draws %s's name the same way", (_label, sender) => {
    const el = drawFeedPeerMessage(
      create(FeedPeerMessageSchema, { from: { name: { text: "desktop-session-9f" }, sender }, body: "hi" }),
    );
    expect(el.querySelector(`.${PEER_NAME_CLASS}`)?.textContent).toBe("desktop-session-9f");
  });

  it("refuses a message with no sender header", () => {
    expect(() => drawFeedPeerMessage(create(FeedPeerMessageSchema, { body: "hi" }))).toThrow(MalformedView);
  });

  it("carries no private toggle of its own", () => {
    const el = drawFeedPeerMessage(peer("review-bot", "hi"));
    expect(el.querySelector("ar-button, .peer-head, .peer-chevron")).toBeNull();
  });

  it("renders the markdown body through the one body pipeline", () => {
    const el = drawFeedPeerMessage(peer("review-bot", "**bold**"));
    expect(el.querySelector(`.bubble-body > .${PEER_BODY_CLASS} strong`)?.textContent).toBe("bold");
  });

  it("starts collapsed", () => {
    const scroll = drawFeedPeerMessage(peer("review-bot", "hi")).querySelector(`.${BUBBLE_SCROLL_CLASS}`);
    expect(scroll?.classList.contains(EXPANDED_CLASS)).toBe(false);
  });
});

describe("drawFeedPeerMessage: the toggle is expand.ts's", () => {
  it("expands the scroll box on a click on the label", () => {
    // Arrange
    const el = mounted(drawFeedPeerMessage(peer("review-bot", "the body")));
    const label = el.querySelector<HTMLElement>(`.${PEER_LABEL_CLASS}`);
    // Act
    label?.click();
    // Assert
    expect(el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("collapses again on a second click on the label", () => {
    // Arrange
    const el = mounted(drawFeedPeerMessage(peer("review-bot", "the body")));
    const label = el.querySelector<HTMLElement>(`.${PEER_LABEL_CLASS}`);
    // Act
    label?.click();
    label?.click();
    // Assert
    expect(el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("collapses on a click on the revealed body, as every bubble does", () => {
    // Arrange
    const el = mounted(drawFeedPeerMessage(peer("review-bot", "the body")));
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
    const el = mounted(drawFeedPeerMessage(peer("review-bot", WIDE_TREE)));
    // Assert
    const widths = treeLineWidths(el);
    expect(widths.length).toBeGreaterThan(3);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("never wraps below its max width", () => {
    // Arrange / Act
    const el = mounted(drawFeedPeerMessage(peer("review-bot", FITTING_TREE)));
    // Assert
    expect(treeLineWidths(el)).toHaveLength(3);
  });
});

describe("drawFeedPeerMessage: a re-push", () => {
  it("updates the previous draw in place", () => {
    // Arrange
    const first = drawFeedPeerMessage(peer("review-bot", "hi"));
    // Act
    const again = drawFeedPeerMessage(peer("review-bot", "hi again"), first);
    // Assert
    expect([again, again.querySelector(`.${PEER_BODY_CLASS}`)?.textContent?.trim()]).toEqual([first, "hi again"]);
  });
});
