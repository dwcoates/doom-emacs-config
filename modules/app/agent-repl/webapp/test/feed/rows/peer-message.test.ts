// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedPeerMessageSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawFeedPeerMessage } from "../../../src/feed/rows/peer-message.js";
import { EXPANDED_CLASS, isCappedSection } from "../../../src/expand.js";
import { BUBBLE_SCROLL_CLASS } from "../../../src/feed/bubble-scroll.js";

/** A peer message with the given sender and body. */
function peer(sender: string, body: string) {
  return create(FeedPeerMessageSchema, { sender, body });
}

describe("drawFeedPeerMessage: the bubble", () => {
  it("is right-aligned like a prompt but wears the purple peer class, not the blue user class", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "hi"));
    // Assert.
    expect(el.classList.contains("peer")).toBe(true);
    expect(el.classList.contains("user")).toBe(false);
  });

  it("shows the sender label in the collapsed head", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "hi"));
    // Assert.
    expect(el.querySelector(".peer-label")?.textContent).toBe("agent Explore");
  });

  it("shows a chevron in the head", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "hi"));
    // Assert.
    expect(el.querySelector(".peer-chevron")).not.toBeNull();
  });
});

describe("drawFeedPeerMessage: collapsed shows no body", () => {
  it("hangs the body in the shared bubble-scroll box", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "the body"));
    // Assert.
    expect(el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)).not.toBeNull();
  });

  it("starts collapsed: the scroll box carries no expanded class", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "the body"));
    // Assert.
    const scroll = el.querySelector(`.${BUBBLE_SCROLL_CLASS}`);
    expect(scroll?.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("marks the head not-expanded for assistive tech while collapsed", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "the body"));
    // Assert.
    expect(el.querySelector(".peer-head")?.getAttribute("aria-expanded")).toBe("false");
  });
});

describe("drawFeedPeerMessage: expand reveals the body", () => {
  it("expands the scroll box when the head is clicked", () => {
    // Arrange.
    const el = drawFeedPeerMessage(peer("agent Explore", "the body"));
    const head = el.querySelector(".peer-head") as HTMLButtonElement;
    const scroll = el.querySelector(`.${BUBBLE_SCROLL_CLASS}`) as HTMLElement;
    // Act.
    head.click();
    // Assert.
    expect(scroll.classList.contains(EXPANDED_CLASS)).toBe(true);
  });

  it("collapses again on a second click (the shared toggle model)", () => {
    // Arrange.
    const el = drawFeedPeerMessage(peer("agent Explore", "the body"));
    const head = el.querySelector(".peer-head") as HTMLButtonElement;
    const scroll = el.querySelector(`.${BUBBLE_SCROLL_CLASS}`) as HTMLElement;
    // Act.
    head.click();
    head.click();
    // Assert.
    expect(scroll.classList.contains(EXPANDED_CLASS)).toBe(false);
  });

  it("marks the bubble expanded so the chevron rotates and the body is revealed", () => {
    // Arrange.
    const el = drawFeedPeerMessage(peer("agent Explore", "the body"));
    const head = el.querySelector(".peer-head") as HTMLButtonElement;
    // Act.
    head.click();
    // Assert.
    expect(el.classList.contains("peer-expanded")).toBe(true);
    expect(head.getAttribute("aria-expanded")).toBe("true");
  });
});

describe("drawFeedPeerMessage: the body reuses markdown", () => {
  it("renders the markdown body into the bubble body", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "**bold**"));
    // Assert.
    expect(el.querySelector(".bubble-body strong")?.textContent).toBe("bold");
  });
});

describe("drawFeedPeerMessage: the 50vh scroll model", () => {
  it("makes the body's box a capped section, so expand.ts caps it at 50vh and reveals scroll", () => {
    // Arrange, Act.
    const el = drawFeedPeerMessage(peer("agent Explore", "hi"));
    const scroll = el.querySelector(`.${BUBBLE_SCROLL_CLASS}`) as HTMLElement;
    // Assert. bubble-scroll is one of expand.ts's CAPPED_CLASSES, so the shared
    // .expanded rule (max-height:50vh; overflow-y:auto) governs it.
    expect(isCappedSection(scroll.classList)).toBe(true);
  });
});
