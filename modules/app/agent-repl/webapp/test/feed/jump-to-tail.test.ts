// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import {
  JUMP_TO_TAIL_CLASS,
  JUMP_TO_TAIL_SHOWN_ATTRIBUTE,
  fartherThanAPage,
  mountJumpToTail,
} from "../../src/feed/jump-to-tail.js";
import { TailFollow, type ReanchorBox } from "../../src/scroll.js";
import { tailTriggers } from "../tail-triggers.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";

describe("fartherThanAPage", () => {
  it.each<[string, ReanchorBox, boolean]>([
    ["at the tail", { scrollTop: 700, scrollHeight: 1000, clientHeight: 300 }, false],
    ["exactly one page above the tail", { scrollTop: 400, scrollHeight: 1000, clientHeight: 300 }, false],
    ["one pixel more than a page above the tail", { scrollTop: 399, scrollHeight: 1000, clientHeight: 300 }, true],
    ["at the top of a long feed", { scrollTop: 0, scrollHeight: 1000, clientHeight: 300 }, true],
    ["on content shorter than two pages", { scrollTop: 0, scrollHeight: 500, clientHeight: 300 }, false],
  ])("is %s: %s", (_name, box, expected) => {
    // Arrange + Act + Assert
    expect(fartherThanAPage(box)).toBe(expected);
  });
});

describe("mountJumpToTail", () => {
  let uninstall: (() => void) | null = null;
  afterEach(() => {
    uninstall?.();
    uninstall = null;
  });

  /** A box at TOP over 1000px of content in a 300px viewport, its tail owner, and the control's slot. */
  function mounted(top: number) {
    const box: ReanchorBox = { scrollTop: top, scrollHeight: 1000, clientHeight: 300 };
    const tail = new TailFollow(box);
    const triggers = tailTriggers(tail);
    const slot = document.createElement("div");
    document.body.append(slot);
    uninstall = mountJumpToTail(slot, tail);
    const control = slot.querySelector<HTMLElement>(`.${JUMP_TO_TAIL_CLASS}`);
    if (control === null) throw new Error("the control was not drawn");
    return {
      box,
      tail,
      slot,
      control,
      /** The reader scrolls the box to TOP. */
      scrollTo: (to: number) => {
        box.scrollTop = to;
        triggers.gesture();
      },
      resize: triggers.resize,
    };
  }

  const shown = (control: HTMLElement): boolean => control.hasAttribute(JUMP_TO_TAIL_SHOWN_ATTRIBUTE);

  it("draws the control hidden and out of the tab order", () => {
    // Arrange + Act
    const m = mounted(0);
    // Assert
    expect([shown(m.control), m.control.tabIndex, m.control.getAttribute("aria-hidden")]).toEqual([false, -1, "true"]);
  });

  it("labels the control for assistive technology", () => {
    // Arrange + Act
    const m = mounted(0);
    // Assert
    expect([m.control.getAttribute("role"), m.control.getAttribute("aria-label")]).toEqual(["button", "Jump to the latest"]);
  });

  it("shows the control once the reader scrolls more than a page above the tail", () => {
    // Arrange
    const m = mounted(700);
    // Act
    m.scrollTo(100);
    // Assert
    expect([shown(m.control), m.control.tabIndex, m.control.getAttribute("aria-hidden")]).toEqual([true, 0, "false"]);
  });

  it("keeps the control hidden while the reader is within a page of the tail", () => {
    // Arrange
    const m = mounted(700);
    // Act
    m.scrollTo(450);
    // Assert
    expect(shown(m.control)).toBe(false);
  });

  it("hides the control again once the reader scrolls back within a page", () => {
    // Arrange
    const m = mounted(700);
    m.scrollTo(100);
    // Act
    m.scrollTo(600);
    // Assert
    expect(shown(m.control)).toBe(false);
  });

  it("shows the control when content grows below a reader who scrolled away", () => {
    // Arrange -- 500px below a 300px view: within a page.
    const m = mounted(700);
    m.scrollTo(200);
    // Act -- a row lands at the tail, and the box's resize pass reports.
    m.box.scrollHeight = 1400;
    m.resize();
    // Assert
    expect(shown(m.control)).toBe(true);
  });

  it("parks the feed at its tail and follows when pressed", () => {
    // Arrange
    const m = mounted(700);
    m.scrollTo(100);
    // Act
    m.control.click();
    // Assert
    expect([m.box.scrollTop, m.tail.isFollowing()]).toEqual([1000, true]);
  });

  it("hides the control once the jump's own scroll event reports the tail", () => {
    // Arrange
    const m = mounted(700);
    m.scrollTo(100);
    m.control.click();
    // Act -- the browser clamps the park to the reachable end and reports it.
    m.box.scrollTop = 700;
    m.resize();
    // Assert
    expect(shown(m.control)).toBe(false);
  });

  it("records the press at INFO", async () => {
    // Arrange
    const m = mounted(700);
    m.scrollTo(100);
    const capture = captureLogRecords("info");
    // Act
    m.control.click();
    // Assert
    const record = await forwardedRecord(capture, "feed.jump-to-tail");
    expect(record.context).toMatchObject({ shown: true });
  });

  it("records the move under the tailJumped cause", async () => {
    // Arrange
    const m = mounted(700);
    m.scrollTo(100);
    const capture = captureLogRecords("info");
    // Act
    m.control.click();
    // Assert
    const record = await forwardedRecord(capture, "scroll.feed-moved");
    expect(record.context).toMatchObject({ cause: "tailJumped", from: 100, to: 1000 });
  });

  it("records the control showing at DEBUG with how far below the view the tail is", async () => {
    // Arrange
    const m = mounted(700);
    const capture = captureLogRecords("debug");
    // Act
    m.scrollTo(100);
    // Assert
    const record = await forwardedRecord(capture, "feed.jump-to-tail-shown");
    expect(record.context).toMatchObject({ shown: true, below_view: 600, client_height: 300 });
  });

  it("removes the control when uninstalled", () => {
    // Arrange
    const m = mounted(0);
    // Act
    uninstall?.();
    uninstall = null;
    // Assert
    expect(m.slot.childElementCount).toBe(0);
  });
});
