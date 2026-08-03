// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  DEFERRED_CLASS,
  HEIGHT_VAR,
  LazyUpgrader,
  PLACEHOLDER_CHARS_PER_LINE,
  PLACEHOLDER_CLASS,
  PLACEHOLDER_CHROME_PX,
  PLACEHOLDER_LINE_PX,
  PLACEHOLDER_MAX_PX,
  PLACEHOLDER_TEXT_CAP,
  UPGRADE_MARGIN_PX,
  canDeferItems,
  estimateHeightPx,
  isHeavyItem,
  itemPlainText,
  placeholderHtml,
} from "../src/lazy-item.js";
import { ConversationItem } from "../src/store.js";
import { StubIntersectionObserver, withIntersectionObserver } from "./intersection-stub.js";

const TS = new Date(2026, 4, 24, 9, 5).toISOString();

function text(body: string): ConversationItem {
  return { kind: "text", blockId: "b1", messageId: "m1", text: body, done: true, ts: TS };
}

describe("isHeavyItem: which items are worth standing in for", () => {
  it("defers a text block, whose render runs markdown and highlighting", () => {
    // Arrange / Act / Assert
    expect(isHeavyItem(text("hello"))).toBe(true);
  });

  it("defers a thinking block, whose render runs markdown too", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "thinking",
      blockId: "t1",
      messageId: "m1",
      text: "pondering",
      done: true,
    };
    // Act / Assert
    expect(isHeavyItem(item)).toBe(true);
  });

  it("defers a tool call, whose render builds a whole card", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      ts: TS,
      inputJson: '{"command":"ls"}',
      inputDone: true,
    };
    // Act / Assert
    expect(isHeavyItem(item)).toBe(true);
  });

  it("defers a user turn, whose prompt body renders fenced code", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "text", text: "do the thing" }],
      ts: TS,
    };
    // Act / Assert
    expect(isHeavyItem(item)).toBe(true);
  });

  it("never defers a merge-origin user turn, which renders one constant card", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "text", text: "resolve" }],
      ts: TS,
      origin: "merge",
    };
    // Act / Assert
    expect(isHeavyItem(item)).toBe(false);
  });

  it("never defers a permission prompt, whose buttons are the item", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "permission",
      requestId: "r1",
      toolUseId: "t1",
      toolName: "Bash",
      input: { command: "rm -rf /" },
    };
    // Act / Assert
    expect(isHeavyItem(item)).toBe(false);
  });

  it("never defers a system note, already cheaper than its estimate", () => {
    // Arrange
    const item: ConversationItem = { kind: "system", subtype: "init" };
    // Act / Assert
    expect(isHeavyItem(item)).toBe(false);
  });
});

describe("itemPlainText: the text a placeholder stands in with", () => {
  it("reads a text block's body straight off the item", () => {
    // Arrange / Act / Assert
    expect(itemPlainText(text("the answer"))).toBe("the answer");
  });

  it("reads a user turn's prompt through the turn's own text projection", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "user-turn",
      requestId: "r1",
      content: [{ type: "text", text: "do the thing" }],
      ts: TS,
    };
    // Act / Assert
    expect(itemPlainText(item)).toBe("do the thing");
  });

  it("stands a tool call in with its name and raw input, which has no prose", () => {
    // Arrange
    const item: ConversationItem = {
      kind: "tool",
      toolUseId: "t1",
      toolName: "Bash",
      messageId: "m1",
      ts: TS,
      inputJson: '{"command":"ls"}',
      inputDone: true,
    };
    // Act / Assert
    expect(itemPlainText(item)).toBe('Bash {"command":"ls"}');
  });

  it("has no text for a kind that is never deferred", () => {
    // Arrange
    const item: ConversationItem = { kind: "system", subtype: "init" };
    // Act / Assert
    expect(itemPlainText(item)).toBe("");
  });
});

describe("estimateHeightPx: sizing the box a skipped item leaves", () => {
  it("gives a one-word item a single line plus the bubble's chrome", () => {
    // Arrange / Act / Assert
    expect(estimateHeightPx("hi")).toBe(PLACEHOLDER_LINE_PX + PLACEHOLDER_CHROME_PX);
  });

  it("counts a hard newline as its own line", () => {
    // Arrange — four short lines, comfortably over the floor.
    const body = "a\nb\nc\nd";
    // Act
    const height = estimateHeightPx(body);
    // Assert
    expect(height).toBe(4 * PLACEHOLDER_LINE_PX + PLACEHOLDER_CHROME_PX);
  });

  it("wraps a long line into as many lines as it takes", () => {
    // Arrange — exactly three lines' worth of characters, unbroken.
    const body = "x".repeat(PLACEHOLDER_CHARS_PER_LINE * 3);
    // Act
    const height = estimateHeightPx(body);
    // Assert
    expect(height).toBe(3 * PLACEHOLDER_LINE_PX + PLACEHOLDER_CHROME_PX);
  });

  it("caps an enormous item so one of them cannot claim the whole scrollbar", () => {
    // Arrange
    const body = "line\n".repeat(10_000);
    // Act / Assert
    expect(estimateHeightPx(body)).toBe(PLACEHOLDER_MAX_PX);
  });
});

describe("placeholderHtml: the stand-in markup", () => {
  it("carries the item's text so the feed search still finds it", () => {
    // Arrange / Act
    const html = placeholderHtml(text("the missing needle"));
    // Assert
    expect(html).toContain("the missing needle");
  });

  it("escapes the text rather than letting it re-enter as markup", () => {
    // Arrange / Act
    const html = placeholderHtml(text("<script>boom</script>"));
    // Assert
    expect(html).not.toContain("<script>");
    expect(html).toContain("&lt;script&gt;");
  });

  it("wears the placeholder class, so the stylesheet can lay it out", () => {
    // Arrange / Act
    const html = placeholderHtml(text("body"));
    // Assert
    expect(html).toContain(`class="${PLACEHOLDER_CLASS}"`);
  });

  it("truncates a pathologically long body to the insert cap", () => {
    // Arrange
    const body = "y".repeat(PLACEHOLDER_TEXT_CAP + 500);
    // Act
    const html = placeholderHtml(text(body));
    // Assert
    expect(html).toContain("y".repeat(PLACEHOLDER_TEXT_CAP));
    expect(html).not.toContain("y".repeat(PLACEHOLDER_TEXT_CAP + 1));
  });

  it("renders nothing for an item the feed draws nothing for", () => {
    // Arrange / Act
    const html = placeholderHtml({ kind: "system", subtype: "init" });
    // Assert — an empty body, so `.feed-item:empty` still hides the node.
    expect(html).toBe(`<div class="${PLACEHOLDER_CLASS}"></div>`);
  });
});

describe("canDeferItems: deferral needs a signal that an item came near", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("declines to defer where IntersectionObserver is absent", () => {
    // Arrange — jsdom ships none, which is the bare environment.
    // Act / Assert
    expect(canDeferItems()).toBe(false);
  });

  it("defers once an IntersectionObserver is available", () => {
    // Arrange
    vi.stubGlobal("IntersectionObserver", StubIntersectionObserver);
    // Act / Assert
    expect(canDeferItems()).toBe(true);
  });
});

describe("LazyUpgrader: reporting the items that came near", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
    StubIntersectionObserver.instances.length = 0;
  });

  /** A `.feed-item`-shaped element carrying KEY, as the renderer mounts them. */
  function itemEl(key: string): HTMLElement {
    const el = document.createElement("div");
    el.className = `feed-item ${DEFERRED_CLASS}`;
    el.dataset.key = key;
    el.style.setProperty(HEIGHT_VAR, "200px");
    return el;
  }

  it("watches the feed's own scroll box, not the document viewport", () => {
    // Arrange
    const root = document.createElement("div");
    // Act
    withIntersectionObserver(() => new LazyUpgrader(root, () => {}));
    // Assert
    expect(StubIntersectionObserver.instances[0].options?.root).toBe(root);
  });

  it("upgrades ahead of the viewport by the whole margin", () => {
    // Arrange / Act
    withIntersectionObserver(() => new LazyUpgrader(document.createElement("div"), () => {}));
    // Assert
    expect(StubIntersectionObserver.instances[0].options?.rootMargin).toBe(
      `${UPGRADE_MARGIN_PX}px 0px`,
    );
  });

  it("reports a watched item's key once it comes near", () => {
    // Arrange
    const seen: string[][] = [];
    const el = itemEl("text:b1");
    const upgrader = withIntersectionObserver(
      () => new LazyUpgrader(document.createElement("div"), (keys) => seen.push([...keys])),
    );
    upgrader.watch(el);
    // Act
    StubIntersectionObserver.instances[0].fire([el]);
    // Assert
    expect(seen).toEqual([["text:b1"]]);
  });

  it("reports a whole batch in one call, since one repaint answers them all", () => {
    // Arrange
    const seen: string[][] = [];
    const a = itemEl("text:b1");
    const b = itemEl("text:b2");
    const upgrader = withIntersectionObserver(
      () => new LazyUpgrader(document.createElement("div"), (keys) => seen.push([...keys])),
    );
    upgrader.watch(a);
    upgrader.watch(b);
    // Act
    StubIntersectionObserver.instances[0].fire([a, b]);
    // Assert
    expect(seen).toEqual([["text:b1", "text:b2"]]);
  });

  it("ignores an entry that merely left the margin", () => {
    // Arrange
    const seen: string[][] = [];
    const el = itemEl("text:b1");
    const upgrader = withIntersectionObserver(
      () => new LazyUpgrader(document.createElement("div"), (keys) => seen.push([...keys])),
    );
    upgrader.watch(el);
    // Act
    StubIntersectionObserver.instances[0].fireWith([{ target: el, isIntersecting: false }]);
    // Assert
    expect(seen).toEqual([]);
  });

  it("stops watching an item it has reported, an upgrade being final", () => {
    // Arrange
    const el = itemEl("text:b1");
    const upgrader = withIntersectionObserver(
      () => new LazyUpgrader(document.createElement("div"), () => {}),
    );
    upgrader.watch(el);
    // Act
    StubIntersectionObserver.instances[0].fire([el]);
    // Assert
    expect(StubIntersectionObserver.instances[0].targets.has(el)).toBe(false);
  });

  it("drops every watch on reset, the nodes having been discarded wholesale", () => {
    // Arrange
    const el = itemEl("text:b1");
    const upgrader = withIntersectionObserver(
      () => new LazyUpgrader(document.createElement("div"), () => {}),
    );
    upgrader.watch(el);
    // Act
    upgrader.reset();
    // Assert
    expect(StubIntersectionObserver.instances[0].targets.size).toBe(0);
  });

  it("watches nothing where no IntersectionObserver exists, rather than throwing", () => {
    // Arrange — no stub installed, so the environment has none.
    const upgrader = new LazyUpgrader(document.createElement("div"), () => {});
    // Act / Assert
    expect(() => upgrader.watch(itemEl("text:b1"))).not.toThrow();
  });
});
