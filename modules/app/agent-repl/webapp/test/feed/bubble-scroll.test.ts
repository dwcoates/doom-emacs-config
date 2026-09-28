// @vitest-environment jsdom
/**
 * THE BUBBLE-GEOMETRY DOM CONTRACT (owner ruling, 2026-09-14).
 *
 * "ensure the scrollbar of the response bubble (and all bubbles, in fact)
 * abuts (or close to) the right hand side of the bubble (currently there's
 * quite a bit of gap), and starts UNDER the token count / metadata in the top
 * right corner of the bubble."
 *
 * The stylesheet half of that is asserted in `test/styles.test.ts`. What is
 * asserted HERE is the half no rule can supply: the NESTING each of those rules
 * assumes. For every bubble kind the feed draws, the metadata strip must come
 * before the scroll box in document order and must not be inside it — a strip
 * inside the box scrolls away with the text, and a strip beside it puts the bar
 * alongside the token figure instead of under it.
 *
 * THE AGENT'S RESPONSE IS THE ONE EXCEPTION (owner ruling, 2026-09-15): its
 * cost corner is deliberately moved INSIDE the scroll box, floated top-right
 * before the body, so the prose's first line wraps around it. Its nesting is
 * therefore asserted in `test/feed/cards/response.test.ts` ("the usage corner's
 * first-line float"), not by the strip-above table below.
 */
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedAgentPromptSchema,
  FeedRowSchema,
  FeedShellSchema,
  FeedSimpleToolCallSchema,
} from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { BUBBLE_BOX_CLASS, BUBBLE_SCROLL_CLASS, bubbleBox } from "../../src/feed/bubble-scroll.js";
import { agenticBubble } from "../../src/feed/cards/controls.js";
import { drawFeedShellBody } from "../../src/feed/cards/shell.js";
import { drawFeedSimpleToolCall } from "../../src/feed/cards/tool-call.js";
import { drawFeedAgentPrompt } from "../../src/feed/rows/agent-prompt.js";
import { mountBubble } from "../../src/feed/bubble.js";
import { defaultBubbleBody } from "../../src/feed/renderers.js";
import { feedId, harness, rowContext, stubRenderers, subagentRow } from "./harness.js";
import { orderFor } from "../feed-order.js";

/** A row context on ROW, with no verb of its own scripted. */
function rc(id: string) {
  const h = harness();
  return rowContext(h.ctx, create(FeedRowSchema, { id: feedId(id), order: orderFor(id) }));
}

/** One bubble kind: what it draws, and the two elements the ruling names. */
interface Kind {
  readonly name: string;
  readonly draw: () => HTMLElement;
  /** The metadata / head strip: the token figure, the author, the tool head. */
  readonly meta: string;
  /** The element the stylesheet caps and scrolls. */
  readonly scroll: string;
  /** Whether the two are DIRECT siblings (every kind but the shell's spool). */
  readonly siblings: boolean;
}

const KINDS: readonly Kind[] = [
  // The agent's response is NOT one of these kinds: its cost corner floats
  // INSIDE the scroll box (owner ruling, 2026-09-15), so it does not obey the
  // strip-above contract this table asserts — its nesting is covered in
  // `test/feed/cards/response.test.ts` instead.
  //
  // A person's own prompt is NOT one of these kinds: it carries no metadata
  // strip at all (no "You" label, no other attribution) since the owner ruled
  // it gone (2026-09-14), so there is no meta/scroll nesting for this table to
  // assert about it.
  {
    name: "an agent-addressed prompt",
    draw: () =>
      drawFeedAgentPrompt(
        create(FeedAgentPromptSchema, {
          address: { text: "→ Explore" },
          body: { blocks: [{ block: { case: "text", value: { text: "go" } } }] },
        }),
      ),
    meta: ".prompt-address",
    scroll: `.${BUBBLE_SCROLL_CLASS}`,
    siblings: true,
  },
  {
    name: "a tool card",
    draw: () =>
      drawFeedSimpleToolCall(
        create(FeedSimpleToolCallSchema, {
          name: { text: "Read" },
          input: { text: "/w/file.ts" },
          outcome: { case: "running", value: {} },
        }),
        rc("tool-1"),
      ),
    meta: ".tool-head",
    scroll: ".tool-input",
    siblings: true,
  },
  {
    name: "a detached shell's spool body",
    draw: () =>
      // The shell's HEAD (command, clock) and its spool BODY are now separate
      // bubble rows; the scroll box lives on the body. The truncation count is
      // the strip above it — outside the box, so it stays put while the box
      // scrolls.
      drawFeedShellBody(
        create(FeedShellSchema, {
          spool: { text: "line\n", omitted: { text: "3 earlier lines not shown" } },
        }),
        rc("shell-1"),
      ),
    meta: ".shell-omitted",
    scroll: ".shell-tail",
    siblings: true,
  },
];

describe("bubbleBox: the shared box", () => {
  it("wears the class the stylesheet caps and paints a scrollbar on", () => {
    // Arrange
    const body = document.createElement("div");
    body.className = "bubble-body";

    // Act
    const scroll = bubbleBox(body, true);

    // Assert
    expect(scroll.className).toBe(`${BUBBLE_SCROLL_CLASS} ${BUBBLE_BOX_CLASS}`);
  });

  it("builds an uncapped box with the structural class alone, never the scroll class", () => {
    // Arrange
    const body = document.createElement("div");
    body.className = "bubble-body";

    // Act
    const box = bubbleBox(body, false);

    // Assert
    expect(box.className).toBe(BUBBLE_BOX_CLASS);
  });

  it("holds the body as its only child, so the box's edge is not the text's", () => {
    // Arrange
    const body = document.createElement("div");
    body.className = "bubble-body";

    // Act
    const scroll = bubbleBox(body, true);

    // Assert
    expect([...scroll.children]).toEqual([body]);
  });
});

describe("every bubble kind: the strip is above the scroll box", () => {
  it.each(KINDS)("draws $name's metadata strip before its scroll box", (kind) => {
    // Arrange
    const el = kind.draw();

    // Act
    const meta = el.querySelector(kind.meta);
    const scroll = el.querySelector(kind.scroll);

    // Assert
    expect(
      meta === null || scroll === null
        ? null
        : Boolean(meta.compareDocumentPosition(scroll) & Node.DOCUMENT_POSITION_FOLLOWING),
    ).toBe(true);
  });

  it.each(KINDS)("keeps $name's metadata strip OUTSIDE the scroll box", (kind) => {
    // Arrange
    const el = kind.draw();

    // Act
    const scroll = el.querySelector(kind.scroll);

    // Assert
    expect(scroll?.querySelector(kind.meta) ?? null).toBeNull();
  });

  it.each(KINDS.filter((kind) => kind.siblings))(
    "makes $name's scroll box a SIBLING of the strip, not a child",
    (kind) => {
      // Arrange
      const el = kind.draw();

      // Act
      const meta = el.querySelector(kind.meta);
      const scroll = el.querySelector(kind.scroll);

      // Assert
      expect(scroll?.parentElement).toBe(meta?.parentElement);
    },
  );
});

describe("the agentic bubbles: the same box as the response they copy", () => {
  it("hangs the purple bubble's body in the shared scroll box", () => {
    // Arrange / Act
    const bubble = agenticBubble({ state: "published", content: [] });
    const body = bubble.querySelector<HTMLElement>(".bubble-body");
    if (body === null) throw new Error("no body");

    // Assert
    expect(body.parentElement?.className).toBe(`${BUBBLE_SCROLL_CLASS} ${BUBBLE_BOX_CLASS}`);
    expect(bubble.querySelector(`.${BUBBLE_SCROLL_CLASS}`)).toBe(body.parentElement);
  });
});

describe("the subagent bubble: the head strip over the sub-feed", () => {
  /** A collapsed subagent bubble, with a stub head and the default body. */
  function subagentBubble(): HTMLElement {
    const h = harness();
    const row = subagentRow("b1");
    return mountBubble({
      ctx: h.ctx,
      row,
      rc: rowContext(h.ctx, row),
      head: () => {
        const el = document.createElement("span");
        el.className = "stub-head";
        return el;
      },
      body: defaultBubbleBody,
      renderers: stubRenderers(),
      revealRow: async () => false,
      bubble: () => {
        throw new Error("no nested bubble in this fixture");
      },
      initialFolded: true,
    }).element;
  }

  it("draws the head line before the sub-feed panel it scrolls", () => {
    // Arrange / Act
    const el = subagentBubble();
    const head = el.querySelector(".bubble-head");
    const panel = el.querySelector(".bubble-subfeed");

    // Assert
    expect(
      head === null || panel === null
        ? null
        : Boolean(head.compareDocumentPosition(panel) & Node.DOCUMENT_POSITION_FOLLOWING),
    ).toBe(true);
  });

  it("keeps the head line OUTSIDE the sub-feed panel, so it cannot scroll away", () => {
    // Arrange / Act
    const el = subagentBubble();

    // Assert
    expect(el.querySelector(".bubble-subfeed")?.querySelector(".bubble-head") ?? null).toBeNull();
  });

  it("makes the panel a SIBLING of the head, which is what puts the bar under it", () => {
    // Arrange / Act
    const el = subagentBubble();

    // Assert
    expect(el.querySelector(".bubble-subfeed")?.parentElement).toBe(
      el.querySelector(".bubble-head")?.parentElement,
    );
  });
});
