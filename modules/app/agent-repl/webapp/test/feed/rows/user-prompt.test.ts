// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedUserPromptSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedUserPrompt } from "../../../src/feed/rows/user-prompt.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import { EXPANDED_CLASS, installClickExpand } from "../../../src/expand.js";
import { BUBBLE_SCROLL_CLASS } from "../../../src/feed/bubble-scroll.js";
import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
} from "../../../src/breathing.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  USER_PROMPT_CAP_LINES,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_VARIANT_ATTRIBUTE,
} from "../../../src/bubble/draw.js";
import { BUBBLE_QUOTE_CLASS, QUOTE_MARKER_CLASS, QUOTE_PARTS_CLASS } from "../../../src/bubble/quote.js";
import { KNOWN_COMMAND_CLASS } from "../../../src/feed/rows/known-commands.js";
import { PROMPT_COPY_CLASS } from "../../../src/feed/rows/blocks.js";
import { boundCopyText } from "../../../src/copy-control.js";
import { SELECTED_ENTRY_CLASS } from "../../../src/feed/selected-entry.js";
import { cascadedValue, installStylesheet } from "../../stylesheet.js";
import { FITTING_TREE, WIDE_TREE, stagedCols, treeLineWidths, useTreeLayout } from "../../tree-layout.js";
import { servedProse } from "../../prose.js";

/** A prompt with the given blocks. */
function prompt(
  blocks: unknown[],
  author = "You",
  working = false,
  commands: string[] = [],
  awaitingUser = false,
) {
  return create(FeedUserPromptSchema, {
    author: { label: author },
    result: { case: "success", value: { body: { blocks: blocks as never } } },
    working,
    awaitingUser,
    commands: commands.map((name) => ({ name })),
  });
}

/** A prompt of one text block naming COMMANDS as known. */
function said(text: string, commands: string[]) {
  return prompt([{ block: { case: "text", value: { prose: servedProse(text) } } }], "You", false, commands);
}

describe("drawFeedUserPrompt: the known commands", () => {
  it("marks a slash word naming a command the row lists", () => {
    // Arrange / Act
    const el = drawFeedUserPrompt(said("/merge-queue own", ["merge-queue"]));
    // Assert
    expect(el.querySelector(`.${KNOWN_COMMAND_CLASS}`)?.textContent).toBe("/merge-queue");
  });

  it("marks nothing when the row lists no command", () => {
    // Arrange / Act
    const el = drawFeedUserPrompt(said("/merge-queue own", []));
    // Assert
    expect(el.querySelector(`.${KNOWN_COMMAND_CLASS}`)).toBeNull();
  });

  it("marks a command the row gains when it is redrawn in place", () => {
    // Arrange
    const before = drawFeedUserPrompt(said("/merge-queue own", []));
    // Act
    const after = drawFeedUserPrompt(said("/merge-queue own", ["merge-queue"]), before);
    // Assert
    expect(after.querySelector(`.${KNOWN_COMMAND_CLASS}`)?.textContent).toBe("/merge-queue");
  });

  it("records how many commands the row lists", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    drawFeedUserPrompt(said("/a /b", ["a", "b"]));
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-user-prompt");
    expect(record.context).toEqual(expect.objectContaining({ commands: 2 }));
  });

  it("colors a marked command with the known-command color", () => {
    // Arrange
    const restore = installStylesheet();
    const el = drawFeedUserPrompt(said("/merge-queue", ["merge-queue"]));
    document.body.append(el);
    try {
      // Act
      const mark = el.querySelector(`.${KNOWN_COMMAND_CLASS}`) as Element;
      // Assert
      expect(cascadedValue(mark, "color")).toBe("var(--known-command)");
    } finally {
      el.remove();
      restore();
    }
  });
});

describe("drawFeedUserPrompt: the bubble", () => {
  it("wears the one bubble's classes and the prompt hook the feed finds it by", () => {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("hi") } } }]));
    expect(el.className).toBe("bubble md user");
  });

  it("stamps the wave's phase inline, so a redraw does not jump it back", () => {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("hi") } } }]));
    expect(el.getAttribute("style")).toMatch(/animation-delay:-\d+ms/);
  });

  it("waves when its row says the turn is working", () => {
    const el = drawFeedUserPrompt(prompt([], "You", true));
    expect(el.getAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(PROMPT_WAVE_WORKING);
  });

  it("does not wave when its row says the turn is not working", () => {
    const el = drawFeedUserPrompt(prompt([], "You", false));
    expect(el.hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("does not wave while its row says the working turn awaits the user", () => {
    const el = drawFeedUserPrompt(prompt([], "You", true, [], true));
    expect(el.hasAttribute(PROMPT_WAVE_ATTRIBUTE)).toBe(false);
  });

  it("draws no author label, however the daemon resolved it", () => {
    const el = drawFeedUserPrompt(prompt([], "Explore"));
    expect(el.querySelector(".prompt-author")).toBeNull();
  });

  it("puts the blocks in the capped body, which the stylesheet already caps", () => {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("hi") } } }]));
    expect(el.querySelector(".bubble-body")).not.toBeNull();
  });
});

describe("drawFeedUserPrompt: the body", () => {
  it("draws every block, in the composed order", () => {
    const el = drawFeedUserPrompt(
      prompt([
        { block: { case: "text", value: { prose: servedProse("one") } } },
        { block: { case: "image", value: { src: "/s.png", alt: "shot" } } },
      ]),
    );
    expect(el.querySelectorAll(".bubble-body > *")).toHaveLength(2);
  });

  it("draws an empty body for a prompt with no blocks", () => {
    expect(drawFeedUserPrompt(prompt([])).querySelector(".bubble-body")?.children).toHaveLength(0);
  });
});

describe("drawFeedUserPrompt: refusals", () => {
  it("refuses a prompt with no author, rather than drawing an unattributed one", () => {
    const msg = create(FeedUserPromptSchema, {
      result: { case: "success", value: { body: { blocks: [] } } },
    });
    expect(() => drawFeedUserPrompt(msg)).toThrow(MalformedView);
  });

  it("refuses an unset result, one arm being one arm and not none", () => {
    const msg = create(FeedUserPromptSchema, { author: { label: "You" } });
    expect(() => drawFeedUserPrompt(msg)).toThrow(MalformedView);
  });

  it("refuses a success with no body", () => {
    const msg = create(FeedUserPromptSchema, {
      author: { label: "You" },
      result: { case: "success", value: {} },
    });
    expect(() => drawFeedUserPrompt(msg)).toThrow(MalformedView);
  });
});

describe("drawFeedUserPrompt: an arm this build has no case for", () => {
  it("refuses a result arm a NEWER daemon set, quoting the arm it could not draw", () => {
    // Arrange: one arm today, so a second one can only come from a newer schema.
    const msg = create(FeedUserPromptSchema, { author: { label: "You" } });
    (msg as unknown as { result: unknown }).result = { case: "redacted", value: {} };

    // Act
    let thrown: unknown;
    try {
      drawFeedUserPrompt(msg);
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).path).toBe("FeedUserPrompt.result");
    expect((thrown as MalformedView).detail).toBe(
      "arm 'redacted' is not one this build can draw",
    );
  });
});

describe("drawFeedUserPrompt: the record of the row", () => {
  it("records the drawn row at info, a row being drawn exactly once", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("hi") } } }]));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-user-prompt");
    expect(record.level.case).toBe("info");
  });
});

describe("drawFeedUserPrompt: its spec", () => {
  it("is a prompt-role bubble", () => {
    const el = drawFeedUserPrompt(prompt([]));
    expect(el.getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("prompt");
  });

  it("is the user variant", () => {
    const el = drawFeedUserPrompt(prompt([]));
    expect(el.getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("user");
  });

  it("folds at the user prompt cap, which has no line limit", () => {
    const el = drawFeedUserPrompt(prompt([]));
    expect(el.getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(String(USER_PROMPT_CAP_LINES));
  });

  it("stays expandable on a click, unlike a response (which is never abbreviated)", () => {
    // Arrange
    const host = document.createElement("div");
    document.body.append(host);
    const uninstall = installClickExpand(host, () => "");
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("go") } } }]));
    host.append(el);
    const box = el.querySelector<HTMLElement>(`:scope > .${BUBBLE_SCROLL_CLASS}`);
    // Act
    box?.click();
    // Assert
    expect(box?.classList.contains(EXPANDED_CLASS)).toBe(true);
    uninstall();
    host.remove();
  });
});

describe("drawFeedUserPrompt: the whole prompt, folded or open", () => {
  /** A prompt of forty lines, mounted with the real stylesheet; CHECK reads its scroll box. */
  function withTallPrompt(check: (box: HTMLElement) => void): void {
    const restore = installStylesheet();
    const text = Array.from({ length: 40 }, (_, i) => `line ${i}`).join("\n\n");
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse(text) } } }]));
    document.body.append(el);
    try {
      check(el.querySelector<HTMLElement>(`:scope > .${BUBBLE_SCROLL_CLASS}`) as HTMLElement);
    } finally {
      el.remove();
      restore();
    }
  }

  it("puts no height cap on the folded bubble", () => {
    // Arrange / Act / Assert
    withTallPrompt((box) => expect(cascadedValue(box, "max-height")).toBe("none"));
  });

  it("puts no height cap on the open bubble, so opening it never shrinks it", () => {
    // Arrange / Act / Assert
    withTallPrompt((box) => {
      box.classList.add(EXPANDED_CLASS);
      expect(cascadedValue(box, "max-height")).toBe("none");
    });
  });
});

describe("drawFeedUserPrompt: a tree the person typed", () => {
  const staged = useTreeLayout();

  /** A prompt of one text block holding TEXT, attached under its own column. */
  function mounted(text: string): HTMLElement {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse(text) } } }]));
    const column = document.createElement("div");
    column.append(el);
    document.body.append(column);
    return el;
  }

  it("wraps at the prompt bubble's own cap", () => {
    // Arrange / Act
    const el = mounted(WIDE_TREE);
    // Assert — more lines than branches, none wider than the cap's budget.
    const widths = treeLineWidths(el);
    expect(widths.length).toBeGreaterThan(3);
    expect(Math.max(...widths)).toBeLessThanOrEqual(stagedCols(staged.layout));
  });

  it("never wraps below its max width", () => {
    // Arrange / Act
    const el = mounted(FITTING_TREE);
    // Assert — one line per branch.
    expect(treeLineWidths(el)).toHaveLength(3);
  });
});

describe("drawFeedUserPrompt: a re-push", () => {
  it("updates the previous draw in place, keeping its scroll box", () => {
    // Arrange
    const first = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("hi") } } }]));
    const box = first.querySelector(".bubble-scroll");
    // Act
    const again = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("hi") } } }]), first);
    // Assert
    expect([again, again.querySelector(".bubble-scroll")]).toEqual([first, box]);
  });
});

describe("drawFeedUserPrompt: a reply's quote", () => {
  /** A reply: the quote block, then the person's words. */
  const reply = () =>
    prompt([
      { block: { case: "quote", value: { heading: "⟢ Replying", quoted: "Paris.", closing: "⟢ My message" } } },
      { block: { case: "text", value: { prose: servedProse("and its population?") } } },
    ]);

  it("draws the quote in the body, ahead of the words", () => {
    // Act
    const el = drawFeedUserPrompt(reply());
    // Assert
    const body = el.querySelector(".bubble-body");
    expect([...(body?.children ?? [])].map((child) => child.classList.contains(QUOTE_PARTS_CLASS))).toEqual([true, false]);
  });

  /** The display of every expand-only part of EL's quote, its heading, and its words. */
  const displays = (el: HTMLElement) => ({
    parts: [...el.querySelectorAll(`.${BUBBLE_QUOTE_CLASS}`)].map((part) => cascadedValue(part, "display") === "none"),
    heading: cascadedValue(el.querySelector(`.${QUOTE_MARKER_CLASS}`) as Element, "display") === "none",
    words: cascadedValue(el.querySelector(".prompt-block-text") as Element, "display") === "none",
  });

  it("hides the quote but its heading while the bubble is collapsed, leaving the words", () => {
    // Arrange
    const restore = installStylesheet();
    const el = drawFeedUserPrompt(reply());
    document.body.append(el);
    try {
      // Act
      const shown = displays(el);
      // Assert
      expect(shown).toEqual({ parts: [true, true, true], heading: false, words: false });
    } finally {
      el.remove();
      restore();
    }
  });

  it("shows the whole quote once the bubble is expanded", () => {
    // Arrange
    const restore = installStylesheet();
    const el = drawFeedUserPrompt(reply());
    document.body.append(el);
    try {
      // Act
      el.querySelector(`.${BUBBLE_SCROLL_CLASS}`)?.classList.add(EXPANDED_CLASS);
      // Assert
      expect(displays(el)).toEqual({ parts: [false, false, false], heading: false, words: false });
    } finally {
      el.remove();
      restore();
    }
  });

  it("fills the quoted response's code block with the response bubble's grey", () => {
    // Arrange
    const restore = installStylesheet();
    const el = drawFeedUserPrompt(reply());
    document.body.append(el);
    try {
      // Act
      const parts = el.querySelector(`.${QUOTE_PARTS_CLASS}`) as Element;
      // Assert
      expect([cascadedValue(parts, "--code-block-bg"), parts.querySelector("pre.md-code") !== null]).toEqual([
        "var(--assistant)",
        true,
      ]);
    } finally {
      el.remove();
      restore();
    }
  });

  it("leaves the person's own code blocks the prompt's blue", () => {
    // Arrange
    const restore = installStylesheet();
    const el = drawFeedUserPrompt(reply());
    document.body.append(el);
    try {
      // Act
      const words = el.querySelector(".prompt-block-text") as Element;
      // Assert
      expect(cascadedValue(words, "--code-block-bg")).toBe("var(--prompt-code-bg)");
    } finally {
      el.remove();
      restore();
    }
  });
});

describe("drawFeedUserPrompt: the copy control", () => {
  /** The prompt bubble drawn from BLOCKS, and its copy control. */
  function copyControlOf(blocks: unknown[]): { el: HTMLElement; control: HTMLElement } {
    const el = drawFeedUserPrompt(prompt(blocks));
    const control = el.querySelector<HTMLElement>(`.${PROMPT_COPY_CLASS}`);
    if (control === null) throw new Error("the prompt drew no copy control");
    return { el, control };
  }

  it("floats in the scroll box ahead of the body, as the bubble's corner", () => {
    // Act
    const { control } = copyControlOf([{ block: { case: "text", value: { prose: servedProse("hi") } } }]);
    // Assert
    expect([control.parentElement?.classList.contains("bubble-box"), control.nextElementSibling?.className]).toEqual([
      true,
      "bubble-body",
    ]);
  });

  it("copies the whole prompt, quote and words", () => {
    // Act
    const { control } = copyControlOf([
      { block: { case: "quote", value: { heading: "⟢ Replying", quoted: "Paris.", closing: "⟢ My message" } } },
      { block: { case: "text", value: { prose: servedProse("and *Rome*?") } } },
    ]);
    // Assert
    expect(boundCopyText(control).text).toBe("⟢ Replying:\n\nParis.\n\n⟢ My message:\n\nand *Rome*?");
  });

  it("copies what the latest push says, after a redraw in place", () => {
    // Arrange
    const first = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("old") } } }]));
    // Act
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { prose: servedProse("new") } } }]), first);
    // Assert
    const controls = el.querySelectorAll(`.${PROMPT_COPY_CLASS}`);
    expect([controls.length, boundCopyText(controls[0]).text]).toEqual([1, "new"]);
  });

  it("adds no text to the bubble", () => {
    // Act
    const { el, control } = copyControlOf([{ block: { case: "text", value: { prose: servedProse("just this") } } }]);
    const withControl = el.textContent;
    // Assert
    control.remove();
    expect(el.textContent).toBe(withControl);
  });

  it.each([
    ["hidden while the bubble is not selected", false, "none"],
    ["shown while the bubble is selected", true, "inline-flex"],
  ])("is %s", (_name, selected, display) => {
    // Arrange
    const restore = installStylesheet();
    const { el, control } = copyControlOf([{ block: { case: "text", value: { prose: servedProse("hi") } } }]);
    el.classList.toggle(SELECTED_ENTRY_CLASS, selected);
    document.body.append(el);
    try {
      // Act / Assert
      expect(cascadedValue(control, "display")).toBe(display);
    } finally {
      el.remove();
      restore();
    }
  });
});
