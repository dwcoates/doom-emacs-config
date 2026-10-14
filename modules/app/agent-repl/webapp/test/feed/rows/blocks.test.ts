// @vitest-environment jsdom
import stylesheet from "../../../src/styles.css?raw";
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedImageBlockSchema,
  FeedQuoteBlockSchema,
  FeedTextBlockSchema,
  FeedUnsupportedBlockSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  IMAGE_MISSING_TEXT,
  drawFeedImageBlock,
  drawFeedQuoteBlock,
  drawFeedTextBlock,
  drawFeedUnsupportedBlock,
  drawPromptBlockArm,
  drawPromptCopyControl,
  PROMPT_COPY_CLASS,
  promptBlockCopyText,
} from "../../../src/feed/rows/blocks.js";
import { boundCopyText, COPY_CONTROL_CLASS, COPY_KIND_ATTRIBUTE } from "../../../src/copy-control.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import imageThumbnail from "../../../../proto/vocab/image-thumbnail.json";
import { BUBBLE_QUOTE_CLASS, QUOTE_MARKER_CLASS, QUOTE_PARTS_CLASS } from "../../../src/bubble/quote.js";
import { MARKDOWN_SLOT_ATTRIBUTE, createBubbleBody, paintBody } from "../../../src/bubble/body.js";

/** Paint BLOCK through the one bubble body, as a prompt bubble does. */
function painted(block: HTMLElement): HTMLElement {
  paintBody(createBubbleBody(), [block]);
  return block;
}

describe("drawFeedTextBlock", () => {
  it("is a markdown slot, so the bubble's body pipeline paints it", () => {
    const el = drawFeedTextBlock(create(FeedTextBlockSchema, { text: "**bold**" }));
    expect(el.hasAttribute(MARKDOWN_SLOT_ATTRIBUTE)).toBe(true);
  });

  it("renders the text as markdown, which is what the schema says it is", () => {
    const el = painted(drawFeedTextBlock(create(FeedTextBlockSchema, { text: "**bold**" })));
    expect(el.querySelector("strong")?.textContent).toBe("bold");
  });

  it("draws a typed newline as a line break inside one paragraph, spaced as a response's lines are", () => {
    const el = painted(drawFeedTextBlock(create(FeedTextBlockSchema, { text: "one\ntwo" })));
    expect(el.querySelectorAll("p").length).toBe(1);
    expect(el.querySelector("p br")).not.toBeNull();
  });

  it("draws an empty block as an empty body rather than refusing", () => {
    expect(painted(drawFeedTextBlock(create(FeedTextBlockSchema, { text: "" }))).textContent).toBe("");
  });
});

describe("drawFeedQuoteBlock", () => {
  /** A quote of RESPONSE's markdown, taken apart as the daemon does. */
  const quote = (quoted: string) =>
    drawFeedQuoteBlock(
      create(FeedQuoteBlockSchema, {
        heading: "⟢ Replying to an earlier response of yours",
        quoted,
        closing: "⟢ My message",
      }),
    );

  it("draws the heading, its colon, the quoted text and the closing marker in order", () => {
    expect(quote("Paris.").textContent).toBe("⟢ Replying to an earlier response of yours:Paris.⟢ My message:");
  });

  it("draws the quoted text as a code block, verbatim", () => {
    expect(quote("Paris.").querySelector("pre.md-code code")?.textContent).toBe("Paris.");
  });

  it("draws a quoted metaprompt tree as code rather than as a tree", () => {
    // Arrange
    const tree = "Response (👀 no changes made)\n\n1 🔎 a root\n├── 1.1 a child\n└── 1.2 another";
    // Act
    const el = painted(quote(tree));
    // Assert
    expect([el.querySelector(".mp-tree"), el.querySelector("pre code")?.textContent]).toEqual([null, tree]);
  });

  it("escapes the quoted text rather than reading it as markup", () => {
    expect(quote("<b>raw</b>").querySelector("pre code")?.innerHTML).toBe("&lt;b&gt;raw&lt;/b&gt;");
  });

  it("marks the heading and the closing marker, and only those, as the quote's markers", () => {
    const markers = [...quote("Paris.").querySelectorAll(`.${QUOTE_MARKER_CLASS}`)].map((el) => el.textContent);
    expect(markers).toEqual(["⟢ Replying to an earlier response of yours:", "⟢ My message:"]);
  });

  it("hides with the fold every part but the heading's words", () => {
    const hidden = [...quote("Paris.").querySelectorAll(`.${BUBBLE_QUOTE_CLASS}`)].map((el) => el.textContent);
    expect(hidden).toEqual([":", "Paris.", "⟢ My message:"]);
  });

  it("is not itself hidden by the fold, so its heading shows collapsed", () => {
    const el = quote("Paris.");
    expect([el.classList.contains(BUBBLE_QUOTE_CLASS), el.classList.contains(QUOTE_PARTS_CLASS)]).toEqual([false, true]);
  });

  it("records the quote it draws at info, with its heading and length", async () => {
    // Arrange
    const capture = captureLogRecords();
    // Act
    quote("Paris.");
    // Assert
    const record = await forwardedRecord(capture, "feed.draw-quote-block");
    expect([record.level.case, record.context?.heading, record.context?.characters]).toEqual([
      "info",
      "⟢ Replying to an earlier response of yours",
      6,
    ]);
  });
});

describe("the attached image's own rule in the stylesheet", () => {
  it("caps an attached image at the width of the bubble it sits in", () => {
    // Arrange: the rule as the stylesheet declares it. An unconstrained <img>
    // of a screenshot is a couple of thousand pixels wide and would push the
    // bubble, and with it the feed's column, off the panel.
    const rule = stylesheet.match(/\.prompt-block-image\s*\{[^}]*\}/)?.[0] ?? "";

    // Act / Assert.
    expect(rule).toContain("max-width: 100%");
  });

  it.each([
    ["draws every image at the shared thumbnail height, large or small", `height: ${imageThumbnail.height_px}px`],
    ["keeps the aspect ratio when the width cap bites", "object-fit: contain"],
  ])("%s", (_name, declaration) => {
    // Arrange.
    const rule = stylesheet.match(/\.prompt-block-image\s*\{[^}]*\}/)?.[0] ?? "";

    // Act / Assert.
    expect(rule).toContain(declaration);
  });
});

describe("drawFeedImageBlock", () => {
  it("uses the daemon's resolved src verbatim", () => {
    const el = drawFeedImageBlock(create(FeedImageBlockSchema, { src: "/x.png", alt: "a" }));
    expect(el.getAttribute("src")).toBe("/x.png");
  });

  it("keeps an empty alt empty, that being a legitimate resolution", () => {
    const el = drawFeedImageBlock(create(FeedImageBlockSchema, { src: "/x.png", alt: "" }));
    expect(el.getAttribute("alt")).toBe("");
  });

  it("says the shared words for a lost image", () => {
    expect(IMAGE_MISSING_TEXT).toBe(imageThumbnail.missing_text);
  });

  it("says the image is no longer available, in its place, when it cannot be loaded", () => {
    // Arrange
    const box = document.createElement("div");
    const el = drawFeedImageBlock(create(FeedImageBlockSchema, { src: "/gone.png", alt: "clip.png" }));
    box.appendChild(el);

    // Act
    el.dispatchEvent(new Event("error"));

    // Assert
    expect([box.children.length, box.firstElementChild?.className, box.textContent, (box.firstElementChild as HTMLElement).title]).toEqual([
      1,
      "prompt-block prompt-block-image-missing",
      IMAGE_MISSING_TEXT,
      "clip.png",
    ]);
  });

  it("records an image that cannot be loaded at info, naming its src", async () => {
    // Arrange
    const capture = captureLogRecords();
    const el = drawFeedImageBlock(create(FeedImageBlockSchema, { src: "/gone.png", alt: "" }));
    document.createElement("div").appendChild(el);

    // Act
    el.dispatchEvent(new Event("error"));

    // Assert
    const record = await forwardedRecord(capture, "feed.draw-image-block.missing");
    expect([record.level.case, record.context?.src]).toEqual(["info", "/gone.png"]);
  });
});

describe("drawFeedUnsupportedBlock", () => {
  it("names the kind, so a maintainer knows what to model next", () => {
    const el = drawFeedUnsupportedBlock(create(FeedUnsupportedBlockSchema, { kind: "audio" }));
    expect(el.textContent).toBe("unsupported block: audio");
  });
});

describe("drawPromptBlockArm", () => {
  it("draws the text arm", () => {
    const el = drawPromptBlockArm(
      { case: "text", value: create(FeedTextBlockSchema, { text: "hi" }) },
      "p",
    );
    expect(el.className).toContain("prompt-block-text");
  });

  it("draws the image arm", () => {
    const el = drawPromptBlockArm(
      { case: "image", value: create(FeedImageBlockSchema, { src: "/a", alt: "" }) },
      "p",
    );
    expect(el.tagName).toBe("IMG");
  });

  it("draws the unsupported arm", () => {
    const el = drawPromptBlockArm(
      { case: "unsupported", value: create(FeedUnsupportedBlockSchema, { kind: "x" }) },
      "p",
    );
    expect(el.className).toContain("prompt-block-unsupported");
  });

  it("draws the quote arm", () => {
    const el = drawPromptBlockArm(
      { case: "quote", value: create(FeedQuoteBlockSchema, { heading: "h", quoted: "q", closing: "c" }) },
      "p",
    );
    expect(el.className).toContain("prompt-block-quote");
  });

  it("refuses an unset arm rather than drawing an empty block", () => {
    expect(() => drawPromptBlockArm({ case: undefined }, "p.block")).toThrow(MalformedView);
  });

  it("names the path of the refusal, so the producer's field is findable", () => {
    try {
      drawPromptBlockArm({ case: undefined }, "p.block");
      expect.unreachable();
    } catch (err) {
      expect((err as MalformedView).path).toBe("p.block");
    }
  });
});

describe("drawPromptBlockArm: an arm this build has no case for", () => {
  it("refuses a block arm a NEWER daemon set, quoting the arm it could not draw", () => {
    // Arrange: the shape a build one schema ahead would hand this switch.
    const arm = { case: "video", value: {} } as never;

    // Act
    let thrown: unknown;
    try {
      drawPromptBlockArm(arm, "p.block");
    } catch (err) {
      thrown = err;
    }

    // Assert
    expect(thrown).toBeInstanceOf(MalformedView);
    expect((thrown as MalformedView).detail).toBe("arm 'video' is not one this build can draw");
  });
});

describe("the records of the drawn blocks", () => {
  it("records a drawn text block at info, a block being drawn exactly once", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedTextBlock(create(FeedTextBlockSchema, { text: "hi" }));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-text-block");
    expect(record.level.case).toBe("info");
  });

  it("records a drawn image block at info, its sibling being drawn the same way", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedImageBlock(create(FeedImageBlockSchema, { src: "/x.png", alt: "a" }));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-image-block");
    expect(record.level.case).toBe("info");
  });
});

describe("promptBlockCopyText", () => {
  it("copies a text block as the markdown the person wrote", () => {
    // Act / Assert
    expect(promptBlockCopyText({ case: "text", value: create(FeedTextBlockSchema, { text: "**fix** `a.ts`" }) }, "b")).toBe(
      "**fix** `a.ts`",
    );
  });

  it("copies an image block as its bracketed alt text", () => {
    // Act / Assert
    expect(promptBlockCopyText({ case: "image", value: create(FeedImageBlockSchema, { alt: "shot.png" }) }, "b")).toBe(
      "[image: shot.png]",
    );
  });

  it("copies an image block with no alt text as a bare bracketed image", () => {
    // Act / Assert
    expect(promptBlockCopyText({ case: "image", value: create(FeedImageBlockSchema, {}) }, "b")).toBe("[image]");
  });

  it("copies an unsupported block as its bracketed kind, as it is drawn", () => {
    // Act / Assert
    expect(
      promptBlockCopyText({ case: "unsupported", value: create(FeedUnsupportedBlockSchema, { kind: "document" }) }, "b"),
    ).toBe("[unsupported block: document]");
  });

  it("copies a quote as its heading, quoted text and closing marker", () => {
    // Arrange
    const quote = create(FeedQuoteBlockSchema, { heading: "⟢ Replying", quoted: "Paris.", closing: "⟢ My message" });
    // Act / Assert
    expect(promptBlockCopyText({ case: "quote", value: quote }, "b")).toBe("⟢ Replying:\n\nParis.\n\n⟢ My message:");
  });

  it("leaves an empty quoted text out of a quote's copy", () => {
    // Arrange
    const quote = create(FeedQuoteBlockSchema, { heading: "⟢ Replying", quoted: "", closing: "⟢ My message" });
    // Act / Assert
    expect(promptBlockCopyText({ case: "quote", value: quote }, "b")).toBe("⟢ Replying:\n\n⟢ My message:");
  });

  it("refuses an unset block, naming its field", () => {
    // Act / Assert
    expect(() => promptBlockCopyText({ case: undefined }, "FeedUserPrompt.body.blocks[0].block")).toThrow(MalformedView);
  });
});

describe("drawPromptCopyControl", () => {
  it("is the one copy control, of the prompt kind, placed as the prompt's", () => {
    // Act
    const control = drawPromptCopyControl([], "p");
    // Assert
    expect([
      control.classList.contains(COPY_CONTROL_CLASS),
      control.classList.contains(PROMPT_COPY_CLASS),
      control.getAttribute(COPY_KIND_ATTRIBUTE),
      control.title,
    ]).toEqual([true, true, "prompt", "Copy prompt"]);
  });

  it("copies every block in order, a blank line apart", () => {
    // Arrange
    const blocks = [
      { block: { case: "quote" as const, value: create(FeedQuoteBlockSchema, { heading: "H", quoted: "Q", closing: "C" }) } },
      { block: { case: "text" as const, value: create(FeedTextBlockSchema, { text: "my words" }) } },
    ];
    // Act
    const control = drawPromptCopyControl(blocks, "p");
    // Assert
    expect(boundCopyText(control).text).toBe("H:\n\nQ\n\nC:\n\nmy words");
  });

  it("refuses an unset block, naming its index under the field", () => {
    // Act / Assert
    expect(() => drawPromptCopyControl([{ block: { case: undefined } }], "FeedUserPrompt.body.blocks")).toThrow(
      "FeedUserPrompt.body.blocks[0].block",
    );
  });
});

describe("the prompt kinds' copy controls", () => {
  it("both hand their body's blocks to drawPromptCopyControl unwrapped by nothing", () => {
    // Arrange
    const here = path.dirname(fileURLToPath(import.meta.url));
    const sources = ["user-prompt.ts", "agent-prompt.ts"].map((file) =>
      readFileSync(path.join(here, "../../../src/feed/rows", file), "utf8"),
    );
    // Act
    const calls = sources.map((code) => /drawPromptCopyControl\(body\.blocks,/.test(code));
    // Assert
    expect(calls).toEqual([true, true]);
  });
});
