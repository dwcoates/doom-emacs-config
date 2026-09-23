// @vitest-environment jsdom
import stylesheet from "../../../src/styles.css?raw";
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedImageBlockSchema,
  FeedTextBlockSchema,
  FeedUnsupportedBlockSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  drawFeedImageBlock,
  drawFeedTextBlock,
  drawFeedUnsupportedBlock,
  drawPromptBlockArm,
} from "../../../src/feed/rows/blocks.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
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

  it("draws an empty block as an empty body rather than refusing", () => {
    expect(painted(drawFeedTextBlock(create(FeedTextBlockSchema, { text: "" }))).textContent).toBe("");
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

  it("leaves the height to follow the capped width, so the picture is not squashed", () => {
    // Arrange.
    const rule = stylesheet.match(/\.prompt-block-image\s*\{[^}]*\}/)?.[0] ?? "";

    // Act / Assert.
    expect(rule).toContain("height: auto");
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
