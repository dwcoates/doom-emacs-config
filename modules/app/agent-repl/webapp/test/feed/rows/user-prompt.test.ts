// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedUserPromptSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedUserPrompt } from "../../../src/feed/rows/user-prompt.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
} from "../../../src/breathing.js";

/** A prompt with the given blocks. */
function prompt(blocks: unknown[], author = "You", working = false) {
  return create(FeedUserPromptSchema, {
    author: { label: author },
    result: { case: "success", value: { body: { blocks: blocks as never } } },
    working,
  });
}

describe("drawFeedUserPrompt: the bubble", () => {
  it("keeps the existing prompt-bubble classes, unchanged by the port", () => {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { text: "hi" } } }]));
    expect(el.className).toBe("bubble user");
  });

  it("stamps the wave's phase inline, so a redraw does not jump it back", () => {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { text: "hi" } } }]));
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

  it("draws no author label, however the daemon resolved it", () => {
    const el = drawFeedUserPrompt(prompt([], "Explore"));
    expect(el.querySelector(".prompt-author")).toBeNull();
  });

  it("puts the blocks in the capped body, which the stylesheet already caps", () => {
    const el = drawFeedUserPrompt(prompt([{ block: { case: "text", value: { text: "hi" } } }]));
    expect(el.querySelector(".bubble-body")).not.toBeNull();
  });
});

describe("drawFeedUserPrompt: the body", () => {
  it("draws every block, in the composed order", () => {
    const el = drawFeedUserPrompt(
      prompt([
        { block: { case: "text", value: { text: "one" } } },
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
    drawFeedUserPrompt(prompt([{ block: { case: "text", value: { text: "hi" } } }]));
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-user-prompt");
    expect(record.level.case).toBe("info");
  });
});
