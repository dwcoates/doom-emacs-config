// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { FeedUserPromptSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedUserPrompt } from "../../../src/feed/rows/user-prompt.js";

/** A prompt with the given blocks. */
function prompt(blocks: unknown[], author = "You") {
  return create(FeedUserPromptSchema, {
    author: { label: author },
    result: { case: "success", value: { body: { blocks: blocks as never } } },
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

  it("draws the author label the daemon resolved", () => {
    const el = drawFeedUserPrompt(prompt([], "Explore"));
    expect(el.querySelector(".prompt-author")?.textContent).toBe("Explore");
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
