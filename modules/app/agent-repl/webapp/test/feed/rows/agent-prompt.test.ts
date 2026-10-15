// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedAgentPromptQueuedToLiveSchema,
  FeedAgentPromptRefusedSchema,
  FeedAgentPromptResumedRecipientSchema,
  FeedAgentPromptSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawFeedAgentPrompt } from "../../../src/feed/rows/agent-prompt.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
} from "../../../src/breathing.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  AGENT_PROMPT_CAP_LINES,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANT_ATTRIBUTE,
} from "../../../src/bubble/draw.js";

function agentPrompt(address = "→ Explore", blocks: unknown[] = [], working = false) {
  return create(FeedAgentPromptSchema, {
    address: { text: address },
    body: { blocks: blocks as never },
    working,
  });
}

describe("drawFeedAgentPrompt", () => {
  it("wears the prompt bubble's own shape, being the user kind's sibling", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).classList.contains("user")).toBe(true);
  });

  it("marks itself for the ORANGE border the schema names for it", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).classList.contains("prompt-agent")).toBe(true);
  });

  it("waves when its row says the turn is working, like the user kind", () => {
    expect(
      drawFeedAgentPrompt(agentPrompt("→ Explore", [], true)).getAttribute(PROMPT_WAVE_ATTRIBUTE),
    ).toBe(PROMPT_WAVE_WORKING);
  });

  it("does not wave when its row says the turn is not working", () => {
    expect(
      drawFeedAgentPrompt(agentPrompt("→ Explore", [], false)).hasAttribute(PROMPT_WAVE_ATTRIBUTE),
    ).toBe(false);
  });

  it("stamps the wave's phase inline, so a redraw does not jump it back", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute("style")).toMatch(
      /animation-delay:-\d+ms/,
    );
  });

  it("draws the composed address line verbatim, whichever end this feed is", () => {
    const el = drawFeedAgentPrompt(agentPrompt("from Plan"));
    expect(el.querySelector(".prompt-address")?.textContent).toBe("from Plan");
  });

  it("draws the shared block vocabulary in the capped body", () => {
    const el = drawFeedAgentPrompt(
      agentPrompt("→ Explore", [{ block: { case: "text", value: { text: "go" } } }]),
    );
    expect(el.querySelector(".bubble-body")?.children).toHaveLength(1);
  });

  it.each([
    ["queuedToLive", () => create(FeedAgentPromptQueuedToLiveSchema, {})],
    ["resumedRecipient", () => create(FeedAgentPromptResumedRecipientSchema, {})],
    [
      "refused",
      () => create(FeedAgentPromptRefusedSchema, { reason: { text: "The agent was stopped." } }),
    ],
  ] as const)("draws no delivery line for a %s send, the strip being the address alone", (arm, value) => {
    // Arrange
    const msg = agentPrompt("→ Explore");
    msg.delivery = { case: arm, value: value() } as never;
    // Act
    const el = drawFeedAgentPrompt(msg);
    // Assert
    expect(
      [...el.querySelectorAll(`.${BUBBLE_STRIP_CLASS}`)].map((c) => c.textContent),
    ).toEqual(["→ Explore"]);
  });

  it("refuses a prompt with no address, rather than drawing an unaddressed one", () => {
    const msg = create(FeedAgentPromptSchema, { body: { blocks: [] } });
    expect(() => drawFeedAgentPrompt(msg)).toThrow(MalformedView);
  });

  it("refuses a prompt with no body", () => {
    const msg = create(FeedAgentPromptSchema, { address: { text: "→ x" } });
    expect(() => drawFeedAgentPrompt(msg)).toThrow(MalformedView);
  });
});

describe("drawFeedAgentPrompt: the record of the row", () => {
  it("records the drawn row at info, a row being drawn exactly once", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedAgentPrompt(agentPrompt());
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-agent-prompt");
    expect(record.level.case).toBe("info");
  });
});

describe("drawFeedAgentPrompt: its spec", () => {
  it("is a prompt-role bubble", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("prompt");
  });

  it("is the agent variant, whose border is its own", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("agent");
  });

  it("collapses at the five-line subagent prompt cap", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(String(AGENT_PROMPT_CAP_LINES));
  });

  it("puts the address line in the header strip", () => {
    const el = drawFeedAgentPrompt(agentPrompt());
    expect(el.querySelector(".prompt-address")?.classList.contains(BUBBLE_STRIP_CLASS)).toBe(true);
  });
});

describe("drawFeedAgentPrompt: a re-push", () => {
  it("updates the previous draw in place", () => {
    // Arrange
    const first = drawFeedAgentPrompt(agentPrompt());
    // Act
    const again = drawFeedAgentPrompt(agentPrompt(), first);
    // Assert
    expect(again).toBe(first);
  });
});

describe("drawFeedAgentPrompt: the header strip", () => {
  it("holds the address and then the body, with no delivery line between", () => {
    // Arrange / Act
    const el = drawFeedAgentPrompt(
      create(FeedAgentPromptSchema, {
        address: { text: "→ Explore" },
        body: { blocks: [] },
        delivery: { case: "queuedToLive", value: create(FeedAgentPromptQueuedToLiveSchema, {}) },
      }),
    );
    // Assert
    expect([...el.children].map((c) => c.classList[0])).toEqual([
      "prompt-author",
      "bubble-scroll",
    ]);
  });
});
