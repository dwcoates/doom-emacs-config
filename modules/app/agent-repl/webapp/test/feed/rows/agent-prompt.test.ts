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
import { DELIVERY_WORDS, drawFeedAgentPrompt } from "../../../src/feed/rows/agent-prompt.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import {
  PROMPT_WAVE_ATTRIBUTE,
  PROMPT_WAVE_WORKING,
} from "../../../src/breathing.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  PROMPT_CAP_LINES,
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

  it("marks the sender's row queued when the recipient was already live", () => {
    const msg = agentPrompt();
    msg.delivery = { case: "queuedToLive", value: create(FeedAgentPromptQueuedToLiveSchema, {}) };
    const el = drawFeedAgentPrompt(msg);
    expect(el.querySelector("[data-delivery]")?.getAttribute("data-delivery")).toBe("queuedToLive");
  });

  it("marks the sender's row resumed when the send woke the recipient", () => {
    const msg = agentPrompt();
    msg.delivery = {
      case: "resumedRecipient",
      value: create(FeedAgentPromptResumedRecipientSchema, {}),
    };
    const el = drawFeedAgentPrompt(msg);
    expect(el.querySelector("[data-delivery]")?.textContent).toBe(
      DELIVERY_WORDS.resumedRecipient,
    );
  });

  it("marks the sender's row refused when the send reached nobody", () => {
    const msg = agentPrompt();
    msg.delivery = {
      case: "refused",
      value: create(FeedAgentPromptRefusedSchema, {}),
    };
    const el = drawFeedAgentPrompt(msg);
    expect(el.querySelector("[data-delivery]")?.getAttribute("data-delivery")).toBe("refused");
  });

  it("draws a refusal apart from a landing, so the two are not read alike", () => {
    const msg = agentPrompt();
    msg.delivery = {
      case: "refused",
      value: create(FeedAgentPromptRefusedSchema, {}),
    };
    const el = drawFeedAgentPrompt(msg);
    expect(el.querySelector("[data-delivery]")?.classList.contains("refused")).toBe(true);
  });

  it("draws the producer's refusal words verbatim beside the marker", () => {
    const msg = agentPrompt();
    msg.delivery = {
      case: "refused",
      value: create(FeedAgentPromptRefusedSchema, {
        reason: { text: "The agent was stopped by the user." },
      }),
    };
    const el = drawFeedAgentPrompt(msg);
    expect(el.querySelector(".prompt-refusal-reason")?.textContent).toBe(
      "The agent was stopped by the user.",
    );
  });

  it("draws a refusal that gave no account with the marker and no reason", () => {
    const msg = agentPrompt();
    msg.delivery = {
      case: "refused",
      value: create(FeedAgentPromptRefusedSchema, {}),
    };
    const el = drawFeedAgentPrompt(msg);
    expect(el.querySelector("[data-delivery]")?.textContent).toBe(DELIVERY_WORDS.refused);
    expect(el.querySelector(".prompt-refusal-reason")).toBeNull();
  });

  it("draws no delivery marker on the recipient's copy, whose delivery is unset", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).querySelector("[data-delivery]")).toBeNull();
  });

  it("refuses a delivery arm this build does not know", () => {
    const msg = agentPrompt();
    msg.delivery = { case: "invented" as never, value: {} as never };
    expect(() => drawFeedAgentPrompt(msg)).toThrow(MalformedView);
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

  it("collapses at the five-line prompt cap, like a person's own prompt", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe(String(PROMPT_CAP_LINES));
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

describe("drawFeedAgentPrompt: the delivery line", () => {
  it("rides the header strip, under the address", () => {
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
      "prompt-delivery",
      "bubble-scroll",
    ]);
  });
});
