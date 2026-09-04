// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedAgentPromptQueuedToLiveSchema,
  FeedAgentPromptResumedRecipientSchema,
  FeedAgentPromptSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { DELIVERY_WORDS, drawFeedAgentPrompt } from "../../../src/feed/rows/agent-prompt.js";

function agentPrompt(address = "→ Explore", blocks: unknown[] = []) {
  return create(FeedAgentPromptSchema, {
    address: { text: address },
    body: { blocks: blocks as never },
  });
}

describe("drawFeedAgentPrompt", () => {
  it("wears the prompt bubble's own shape, being the user kind's sibling", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).classList.contains("user")).toBe(true);
  });

  it("marks itself for the ORANGE border the schema names for it", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).classList.contains("prompt-agent")).toBe(true);
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
