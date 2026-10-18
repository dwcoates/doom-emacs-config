// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  FeedAgentPromptAddressSchema,
  FeedAgentPromptQueuedToLiveSchema,
  FeedAgentPromptRefusedSchema,
  FeedAgentPromptResumedRecipientSchema,
  FeedAgentPromptSchema,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { receivedFrom, receivedFromWorkspace, sentTo, sentToWorkspace } from "../addresses.js";
import { PEER_ARROW_CLASS } from "../../../src/feed/rows/peer-arrow.js";
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

function agentPrompt(address = sentTo("vetter"), blocks: unknown[] = [], working = false, awaitingUser = false) {
  return create(FeedAgentPromptSchema, {
    address,
    body: { blocks: blocks as never },
    working,
    awaitingUser,
  });
}

describe("drawFeedAgentPrompt", () => {
  it("wears the prompt bubble's own shape, being the user kind's sibling", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).classList.contains("user")).toBe(true);
  });

  it("wears the agent prompt's hook", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).classList.contains("prompt-agent")).toBe(true);
  });

  it("waves when its row says the turn is working, like the user kind", () => {
    expect(
      drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [], true)).getAttribute(PROMPT_WAVE_ATTRIBUTE),
    ).toBe(PROMPT_WAVE_WORKING);
  });

  it("does not wave when its row says the turn is not working", () => {
    expect(
      drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [], false)).hasAttribute(PROMPT_WAVE_ATTRIBUTE),
    ).toBe(false);
  });

  it("does not wave while its row says the working turn awaits the user", () => {
    expect(
      drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [], true, true)).hasAttribute(PROMPT_WAVE_ATTRIBUTE),
    ).toBe(false);
  });

  it("stamps the wave's phase inline, so a redraw does not jump it back", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute("style")).toMatch(
      /animation-delay:-\d+ms/,
    );
  });

  it("draws the other end's name verbatim", () => {
    const el = drawFeedAgentPrompt(agentPrompt(receivedFrom("the main agent")));
    expect(el.querySelector(".prompt-address")?.textContent).toBe("the main agent");
  });

  it("draws the shared block vocabulary in the capped body", () => {
    const el = drawFeedAgentPrompt(
      agentPrompt(sentTo("vetter"), [{ block: { case: "text", value: { text: "go" } } }]),
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
    const msg = agentPrompt(sentTo("vetter"));
    msg.delivery = { case: arm, value: value() } as never;
    // Act
    const el = drawFeedAgentPrompt(msg);
    // Assert
    expect(
      [...el.querySelectorAll(`.${BUBBLE_STRIP_CLASS}`)].map((c) => c.textContent),
    ).toEqual(["vetter"]);
  });

  it("refuses a prompt with no address, rather than drawing an unaddressed one", () => {
    const msg = create(FeedAgentPromptSchema, { body: { blocks: [] } });
    expect(() => drawFeedAgentPrompt(msg)).toThrow(MalformedView);
  });

  it("refuses a prompt with no body", () => {
    const msg = create(FeedAgentPromptSchema, { address: sentTo("x") });
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

  it("is the agent variant", () => {
    expect(drawFeedAgentPrompt(agentPrompt()).getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("agent");
  });

  it("folds at the subagent prompt cap, which has no line limit", () => {
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
        address: sentTo("vetter"),
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

describe("drawFeedAgentPrompt: the arrow says which way it went", () => {
  /** The header's children, as "name" and "arrow-<direction>". */
  function headerParts(el: HTMLElement): string[] {
    const header = el.querySelector(".prompt-address");
    return [...(header?.children ?? [])].map((child) =>
      child.classList.contains(PEER_ARROW_CLASS) ? `arrow-${child.getAttribute("data-direction")}` : child.textContent ?? "",
    );
  }

  it("draws a prompt to a subagent as its name, then a right arrow", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(sentTo("vetter"))))).toEqual(["vetter", "arrow-out"]);
  });

  it("draws a prompt from the lead as a left arrow, then its name", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(receivedFrom("the main agent"))))).toEqual([
      "arrow-in",
      "the main agent",
    ]);
  });

  it("draws a prompt from another workspace's agent as a left arrow, then its name", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(receivedFromWorkspace("parser-fix"))))).toEqual([
      "arrow-in",
      "parser-fix",
    ]);
  });

  it("draws a send to another workspace as its name, then a right arrow", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(sentToWorkspace("review-bot"))))).toEqual([
      "review-bot",
      "arrow-out",
    ]);
  });

  it("draws no arrow as text: the arrow is the icon alone", () => {
    expect(drawFeedAgentPrompt(agentPrompt(sentTo("vetter"))).querySelector(".prompt-address")?.textContent).not.toMatch(
      /[→←]/,
    );
  });

  it("refuses a header that names no direction", () => {
    const msg = agentPrompt(create(FeedAgentPromptAddressSchema, { name: { text: "vetter" } }));
    expect(() => drawFeedAgentPrompt(msg)).toThrow(MalformedView);
  });

  it("refuses a header that names no one", () => {
    const address = sentTo("vetter");
    address.name = undefined;
    expect(() => drawFeedAgentPrompt(agentPrompt(address))).toThrow(MalformedView);
  });
});
