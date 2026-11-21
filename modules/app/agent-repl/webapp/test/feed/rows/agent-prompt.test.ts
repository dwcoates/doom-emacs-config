// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { isStill } from "../../bubble/still.js";
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
import { BUBBLE_COPY_CLASS, COPY_ROOM_CLASS } from "../../../src/bubble/copy-room.js";
import { boundCopyText } from "../../../src/copy-control.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";
import {
  BUBBLE_CAP_ATTRIBUTE,
  BUBBLE_MORE_ATTRIBUTE,
  BUBBLE_MORE_ELLIPSIS,
  AGENT_PROMPT_CAP_LINES,
  BUBBLE_ROLE_ATTRIBUTE,
  BUBBLE_STRIP_CLASS,
  BUBBLE_VARIANT_ATTRIBUTE,
} from "../../../src/bubble/draw.js";
import { servedProse } from "../../prose.js";

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
    expect(drawFeedAgentPrompt(agentPrompt(), "row-1").classList.contains("user")).toBe(true);
  });

  it("wears the agent prompt's hook", () => {
    expect(drawFeedAgentPrompt(agentPrompt(), "row-1").classList.contains("prompt-agent")).toBe(true);
  });

  it.each([
    ["working", true, false],
    ["working and awaiting the user", true, true],
    ["at rest", false, false],
  ] as const)("draws a %s prompt with no wave marker and no inline animation", (_label, working, awaitingUser) => {
    // Arrange / Act
    const el = drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [], working, awaitingUser), "row-1");

    // Assert
    expect(isStill(el)).toBe(true);
  });

  it("draws the other end's name verbatim", () => {
    const el = drawFeedAgentPrompt(agentPrompt(receivedFrom("the main agent")), "row-1");
    expect(el.querySelector(".prompt-address")?.textContent).toBe("the main agent");
  });

  it("draws the shared block vocabulary in the capped body", () => {
    const el = drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [{ block: { case: "text", value: { prose: servedProse("go") } } }]), "row-1");
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
    const el = drawFeedAgentPrompt(msg, "row-1");
    // Assert
    expect(
      [...el.querySelectorAll(`.${BUBBLE_STRIP_CLASS}`)].map((c) => c.textContent),
    ).toEqual(["vetter"]);
  });

  it("refuses a prompt with no address, rather than drawing an unaddressed one", () => {
    const msg = create(FeedAgentPromptSchema, { body: { blocks: [] } });
    expect(() => drawFeedAgentPrompt(msg, "row-1")).toThrow(MalformedView);
  });

  it("refuses a prompt with no body", () => {
    const msg = create(FeedAgentPromptSchema, { address: sentTo("x") });
    expect(() => drawFeedAgentPrompt(msg, "row-1")).toThrow(MalformedView);
  });
});

describe("drawFeedAgentPrompt: the record of the row", () => {
  it("records the drawn row at info, a row being drawn exactly once", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    // ACT
    drawFeedAgentPrompt(agentPrompt(), "row-1");
    // ASSERT
    const record = await forwardedRecord(capture, "feed.draw-agent-prompt");
    expect(record.level.case).toBe("info");
  });
});

describe("drawFeedAgentPrompt: its spec", () => {
  it("is a prompt-role bubble", () => {
    expect(drawFeedAgentPrompt(agentPrompt(), "row-1").getAttribute(BUBBLE_ROLE_ATTRIBUTE)).toBe("prompt");
  });

  it("is the agent variant", () => {
    expect(drawFeedAgentPrompt(agentPrompt(), "row-1").getAttribute(BUBBLE_VARIANT_ATTRIBUTE)).toBe("agent");
  });

  it("folds at two lines", () => {
    expect(drawFeedAgentPrompt(agentPrompt(), "row-1").getAttribute(BUBBLE_CAP_ATTRIBUTE)).toBe("2");
  });

  it("states the agent prompt cap as the two-line cap", () => {
    expect(AGENT_PROMPT_CAP_LINES).toBe(2);
  });

  it("ends its folded second line in the ellipsis rather than the fade", () => {
    expect(drawFeedAgentPrompt(agentPrompt(), "row-1").getAttribute(BUBBLE_MORE_ATTRIBUTE)).toBe(BUBBLE_MORE_ELLIPSIS);
  });

  it("puts the address line in the header strip", () => {
    const el = drawFeedAgentPrompt(agentPrompt(), "row-1");
    expect(el.querySelector(".prompt-address")?.classList.contains(BUBBLE_STRIP_CLASS)).toBe(true);
  });
});

describe("drawFeedAgentPrompt: a re-push", () => {
  it("updates the previous draw in place", () => {
    // Arrange
    const first = drawFeedAgentPrompt(agentPrompt(), "row-1");
    // Act
    const again = drawFeedAgentPrompt(agentPrompt(), "row-1", first);
    // Assert
    expect(again).toBe(first);
  });
});

describe("drawFeedAgentPrompt: the header strip", () => {
  it("holds the address and then the body, with no delivery line between", () => {
    // Arrange / Act
    const el = drawFeedAgentPrompt(create(FeedAgentPromptSchema, {
        address: sentTo("vetter"),
        body: { blocks: [] },
        delivery: { case: "queuedToLive", value: create(FeedAgentPromptQueuedToLiveSchema, {}) },
      }), "row-1");
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
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(sentTo("vetter")), "row-1"))).toEqual(["vetter", "arrow-out"]);
  });

  it("draws a prompt from the lead as a left arrow, then its name", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(receivedFrom("the main agent")), "row-1"))).toEqual([
      "arrow-in",
      "the main agent",
    ]);
  });

  it("draws a prompt from another workspace's agent as a left arrow, then its name", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(receivedFromWorkspace("parser-fix")), "row-1"))).toEqual([
      "arrow-in",
      "parser-fix",
    ]);
  });

  it("draws a send to another workspace as its name, then a right arrow", () => {
    expect(headerParts(drawFeedAgentPrompt(agentPrompt(sentToWorkspace("review-bot")), "row-1"))).toEqual([
      "review-bot",
      "arrow-out",
    ]);
  });

  it("draws no arrow as text: the arrow is the icon alone", () => {
    expect(drawFeedAgentPrompt(agentPrompt(sentTo("vetter")), "row-1").querySelector(".prompt-address")?.textContent).not.toMatch(
      /[→←]/,
    );
  });

  it("refuses a header that names no direction", () => {
    const msg = agentPrompt(create(FeedAgentPromptAddressSchema, { name: { text: "vetter" } }));
    expect(() => drawFeedAgentPrompt(msg, "row-1")).toThrow(MalformedView);
  });

  it("refuses a header that names no one", () => {
    const address = sentTo("vetter");
    address.name = undefined;
    expect(() => drawFeedAgentPrompt(agentPrompt(address), "row-1")).toThrow(MalformedView);
  });
});

describe("drawFeedAgentPrompt: the copy control", () => {
  it("copies the whole prompt's blocks", () => {
    // Act
    const el = drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [
        { block: { case: "text", value: { prose: servedProse("first") } } },
        { block: { case: "text", value: { prose: servedProse("second") } } },
      ]), "row-1");
    // Assert
    const control = el.querySelector(`.${BUBBLE_COPY_CLASS}`);
    expect(control === null ? null : boundCopyText(control).text).toBe("first\n\nsecond");
  });

  it("opens the prompt's first line of text with its room, never the address", () => {
    // Act
    const el = drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [{ block: { case: "text", value: { prose: servedProse("go") } } }]), "row-1");
    document.body.append(el);
    // Assert
    const ghosts = [...el.querySelectorAll(`.${COPY_ROOM_CLASS}`)];
    el.remove();
    expect(ghosts.map((g) => [g.parentElement?.tagName, g.parentElement?.textContent])).toEqual([["P", "go"]]);
  });

  it("leaves the address out of what it copies", () => {
    // Act
    const el = drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [{ block: { case: "text", value: { prose: servedProse("go") } } }]), "row-1");
    // Assert
    const control = el.querySelector(`.${BUBBLE_COPY_CLASS}`);
    expect(control === null ? null : boundCopyText(control).text).toBe("go");
  });
});

describe("drawFeedAgentPrompt: the question control", () => {
  it("carries the control for its row in the corner beside the copy control", () => {
    // Act
    const el = drawFeedAgentPrompt(agentPrompt(sentTo("vetter"), [{ block: { case: "text", value: { prose: servedProse("go") } } }]), "row-9");
    // Assert
    expect(el.querySelector(".question-control.bubble-question")?.getAttribute("data-asks-about")).toBe("row-9");
  });
});
