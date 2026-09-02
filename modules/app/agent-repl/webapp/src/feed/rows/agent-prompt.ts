/**
 * agent-prompt — the prompt one agent addressed to another.
 *
 * The user kind's SIBLING, differing only in author: one component draws both
 * ends of the delivery (the sender's outgoing send and the recipient's
 * delivered prompt), because the daemon composes the address line for whichever
 * feed it resolved. So this reuses the prompt bubble's own shape and body
 * vocabulary, and wears the ORANGE border the schema names for it.
 *
 * WHY NOT ONE FUNCTION FOR BOTH KINDS. They are two messages with two
 * different fields where the attribution goes (`author` vs `address`), and the
 * mapping is one base function per message; sharing the BODY vocabulary is what
 * keeps the two from drifting, and that sharing is `blocks.ts`.
 */
import { bubbleWaveStyle } from "../../breathing.js";
import { log } from "../../log.js";
import { requireMessage } from "../../rpc/strict.js";
import type {
  FeedAgentPrompt,
  FeedAgentPromptAddress,
  FeedAgentPromptBlock,
  FeedAgentPromptBody,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawPromptBlockArm } from "./blocks.js";

const PATH = "FeedAgentPrompt";

/**
 * The agent-addressed prompt bubble: the prompt bubble's chrome plus the
 * orange marker class, with the composed address line where the user kind
 * draws its author.
 */
export function drawFeedAgentPrompt(msg: FeedAgentPrompt): HTMLElement {
  log("debug", "drawing an agent prompt row", {
    operation: "feed.draw-agent-prompt",
    context: {},
  });
  const bubble = document.createElement("div");
  bubble.className = "bubble user prompt-agent";
  bubble.setAttribute("style", bubbleWaveStyle());
  bubble.append(drawFeedAgentPromptAddress(requireMessage(msg.address, `${PATH}.address`)));
  bubble.append(
    drawFeedAgentPromptBody(requireMessage(msg.body, `${PATH}.body`), `${PATH}.body`),
  );
  return bubble;
}

/**
 * The address line ("→ Explore", "from Plan"), drawn verbatim. Composed by the
 * daemon for the feed it resolved, so this end never derives a direction.
 */
export function drawFeedAgentPromptAddress(address: FeedAgentPromptAddress): HTMLElement {
  const el = document.createElement("span");
  el.className = "prompt-author prompt-address";
  el.textContent = address.text;
  return el;
}

/** The prompt's blocks, in composed order, capped like any long prompt. */
export function drawFeedAgentPromptBody(body: FeedAgentPromptBody, path: string): HTMLElement {
  const el = document.createElement("div");
  el.className = "bubble-body";
  body.blocks.forEach((block, index) => {
    el.append(drawFeedAgentPromptBlock(block, `${path}.blocks[${index}]`));
  });
  return el;
}

/** One block of an agent prompt — the shared drawn block vocabulary. */
export function drawFeedAgentPromptBlock(block: FeedAgentPromptBlock, path: string): HTMLElement {
  return drawPromptBlockArm(block.block, `${path}.block`);
}
