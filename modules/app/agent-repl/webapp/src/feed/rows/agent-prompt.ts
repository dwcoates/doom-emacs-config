/**
 * agent-prompt — the prompt one agent addressed to another.
 *
 * The user kind's SIBLING, differing only in author: one component draws both
 * ends of the delivery (the sender's outgoing send and the recipient's
 * delivered prompt), because the daemon composes the address line for whichever
 * feed it resolved. So this reuses the prompt bubble's own shape and body
 * vocabulary, and wears the one AMBER agent-to-agent border
 * (`--agent-prompt-border`, owner ruling 2026-09-23), as a peer message does.
 *
 * WHY NOT ONE FUNCTION FOR BOTH KINDS. They are two messages with two
 * different fields where the attribution goes (`author` vs `address`), and the
 * mapping is one base function per message; sharing the BODY vocabulary is what
 * keeps the two from drifting, and that sharing is `blocks.ts`.
 */
import { log } from "../../log.js";
import { AGENT_PROMPT_CAP_LINES, drawBubble } from "../../bubble/draw.js";
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
 * The agent-addressed prompt bubble: the prompt bubble's chrome in the agent
 * variant (the amber border), with the composed address line where the user kind
 * draws its author.
 */
export function drawFeedAgentPrompt(msg: FeedAgentPrompt, previous?: HTMLElement): HTMLElement {
  log.info("drawing an agent prompt row", {
    operation: "feed.draw-agent-prompt",
    context: {},
  });
  // THE HEADER STRIP IS THE ADDRESS ALONE, and the body hangs in the one
  // scroll box beneath it, exactly as a person's own prompt does. THE DELIVERY
  // OUTCOME IS NOT DRAWN (owner ruling, 2026-10-14): the strip states who the
  // prompt is addressed to and nothing under it. PREVIOUS, the row's last draw,
  // is updated in place.
  const strip = [drawFeedAgentPromptAddress(requireMessage(msg.address, `${PATH}.address`))];
  return drawBubble(
    {
      role: "prompt",
      variant: "agent",
      hooks: ["user", "prompt-agent"],
      working: msg.working,
      strip,
      content: drawFeedAgentPromptBody(requireMessage(msg.body, `${PATH}.body`), `${PATH}.body`),
      capLines: AGENT_PROMPT_CAP_LINES,
    },
    previous,
  ).bubble;
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

/** The prompt's blocks, in composed order: the bubble's content. */
export function drawFeedAgentPromptBody(body: FeedAgentPromptBody, path: string): HTMLElement[] {
  return body.blocks.map((block, index) =>
    drawFeedAgentPromptBlock(block, `${path}.blocks[${index}]`),
  );
}

/** One block of an agent prompt — the shared drawn block vocabulary. */
export function drawFeedAgentPromptBlock(block: FeedAgentPromptBlock, path: string): HTMLElement {
  return drawPromptBlockArm(block.block, `${path}.block`);
}
