/**
 * agent-prompt — the prompt one agent addressed to another.
 *
 * The user kind's SIBLING, differing only in author: one component draws both
 * ends of a delivery between two agents of one session (the sender's outgoing
 * send and the recipient's delivered prompt), and the sender's end of a send
 * to ANOTHER Claude session, because the daemon composes the header for
 * whichever feed it resolved. So this reuses the prompt bubble's own shape and
 * body vocabulary.
 *
 * EVERY AGENT-TO-AGENT BUBBLE FOLDS AT TWO LINES (owner request, 2026-11-12): its
 * collapsed text is clamped to two lines ending in `…`, and a click opens it.
 *
 * EVERY AGENT-TO-AGENT BUBBLE IS HEADED ONE WAY (owner spec, 2026-10-17): no
 * border, and a header of the other end's name and an arrow icon. A prompt
 * that went OUT reads the name then a right arrow; one that came IN reads a
 * left arrow then the name. The arrow's side and direction alone say which way
 * it went, and the daemon resolves the name.
 *
 * WHY NOT ONE FUNCTION FOR BOTH KINDS. They are two messages with two
 * different fields where the attribution goes (`author` vs `address`), and the
 * mapping is one base function per message; sharing the BODY vocabulary is what
 * keeps the two from drifting, and that sharing is `blocks.ts`.
 */
import { log } from "../../log.js";
import { AGENT_PROMPT_CAP_LINES, BUBBLE_MORE_ELLIPSIS, drawBubble } from "../../bubble/draw.js";
import { requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedAgentPrompt,
  FeedAgentPromptAddress,
  FeedAgentPromptBlock,
  FeedAgentPromptBody,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { questionControl } from "../response-question.js";
import { drawPromptBlockArm, drawPromptCopyControl } from "./blocks.js";
import { reserveCopyRoom } from "../../bubble/copy-room.js";
import { drawPeerArrow, type PeerArrowDirection } from "./peer-arrow.js";
import { MENTION_TEXT_CLASS, linkWorkspaceMentionsInText } from "../workspace-mentions.js";

const PATH = "FeedAgentPrompt";

/**
 * The agent-addressed prompt bubble: the prompt bubble's chrome in the agent
 * variant, with the header where the user kind draws its author.
 */
export function drawFeedAgentPrompt(msg: FeedAgentPrompt, row: string, previous?: HTMLElement): HTMLElement {
  const address = requireMessage(msg.address, `${PATH}.address`);
  log.info("drawing an agent prompt row", {
    operation: "feed.draw-agent-prompt",
    context: { direction: address.direction.case ?? "unset" },
  });
  // THE HEADER STRIP IS THE ADDRESS ALONE, and the body hangs in the one
  // scroll box beneath it, exactly as a person's own prompt does. THE DELIVERY
  // OUTCOME IS NOT DRAWN (owner ruling, 2026-10-14): the strip states who the
  // prompt is addressed to and nothing under it. PREVIOUS, the row's last draw,
  // is updated in place.
  const strip = [drawFeedAgentPromptAddress(address)];
  const body = requireMessage(msg.body, `${PATH}.body`);
  const content = drawFeedAgentPromptBody(body, `${PATH}.body`);
  reserveCopyRoom(content);
  return drawBubble(
    {
      role: "prompt",
      variant: "agent",
      hooks: ["user", "prompt-agent"],
      strip,
      corner: [questionControl(row), drawPromptCopyControl(body.blocks, `${PATH}.body.blocks`)],
      content,
      capLines: AGENT_PROMPT_CAP_LINES,
      more: BUBBLE_MORE_ELLIPSIS,
    },
    previous,
  ).bubble;
}

/**
 * The header: the other end's NAME, drawn verbatim, and the arrow on the side
 * the direction names — after the name for a prompt that went out, before it
 * for one that came in. Which kind of agent the other end is changes nothing
 * drawn: the daemon resolved the name for it.
 *
 * A NAME THAT IS A WORKSPACE'S IS A MENTION (owner request, 2026-11-10): it
 * wears the mention purple and switches to that workspace on a click, exactly
 * as a workspace's name in a prompt's text does (workspace-mentions.ts), and
 * it is re-linked with every push of the page's mention targets.
 */
export function drawFeedAgentPromptAddress(address: FeedAgentPromptAddress): HTMLElement {
  const text = requireMessage(address.name, `${PATH}.address.name`).text;
  switch (address.direction.case) {
    case "sent":
      return drawPromptAddressHeader(text, "out", true);
    case "received":
      return drawPromptAddressHeader(text, "in", true);
    default:
      return unreachableArm(`${PATH}.address.direction`, String(address.direction.case));
  }
}

/**
 * THE ONE HEADER OF A PROMPT BUBBLE THAT NAMES THE OTHER END: the name beside
 * the arrow on the side DIRECTION names, in the header's one font, size and
 * place. An agent-to-agent prompt and a prompt the daemon itself sent
 * (user-prompt.ts, "Zettel <subsystem>") both draw it here, so the two can
 * never head a bubble differently. MENTION makes the name a workspace
 * mention; a subsystem's name is not one.
 */
export function drawPromptAddressHeader(text: string, direction: PeerArrowDirection, mention: boolean): HTMLElement {
  const el = document.createElement("span");
  el.className = "prompt-author prompt-address";
  const name = document.createElement("span");
  name.className = mention ? `peer-name ${MENTION_TEXT_CLASS}` : "peer-name";
  name.textContent = text;
  if (mention) linkWorkspaceMentionsInText(name);
  if (direction === "out") el.append(name, drawPeerArrow("out"));
  else el.append(drawPeerArrow("in"), name);
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
