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
import { log } from "../../log.js";
import { drawBubble } from "../../bubble/draw.js";
import { armName } from "../renderers.js";
import { requireMessage, unreachableArm } from "../../rpc/strict.js";
import type {
  FeedAgentPrompt,
  FeedAgentPromptAddress,
  FeedAgentPromptBlock,
  FeedAgentPromptBody,
  FeedAgentPromptRefused,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawPromptBlockArm } from "./blocks.js";

const PATH = "FeedAgentPrompt";

/**
 * What each delivery arm says.
 *
 * THE SENDER'S ROW ONLY. `delivery` is unset on the recipient's copy of the
 * same prompt — the outcome is a fact about the SEND, and stating it on the
 * delivered row would tell the recipient something about its own resumption
 * that the sender's feed is the one authority on.
 */
export const DELIVERY_WORDS = {
  queuedToLive: "queued for the live recipient",
  resumedRecipient: "resumed the recipient",
  // NOT A LANDING. The other two say where the message got to; this one says
  // it got nowhere, and says so in the past tense so no reader waits for it.
  refused: "refused — never delivered",
} as const satisfies Record<string, string>;

/**
 * The agent-addressed prompt bubble: the prompt bubble's chrome plus the
 * orange marker class, with the composed address line where the user kind
 * draws its author.
 */
export function drawFeedAgentPrompt(msg: FeedAgentPrompt, previous?: HTMLElement): HTMLElement {
  log.info("drawing an agent prompt row", {
    operation: "feed.draw-agent-prompt",
    context: {},
  });
  // AN UNSET DELIVERY DRAWS NOTHING. The oneof is absent on every recipient
  // copy, which is not a missing fact but the absence of one.
  const footer = msg.delivery.case === undefined ? [] : [drawFeedAgentPromptDelivery(msg.delivery)];
  // The address line is the metadata strip and the body hangs in the shared
  // scroll box beneath it, exactly as a person's own prompt does.
  // PREVIOUS, the row's last draw, is updated in place (drawBubble).
  return drawBubble(
    {
      role: "prompt",
      variant: "agent",
      hooks: ["user", "prompt-agent"],
      working: msg.working,
      strip: [drawFeedAgentPromptAddress(requireMessage(msg.address, `${PATH}.address`))],
      content: drawFeedAgentPromptBody(requireMessage(msg.body, `${PATH}.body`), `${PATH}.body`),
      footer,
      capLines: "feed",
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

/**
 * The delivery outcome marker on the SENDER's row.
 *
 * Stated because a resumption is the cause of another agent's renewed activity
 * and renewed cost (feed.proto, FeedAgentPromptResumedRecipient), which the
 * reader cannot otherwise attribute to their own send.
 */
export function drawFeedAgentPromptDelivery(
  delivery: FeedAgentPrompt["delivery"],
): HTMLElement {
  const el = document.createElement("span");
  el.className = "prompt-delivery";
  switch (delivery.case) {
    case "queuedToLive":
    case "resumedRecipient":
      el.setAttribute("data-delivery", delivery.case);
      el.textContent = DELIVERY_WORDS[delivery.case];
      break;
    case "refused":
      // The SAME marker the two landings wear, carrying the refusal class —
      // a refused send is one of this row's outcomes, not a card of its own.
      el.setAttribute("data-delivery", delivery.case);
      el.classList.add("refused");
      el.textContent = DELIVERY_WORDS.refused;
      appendRefusalReason(el, delivery.value);
      break;
    default:
      return unreachableArm(
        `${PATH}.delivery`,
        armName(delivery as unknown as { case: string }),
      );
  }
  log.debug(`the agent prompt was delivered: ${delivery.case}`, {
    operation: "feed.agent-prompt-delivery",
    context: { delivery: delivery.case },
  });
  return el;
}

/**
 * The refusal's own words, beside the marker.
 *
 * ITS OWN ELEMENT rather than a sentence spliced onto the marker: the words
 * are the producer's and are drawn verbatim, while the marker is this client's
 * wording, and the two must stay tellable apart. UNSET DRAWS NOTHING — the
 * producer observed a refusal with no account, which is not an empty one.
 */
function appendRefusalReason(el: HTMLElement, refused: FeedAgentPromptRefused): void {
  if (refused.reason === undefined) return;
  const reason = document.createElement("span");
  reason.className = "prompt-refusal-reason";
  reason.textContent = refused.reason.text;
  el.append(reason);
}
