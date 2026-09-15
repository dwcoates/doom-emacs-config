/**
 * peer-message — A MESSAGE FROM ANOTHER CLAUDE: an inter-session peer message
 * or a subagent hand-back.
 *
 * IT IS NOT A PROMPT. It is drawn RIGHT-ALIGNED like a prompt (the same side of
 * the feed) but PURPLE, not the blue `.bubble.user` fill — a low-priority,
 * non-intrusive aside. It is ABBREVIATED: collapsed it shows only the sender
 * label and a chevron, NO body; clicking expands it to reveal the body via the
 * shared collapse/expand model (collapsed = no scroll; expand caps at 50vh and
 * only then reveals `overflow-y:auto` — bubble-scroll.ts + the `.expanded`
 * rules in styles.css).
 *
 * THE HEAD DRIVES THE TOGGLE. When collapsed the body's scroll box is hidden,
 * so there is nothing for the feed-wide click-to-expand (expand.ts) to land on;
 * the head's own click toggles the `.expanded` class on the scroll box, which
 * IS a capped section, so the feed's expandedKeys/applyExpanded reconcile keeps
 * the reader's open/closed choice across a re-push, and a click on the revealed
 * body collapses it again exactly as every other capped section does.
 */
import { log } from "../../log.js";
import { bubbleScroll, BUBBLE_SCROLL_CLASS } from "../bubble-scroll.js";
import { EXPANDED_CLASS } from "../../expand.js";
import { renderMarkdown } from "../../markdown.js";
import type { FeedPeerMessage } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/** The class the peer bubble wears — right-aligned like a prompt, but purple. */
export const PEER_BUBBLE_CLASS = "peer";

/** The always-visible head: the sender label plus the expand chevron. */
export const PEER_HEAD_CLASS = "peer-head";

/**
 * The abbreviated peer bubble.
 *
 * COLLAPSED shows the head only. The body lives in a `.bubble-scroll` box the
 * stylesheet hides while the bubble is collapsed and reveals (capped at 50vh,
 * scrollable) once `.expanded`. The body reuses the feed's markdown machinery,
 * so a peer message renders like any other prose.
 */
export function drawFeedPeerMessage(msg: FeedPeerMessage): HTMLElement {
  log.info("drawing a peer-message row", {
    operation: "feed.draw-peer-message",
    context: { sender: msg.sender },
  });
  const bubble = document.createElement("div");
  bubble.className = `bubble ${PEER_BUBBLE_CLASS} md`;

  const head = document.createElement("button");
  head.type = "button";
  head.className = PEER_HEAD_CLASS;

  const label = document.createElement("span");
  label.className = "peer-label";
  // The daemon composes the label ("agent <sender>"); drawn verbatim.
  label.textContent = msg.sender;
  head.append(label);

  const chevron = document.createElement("span");
  chevron.className = "peer-chevron";
  chevron.setAttribute("aria-hidden", "true");
  head.append(chevron);
  bubble.append(head);

  const body = document.createElement("div");
  body.className = "bubble-body";
  body.innerHTML = renderMarkdown(msg.body);
  const scroll = bubbleScroll(body);
  bubble.append(scroll);

  // The head toggles the body's scroll box, the tracked capped section. A click
  // on the revealed body collapses it through the feed-wide handler, so this
  // handler owns only the collapsed→expanded direction the hidden body cannot.
  head.addEventListener("click", () => {
    const expanded = scroll.classList.toggle(EXPANDED_CLASS);
    bubble.classList.toggle("peer-expanded", expanded);
    head.setAttribute("aria-expanded", String(expanded));
  });
  head.setAttribute("aria-expanded", "false");

  // A defensive assertion that the box the head toggles is the one the
  // stylesheet and the reconcile both key on.
  if (!scroll.classList.contains(BUBBLE_SCROLL_CLASS)) {
    throw new Error("peer-message: the body must ride the shared bubble-scroll box");
  }
  return bubble;
}
