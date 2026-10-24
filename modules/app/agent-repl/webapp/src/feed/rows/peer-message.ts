/**
 * peer-message — A MESSAGE FROM ANOTHER CLAUDE SESSION: an inter-session peer
 * message.
 *
 * ITS SPEC, AND NOTHING ELSE (owner rulings, 2026-09-23 and 2026-10-17). It is
 * a prompt-role bubble — the right rail and the prompt fill, like every prompt
 * kind — with NO border: its header strip alone marks it as a message from
 * another agent. The header is a left arrow, then the sending session's name,
 * as the daemon resolved it (`FeedPeerMessageFrom`). Its collapsed line limit
 * is ZERO: collapsed, it shows its header and nothing of its body, which is the
 * abbreviated aside it has always been. It opens and closes through the one
 * toggle every bubble has (expand.ts: a click on the strip or the body toggles
 * the scroll box), wears the one has-more fade, and its body is a markdown
 * slot the one body pipeline paints, so a tree in it wraps at the bubble's cap.
 */
import { log } from "../../log.js";
import { markdownSlot } from "../../bubble/body.js";
import { drawBubble } from "../../bubble/draw.js";
import { requireMessage } from "../../rpc/strict.js";
import type { FeedPeerMessage, FeedPeerMessageFrom } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { drawPeerArrow } from "./peer-arrow.js";

/** The class the peer bubble's hooks know it by. */
export const PEER_BUBBLE_CLASS = "peer";

/** The header strip's one element: the arrow and the sender's name. */
export const PEER_LABEL_CLASS = "peer-label";

/** The sender's name inside the header. */
export const PEER_NAME_CLASS = "peer-name";

/** The class of the markdown slot the peer's body is painted into. */
export const PEER_BODY_CLASS = "peer-body";

/** The peer bubble: its spec, drawn (in place over PREVIOUS) by the one bubble. */
export function drawFeedPeerMessage(msg: FeedPeerMessage, previous?: HTMLElement): HTMLElement {
  const from = requireMessage(msg.from, "FeedPeerMessage.from");
  log.info("drawing a peer-message row", {
    operation: "feed.draw-peer-message",
    context: { sender: from.name?.text ?? "", sender_kind: from.sender.case ?? "unset" },
  });
  return drawBubble(
    {
      role: "prompt",
      variant: "peer",
      hooks: [PEER_BUBBLE_CLASS],
      waving: false,
      strip: [drawFeedPeerMessageFrom(from)],
      content: [markdownSlot(PEER_BODY_CLASS, requireMessage(msg.prose, "FeedPeerMessage.prose"))],
      capLines: 0,
    },
    previous,
  ).bubble;
}

/**
 * The header: a LEFT arrow, then the sending session's name, drawn verbatim.
 * Every sender arm draws the same way: the arm states what the name is, and
 * the name is the daemon's to resolve.
 */
export function drawFeedPeerMessageFrom(from: FeedPeerMessageFrom): HTMLElement {
  const label = document.createElement("div");
  label.className = PEER_LABEL_CLASS;
  const name = document.createElement("span");
  name.className = PEER_NAME_CLASS;
  name.textContent = requireMessage(from.name, "FeedPeerMessage.from.name").text;
  label.append(drawPeerArrow("in"), name);
  return label;
}
