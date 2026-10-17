/**
 * peer-arrow — THE ARROW of a cross-session message's header.
 *
 * Its SIDE and DIRECTION are the whole statement of which way the message went
 * (owner spec, 2026-10-17): a message that came IN is headed by a left arrow
 * before the sending workspace's name, one that went OUT by the receiving
 * workspace's name before a right arrow. Nothing else in either header says
 * so. One glyph for both headers, so the two can never draw it differently: it
 * strokes in `currentColor` and is sized in `em`, so it takes the header's own
 * color and font size, whatever they are.
 */
import { SVG_NS } from "../../dom.js";

/** The arrow glyph's class, which the stylesheet sizes. */
export const PEER_ARROW_CLASS = "peer-arrow";

/** Which way the message went: in (pointing left) or out (pointing right). */
export type PeerArrowDirection = "in" | "out";

/** The arrow glyph, pointing the way DIRECTION went. */
export function drawPeerArrow(direction: PeerArrowDirection): SVGSVGElement {
  const svg = document.createElementNS(SVG_NS, "svg");
  svg.setAttribute("viewBox", "0 0 16 16");
  svg.setAttribute("aria-hidden", "true");
  svg.setAttribute("class", PEER_ARROW_CLASS);
  svg.setAttribute("data-direction", direction);
  const shaft = document.createElementNS(SVG_NS, "path");
  const head = document.createElementNS(SVG_NS, "path");
  if (direction === "in") {
    shaft.setAttribute("d", "M14 8 H3");
    head.setAttribute("d", "M7 4 L3 8 L7 12");
  } else {
    shaft.setAttribute("d", "M2 8 H13");
    head.setAttribute("d", "M9 4 L13 8 L9 12");
  }
  svg.append(shaft, head);
  return svg;
}
