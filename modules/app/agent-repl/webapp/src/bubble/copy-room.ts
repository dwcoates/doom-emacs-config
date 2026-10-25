/**
 * copy-room — A BUBBLE'S COPY CONTROL, AND THE ROOM ITS FIRST LINE OF TEXT
 * LEAVES FOR IT (owner request, 2026-10-25): every bubble that copies its whole
 * text (a prompt of any kind, the turn's final response) places its copy
 * control (copy-control.ts) the same way, from here.
 *
 * THE CONTROL SITS ON THE FIRST LINE OF TEXT, NEVER ON A LINE OF ITS OWN. It
 * is drawn at the bubble's top right (styles.css `.bubble-copy`), and the
 * scroll box floats a reservation of its width there, so the first line wraps
 * short of it and every later line runs the bubble's full width. A float
 * before a BLOCK adds nothing to the bubble's fit-content width, though (the
 * engine takes the wider of the two, never their sum), so a short prompt sized
 * itself to its text alone and the reservation then pushed its one line under
 * the control: the control seemed to sit on a fresh line at the top.
 *
 * So the first line of text also opens with a GHOST of the reservation
 * (`COPY_ROOM_CLASS`): an invisible, zero-height float exactly as wide. A float
 * shares a line's width only inside that line's own inline formatting context,
 * so the ghost widens the bubble by the control's room before its first line
 * wraps, and being zero-height it shortens no line. A first line that already
 * reaches the bubble's cap still wraps short of the control, as the
 * reservation makes it. Content whose first thing is not text (an image) has
 * no ghost, and stands under the control's row.
 */
import { boundCopyControl, type CopyKind } from "../copy-control.js";
import { addSlotMark, isMarkdownSlot } from "./body.js";

/** The class every bubble's copy control wears, which places it at the bubble's top right. */
export const BUBBLE_COPY_CLASS = "bubble-copy";

/** The class of the ghost that opens a bubble's first line of text. */
export const COPY_ROOM_CLASS = "copy-room";

/**
 * The elements a line of text is laid out in: the one the ghost opens is the
 * nearest of these around the first text.
 */
const LINE_HOSTS = "p, li, dt, dd, h1, h2, h3, h4, h5, h6, pre, td, th, div";

/**
 * A bubble's copy control of KIND, copying TEXT, with LABEL on hover: the one
 * copy control, placed at the bubble's top right.
 */
export function bubbleCopyControl(kind: CopyKind, label: string, text: string): HTMLElement {
  return boundCopyControl(kind, BUBBLE_COPY_CLASS, label, text);
}

/**
 * Open the first line of text under ROOT with the ghost: the first character
 * that is not white space, in the nearest line host around it inside ROOT (ROOT
 * itself when there is none). Answers whether ROOT holds any such text, and so
 * a ghost.
 */
export function markCopyRoom(root: HTMLElement): boolean {
  const walker = root.ownerDocument.createTreeWalker(root, NodeFilter.SHOW_TEXT);
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    if (!/\S/.test(node.textContent ?? "")) continue;
    const host = node.parentElement?.closest(LINE_HOSTS);
    const line = host !== null && host !== undefined && root.contains(host) ? host : root;
    const ghost = root.ownerDocument.createElement("span");
    ghost.className = COPY_ROOM_CLASS;
    ghost.setAttribute("aria-hidden", "true");
    line.prepend(ghost);
    return true;
  }
  return false;
}

/**
 * Leave the copy control its room in a bubble's CONTENT, before it is painted:
 * the ghost opens the first content node's first line of text. A markdown slot
 * is rendered on every paint, so its ghost is a mark of every paint
 * (`addSlotMark`); any other node is drawn once, so it is marked now.
 */
export function reserveCopyRoom(content: readonly ChildNode[]): void {
  const first = content[0];
  if (!(first instanceof HTMLElement)) return;
  if (isMarkdownSlot(first)) {
    addSlotMark(first, (rendered) => {
      markCopyRoom(rendered);
    });
    return;
  }
  markCopyRoom(first);
}
