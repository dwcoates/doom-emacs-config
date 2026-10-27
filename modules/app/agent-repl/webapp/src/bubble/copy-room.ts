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
 *
 * A TREE IS FITTED SHORT OF THE CONTROL TOO (owner request, 2026-10-27). A
 * metaprompt tree's lines are fitted to the bubble's cap before they are drawn
 * (body.ts), so a tree that opens the bubble would be broken again by the
 * reservation. Its first line is fitted the room shorter instead
 * (`reserveSlotFirstLine`), the room measured off a probe of the ghost, so the
 * tree's first line sits beside the control like any other text.
 */
import { boundCopyControl, type CopyKind } from "../copy-control.js";
import { log } from "../log.js";
import { addSlotMark, isMarkdownSlot, reserveSlotFirstLine, type FirstLineRoom } from "./body.js";

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
    reserveSlotFirstLine(first, COPY_ROOM);
    return;
  }
  markCopyRoom(first);
}

/**
 * The copy control's room on a first line, in px: the drawn width of a ghost
 * probed inside BODY, which the stylesheet sizes from the same tokens as the
 * reservation. The body judges a width that is not positive unmeasurable.
 */
export function measureCopyRoomPx(body: HTMLElement): number {
  const probe = body.ownerDocument.createElement("span");
  probe.className = COPY_ROOM_CLASS;
  probe.setAttribute("aria-hidden", "true");
  body.append(probe);
  const px = probe.getBoundingClientRect().width;
  probe.remove();
  log.debug("measured a bubble's copy room", { operation: "bubble.copy-room.measure", context: { room_px: px } });
  return px;
}

/** The copy control's first-line room, as the body pipeline keeps it. */
const COPY_ROOM: FirstLineRoom = { px: measureCopyRoomPx };
