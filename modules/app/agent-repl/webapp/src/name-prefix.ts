/**
 * name-prefix — the marks the daemon puts before a workspace's name
 * (`frontend.v1.WorkspaceNamePrefix`, carried by `frontend.v1.RosterRowName.prefix`
 * and `frontend.v1.TopbarWorkspaceName.prefix`). THE ONE DRAWING every
 * surface that draws a workspace's name uses, so the sidebar's rows, their
 * stand-ins and the topbar draw the same marks in the same order.
 *
 * The prefix is its own element BEFORE the name's, never part of the name's
 * text: the name stays the slug the rename, the search and the name's ink
 * read, and the name's tint and breath never touch a mark.
 *
 * THE DRAWN ORDER IS THE MESSAGE'S FIELD ORDER, left to right:
 *
 *   1. the priority icon (`priority`), the picture `images/<arm>.png` that
 *      the Emacs tab bar and switcher draw too;
 *   2. the emoji-ack glyph (`emoji_ack`), the daemon's emoji drawn verbatim,
 *      keeping its own colours whatever ink the name is drawn in.
 *
 * Each mark is sized by the stylesheet's `.name-prefix` rules at the name's
 * own line height, so no mark is drawn taller than the letters beside it.
 */
import type {
  WorkspaceNameEmojiAck,
  WorkspaceNamePrefix,
  WorkspaceNamePriority,
} from "../../proto/gen/ts/frontend/v1/workspace_name_prefix_pb";
import { MalformedView } from "./rpc/malformed.js";
import p05Picture from "../../images/p05.png?inline";
import p1Picture from "../../images/p1.png?inline";
import p2Picture from "../../images/p2.png?inline";
import p3Picture from "../../images/p3.png?inline";
import priorityLabels from "../../proto/vocab/priority-labels.json";

/** The class of the element holding every mark drawn before a name. */
export const NAME_PREFIX_CLASS = "name-prefix";

/** The class every drawn priority icon carries. */
export const NAME_PRIORITY_CLASS = "name-priority";

/** The class every drawn emoji-ack glyph carries. */
export const NAME_EMOJI_ACK_CLASS = "name-emoji-ack";

/** One priority level, as the generated code spells its arm. */
export type PriorityArm = NonNullable<WorkspaceNamePriority["level"]["case"]>;

/**
 * Each level's picture and its spoken label. The picture is bundled from
 * `images/`, never data off the wire; the label, read from the shared
 * vocabulary (`proto/vocab/priority-labels.json`, the table Emacs reads too),
 * is the image's alt text and tooltip, so a reader and a search see the level
 * the picture shows.
 */
const PRIORITY_ICONS: Readonly<Record<PriorityArm, { src: string; label: string }>> = {
  p05: { src: p05Picture, label: priorityLabels.labels.p05 },
  p1: { src: p1Picture, label: priorityLabels.labels.p1 },
  p2: { src: p2Picture, label: priorityLabels.labels.p2 },
  p3: { src: p3Picture, label: priorityLabels.labels.p3 },
};

/**
 * Draw PREFIX, the marks before the name at PATH, or undefined when the name
 * wears none: an unset prefix is the fact. A set prefix carrying no mark is a
 * malformed view (the daemon never sends one), and so is a priority with no
 * level.
 */
export function drawNamePrefix(prefix: WorkspaceNamePrefix | undefined, path: string): HTMLElement | undefined {
  if (prefix === undefined) return undefined;
  const element = document.createElement("span");
  element.className = NAME_PREFIX_CLASS;
  if (prefix.priority !== undefined) {
    element.appendChild(drawNamePriority(prefix.priority, `${path}.priority`));
  }
  if (prefix.emojiAck !== undefined) {
    element.appendChild(drawNameEmojiAck(prefix.emojiAck, `${path}.emoji_ack`));
  }
  if (element.childElementCount === 0) {
    throw new MalformedView(path, "a name prefix carries at least one mark");
  }
  return element;
}

/** Draw the priority icon of PRIORITY, the level at PATH. */
export function drawNamePriority(priority: WorkspaceNamePriority, path: string): HTMLElement {
  const arm = priority.level.case;
  if (arm === undefined) throw new MalformedView(`${path}.level`, "a priority icon names its level");
  const icon = PRIORITY_ICONS[arm];
  const image = document.createElement("img");
  image.className = NAME_PRIORITY_CLASS;
  image.src = icon.src;
  image.alt = icon.label;
  image.title = icon.label;
  image.setAttribute("data-priority", arm);
  return image;
}

/** Draw GLYPH, the emoji-ack glyph at PATH, verbatim in its own element. */
export function drawNameEmojiAck(glyph: WorkspaceNameEmojiAck, path: string): HTMLElement {
  if (glyph.text === "") throw new MalformedView(`${path}.text`, "an emoji-ack glyph carries its glyph");
  const element = document.createElement("span");
  element.className = NAME_EMOJI_ACK_CLASS;
  element.textContent = glyph.text;
  return element;
}
