/**
 * hover-stamp — A BUBBLE'S FOOTER ACTIVITY IS READ IN THE FOOTER WHILE THE
 * POINTER IS ON IT (owner requests, 2026-10-25 for a response, 2026-11-19 for
 * every bubble). Hovering a prompt, a response, a tool call, a merge, a fix
 * gate or any other card the daemon stamps shows its hover stamp
 * (`frontend.v1.FeedHoverStamp`) in the footer's activity cell: what it is,
 * how long it took, and how long ago it was sent or settled. The pointer
 * leaving it, or the focus leaving the page, gives the cell back to the
 * daemon's activity line (src/footer/hover-stamp.ts draws it).
 *
 * ONE RECORDER FOR EVERY KIND. The stamp is the ROW's (`FeedRow.hover`), so
 * the feed records it on the row's element on every push of the row
 * (`recordRowHover`, called by the feed view's row chrome), whatever the row
 * draws: no card records a stamp of its own, and a stamp restated while its
 * row is hovered (an arriving response's figure, a tool call settling)
 * reaches the footer at once, without the pointer moving.
 */
import type { FeedHoverStamp, FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import type { FocusLeft } from "../focus-left.js";
import { heatColor } from "../heat.js";
import { log } from "../log.js";
import { createPointerRest } from "../pointer-rest.js";
import { MalformedView } from "../rpc/malformed.js";
import { msOf, requireMessage } from "../rpc/strict.js";
import { feedRowId, feedRowOf } from "./row-id.js";
import { createListeners } from "../listeners.js";

/** A hover stamp's token figure, read. */
export interface HoverTokens {
  /** The formatted figure, drawn verbatim. */
  readonly text: string;
  /** The figure's color, by its heat (the footer's tokens cell's one rule). */
  readonly color: string;
}

/** A row's hover stamp, read: what the footer draws while it is hovered. */
export interface HoverStamp {
  /** What the bubble is, drawn verbatim first. */
  readonly label: string;
  /** The bubble's token figure, or null when it has none. */
  readonly tokens: HoverTokens | null;
  /** When the bubble was sent or settled, epoch ms, or null while under way. */
  readonly atMs: number | null;
  /** When the bubble's span began, epoch ms, or null when it has none. */
  readonly startedAtMs: number | null;
}

/**
 * STAMP, read, so a stamp the wire got wrong (an instant past a safe integer,
 * a figure with no heat or one outside [0, 1], a span that settled before it
 * began) is refused as a malformed view before anything of it is drawn. PATH
 * names its field.
 */
export function readHoverStamp(stamp: FeedHoverStamp, path: string): HoverStamp {
  const atMs = stamp.atMs === 0n ? null : msOf(stamp.atMs, `${path}.at_ms`);
  const startedAtMs = stamp.startedAtMs === 0n ? null : msOf(stamp.startedAtMs, `${path}.started_at_ms`);
  if (atMs !== null && startedAtMs !== null && atMs < startedAtMs) {
    throw new MalformedView(
      `${path}.started_at_ms`,
      `the span began at ${String(startedAtMs)}, after it settled at ${String(atMs)}`,
    );
  }
  const tokens =
    stamp.tokens === undefined
      ? null
      : {
          text: stamp.tokens.text,
          color: heatColor(requireMessage(stamp.tokens.heat, `${path}.tokens.heat`).position, `${path}.tokens.heat.position`),
        };
  return { label: requireMessage(stamp.label, `${path}.label`).text, tokens, atMs, startedAtMs };
}

/** What the footer is told: the hovered bubble's stamp, or null for none. */
export type ShowHoverStamp = (stamp: HoverStamp | null) => void;

/** Each drawn row's stamp; a row with none has no entry. */
const stamps = new WeakMap<Element, HoverStamp>();

/** Every installed hover's answer to a row's stamp being restated. */
const restaters = createListeners<[Element]>();

/**
 * Record ROW's hover stamp on its element EL, or that it has none: THE ONE
 * RECORDER every row kind goes through, called by the feed view on every push
 * of the row. A malformed stamp is refused (MalformedView) and EL keeps no
 * stamp, so the footer never shows one the wire got wrong.
 */
export function recordRowHover(el: Element, row: FeedRow): void {
  let stamp: HoverStamp | undefined;
  try {
    stamp = row.hover === undefined ? undefined : readHoverStamp(row.hover, "FeedRow.hover");
  } catch (err) {
    setRowHover(el, undefined);
    throw err;
  }
  setRowHover(el, stamp);
}

/** Record EL's stamp, STAMP, or that it has none (undefined). */
export function setRowHover(el: Element, stamp: HoverStamp | undefined): void {
  if (stamp === undefined) stamps.delete(el);
  else stamps.set(el, stamp);
  restaters.notify(el);
}

/** Row element EL's recorded stamp, or null when it has none. */
export function hoverStampOf(el: Element): HoverStamp | null {
  return stamps.get(el) ?? null;
}

/**
 * The row whose card TARGET is on, inside HOST, or null: the innermost feed
 * row TARGET is drawn in. A pointer on a row's own element, outside any of its
 * card (a nested row's margin), is on the card of the row around it.
 */
export function hoveredRowAt(host: HTMLElement, target: EventTarget | null): Element | null {
  if (!(target instanceof Element)) return null;
  let row = feedRowOf(target);
  if (row === target) row = row.parentElement === null ? null : feedRowOf(row.parentElement);
  return row !== null && host.contains(row) ? row : null;
}

/**
 * Show the stamp of the row whose card is under the pointer in HOST through
 * SHOW, and null once the pointer is on none, or on a row with no stamp. SHOW
 * is called only when what it shows changes. Answers the uninstall, which
 * shows null if a stamp is shown.
 *
 * THE FOCUS LEAVING THE PAGE CLEARS IT, ALWAYS (owner request, 2026-10-26):
 * FOCUS tells the hover the moment it does (focus-left.ts), and the hover
 * shows null and stays SUSPENDED until the user is back: the pointer MOVES
 * from where it rested. Until then a pointer WebKit still believes is over a
 * bubble shows nothing, even when a redraw under it reports it there again.
 */
export function installHoverStamp(host: HTMLElement, show: ShowHoverStamp, focus: FocusLeft): () => void {
  let hovered: Element | null = null;
  let shown: HoverStamp | null = null;
  /** Whether the focus left and the pointer has not moved since (pointer-rest.ts). */
  const rest = createPointerRest();
  const showFor = (row: Element | null): void => {
    hovered = row;
    const stamp = row === null ? null : hoverStampOf(row);
    if (stamp === shown) return;
    shown = stamp;
    log.debug(stamp === null ? "the footer no longer shows a hovered bubble's stamp" : "the footer shows the hovered bubble's stamp", {
      operation: "feed.hover-stamp",
      context: { row: rowOf(row), label: stamp?.label ?? null, tokens: stamp?.tokens?.text ?? null },
    });
    show(stamp);
  };
  const onOver = (event: MouseEvent): void => {
    if (rest.suspended()) return;
    showFor(hoveredRowAt(host, event.target));
  };
  const onMove = (event: MouseEvent): void => {
    if (!rest.moved(event.clientX, event.clientY)) return;
    log.debug("the pointer moved after the focus left; the hover answers again", {
      operation: "feed.hover-stamp.resume",
      context: {},
    });
    showFor(hoveredRowAt(host, event.target));
  };
  const onLeave = (): void => showFor(null);
  const onFocusLeft = (why: string): void => {
    rest.suspend();
    log.debug("the focus left the page; the footer lets go of the hovered bubble", {
      operation: "feed.hover-stamp.focus-left",
      context: { why, shown: shown !== null },
    });
    showFor(null);
  };
  const restate = (row: Element): void => {
    if (row === hovered) showFor(row);
  };
  host.addEventListener("mouseover", onOver);
  host.addEventListener("mousemove", onMove);
  host.addEventListener("mouseleave", onLeave);
  const stopFocus = focus.onLeft(onFocusLeft);
  const unrestate = restaters.add(restate);
  return () => {
    host.removeEventListener("mouseover", onOver);
    host.removeEventListener("mousemove", onMove);
    host.removeEventListener("mouseleave", onLeave);
    stopFocus();
    unrestate();
    showFor(null);
  };
}

/** The FeedId of row element ROW, for the hover's record. */
function rowOf(row: Element | null): string {
  if (row === null) return "none";
  return feedRowId(row) ?? "unset";
}
