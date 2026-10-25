/**
 * response-usage — A RESPONSE'S COST IS READ IN THE FOOTER WHILE THE POINTER IS
 * ON IT (owner request, 2026-10-25). A response bubble draws no token figure or
 * duration of its own: hovering it shows its usage stamp (its figure and, once
 * it settled, how long ago) in the footer's activity cell, and the pointer
 * leaving it gives the cell back to the daemon's activity line
 * (src/footer/response-usage.ts draws it).
 *
 * The response draw records each bubble's stamp here on every draw
 * (`setResponseUsage`), so a figure restated while the bubble is hovered (an
 * arriving response's, or the final answer's turn total) reaches the footer
 * at once, without the pointer moving.
 */
import type { FeedResponseUsageStamp } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { BUBBLE_CLASS, BUBBLE_ROLE_ATTRIBUTE } from "../bubble/draw.js";
import { heatColor } from "../heat.js";
import { log } from "../log.js";
import { msOf, requireMessage } from "../rpc/strict.js";
import { feedRowId, feedRowOf } from "./row-id.js";

/** A response's usage stamp, read: what the footer draws while it is hovered. */
export interface ResponseUsage {
  /** The formatted figure, drawn verbatim. */
  readonly text: string;
  /** The figure's color, by its heat (the footer's tokens cell's one rule). */
  readonly color: string;
  /** When the response settled, epoch ms, or null while it is arriving. */
  readonly settledAtMs: number | null;
}

/**
 * STAMP, read at the response's draw, so a stamp the wire got wrong (a settle
 * instant past a safe integer, a heat outside [0, 1]) refuses the row as a
 * malformed view there, before anything of it is drawn. PATH names its field.
 */
export function readResponseUsage(stamp: FeedResponseUsageStamp, path: string): ResponseUsage {
  return {
    text: stamp.text,
    color: heatColor(requireMessage(stamp.heat, `${path}.heat`).position, `${path}.heat.position`),
    settledAtMs: stamp.atMs === 0n ? null : msOf(stamp.atMs, `${path}.at_ms`),
  };
}

/** What the footer is told: the hovered response's usage, or null for none. */
export type ShowResponseUsage = (usage: ResponseUsage | null) => void;

/** Each drawn response bubble's usage; a bubble with none has no entry. */
const usages = new WeakMap<Element, ResponseUsage>();

/** Every installed hover's answer to a bubble's stamp being restated. */
const restaters = new Set<(bubble: Element) => void>();

/** The selector of a response bubble. */
const RESPONSE_BUBBLE = `.${BUBBLE_CLASS}[${BUBBLE_ROLE_ATTRIBUTE}="response"]`;

/** Record BUBBLE's usage, USAGE, or that it has none (undefined). */
export function setResponseUsage(bubble: HTMLElement, usage: ResponseUsage | undefined): void {
  if (usage === undefined) usages.delete(bubble);
  else usages.set(bubble, usage);
  for (const restate of restaters) restate(bubble);
}

/** BUBBLE's recorded usage, or null when it has none. */
export function responseUsageOf(bubble: Element): ResponseUsage | null {
  return usages.get(bubble) ?? null;
}

/**
 * Show the usage of the response bubble under the pointer in HOST through
 * SHOW, and null once the pointer is on none. SHOW is called only when what it
 * shows changes. Answers the uninstall, which shows null if a stamp is shown.
 */
export function installResponseUsageHover(host: HTMLElement, show: ShowResponseUsage): () => void {
  let hovered: Element | null = null;
  let shown: ResponseUsage | null = null;
  const showFor = (bubble: Element | null): void => {
    hovered = bubble;
    const usage = bubble === null ? null : responseUsageOf(bubble);
    if (usage === shown) return;
    shown = usage;
    log.debug(usage === null ? "the footer no longer shows a response's usage" : "the footer shows the hovered response's usage", {
      operation: "feed.response-usage-hover",
      context: { row: rowOf(bubble), tokens: usage?.text ?? null },
    });
    show(usage);
  };
  const onOver = (event: MouseEvent): void => {
    const bubble = event.target instanceof Element ? event.target.closest(RESPONSE_BUBBLE) : null;
    showFor(bubble !== null && host.contains(bubble) ? bubble : null);
  };
  const onLeave = (): void => showFor(null);
  const restate = (bubble: Element): void => {
    if (bubble === hovered) showFor(bubble);
  };
  host.addEventListener("mouseover", onOver);
  host.addEventListener("mouseleave", onLeave);
  restaters.add(restate);
  return () => {
    host.removeEventListener("mouseover", onOver);
    host.removeEventListener("mouseleave", onLeave);
    restaters.delete(restate);
    showFor(null);
  };
}

/** The FeedId of the row BUBBLE is drawn in, for the hover's record. */
function rowOf(bubble: Element | null): string {
  if (bubble === null) return "none";
  const row = feedRowOf(bubble);
  return (row === null ? null : feedRowId(row)) ?? "unset";
}
