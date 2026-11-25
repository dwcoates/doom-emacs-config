/**
 * THE GATE DOCK'S FIXTURES: the real page shell, a gate's row, the feed's body
 * and the observer's settle. Shared by the dock's behavior tests
 * (test/feed/gate-dock.test.ts) and its rules over the stylesheet
 * (rules/feed/gate-dock.rules.test.ts).
 */
import { shellElements } from "../../src/shell.js";
import { shellHTML } from "../shell-html.js";

/** MutationObserver delivers on a microtask; this lets it run. */
export async function settle(): Promise<void> {
  await Promise.resolve();
  await Promise.resolve();
}

/** The real page shell, from index.html. */
export function page(): { feed: HTMLElement; dock: HTMLElement; footer: HTMLElement } {
  document.body.innerHTML = shellHTML();
  const shell = shellElements(document);
  return { feed: shell.feed, dock: shell.gateDock, footer: shell.footer };
}

/** A root row holding a cold-gate card in STATE. */
export function gateRow(state: string): { row: HTMLElement; card: HTMLElement } {
  const row = document.createElement("div");
  row.className = "feed-item";
  const card = document.createElement("div");
  card.className = "cold-gate";
  card.setAttribute("data-state", state);
  row.append(card);
  return { row, card };
}

/** The feed as feed-view.ts builds it: every row inside one body. */
export function bodiedFeed(feed: HTMLElement, ...rows: HTMLElement[]): HTMLElement {
  const body = document.createElement("div");
  body.className = "feed-body";
  body.append(...rows);
  feed.append(body);
  return body;
}
