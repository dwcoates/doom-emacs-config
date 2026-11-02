/**
 * notices — THE SIDEBAR'S OWN WARNINGS AND ERRORS, behind its version (owner
 * request, 2026-10-31).
 *
 * The sidebar runs in a web view of its own beside the pages (host/
 * sidebar-page.ts), so the topbar's warning chip, the pages' one error
 * surface, cannot list what goes wrong in it. Instead the version in the
 * rail's top-right corner is tinted by what the sidebar has recorded since it
 * last started, in a subtle shade: red when there was an error, yellow when
 * there was a warning and no error, green when there was neither. A click on
 * the version lists them, the failures standing now first, then every warning
 * and error recorded, newest first.
 *
 * WHAT IS COUNTED. Every record the sidebar's document writes at warn or error
 * (its logger tells `record` each one, host/document.ts), and every failure
 * standing in it. A failure is filed with a record of its own too, so the
 * tint never needs the standing set; the list shows it so a failure still
 * standing reads apart from one that came and went.
 */
import type { LocalFailureView } from "../failure/local.js";
import type { ClientLogLevel } from "../log.js";
import { createListeners } from "../listeners.js";
import { clampReveal } from "../topbar/clamp.js";
import { applyPlacement, viewportOf } from "../placement.js";
import type { Dropdowns } from "./dropdowns.js";

/** How many records the list keeps; the counts behind the tint keep counting past it. */
export const NOTICE_LIMIT = 100;

/** The class of the version's notices panel. */
export const NOTICES_PANEL_CLASS = "sb-notices";

/** The version's tint: what the sidebar recorded since it started. */
export type NoticesTone = "ok" | "warn" | "error";

/** One warning or error the sidebar recorded. */
export interface Notice {
  readonly level: "warn" | "error";
  /** The record's own timestamp, as it was written. */
  readonly timestamp: string;
  readonly operation: string;
  readonly message: string;
}

/** The sidebar's notices since its document started. */
export interface SidebarNotices {
  /** Take one record the document wrote, LEVEL and its JSON LINE; below warn is ignored. */
  record(level: ClientLogLevel, line: string): void;
  /** The records kept, oldest first. */
  entries(): readonly Notice[];
  /** The version's tint now. */
  tone(): NoticesTone;
  /** Call LISTENER after every record taken; answers the unsubscribe. */
  subscribe(listener: () => void): () => void;
}

/** A field of a record's JSON line, as a string. */
function field(record: Record<string, unknown>, key: string): string {
  const value = record[key];
  return typeof value === "string" ? value : "";
}

/** An empty set of notices. */
export function createSidebarNotices(): SidebarNotices {
  let kept: Notice[] = [];
  let warnings = 0;
  let errors = 0;
  const listeners = createListeners();
  return {
    record(level, line) {
      if (level !== "warn" && level !== "error") return;
      // THE LINE IS THE RECORD'S OWN JSON (log.ts `emit`), so it always parses.
      const record = JSON.parse(line) as Record<string, unknown>;
      if (level === "error") errors += 1;
      else warnings += 1;
      kept.push({
        level,
        timestamp: field(record, "timestamp"),
        operation: field(record, "operation"),
        message: field(record, "message"),
      });
      if (kept.length > NOTICE_LIMIT) kept = kept.slice(kept.length - NOTICE_LIMIT);
      listeners.notify();
    },
    entries: () => kept,
    tone: () => (errors > 0 ? "error" : warnings > 0 ? "warn" : "ok"),
    subscribe(listener) {
      return listeners.add(listener);
    },
  };
}

/** The class the version wears for TONE. */
export function noticesToneClass(tone: NoticesTone): string {
  return `notices-${tone}`;
}

/** Fill PANEL with FAILURES standing and NOTICES, newest first. */
export function fillNoticesPanel(panel: HTMLElement, failures: LocalFailureView, notices: SidebarNotices): void {
  const sections: HTMLElement[] = [];
  const standing = failures.standing();
  if (standing.length > 0) {
    const list = document.createElement("ul");
    list.className = "sb-notices-standing";
    for (const failure of standing) {
      const item = document.createElement("li");
      item.className = "error";
      item.textContent = failure.headline;
      list.appendChild(item);
    }
    sections.push(heading("Standing"), list);
  }
  const entries = notices.entries();
  if (entries.length === 0) {
    const none = document.createElement("p");
    none.className = "sb-notices-none";
    none.textContent = "No warnings or errors since the sidebar started.";
    sections.push(none);
  } else {
    const list = document.createElement("ul");
    list.className = "sb-notices-list";
    for (const notice of [...entries].reverse()) {
      const item = document.createElement("li");
      item.className = notice.level;
      const when = document.createElement("time");
      when.textContent = notice.timestamp;
      const text = document.createElement("span");
      text.textContent = notice.message;
      item.append(when, text);
      item.title = notice.operation;
      list.appendChild(item);
    }
    sections.push(heading("Since the sidebar started"), list);
  }
  panel.replaceChildren(...sections);
}

function heading(text: string): HTMLElement {
  const el = document.createElement("h4");
  el.textContent = text;
  return el;
}

/** The version's notices, as the rail installs them. */
export interface VersionNotices {
  /** Whether the notices panel is open. */
  isOpen(): boolean;
  dispose(): void;
}

/**
 * Tint VERSION by NOTICES and open the list in a panel of RAIL's on a click.
 *
 * The panel is one of the rail's dropdowns (`dropdowns.ts`): a click outside
 * it, or the rail losing the user, closes it, and a second click on the
 * version does too. It is placed under the version by `clampReveal`, inside
 * the sidebar's window, and refilled while open as records arrive.
 */
export function installVersionNotices(
  version: HTMLElement,
  rail: HTMLElement,
  failures: LocalFailureView,
  notices: SidebarNotices,
  dropdowns: Dropdowns,
): VersionNotices {
  const panel = document.createElement("div");
  panel.className = `detail ${NOTICES_PANEL_CLASS}`;
  rail.appendChild(panel);
  const tones: readonly NoticesTone[] = ["ok", "warn", "error"];
  const tint = (): void => {
    const tone = notices.tone();
    for (const each of tones) version.classList.toggle(noticesToneClass(each), each === tone);
  };
  const place = (): void => {
    applyPlacement(panel, clampReveal(version.getBoundingClientRect(), panel.getBoundingClientRect(), viewportOf()));
  };
  const isOpen = (): boolean => panel.classList.contains("open");
  const close = (): void => {
    panel.classList.remove("open");
  };
  const refresh = (): void => {
    tint();
    if (!isOpen()) return;
    fillNoticesPanel(panel, failures, notices);
    place();
  };
  const onClick = (): void => {
    if (isOpen()) {
      close();
      dropdowns.released(panel);
      return;
    }
    fillNoticesPanel(panel, failures, notices);
    panel.classList.add("open");
    place();
    dropdowns.opened({ kind: "version-notices", element: panel, openers: [version], close });
  };
  version.addEventListener("click", onClick);
  const offNotices = notices.subscribe(refresh);
  const offFailures = failures.subscribe(refresh);
  tint();
  return {
    isOpen,
    dispose() {
      version.removeEventListener("click", onClick);
      offNotices();
      offFailures();
      dropdowns.released(panel);
      panel.remove();
    },
  };
}
