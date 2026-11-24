/**
 * notices — THE SIDEBAR'S OWN WARNINGS AND ERRORS, behind its version (owner
 * request, 2026-10-31).
 *
 * The sidebar runs in a web view of its own beside the pages (host/
 * sidebar-page.ts), so the topbar's warning chip, the pages' one error
 * surface, cannot list what goes wrong in it. Instead the version in the
 * rail's top-right corner is tinted by what the sidebar has recorded since it
 * last started, in a subtle shade: red when there was an error, yellow when
 * there was a warning and no error, green when there was neither. The records
 * themselves are listed nowhere for now (owner request, 2026-11-24): the
 * version's click opens its memory popup (memory.ts) instead.
 *
 * WHAT IS COUNTED. Every record the sidebar's document writes at warn or error
 * (its logger tells `record` each one, host/document.ts). A failure standing
 * in the document is filed with a record of its own too, so the tint counts
 * it.
 */
import type { ClientLogLevel } from "../log.js";
import { createListeners } from "../listeners.js";

/** How many records the list keeps; the counts behind the tint keep counting past it. */
export const NOTICE_LIMIT = 100;

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

/**
 * Tint VERSION by NOTICES, again after every record taken; answers the
 * unsubscribe.
 */
export function installVersionTint(version: HTMLElement, notices: SidebarNotices): () => void {
  const tones: readonly NoticesTone[] = ["ok", "warn", "error"];
  const tint = (): void => {
    const tone = notices.tone();
    for (const each of tones) version.classList.toggle(noticesToneClass(each), each === tone);
  };
  tint();
  return notices.subscribe(tint);
}
