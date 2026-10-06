/**
 * view — the sidebar's VIEW STATE, which is the DAEMON'S (owner rulings,
 * 2026-10-06): "the sidebar must look exactly the same in every workspace's
 * page".
 *
 * Every workspace has its own page, and each page draws the view the roster
 * push carries — a section's fold (`RosterRepoSection.fold`,
 * `RosterTaskSection.fold`, `RosterMergedSection.fold`) and the grouping shown
 * (`WorkspaceRoster.shown`) — so a change asked for in one page reaches every
 * page on the same push.
 *
 * THE GESTURE IS INSTANT. A change the user asks for is painted on this page
 * at once and HELD until the wire agrees: a push drawn while the ask is in
 * flight still shows the asked value, so nothing flickers back. The hold ends
 * the moment a push carries the asked value, or when the ask fails — then
 * every copy is repainted from the wire, and the failure is drawn beside the
 * control by the verb runner.
 *
 * A held ask is identified by a TOKEN, so a second gesture on the same piece
 * while the first is in flight owns the hold: the first ask's failure cannot
 * release it.
 *
 * NOT VIEW STATE: a dropdown (a row's menu or detail popover) or a form open
 * mid-gesture is transient to the page that opened it, and never passes
 * through here.
 */
import { log } from "../log.js";

/** A piece of view state's value: a fold or a grouping. */
export type ViewValue = boolean | string;

/** Paint one copy of a piece of view state. */
export type ViewPaint<T extends ViewValue> = (value: T) => void;

/** The key a section's fold is held under. */
export function foldViewKey(sectionKey: string): string {
  return `fold:${sectionKey}`;
}

/** The key the grouping shown is held under. */
export const GROUPING_VIEW_KEY = "grouping";

/**
 * Say on a section, and on every fold triangle inside it, which way it stands.
 *
 * The triangle carries the state as `[data-section-fold][data-folded]`, so the
 * element that takes the gesture is also the one that reports it.
 */
export function paintSectionFold(section: HTMLElement, folded: boolean): void {
  section.classList.toggle("folded", folded);
  for (const triangle of section.querySelectorAll<HTMLElement>("[data-section-fold]")) {
    paintTriangle(triangle, folded);
  }
}

/** One triangle, told which way its section stands. */
export function paintTriangle(triangle: HTMLElement, folded: boolean): void {
  triangle.setAttribute("data-folded", folded ? "true" : "false");
  triangle.textContent = folded ? "▸" : "▾";
}

/** The page's view of the daemon-held view state, with its asks in flight. */
export interface SidebarView {
  /** A new roster is being drawn: the copies drawn from the last one are gone. */
  beginDraw(): void;
  /**
   * The value KEY draws, given what the wire says, and track PAINT as one
   * copy of it (the merged band is drawn once per grouping, and the grouping
   * picker is a copy of the grouping).
   *
   * A held ask the wire now agrees with is released here.
   */
  track<T extends ViewValue>(key: string, wire: T, paint: ViewPaint<T>): T;
  /** Paint an asked value on every copy at once and hold it. Answers its token. */
  ask<T extends ViewValue>(key: string, value: T): number;
  /** The ask under TOKEN failed: release it and repaint from the wire. */
  abandon(key: string, token: number): void;
}

/** A value asked for and not yet carried by a push. */
interface HeldAsk {
  token: number;
  value: ViewValue;
}

export function createSidebarView(): SidebarView {
  const wire = new Map<string, ViewValue>();
  const held = new Map<string, HeldAsk>();
  const copies = new Map<string, Array<ViewPaint<ViewValue>>>();
  let nextToken = 1;

  const paintCopies = (key: string, value: ViewValue): void => {
    for (const paint of copies.get(key) ?? []) paint(value);
  };

  return {
    beginDraw(): void {
      copies.clear();
    },
    track<T extends ViewValue>(key: string, value: T, paint: ViewPaint<T>): T {
      wire.set(key, value);
      copies.set(key, [...(copies.get(key) ?? []), paint as ViewPaint<ViewValue>]);
      const ask = held.get(key);
      if (ask === undefined) return value;
      if (ask.value === value) {
        held.delete(key);
        log.debug("a roster push carried the view this page asked for", {
          operation: "sidebar.view.reconciled",
          context: { key, value },
        });
        return value;
      }
      return ask.value as T;
    },
    ask<T extends ViewValue>(key: string, value: T): number {
      const token = nextToken++;
      held.set(key, { token, value });
      paintCopies(key, value);
      return token;
    },
    abandon(key: string, token: number): void {
      if (held.get(key)?.token !== token) return;
      held.delete(key);
      const value = wire.get(key);
      if (value === undefined) {
        // A piece of the view is only reachable by a gesture once a push has
        // drawn it, and drawing it is what records its wire value: an ask on a
        // key never drawn is a caller defect, not a state to paint around.
        log.error("a failed view change names a piece no push ever drew", {
          operation: "sidebar.view.unknown-key",
          context: { key },
        });
        throw new Error(`sidebar view: no wire value is recorded for ${key}`);
      }
      paintCopies(key, value);
    },
  };
}
