/**
 * row-hover — the ONE owner of a row detail panel's hover and focus gesture,
 * for the whole page.
 *
 * WHY THE GESTURE IS NOT THE ROW'S. A roster push throws every drawn row away
 * and draws new ones, and a push lands whenever a workspace's state moves — a
 * prompt sent from Emacs is one. When each row node owned its own timers, a
 * push arriving while the pointer rested on a trigger left the DETACHED row's
 * timer armed: it fired seconds later, wrote the workspace into
 * `SidebarContext.openDetails` and registered an invisible panel, and the
 * NEXT push drew that row open, long after the pointer had gone (the "old
 * hovers come back" defect, 2026-10-14). Here the timers and the pointer's
 * whereabouts are the page's, keyed by `WorkspaceRef.id`, and
 * every open or close acts only on the copies still drawn (`drawn`, each
 * dropped by `forgetWithin` when its section's drawing is thrown away), so a
 * thrown-away row can never be acted on.
 *
 * WHERE THE POINTER IS, ACROSS A REDRAW. A row's line and panel report their
 * own `mouseenter`/`mouseleave`. A row drawn under a resting pointer never
 * saw the pointer arrive, so it will not see it leave either; the rail's host
 * reports every pointer move and its own `mouseleave` through `pointerAt`,
 * which forgets any workspace the pointer is no longer over. Focus is read
 * live from `document.activeElement` when a close comes due, so a focused
 * line a redraw removed cannot hold a panel open.
 *
 * THE RAIL LOSING THE USER CLOSES EVERY PANEL (`transients.ts`): the page's
 * focus gone, the page hidden or the selection moved `dismiss`es them, and no
 * panel opens again until the user is back in the rail (`resume`). Until then
 * neither a pointer the rail still finds on a line (a redraw under it, a
 * row WebKit re-hovers) nor the focus WebKit hands back to the line it left
 * reopens one.
 */
import { log } from "../log.js";
import { createKeyedTimers } from "../timers.js";
import type { Dropdowns } from "./dropdowns.js";
import { paintRowDetail } from "./row.js";
import type { PopupMirror } from "../host/sidebar-popups.js";

/**
 * The detail panel opens on HOVER. There is no chevron (owner ruling,
 * 2026-09-14): "in the sidebar, when you hover a workspace, the chevron
 * appears. There should be no chevron. Hovering the workspace should have the
 * same effect as opening the chevron currently does."
 *
 * THE TRIGGER IS THE ROW'S WHOLE LINE, and the panel opens once the pointer
 * has rested on it for `HOVER_OPEN_DELAY_MS` (owner requests, 2026-10-31,
 * which retired the duration field as the only trigger of 2026-10-14: a
 * trigger a few characters wide made the panel "finnicky", opening for some
 * hovers and not others). The delay keeps a pointer crossing the rail on its
 * way elsewhere from opening a panel for every row it passes.
 *
 * THE DURATION FIELD STILL OPENS IT AT ONCE (owner request, 2026-10-31): the
 * pointer reaching the duration, which lies inside the line, opens the panel
 * with no delay.
 *
 * SO DOES THE "⋯" CONTROL at the line's far right (owner request,
 * 2026-11-02): the pointer reaching it opens the panel at once.
 *
 * THREE QUARTERS OF A SECOND (owner request, 2026-11-01), down from two.
 */
export const HOVER_OPEN_DELAY_MS = 750;

/**
 * The other delay is travel: `HOVER_CLOSE_GRACE_MS`. The panel stands beside
 * the row's line rather than touching it, so the pointer is briefly over
 * neither on its way in, and a close on the bare `mouseleave` would snatch the
 * panel away as it is being reached for.
 */
export const HOVER_CLOSE_GRACE_MS = 120;

/** One drawn copy of a row, as the current draw registered it. */
export interface DrawnRow {
  /** The row's box, which carries the `.open` class. */
  ws: HTMLElement;
  /** The row's line: the pointer's trigger, and keyboard focus inside it holds the panel open. */
  line: HTMLElement;
  /** The detail panel. */
  detail: HTMLElement;
}

/** Which part of a row the pointer is over. */
type HoverPart = "line" | "duration" | "more" | "panel";

/** The parts inside the line that open the panel at once: the duration field and the "⋯" control. */
type InstantPart = "duration" | "more";

function isInstant(part: HoverPart): part is InstantPart {
  return part === "duration" || part === "more";
}

export interface RowHover {
  /** The drawing under ROOT was thrown away: its row copies are gone, every other stays. */
  forgetWithin(root: Node): void;
  /** The current draw drew a copy of workspace ID's row. */
  drawn(id: string, row: DrawnRow): void;
  /** The pointer arrived on PART of workspace ID's row. */
  enter(id: string, part: HoverPart): void;
  /** The pointer left PART of workspace ID's row. */
  leave(id: string, part: HoverPart): void;
  /** The keyboard focus arrived in workspace ID's row line. */
  focusIn(id: string): void;
  /** The focus left workspace ID's row line for NEXT. */
  focusOut(id: string, next: EventTarget | null): void;
  /** The pointer is over TARGET in the rail, or (null) has left it. */
  pointerAt(target: EventTarget | null): void;
  /** The rail lost the user, for WHY: close every open panel, and open none until `resume`. */
  dismiss(why: string): void;
  /** The user is back in the rail: open the panel of the row the pointer rests on, at once on its duration or "⋯", after the delay elsewhere on its line. */
  resume(): void;
  /**
   * Hand every open panel's live copy to the popup mirror again (a redraw
   * replaced its row, a scroll or a resize moved its line). Nothing without a
   * mirror.
   */
  mirrorOpen(): void;
  /** Cancel every timer: the rail is gone. */
  dispose(): void;
}

/** Where the pointer is over one workspace's row. */
interface Pointer {
  line: boolean;
  /** The duration field, which lies inside the line. */
  duration: boolean;
  /** The "⋯" control, which lies inside the line. */
  more: boolean;
  panel: boolean;
}

/**
 * Build the page's one hover owner. MIRROR, in the sidebar page, draws each
 * open panel over the feed in the host instead of beside the row
 * (host/sidebar-popups.ts); a page drawing its own sidebar has none.
 */
export function createRowHover(dropdowns: Dropdowns, openDetails: Set<string>, mirror: PopupMirror | null = null): RowHover {
  let copies = new Map<string, DrawnRow[]>();
  const pointers = new Map<string, Pointer>();
  const closeTimers = createKeyedTimers<string>();
  const openTimers = createKeyedTimers<string>();
  /** Set by `dismiss`, cleared by `resume`: no panel opens while it stands. */
  let suspended = false;

  const cancelTimers = (): void => {
    closeTimers.cancelAll();
    openTimers.cancelAll();
  };

  const pointerOver = (id: string): boolean => {
    const at = pointers.get(id);
    return at !== undefined && (at.line || at.panel);
  };

  const focusedIn = (id: string): boolean => {
    const active = document.activeElement;
    return active !== null && (copies.get(id) ?? []).some((row) => row.line.contains(active));
  };

  // THE POPOVER IS A DROPDOWN: a click in the rail outside it (and outside its
  // row's line, which opens it on focus) closes it, and opening another
  // dropdown closes it.
  const register = (id: string, row: DrawnRow): void => {
    dropdowns.opened({
      kind: "row-detail",
      key: `row-detail:${id}`,
      element: row.detail,
      openers: [row.line],
      close: () => {
        setOpen(id, false, "dismiss");
      },
    });
  };

  function setOpen(id: string, open: boolean, via: string): void {
    closeTimers.cancel(id);
    openTimers.cancel(id);
    if (openDetails.has(id) === open) return;
    log.debug("toggling a row's detail panel", {
      operation: "sidebar.row.detail-toggle",
      context: { workspace: id, open, via, copies: copies.get(id)?.length ?? 0 },
    });
    if (open) openDetails.add(id);
    else openDetails.delete(id);
    for (const row of copies.get(id) ?? []) {
      paintRowDetail(row.ws, open);
      if (open) register(id, row);
      else dropdowns.released(row.detail);
    }
    if (open) mirrorOne(id);
    else mirror?.hide(id);
  }

  const scheduleClose = (id: string): void => {
    closeTimers.start(id, HOVER_CLOSE_GRACE_MS, () => {
      if (pointerOver(id) || focusedIn(id)) return;
      setOpen(id, false, "leave");
    });
  };

  // Every path that takes the pointer off the line (`leave`, `pointerAt`) or
  // stops the rail answering it (`dismiss`, `dispose`) cancels this timer, so
  // it fires only for a pointer still resting on the line.
  const scheduleOpen = (id: string): void => {
    openTimers.start(id, HOVER_OPEN_DELAY_MS, () => {
      setOpen(id, true, "hover");
    });
  };

  /** The copy of ID's row to mirror: the first whose line is laid out, or none. */
  const mirrored = (id: string): DrawnRow | undefined =>
    (copies.get(id) ?? []).find((row) => row.line.isConnected && row.line.getClientRects().length > 0);

  /** Hand ID's open panel to the mirror, or take it back when no copy is laid out. */
  const mirrorOne = (id: string): void => {
    if (mirror === null) return;
    const row = mirrored(id);
    if (row === undefined) mirror.hide(id);
    else mirror.show(id, row.detail, row.line);
  };

  /** Whether NODE is inside the line or the panel of a drawn copy of ID's row. */
  const within = (id: string, node: Node | null): boolean =>
    node !== null &&
    (copies.get(id) ?? []).some((row) => row.line.contains(node) || row.detail.contains(node));

  return {
    forgetWithin(root: Node): void {
      for (const [id, rows] of [...copies]) {
        const kept = rows.filter((row) => !root.contains(row.line));
        if (kept.length === 0) copies.delete(id);
        else copies.set(id, kept);
      }
    },
    drawn(id: string, row: DrawnRow): void {
      const list = copies.get(id);
      if (list === undefined) copies.set(id, [row]);
      else list.push(row);
      // A row a redraw drew open is still the page's open dropdown.
      if (openDetails.has(id)) register(id, row);
    },
    enter(id: string, part: HoverPart): void {
      const at = pointers.get(id) ?? { line: false, duration: false, more: false, panel: false };
      at[part] = true;
      pointers.set(id, at);
      closeTimers.cancel(id);
      if (isInstant(part) && !suspended) setOpen(id, true, "hover");
      if (part === "line" && !suspended && !openDetails.has(id)) scheduleOpen(id);
    },
    leave(id: string, part: HoverPart): void {
      const at = pointers.get(id);
      if (at === undefined || !at[part]) return;
      at[part] = false;
      // The duration and the "⋯" lie inside the line: the pointer leaving
      // either is still on the row, unless the line reports its own leave.
      if (isInstant(part)) return;
      if (!at.line && !at.panel) pointers.delete(id);
      if (part === "line") openTimers.cancel(id);
      scheduleClose(id);
    },
    focusIn(id: string): void {
      if (!suspended) setOpen(id, true, "focus");
    },
    focusOut(id: string, next: EventTarget | null): void {
      // `focusout` fires BEFORE the next element takes focus, so
      // `activeElement` is not yet the answer; the event's own
      // `relatedTarget` is.
      const stays = next instanceof Node && (copies.get(id) ?? []).some((row) => row.line.contains(next));
      if (!stays) scheduleClose(id);
    },
    pointerAt(target: EventTarget | null): void {
      if (pointers.size === 0) return;
      const node = target instanceof Node ? target : null;
      for (const id of [...pointers.keys()]) {
        if (within(id, node)) continue;
        log.debug("the pointer left a row it was over without the row seeing it", {
          operation: "sidebar.row.detail-pointer-gone",
          context: { workspace: id, left_rail: node === null },
        });
        pointers.delete(id);
        openTimers.cancel(id);
        scheduleClose(id);
      }
    },
    dismiss(why: string): void {
      suspended = true;
      cancelTimers();
      for (const id of [...openDetails]) setOpen(id, false, why);
    },
    resume(): void {
      suspended = false;
      for (const [id, at] of pointers) {
        if (at.duration || at.more) setOpen(id, true, "hover");
        else if (at.line && !openDetails.has(id)) scheduleOpen(id);
      }
    },
    mirrorOpen(): void {
      for (const id of openDetails) mirrorOne(id);
    },
    dispose(): void {
      cancelTimers();
      pointers.clear();
      copies = new Map();
    },
  };
}
