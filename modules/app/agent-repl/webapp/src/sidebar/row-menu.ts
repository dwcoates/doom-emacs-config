/**
 * row-menu — a row's "⋯" verb menu, opened as a popup BESIDE ITS ROW, where
 * the row's hover panel opens and looking as it does (owner request,
 * 2026-11-06).
 *
 * The menu used to open downward inside the row's own box, pushing the rows
 * under it down the rail. It now stands where the row's detail panel stands:
 * its top-left corner on the top-right corner of the row's line, placed by the
 * same `placeBeside`, drawn in the same card. In the sidebar page it is drawn
 * OVER THE FEED by the host (host/sidebar-popups.ts), as the panel is, and the
 * host replays every click and keystroke on its copy here, on the rail's own
 * menu, so the verbs keep running from the one place that holds their
 * requests and draws their refusals.
 *
 * IT CLOSES AS THE HOVER POPUPS CLOSE. It is one of the rail's dropdowns
 * (`dropdowns.ts`): a click in the rail outside it, another dropdown opening
 * (the row's own panel among them), the user leaving the rail
 * (`transients.ts`) and a press over the feed outside the host's copy
 * (`dismiss`) all close it. The "⋯" control and the row's right-click toggle
 * it.
 *
 * ONE OPEN MENU FOR THE PAGE, held here and not by the row: a push that
 * redraws the row draws a new, shut menu, and the menu the reader had open
 * went with the old drawing, so `placeOpen` lets go of its copy over the feed
 * rather than leave a menu standing whose controls are gone.
 */
import { log } from "../log.js";
import { placeBeside, viewportOf } from "../placement.js";
import type { PopupMirror } from "../host/sidebar-popups.js";
import type { Dropdowns } from "./dropdowns.js";

/** The key a row's menu is mirrored under (host/sidebar-popups.ts), apart from its panel's. */
export function rowMenuKey(id: string): string {
  return `row-menu:${id}`;
}

/** The page's one owner of the open row menu. */
export interface RowMenus {
  /** Open the menu of workspace ID's row WS, or shut it when it is the one open. */
  toggle(id: string, ws: HTMLElement): void;
  /**
   * Place the open menu again (a draw, a scroll or a resize moved its line),
   * or let go of it when the draw threw its row away.
   */
  placeOpen(): void;
}

/** The parts of a drawn row the menu stands beside and is opened from. */
interface MenuParts {
  menu: HTMLElement;
  line: HTMLElement;
  more: HTMLElement | null;
}

/**
 * WS's own menu and line, or null when WS draws no menu of its own (a nested
 * row's menu is that row's). A row with a menu and no line is a drawing this
 * module cannot place, and is refused.
 */
function partsOf(ws: HTMLElement): MenuParts | null {
  const menu = ws.querySelector<HTMLElement>(":scope > .sb-menu");
  if (menu === null) return null;
  const line = ws.querySelector<HTMLElement>(":scope > .row");
  if (line === null) {
    log.error("a row's verb menu has no row line to stand beside", {
      operation: "sidebar.row-menu.no-line",
      context: { workspace: ws.getAttribute("data-roster-row") },
    });
    throw new Error("agent-repl: a row's verb menu has no row line to stand beside");
  }
  return { menu, line, more: line.querySelector<HTMLElement>(":scope > .sb-more") };
}

/**
 * Build the page's one row menu owner. MIRROR, in the sidebar page, draws the
 * open menu over the feed; a page drawing its own sidebar has none, and the
 * menu stands beside the row in the page itself.
 */
export function createRowMenus(dropdowns: Dropdowns, mirror: PopupMirror | null = null): RowMenus {
  let open: (MenuParts & { id: string }) | null = null;

  const place = (): void => {
    if (open === null) return;
    if (mirror !== null) {
      mirror.show(rowMenuKey(open.id), open.menu, open.line);
      return;
    }
    placeBeside(open.menu, open.line.getBoundingClientRect(), viewportOf());
  };

  /** Shut the open menu, for WHY; RELEASE when its own gesture shut it rather than the dropdowns. */
  const shut = (why: string, release: boolean): void => {
    if (open === null) return;
    const { id, menu } = open;
    open = null;
    menu.hidden = true;
    if (release) dropdowns.released(menu);
    mirror?.hide(rowMenuKey(id));
    log.debug("shutting a row's verb menu", {
      operation: "sidebar.row-menu.shut",
      context: { workspace: id, why },
    });
  };

  return {
    toggle(id, ws) {
      const parts = partsOf(ws);
      if (parts === null) return;
      if (open !== null && open.menu === parts.menu) {
        shut("its own control", true);
        return;
      }
      parts.menu.hidden = false;
      const opened = { ...parts, id };
      // REGISTERED BEFORE IT IS PLACED: the row panel it closes takes its copy
      // off the feed first, so the menu's copy is the one left standing.
      dropdowns.opened({
        kind: "row-menu",
        element: parts.menu,
        openers: parts.more === null ? [] : [parts.more],
        close: (why) => {
          if (open === opened) shut(why, false);
          else parts.menu.hidden = true;
        },
      });
      open = opened;
      log.debug("opening a row's verb menu beside its row", {
        operation: "sidebar.row-menu.open",
        context: { workspace: id, mirrored: mirror !== null },
      });
      place();
    },
    placeOpen() {
      if (open === null) return;
      if (!open.menu.isConnected) {
        shut("its row was redrawn", true);
        return;
      }
      place();
    },
  };
}
