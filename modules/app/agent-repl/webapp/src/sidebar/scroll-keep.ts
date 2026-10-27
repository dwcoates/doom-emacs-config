/**
 * scroll-keep — A REDRAW NEVER MOVES THE RAIL.
 *
 * WHY IT EXISTS (owner request, 2026-10-27). A push whose section changed
 * draws that section anew (`roster.ts` `keepOrDraw`): the Open section on
 * every workspace opened or closed, Deploys on every landing and every merge
 * queue change. The new section brings a new rows region, and a new region
 * starts at its top, so restoring a workspace from Deploys threw both
 * sections back to their tops. A kept region can be moved by a draw too: its
 * rows are swapped, and a layout read between the swap's halves clamps it.
 *
 * So the rail reads every viewport's position before a draw and puts it back
 * once the draw has fitted the bands (`sidebar.ts` `drawRosterNow`), whatever
 * the draw did to the elements. The position is read and restored inside the
 * draw's one task, so no gesture of the reader's can fall between the two.
 * Only a viewport that can no longer scroll as far (its rows shrank) stops
 * short, where the box itself clamps it.
 *
 * Each viewport is named (`sectionRegions`, and the rail's body under the app
 * it shows), and a name's position outlives a draw that does not show it, so
 * returning from Chat finds the workspace sections where they were left.
 */
import { log } from "../log.js";
import { railScrollKept } from "../scroll.js";
import { sectionRegions } from "./band-fit.js";

/** A viewport of the rail, by the name its position is kept under. */
interface NamedViewport {
  name: string;
  region: HTMLElement;
}

/** The rail's keeper of its viewports' positions across a draw. */
export interface RailScrollKeep {
  /** Read every viewport's position under BODY, before a draw. */
  capture(body: HTMLElement, app: string): void;
  /** Put every viewport under BODY back where `capture` read it. */
  restore(body: HTMLElement, app: string): void;
}

/** BODY's viewports: the rail's body itself, under APP, then each section's rows region. */
function viewports(body: HTMLElement, app: string): NamedViewport[] {
  return [{ name: `body:${app}`, region: body }, ...sectionRegions(body)];
}

/** A keeper remembering each named viewport's last position. */
export function createRailScrollKeep(): RailScrollKeep {
  const positions = new Map<string, number>();
  return {
    capture(body: HTMLElement, app: string): void {
      for (const { name, region } of viewports(body, app)) positions.set(name, region.scrollTop);
    },
    restore(body: HTMLElement, app: string): void {
      for (const { name, region } of viewports(body, app)) {
        const kept = positions.get(name);
        if (kept === undefined) continue;
        const found = region.scrollTop;
        if (found === kept) continue;
        railScrollKept(region, kept);
        const to = region.scrollTop;
        log.info("the rail's redraw moved a section; it is back where the reader left it", {
          operation: "sidebar.scroll-kept",
          context: { viewport: name, drawn_at: found, kept, to, clamped: to !== kept },
        });
      }
    },
  };
}
