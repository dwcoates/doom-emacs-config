/**
 * scope — what one drawn section of the rail registered, kept together so it
 * lives exactly as long as that section's drawing.
 *
 * WHY IT EXISTS (2026-10-15). The rail used to throw its whole drawing away
 * on every roster push and rebuild it: the open sections, the hidden band and
 * a hundred recently-merged rows, for a push that changed one row. A section
 * a push leaves unchanged is now kept as it was drawn (`roster.ts`), and what
 * it registered while drawing — its copies of the view (`view.ts`), its
 * ticking clocks, its attention markers, the dropdowns and hover panels its
 * rows opened — must outlive the push with it. A scope holds exactly that,
 * and disposing it when its section is replaced takes it all down at once.
 */
import type { AttentionMarks, RailContext, SidebarContext } from "./context.js";

/** One drawn section's registrations, and the context it draws through. */
export interface DrawScope {
  /** SC bound to this scope: every registration made through it is the scope's. */
  readonly context: SidebarContext;
  /**
   * A new drawing pass keeps this scope's section: mark its attention markers
   * again on BASE (the page's registry), so the pass does not drop them.
   */
  remark(base: AttentionMarks): void;
  /** The section drawn under ROOT is gone: tear down everything it registered. */
  dispose(root: Node): void;
}

/** Open a scope for one section of RAIL's drawing. */
export function openScope(rail: RailContext): DrawScope {
  const viewScope = rail.view.scope();
  const disposers: Array<() => void> = [];
  const marks: Array<{ id: string; element: HTMLElement }> = [];
  const context: SidebarContext = {
    ctx: rail.ctx,
    parentWorkspace: () => rail.parentWorkspace(),
    openDetails: rail.openDetails,
    drawnStatus: rail.drawnStatus,
    dropdowns: rail.dropdowns,
    rowHover: rail.rowHover,
    view: {
      track: (key, wire, paint) => viewScope.track(key, wire, paint),
      ask: (key, value) => rail.view.ask(key, value),
      abandon: (key, token) => rail.view.abandon(key, token),
    },
    attention: {
      mark: (id, element) => {
        marks.push({ id, element });
        rail.attention.mark(id, element);
      },
    },
    onDispose: (fn) => {
      disposers.push(fn);
    },
  };
  return {
    context,
    remark(base: AttentionMarks): void {
      for (const { id, element } of marks) base.mark(id, element);
    },
    dispose(root: Node): void {
      for (const fn of disposers.splice(0)) fn();
      viewScope.dispose();
      rail.dropdowns.forgetWithin(root);
      rail.rowHover.forgetWithin(root);
    },
  };
}
