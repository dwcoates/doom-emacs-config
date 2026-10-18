/**
 * THE ONE WAY A TEST SAYS THE PAGE HID OR SHOWED: a page transition event
 * dispatched on the window the test document belongs to.
 *
 * The page's own window, not the global `window`: in the un-isolated unit run
 * (vitest.config.ts, `isolate: false`) the two need not be the same object, and
 * every listener under test is registered on the document's own window.
 */

/** Dispatch a TYPE page transition, persisted or not, on the test document's window. */
export function dispatchPageTransition(type: "pagehide" | "pageshow", persisted: boolean): void {
  const win = document.defaultView;
  if (win === null) throw new Error("dispatchPageTransition: the test document has no window");
  win.dispatchEvent(new win.PageTransitionEvent(type, { persisted }));
}
