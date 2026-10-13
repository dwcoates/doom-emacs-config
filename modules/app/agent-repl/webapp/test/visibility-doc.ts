/**
 * visibilityDoc — a document whose `visibilityState` the test sets.
 *
 * jsdom's own document is always "visible", so a test of anything that reads
 * whether the page is hidden (page-seen.ts, the rail's unseen gate) builds one
 * of these and moves it with `set`, which fires `visibilitychange` exactly as
 * the platform does.
 */
export interface VisibilityDoc {
  readonly doc: Document;
  /** Set the visibility to STATE and fire the platform's event. */
  set(state: DocumentVisibilityState): void;
}

export function visibilityDoc(initial: DocumentVisibilityState): VisibilityDoc {
  const doc = document.implementation.createHTMLDocument("visibility");
  let state = initial;
  Object.defineProperty(doc, "visibilityState", { configurable: true, get: () => state });
  return {
    doc,
    set(next: DocumentVisibilityState): void {
      state = next;
      doc.dispatchEvent(new Event("visibilitychange"));
    },
  };
}
