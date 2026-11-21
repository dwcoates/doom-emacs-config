/**
 * per-document — ONE OF A THING PER PAGE, made on first use.
 *
 * A page's presence, its focus-left signal and its auto-collapse owner each
 * exist once per document: every reader on the page must see the same one,
 * and a test's own document gets its own. The memo is kept here once, keyed
 * weakly by the document, so a page that goes away takes its thing with it.
 */
import { stopAtPageTeardown } from "./page-teardown.js";

/**
 * The thing MAKE makes for a document, made on that document's first ask.
 *
 * A THING THAT LISTENS IS DISPOSED WITH ITS PAGE. Given DISPOSE, the thing is
 * forgotten and disposed when the page is torn down (page-teardown.ts), so a
 * listener it put on the window or the document never outlives the page: a
 * booted test page's window `blur` listener fired into later test files,
 * which share the window (2026-11-21, a merge gate). The next ask after the
 * teardown makes the document a new one.
 */
export function perDocument<T>(make: (doc: Document) => T, dispose?: (thing: T) => void): (doc: Document) => T {
  const made = new WeakMap<Document, T>();
  return (doc) => {
    let thing = made.get(doc);
    if (thing === undefined) {
      const fresh = make(doc);
      made.set(doc, fresh);
      if (dispose !== undefined) {
        stopAtPageTeardown(() => {
          if (made.get(doc) === fresh) made.delete(doc);
          dispose(fresh);
        });
      }
      thing = fresh;
    }
    return thing;
  };
}
