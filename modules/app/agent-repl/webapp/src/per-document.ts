/**
 * per-document — ONE OF A THING PER PAGE, made on first use.
 *
 * A page's presence, its focus-left signal and its auto-collapse owner each
 * exist once per document: every reader on the page must see the same one,
 * and a test's own document gets its own. The memo is kept here once, keyed
 * weakly by the document, so a page that goes away takes its thing with it.
 */

/** The thing MAKE makes for a document, made on that document's first ask. */
export function perDocument<T>(make: (doc: Document) => T): (doc: Document) => T {
  const made = new WeakMap<Document, T>();
  return (doc) => {
    let thing = made.get(doc);
    if (thing === undefined) {
      thing = make(doc);
      made.set(doc, thing);
    }
    return thing;
  };
}
