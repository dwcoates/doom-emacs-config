import { afterEach, beforeEach, vi } from "vitest";

/**
 * WORKER HYGIENE FOR THE UN-ISOLATED UNIT RUN — and only for it.
 *
 * `vitest.config.ts` sets `isolate: false`, so every file in a worker shares
 * one environment. Two things would otherwise leak from one file into the
 * next:
 *
 * - a fake clock installed by one file would still be frozen when the next
 *   file's first test awaits a real timer, and
 * - a node one test appended to the shared jsdom document would still be
 *   found by the next test's query — and a leftover `data-unit` or
 *   `data-component` is exactly what the routing code looks for, and
 * - an attribute or class one test set on the root element (the feed's
 *   `data-conversation-drawn` mark, a drag's class) would still be read by
 *   the next: `markConversationDrawn` marks a page once and never again
 *   while the mark stands, so `conversation-drawn.test.ts` failed behind
 *   any file whose feed was drawn (2026-11-20, under vitest 5's file order),
 *   and
 * - a property one test defined on the document itself (a `visibilityState`
 *   it controls) would still answer for the next: `page-stall.test.ts`'s
 *   page never showed behind a file that left the document hidden. The
 *   document's own properties as the environment made them (vitest's own
 *   among them, such as the `defaultView` that answers the global window)
 *   are recorded once per worker, and every test starts on exactly those.
 *
 * So every unit test starts on an empty page with a bare root element and
 * document and ENDS on the real clock; a file that wants a fake clock installs it in its
 * own hook. The clock is
 * handed back AFTER each test, not before the next: `test/setup.ts`'s
 * `beforeEach` runs ahead of this file's and arms a real timer through the
 * logger, and a fake clock left behind by the previous file's last test has
 * already lost its timers to that file's environment teardown, so the next
 * file's first `beforeEach` found no `setTimeout` at all (2026-10-17: the
 * whole of `stop-control.test.ts` failed behind `drive.test.ts`). Files that
 * ask for no dom environment have no document to empty.
 *
 * This is NOT in `test/setup.ts`, because the integration and webapp-layer
 * projects also load that file and they mount ONCE per file in `beforeAll`,
 * fake clock included: a per-test wipe there would tear down the page every
 * scenario is about to look at. Those projects run isolated and need no
 * cross-file hygiene.
 */
/** Where the worker keeps each document's own properties as the environment made them. */
const PRISTINE = Symbol.for("agent-repl.test.pristine-documents");

/**
 * DOC's own properties as the environment made them, recorded the first time
 * this file loads with DOC in the worker, before any test file's code runs:
 * setup files run again before every test file, and a later recording would
 * take an earlier file's leftovers for the environment's own.
 */
function pristine(doc: Document): Map<string | symbol, PropertyDescriptor> {
  const holder = globalThis as { [PRISTINE]?: WeakMap<Document, Map<string | symbol, PropertyDescriptor>> };
  holder[PRISTINE] ??= new WeakMap();
  let made = holder[PRISTINE].get(doc);
  if (made === undefined) {
    made = new Map(Reflect.ownKeys(doc).map((key) => [key, Object.getOwnPropertyDescriptor(doc, key)!]));
    holder[PRISTINE].set(doc, made);
  }
  return made;
}

if (typeof document !== "undefined") pristine(document);

/**
 * Puts DOC's own properties back as the environment made them: a property a
 * test added is removed, and one a test redefined is defined again as it was.
 */
function resetDocument(doc: Document): void {
  const made = pristine(doc);
  for (const key of Reflect.ownKeys(doc)) {
    if (!made.has(key)) Reflect.deleteProperty(doc, key);
  }
  for (const [key, descriptor] of made) {
    const now = Object.getOwnPropertyDescriptor(doc, key);
    const same =
      now !== undefined &&
      now.value === descriptor.value &&
      now.get === descriptor.get &&
      now.set === descriptor.set;
    if (!same && descriptor.configurable === true) Object.defineProperty(doc, key, descriptor);
  }
}

beforeEach(() => {
  if (typeof document === "undefined") return;
  document.body.replaceChildren();
  const root = document.documentElement;
  for (const name of root.getAttributeNames()) root.removeAttribute(name);
  resetDocument(document);
});

afterEach(() => {
  vi.useRealTimers();
});
