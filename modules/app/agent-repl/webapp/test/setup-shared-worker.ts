import { beforeEach, vi } from "vitest";

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
 *   `data-component` is exactly what the routing code looks for.
 *
 * So every unit test starts on the real clock and an empty page; a file that
 * wants a fake clock installs it in its own hook, which runs after this one.
 * Files that ask for no dom environment have no document to empty.
 *
 * This is NOT in `test/setup.ts`, because the integration and webapp-layer
 * projects also load that file and they mount ONCE per file in `beforeAll`,
 * fake clock included: a per-test wipe there would tear down the page every
 * scenario is about to look at. Those projects run isolated and need no
 * cross-file hygiene.
 */
beforeEach(() => {
  vi.useRealTimers();
  if (typeof document !== "undefined") document.body.replaceChildren();
});
