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
 *   `data-component` is exactly what the routing code looks for.
 *
 * So every unit test starts on an empty page and ENDS on the real clock; a
 * file that wants a fake clock installs it in its own hook. The clock is
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
beforeEach(() => {
  if (typeof document !== "undefined") document.body.replaceChildren();
});

afterEach(() => {
  vi.useRealTimers();
});
