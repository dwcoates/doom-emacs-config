/**
 * page-teardown — the stops of everything the boot installs for the page's
 * whole lifetime: its timers and its wrapped globals.
 *
 * A PAGE IN PRODUCTION IS NEVER TORN DOWN: it lives until its document is
 * unloaded, and the unload takes every timer and global with it. A TEST that
 * boots the page is torn down, though, and the unit suites share one
 * environment per worker (vitest.config.ts, `isolate: false`). So a boot's
 * page-lifetime install would outlive its test file: the page census's
 * one-minute timer kept firing into later files against a document that was no
 * longer the environment's (2026-10-17, a merge gate's run long enough for the
 * first tick), and every boot stacked another busy probe over the timers.
 * The boot registers each stop here, and a boot test tears the page down.
 */

const stops: (() => void)[] = [];

/** Register STOP, run when the page is torn down. */
export function stopAtPageTeardown(stop: () => void): void {
  stops.push(stop);
}

/** Run every registered stop, the latest first, and forget them. */
export function tearDownPage(): void {
  for (const stop of stops.splice(0).reverse()) stop();
}
