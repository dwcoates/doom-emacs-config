import { beforeAll, vi } from "vitest";

/**
 * FILE HYGIENE FOR THE UN-ISOLATED INTEGRATION AND WEBAPP-LAYER RUNS.
 *
 * `vitest.integration.config.ts` and `vitest.webapp-layer.config.ts` set
 * `isolate: false`, so every file in a worker shares one jsdom. An integration file mounts its app ONCE, in a
 * `beforeAll`, and its scenarios look at that page, so the wipe the unit run
 * does before every test (test/setup-shared-worker.ts) would tear the page
 * down mid-file. Here the reset runs once per FILE instead, before that
 * file's own `beforeAll`: the real clock back, and an empty page, so nothing
 * the previous file left behind is found by this file's queries.
 */
beforeAll(() => {
  vi.useRealTimers();
  if (typeof document !== "undefined") document.body.replaceChildren();
});
