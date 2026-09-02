/**
 * vitest.integration.config.ts — the INTEGRATION suite, run on its own.
 *
 * A separate config rather than a second `include` on the unit config, for two
 * reasons that both matter to whoever runs these:
 *
 *   - `npm test` must stay the fast, hermetic unit suite. The integration
 *     suite spawns a real process per test and depends on `npm run build`
 *     having produced `dist/main.js`, so folding it into `npm test` would make
 *     a fresh checkout's unit run fail for want of a bundle.
 *   - the timeouts are different in kind. A unit test that takes five seconds
 *     is broken; an integration test that spawns a node process, binds a
 *     socket, drives a mocked vendor through a turn and stands the process
 *     down legitimately takes longer, and a shared budget would either mask
 *     unit-suite regressions or flake here.
 *
 * `test/setup.ts` is deliberately NOT loaded: it configures the IN-PROCESS
 * vendor guard, and the vendor here runs in the CHILD, where the harness sets
 * `AGENT_REPL_FORBID_VENDOR_CALLS=1` in the spawn environment itself.
 *
 * NO FAIL-FAST. `bail` stays unset so one run reports every failure it found;
 * fixing them one process-restart at a time is what this suite exists to avoid.
 */
import { defineConfig } from "vitest/config";
import { fileURLToPath } from "node:url";

// Same reason as the unit config: the generated protobuf stubs live outside
// this package, and vite's fs guard blocks them without this allowance.
const agentReplRoot = fileURLToPath(new URL("../../../", import.meta.url));

export default defineConfig({
  server: { fs: { allow: [agentReplRoot] } },
  test: {
    include: ["test/integration/**/*.test.ts"],
    // Each test spawns and tears down its own shim, so they must not share a
    // process-global; threads are fine, but the per-test budget has to cover a
    // real spawn.
    //
    // Tight on purpose: the observed healthy max across 302 tests is ~5.14s
    // (test/integration/session.test.ts, a forced KillSession draining a
    // detached stream) and ~4.25s (test/integration/record.test.ts, a write
    // that rides the real DEFAULT_RETRY_POLICY backoff schedule — 50+200+
    // 800+3000ms — through a real store outage). 16s is ~3x that observed
    // max, with no per-site exception needed: both of those legitimately-slow
    // scenarios already fit inside it with margin. A test hitting this
    // timeout is hung, not merely slow — raise it only with a new measured
    // reason, never to paper over a hang. Hooks here are only
    // `afterEach(cleanupShims)` (SIGKILL + temp-dir removal), which is far
    // cheaper than any test body, so it shares the test budget rather than
    // getting its own inflated one.
    testTimeout: 16_000,
    hookTimeout: 16_000,
    teardownTimeout: 10_000,
  },
});
