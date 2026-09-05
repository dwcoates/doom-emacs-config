import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";

/**
 * Vitest stubs CSS imports out to an empty module by default, which would
 * silently empty the `?raw` stylesheet import that test/styles.test.ts asserts
 * against. Processing CSS keeps that import carrying the real source.
 *
 * Vite's own build reads vite.config.ts, so this file is test-only.
 */
export default defineConfig({
  resolve: { alias: protobufRuntimeAliases },
  test: {
    css: true,
    setupFiles: ["./test/setup.ts", "./test/setup-shared-worker.ts"],
    // The unit suite spent far more wall time standing a fresh jsdom up for
    // each of its 97 files than running the 3369 tests inside them: ~13.5s
    // isolated against ~5.5s here, with the tests themselves unchanged.
    // Reusing one environment per worker is only safe because no file leaves
    // global state behind for the next one — test/setup-shared-worker.ts hands back the real
    // clock and empties the page before every test, and each file uninstalls
    // what it installs. `npx vitest run --no-isolate --sequence.shuffle
    // --sequence.seed=<n>` is how that is checked; it must stay green for any
    // seed, so a new order dependency is a bug in the file that leaks, never a
    // reason to turn isolation back on.
    isolate: false,
    // The integration suite has its own config (vitest.integration.config.ts):
    // it boots the app against a real loopback Connect server, so it must not
    // ride along in the fast unit run. The webapp e2e layer
    // (vitest.webapp-layer.config.ts) is excluded for a stronger reason: it
    // needs the REAL daemon the Go e2e world spawns, and refuses to run
    // without it, so riding along here would fail every unit run.
    exclude: ["**/node_modules/**", "**/dist/**", "test/integration/**", "test/webapp-layer/**"],
    // TIGHT ON PURPOSE, RE-MEASURED after the 300ms bound tripped three times
    // under load on otherwise-passing tests (question.test.ts, shell.test.ts,
    // feed.test.ts). Four `npx vitest run --reporter=json` passes (2 quiet, 2
    // with `yes` x4 pinning all 16 cores) put the observed healthy max at
    // 272.8ms — already inside the old 300ms bound with no headroom, which is
    // the flake: no test here has a real timer or a heavy fixture, the whole
    // suite is mocked/fake-timered, and host scheduling noise alone closes the
    // gap. ~3x that observed max, so real variance has headroom without
    // masking a hang. If a test needs more, it gets its own
    // `{ timeout: ... }` with a one-line reason, not a raised global.
    testTimeout: 850,
    hookTimeout: 850,
    coverage: {
      provider: "v8",
      all: true,
      include: ["src/**/*.ts"],
      exclude: ["src/**/*.d.ts", "src/**/generated/**"],
      reporter: ["text", "json", "json-summary", "html"],
      reportsDirectory: "coverage",
      // WHY: Establish the baseline before coverage-closing work enforces 90%.
      //
      // `npm run coverage` passes `--isolate` back, overriding the `isolate:
      // false` above, because the v8 provider attributes a module's execution
      // to the file run that instantiated it. When files share a worker the
      // module is instantiated once and later files' exercise of it goes
      // unattributed: src/rpc/streams.ts measured 130 covered lines isolated
      // and 115 un-isolated, differing run to run. A number that moves without
      // the code moving is not a measurement, so the reported figure is taken
      // the slow, accurate way.
    },
  },
});
