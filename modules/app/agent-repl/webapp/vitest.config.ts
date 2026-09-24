// Refuses a run that did not come through bin/background.sh: tests only ever
// run at background priority (see that script and require-background.mjs).
import "../bin/require-background.mjs";
import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";
import { viteCacheDir } from "./vite-cache";

/**
 * Vitest stubs CSS imports out to an empty module by default, which would
 * silently empty the `?raw` stylesheet import that test/styles.test.ts asserts
 * against. Processing CSS keeps that import carrying the real source.
 *
 * Vite's own build reads vite.config.ts, so this file is test-only.
 */
export default defineConfig({
  // OUT OF `node_modules`, WHICH IS A READ-ONLY SYMLINK IN THE E2E SANDBOX
  // AND A GARBAGE-COLLECTED SHARED TREE ON THE HOST. Vite's default
  // `cacheDir` is `node_modules/.vite`; see vite-cache.ts for the whole
  // account, and test/vite-cache.test.ts for the check that keeps every
  // config in this package on it.
  cacheDir: viteCacheDir,
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
    // WORKER COUNT IS CAPPED, because the default is "one per CPU" and that
    // is a claim on the WHOLE machine. Measured on a 16-CPU host: 16 workers
    // at a 116 MiB mean and a 333 MiB peak, so one `npm test` alone holds
    // 2-5 GiB. That is fine when a suite runs alone and ruinous when several
    // do -- four concurrent runs took this box to a load average of 253 and
    // made an Emacs layer run fail 37 of 45 scenarios on boot alone.
    // `bin/suite-slot.sh` is the gate that stops suites overlapping; this cap
    // is the second half of the same promise, for the runs that slip past it.
    // Half the CPUs measured at 5.3s against 5.2s for all sixteen: the suite
    // is not CPU-bound at this size, so the cap costs nothing to buy back.
    // minWorkers travels with it: vitest refuses a max below the default min.
    minWorkers: 1,
    maxWorkers: "50%",

    // The integration suite has its own config (vitest.integration.config.ts):
    // it boots the app against a real loopback Connect server, so it must not
    // ride along in the fast unit run. The webapp e2e layer
    // (vitest.webapp-layer.config.ts) is excluded for a stronger reason: it
    // needs the REAL daemon the Go e2e world spawns, and refuses to run
    // without it, so riding along here would fail every unit run.
    //
    // THE LAYER EXCLUSION IS BY FILE SUFFIX, NOT BY DIRECTORY. `.layer.test.ts`
    // is already the layer's own name for "a file the Go world drives" — the
    // scenario-matrix check reads the directory by that exact suffix
    // (e2e/scenariomatrix_test.go, deriveWebappCoverage) — and the directory
    // also holds the layer's HELPERS (drive.ts, perf.ts, real-daemon.ts),
    // which need no daemon and whose own tests belong in the fast run like any
    // other unit test. Excluding the whole directory left them untestable.
    exclude: ["**/node_modules/**", "**/dist/**", "test/integration/**", "test/webapp-layer/**/*.layer.test.ts"],
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
      // ISTANBUL, NOT V8, and the swap is a measurement rather than a taste.
      //
      // `@vitest/coverage-v8@2.1.9` merges the RAW V8 coverage of every
      // test-file window with `mergeProcessCovs` BEFORE remapping it through
      // the source maps. A module compiled in more than one window then loses
      // most of one contributor's counts: src/scroll.ts read 100% statements
      // with only test/scroll.test.ts running and 47.71% with test/feed added,
      // src/format.ts 100% against 84.61%, src/markdown.ts 100% against
      // 59.52%. Worse, the surviving contributor depends on which files share
      // a process, so eleven files' counts moved between two runs of the whole
      // suite that differed only in test-file order. Isolation does not repair
      // it -- every one of those figures was taken with `--isolate` on.
      //
      // Istanbul instruments at transform time and counts inside the module,
      // so there is no attribution to reconstruct afterwards: the same two
      // runs agreed on all 100 files, and src/scroll.ts reads the same alone
      // as it does in the full suite. `bin/coverage-honesty.mjs`
      // (`npm run coverage:verify`) is the check that keeps it that way.
      provider: "istanbul",
      all: true,
      include: ["src/**/*.ts"],
      exclude: ["src/**/*.d.ts", "src/**/generated/**"],
      reporter: ["text", "json", "json-summary", "html"],
      reportsDirectory: "coverage",
      // WHY `npm run coverage` PASSES `--isolate` BACK, overriding the
      // `isolate: false` above: the istanbul provider snapshots and RESETS the
      // per-module counters at the end of each test file, and a module is only
      // instantiated once per shared environment. Un-isolated, a module first
      // loaded by one file therefore reports nothing for every later file that
      // exercises it -- src/main.ts, src/feed/cards/hook.ts and
      // src/panels/refused.ts each read 0% that way against 0%, 100% and
      // 98.64% isolated. So the fast unit run keeps the shared environment and
      // the coverage run buys a fresh one per file: 8.2s against 3.7s, which
      // is what an honest per-file number costs here.
    },
  },
});
