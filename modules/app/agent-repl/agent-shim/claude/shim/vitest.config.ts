// Refuses a run that did not come through bin/background.sh: tests only ever
// run at background priority (see that script and require-background.mjs).
import "../../../bin/require-background.mjs";
import { defineConfig } from "vitest/config";
import { fileURLToPath } from "node:url";

// The generated protobuf TS stubs live at proto/gen/ts, outside this
// package. Vite's dev-server fs guard blocks files outside the project
// root by default; allow the agent-repl subtree so the relatively-imported
// stubs load. Runtime bare-import resolution (@bufbuild/protobuf) still
// comes from this package's node_modules, which vite anchors at the root.
const agentReplRoot = fileURLToPath(new URL("../../../", import.meta.url));

export default defineConfig({
  server: { fs: { allow: [agentReplRoot] } },
  test: {
    // WORKER COUNT IS CAPPED. Vitest defaults to one worker per CPU, which is
    // a claim on the whole machine; measured on a 16-CPU host that is 2-5 GiB
    // for one run, and several concurrent runs took the box to a load average
    // of 253 and cost a whole Emacs layer run its evidence. `bin/suite-slot.sh`
    // at the module root is the gate that stops suites overlapping; this cap
    // is the second half of the same promise. minWorkers travels with it
    // because vitest refuses a max below the default min.
    minWorkers: 1,
    maxWorkers: "50%",
    // The vendor guard keeps every test offline. The log setup installs a
    // deterministic inherited sink for canonical JSON logging assertions.
    setupFiles: ["./test/setup.ts", "./test/log-setup.ts"],
    // The integration suite runs under vitest.integration.config.ts
    // (`npm run test:integration`): it spawns the BUILT bundle, so including it
    // here would make a fresh checkout's `npm test` fail for want of dist/.
    exclude: ["**/node_modules/**", "**/dist/**", "test/integration/**"],
    // Tight on purpose: this suite is pure in-process work (no spawned
    // process, no real vendor, no real store). The observed healthy max
    // across 3,806 tests is ~640ms (test/log.test.ts, a bootstrap-stderr
    // logging test); these are ~3x that, rounded. A unit test or hook
    // hitting this is broken, not slow — raise it only with a measured
    // reason, never to paper over a hang.
    testTimeout: 2_500,
    hookTimeout: 2_500,
    teardownTimeout: 2_500,
    coverage: {
      // ISTANBUL, NOT V8, and the swap is a measurement rather than a taste.
      //
      // `@vitest/coverage-v8@2.1.9` merges the RAW V8 coverage of every
      // test-file window with `mergeProcessCovs` BEFORE remapping it through
      // the source maps, so a module compiled in more than one window loses
      // most of one contributor's counts and which contributor survives
      // depends on which files shared a process. Measured here: five files
      // (src/convert/hooks.ts, src/convert/stream-events.ts,
      // src/convert/tools/cron.ts, src/convert/tools/unmodeled.ts,
      // src/engine/turn.ts) reported different branch counts between two runs
      // of this whole suite that differed only in test-FILE order. The webapp
      // package, which shares more modules across its files, lost far more:
      // its src/scroll.ts read 47.71% of statements in the package report
      // against 100% with only its own test file running.
      //
      // Istanbul instruments at transform time and counts inside the module,
      // so there is nothing to reconstruct afterwards: the same two file
      // orders agreed on all 93 files. `bin/coverage-honesty.mjs` at the
      // module root (`npm run coverage:verify`) is the check that keeps it
      // that way, and it depends on this suite keeping one test file per
      // module's worth of isolation -- vitest's default `isolate: true`, which
      // this config does not turn off.
      //
      // The headline number MOVED with the swap, from 99.34% of statements to
      // 98.61%, and the honest figure is the lower one: v8 also credited
      // whole type-only modules (src/proto.ts and its siblings) with 100% of
      // nothing, which istanbul drops for carrying no executable statement.
      provider: "istanbul",
      all: true,
      include: ["src/**/*.ts"],
      exclude: [
        "src/**/*.d.ts",
        "src/**/__generated__/**",
        "src/**/*.generated.ts",
      ],
      reporter: ["text", "json", "json-summary", "html"],
    },
  },
});
