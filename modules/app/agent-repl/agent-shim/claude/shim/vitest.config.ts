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
      provider: "v8",
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
