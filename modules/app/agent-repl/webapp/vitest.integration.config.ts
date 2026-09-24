import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";
import { viteCacheDir } from "./vite-cache";

/**
 * The INTEGRATION suite's config, separate from the unit one.
 *
 * These tests boot the whole app under jsdom against a real Connect server on
 * loopback, so they are slower, they hold ports, and they fail for different
 * reasons than a unit test does. Keeping them out of `npm test` means a unit
 * run stays fast and a red integration run names an integration fault.
 *
 * AGENT_REPL_FORBID_VENDOR_CALLS is set as a standing tripwire: nothing in
 * this suite may reach a vendor, and a component that tried would find the
 * flag rather than the network.
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
    environment: "jsdom",
    // The same logger bootstrap the unit run uses: production installs the
    // ClientLog-forwarding logger before any component draws, and `log`
    // refuses a record with no bound identity — so a suite without it fails
    // inside the logger rather than inside the code under test.
    setupFiles: ["./test/setup.ts"],
    include: ["test/integration/**/*.test.ts"],
    env: { AGENT_REPL_FORBID_VENDOR_CALLS: "1" },
    css: true,
    // TIGHT ON PURPOSE. These tests boot the whole app against a real
    // loopback fake daemon, but the daemon is in-process and instant to
    // start: a healthy run's slowest test (measured across all 13 files) is
    // 274.8ms, in refusals.integration.test.ts. Vitest's own defaults
    // (5000ms/10000ms) would let a genuinely hung test burn 20-40x that
    // before failing. ~3x the observed max, so real variance has headroom
    // without masking a hang. If a test needs more, it gets its own
    // `{ timeout: ... }` with a one-line reason, not a raised global.
    // A file's COLD boot is the one standing exception: it is paid once per
    // file in `bootColdOnce`'s `beforeAll` (test/integration/harness.ts),
    // under its own measured bound, so no test body carries it.
    testTimeout: 900,
    hookTimeout: 900,
  },
});
