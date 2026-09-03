import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";

/**
 * THE WEBAPP E2E LAYER — the real app against the REAL daemon.
 *
 * The third and last vitest project in this package, and the only one that
 * does not start its own server. The Go cross-system e2e world
 * (`e2e/webapplayer_e2e_test.go`) owns the whole quartet's lifecycle — real
 * store, real sidecar, real `claude-repld`, real shim over the fake SDK — and
 * launches this project as a child process with the daemon's loopback address
 * in the environment. See `e2e/WEBAPP-LAYER-SPEC.md` for why lifecycle lives
 * there and not here (there is exactly ONE bring-up definition, and it is
 * `NewWorld`).
 *
 * So `npm run test:webapp-layer` on its own is NOT a way to run this: with no
 * daemon handed to it, `test/webapp-layer/real-daemon.ts` throws and names the
 * `go test` invocation that does. A run that found no daemon must never look
 * like a pass.
 *
 * AGENT_REPL_FORBID_VENDOR_CALLS is set as the same standing tripwire the
 * fake-daemon integration project sets: the vendor in this chain is the fake
 * SDK inside the real shim, and a component that reached for a network vendor
 * would find the flag.
 */
export default defineConfig({
  resolve: { alias: protobufRuntimeAliases },
  test: {
    environment: "jsdom",
    // The shared setup, plus this layer's own: the layer mounts in
    // `beforeAll`, which runs before the shared setup's `beforeEach`, so the
    // logger invariant needs installing one hook earlier (see that file).
    setupFiles: ["./test/setup.ts", "./test/webapp-layer/setup.ts"],
    include: ["test/webapp-layer/**/*.test.ts"],
    env: { AGENT_REPL_FORBID_VENDOR_CALLS: "1" },
    css: true,
    // NOT WIDENED FROM THE INTEGRATION PROJECT. 900ms is the measured bound
    // there (slowest healthy integration file: 274.8ms, ~3x) and it stays the
    // global here, because most of what this layer asserts is still DOM
    // drawing that has already arrived. The two things that genuinely cost
    // real process time — booting the app against the real daemon, and
    // driving a real turn through the real shim/store/sidecar — take their own
    // per-site budget with a stated reason at the site, never a raised
    // global.
    testTimeout: 900,
    hookTimeout: 900,
    // ONE WORLD, ONE PAGE AT A TIME. The Go driver hands this child a single
    // daemon and a single registered workspace; two files mounting the same
    // page against it in parallel would interleave their DOM and their
    // adoption. Serial is a correctness requirement here, not a speed
    // tradeoff.
    fileParallelism: false,
  },
});
