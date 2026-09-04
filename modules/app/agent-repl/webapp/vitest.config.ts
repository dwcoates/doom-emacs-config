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
    setupFiles: ["./test/setup.ts"],
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
    },
  },
});
