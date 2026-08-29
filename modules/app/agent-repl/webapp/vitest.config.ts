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
    // `legacy/` is reference-only, does not compile, and carries no suites;
    // keep vitest's default discovery from ever reaching into it. The
    // integration suite has its own config (vitest.integration.config.ts): it
    // boots the app against a real loopback Connect server, so it must not
    // ride along in the fast unit run either.
    exclude: ["**/node_modules/**", "**/dist/**", "legacy/**", "test/integration/**"],
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
