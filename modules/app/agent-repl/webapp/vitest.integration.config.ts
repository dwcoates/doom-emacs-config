import { defineConfig } from "vitest/config";
import { protobufRuntimeAliases } from "./protobuf-runtime-aliases";

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
  resolve: { alias: protobufRuntimeAliases },
  test: {
    environment: "jsdom",
    include: ["test/integration/**/*.test.ts"],
    env: { AGENT_REPL_FORBID_VENDOR_CALLS: "1" },
    css: true,
  },
});
