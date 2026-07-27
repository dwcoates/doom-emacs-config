// Global vitest setup: forbid real Claude Agent SDK calls for the whole suite.
//
// Set here rather than per-test so no individual test can forget. With this
// set, src/vendor-guard.ts throws at the one chokepoint that can load the
// vendor SDK, so a test that accidentally takes a non-fake path fails loudly
// instead of spending tokens.
process.env.AGENT_REPL_FORBID_VENDOR_CALLS = "1";
