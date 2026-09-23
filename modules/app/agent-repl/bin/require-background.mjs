// require-background.mjs -- every vitest config in this module imports this
// for its side effect, so vitest refuses to start a run that is not at
// background priority.
//
// The package scripts (`npm test`, `npm run test:integration`, ...) route
// through bin/background.sh, which demotes the run and exports
// AGENT_REPL_BACKGROUND_PRIORITY. A raw `npx vitest` skips the script, and
// with it the demotion: its worker pool would compete with the owner's live
// shim, daemon and store at full priority, which is the load-281 incident
// bin/background.sh records. So a run without the marker fails here, loudly,
// before a single worker starts.
//
// Only bin/background.sh sets the marker. Never set it by hand.

if (!process.env.AGENT_REPL_BACKGROUND_PRIORITY) {
  throw new Error(
    "vitest REFUSED TO START: tests run only at background priority, through bin/background.sh.\n" +
      "  Use the package script (`npm test`, `npm run test:integration`, ...), or wrap the\n" +
      "  command yourself: `<module>/bin/background.sh npx vitest run ...`.\n" +
      "  AGENT_REPL_BACKGROUND_PRIORITY is unset, so this run was never demoted.",
  );
}
