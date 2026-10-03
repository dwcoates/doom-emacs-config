/**
 * test/run-tmp-root.ts — every vitest run's temp files live under ONE root the
 * run owns, and the run removes it.
 *
 * A vitest `globalSetup`: it runs in the main vitest process before any worker
 * starts, so the `TMPDIR` it sets is the one every worker and every process a
 * test spawns inherits, and every `os.tmpdir()` in the run answers the root.
 *
 * WHY. This suite makes a fresh temp directory in most of its tests (a fake
 * vendor drive, a session, a store socket) and few of them removed theirs. On
 * the host that measured it, the user temp directory had grown to 856,127
 * entries, 391,496 of them `shim-session-*` and 232,634 `fake-drive-*`, and it
 * is the directory every other suite in `bin/test-all.sh` creates in too. A
 * create in that directory measured 0.07ms median on a quiet host and 42ms
 * median with the e2e suite running beside it (0.25ms in a small directory
 * under the same load), with stalls of several seconds: a single fake drive
 * whose own CPU time was 4ms took 5.08s of wall clock, and tests under the
 * 2,500ms unit bound timed out on nothing but those creates. A run-owned root
 * keeps every create in a small directory nobody else writes to, and its
 * removal at teardown means no test can leak one, whether it cleans up or not.
 *
 * WHY UNDER `/tmp` AND NOT `os.tmpdir()`. A unix socket path is capped at 104
 * bytes on macOS, the user temp directory's own path is 49 of them, and the
 * longest socket a test here builds (`test/store/persistence-fixtures.ts`)
 * already spends 97. `/tmp` keeps the root's prefix shorter than the directory
 * it replaces, so no socket path grows.
 */
import { mkdtempSync, rmSync } from "node:fs";
import { join } from "node:path";

/** Where the run's root is made. Short on purpose: see the header. */
export const RUN_TMP_PARENT = "/tmp";

/** Set to the root while a run holds it, so a test can check it is inside one. */
export const RUN_TMP_ROOT_ENV = "AGENT_REPL_VITEST_TMP_ROOT";

/**
 * Make the run's root and point `TMPDIR` at it. Returns the teardown, which
 * removes the root and puts both variables back as they were. A failed
 * removal throws: a root that cannot be removed is a process this run left
 * running, and it is reported, never left behind quietly.
 */
export function makeRunTmpRoot(parent: string): () => void {
  const root = mkdtempSync(join(parent, "sv-"));
  const saved = { tmpdir: process.env.TMPDIR, root: process.env[RUN_TMP_ROOT_ENV] };
  process.env.TMPDIR = root;
  process.env[RUN_TMP_ROOT_ENV] = root;
  return () => {
    restore("TMPDIR", saved.tmpdir);
    restore(RUN_TMP_ROOT_ENV, saved.root);
    rmSync(root, { recursive: true });
  };
}

/** The `globalSetup` entry. Vitest hands it a context argument, so it takes none of its own. */
export default function setup(): () => void {
  return makeRunTmpRoot(RUN_TMP_PARENT);
}

function restore(name: string, value: string | undefined): void {
  if (value === undefined) delete process.env[name];
  else process.env[name] = value;
}
