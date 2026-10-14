/**
 * test/integration-support/detach-gate.ts — the lever that parks the mocked
 * vendor's detached work.
 *
 * The fake's `!bash-detach` scenario reads `AGENT_REPL_FAKE_DETACH_GATE`: when
 * it names a path, the run parks after its first spool line until that path
 * exists. A test hands the gate's path to the shim's environment and releases
 * it once it has observed what must hold while the work is still live.
 */
import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

/** A detach gate: a path the mock vendor's detached work parks on until it exists. */
export interface DetachGate {
  /** The path to name in `AGENT_REPL_FAKE_DETACH_GATE`. */
  readonly path: string;
  /** Let the parked work go on. */
  release(): void;
  /** Remove the gate's directory. */
  dispose(): void;
}

/** A fresh, closed detach gate in a directory of its own. */
export function detachGate(): DetachGate {
  const dir = mkdtempSync(join(tmpdir(), "shim-detach-gate-"));
  const path = join(dir, "release");
  return {
    path,
    release: () => writeFileSync(path, ""),
    dispose: () => rmSync(dir, { recursive: true, force: true }),
  };
}
