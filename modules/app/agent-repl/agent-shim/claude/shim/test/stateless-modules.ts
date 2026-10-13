/**
 * Keep the shim's generated protobuf modules across `vi.resetModules()`.
 *
 * A suite that re-imports `main.js` for a fresh module graph per test (the
 * logger refuses a second configuration, so a fresh graph is its isolation)
 * re-evaluated every generated descriptor with it: 220-270ms of each
 * re-import, measured 2026-10-13, which a loaded host stretched past the 2.5s
 * test timeout. The generated code holds no state, so one instance serves
 * every graph. A module mock survives `vi.resetModules()`; the rest of the
 * graph is still evaluated fresh.
 *
 * ONLY THE SUITES THAT RESET CALL THIS. Pinned in the shared setup file, every
 * test file evaluated the generated code whether it used it or not, which cost
 * the whole suite more CPU than it saved.
 */
import { vi } from "vitest";
import * as proto from "../src/proto.js";

/** Make every later import of `src/proto.js` answer the one evaluated instance. */
export function keepGeneratedProtosAcrossResets(): void {
  vi.doMock("../src/proto.js", () => proto);
}
