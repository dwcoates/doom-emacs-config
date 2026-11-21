/**
 * A profiling test run (testrun.v1.TestRunRequest.profile) for the shim's
 * integration harness: every shim a test spawns profiles its whole life into
 * the test's own directory under the unit's (`AGENT_REPL_TEST_PROFILE_DIR`),
 * named as `testrun/testenv.ProfileName` names it, through the shim's
 * `EXIT_PROFILE_ENV`.
 *
 * The unit itself runs under node's `--cpu-prof` (NODE_OPTIONS), which a
 * spawned shim would inherit and profile itself a second time, unnamed; the
 * shim is told its own directory instead, and the flags are taken off.
 */
import { mkdirSync } from "node:fs";
import { basename, isAbsolute, join } from "node:path";
import { expect } from "vitest";

import { EXIT_PROFILE_ENV } from "../../../../logging/ts/profile-session.js";

/** The variable a profiling run names each unit's profiles directory by. */
export const PROFILE_DIR_ENV = "AGENT_REPL_TEST_PROFILE_DIR";

/** NAME as one path component, exactly as `testenv.ProfileName` makes it. */
export function profileName(name: string): string {
  if (!name) throw new Error("profiling: a profile is named by a unit or a test, never by nothing");
  return name.replace(/[^A-Za-z0-9._-]+/g, "_");
}

/** NODE_OPTIONS without node's CPU profiler flags. */
export function withoutCpuProf(options: string): string {
  return options
    .replace(/(^|\s)--cpu-prof-dir=("[^"]*"|\S+)/g, "")
    .replace(/(^|\s)--cpu-prof(?=\s|$)/g, "")
    .trim();
}

/**
 * The environment a shim the test named TEST spawns gets: under a profiling
 * run, its whole-life profile directory (created) and NODE_OPTIONS without the
 * unit's own profiler; outside one, nothing.
 */
export function shimProfileEnv(env: Readonly<Record<string, string | undefined>>, test: string): Record<string, string | undefined> {
  const root = env[PROFILE_DIR_ENV];
  if (!root) return {};
  if (!isAbsolute(root)) throw new Error(`profiling: ${PROFILE_DIR_ENV} names ${JSON.stringify(root)}, which is not absolute`);
  const dir = join(root, profileName(test));
  mkdirSync(dir, { recursive: true });
  const nodeOptions = withoutCpuProf(env.NODE_OPTIONS ?? "");
  return { [EXIT_PROFILE_ENV]: dir, NODE_OPTIONS: nodeOptions === "" ? undefined : nodeOptions };
}

/** The test running now, by its full name, or its file when none runs (a beforeAll). */
export function currentTestName(): string {
  const state = expect.getState();
  return state.currentTestName ?? basename(state.testPath ?? "unknown-test");
}
