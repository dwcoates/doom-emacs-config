/**
 * SHARED WORLDS AND MULTI-TURN SCENARIOS.
 *
 * WHY THIS MODULE EXISTS: three scenarios in the corpus carried a
 * `manual_setup` note asking the operator to do something the harness could
 * not — because the harness gave every scenario its own fresh `mkdtemp` config
 * root and let it submit exactly ONE prompt. Under that shape:
 *
 *   - `identity-rotation-clear` sends `/clear` to a session with no history,
 *     so nothing rotates and there is no before/after to compare;
 *   - `compaction-directed` sends `/compact` to an empty session, which
 *     compacts nothing and captures nothing;
 *   - both were annotated "run this second, against the same config root as
 *     prose-streamed" — an instruction to a human because the harness had no
 *     way to express it.
 *
 * A note asking the operator to hand-arrange state the script could arrange is
 * not a precondition, it is a missing feature. This module is that feature.
 *
 * TWO CONCEPTS:
 *
 *   A WORLD is a shared cwd and account root. Scenarios naming the same world
 *   run in CORPUS ORDER against it, so a later one sees everything the earlier
 *   ones did. That is how a `/clear` has an identity to rotate and a `/compact`
 *   has a conversation to compact.
 *
 *   A MULTI-TURN scenario submits several prompts. Turns run on ONE query by
 *   default; a turn marked `resume` closes the query and opens a fresh one
 *   resuming the same vendor session id — the shim's own resume path, and the
 *   only way to capture what a resume actually costs.
 *
 * Pure functions; the run loop lives in capture.mjs.
 */

/** The corpus spelling that puts a scenario in a shared world. */
export const WORLD_PREFIX = "shared-world:";

/**
 * The world a scenario belongs to, or `null` for an isolated one.
 *
 * A malformed `config_root` throws rather than silently isolating the
 * scenario: a typo'd world name would make `identity-rotation-clear` run
 * against an empty session again and capture nothing, which is precisely the
 * failure this module exists to end. It must be loud.
 */
export function worldOf(scenario) {
  const value = scenario?.config_root;
  if (value === undefined || value === null) return null;
  if (typeof value !== "string" || !value.startsWith(WORLD_PREFIX)) {
    throw new Error(
      `capture: scenario ${scenario?.name} has config_root ${JSON.stringify(value)}; ` +
        `it must be "${WORLD_PREFIX}<name>" or absent`,
    );
  }
  const name = value.slice(WORLD_PREFIX.length);
  if (name === "") {
    throw new Error(`capture: scenario ${scenario?.name} names an empty world`);
  }
  return name;
}

/**
 * Group scenarios into run order: one group per world, isolated scenarios in
 * groups of their own.
 *
 * A world's group appears at the position of its FIRST member, and its members
 * keep corpus order — so the corpus file itself is the source of truth for
 * "what has happened in this world so far", readable top to bottom.
 */
export function planWorlds(scenarios) {
  const groups = [];
  const byWorld = new Map();
  for (const scenario of scenarios) {
    const world = worldOf(scenario);
    if (world === null) {
      groups.push({ world: null, scenarios: [scenario] });
      continue;
    }
    const existing = byWorld.get(world);
    if (existing === undefined) {
      const group = { world, scenarios: [scenario] };
      byWorld.set(world, group);
      groups.push(group);
      continue;
    }
    existing.scenarios.push(scenario);
  }
  return groups;
}

/**
 * Normalize a scenario's prompt(s) into an ordered list of turns.
 *
 * Accepts the single-prompt spelling unchanged, so the 60-odd one-shot
 * scenarios in the corpus need no edit.
 */
export function promptTurnsOf(scenario) {
  if (Array.isArray(scenario?.prompts)) {
    return scenario.prompts.map((entry, index) => normalizeTurn(entry, scenario, index));
  }
  if (typeof scenario?.prompt === "string" && scenario.prompt !== "") {
    return [{ text: scenario.prompt, resume: false }];
  }
  return [];
}

/** One entry of a `prompts` array, as a turn. */
function normalizeTurn(entry, scenario, index) {
  if (typeof entry === "string") {
    if (entry === "") {
      throw new Error(`capture: scenario ${scenario?.name} prompt ${index} is empty`);
    }
    return { text: entry, resume: false };
  }
  if (entry !== null && typeof entry === "object" && typeof entry.text === "string" && entry.text !== "") {
    return { text: entry.text, resume: entry.resume === true };
  }
  throw new Error(
    `capture: scenario ${scenario?.name} prompt ${index} must be a non-empty string or ` +
      `{ text, resume? }`,
  );
}

/** Whether a scenario can be driven by prompts alone (as opposed to MANUAL). */
export function isPromptDriven(scenario) {
  return promptTurnsOf(scenario).length > 0;
}

/**
 * The turn on which a resume happens, if any — used by the run loop to decide
 * when it must tear the query down and open a new one.
 */
export function resumeTurnIndexes(scenario) {
  return promptTurnsOf(scenario)
    .map((turn, index) => (turn.resume ? index : -1))
    .filter((index) => index >= 0);
}

/**
 * A scenario that resumes a session captured on an EARLIER RUN, days ago.
 *
 * WHY THIS EXISTS: the `resume: true` TURN spelling reopens the session inside
 * the SAME process, moments after the first turn — a WARM resume, with the
 * vendor's prompt cache still live. The cold-context gate the `cold-resume`
 * scenario exists for trips only once that cache has LAPSED, which no
 * same-process run can wait out, and which the corpus therefore could not
 * reach: two 2026-09-03 attempts and the harness fix that followed all captured
 * warm resumes.
 *
 * This field names a capture ALREADY IN THE CORPUS instead. Its session is as
 * cold as its commit date, so resuming it is exactly the lapsed-cache case —
 * reproducibly, from committed bytes, with no waiting.
 *
 * Returns the capture directory name, or `null` for an ordinary scenario.
 */
export function resumeCaptureOf(scenario) {
  const value = scenario?.resume_capture;
  if (value === undefined || value === null) return null;
  if (typeof value !== "string" || value === "") {
    throw new Error(
      `capture: scenario ${scenario?.name} has resume_capture ` +
        `${JSON.stringify(value)}; it must be a non-empty capture directory name`,
    );
  }
  if (worldOf(scenario) !== null) {
    throw new Error(
      `capture: scenario ${scenario?.name} names both a world and a resume_capture; ` +
        "a resumed session brings its own cwd and cannot share one",
    );
  }
  return value;
}
