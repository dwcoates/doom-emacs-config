/**
 * fake/registry.ts — prompt text → scenario.
 *
 * # Exact `!name` prefixes, and why not a regex
 *
 * A scenario is selected when the prompt STARTS with `!<name>` and the name is
 * followed by whitespace or end-of-string. Nothing fuzzier: a prefix match
 * alone would make `!bash` swallow `!bash-detach`, and a substring match would
 * make any prose mentioning a scenario name change what the turn does. The
 * longest matching name wins, so adding `!bash-detach` beside `!bash` never
 * needs the two to be ordered by hand.
 *
 * Unrecognized text is not an error — it is the ordinary case. It falls through
 * to the plain-prose scenario, which is why an offline session driven by a
 * human behaves like a session rather than refusing every message.
 *
 * # This module is the table's source
 *
 * `AGENTS.md`'s "Mocked vendor: prompt → scenario table" is generated from
 * `SCENARIOS`, and `test/fake/registry.test.ts` asserts the two agree in BOTH
 * directions. So a scenario added here and forgotten there fails the suite, and
 * a table row with no scenario behind it does too.
 */
import { bindLog } from "../log.js";
import type { Scenario } from "./scenario.js";
import { AUTOMATION_SCENARIOS } from "./scenarios/automation.js";
import { FAIL_MARKER, FAILURE_SCENARIOS } from "./scenarios/failures.js";
import { FILE_SCENARIOS } from "./scenarios/files.js";
import { HOOK_SCENARIOS } from "./scenarios/hooks.js";
import { LIFECYCLE_SCENARIOS } from "./scenarios/lifecycle.js";
import { PERMISSION_SCENARIOS } from "./scenarios/permissions.js";
import { PROSE, PROSE_SCENARIOS } from "./scenarios/prose.js";
import { QUESTION_SCENARIOS } from "./scenarios/questions.js";
import { SESSION_SCENARIOS } from "./scenarios/session.js";
import { SHELL_SCENARIOS } from "./scenarios/shell.js";
import { SKILL_SCENARIOS } from "./scenarios/skills.js";
import { SUBAGENT_SCENARIOS } from "./scenarios/subagents.js";
import { TASK_SCENARIOS } from "./scenarios/tasks.js";
import { WEB_SCENARIOS } from "./scenarios/web.js";

const LOGGER = bindLog({ component: "shim-fake", operation: "shim.fake.registry" });

/** Every registered scenario. The default (`name: ""`) is first. */
export const SCENARIOS: readonly Scenario[] = [
  ...PROSE_SCENARIOS,
  ...FILE_SCENARIOS,
  ...SHELL_SCENARIOS,
  ...WEB_SCENARIOS,
  ...SKILL_SCENARIOS,
  ...TASK_SCENARIOS,
  ...SUBAGENT_SCENARIOS,
  ...AUTOMATION_SCENARIOS,
  ...HOOK_SCENARIOS,
  ...PERMISSION_SCENARIOS,
  ...QUESTION_SCENARIOS,
  ...SESSION_SCENARIOS,
  ...FAILURE_SCENARIOS,
  ...LIFECYCLE_SCENARIOS,
];

/**
 * Named scenarios, longest name first.
 *
 * The ordering is what makes `!bash-detach` beat `!bash` without either
 * knowing about the other, so a family can add a longer sibling name freely.
 */
const NAMED: readonly Scenario[] = SCENARIOS.filter((s) => s.name !== "").sort(
  (a, b) => b.name.length - a.name.length,
);

/** Every registered name, for the duplicate check the suite runs. */
export function scenarioNames(): string[] {
  return SCENARIOS.map((s) => s.name);
}

/**
 * The prompt MARKER that selects the failing turn.
 *
 * It survives verbatim from the previous fake because the daemon's e2e
 * acceptance gate spells it identically (`mergeactions_e2e_test.go`): a turn
 * failure is otherwise unprovokable offline, and the daemon's merge pipeline
 * classifies a before-action failure and an after-action failure in OPPOSITE
 * directions, so neither branch is reachable without a way to make a turn go
 * badly.
 *
 * A marker rather than an `!name` because that gate sends readable prose that
 * merely CONTAINS it, so a caller can say which of its turns should fail and
 * still send a sensible message. Named scenarios still win: an explicit
 * `!scenario` is a stronger statement of intent than a marker buried in prose.
 */
export const FAIL_TURN_MARKER = "e2e-fail-this-turn";

/** The scenario one prompt selects. Never throws; unknown text is prose. */
export function selectScenario(text: string): Scenario {
  const trimmed = text.trimStart();
  for (const candidate of NAMED) {
    const token = `!${candidate.name}`;
    if (!trimmed.startsWith(token)) continue;
    const next = trimmed.charAt(token.length);
    if (next === "" || /\s/.test(next)) {
      LOGGER.logVerbose({ scenario: candidate.name }, "fake registry selected a named scenario");
      return candidate;
    }
  }
  if (text.includes(FAIL_TURN_MARKER)) {
    LOGGER.logVerbose({ scenario: FAIL_MARKER.name }, "fake registry matched the e2e failure marker");
    return FAIL_MARKER;
  }
  LOGGER.logVerbose({ scenario: "prose" }, "fake registry fell through to the plain-prose scenario");
  return PROSE;
}
