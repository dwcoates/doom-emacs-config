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
 * # Golden-name ALIASES
 *
 * `testdata/captures/MANIFEST.md`'s capture-directory names and this
 * registry's own `!name` prompts drifted apart over time (`hook-succeeded`
 * vs. `!hook-success`, and so on). `ALIASES` closes that gap for every
 * golden that reproduces from EXACTLY ONE registered scenario: the golden's
 * own name becomes a second, equally valid `!name` token that resolves to
 * the SAME `Scenario` object. An alias never renames a scenario (its `.name`
 * stays the canonical one the e2e suite already spells) and is never added
 * to `SCENARIOS`, so the AGENTS.md table and the registry's one-row-per-
 * scenario contract are untouched. A golden that only reproduces from
 * SEVERAL scenarios in combination (`artifact-publish-and-list`,
 * `schedule-wakeup-schedule-and-stop`, `send-message-queued-and-resumed`,
 * `task-acts-create-change-reject`, `worktree-enter-exit-kept-and-removed`,
 * `read-whole-head-range`, `write-created-and-updated`,
 * `grep-content-files-count`) has no alias here — see
 * `testdata/captures/MANIFEST.md`'s `scenarios:` column for those.
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
import { NETWORK_RESUME, SUBAGENT_SCENARIOS } from "./scenarios/subagents.js";
import { isNetworkResumePrompt } from "../engine/network-resume-prompt.js";
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
 * Golden capture name → the ONE registered scenario that reproduces it.
 *
 * See "Golden-name ALIASES" above. Keys are MANIFEST.md capture-directory
 * names (never `!`-prefixed); values are canonical `Scenario.name`s that must
 * already exist in `SCENARIOS` — `test/fake/registry.test.ts` asserts every
 * value resolves and every key round-trips through `selectScenario`.
 */
export const ALIASES: Readonly<Record<string, string>> = {
  "account-usage": "usage-full",
  "bash-detached": "bash-detach",
  "bash-foreground-completed": "bash",
  "bash-image-output": "bash-image",
  "bash-interrupted-by-timeout": "bash-timeout",
  "bash-nonzero-exit": "bash-fail",
  "bash-partial-output-with-spill": "bash-spill",
  "compaction-directed": "compact",
  "context-injected-memory": "memory",
  "context-injected-skills": "skills-injected",
  "context-usage": "context-usage-drift",
  "fan-wide-cancel": "cancel-all",
  "fast-mode": "fast-on",
  "held-turn-gate": "hold",
  "hook-succeeded": "hook-success",
  "ide-diagnostics-after-edit": "ide-diagnostics",
  "identity-rotation-clear": "rotate",
  "mcp-server-healths": "mcp-all",
  "mcp-unmodeled-tool": "mcp-tool",
  "model-changed": "model-fallback",
  "permission-mode-changed": "perm-allow-standing-mode",
  "permission-undecidable-parked": "perm-hold",
  "plan-mode-enter-exit": "plan",
  "prose-streamed": "",
  "push-notification-sent": "push-sent",
  "question-free-text": "ask-free",
  "question-multi-select": "ask-multi",
  "question-multiple-in-one-batch": "ask-multi",
  "question-single-select": "ask-single",
  "question-unanswered": "ask-unanswered",
  "report-findings": "findings",
  "skill-invocation": "skill",
  "subagent-sync-nested-activity": "subagent",
  "turn-stop-error-during-execution": "fail-execution",
  "turn-stop-hook-stop": "fail-stop-hook",
  "turn-stop-max-budget-usd": "fail-budget",
  "turn-stop-max-structured-output-retries": "fail-structured-output",
  "turn-stop-max-turns": "fail-max-turns",
  "vendor-answered-slash-commands": "slash",
};

function scenarioNamed(name: string): Scenario {
  const found = SCENARIOS.find((s) => s.name === name);
  if (found === undefined) {
    throw new Error(`fake registry: ALIASES points at unknown scenario "${name}"`);
  }
  return found;
}

/** One matchable `!name` token, and the scenario it resolves to. */
interface NamedCandidate {
  readonly token: string;
  readonly scenario: Scenario;
}

/**
 * Named scenarios AND golden-name aliases, longest token first.
 *
 * The ordering is what makes `!bash-detach` beat `!bash` without either
 * knowing about the other, so a family can add a longer sibling name freely.
 * An alias's token is its golden name, but the candidate it resolves to is
 * the ALIASED scenario, unchanged — selecting `!hook-succeeded` returns the
 * same `Scenario` object `!hook-success` does, `.name` included.
 */
const NAMED: readonly NamedCandidate[] = [
  ...SCENARIOS.filter((s) => s.name !== "").map((s) => ({ token: s.name, scenario: s })),
  ...Object.entries(ALIASES).map(([alias, target]) => ({ token: alias, scenario: scenarioNamed(target) })),
].sort((a, b) => b.token.length - a.token.length);

/** Every registered name, for the duplicate check the suite runs. */
export function scenarioNames(): string[] {
  return SCENARIOS.map((s) => s.name);
}

/**
 * The prompt MARKER that selects the failing turn.
 *
 * It survives verbatim from the previous fake because the daemon's e2e
 * acceptance gate spells it identically: the merge pipeline classifies a
 * before-action failure and an after-action failure in OPPOSITE directions (a
 * failed precondition refuses the landing; a post-landing failure only rides
 * the terminal), so neither branch is reachable without a way to make a turn
 * go badly.
 *
 * A marker rather than an `!name` because a CONFIGURED MERGE ACTION *is* its
 * text — the daemon submits the recorded words verbatim — so the only way to
 * fail one is to write an action a human would plausibly configure and bury
 * the marker in it. Named scenarios still win: an explicit `!scenario` is a
 * stronger statement of intent than a marker buried in prose.
 *
 * THE GATE IS `e2e/mergequeue_e2e_test.go`'s
 * TestFailMarkerFailsABeforeActionRunAndRidesAnAfterActionTerminal. It
 * replaces `mergeactions_e2e_test.go`, which this comment cited until that
 * file was deleted — leaving the marker with no caller in any counted e2e
 * layer at all.
 */
export const FAIL_TURN_MARKER = "e2e-fail-this-turn";

/** The scenario one prompt selects. Never throws; unknown text is prose. */
export function selectScenario(text: string): Scenario {
  const trimmed = text.trimStart();
  for (const candidate of NAMED) {
    const token = `!${candidate.token}`;
    if (!trimmed.startsWith(token)) continue;
    const next = trimmed.charAt(token.length);
    if (next === "" || /\s/.test(next)) {
      LOGGER.logVerbose(
        { scenario: candidate.scenario.name, matched_token: candidate.token },
        "fake registry selected a named scenario",
      );
      return candidate.scenario;
    }
  }
  // THE SHIM'S OWN RESUME PROMPT is answered the way the vendor's main agent
  // answers it: a `SendMessage` per agent it names (engine/network-resume.ts).
  if (isNetworkResumePrompt(text)) {
    LOGGER.logVerbose({ scenario: NETWORK_RESUME.name }, "fake registry matched the shim's network-resume prompt");
    return NETWORK_RESUME;
  }
  if (text.includes(FAIL_TURN_MARKER)) {
    LOGGER.logVerbose({ scenario: FAIL_MARKER.name }, "fake registry matched the e2e failure marker");
    return FAIL_MARKER;
  }
  LOGGER.logVerbose({ scenario: "prose" }, "fake registry fell through to the plain-prose scenario");
  return PROSE;
}
