#!/usr/bin/env node
/**
 * THE CAPTURE HARNESS — the one sanctioned exception to the no-real-calls rule.
 *
 * Deferred vetting item 5 (`docs/overhaul/shim.md`) assigns this script here:
 * the golden transcripts the converter suites are graded against must be REAL
 * captures from the actual agent binary, taken in a one-time supervised run
 * the PROJECT LEAD dispatches. The mocked vendor's scenario scripts are then
 * rebuilt FROM these captures, so the fake cannot agree with the converter by
 * construction — a mistake in our reading of the vendor shows up as a failing
 * golden, not as two wrong halves that match.
 *
 * THIS SCRIPT IS NOT RUN THIS WAVE. Until the lead dispatches the capture, the
 * repository's goldens are the existing `testdata/corpus` fixtures plus the
 * pinned SDK's declared types, and `test/sdk-canary.test.ts` guards the pin.
 *
 * THE REFUSAL IS THE POINT. Running it is a deliberate, two-key act: the
 * ordinary test-mode variable AGENT_REPL_FORBID_VENDOR_CALLS must be UNSET and
 * the operator must pass `--i-am-the-project-lead-capture-run`. Anything else
 * exits 2 with a loud explanation and never loads the SDK. The refusal is
 * unit-tested by spawning this file (`capture-refusal.test.mjs`), which is why
 * the SDK import is dynamic and lives past the gate rather than at the top.
 *
 * WHAT IT PRODUCES, per scenario:
 *   captures/<scenario>/stream.jsonl   every SDK message and every control
 *                                      exchange, one JSONL line each:
 *                                      {t_ms, dir: "sdk"|"control", msg}
 *   captures/<scenario>/files/         the scratch CLAUDE_CONFIG_DIR's
 *                                      projects/ tree and the spool root, as
 *                                      the run left them
 *   captures/<scenario>/meta.json      what was run, and what refused to run
 *
 * Everything written is passed through `anonymize.mjs` — the walker
 * `testdata/corpus/MANIFEST.md` specifies — before it lands.
 *
 * Node built-ins only, no build step: this file is run directly with `node`.
 */

import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  realpathSync,
  readFileSync,
  readdirSync,
  statSync,
  writeFileSync,
  appendFileSync,
  renameSync,
  rmSync,
} from "node:fs";
import { homedir, tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  anonymize,
  anonymizeJsonl,
  anonymizePlainText,
} from "./anonymize.mjs";
import { apiKeySource, classifyCapture, verdictLine } from "./outcome.mjs";
import { isPromptDriven, planWorlds, promptTurnsOf, worldOf } from "./worlds.mjs";
import {
  AUTH_CONFIG_ROOT,
  AuthRefusedError,
  prepareAccountRoot,
  preflightAuth,
  reclaimScratchProject,
  releaseScratchProject,
  resolveAuthMode,
  accountRootEnvFor,
  sdkEnvFor,
  wipeSeededCredentials,
} from "./auth.mjs";

const HERE = path.dirname(fileURLToPath(import.meta.url));

/** The environment variable that forbids real vendor calls (src/vendor-guard.ts). */
export const FORBID_VENDOR_CALLS_ENV = "AGENT_REPL_FORBID_VENDOR_CALLS";

/** The deliberately unmistakable flag the project lead must pass. */
export const CAPTURE_FLAG = "--i-am-the-project-lead-capture-run";

/** Exit code for a refused run. Distinct from 1 so the test can be exact. */
export const EXIT_REFUSED = 2;

/** Exit code for a run that was authorized but failed. */
export const EXIT_FAILED = 1;

/**
 * Exit code when the run completed but one or more scenarios were QUARANTINED.
 *
 * Distinct from EXIT_FAILED so an operator (or a CI wrapper) can tell "the
 * harness broke" from "the harness worked and the vendor gave us nothing
 * usable" — the second is the case the first real run hit.
 */
export const EXIT_POISONED = 3;

/** Where a capture lands while it is still being written. */
export const INFLIGHT_DIR = "_inflight";

/**
 * Where a capture lands when it did not earn the right to be a golden.
 *
 * The evidence is KEPT, not deleted: the operator needs to see the "Not logged
 * in" transcript to know what to fix. It just must not sit where the converter
 * suites will pick it up as truth.
 */
export const FAILED_DIR = "_failed";

/**
 * Thrown by {@link assertCaptureAuthorized}. A named class so the spawn test
 * can assert the gate fired rather than pattern-matching prose.
 */
export class CaptureRefusedError extends Error {
  constructor(reason) {
    super(
      `capture.mjs REFUSED: ${reason}\n` +
        `This script makes REAL Claude API calls against the user's account. It runs\n` +
        `only in the project lead's one-time supervised capture, and only when BOTH\n` +
        `hold:\n` +
        `  1. ${FORBID_VENDOR_CALLS_ENV} is unset or empty (it is set by every test\n` +
        `     harness in this repo, and by the shim agents' shells, on purpose), and\n` +
        `  2. ${CAPTURE_FLAG} is passed on the command line.\n` +
        `Use --list or --check to inspect the prompt corpus without calling anything.`,
    );
    this.name = "CaptureRefusedError";
  }
}

/**
 * The two-key gate. Throws {@link CaptureRefusedError} when either key is
 * missing; returns nothing when the run is authorized.
 *
 * Takes `env` and `argv` rather than reading globals so the unit test can
 * exercise every arm in-process as well as by spawning.
 */
export function assertCaptureAuthorized(env, argv) {
  const forbid = env[FORBID_VENDOR_CALLS_ENV];
  if (forbid !== undefined && forbid !== "") {
    throw new CaptureRefusedError(
      `${FORBID_VENDOR_CALLS_ENV}=${JSON.stringify(forbid)} is set`,
    );
  }
  if (!argv.includes(CAPTURE_FLAG)) {
    throw new CaptureRefusedError(`${CAPTURE_FLAG} was not passed`);
  }
}

/** Parse the command line. Pure; the unit test drives it directly. */
export function parseArgv(argv) {
  const opts = {
    list: false,
    check: false,
    authorized: false,
    only: [],
    outDir: path.join(HERE, "captures"),
    promptsPath: path.join(HERE, "prompts.json"),
    includeManual: false,
    configRoot: null,
    seedCredentials: false,
    credentialsFrom: null,
  };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--list") opts.list = true;
    else if (arg === "--check") opts.check = true;
    else if (arg === CAPTURE_FLAG) opts.authorized = true;
    else if (arg === "--include-manual") opts.includeManual = true;
    else if (arg === "--seed-credentials") opts.seedCredentials = true;
    else if (arg === "--config-root") opts.configRoot = path.resolve(String(argv[++i]));
    else if (arg.startsWith("--config-root=")) {
      opts.configRoot = path.resolve(arg.slice("--config-root=".length));
    } else if (arg === "--credentials-from") {
      opts.credentialsFrom = path.resolve(String(argv[++i]));
    } else if (arg.startsWith("--credentials-from=")) {
      opts.credentialsFrom = path.resolve(arg.slice("--credentials-from=".length));
    }
    else if (arg === "--only") opts.only.push(...String(argv[++i]).split(","));
    else if (arg.startsWith("--only=")) {
      opts.only.push(...arg.slice("--only=".length).split(","));
    } else if (arg === "--out") opts.outDir = path.resolve(String(argv[++i]));
    else if (arg.startsWith("--out=")) {
      opts.outDir = path.resolve(arg.slice("--out=".length));
    } else if (arg === "--prompts") {
      opts.promptsPath = path.resolve(String(argv[++i]));
    } else if (arg.startsWith("--prompts=")) {
      opts.promptsPath = path.resolve(arg.slice("--prompts=".length));
    } else {
      throw new Error(`capture.mjs: unrecognized argument ${JSON.stringify(arg)}`);
    }
  }
  return opts;
}

/** Load and shallow-validate the prompt corpus. */
export function loadPrompts(promptsPath) {
  const doc = JSON.parse(readFileSync(promptsPath, "utf8"));
  if (!Array.isArray(doc.scenarios)) {
    throw new Error(`${promptsPath}: expected a top-level "scenarios" array`);
  }
  const seen = new Set();
  for (const scenario of doc.scenarios) {
    if (typeof scenario.name !== "string" || scenario.name === "") {
      throw new Error(`${promptsPath}: a scenario has no name`);
    }
    if (seen.has(scenario.name)) {
      throw new Error(`${promptsPath}: duplicate scenario name ${scenario.name}`);
    }
    seen.add(scenario.name);
    // Throws on a malformed config_root or prompts entry, so a typo is caught
    // by `--check` rather than by an empty capture hours later.
    worldOf(scenario);
    const hasPrompt = promptTurnsOf(scenario).length > 0;
    const hasManual = typeof scenario.manual === "string" && scenario.manual !== "";
    if (!hasPrompt && !hasManual) {
      throw new Error(
        `${promptsPath}: scenario ${scenario.name} has neither a prompt nor a manual procedure`,
      );
    }
    if (!Array.isArray(scenario.expect) || scenario.expect.length === 0) {
      throw new Error(`${promptsPath}: scenario ${scenario.name} declares no expectations`);
    }
  }
  return doc;
}

/**
 * The vendor's project-directory slug for a cwd: every `/` and `.` replaced by
 * `-` (observed: `/Users/x/.config/y` -> `-Users-x--config-y`).
 */
export function cwdSlug(cwd) {
  return cwd.replace(/[/._]/g, "-");
}

/** A pushable async iterable — the SDK's streaming-input prompt channel. */
export function createInputChannel() {
  const queued = [];
  const waiters = [];
  let done = false;
  return {
    push(message) {
      if (done) throw new Error("capture.mjs: pushed onto a closed input channel");
      const waiter = waiters.shift();
      if (waiter) waiter({ value: message, done: false });
      else queued.push(message);
    },
    close() {
      done = true;
      while (waiters.length > 0) waiters.shift()({ value: undefined, done: true });
    },
    [Symbol.asyncIterator]() {
      return {
        next() {
          if (queued.length > 0) {
            return Promise.resolve({ value: queued.shift(), done: false });
          }
          if (done) return Promise.resolve({ value: undefined, done: true });
          return new Promise((resolve) => waiters.push(resolve));
        },
      };
    },
  };
}

/**
 * Match a name against a scenario's pattern spelling.
 *
 * A plain string is an exact match; a `/pattern/` or `/pattern/flags` string is
 * a regular expression. The flags half is not decoration — the corpus keys
 * questions off case-insensitive fragments of the question text, since the
 * model chooses the exact wording and the script cannot.
 */
export function patternMatches(pattern, candidate) {
  const text = String(pattern ?? "");
  const asRegex = /^\/(.*)\/([a-z]*)$/s.exec(text);
  if (asRegex === null) return text === candidate;
  return new RegExp(asRegex[1], asRegex[2]).test(candidate);
}

/**
 * Resolve a scripted permission decision for one gate call.
 *
 * `script` is the scenario's `permission_script`. Rules are tried in order and
 * the first whose `tool` matches (exact name, or a `/regex/` spelling) wins;
 * `default` catches the rest. The decisions are the four the contract names
 * plus `park`, which never resolves — the undecidable case, which the shim's
 * teardown must resolve as denied.
 */
export function resolvePermissionDecision(script, toolName) {
  const rules = Array.isArray(script?.rules) ? script.rules : [];
  for (const rule of rules) {
    if (patternMatches(rule.tool, toolName)) return rule;
  }
  return { tool: toolName, decision: script?.default ?? "allow_once" };
}

/**
 * Build the SDK `PermissionResult` a scripted decision means.
 *
 * `allow_standing` returns the vendor's OWN offered suggestions as
 * `updatedPermissions` — that round-trip is exactly what the shim's standing
 * echo token has to reproduce, so the capture must exercise it rather than
 * inventing rules of its own.
 */
export function permissionResultFor(rule, input, options) {
  switch (rule.decision) {
    case "allow_once":
      return { behavior: "allow", updatedInput: input };
    case "allow_standing":
      return {
        behavior: "allow",
        updatedInput: input,
        updatedPermissions: options?.suggestions ?? [],
      };
    case "deny":
      return {
        behavior: "deny",
        message: rule.message ?? "denied by the capture script",
        ...(rule.interrupt === true ? { interrupt: true } : {}),
      };
    case "park":
      return null; // never resolves; the caller parks the promise
    default:
      throw new Error(`capture.mjs: unknown permission decision ${JSON.stringify(rule.decision)}`);
  }
}

/**
 * Build the AskUserQuestion answer payload for a scripted question script.
 *
 * The vendor keys answers by question TEXT and comma-joins a multi-select (see
 * `testdata/corpus/tool-results/ask_user_question.jsonl`). The capture answers
 * in exactly that spelling; a scenario that scripts NO answer for a question
 * leaves it out, which is how the "unanswered" arm is provoked.
 */
export function answersFor(script, questionsInput) {
  const scripted = Array.isArray(script?.answers) ? script.answers : [];
  const answers = {};
  for (const question of questionsInput?.questions ?? []) {
    const entry = scripted.find(
      (candidate) =>
        patternMatches(candidate.question, question.question) ||
        patternMatches(candidate.question, question.header),
    );
    if (entry === undefined) continue;
    const labels = Array.isArray(entry.select) ? entry.select.slice() : [];
    if (typeof entry.free_text === "string" && entry.free_text !== "") {
      labels.push(entry.free_text);
    }
    if (labels.length === 0) continue;
    answers[question.question] = labels.join(",");
  }
  return answers;
}

/** Whether an SDK message satisfies a control's `after` matcher. */
export function messageMatches(matcher, msg) {
  if (matcher === undefined) return false;
  if (matcher.type !== undefined && msg.type !== matcher.type) return false;
  if (matcher.subtype !== undefined && msg.subtype !== matcher.subtype) return false;
  if (matcher.contains !== undefined) {
    if (!JSON.stringify(msg ?? null).includes(matcher.contains)) return false;
  }
  return true;
}

/**
 * Pull messages off ONE query's iterator until that turn's result.
 *
 * WHY AN EXPLICIT ITERATOR AND NOT `for await (const msg of query)`: breaking
 * out of a for-await loop calls the iterator's `return()`, which ENDS the
 * async generator the SDK query is. On a multi-turn scenario that silently
 * destroyed every turn after the first — the loop broke on turn 1's result,
 * the query was finished, and turn 2's `for await` completed immediately
 * without yielding anything. The recorded evidence is exactly that: in
 * `compaction-directed` and `identity-rotation-clear` the stream holds one
 * result, then `turn_submitted` lines for later turns with no SDK message
 * after them, no second `system:init`, no `compact_boundary`, no
 * `conversation_reset` — and every later control failed with
 * "ProcessTransport is not ready for writing", because the transport had been
 * closed by that first `break`.
 *
 * Holding the iterator across turns is the fix: it is pulled with `next()` and
 * never returned, so the query stays open until the scenario closes it.
 *
 * Returns the result message, or `null` if the query ended without one.
 */
export async function drainToResult(iterator, onMessage) {
  for (;;) {
    const { value, done } = await iterator.next();
    if (done === true) return null;
    await onMessage(value);
    if (value?.type === "result") return value;
  }
}

/**
 * Whether a CONTROL record satisfies a control's `after` matcher.
 *
 * A parked permission gate is recorded as a control entry (`can_use_tool_parked`),
 * never as a stream message, so an `on_message` trigger can never observe it and
 * the scenario waits forever. `at: "on_control"` matches these records instead:
 * `kind` and `tool_name` select the record, `contains` is a substring of it.
 */
export function controlMatches(matcher, entry) {
  if (matcher === undefined) return false;
  if (matcher.kind !== undefined && entry?.kind !== matcher.kind) return false;
  if (matcher.tool_name !== undefined && entry?.tool_name !== matcher.tool_name) return false;
  if (matcher.contains !== undefined) {
    if (!JSON.stringify(entry ?? null).includes(matcher.contains)) return false;
  }
  return true;
}

/**
 * Fire every not-yet-fired control whose trigger point is `at` and whose
 * matcher accepts `payload`.
 *
 * ONE dispatcher for both trigger kinds, so the two can never drift: an
 * `on_message` control is matched against a stream message and an `on_control`
 * control against a control record, and neither ever sees the other's payload.
 */
export async function fireTriggers(query, controls, at, payload, fired, record) {
  const results = [];
  for (const control of controls ?? []) {
    if (control.at !== at) continue;
    const key = JSON.stringify([control.at, control.do, control.after ?? null]);
    if (fired.has(key)) continue;
    const matched = at === "on_control"
      ? controlMatches(control.after, payload)
      : messageMatches(control.after, payload);
    if (!matched) continue;
    fired.add(key);
    results.push(await driveControl(query, control, record));
  }
  return results;
}

/**
 * Drive one scripted control verb against the live query.
 *
 * Every verb the shim relies on is reachable from here, because the capture is
 * also the only place their real answers are ever observed: the canary proves
 * they EXIST in the declarations, the capture proves what they RETURN.
 */
export async function runControl(query, control) {
  const args = control.args ?? {};
  switch (control.do) {
    case "interrupt":
      return { verb: "interrupt", value: await query.interrupt() };
    case "stopTask":
      return { verb: "stopTask", value: await query.stopTask(args.task_id) };
    case "setModel":
      return { verb: "setModel", value: await query.setModel(args.model) };
    case "setPermissionMode":
      return {
        verb: "setPermissionMode",
        value: await query.setPermissionMode(args.mode),
      };
    case "getContextUsage":
      return { verb: "getContextUsage", value: await query.getContextUsage() };
    case "usage_EXPERIMENTAL":
      return {
        verb: "usage_EXPERIMENTAL",
        value: await query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET(),
      };
    case "accountInfo":
      return { verb: "accountInfo", value: await query.accountInfo() };
    case "mcpServerStatus":
      return { verb: "mcpServerStatus", value: await query.mcpServerStatus() };
    case "supportedModels":
      return { verb: "supportedModels", value: await query.supportedModels() };
    case "supportedCommands":
      return { verb: "supportedCommands", value: await query.supportedCommands() };
    case "supportedAgents":
      return { verb: "supportedAgents", value: await query.supportedAgents() };
    case "initializationResult":
      return {
        verb: "initializationResult",
        value: await query.initializationResult(),
      };
    case "backgroundTasks":
      return {
        verb: "backgroundTasks",
        value: await query.backgroundTasks(args.tool_use_id),
      };
    default:
      throw new Error(`capture.mjs: unknown control verb ${JSON.stringify(control.do)}`);
  }
}

/** Materialize a scenario's `cwd_setup` entries under the scratch cwd. */
export function materializeCwd(cwd, setup) {
  for (const entry of setup ?? []) {
    const target = path.join(cwd, entry.path);
    if (entry.dir === true) {
      mkdirSync(target, { recursive: true });
      continue;
    }
    mkdirSync(path.dirname(target), { recursive: true });
    writeFileSync(target, entry.content ?? "", "utf8");
    if (typeof entry.mode === "string") {
      spawnSync("chmod", [entry.mode, target]);
    }
  }
}

/**
 * The placeholder a corpus entry uses for this directory's absolute path.
 *
 * `prompts.json` is static and cannot know where the checkout lives, but
 * `options.mcpServers` has to name `mcp-echo.mjs` by absolute path. One
 * substitution token beats teaching the corpus about the filesystem.
 */
export const CAPTURE_DIR_TOKEN = "{{CAPTURE_DIR}}";

/** Substitute {@link CAPTURE_DIR_TOKEN} throughout a scenario's options. */
export function resolveTokens(value, captureDir = HERE) {
  if (typeof value === "string") return value.split(CAPTURE_DIR_TOKEN).join(captureDir);
  if (Array.isArray(value)) return value.map((item) => resolveTokens(item, captureDir));
  if (value !== null && typeof value === "object") {
    return Object.fromEntries(
      Object.entries(value).map(([key, child]) => [key, resolveTokens(child, captureDir)]),
    );
  }
  return value;
}

/**
 * Run a scenario's `cwd_init` shell command in the scratch cwd.
 *
 * WHY IT EXISTS: `worktree-enter-exit-kept-and-removed` shipped a
 * `.capture-init.sh` that `materializeCwd` faithfully wrote to disk and NOTHING
 * EVER RAN. The worktree tools need a real git repository, so the scenario
 * would have failed on an uninitialized directory while looking correctly
 * configured. A setup file nobody executes is worse than no setup file: it
 * reads as done.
 *
 * A non-zero exit is FATAL to the scenario rather than a warning — a scenario
 * whose precondition failed captures a golden of the wrong situation.
 */
export function runCwdInit(cwd, command) {
  if (typeof command !== "string" || command === "") return null;
  const run = spawnSync("bash", ["-lc", command], {
    cwd,
    encoding: "utf8",
    env: { ...process.env, GIT_CONFIG_GLOBAL: "/dev/null", GIT_CONFIG_SYSTEM: "/dev/null" },
  });
  if (run.status !== 0) {
    throw new Error(
      `capture: cwd_init failed (exit ${run.status}) in ${cwd}: ${command}\n` +
        `stdout: ${run.stdout}\nstderr: ${run.stderr}`,
    );
  }
  return { command, stdout: run.stdout, stderr: run.stderr };
}

/**
 * Copy a tree into the capture, anonymizing every file on the way.
 *
 * `.jsonl` and `.json` files go through the JSON walker; everything else
 * (spools, hook stderr, plan files) through the plain-text credential pass.
 * Nothing is copied verbatim: a capture directory must be safe to commit.
 */
export function copyTreeAnonymized(sourceDir, destDir, report) {
  if (!existsSync(sourceDir)) return;
  mkdirSync(destDir, { recursive: true });
  for (const name of readdirSync(sourceDir)) {
    const from = path.join(sourceDir, name);
    const to = path.join(destDir, name);
    const info = statSync(from);
    if (info.isDirectory()) {
      copyTreeAnonymized(from, to, report);
      continue;
    }
    if (!info.isFile()) continue;
    const raw = readFileSync(from, "utf8");
    if (name.endsWith(".jsonl")) {
      writeFileSync(
        to,
        anonymizeJsonl(raw, (lineNo, err) =>
          report.unparsed.push({ file: from, line: lineNo, error: String(err) }),
        ),
        "utf8",
      );
    } else if (name.endsWith(".json")) {
      writeFileSync(to, `${JSON.stringify(anonymize(JSON.parse(raw)), null, 2)}\n`, "utf8");
    } else {
      writeFileSync(to, anonymizePlainText(raw), "utf8");
    }
  }
}

/**
 * Merge a source tree into an existing capture tree, anonymizing on the way.
 *
 * Used only by the SWEEP-END LATE RECLAIM. It differs from
 * `copyTreeAnonymized` in one rule: a destination file is never overwritten by
 * a SMALLER one. The vendor's late flush rewrites a transcript it had already
 * written, and a truncated re-write landing on top of the full capture would
 * silently destroy the golden.
 */
export function mergeTreeAnonymized(sourceDir, destDir, report, moved = []) {
  if (!existsSync(sourceDir)) return moved;
  mkdirSync(destDir, { recursive: true });
  for (const name of readdirSync(sourceDir)) {
    const from = path.join(sourceDir, name);
    const to = path.join(destDir, name);
    const info = statSync(from);
    if (info.isDirectory()) {
      mergeTreeAnonymized(from, to, report, moved);
      continue;
    }
    if (!info.isFile()) continue;
    const raw = readFileSync(from, "utf8");
    let text;
    if (name.endsWith(".jsonl")) {
      text = anonymizeJsonl(raw, (lineNo, err) =>
        report.unparsed.push({ file: from, line: lineNo, error: String(err) }),
      );
    } else if (name.endsWith(".json")) {
      text = `${JSON.stringify(anonymize(JSON.parse(raw)), null, 2)}\n`;
    } else {
      text = anonymizePlainText(raw);
    }
    if (existsSync(to) && statSync(to).size >= Buffer.byteLength(text, "utf8")) continue;
    writeFileSync(to, text, "utf8");
    moved.push(to);
  }
  return moved;
}

/**
 * The SECOND reclaim pass, run once at sweep end.
 *
 * The vendor re-writes small late transcript flushes into
 * `<config-root>/projects/<slug>/` AFTER the per-scenario reclaim has already
 * emptied it — the SDK child is still alive then. Anything that reappeared is
 * merged into the scenario's capture and the slug directory is deleted from the
 * operator's root, so the account really is left as found. ONLY the slugs this
 * run created are ever looked at; no other project directory is read or removed.
 */
export function lateReclaimSlug({ accountRoot, slug, captureDir, report, log }) {
  const projectDir = path.join(accountRoot, "projects", slug);
  if (!existsSync(projectDir)) return { slug, moved: [] };
  const moved = mergeTreeAnonymized(
    projectDir,
    path.join(captureDir, "files", "projects", slug),
    report,
  );
  rmSync(projectDir, { recursive: true, force: true });
  log?.(
    `capture.mjs: late reclaim ${slug} — ${moved.length} file(s) re-copied, ` +
      `${projectDir} removed\n`,
  );
  return { slug, moved };
}

/** Run the sweep-end late reclaim for every scenario slug this run created. */
export function lateReclaimAll(auth, reclaims, log) {
  if (auth.mode !== AUTH_CONFIG_ROOT) return [];
  return reclaims.map((entry) => lateReclaimSlug({ ...entry, log }));
}

/**
 * Create the scratch world a group of scenarios shares.
 *
 * The CWD IS ALWAYS SCRATCH, in every authentication mode — a capture must
 * never run against the operator's real working tree. Only the account root
 * depends on the mechanism, and under `--config-root` it is the operator's real
 * one, which is why each scenario's project directory is reclaimed and deleted
 * afterwards.
 */
export function createWorld(auth, label) {
  // realpath, not the mkdtemp spelling: the vendor slugs the project directory
  // from the cwd's RESOLVED path (macOS `/var` is `/private/var`), and the
  // reclaim step must compute the same slug or it misses the transcripts.
  const scratch = realpathSync(mkdtempSync(path.join(tmpdir(), `agent-repl-capture-${label}-`)));
  const cwd = path.join(scratch, "cwd");
  const scratchConfigDir = path.join(scratch, "config");
  const spoolRoot = path.join(scratch, "spool");
  mkdirSync(cwd, { recursive: true });
  mkdirSync(spoolRoot, { recursive: true });
  return {
    world: label,
    scratch,
    cwd,
    scratchConfigDir,
    spoolRoot,
    configDir: prepareAccountRoot(auth, scratchConfigDir),
  };
}

/**
 * Run ONE scenario end to end. Only reached past the authorization gate.
 *
 * The options are the SHIM'S PRODUCTION OPTIONS, deliberately: a capture taken
 * under different options is not a golden for the shim, it is a golden for
 * something nobody ships. The `claude_code` preset carries the environment
 * block the model needs to resolve `~`; `settingSources` user+project+local is
 * what makes the vendor emit the `denied.by_policy` messages the permission
 * gate relies on; `includePartialMessages` is the whole streamed-prose plane;
 * and without `forwardSubagentText` a subagent's prose never arrives at all.
 */
async function runScenario(sdk, scenario, opts, auth, world) {
  // STAGED, NEVER WRITTEN IN PLACE: a capture becomes a golden only after it
  // is classified, so a poisoned run can never occupy captures/<name>/ even
  // for an instant (a run interrupted mid-scenario leaves _inflight/, which is
  // obviously not a fixture).
  const outDir = path.join(opts.outDir, INFLIGHT_DIR, scenario.name);
  rmSync(outDir, { recursive: true, force: true });
  mkdirSync(outDir, { recursive: true });
  const streamPath = path.join(outDir, "stream.jsonl");
  writeFileSync(streamPath, "", "utf8");

  const report = { unparsed: [], controls: [], errors: [] };
  const entries = [];
  // The WORLD supplies the cwd and account root. An isolated scenario gets a
  // world of its own; scenarios sharing a `config_root` share one, so a later
  // scenario sees what the earlier ones did.
  const { cwd, scratchConfigDir, configDir, spoolRoot } = world;
  materializeCwd(cwd, scenario.cwd_setup);
  const cwdInit = runCwdInit(cwd, scenario.cwd_init);

  const t0 = Date.now();
  const record = (dir, msg) => {
    const entry = { t_ms: Date.now() - t0, dir, msg: anonymize(msg) };
    entries.push(entry);
    appendFileSync(streamPath, `${JSON.stringify(entry)}\n`, "utf8");
  };

  // The scratch config root is the account root for THIS scenario only, so
  // every vendor file the run writes lands inside the capture rather than in
  // the operator's real ~/.claude.
  const previousConfigDir = process.env.CLAUDE_CONFIG_DIR;
  const rootEnv = accountRootEnvFor(auth, configDir, process.env);
  if ("CLAUDE_CONFIG_DIR" in rootEnv) process.env.CLAUDE_CONFIG_DIR = rootEnv.CLAUDE_CONFIG_DIR;
  else delete process.env.CLAUDE_CONFIG_DIR;

  const abort = new AbortController();
  const input = createInputChannel();
  const parked = [];
  // Controls fire at most once per scenario. Tracked in a LOCAL set rather than
  // by stamping the control object: the corpus is loaded once per process, so a
  // flag written onto it would leak across runs (and across tests).
  const firedControls = new Set();
  let query;
  // The vendor's own session id, learned from system:init and needed by a
  // `resume` turn.
  let vendorSessionId = null;

  // A control record the VENDOR provoked (not one the capture drove itself),
  // recorded and then offered to the scenario's `on_control` triggers.
  const recordVendorControl = async (payload) => {
    record("control", payload);
    report.controls.push(
      ...(await fireTriggers(query, scenario.controls, "on_control", payload, firedControls, record)),
    );
  };

  const canUseTool = async (toolName, toolInput, options) => {
    await recordVendorControl({ kind: "can_use_tool_request", tool_name: toolName, input: toolInput, options });
    if (toolName === "AskUserQuestion") {
      const answers = answersFor(scenario.question_script, toolInput);
      const result = Object.keys(answers).length === 0
        ? { behavior: "deny", message: "left unanswered by the capture script" }
        : { behavior: "allow", updatedInput: { ...toolInput, answers } };
      await recordVendorControl({ kind: "can_use_tool_response", tool_name: toolName, result });
      return result;
    }
    const rule = resolvePermissionDecision(scenario.permission_script, toolName);
    const result = permissionResultFor(rule, toolInput, options);
    if (result === null) {
      // The undecidable arm: never resolved here. The scenario's `on_control`
      // trigger fires off THIS record and aborts the turn, and the settle below
      // denies every parked callback so the vendor process is not left wedged.
      await recordVendorControl({ kind: "can_use_tool_parked", tool_name: toolName, rule });
      return new Promise((resolve) => parked.push(resolve));
    }
    await recordVendorControl({ kind: "can_use_tool_response", tool_name: toolName, result });
    return result;
  };

  const options = {
    cwd,
    abortController: abort,
    canUseTool,
    includePartialMessages: true,
    forwardSubagentText: true,
    settingSources: ["user", "project", "local"],
    persistSession: true,
    systemPrompt: { type: "preset", preset: "claude_code" },
    env: {
      ...accountRootEnvFor(auth, configDir, process.env),
      AGENT_REPL_OWNED: "1",
      ...sdkEnvFor(auth, process.env),
    },
    ...resolveTokens(scenario.options ?? {}),
  };
  if (typeof scenario.model === "string") options.model = scenario.model;
  if (typeof scenario.permission_mode === "string") {
    options.permissionMode = scenario.permission_mode;
  }

  // The metaprompt append is production behavior; a capture without it is a
  // capture of a different system prompt. Absence of the checkout is normal
  // and silent, exactly as src/metaprompt.ts rules.
  const metapromptPath = path.join(
    homedir(),
    ".config/doom/modules/app/agent-repl/metaprompt.md",
  );
  if (existsSync(metapromptPath)) {
    const append = readFileSync(metapromptPath, "utf8").trim();
    if (append !== "") options.systemPrompt = { ...options.systemPrompt, append };
  }

  const turns = promptTurnsOf(scenario);

  // The query's iterator, held across turns. NEVER re-derived per turn and
  // never `return()`ed by a for-await break: see drainToResult.
  let iterator = null;

  const openQuery = (extraOptions) => {
    query = sdk.query({ prompt: input, options: { ...options, ...extraOptions } });
    iterator = query[Symbol.asyncIterator]();
    record("control", {
      kind: "query_started",
      options: {
        ...options,
        ...extraOptions,
        canUseTool: "[function]",
        abortController: "[AbortController]",
        env: "[inherited]",
      },
    });
    return query;
  };

  try {
    openQuery({});

    for (const control of scenario.controls ?? []) {
      if (control.at !== "session_start") continue;
      report.controls.push(await driveControl(query, control, record));
    }

    for (let turnIndex = 0; turnIndex < turns.length; turnIndex += 1) {
      const turn = turns[turnIndex];

      if (turn.resume) {
        // A RESUME IS A NEW PROCESS. The point of the arm is to capture what a
        // resume actually costs (the cold read lands with the first prompt), so
        // the query must genuinely be torn down and reopened against the same
        // vendor session id — steering the existing one would capture nothing.
        if (vendorSessionId === null) {
          throw new Error(
            `capture: ${scenario.name} turn ${turnIndex} asks to resume, but no vendor ` +
              "session id has been observed yet (no system:init arrived)",
          );
        }
        try { query?.close(); } catch { /* already gone */ }
        record("control", { kind: "resuming", session_id: vendorSessionId, turn: turnIndex });
        openQuery({ resume: vendorSessionId });
      }

      record("control", { kind: "turn_submitted", turn: turnIndex, text: turn.text });
      input.push({
        type: "user",
        message: { role: "user", content: turn.text },
        parent_tool_use_id: null,
        session_id: "",
      });

      // WAIT FOR THIS TURN'S RESULT before the next turn is submitted. Without
      // it the loop races ahead and every later turn is submitted into a query
      // that is no longer reading — which is what the recorded t_ms ordering of
      // the multi-turn captures shows.
      const result = await drainToResult(iterator, async (msg) => {
        record("sdk", msg);
        if (msg?.type === "system" && msg?.subtype === "init" && typeof msg.session_id === "string") {
          vendorSessionId = msg.session_id;
        }
        report.controls.push(
          ...(await fireTriggers(query, scenario.controls, "on_message", msg, firedControls, record)),
        );
      });
      if (result === null) {
        // The query ended without a terminal. Submitting the remaining turns
        // into a dead query would record turn_submitted lines that never
        // happened, so the scenario stops here and the gate quarantines it for
        // the missing result.
        record("control", { kind: "query_ended_without_result", turn: turnIndex });
        break;
      }
    }

    for (const control of scenario.controls ?? []) {
      if (control.at !== "turn_end") continue;
      report.controls.push(await driveControl(query, control, record));
    }
  } catch (err) {
    report.errors.push({ stage: "query", error: String(err && err.stack ? err.stack : err) });
    record("control", { kind: "query_threw", error: String(err) });
  } finally {
    // PERMISSION-CALLBACK LIVENESS: resolve every parked callback as denied
    // before tearing down, or the vendor process wedges (shim.md, process-level
    // obligations). The capture obeys the same rule the shim does.
    for (const resolve of parked) {
      resolve({ behavior: "deny", message: "capture teardown: pending permission abandoned" });
    }
    try { input.close(); } catch { /* already closed */ }
    try { query?.close(); } catch { /* already gone */ }
    abort.abort();
    if (previousConfigDir === undefined) delete process.env.CLAUDE_CONFIG_DIR;
    else process.env.CLAUDE_CONFIG_DIR = previousConfigDir;
  }

  // The vendor's own files are half the capture: the transcript, the subagent
  // sidechains and their .meta.json, and the spool the sidecar tails. A stream
  // without them cannot exercise the sidecar or the compaction experiment.
  const filesDir = path.join(outDir, "files");
  // Under --config-root the operator's root holds every project they have ever
  // opened, so only THIS scenario's own slug is harvested — never the tree.
  const scratchProject = reclaimScratchProject(auth, configDir, cwdSlug(cwd));
  if (auth.mode === AUTH_CONFIG_ROOT) {
    if (scratchProject !== null) {
      copyTreeAnonymized(
        scratchProject,
        path.join(filesDir, "projects", cwdSlug(cwd)),
        report,
      );
    }
  } else {
    copyTreeAnonymized(path.join(configDir, "projects"), path.join(filesDir, "projects"), report);
  }
  copyTreeAnonymized(spoolRoot, path.join(filesDir, "spool"), report);
  const defaultSpool = path.join("/tmp", `claude-${process.getuid?.() ?? 0}`, cwdSlug(cwd));
  copyTreeAnonymized(defaultSpool, path.join(filesDir, "spool-default"), report);

  // LEAVE THE ACCOUNT AS FOUND. The harvest above already has the transcripts;
  // what the vendor wrote into the operator's real root is now removed.
  releaseScratchProject(scratchProject);
  // A seeded credential must never reach the capture directory.
  wipeSeededCredentials(auth, scratchConfigDir);

  // THE GATE. A capture is a golden the converter suites are graded against
  // and the mock is rebuilt from, so it must earn that standing rather than
  // inherit it from having finished.
  const outcome = classifyCapture(entries, scenario, report);

  writeFileSync(
    path.join(outDir, "meta.json"),
    `${JSON.stringify(
      {
        scenario: scenario.name,
        ok: outcome.ok,
        failure_reasons: outcome.reasons,
        prompt: scenario.prompt ?? null,
        prompts: turns,
        world: world.world,
        cwd_init: cwdInit,
        vendor_session_id: vendorSessionId,
        manual: scenario.manual ?? null,
        expect: scenario.expect,
        cwd_slug: cwdSlug(cwd),
        api_key_source: apiKeySource(entries),
        auth_mode: auth.mode,
        captured_at: new Date().toISOString(),
        controls: report.controls,
        unparsed_lines: report.unparsed,
        errors: report.errors,
      },
      null,
      2,
    )}\n`,
    "utf8",
  );

  const finalDir = outcome.ok
    ? path.join(opts.outDir, scenario.name)
    : path.join(opts.outDir, FAILED_DIR, scenario.name);
  rmSync(finalDir, { recursive: true, force: true });
  mkdirSync(path.dirname(finalDir), { recursive: true });
  renameSync(outDir, finalDir);

  process.stderr.write(`${verdictLine(scenario.name, outcome)}\n`);
  return { report, outcome, dir: finalDir, slug: cwdSlug(cwd), accountRoot: configDir };
}

/** Run one control and record both halves of the exchange. */
async function driveControl(query, control, record) {
  record("control", { kind: "control_request", verb: control.do, args: control.args ?? {} });
  try {
    const answer = await runControl(query, control);
    record("control", { kind: "control_response", ...answer });
    return { verb: control.do, ok: true };
  } catch (err) {
    record("control", { kind: "control_failed", verb: control.do, error: String(err) });
    return { verb: control.do, ok: false, error: String(err) };
  }
}

/** Entrypoint. */
async function main(argv, env) {
  const opts = parseArgv(argv);
  const doc = loadPrompts(opts.promptsPath);
  const selected = opts.only.length > 0
    ? doc.scenarios.filter((scenario) => opts.only.includes(scenario.name))
    : doc.scenarios;

  if (opts.list) {
    for (const scenario of doc.scenarios) {
      const turns = promptTurnsOf(scenario);
      const kind = turns.length === 0 ? "MANUAL" : turns.length === 1 ? "prompt" : `prompt x${turns.length}`;
      process.stdout.write(`${scenario.name}\t${kind}\t${scenario.covers ?? ""}\n`);
    }
    return 0;
  }
  if (opts.check) {
    process.stdout.write(
      `${doc.scenarios.length} scenarios, ` +
        `${doc.scenarios.filter(isPromptDriven).length} prompt-driven, ` +
        `${doc.scenarios.filter((s) => !isPromptDriven(s)).length} manual, ` +
        `${planWorlds(doc.scenarios).filter((g) => g.world !== null).length} shared world(s)\n`,
    );
    return 0;
  }

  assertCaptureAuthorized(env, argv);

  // BEFORE THE FIRST SCENARIO, ALWAYS. Discovering a credential problem after
  // a long run is how the first attempt was wasted.
  const auth = resolveAuthMode(opts, env);
  const preflight = preflightAuth(auth);
  process.stderr.write(
    `capture.mjs: authenticating via ${preflight.mode} (${preflight.source})\n`,
  );

  const sdk = await import("@anthropic-ai/claude-agent-sdk");
  mkdirSync(opts.outDir, { recursive: true });
  const skipped = [];
  const poisoned = [];
  // Every scenario slug this run created, for the sweep-end late reclaim.
  const reclaims = [];
  // Scenarios sharing a world run against ONE cwd and account root, in corpus
  // order, so a later one sees what the earlier ones did — that is how a
  // /clear has an identity to rotate and a /compact has a conversation.
  for (const group of planWorlds(selected)) {
    const runnable = group.scenarios.filter((scenario) => {
      if (isPromptDriven(scenario)) return true;
      if (opts.includeManual) {
        process.stderr.write(`capture.mjs: ${scenario.name} is MANUAL — ${scenario.manual}\n`);
      }
      skipped.push(scenario.name);
      return false;
    });
    if (runnable.length === 0) continue;

    const world = createWorld(auth, group.world ?? runnable[0].name);
    if (group.world !== null) {
      process.stderr.write(
        `capture.mjs: world ${group.world} — ${runnable.map((s) => s.name).join(" -> ")}\n`,
      );
    }
    for (const scenario of runnable) {
      process.stderr.write(`capture.mjs: capturing ${scenario.name}\n`);
      const run = await runScenario(sdk, scenario, opts, auth, world);
      reclaims.push({
        accountRoot: run.accountRoot,
        slug: run.slug,
        captureDir: run.dir,
        report: run.report,
      });
      if (!run.outcome.ok) poisoned.push({ scenario: scenario.name, reasons: run.outcome.reasons });
    }
  }

  // SWEEP END, after every query is closed and every SDK child has gone: the
  // vendor flushes late transcript writes into the operator's real root after
  // the per-scenario reclaim ran, and this pass is what keeps that root clean.
  lateReclaimAll(auth, reclaims, (line) => process.stderr.write(line));
  writeFileSync(
    path.join(opts.outDir, "SKIPPED.json"),
    `${JSON.stringify({ skipped, reason: "manual scenarios have no prompt-only provocation" }, null, 2)}\n`,
    "utf8",
  );

  if (poisoned.length > 0) {
    // LOUD, LAST, AND NON-ZERO. The first real run exited 0 with an
    // authentication failure sitting in captures/ as a fixture; the whole
    // point of this block is that that outcome is now impossible to miss.
    process.stderr.write(
      `\ncapture.mjs: ${poisoned.length} of ${selected.length} scenario(s) DID NOT CAPTURE A USABLE GOLDEN.\n` +
        `They are quarantined under ${path.join(opts.outDir, FAILED_DIR)}/ and are NOT fixtures.\n`,
    );
    for (const { scenario, reasons } of poisoned) {
      process.stderr.write(`  ${scenario}: ${reasons.join("; ")}\n`);
    }
    return EXIT_POISONED;
  }
  return 0;
}

// Only run when executed directly; importing this file (the unit tests do)
// must have no side effects at all.
if (process.argv[1] !== undefined && path.resolve(process.argv[1]) === path.resolve(fileURLToPath(import.meta.url))) {
  main(process.argv.slice(2), process.env).then(
    (code) => process.exit(code),
    (err) => {
      process.stderr.write(`${err instanceof Error ? err.message : String(err)}\n`);
      const refused = err instanceof CaptureRefusedError || err instanceof AuthRefusedError;
      process.exit(refused ? EXIT_REFUSED : EXIT_FAILED);
    },
  );
}
