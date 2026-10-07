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
import { homedir, tmpdir, userInfo } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  anonymize,
  anonymizeJsonl,
  anonymizePlainText,
  expandHome,
  scrubPersonal,
} from "./anonymize.mjs";
import { apiKeySource, classifyCapture, verdictLine } from "./outcome.mjs";
import { isPromptDriven, planWorlds, promptTurnsOf, resumeCaptureOf, worldOf } from "./worlds.mjs";
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
/**
 * ONE QUERY SESSION per scenario, with ONE AbortController PER OPEN.
 *
 * A resume is a new vendor process: the previous query is closed and a fresh
 * one opened against the same vendor session id. The controller must NOT be
 * shared across those opens. Closing the first query aborts the controller it
 * was given, so a controller reused for the resumed query hands the SDK an
 * already-fired signal and the resumed `query()` throws "Operation aborted"
 * moments after the resume lands — which is exactly what the `cold-resume`
 * scenario hit. Each `open()` therefore tears the previous query down, aborts
 * only THAT open's controller, and mints a new one for the new query.
 */
export function createQuerySession({ sdk, prompt, options }) {
  let controller = null;
  let query = null;
  let iterator = null;

  const teardown = () => {
    const deadQuery = query;
    const deadController = controller;
    query = null;
    iterator = null;
    controller = null;
    if (deadQuery !== null) {
      try { deadQuery.close(); } catch { /* already gone */ }
    }
    // Abort only the controller that belonged to the query just closed. A
    // later open's signal is a different object and cannot be reached here.
    if (deadController !== null) deadController.abort();
  };

  return {
    get query() { return query; },
    get iterator() { return iterator; },
    get controller() { return controller; },
    open(extraOptions = {}) {
      teardown();
      controller = new AbortController();
      const merged = { ...options, ...extraOptions, abortController: controller };
      query = sdk.query({ prompt, options: merged });
      iterator = query[Symbol.asyncIterator]();
      return { query, options: merged };
    },
    close() { teardown(); },
  };
}

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
 * A meta document with every personal value scrubbed, keys and strings alike,
 * and nothing else touched: the meta is the harness's own record, so the
 * corpus's truncation and redaction rules do not apply to it.
 */
export function anonymizeMeta(meta, personal) {
  requirePersonal(personal, "anonymizeMeta");
  return JSON.parse(scrubPersonal(JSON.stringify(meta), personal));
}

/** Split a comma-separated environment list, dropping blanks. */
export function envList(value) {
  return (value ?? "").split(",").map((item) => item.trim()).filter((item) => item !== "");
}

/**
 * The signed-in email a vendor identity file names, or `null` when the file is
 * absent or names none. A file that exists but does not parse is an error:
 * an account whose email cannot be read is an email the scrub would miss.
 */
export function accountEmailIn(file) {
  let raw;
  try {
    raw = readFileSync(file, "utf8");
  } catch (err) {
    if (err.code === "ENOENT") return null;
    throw err;
  }
  const email = JSON.parse(raw)?.oauthAccount?.emailAddress;
  return typeof email === "string" && email !== "" ? email : null;
}

/**
 * The personal values of whoever is running the capture, for the anonymizer's
 * scrub: the home directory; every account email signed in under the usual
 * account roots (and `configRoot`, when the run names one), plus
 * $CAPTURE_PERSONAL_EMAILS; and the login name, the home's last segment, every
 * word of the global git user name, plus $CAPTURE_PERSONAL_NAMES. Name words
 * under three letters are left out: they match too much ordinary text.
 */
export function personalValuesFromHost(
  env,
  {
    home = homedir(),
    username = userInfo().username,
    gitUserName = () =>
      spawnSync("git", ["config", "--global", "user.name"], { encoding: "utf8" }).stdout ?? "",
    configRoot = null,
  } = {},
) {
  const identityFiles = [
    path.join(home, ".claude.json"),
    path.join(home, ".claude", ".claude.json"),
    path.join(home, ".claude-chesscom", ".claude.json"),
  ];
  if (configRoot !== null) identityFiles.push(path.join(configRoot, ".claude.json"));
  const emails = new Set();
  for (const file of identityFiles) {
    const email = accountEmailIn(file);
    if (email !== null) emails.add(email);
  }
  for (const email of envList(env.CAPTURE_PERSONAL_EMAILS)) emails.add(email);
  const names = new Set([username, path.basename(home)]);
  for (const word of gitUserName().split(/\s+/)) names.add(word);
  for (const name of envList(env.CAPTURE_PERSONAL_NAMES)) names.add(name);
  return {
    home,
    emails: [...emails],
    names: [...names].filter((name) => name.length >= 3),
  };
}

/**
 * Fail at once when a capture write was handed no personal values. Every write
 * into a capture goes through the scrub, so a call site that forgot to thread
 * them is a defect, not a capture without personal values.
 */
function requirePersonal(personal, site) {
  if (personal === undefined || personal === null) {
    throw new Error(`capture: ${site} was called without the personal values to scrub`);
  }
  return personal;
}

/**
 * Copy a tree into the capture, anonymizing every file on the way.
 *
 * `.jsonl` and `.json` files go through the JSON walker; everything else
 * (spools, hook stderr, plan files) through the plain-text credential pass.
 * Nothing is copied verbatim: a capture directory must be safe to commit.
 */
export function copyTreeAnonymized(sourceDir, destDir, report, personal) {
  requirePersonal(personal, "copyTreeAnonymized");
  if (!existsSync(sourceDir)) return;
  mkdirSync(destDir, { recursive: true });
  for (const name of readdirSync(sourceDir)) {
    const from = path.join(sourceDir, name);
    const to = path.join(destDir, scrubPersonal(name, personal));
    const info = statSync(from);
    if (info.isDirectory()) {
      copyTreeAnonymized(from, to, report, personal);
      continue;
    }
    if (!info.isFile()) continue;
    writeFileSync(to, anonymizeFile(name, from, readFileSync(from, "utf8"), report, personal), "utf8");
  }
}

/**
 * One file's anonymized text, by kind: `.jsonl` and `.json` through the JSON
 * walker, everything else through the plain-text pass.
 */
function anonymizeFile(name, from, raw, report, personal) {
  if (name.endsWith(".jsonl")) {
    return anonymizeJsonl(
      raw,
      (lineNo, err) => report.unparsed.push({ file: from, line: lineNo, error: String(err) }),
      personal,
    );
  }
  if (name.endsWith(".json")) {
    return `${JSON.stringify(anonymize(JSON.parse(raw), undefined, personal), null, 2)}\n`;
  }
  return anonymizePlainText(raw, personal);
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
export function mergeTreeAnonymized(sourceDir, destDir, report, personal, moved = []) {
  requirePersonal(personal, "mergeTreeAnonymized");
  if (!existsSync(sourceDir)) return moved;
  mkdirSync(destDir, { recursive: true });
  for (const name of readdirSync(sourceDir)) {
    const from = path.join(sourceDir, name);
    const to = path.join(destDir, scrubPersonal(name, personal));
    const info = statSync(from);
    if (info.isDirectory()) {
      mergeTreeAnonymized(from, to, report, personal, moved);
      continue;
    }
    if (!info.isFile()) continue;
    const text = anonymizeFile(name, from, readFileSync(from, "utf8"), report, personal);
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
export function lateReclaimSlug({ accountRoot, slug, captureDir, report, personal, log }) {
  requirePersonal(personal, "lateReclaimSlug");
  const projectDir = path.join(accountRoot, "projects", slug);
  if (!existsSync(projectDir)) return { slug, moved: [] };
  const moved = mergeTreeAnonymized(
    projectDir,
    path.join(captureDir, "files", "projects", scrubPersonal(slug, personal)),
    report,
    personal,
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
 * THE ORDERING HOLE this module closes: `session.close()` (the SDK's
 * `Query#close`) aborts the query's `AbortController` and starts the
 * transport tearing the CLI child down; it does not wait for that child to
 * actually exit. `QueryLike` (src/sdk/types.ts) exposes no exit event and no
 * pid — "aborting it ends the CLI child" describes what STARTS the shutdown,
 * not when it FINISHES — so a reclaim run immediately after `close()` can
 * still be racing a live vendor process that goes on to flush one more
 * transcript write into the account root after the reclaim already ran. That
 * is exactly the seven `*agent-repl-capture-*` residue directories
 * `testdata/captures/MANIFEST.md` records: one late-flushed file each,
 * written after both the per-scenario reclaim AND the sweep-end
 * `lateReclaimAll` had already completed.
 *
 * There is no supported SDK API to await the real exit, so this uses the same
 * technique the MANIFEST.md investigation already used by hand: walk the
 * capture process's own descendant tree with `pgrep -P` and look for the
 * vendor's own bundled binary among the survivors (its path always carries
 * `claude-agent-sdk`, per the `query-death` scenario's note).
 */
export const VENDOR_CHILD_MARKER = "claude-agent-sdk";

/** Every live descendant of `pid`, found by walking `pgrep -P` breadth-first. */
export function descendantPids(pid) {
  const seen = new Set();
  const frontier = [pid];
  while (frontier.length > 0) {
    const current = frontier.shift();
    const run = spawnSync("pgrep", ["-P", String(current)], { encoding: "utf8" });
    if (run.status !== 0 || typeof run.stdout !== "string") continue;
    for (const line of run.stdout.split("\n")) {
      const trimmed = line.trim();
      if (trimmed === "") continue;
      const childPid = Number(trimmed);
      if (Number.isInteger(childPid) && !seen.has(childPid)) {
        seen.add(childPid);
        frontier.push(childPid);
      }
    }
  }
  return [...seen];
}

/** Descendants of `pid` whose command line names the vendor's own CLI binary. */
export function vendorChildPids(pid) {
  const descendants = descendantPids(pid);
  if (descendants.length === 0) return [];
  const run = spawnSync("ps", ["-o", "pid=,command=", "-p", descendants.join(",")], {
    encoding: "utf8",
  });
  if (run.status !== 0 || typeof run.stdout !== "string") return [];
  const survivors = [];
  for (const line of run.stdout.split("\n")) {
    const trimmed = line.trim();
    if (trimmed === "" || !trimmed.includes(VENDOR_CHILD_MARKER)) continue;
    const survivorPid = Number(trimmed.split(/\s+/)[0]);
    if (Number.isInteger(survivorPid)) survivors.push(survivorPid);
  }
  return survivors;
}

/**
 * Block until every vendor CLI descendant of `pid` has actually exited.
 *
 * BOUNDED AND LOUD, on purpose: a vendor process wedged mid-shutdown must fail
 * this run rather than let the reclaim quietly race it again. `listSurvivors`
 * and `sleep` are injected so the unit tests drive the retry loop directly
 * instead of spawning real processes.
 */
export async function waitForVendorChildExit({
  pid = process.pid,
  timeoutMs = 10_000,
  intervalMs = 100,
  listSurvivors = vendorChildPids,
  sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
} = {}) {
  const deadline = Date.now() + timeoutMs;
  for (;;) {
    const survivors = listSurvivors(pid);
    if (survivors.length === 0) return;
    if (Date.now() >= deadline) {
      throw new Error(
        `capture: vendor CLI child process(es) still alive ${timeoutMs}ms after query close: ` +
          `pid(s) ${survivors.join(", ")}. Refusing to reclaim against a live child — anything ` +
          "it still flushes would resurrect residue in the account root.",
      );
    }
    await sleep(intervalMs);
  }
}

/**
 * Any `*agent-repl-capture-*` directory still under `<accountRoot>/projects/`.
 *
 * The final verification behind the "LEAVE THE ACCOUNT AS FOUND" promise: even
 * with {@link waitForVendorChildExit} closing the ordering hole, this is what
 * turns a residual directory into a loud, non-zero failure instead of a silent
 * leak the next MANIFEST.md audit has to rediscover by hand.
 */
export function findCaptureResidue(accountRoot) {
  const projectsDir = path.join(accountRoot, "projects");
  if (!existsSync(projectsDir)) return [];
  return readdirSync(projectsDir)
    .filter((name) => name.includes("agent-repl-capture-"))
    .map((name) => path.join(projectsDir, name));
}

/**
 * Verify every distinct account root this run touched is clean, and throw —
 * loud, never silent — if any `agent-repl-capture-*` directory remains.
 */
export function assertNoCaptureResidue(auth, reclaims) {
  if (auth.mode !== AUTH_CONFIG_ROOT) return [];
  const roots = [...new Set(reclaims.map((entry) => entry.accountRoot))];
  const residue = roots.flatMap((root) => findCaptureResidue(root));
  if (residue.length > 0) {
    throw new Error(
      `capture: ${residue.length} agent-repl-capture-* residue director${
        residue.length === 1 ? "y" : "ies"
      } remain under the account root's projects/ after reclaim:\n` +
        `${residue.map((p) => `  ${p}`).join("\n")}\n` +
        "The account is not left as found; this run must not exit 0.",
    );
  }
  return roots;
}

/** Where the committed capture corpus lives, relative to this script. */
export const CORPUS_DIR = path.join(HERE, "..", "..", "testdata", "captures");

/**
 * Read a committed capture's own cwd and vendor session id back off disk.
 *
 * The cwd is NOT reconstructed from the project slug. The slug flattens both
 * `/` and `_` to `-`, so inverting it is ambiguous — and a wrong guess does not
 * fail loudly, it resumes in a directory the vendor has no session for and
 * silently starts a FRESH one, which is the exact non-event this scenario has
 * already produced twice. The cwd is read from the transcript, which records it
 * verbatim on its own lines, and cross-checked against the slug the committed
 * directory is named for.
 */
export function readCapturedSession(corpusDir, captureName, home = homedir()) {
  const projectsDir = path.join(corpusDir, captureName, "files", "projects");
  if (!existsSync(projectsDir)) {
    throw new Error(
      `capture: resume_capture ${captureName} has no files/projects tree at ${projectsDir}`,
    );
  }
  const slugs = readdirSync(projectsDir).filter((entry) =>
    statSync(path.join(projectsDir, entry)).isDirectory(),
  );
  if (slugs.length !== 1) {
    throw new Error(
      `capture: resume_capture ${captureName} has ${slugs.length} project slugs; expected exactly 1`,
    );
  }
  const slugDir = path.join(projectsDir, slugs[0]);
  const transcripts = readdirSync(slugDir).filter((entry) => entry.endsWith(".jsonl"));
  if (transcripts.length !== 1) {
    throw new Error(
      `capture: resume_capture ${captureName} has ${transcripts.length} transcripts; expected exactly 1`,
    );
  }
  const transcriptPath = path.join(slugDir, transcripts[0]);
  const sessionId = transcripts[0].slice(0, -".jsonl".length);
  let cwd = null;
  for (const line of readFileSync(transcriptPath, "utf8").split("\n")) {
    if (line.trim() === "") continue;
    let parsed;
    try {
      parsed = JSON.parse(line);
    } catch {
      continue;
    }
    if (typeof parsed?.cwd === "string" && parsed.cwd !== "") {
      // The recording names no one; it is replayed under this machine's home.
      cwd = expandHome(parsed.cwd, home);
      break;
    }
  }
  if (cwd === null) {
    throw new Error(
      `capture: resume_capture ${captureName}'s transcript records no cwd, so the session ` +
        "cannot be resumed into the directory the vendor slugged it under",
    );
  }
  const slug = expandHome(slugs[0], home);
  if (cwdSlug(cwd) !== slug) {
    throw new Error(
      `capture: resume_capture ${captureName}'s recorded cwd ${cwd} slugs to ` +
        `${cwdSlug(cwd)}, not the committed ${slug}`,
    );
  }
  return { sessionId, slug, cwd, transcriptPath, home };
}

/**
 * Put a committed capture's transcript where the vendor will find it, and
 * recreate the cwd it was slugged from, so `resume: <id>` resolves.
 *
 * THE ACCOUNT ROOT MUST BE SCRATCH, and this refuses rather than trusts the
 * caller. `--config-root` names the operator's REAL root (`~/.claude` by
 * default), which is bind-mounted and shared; seeding a synthetic transcript
 * into it would leave a session there that nothing cleans up, in a tree a stray
 * write has damaged before. A resumed-capture scenario therefore runs ONLY
 * under an account root the harness itself created and throws away — the
 * `--seed-credentials` mode — and the refusal here is what makes that
 * structural rather than a note somebody has to remember.
 */
export function seedResumableSession(auth, accountRoot, seed) {
  if (auth.mode === AUTH_CONFIG_ROOT) {
    throw new Error(
      `capture: a resume_capture scenario must not seed a transcript into the operator's ` +
        `real account root (${accountRoot}). Re-run it with --seed-credentials, which gives ` +
        "the run a throwaway account root of its own.",
    );
  }
  mkdirSync(seed.cwd, { recursive: true });
  const projectDir = path.join(accountRoot, "projects", seed.slug);
  const target = path.join(projectDir, `${seed.sessionId}.jsonl`);
  if (existsSync(target)) {
    throw new Error(`capture: ${target} already exists; refusing to overwrite it`);
  }
  mkdirSync(projectDir, { recursive: true });
  writeFileSync(target, expandHome(readFileSync(seed.transcriptPath, "utf8"), seed.home), "utf8");
  return { projectDir, target };
}

/**
 * The environment variables that name agent-repl's state root.
 *
 * `AGENT_REPL_STATE_DIR` is the daemon's own contract (daemon/internal/envc)
 * and what every in-repo producer of the command-file ingress honors;
 * `CLAUDE_REPL_STATE_DIR` is the older spelling the user-level workspace skill
 * still reads.
 */
export const AGENT_REPL_STATE_ENV_VARS = ["AGENT_REPL_STATE_DIR", "CLAUDE_REPL_STATE_DIR"];

/** The operator's live state root when nothing relocates it. */
export const LIVE_STATE_DIR_NAME = ".claude-emacs";

/**
 * The environment of the vendor child: the operator's environment, the
 * account root, the daemon's ownership mark, the inherited token, and LAST the
 * world's own agent-repl state root.
 *
 * Last, because a capture must never reach the live daemon or its registry:
 * the vendor runs the operator's user-level skills and hooks under
 * `--config-root`, and a skill that dispatches a workspace command (the
 * worktree scenario's model reached for create-or-update-workspace) writes it
 * into whichever state root its environment names. An inherited
 * AGENT_REPL_STATE_DIR pointing at the live root must lose to the scratch one.
 */
export function childEnvFor(auth, world, env) {
  const out = {
    ...accountRootEnvFor(auth, world.configDir, env),
    AGENT_REPL_OWNED: "1",
    ...sdkEnvFor(auth, env),
  };
  for (const name of AGENT_REPL_STATE_ENV_VARS) out[name] = world.agentReplStateDir;
  return out;
}

/**
 * The operator's live agent-repl state root, resolved the way the daemon
 * resolves it: AGENT_REPL_STATE_DIR when set, else ~/.claude-emacs.
 */
export function liveAgentReplStateDir(env, home = homedir()) {
  const named = env.AGENT_REPL_STATE_DIR;
  if (typeof named === "string" && named !== "") return path.resolve(named);
  return path.join(home, LIVE_STATE_DIR_NAME);
}

/** The command-file ingress glob, as daemon/internal/commandfile spells it. */
const DISPATCH_FILE = /^workspace_commands_.*\.json$/;

/**
 * Every command file in a live ingress -- pending, claimed, applied or
 * quarantined -- whose text names `needle` (a world's scratch path). READ
 * ONLY: the live directory is inspected, never written. An absent directory
 * holds nothing; any other read failure throws, because a tripwire that
 * cannot look must not report "clean".
 */
export function liveDispatchesNaming(stateDir, needle) {
  const output = path.join(stateDir, "output");
  const hits = [];
  for (const sub of [".", "claimed", "applied", "quarantine"]) {
    const dir = path.join(output, sub);
    let names;
    try {
      names = readdirSync(dir);
    } catch (err) {
      if (err && err.code === "ENOENT") continue;
      throw err;
    }
    for (const name of names) {
      if (!DISPATCH_FILE.test(name)) continue;
      const file = path.join(dir, name);
      if (readFileSync(file, "utf8").includes(needle)) hits.push(file);
    }
  }
  return hits.sort();
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
export function createWorld(auth, label, seedCwd = null) {
  // realpath, not the mkdtemp spelling: the vendor slugs the project directory
  // from the cwd's RESOLVED path (macOS `/var` is `/private/var`), and the
  // reclaim step must compute the same slug or it misses the transcripts.
  const scratch = realpathSync(mkdtempSync(path.join(tmpdir(), `agent-repl-capture-${label}-`)));
  // A RESUMED world keeps the ORIGINAL capture's cwd: the vendor slugs its
  // project directory from the cwd, so resuming from anywhere else looks up a
  // project holding no such session and quietly starts a fresh one instead.
  const cwd = seedCwd === null ? path.join(scratch, "cwd") : seedCwd;
  const scratchConfigDir = path.join(scratch, "config");
  const spoolRoot = path.join(scratch, "spool");
  const agentReplStateDir = path.join(scratch, "agent-repl-state");
  mkdirSync(cwd, { recursive: true });
  mkdirSync(spoolRoot, { recursive: true });
  mkdirSync(agentReplStateDir, { recursive: true });
  return {
    world: label,
    scratch,
    cwd,
    scratchConfigDir,
    spoolRoot,
    agentReplStateDir,
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
    const entry = { t_ms: Date.now() - t0, dir, msg: anonymize(msg, undefined, opts.personal) };
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
    canUseTool,
    includePartialMessages: true,
    forwardSubagentText: true,
    settingSources: ["user", "project", "local"],
    persistSession: true,
    systemPrompt: { type: "preset", preset: "claude_code" },
    env: childEnvFor(auth, world, process.env),
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

  // ONE session for the scenario. It holds the iterator across turns (NEVER
  // re-derived per turn and never `return()`ed by a for-await break: see
  // drainToResult) and mints a FRESH AbortController per open, so the first
  // query's teardown cannot abort a resumed query.
  const session = createQuerySession({ sdk, prompt: input, options });

  const openQuery = (extraOptions) => {
    const opened = session.open(extraOptions);
    query = opened.query;
    record("control", {
      kind: "query_started",
      options: {
        ...opened.options,
        canUseTool: "[function]",
        abortController: "[AbortController]",
        env: "[inherited]",
      },
    });
    return query;
  };

  try {
    // A resumed-capture scenario opens its FIRST query against the old
    // session: the cold read is what lands with the first prompt, so opening
    // warm and resuming later would capture the wrong thing entirely.
    openQuery(world.resumeSessionId ? { resume: world.resumeSessionId } : {});

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
        record("control", { kind: "resuming", session_id: vendorSessionId, turn: turnIndex });
        // `openQuery` closes the previous query and aborts ITS OWN controller
        // before minting the new one, so the resumed query starts unaborted.
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
      const result = await drainToResult(session.iterator, async (msg) => {
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
    session.close();
    if (previousConfigDir === undefined) delete process.env.CLAUDE_CONFIG_DIR;
    else process.env.CLAUDE_CONFIG_DIR = previousConfigDir;
  }

  // NEVER HARVEST OR RECLAIM AGAINST A LIVE VENDOR CHILD: `session.close()`
  // above only STARTS its shutdown (see waitForVendorChildExit's own doc
  // comment for why). Bounded and loud — a wedged child fails the scenario
  // rather than let the reclaim below race it.
  await waitForVendorChildExit();

  // THE LIVE-REGISTRY TRIPWIRE. childEnvFor points every agent-repl state
  // variable at this world's scratch root, but a producer that prefers
  // ~/.claude-emacs over an explicit override (the out-of-repo
  // create-or-update-workspace skill does) still reaches the operator's live
  // ingress. Such a leak quarantines the scenario, loudly, by name.
  for (const file of liveDispatchesNaming(liveAgentReplStateDir(process.env), world.scratch)) {
    report.errors.push({
      stage: "isolation",
      error: `the capture reached the LIVE agent-repl ingress: ${file} names this world's scratch ${world.scratch}`,
    });
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
        path.join(filesDir, "projects", scrubPersonal(cwdSlug(cwd), opts.personal)),
        report,
        opts.personal,
      );
    }
  } else {
    copyTreeAnonymized(
      path.join(configDir, "projects"),
      path.join(filesDir, "projects"),
      report,
      opts.personal,
    );
  }
  copyTreeAnonymized(spoolRoot, path.join(filesDir, "spool"), report, opts.personal);
  const defaultSpool = path.join("/tmp", `claude-${process.getuid?.() ?? 0}`, cwdSlug(cwd));
  copyTreeAnonymized(defaultSpool, path.join(filesDir, "spool-default"), report, opts.personal);

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
    // The meta records paths and the operator's own report (an unparsed
    // line's file, a cwd_init's output), so it is scrubbed as one document.
    `${JSON.stringify(
      anonymizeMeta({
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
      }, opts.personal),
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
  // Every write into a capture scrubs these; see anonymize.mjs.
  opts.personal = personalValuesFromHost(env, { configRoot: opts.configRoot });

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

    // A RESUMED-CAPTURE scenario reopens a session recorded on an earlier run,
    // days cold. `resumeCaptureOf` forbids pairing it with a shared world, so
    // such a group is always exactly one scenario.
    const resumeCapture = resumeCaptureOf(runnable[0]);
    const seed = resumeCapture === null ? null : readCapturedSession(CORPUS_DIR, resumeCapture);
    const world = createWorld(auth, group.world ?? runnable[0].name, seed?.cwd ?? null);
    if (seed !== null) {
      seedResumableSession(auth, world.configDir, seed);
      world.resumeSessionId = seed.sessionId;
      process.stderr.write(
        `capture.mjs: ${runnable[0].name} resumes ${resumeCapture}'s session ` +
          `${seed.sessionId} (captured earlier, so its prompt cache has long lapsed)\n`,
      );
    }
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
        personal: opts.personal,
      });
      if (!run.outcome.ok) poisoned.push({ scenario: scenario.name, reasons: run.outcome.reasons });
    }
  }

  // SWEEP END, after every query is closed and every SDK child has gone —
  // STRUCTURALLY, now: each scenario's own `waitForVendorChildExit` already
  // blocked until its vendor process actually exited, not merely started
  // closing. The vendor flushes late transcript writes into the operator's
  // real root after the per-scenario reclaim ran, and this pass is what keeps
  // that root clean.
  lateReclaimAll(auth, reclaims, (line) => process.stderr.write(line));
  // THE FINAL VERIFICATION. Never trust that the sweep above actually left
  // the account as found — check, and fail loudly, never silently, if any
  // `agent-repl-capture-*` directory remains under its projects/.
  assertNoCaptureResidue(auth, reclaims);
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
