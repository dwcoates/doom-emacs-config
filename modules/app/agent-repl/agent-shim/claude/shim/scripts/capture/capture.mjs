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
  cpSync,
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  readdirSync,
  statSync,
  writeFileSync,
  appendFileSync,
} from "node:fs";
import { homedir, tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import {
  anonymize,
  anonymizeJsonl,
  anonymizePlainText,
} from "./anonymize.mjs";

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
  };
  for (let i = 0; i < argv.length; i += 1) {
    const arg = argv[i];
    if (arg === "--list") opts.list = true;
    else if (arg === "--check") opts.check = true;
    else if (arg === CAPTURE_FLAG) opts.authorized = true;
    else if (arg === "--include-manual") opts.includeManual = true;
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
    const hasPrompt = typeof scenario.prompt === "string" && scenario.prompt !== "";
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
  return cwd.replace(/[/.]/g, "-");
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
async function runScenario(sdk, scenario, opts) {
  const outDir = path.join(opts.outDir, scenario.name);
  mkdirSync(outDir, { recursive: true });
  const streamPath = path.join(outDir, "stream.jsonl");
  writeFileSync(streamPath, "", "utf8");

  const report = { unparsed: [], controls: [], errors: [] };
  const scratch = mkdtempSync(path.join(tmpdir(), `agent-repl-capture-${scenario.name}-`));
  const cwd = path.join(scratch, "cwd");
  const configDir = path.join(scratch, "config");
  const spoolRoot = path.join(scratch, "spool");
  mkdirSync(cwd, { recursive: true });
  mkdirSync(configDir, { recursive: true });
  mkdirSync(spoolRoot, { recursive: true });
  materializeCwd(cwd, scenario.cwd_setup);

  const t0 = Date.now();
  const record = (dir, msg) => {
    appendFileSync(
      streamPath,
      `${JSON.stringify({ t_ms: Date.now() - t0, dir, msg: anonymize(msg) })}\n`,
      "utf8",
    );
  };

  // The scratch config root is the account root for THIS scenario only, so
  // every vendor file the run writes lands inside the capture rather than in
  // the operator's real ~/.claude.
  const previousConfigDir = process.env.CLAUDE_CONFIG_DIR;
  process.env.CLAUDE_CONFIG_DIR = configDir;

  const abort = new AbortController();
  const input = createInputChannel();
  const parked = [];
  let query;

  const canUseTool = async (toolName, toolInput, options) => {
    record("control", { kind: "can_use_tool_request", tool_name: toolName, input: toolInput, options });
    if (toolName === "AskUserQuestion") {
      const answers = answersFor(scenario.question_script, toolInput);
      const result = Object.keys(answers).length === 0
        ? { behavior: "deny", message: "left unanswered by the capture script" }
        : { behavior: "allow", updatedInput: { ...toolInput, answers } };
      record("control", { kind: "can_use_tool_response", tool_name: toolName, result });
      return result;
    }
    const rule = resolvePermissionDecision(scenario.permission_script, toolName);
    const result = permissionResultFor(rule, toolInput, options);
    if (result === null) {
      record("control", { kind: "can_use_tool_parked", tool_name: toolName, rule });
      // The undecidable arm: never resolved here. The scenario's controls are
      // expected to abort, and the settle below denies every parked callback
      // so the vendor process is not left wedged.
      return new Promise((resolve) => parked.push(resolve));
    }
    record("control", { kind: "can_use_tool_response", tool_name: toolName, result });
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
      ...process.env,
      CLAUDE_CONFIG_DIR: configDir,
      AGENT_REPL_OWNED: "1",
    },
    ...(scenario.options ?? {}),
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

  try {
    query = sdk.query({ prompt: input, options });
    record("control", {
      kind: "query_started",
      options: { ...options, canUseTool: "[function]", abortController: "[AbortController]", env: "[inherited]" },
    });

    for (const control of scenario.controls ?? []) {
      if (control.at !== "session_start") continue;
      report.controls.push(await driveControl(query, control, record));
    }

    input.push({
      type: "user",
      message: { role: "user", content: scenario.prompt },
      parent_tool_use_id: null,
      session_id: "",
    });

    for await (const msg of query) {
      record("sdk", msg);
      for (const control of scenario.controls ?? []) {
        if (control.at !== "on_message") continue;
        if (control.fired === true) continue;
        if (!messageMatches(control.after, msg)) continue;
        control.fired = true;
        report.controls.push(await driveControl(query, control, record));
      }
      if (msg.type === "result") break;
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
  copyTreeAnonymized(path.join(configDir, "projects"), path.join(filesDir, "projects"), report);
  copyTreeAnonymized(spoolRoot, path.join(filesDir, "spool"), report);
  const defaultSpool = path.join("/tmp", `claude-${process.getuid?.() ?? 0}`, cwdSlug(cwd));
  copyTreeAnonymized(defaultSpool, path.join(filesDir, "spool-default"), report);

  writeFileSync(
    path.join(outDir, "meta.json"),
    `${JSON.stringify(
      {
        scenario: scenario.name,
        prompt: scenario.prompt ?? null,
        manual: scenario.manual ?? null,
        expect: scenario.expect,
        cwd_slug: cwdSlug(cwd),
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
  return report;
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
      const kind = scenario.prompt ? "prompt" : "MANUAL";
      process.stdout.write(`${scenario.name}\t${kind}\t${scenario.covers ?? ""}\n`);
    }
    return 0;
  }
  if (opts.check) {
    process.stdout.write(
      `${doc.scenarios.length} scenarios, ` +
        `${doc.scenarios.filter((s) => s.prompt).length} prompt-driven, ` +
        `${doc.scenarios.filter((s) => !s.prompt).length} manual\n`,
    );
    return 0;
  }

  assertCaptureAuthorized(env, argv);

  const sdk = await import("@anthropic-ai/claude-agent-sdk");
  mkdirSync(opts.outDir, { recursive: true });
  const skipped = [];
  for (const scenario of selected) {
    if (!scenario.prompt) {
      if (!opts.includeManual) {
        skipped.push(scenario.name);
        continue;
      }
      process.stderr.write(
        `capture.mjs: ${scenario.name} is MANUAL — ${scenario.manual}\n` +
          `Perform the step, then press Enter is NOT wired: run it by hand and capture separately.\n`,
      );
      skipped.push(scenario.name);
      continue;
    }
    process.stderr.write(`capture.mjs: capturing ${scenario.name}\n`);
    await runScenario(sdk, scenario, opts);
  }
  writeFileSync(
    path.join(opts.outDir, "SKIPPED.json"),
    `${JSON.stringify({ skipped, reason: "manual scenarios have no prompt-only provocation" }, null, 2)}\n`,
    "utf8",
  );
  return 0;
}

// Only run when executed directly; importing this file (the unit tests do)
// must have no side effects at all.
if (process.argv[1] !== undefined && path.resolve(process.argv[1]) === path.resolve(fileURLToPath(import.meta.url))) {
  main(process.argv.slice(2), process.env).then(
    (code) => process.exit(code),
    (err) => {
      process.stderr.write(`${err instanceof Error ? err.message : String(err)}\n`);
      process.exit(err instanceof CaptureRefusedError ? EXIT_REFUSED : EXIT_FAILED);
    },
  );
}
