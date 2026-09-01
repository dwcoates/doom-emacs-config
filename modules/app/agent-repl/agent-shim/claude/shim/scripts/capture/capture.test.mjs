/**
 * Unit tests for the capture harness itself.
 *
 * THE REFUSAL IS TESTED BY SPAWNING the real script, not by calling the gate
 * in-process: the guarantee that matters is that an operator (or a stray CI
 * job) who runs `node capture.mjs` gets a refusal and no vendor call, and only
 * a spawn proves the module's top level does not import the SDK on the way to
 * the gate.
 */
import { spawnSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  realpathSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { TOKEN_ENV_VARS } from "./auth.mjs";
import { isPromptDriven } from "./worlds.mjs";
import {
  CAPTURE_FLAG,
  CaptureRefusedError,
  EXIT_REFUSED,
  FORBID_VENDOR_CALLS_ENV,
  answersFor,
  assertCaptureAuthorized,
  createInputChannel,
  controlMatches,
  createWorld,
  cwdSlug,
  fireTriggers,
  lateReclaimSlug,
  loadPrompts,
  mergeTreeAnonymized,
  messageMatches,
  parseArgv,
  patternMatches,
  resolveTokens,
  runCwdInit,
  permissionResultFor,
  resolvePermissionDecision,
} from "./capture.mjs";

const HERE = path.dirname(fileURLToPath(import.meta.url));
const SCRIPT = path.join(HERE, "capture.mjs");

/**
 * Spawn the script with a controlled environment, never inheriting the suite's.
 *
 * The operator's own credentials are stripped from every spawn: a machine with
 * ANTHROPIC_API_KEY exported would otherwise satisfy the auth preflight and the
 * child would go on to run a REAL scenario against the vendor. The tests here
 * are about refusals, and a refusal test that can accidentally succeed at
 * calling the API is a bug in the test.
 */
function runScript(args, extraEnv = {}) {
  const env = { ...process.env, ...extraEnv };
  if (extraEnv[FORBID_VENDOR_CALLS_ENV] === undefined) {
    delete env[FORBID_VENDOR_CALLS_ENV];
  }
  for (const tokenVar of TOKEN_ENV_VARS) {
    if (extraEnv[tokenVar] === undefined) delete env[tokenVar];
  }
  return spawnSync(process.execPath, [SCRIPT, ...args], { env, encoding: "utf8" });
}

describe("the refusal gate, by spawn", () => {
  it("exits 2 when neither key is present", () => {
    const run = runScript([]);
    expect(run.status).toBe(EXIT_REFUSED);
  });

  it("names the missing flag when only the env key is satisfied", () => {
    const run = runScript([]);
    expect(run.stderr).toContain(CAPTURE_FLAG);
  });

  it("exits 2 when the flag is passed but the forbid variable is set", () => {
    const run = runScript([CAPTURE_FLAG], { [FORBID_VENDOR_CALLS_ENV]: "1" });
    expect(run.status).toBe(EXIT_REFUSED);
  });

  it("names the forbid variable when it is what blocked the run", () => {
    const run = runScript([CAPTURE_FLAG], { [FORBID_VENDOR_CALLS_ENV]: "1" });
    expect(run.stderr).toContain(FORBID_VENDOR_CALLS_ENV);
  });

  it("refuses without printing any capture output", () => {
    const run = runScript([CAPTURE_FLAG], { [FORBID_VENDOR_CALLS_ENV]: "1" });
    expect(run.stdout).toBe("");
  });

  it("still lists the corpus without either key, because listing calls nothing", () => {
    const run = runScript(["--list"], { [FORBID_VENDOR_CALLS_ENV]: "1" });
    expect(run.status).toBe(0);
  });

  it("still checks the corpus without either key", () => {
    const run = runScript(["--check"], { [FORBID_VENDOR_CALLS_ENV]: "1" });
    expect(run.status).toBe(0);
  });
});

describe("the authentication preflight, by spawn", () => {
  it("refuses a run with no authentication mechanism, before any scenario", () => {
    const run = runScript([CAPTURE_FLAG]);
    expect(run.status).toBe(EXIT_REFUSED);
  });

  it("explains the logged-out capture it is preventing", () => {
    expect(runScript([CAPTURE_FLAG]).stderr).toContain("Not logged in");
  });

  it("refuses a --config-root that does not exist", () => {
    const run = runScript([CAPTURE_FLAG, "--config-root", "/no/such/account/root"]);
    expect(run.status).toBe(EXIT_REFUSED);
  });

  it("refuses two mechanisms at once as ambiguous", () => {
    const run = runScript([CAPTURE_FLAG, "--config-root", "/tmp", "--seed-credentials"]);
    expect(run.stderr).toMatch(/mechanisms are in effect at once/);
  });

  it("refuses a config root that is ambiguous with an inherited token", () => {
    const run = runScript([CAPTURE_FLAG, "--config-root", "/tmp"], {
      ANTHROPIC_API_KEY: "not-a-real-key",
    });
    expect(run.status).toBe(EXIT_REFUSED);
  });

  it("still refuses on the two-key gate before it ever reaches authentication", () => {
    const run = runScript(["--config-root", "/tmp"], { [FORBID_VENDOR_CALLS_ENV]: "1" });
    expect(run.stderr).toContain(CAPTURE_FLAG);
  });

  it("lists the corpus without authenticating, because listing calls nothing", () => {
    expect(runScript(["--list"]).status).toBe(0);
  });
});

describe("assertCaptureAuthorized", () => {
  it("throws a named error when the forbid variable is set", () => {
    expect(() => assertCaptureAuthorized({ [FORBID_VENDOR_CALLS_ENV]: "1" }, [CAPTURE_FLAG])).toThrow(
      CaptureRefusedError,
    );
  });

  it("throws when the flag is absent", () => {
    expect(() => assertCaptureAuthorized({}, [])).toThrow(CaptureRefusedError);
  });

  it("treats an empty forbid variable as unset", () => {
    expect(() => assertCaptureAuthorized({ [FORBID_VENDOR_CALLS_ENV]: "" }, [CAPTURE_FLAG])).not.toThrow();
  });

  it("passes when both keys are present", () => {
    expect(() => assertCaptureAuthorized({}, [CAPTURE_FLAG])).not.toThrow();
  });
});

describe("parseArgv", () => {
  it("defaults to running, not listing", () => {
    expect(parseArgv([]).list).toBe(false);
  });

  it("reads --only as a comma-separated list", () => {
    expect(parseArgv(["--only", "a,b"]).only).toEqual(["a", "b"]);
  });

  it("reads --only=<names> in the equals spelling", () => {
    expect(parseArgv(["--only=a,b"]).only).toEqual(["a", "b"]);
  });

  it("resolves --out to an absolute path", () => {
    expect(path.isAbsolute(parseArgv(["--out", "somewhere"]).outDir)).toBe(true);
  });

  it("records the authorization flag", () => {
    expect(parseArgv([CAPTURE_FLAG]).authorized).toBe(true);
  });

  it("reads --config-root as an absolute path", () => {
    expect(path.isAbsolute(parseArgv(["--config-root", "root"]).configRoot)).toBe(true);
  });

  it("reads --config-root=<dir> in the equals spelling", () => {
    expect(parseArgv(["--config-root=/tmp/x"]).configRoot).toBe("/tmp/x");
  });

  it("reads --seed-credentials", () => {
    expect(parseArgv(["--seed-credentials"]).seedCredentials).toBe(true);
  });

  it("reads --credentials-from", () => {
    expect(parseArgv(["--credentials-from=/tmp/root"]).credentialsFrom).toBe("/tmp/root");
  });

  it("defaults to no authentication mechanism, so a bare run must refuse", () => {
    const opts = parseArgv([]);
    expect(opts.configRoot).toBeNull();
    expect(opts.seedCredentials).toBe(false);
  });

  it("refuses an unrecognized argument rather than ignoring it", () => {
    expect(() => parseArgv(["--wat"])).toThrow(/unrecognized argument/);
  });
});

describe("loadPrompts", () => {
  it("loads the committed corpus", () => {
    const doc = loadPrompts(path.join(HERE, "prompts.json"));
    expect(doc.scenarios.length).toBeGreaterThan(0);
  });

  it("gives every scenario either prompt turns or a manual procedure", () => {
    const doc = loadPrompts(path.join(HERE, "prompts.json"));
    const orphans = doc.scenarios.filter((s) => !isPromptDriven(s) && !s.manual);
    expect(orphans.map((s) => s.name)).toEqual([]);
  });

  it("gives every scenario a coverage-list item", () => {
    const doc = loadPrompts(path.join(HERE, "prompts.json"));
    const uncovered = doc.scenarios.filter((s) => typeof s.covers !== "string" || s.covers === "");
    expect(uncovered.map((s) => s.name)).toEqual([]);
  });

  it("gives every scenario at least one expectation", () => {
    const doc = loadPrompts(path.join(HERE, "prompts.json"));
    const bare = doc.scenarios.filter((s) => !Array.isArray(s.expect) || s.expect.length === 0);
    expect(bare.map((s) => s.name)).toEqual([]);
  });

  it("names every scenario uniquely", () => {
    const doc = loadPrompts(path.join(HERE, "prompts.json"));
    const names = doc.scenarios.map((s) => s.name);
    expect(new Set(names).size).toBe(names.length);
  });
});

describe("runCwdInit — the setup file that used to be written and never run", () => {
  it("does nothing when a scenario declares no init command", () => {
    expect(runCwdInit(HERE, undefined)).toBeNull();
  });

  it("does nothing for an empty command", () => {
    expect(runCwdInit(HERE, "")).toBeNull();
  });

  it("runs the command IN the scratch cwd", () => {
    const dir = mkdtempSync(path.join(tmpdir(), "capture-cwdinit-"));
    runCwdInit(dir, "touch marker-from-init");
    expect(existsSync(path.join(dir, "marker-from-init"))).toBe(true);
  });

  it("initializes a real git repository, which the worktree scenario needs", () => {
    const dir = mkdtempSync(path.join(tmpdir(), "capture-cwdinit-git-"));
    writeFileSync(path.join(dir, "README.md"), "# fixture\n", "utf8");
    runCwdInit(
      dir,
      "git init -q . && git add -A && git -c user.email=c@e.invalid -c user.name=c commit -qm init",
    );
    expect(existsSync(path.join(dir, ".git"))).toBe(true);
  });

  it("THROWS on a non-zero exit rather than capturing a golden of the wrong situation", () => {
    const dir = mkdtempSync(path.join(tmpdir(), "capture-cwdinit-fail-"));
    expect(() => runCwdInit(dir, "exit 3")).toThrow(/cwd_init failed \(exit 3\)/);
  });

  it("includes the failing command's stderr in the error", () => {
    const dir = mkdtempSync(path.join(tmpdir(), "capture-cwdinit-fail2-"));
    expect(() => runCwdInit(dir, "echo boom >&2; exit 1")).toThrow(/boom/);
  });
});

describe("the corpus uses the features that retired its manual_setup notes", () => {
  const doc = loadPrompts(path.join(HERE, "prompts.json"));
  const by = Object.fromEntries(doc.scenarios.map((s) => [s.name, s]));

  it("has no scenario left carrying a manual_setup note", () => {
    const remaining = doc.scenarios.filter((s) => s.manual_setup !== undefined);
    expect(remaining.map((s) => s.name)).toEqual([]);
  });

  it("gives the worktree scenario a cwd_init that makes a real git repository", () => {
    expect(by["worktree-enter-exit-kept-and-removed"].cwd_init).toMatch(/git init/);
  });

  it("no longer ships the worktree init script that was never executed", () => {
    const paths = by["worktree-enter-exit-kept-and-removed"].cwd_setup.map((f) => f.path);
    expect(paths).not.toContain(".capture-init.sh");
  });

  it("puts identity-rotation-clear in a world with prior conversation", () => {
    expect(by["identity-rotation-clear"].config_root).toBe(by["prose-streamed"].config_root);
  });

  it("puts compaction-directed in that same world", () => {
    expect(by["compaction-directed"].config_root).toBe(by["prose-streamed"].config_root);
  });

  it("gives identity-rotation-clear a turn before the /clear and one after", () => {
    const turns = by["identity-rotation-clear"].prompts;
    expect(turns).toHaveLength(3);
    expect(turns[1]).toBe("/clear");
  });

  it("drives the cold-resume scenario's resume from the harness", () => {
    const turns = by["cold-resume"].prompts;
    expect(turns[turns.length - 1].resume).toBe(true);
  });
});

describe("resolveTokens — how a static corpus names a file on disk", () => {
  it("substitutes the capture directory into a string", () => {
    expect(resolveTokens("{{CAPTURE_DIR}}/mcp-echo.mjs", "/x")).toBe("/x/mcp-echo.mjs");
  });

  it("substitutes deep inside an options object", () => {
    const resolved = resolveTokens(
      { mcpServers: { p: { command: "node", args: ["{{CAPTURE_DIR}}/mcp-echo.mjs"] } } },
      "/x",
    );
    expect(resolved.mcpServers.p.args[0]).toBe("/x/mcp-echo.mjs");
  });

  it("leaves non-string values alone", () => {
    expect(resolveTokens({ n: 5, b: true, z: null }, "/x")).toEqual({ n: 5, b: true, z: null });
  });

  it("leaves a string with no token unchanged", () => {
    expect(resolveTokens("plain", "/x")).toBe("plain");
  });
});

describe("the MCP scenarios point at the real echo server", () => {
  const doc = loadPrompts(path.join(HERE, "prompts.json"));
  const by = Object.fromEntries(doc.scenarios.map((s) => [s.name, s]));

  it("wires capture-probe into mcp-unmodeled-tool's options", () => {
    const resolved = resolveTokens(by["mcp-unmodeled-tool"].options, HERE);
    expect(resolved.mcpServers["capture-probe"].args[0]).toBe(path.join(HERE, "mcp-echo.mjs"));
  });

  it("resolves to a server file that actually exists", () => {
    const resolved = resolveTokens(by["mcp-unmodeled-tool"].options, HERE);
    expect(existsSync(resolved.mcpServers["capture-probe"].args[0])).toBe(true);
  });

  it("no longer asks the operator to supply an MCP server", () => {
    expect(by["mcp-unmodeled-tool"].manual).toBeUndefined();
  });

  it("gives mcp-server-healths both a healthy and a broken server", () => {
    const servers = by["mcp-server-healths"].options.mcpServers;
    expect(Object.keys(servers)).toEqual(["capture-ok", "capture-broken"]);
  });

  it("no longer ships a comment-only mcp-echo.mjs stub in any cwd_setup", () => {
    const stubs = doc.scenarios.flatMap((s) =>
      (s.cwd_setup ?? []).filter((f) => f.path === "mcp-echo.mjs"),
    );
    expect(stubs).toEqual([]);
  });
});

describe("the structured-output scenario carries an unsatisfiable schema", () => {
  const doc = loadPrompts(path.join(HERE, "prompts.json"));
  const scenario = doc.scenarios.find((s) => s.name === "turn-stop-max-structured-output-retries");

  it("declares a json_schema output format", () => {
    expect(scenario.options.outputFormat.type).toBe("json_schema");
  });

  it("requires the impossible property, so the model cannot omit it", () => {
    expect(scenario.options.outputFormat.schema.required).toEqual(["impossible"]);
  });

  it("makes that property UNSATISFIABLE: minimum above maximum admits no integer", () => {
    const field = scenario.options.outputFormat.schema.properties.impossible;
    expect(field.minimum).toBeGreaterThan(field.maximum);
  });

  it("forbids additional properties, so no other key can satisfy the schema instead", () => {
    expect(scenario.options.outputFormat.schema.additionalProperties).toBe(false);
  });

  it("is now prompt-driven rather than an operator instruction", () => {
    expect(scenario.manual).toBeUndefined();
    expect(isPromptDriven(scenario)).toBe(true);
  });
});

describe("cwdSlug", () => {
  it("replaces every slash and dot, matching the observed vendor spelling", () => {
    expect(cwdSlug("/Users/x/.config/y")).toBe("-Users-x--config-y");
  });

  it("replaces an underscore too, as the vendor does for macOS temp paths", () => {
    expect(cwdSlug("/private/var/folders/_m/T/cwd")).toBe("-private-var-folders--m-T-cwd");
  });
});

describe("createWorld", () => {
  it("returns a cwd whose path is already resolved, so the vendor's slug and the reclaim slug agree", () => {
    const world = createWorld({ mode: "inherited_token", tokenVar: "ANTHROPIC_API_KEY" }, "slug-test");
    expect(world.cwd).toBe(realpathSync(world.cwd));
    expect(world.scratch).toBe(realpathSync(world.scratch));
  });
});

describe("patternMatches", () => {
  it("matches a plain string exactly", () => {
    expect(patternMatches("Bash", "Bash")).toBe(true);
  });

  it("rejects a plain string that only shares a prefix", () => {
    expect(patternMatches("Bash", "BashOutput")).toBe(false);
  });

  it("matches a bare regex spelling", () => {
    expect(patternMatches("/^mcp__/", "mcp__probe__echo")).toBe(true);
  });

  it("honors regex FLAGS, which the question corpus depends on", () => {
    expect(patternMatches("/format/i", "Which FORMAT?")).toBe(true);
  });

  it("treats a lone slash as a literal, not a broken regex", () => {
    expect(patternMatches("/", "/")).toBe(true);
  });
});

describe("resolvePermissionDecision", () => {
  it("falls back to the script's default when no rule matches", () => {
    const rule = resolvePermissionDecision({ default: "deny", rules: [] }, "Bash");
    expect(rule.decision).toBe("deny");
  });

  it("defaults to allow_once when the script names no default", () => {
    expect(resolvePermissionDecision(undefined, "Bash").decision).toBe("allow_once");
  });

  it("matches a rule by exact tool name", () => {
    const script = { default: "deny", rules: [{ tool: "Bash", decision: "allow_once" }] };
    expect(resolvePermissionDecision(script, "Bash").decision).toBe("allow_once");
  });

  it("matches a rule by regex spelling", () => {
    const script = { default: "deny", rules: [{ tool: "/^mcp__/", decision: "allow_once" }] };
    expect(resolvePermissionDecision(script, "mcp__probe__echo").decision).toBe("allow_once");
  });

  it("takes the first matching rule, not the last", () => {
    const script = {
      default: "deny",
      rules: [{ tool: "Bash", decision: "allow_once" }, { tool: "Bash", decision: "deny" }],
    };
    expect(resolvePermissionDecision(script, "Bash").decision).toBe("allow_once");
  });
});

describe("permissionResultFor", () => {
  it("allows once, echoing the input back unchanged", () => {
    const input = { command: "ls" };
    expect(permissionResultFor({ decision: "allow_once" }, input, {})).toEqual({
      behavior: "allow",
      updatedInput: input,
    });
  });

  it("allows standing by returning the vendor's OWN offered suggestions", () => {
    const suggestions = [{ type: "addRules", rules: [], behavior: "allow", destination: "session" }];
    const result = permissionResultFor({ decision: "allow_standing" }, {}, { suggestions });
    expect(result.updatedPermissions).toBe(suggestions);
  });

  it("allows standing with an empty list when the vendor offered none", () => {
    expect(permissionResultFor({ decision: "allow_standing" }, {}, {}).updatedPermissions).toEqual([]);
  });

  it("denies with the scripted message", () => {
    expect(permissionResultFor({ decision: "deny", message: "no" }, {}, {})).toEqual({
      behavior: "deny",
      message: "no",
    });
  });

  it("sets interrupt only when the rule asks for it", () => {
    expect(permissionResultFor({ decision: "deny", interrupt: true }, {}, {}).interrupt).toBe(true);
  });

  it("returns null for the parked (undecidable) arm", () => {
    expect(permissionResultFor({ decision: "park" }, {}, {})).toBeNull();
  });

  it("refuses an unknown decision rather than defaulting to allow", () => {
    expect(() => permissionResultFor({ decision: "maybe" }, {}, {})).toThrow(/unknown permission decision/);
  });
});

describe("answersFor", () => {
  const questions = {
    questions: [
      { question: "Which format?", header: "Format", options: [{ label: "JSON" }, { label: "YAML" }] },
    ],
  };

  it("keys the answer by the question TEXT, as the vendor does", () => {
    const answers = answersFor({ answers: [{ question: "Which format?", select: ["JSON"] }] }, questions);
    expect(answers).toEqual({ "Which format?": "JSON" });
  });

  it("comma-joins a multi-select, as the vendor does", () => {
    const answers = answersFor(
      { answers: [{ question: "Which format?", select: ["JSON", "YAML"] }] },
      questions,
    );
    expect(answers["Which format?"]).toBe("JSON,YAML");
  });

  it("matches a scripted answer by header", () => {
    const answers = answersFor({ answers: [{ question: "Format", select: ["JSON"] }] }, questions);
    expect(answers["Which format?"]).toBe("JSON");
  });

  it("matches a scripted answer by regex", () => {
    const answers = answersFor({ answers: [{ question: "/format/i", select: ["JSON"] }] }, questions);
    expect(answers["Which format?"]).toBe("JSON");
  });

  it("appends free text after the selected labels (the residue rule's provocation)", () => {
    const answers = answersFor(
      { answers: [{ question: "Which format?", select: ["JSON"], free_text: "or TOML" }] },
      questions,
    );
    expect(answers["Which format?"]).toBe("JSON,or TOML");
  });

  it("answers with free text alone when no label was selected", () => {
    const answers = answersFor(
      { answers: [{ question: "Which format?", select: [], free_text: "quarterdeck" }] },
      questions,
    );
    expect(answers["Which format?"]).toBe("quarterdeck");
  });

  it("leaves a question UNANSWERED when the script names no entry for it", () => {
    expect(answersFor({ answers: [] }, questions)).toEqual({});
  });
});

describe("messageMatches", () => {
  it("does not match when there is no matcher at all", () => {
    expect(messageMatches(undefined, { type: "assistant" })).toBe(false);
  });

  it("matches on type", () => {
    expect(messageMatches({ type: "assistant" }, { type: "assistant" })).toBe(true);
  });

  it("rejects a different type", () => {
    expect(messageMatches({ type: "assistant" }, { type: "system" })).toBe(false);
  });

  it("matches on subtype", () => {
    expect(messageMatches({ type: "system", subtype: "init" }, { type: "system", subtype: "init" })).toBe(true);
  });

  it("rejects a different subtype", () => {
    expect(messageMatches({ subtype: "init" }, { subtype: "status" })).toBe(false);
  });

  it("matches on a substring of the serialized message, at any depth", () => {
    expect(messageMatches({ contains: "\"name\":\"Bash\"" }, { tool: { name: "Bash" } })).toBe(true);
  });

  it("rejects a message the substring is absent from", () => {
    expect(messageMatches({ contains: "\"name\":\"Bash\"" }, { name: "Glob" })).toBe(false);
  });
});

describe("createInputChannel", () => {
  it("delivers a message pushed before the reader arrives", async () => {
    const channel = createInputChannel();
    channel.push({ n: 1 });
    const iterator = channel[Symbol.asyncIterator]();
    await expect(iterator.next()).resolves.toEqual({ value: { n: 1 }, done: false });
  });

  it("delivers a message pushed after the reader is waiting", async () => {
    const channel = createInputChannel();
    const iterator = channel[Symbol.asyncIterator]();
    const pending = iterator.next();
    channel.push({ n: 2 });
    await expect(pending).resolves.toEqual({ value: { n: 2 }, done: false });
  });

  it("ends a waiting reader when the channel closes", async () => {
    const channel = createInputChannel();
    const iterator = channel[Symbol.asyncIterator]();
    const pending = iterator.next();
    channel.close();
    await expect(pending).resolves.toEqual({ value: undefined, done: true });
  });

  it("refuses a push after close rather than dropping it", () => {
    const channel = createInputChannel();
    channel.close();
    expect(() => channel.push({})).toThrow(/closed input channel/);
  });
});

describe("controlMatches", () => {
  it("does not match when there is no matcher at all", () => {
    expect(controlMatches(undefined, { kind: "can_use_tool_parked" })).toBe(false);
  });

  it("matches a parked gate on kind", () => {
    expect(controlMatches({ kind: "can_use_tool_parked" }, { kind: "can_use_tool_parked" })).toBe(true);
  });

  it("rejects a different control kind", () => {
    expect(controlMatches({ kind: "can_use_tool_parked" }, { kind: "can_use_tool_request" })).toBe(false);
  });

  it("matches on tool_name", () => {
    expect(
      controlMatches({ tool_name: "Bash" }, { kind: "can_use_tool_parked", tool_name: "Bash" }),
    ).toBe(true);
  });

  it("rejects a different tool_name", () => {
    expect(
      controlMatches({ tool_name: "Bash" }, { kind: "can_use_tool_parked", tool_name: "Read" }),
    ).toBe(false);
  });

  it("matches on a substring of the serialized control record", () => {
    expect(controlMatches({ contains: "park" }, { kind: "can_use_tool_parked" })).toBe(true);
  });
});

/** A query stub that records which control verbs were driven against it. */
function recordingQuery() {
  const calls = [];
  return {
    calls,
    interrupt: async () => {
      calls.push("interrupt");
      return { ok: true };
    },
  };
}

describe("fireTriggers", () => {
  const parkedControl = {
    at: "on_control",
    after: { kind: "can_use_tool_parked" },
    do: "interrupt",
  };

  it("fires the interrupt when a parked-gate control record is replayed", async () => {
    const query = recordingQuery();
    await fireTriggers(
      query,
      [parkedControl],
      "on_control",
      { kind: "can_use_tool_parked", tool_name: "Bash", rule: "park" },
      new Set(),
      () => {},
    );
    expect(query.calls).toEqual(["interrupt"]);
  });

  it("reports the driven verb back to the caller", async () => {
    const results = await fireTriggers(
      recordingQuery(),
      [parkedControl],
      "on_control",
      { kind: "can_use_tool_parked" },
      new Set(),
      () => {},
    );
    expect(results).toEqual([{ verb: "interrupt", ok: true }]);
  });

  it("does not fire an on_control trigger for a plain stream message", async () => {
    const query = recordingQuery();
    await fireTriggers(
      query,
      [parkedControl],
      "on_message",
      { type: "assistant", message: { content: "can_use_tool_parked" } },
      new Set(),
      () => {},
    );
    expect(query.calls).toEqual([]);
  });

  it("does not fire the same control twice", async () => {
    const query = recordingQuery();
    const fired = new Set();
    const payload = { kind: "can_use_tool_parked" };
    await fireTriggers(query, [parkedControl], "on_control", payload, fired, () => {});
    await fireTriggers(query, [parkedControl], "on_control", payload, fired, () => {});
    expect(query.calls).toEqual(["interrupt"]);
  });

  it("leaves an on_message control alone while dispatching on_control", async () => {
    const query = recordingQuery();
    const messageControl = { at: "on_message", after: { type: "assistant" }, do: "interrupt" };
    await fireTriggers(
      query,
      [messageControl],
      "on_control",
      { kind: "can_use_tool_parked" },
      new Set(),
      () => {},
    );
    expect(query.calls).toEqual([]);
  });
});

/** A fake account root and capture directory — no vendor, no real ~/.claude. */
function fakeRoot(slug) {
  const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-late-reclaim-")));
  const accountRoot = path.join(base, "root");
  const captureDir = path.join(base, "capture");
  mkdirSync(path.join(accountRoot, "projects", slug), { recursive: true });
  mkdirSync(path.join(captureDir, "files", "projects", slug), { recursive: true });
  return { base, accountRoot, captureDir };
}

describe("lateReclaimSlug", () => {
  const slug = "-private-var-folders-scratch-cwd";

  it("moves a file the vendor wrote after the first reclaim into the capture", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    writeFileSync(
      path.join(accountRoot, "projects", slug, "late.jsonl"),
      '{"type":"user"}\n',
      "utf8",
    );
    lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] } });
    expect(
      readFileSync(path.join(captureDir, "files", "projects", slug, "late.jsonl"), "utf8"),
    ).toContain('"type":"user"');
  });

  it("leaves the operator's root clean afterwards", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    writeFileSync(path.join(accountRoot, "projects", slug, "late.jsonl"), "{}\n", "utf8");
    lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] } });
    expect(existsSync(path.join(accountRoot, "projects", slug))).toBe(false);
  });

  it("does not touch an unrelated project directory", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    const other = path.join(accountRoot, "projects", "-Users-someone-real-project");
    mkdirSync(other, { recursive: true });
    writeFileSync(path.join(other, "session.jsonl"), "{}\n", "utf8");
    lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] } });
    expect(existsSync(path.join(other, "session.jsonl"))).toBe(true);
  });

  it("logs the late reclaim", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    writeFileSync(path.join(accountRoot, "projects", slug, "late.jsonl"), "{}\n", "utf8");
    const lines = [];
    lateReclaimSlug({
      accountRoot,
      slug,
      captureDir,
      report: { unparsed: [] },
      log: (line) => lines.push(line),
    });
    expect(lines.join("")).toContain(`late reclaim ${slug}`);
  });

  it("reports nothing moved when the vendor wrote nothing late", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    expect(lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] } })).toEqual({
      slug,
      moved: [],
    });
  });
});

describe("mergeTreeAnonymized", () => {
  it("refuses to overwrite a larger captured file with a smaller late flush", () => {
    const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-merge-")));
    const from = path.join(base, "from");
    const to = path.join(base, "to");
    mkdirSync(from, { recursive: true });
    mkdirSync(to, { recursive: true });
    const full = `${'{"a":1}\n'.repeat(20)}`;
    writeFileSync(path.join(to, "session.jsonl"), full, "utf8");
    writeFileSync(path.join(from, "session.jsonl"), '{"a":1}\n', "utf8");
    mergeTreeAnonymized(from, to, { unparsed: [] });
    expect(readFileSync(path.join(to, "session.jsonl"), "utf8")).toBe(full);
  });

  it("writes a file the capture does not have yet", () => {
    const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-merge-")));
    const from = path.join(base, "from");
    const to = path.join(base, "to");
    mkdirSync(from, { recursive: true });
    writeFileSync(path.join(from, "new.txt"), "hello", "utf8");
    expect(mergeTreeAnonymized(from, to, { unparsed: [] })).toEqual([path.join(to, "new.txt")]);
  });

  it("merges a nested subagent sidechain directory", () => {
    const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-merge-")));
    const from = path.join(base, "from");
    const to = path.join(base, "to");
    mkdirSync(path.join(from, "sub"), { recursive: true });
    writeFileSync(path.join(from, "sub", "agent.json"), '{"id":"x"}', "utf8");
    mergeTreeAnonymized(from, to, { unparsed: [] });
    expect(existsSync(path.join(to, "sub", "agent.json"))).toBe(true);
  });
});
