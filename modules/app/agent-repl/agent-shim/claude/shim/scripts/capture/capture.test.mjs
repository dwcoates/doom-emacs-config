/**
 * Unit tests for the capture harness itself.
 *
 * THE REFUSAL IS TESTED BY SPAWNING the real script, not by calling the gate
 * in-process: the guarantee that matters is that an operator (or a stray CI
 * job) who runs `node capture.mjs` gets a refusal and no vendor call, and only
 * a spawn proves the module's top level does not import the SDK on the way to
 * the gate.
 */
import { spawn, spawnSync } from "node:child_process";
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

import { describe, expect, it, vi } from "vitest";

import { TOKEN_ENV_VARS } from "./auth.mjs";
import { isPromptDriven } from "./worlds.mjs";
import {
  AGENT_REPL_STATE_ENV_VARS,
  CAPTURE_FLAG,
  CaptureRefusedError,
  EXIT_REFUSED,
  FORBID_VENDOR_CALLS_ENV,
  VENDOR_CHILD_MARKER,
  answersFor,
  assertCaptureAuthorized,
  assertNoCaptureResidue,
  childEnvFor,
  createInputChannel,
  controlMatches,
  createQuerySession,
  createWorld,
  descendantPids,
  drainToResult,
  cwdSlug,
  findCaptureResidue,
  fireTriggers,
  lateReclaimSlug,
  liveAgentReplStateDir,
  liveDispatchesNaming,
  loadPrompts,
  mergeTreeAnonymized,
  messageMatches,
  parseArgv,
  patternMatches,
  readCapturedSession,
  resolveTokens,
  runCwdInit,
  seedResumableSession,
  permissionResultFor,
  resolvePermissionDecision,
  vendorChildPids,
  waitForVendorChildExit,
  accountEmailIn,
  anonymizeMeta,
  copyTreeAnonymized,
  personalValuesFromHost,
} from "./capture.mjs";
import { NO_PERSONAL_VALUES } from "./anonymize.mjs";
import { AUTH_CONFIG_ROOT, AUTH_SEED_CREDENTIALS } from "./auth.mjs";

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

  it("runs the worktree scenario's git setup, every call in order", () => {
    // NO TEST RUNS REAL GIT. The command runs under `bash -lc`, whose login
    // profile rebuilds PATH ahead of anything a test prepends, so the fake
    // (bin/fake-git.sh) is handed in as an EXPORTED bash function, which a
    // login shell still imports and which wins over every PATH entry.
    const dir = mkdtempSync(path.join(tmpdir(), "capture-cwdinit-git-"));
    const log = path.join(dir, "git-calls.log");
    const fakeGit = path.join(HERE, "..", "..", "..", "..", "..", "bin", "fake-git.sh");
    writeFileSync(path.join(dir, "README.md"), "# fixture\n", "utf8");
    vi.stubEnv("BASH_FUNC_git%%", `() { "${fakeGit}" "$@"; }`);
    vi.stubEnv("FAKE_GIT_LOG", log);
    try {
      runCwdInit(
        dir,
        "git init -q . && git add -A && git -c user.email=c@e.invalid -c user.name=c commit -qm init",
      );
    } finally {
      vi.unstubAllEnvs();
    }
    expect(readFileSync(log, "utf8")).toBe(
      "git init -q .\ngit add -A\ngit -c user.email=c@e.invalid -c user.name=c commit -qm init\n",
    );
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

  it("drives cold-resume by resuming a capture already in the corpus", () => {
    // NOT a `resume: true` TURN any more. That spelling only ever reached a
    // WARM resume — same process, vendor cache still live — which cannot trip
    // the cold-context gate this scenario exists for. It now names a committed
    // capture whose session went cold days ago.
    expect(by["cold-resume"].resume_capture).toBe("prose-streamed");
    expect(by["cold-resume"].prompts).toBeUndefined();
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

describe("childEnvFor — a capture never reaches the live agent-repl state root", () => {
  const TOKEN_AUTH = { mode: "inherited_token", tokenVar: "ANTHROPIC_API_KEY" };
  const WORLD = { configDir: "/scratch/config", agentReplStateDir: "/scratch/agent-repl-state" };

  it.each(AGENT_REPL_STATE_ENV_VARS)("points %s at the world's scratch state root", (name) => {
    // Arrange
    const env = { HOME: "/home/op" };

    // Act
    const out = childEnvFor(TOKEN_AUTH, WORLD, env);

    // Assert
    expect(out[name]).toBe("/scratch/agent-repl-state");
  });

  it("overrides an inherited AGENT_REPL_STATE_DIR naming the live root", () => {
    // Arrange
    const env = { AGENT_REPL_STATE_DIR: "/home/op/.claude-emacs" };

    // Act
    const out = childEnvFor(TOKEN_AUTH, WORLD, env);

    // Assert
    expect(out.AGENT_REPL_STATE_DIR).toBe("/scratch/agent-repl-state");
  });

  it("keeps the daemon's ownership mark", () => {
    // Act
    const out = childEnvFor(TOKEN_AUTH, WORLD, {});

    // Assert
    expect(out.AGENT_REPL_OWNED).toBe("1");
  });

  it("keeps the account root the auth mode names", () => {
    // Act
    const out = childEnvFor(TOKEN_AUTH, WORLD, {});

    // Assert
    expect(out.CLAUDE_CONFIG_DIR).toBe("/scratch/config");
  });
});

describe("createWorld's agent-repl state root", () => {
  it("lives inside the world's scratch", () => {
    // Act
    const world = createWorld({ mode: "inherited_token", tokenVar: "ANTHROPIC_API_KEY" }, "state-root");

    // Assert
    expect(world.agentReplStateDir).toBe(path.join(world.scratch, "agent-repl-state"));
  });

  it("exists before the vendor starts", () => {
    // Act
    const world = createWorld({ mode: "inherited_token", tokenVar: "ANTHROPIC_API_KEY" }, "state-root");

    // Assert
    expect(existsSync(world.agentReplStateDir)).toBe(true);
  });
});

describe("liveAgentReplStateDir", () => {
  it("is ~/.claude-emacs when nothing relocates it", () => {
    expect(liveAgentReplStateDir({}, "/home/op")).toBe("/home/op/.claude-emacs");
  });

  it("follows AGENT_REPL_STATE_DIR when the operator set one", () => {
    expect(liveAgentReplStateDir({ AGENT_REPL_STATE_DIR: "/elsewhere" }, "/home/op")).toBe("/elsewhere");
  });

  it("treats an empty AGENT_REPL_STATE_DIR as unset", () => {
    expect(liveAgentReplStateDir({ AGENT_REPL_STATE_DIR: "" }, "/home/op")).toBe("/home/op/.claude-emacs");
  });
});

describe("liveDispatchesNaming — the live-registry tripwire", () => {
  const NEEDLE = "/tmp/agent-repl-capture-x-abc";

  function stateRoot(files) {
    const root = mkdtempSync(path.join(tmpdir(), "capture-tripwire-"));
    for (const [rel, text] of Object.entries(files)) {
      const file = path.join(root, "output", rel);
      mkdirSync(path.dirname(file), { recursive: true });
      writeFileSync(file, text, "utf8");
    }
    return root;
  }

  it.each([".", "claimed", "applied", "quarantine"])("finds a command file in %s naming the scratch", (sub) => {
    // Arrange
    const root = stateRoot({ [path.join(sub, "workspace_commands_1.json")]: `[{"git_root":"${NEEDLE}/cwd"}]` });

    // Act
    const hits = liveDispatchesNaming(root, NEEDLE);

    // Assert
    expect(hits).toEqual([path.join(root, "output", sub, "workspace_commands_1.json")]);
  });

  it("ignores a command file that names some other directory", () => {
    // Arrange
    const root = stateRoot({ "workspace_commands_1.json": '[{"git_root":"/Users/op/repo"}]' });

    // Act / Assert
    expect(liveDispatchesNaming(root, NEEDLE)).toEqual([]);
  });

  it("ignores a producer's dot-prefixed staging file", () => {
    // Arrange
    const root = stateRoot({ ".workspace_commands_tmp": NEEDLE });

    // Act / Assert
    expect(liveDispatchesNaming(root, NEEDLE)).toEqual([]);
  });

  it("finds nothing in a state root that has no output directory", () => {
    // Arrange
    const root = mkdtempSync(path.join(tmpdir(), "capture-tripwire-"));

    // Act / Assert
    expect(liveDispatchesNaming(root, NEEDLE)).toEqual([]);
  });

  it("throws when the ingress cannot be read, rather than reporting it clean", () => {
    // Arrange: `output` is a FILE, so listing beneath it fails with ENOTDIR.
    const root = mkdtempSync(path.join(tmpdir(), "capture-tripwire-"));
    writeFileSync(path.join(root, "output"), "", "utf8");

    // Act / Assert
    expect(() => liveDispatchesNaming(root, NEEDLE)).toThrow(/ENOTDIR/);
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
    lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] }, personal: NO_PERSONAL_VALUES });
    expect(
      readFileSync(path.join(captureDir, "files", "projects", slug, "late.jsonl"), "utf8"),
    ).toContain('"type":"user"');
  });

  it("leaves the operator's root clean afterwards", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    writeFileSync(path.join(accountRoot, "projects", slug, "late.jsonl"), "{}\n", "utf8");
    lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] }, personal: NO_PERSONAL_VALUES });
    expect(existsSync(path.join(accountRoot, "projects", slug))).toBe(false);
  });

  it("does not touch an unrelated project directory", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    const other = path.join(accountRoot, "projects", "-Users-someone-real-project");
    mkdirSync(other, { recursive: true });
    writeFileSync(path.join(other, "session.jsonl"), "{}\n", "utf8");
    lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] }, personal: NO_PERSONAL_VALUES });
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
      personal: NO_PERSONAL_VALUES,
      log: (line) => lines.push(line),
    });
    expect(lines.join("")).toContain(`late reclaim ${slug}`);
  });

  it("reports nothing moved when the vendor wrote nothing late", () => {
    const { accountRoot, captureDir } = fakeRoot(slug);
    expect(lateReclaimSlug({ accountRoot, slug, captureDir, report: { unparsed: [] }, personal: NO_PERSONAL_VALUES })).toEqual({
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
    mergeTreeAnonymized(from, to, { unparsed: [] }, NO_PERSONAL_VALUES);
    expect(readFileSync(path.join(to, "session.jsonl"), "utf8")).toBe(full);
  });

  it("writes a file the capture does not have yet", () => {
    const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-merge-")));
    const from = path.join(base, "from");
    const to = path.join(base, "to");
    mkdirSync(from, { recursive: true });
    writeFileSync(path.join(from, "new.txt"), "hello", "utf8");
    expect(mergeTreeAnonymized(from, to, { unparsed: [] }, NO_PERSONAL_VALUES)).toEqual([path.join(to, "new.txt")]);
  });

  it("merges a nested subagent sidechain directory", () => {
    const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-merge-")));
    const from = path.join(base, "from");
    const to = path.join(base, "to");
    mkdirSync(path.join(from, "sub"), { recursive: true });
    writeFileSync(path.join(from, "sub", "agent.json"), '{"id":"x"}', "utf8");
    mergeTreeAnonymized(from, to, { unparsed: [] }, NO_PERSONAL_VALUES);
    expect(existsSync(path.join(to, "sub", "agent.json"))).toBe(true);
  });
});

describe("descendantPids and vendorChildPids", () => {
  // Spawns a REAL, harmless child process (its own subtree, torn down at the
  // end of the test) to prove the pid-walk itself, since `pgrep`/`ps` are OS
  // utilities the harness already relies on directly — never the vendor.
  function withSleepingChild(scriptDir, run) {
    const dir = mkdtempSync(path.join(tmpdir(), scriptDir));
    const script = path.join(dir, "child.mjs");
    writeFileSync(script, "setTimeout(() => {}, 30000);\n", "utf8");
    const child = spawn(process.execPath, [script], { stdio: "ignore" });
    return new Promise((resolve, reject) => {
      child.once("spawn", async () => {
        try {
          await run(child.pid);
          resolve();
        } catch (err) {
          reject(err);
        } finally {
          child.kill("SIGKILL");
        }
      });
      child.once("error", reject);
    });
  }

  // Explicit, generous timeouts (not the suite's tight in-process default):
  // these fork+exec a real OS process and shell out to pgrep/ps, which is
  // measurably slower than the pure-JS logic the rest of this file covers.
  it(
    "descendantPids finds a real spawned child of this process",
    () =>
      withSleepingChild("capture-descendant-", async (childPid) => {
        // Give the OS a moment to register the fork before pgrep looks for it.
        for (let attempt = 0; attempt < 20; attempt += 1) {
          if (descendantPids(process.pid).includes(childPid)) return;
          await new Promise((r) => setTimeout(r, 50));
        }
        expect(descendantPids(process.pid)).toContain(childPid);
      }),
    5000,
  );

  it(
    "vendorChildPids ignores a descendant that is not the vendor binary",
    () =>
      withSleepingChild("capture-nonvendor-", async (childPid) => {
        for (let attempt = 0; attempt < 20; attempt += 1) {
          if (descendantPids(process.pid).includes(childPid)) break;
          await new Promise((r) => setTimeout(r, 50));
        }
        expect(vendorChildPids(process.pid)).not.toContain(childPid);
      }),
    5000,
  );

  it(
    "vendorChildPids finds a descendant whose command line names the marker",
    () =>
      withSleepingChild(`capture-${VENDOR_CHILD_MARKER}-`, async (childPid) => {
        for (let attempt = 0; attempt < 40; attempt += 1) {
          if (vendorChildPids(process.pid).includes(childPid)) return;
          await new Promise((r) => setTimeout(r, 50));
        }
        expect(vendorChildPids(process.pid)).toContain(childPid);
      }),
    5000,
  );

  it("returns an empty list for a pid with no children", () => {
    expect(descendantPids(999999)).toEqual([]);
    expect(vendorChildPids(999999)).toEqual([]);
  });
});

describe("waitForVendorChildExit — the ordering hole's structural fix", () => {
  it("returns immediately when no vendor child survives", async () => {
    const seen = [];
    await waitForVendorChildExit({
      pid: 4242,
      listSurvivors: (pid) => {
        seen.push(pid);
        return [];
      },
      sleep: () => {
        throw new Error("must not sleep when the first poll is already clean");
      },
    });
    expect(seen).toEqual([4242]);
  });

  it("polls until the vendor child is gone, then returns", async () => {
    let calls = 0;
    const sleeps = [];
    await waitForVendorChildExit({
      pid: 1,
      listSurvivors: () => {
        calls += 1;
        return calls < 3 ? [999] : [];
      },
      sleep: (ms) => {
        sleeps.push(ms);
        return Promise.resolve();
      },
    });
    expect(calls).toBe(3);
    expect(sleeps.length).toBe(2);
  });

  it("throws — bounded and loud — rather than wait forever for a wedged child", async () => {
    let now = 0;
    const realNow = Date.now;
    Date.now = () => now;
    try {
      await expect(
        waitForVendorChildExit({
          pid: 7,
          timeoutMs: 500,
          intervalMs: 100,
          listSurvivors: () => [555],
          sleep: (ms) => {
            now += ms;
            return Promise.resolve();
          },
        }),
      ).rejects.toThrow(/vendor CLI child process\(es\) still alive/);
    } finally {
      Date.now = realNow;
    }
  });

  it("names the surviving pid(s) in the thrown error", async () => {
    let now = 0;
    const realNow = Date.now;
    Date.now = () => now;
    try {
      await expect(
        waitForVendorChildExit({
          pid: 7,
          timeoutMs: 100,
          intervalMs: 100,
          listSurvivors: () => [555, 556],
          sleep: (ms) => {
            now += ms;
            return Promise.resolve();
          },
        }),
      ).rejects.toThrow(/555, 556/);
    } finally {
      Date.now = realNow;
    }
  });
});

describe("findCaptureResidue", () => {
  it("finds an agent-repl-capture-* directory left under projects/", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    const residue = path.join(
      accountRoot,
      "projects",
      "-private-var-folders-agent-repl-capture-prose-streamed-abc123-cwd",
    );
    mkdirSync(residue, { recursive: true });
    expect(findCaptureResidue(accountRoot)).toEqual([residue]);
  });

  it("ignores the operator's own real project directories", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    mkdirSync(path.join(accountRoot, "projects", "-Users-someone-real-project"), {
      recursive: true,
    });
    expect(findCaptureResidue(accountRoot)).toEqual([]);
  });

  it("returns an empty list when the account root has no projects/ at all", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    expect(findCaptureResidue(accountRoot)).toEqual([]);
  });
});

describe("assertNoCaptureResidue", () => {
  it("does nothing under a non-config-root auth mode", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    mkdirSync(path.join(accountRoot, "projects", "agent-repl-capture-leftover"), {
      recursive: true,
    });
    expect(() =>
      assertNoCaptureResidue({ mode: AUTH_SEED_CREDENTIALS }, [{ accountRoot }]),
    ).not.toThrow();
  });

  it("passes silently when every touched account root is clean", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    mkdirSync(path.join(accountRoot, "projects"), { recursive: true });
    expect(() =>
      assertNoCaptureResidue({ mode: AUTH_CONFIG_ROOT }, [{ accountRoot }]),
    ).not.toThrow();
  });

  it("throws — loud, never silent — naming the exact residue path", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    const residue = path.join(accountRoot, "projects", "agent-repl-capture-leftover");
    mkdirSync(residue, { recursive: true });
    expect(() => assertNoCaptureResidue({ mode: AUTH_CONFIG_ROOT }, [{ accountRoot }])).toThrow(
      new RegExp(residue.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")),
    );
  });

  it("checks every distinct account root exactly once, deduplicated", () => {
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-residue-")));
    mkdirSync(path.join(accountRoot, "projects"), { recursive: true });
    expect(
      assertNoCaptureResidue({ mode: AUTH_CONFIG_ROOT }, [
        { accountRoot },
        { accountRoot },
      ]),
    ).toEqual([accountRoot]);
  });
});

describe("the corpus's control triggers", () => {
  const doc = loadPrompts(path.join(HERE, "prompts.json"));
  const controls = doc.scenarios.flatMap((scenario) =>
    (scenario.controls ?? []).map((control) => ({ scenario: scenario.name, control })),
  );

  // THE DEFECT SHAPE, PINNED SHUT. A permission gate is recorded on the control
  // plane (can_use_tool_request / _response / _parked), never as a stream
  // message, so an `on_message` trigger naming one of those spellings can never
  // fire — which is exactly how permission-undecidable-parked wedged forever.
  it("carries no on_message trigger whose contains names a can_use_tool spelling", () => {
    const offenders = controls
      .filter(({ control }) => control.at === "on_message")
      .filter(({ control }) => (control.after?.contains ?? "").includes("can_use_tool"))
      .map(({ scenario }) => scenario);
    expect(offenders).toEqual([]);
  });

  it("triggers held-turn-gate off the gate's own control record", () => {
    const scenario = doc.scenarios.find((entry) => entry.name === "held-turn-gate");
    expect(scenario.controls).toEqual([
      { at: "on_control", after: { kind: "can_use_tool_request" }, do: "getContextUsage" },
    ]);
  });

  it("uses only trigger points the harness dispatches", () => {
    const unknown = controls
      .filter(({ control }) => !["session_start", "on_message", "on_control", "turn_end"].includes(control.at))
      .map(({ scenario, control }) => `${scenario}:${control.at}`);
    expect(unknown).toEqual([]);
  });

  it("gives every on_control trigger a matcher, since an absent one never fires", () => {
    const unmatched = controls
      .filter(({ control }) => control.at === "on_control" && control.after === undefined)
      .map(({ scenario }) => scenario);
    expect(unmatched).toEqual([]);
  });
});

/**
 * A fake query: an async generator over scripted per-turn message batches, with
 * the SDK's own surface (an async iterator plus `close`).
 *
 * It records whether `return()` was ever called on its iterator — the exact
 * thing a `for await ... break` does, and the reason every turn after the first
 * went unrecorded in the real multi-turn captures.
 */
function fakeQuery(turnBatches) {
  const state = { returned: false, closed: false, pulls: 0 };
  const batches = turnBatches.map((batch) => [...batch]);
  const generator = (async function* messages() {
    try {
      for (const batch of batches) {
        for (const msg of batch) {
          state.pulls += 1;
          yield msg;
        }
      }
    } finally {
      state.returned = true;
    }
  })();
  return {
    state,
    close: () => {
      state.closed = true;
    },
    [Symbol.asyncIterator]: () => generator,
  };
}

const TURN_ONE = [
  { type: "system", subtype: "init", session_id: "s-1" },
  { type: "assistant", message: { content: "one" } },
  { type: "result", subtype: "success" },
];
const TURN_TWO = [
  { type: "system", subtype: "compact_boundary" },
  { type: "result", subtype: "success" },
];

describe("drainToResult", () => {
  it("returns the turn's result message", async () => {
    const query = fakeQuery([TURN_ONE]);
    const iterator = query[Symbol.asyncIterator]();
    await expect(drainToResult(iterator, () => {})).resolves.toEqual({
      type: "result",
      subtype: "success",
    });
  });

  it("hands every message of the turn to the recorder, in order", async () => {
    const query = fakeQuery([TURN_ONE]);
    const seen = [];
    await drainToResult(query[Symbol.asyncIterator](), (msg) => seen.push(msg.type));
    expect(seen).toEqual(["system", "assistant", "result"]);
  });

  it("stops at the result rather than draining the next turn", async () => {
    const query = fakeQuery([TURN_ONE, TURN_TWO]);
    const seen = [];
    await drainToResult(query[Symbol.asyncIterator](), (msg) => seen.push(msg.subtype));
    expect(seen).toEqual(["init", undefined, "success"]);
  });

  it("does NOT end the iterator when it stops at a result", async () => {
    const query = fakeQuery([TURN_ONE, TURN_TWO]);
    await drainToResult(query[Symbol.asyncIterator](), () => {});
    expect(query.state.returned).toBe(false);
  });

  it("records the SECOND turn on the same held iterator", async () => {
    const query = fakeQuery([TURN_ONE, TURN_TWO]);
    const iterator = query[Symbol.asyncIterator]();
    await drainToResult(iterator, () => {});
    const seen = [];
    await drainToResult(iterator, (msg) => seen.push(msg.subtype));
    expect(seen).toEqual(["compact_boundary", "success"]);
  });

  it("awaits an async recorder before pulling the next message", async () => {
    const query = fakeQuery([TURN_ONE]);
    const order = [];
    await drainToResult(query[Symbol.asyncIterator](), async (msg) => {
      order.push(`enter:${msg.type}`);
      await Promise.resolve();
      order.push(`leave:${msg.type}`);
    });
    expect(order.slice(0, 4)).toEqual([
      "enter:system",
      "leave:system",
      "enter:assistant",
      "leave:assistant",
    ]);
  });

  it("returns null when the query ends without a result", async () => {
    const query = fakeQuery([[{ type: "assistant", message: { content: "orphan" } }]]);
    await expect(drainToResult(query[Symbol.asyncIterator](), () => {})).resolves.toBeNull();
  });

  it("still reports the messages seen before a query ended without a result", async () => {
    const query = fakeQuery([[{ type: "assistant" }]]);
    const seen = [];
    await drainToResult(query[Symbol.asyncIterator](), (msg) => seen.push(msg.type));
    expect(seen).toEqual(["assistant"]);
  });

  it("leaves the query open for a turn_end control to be driven against", async () => {
    const query = fakeQuery([TURN_ONE]);
    await drainToResult(query[Symbol.asyncIterator](), () => {});
    expect(query.state.closed).toBe(false);
  });
});

describe("the corpus's declared error terminals", () => {
  const doc = loadPrompts(path.join(HERE, "prompts.json"));
  const scenarioNamed = (name) => doc.scenarios.find((entry) => entry.name === name);

  // The parked gate's golden IS an aborted terminal: the interrupt fires while
  // a permission callback is pending, and the vendor ends the turn with
  // subtype "error_during_execution" / terminal_reason "aborted_tools"
  // (observed in captures/_failed/permission-undecidable-parked). Without the
  // declaration the quarantine rule condemns the capture the scenario exists
  // for, exactly as it did on the first real run.
  it("declares the aborted terminal permission-undecidable-parked exists to capture", () => {
    expect(scenarioNamed("permission-undecidable-parked").expects_error_subtypes).toEqual([
      "error_during_execution",
    ]);
  });

  it("keeps that scenario's interrupt scripted off the parked control record", () => {
    expect(scenarioNamed("permission-undecidable-parked").controls).toEqual([
      { at: "on_control", after: { kind: "can_use_tool_parked" }, do: "interrupt" },
    ]);
  });

  // NOT every interrupt-driven scenario: hook-cancelled's real capture ended
  // `success` / `completed`, so declaring an error terminal for it would
  // whitelist a failure it is not supposed to have.
  it("declares an error terminal only as a non-empty list of subtypes", () => {
    const malformed = doc.scenarios
      .filter((entry) => entry.expects_error_subtypes !== undefined)
      .filter((entry) => {
        const declared = entry.expects_error_subtypes;
        return (
          !Array.isArray(declared) ||
          declared.length === 0 ||
          declared.some((subtype) => typeof subtype !== "string" || subtype === "")
        );
      })
      .map((entry) => entry.name);
    expect(malformed).toEqual([]);
  });

  // A provocation the model declines on its own judgement never reaches the
  // gate: `rm -rf .` recorded no can_use_tool at all, so the scenario captured
  // nothing about denial.
  it("provokes permission-denied-by-user with a command the model will attempt", () => {
    expect(scenarioNamed("permission-denied-by-user").prompt).not.toContain("rm -rf");
  });

  it("materializes the file that scenario's command acts on", () => {
    expect(scenarioNamed("permission-denied-by-user").cwd_setup).toEqual([
      { path: "stale.log", content: "stale\n" },
    ]);
  });

  it("keeps that scenario's expectations about the denial", () => {
    expect(scenarioNamed("permission-denied-by-user").expect).toEqual([
      "can_use_tool request",
      "PermissionResult deny",
      "no activity frames for the denied call",
    ]);
  });
});

/**
 * A fake SDK that mimics the ONE property of the real one that caused the
 * `cold-resume` failure: `query()` refuses if the AbortController it is handed
 * has already fired, and `close()` on a query aborts the controller that query
 * was opened with. No vendor call is involved.
 */
function fakeAbortAwareSdk() {
  const opens = [];
  return {
    opens,
    query({ prompt, options }) {
      if (options.abortController.signal.aborted) throw new Error("Operation aborted");
      const opened = {
        prompt,
        options,
        signal: options.abortController.signal,
        closed: false,
      };
      opened.close = () => {
        opened.closed = true;
        options.abortController.abort();
      };
      opens.push(opened);
      return Object.assign(opened, {
        [Symbol.asyncIterator]: () => ({ next: async () => ({ done: true }) }),
      });
    },
  };
}

describe("createQuerySession", () => {
  it("gives the first open its own live controller", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    expect(session.controller.signal.aborted).toBe(false);
  });

  it("reopens for a resume without throwing the aborted error", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    expect(() => session.open({ resume: "s-1" })).not.toThrow();
  });

  it("hands the resumed open an unaborted signal", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[1].signal.aborted).toBe(false);
  });

  it("gives the resumed open a signal distinct from the first turn's", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[1].signal).not.toBe(sdk.opens[0].signal);
  });

  it("leaves the resumed signal unaborted when the first turn's controller fires", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    const firstController = sdk.opens[0].options.abortController;
    session.open({ resume: "s-1" });
    firstController.abort();
    expect(sdk.opens[1].signal.aborted).toBe(false);
  });

  it("closes the previous query before reopening", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[0].closed).toBe(true);
  });

  it("aborts the first turn's controller when that query is torn down", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[0].signal.aborted).toBe(true);
  });

  it("threads the resume id into the reopened query's options", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[1].options.resume).toBe("s-1");
  });

  it("carries the base options into every open", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[1].options.cwd).toBe("/w");
  });

  it("does not carry the previous open's extra options forward", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({ resume: "s-1" });
    session.open({});
    expect(sdk.opens[1].options.resume).toBeUndefined();
  });

  it("keeps the same prompt channel across a resume", () => {
    const sdk = fakeAbortAwareSdk();
    const prompt = { channel: true };
    const session = createQuerySession({ sdk, prompt, options: { cwd: "/w" } });
    session.open({});
    session.open({ resume: "s-1" });
    expect(sdk.opens[1].prompt).toBe(prompt);
  });

  it("exposes the reopened query's own iterator", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    const first = session.iterator;
    session.open({ resume: "s-1" });
    expect(session.iterator).not.toBe(first);
  });

  it("aborts the live controller on close", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    const controller = session.controller;
    session.close();
    expect(controller.signal.aborted).toBe(true);
  });

  it("closes the live query on close", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.close();
    expect(sdk.opens[0].closed).toBe(true);
  });

  it("is idempotent on a second close", () => {
    const sdk = fakeAbortAwareSdk();
    const session = createQuerySession({ sdk, prompt: "p", options: { cwd: "/w" } });
    session.open({});
    session.close();
    expect(() => session.close()).not.toThrow();
  });

  it("survives a query whose close throws", () => {
    const sdk = {
      query: () => ({
        close: () => { throw new Error("already gone"); },
        [Symbol.asyncIterator]: () => ({ next: async () => ({ done: true }) }),
      }),
    };
    const session = createQuerySession({ sdk, prompt: "p", options: {} });
    session.open({});
    expect(() => session.close()).not.toThrow();
  });

  it("still opens the resumed query when the previous close throws", () => {
    let opened = 0;
    const sdk = {
      query: () => {
        opened += 1;
        return {
          close: () => { throw new Error("already gone"); },
          [Symbol.asyncIterator]: () => ({ next: async () => ({ done: true }) }),
        };
      },
    };
    const session = createQuerySession({ sdk, prompt: "p", options: {} });
    session.open({});
    session.open({ resume: "s-1" });
    expect(opened).toBe(2);
  });
});

describe("resuming a session captured on an earlier run", () => {
  /** A committed capture's committed layout, in a throwaway corpus. */
  const corpusWith = (linesOf) => {
    const corpus = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-corpus-")));
    const cwd = path.join(corpus, "world", "cwd");
    const slugDir = path.join(corpus, "old", "files", "projects", cwdSlug(cwd));
    mkdirSync(slugDir, { recursive: true });
    writeFileSync(
      path.join(slugDir, "11111111-2222-3333-4444-555555555555.jsonl"),
      linesOf(cwd)
        .map((line) => JSON.stringify(line))
        .join("\n"),
      "utf8",
    );
    return { corpus, cwd };
  };
  const SCRATCH_AUTH = { mode: AUTH_SEED_CREDENTIALS };
  const WORLD_AUTH = { mode: "inherited_token", tokenVar: "ANTHROPIC_API_KEY" };

  it("reads the session id, slug and cwd back off a committed capture", () => {
    const { corpus, cwd } = corpusWith((dir) => [{ type: "attachment", cwd: dir }]);
    const seed = readCapturedSession(corpus, "old");
    expect(seed.sessionId).toBe("11111111-2222-3333-4444-555555555555");
    expect(seed.cwd).toBe(cwd);
    expect(seed.slug).toBe(cwdSlug(cwd));
  });

  it("skips lines that carry no cwd rather than giving up on the first one", () => {
    // The real transcripts open with `queue-operation` records, which have no
    // cwd at all; the cwd first appears a couple of lines in.
    const { corpus, cwd } = corpusWith((dir) => [{ type: "queue-operation" }, { type: "attachment", cwd: dir }]);
    expect(readCapturedSession(corpus, "old").cwd).toBe(cwd);
  });

  it("refuses a capture whose transcript records no cwd", () => {
    const { corpus } = corpusWith(() => [{ type: "queue-operation" }]);
    expect(() => readCapturedSession(corpus, "old")).toThrow(/records no cwd/);
  });

  it("refuses a capture whose recorded cwd disagrees with its committed slug", () => {
    // The guard that makes a silently-wrong resume impossible: a cwd that does
    // not slug to the committed directory would resume in a project holding no
    // such session, and the vendor would quietly start a fresh one.
    const { corpus } = corpusWith(() => [{ type: "attachment", cwd: "/somewhere/else" }]);
    expect(() => readCapturedSession(corpus, "old")).toThrow(/not the committed/);
  });

  it("refuses a capture directory that was never committed", () => {
    const { corpus } = corpusWith(() => [{ type: "attachment", cwd: "/x" }]);
    expect(() => readCapturedSession(corpus, "absent")).toThrow(/no files\/projects tree/);
  });

  it("seeds the transcript into a scratch account root, and recreates the cwd", () => {
    const { corpus } = corpusWith((dir) => [{ type: "attachment", cwd: dir }]);
    const seed = readCapturedSession(corpus, "old");
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-root-")));
    const { target } = seedResumableSession(SCRATCH_AUTH, accountRoot, seed);
    expect(existsSync(target)).toBe(true);
    expect(existsSync(seed.cwd)).toBe(true);
    expect(target).toBe(path.join(accountRoot, "projects", seed.slug, `${seed.sessionId}.jsonl`));
  });

  it("REFUSES to seed into the operator's real account root", () => {
    // ~/.claude is bind-mounted and shared; a synthetic transcript seeded there
    // would outlive the run in a tree a stray write has damaged before. The
    // refusal is what makes "scratch only" structural instead of a note.
    const { corpus } = corpusWith((dir) => [{ type: "attachment", cwd: dir }]);
    const seed = readCapturedSession(corpus, "old");
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-root-")));
    expect(() => seedResumableSession({ mode: AUTH_CONFIG_ROOT }, accountRoot, seed)).toThrow(
      /--seed-credentials/,
    );
    expect(existsSync(path.join(accountRoot, "projects"))).toBe(false);
  });

  it("refuses to overwrite a transcript already sitting at the target", () => {
    const { corpus } = corpusWith((dir) => [{ type: "attachment", cwd: dir }]);
    const seed = readCapturedSession(corpus, "old");
    const accountRoot = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-root-")));
    seedResumableSession(SCRATCH_AUTH, accountRoot, seed);
    expect(() => seedResumableSession(SCRATCH_AUTH, accountRoot, seed)).toThrow(
      /refusing to overwrite/,
    );
  });

  it("gives a resumed world the captured cwd instead of a fresh scratch one", () => {
    const { corpus } = corpusWith((dir) => [{ type: "attachment", cwd: dir }]);
    const seed = readCapturedSession(corpus, "old");
    const world = createWorld(WORLD_AUTH, "cold-resume", seed.cwd);
    expect(world.cwd).toBe(seed.cwd);
  });

  it("still gives an ordinary world a scratch cwd of its own", () => {
    const world = createWorld(WORLD_AUTH, "ordinary");
    expect(world.cwd).toBe(path.join(world.scratch, "cwd"));
  });
});

describe("personal values", () => {
  /** A fake home holding the given identity files (path -> JSON text). */
  function fakeHome(files) {
    const home = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-home-")));
    for (const [rel, text] of Object.entries(files)) {
      mkdirSync(path.dirname(path.join(home, rel)), { recursive: true });
      writeFileSync(path.join(home, rel), text, "utf8");
    }
    return home;
  }
  const identity = (email) => JSON.stringify({ oauthAccount: { emailAddress: email } });

  it("collects every account root's email, the configured root's, and the env's", () => {
    const home = fakeHome({
      ".claude.json": identity("personal@host.test"),
      ".claude-chesscom/.claude.json": identity("work@host.test"),
      "other/.claude.json": identity("other@host.test"),
    });
    const got = personalValuesFromHost(
      { CAPTURE_PERSONAL_EMAILS: "extra@host.test, " },
      { home, username: "login", gitUserName: () => "", configRoot: path.join(home, "other") },
    );
    expect(got.emails.sort()).toEqual(
      ["extra@host.test", "other@host.test", "personal@host.test", "work@host.test"],
    );
  });

  it("collects the login, the home's last segment, the git name's words and the env's names", () => {
    const home = fakeHome({});
    const got = personalValuesFromHost(
      { CAPTURE_PERSONAL_NAMES: "Nickname" },
      { home, username: "login", gitUserName: () => "Ann Example\n" },
    );
    expect(got.home).toBe(home);
    expect(got.names.sort()).toEqual(
      ["Ann", "Example", "Nickname", path.basename(home), "login"].sort(),
    );
  });

  it("leaves out name words under three letters", () => {
    const home = fakeHome({});
    const got = personalValuesFromHost({}, { home, username: "login", gitUserName: () => "Al B" });
    expect(got.names).not.toContain("Al");
    expect(got.names).not.toContain("B");
  });

  it("reads an absent identity file as no email", () => {
    expect(accountEmailIn(path.join(fakeHome({}), "missing.json"))).toBeNull();
  });

  it("refuses an identity file that does not parse", () => {
    const home = fakeHome({ ".claude.json": "{not json" });
    expect(() => accountEmailIn(path.join(home, ".claude.json"))).toThrow();
  });
});

describe("the capture writes scrub personal values", () => {
  const personal = { home: "/Users/ann", emails: ["ann@host.test"], names: ["ann"] };

  it("copyTreeAnonymized scrubs file names and contents", () => {
    const base = realpathSync(mkdtempSync(path.join(tmpdir(), "capture-copy-")));
    const from = path.join(base, "from", "-Users-ann-proj");
    mkdirSync(from, { recursive: true });
    writeFileSync(
      path.join(from, "s.jsonl"),
      `${JSON.stringify({ cwd: "/Users/ann/proj", email: "ann@host.test", who: "Ann" })}\n`,
      "utf8",
    );
    copyTreeAnonymized(path.join(base, "from"), path.join(base, "to"), { unparsed: [] }, personal);
    const written = readFileSync(path.join(base, "to", "--proj", "s.jsonl"), "utf8");
    expect(JSON.parse(written)).toEqual({ cwd: "~/proj", email: "person1@example.com", who: "Someone" });
  });

  it("anonymizeMeta scrubs keys and strings and nothing else", () => {
    expect(anonymizeMeta({ "/Users/ann/x": 1, note: "Ann ran it", n: 2 }, personal)).toEqual({
      "~/x": 1,
      note: "Someone ran it",
      n: 2,
    });
  });

  const missing = [
    ["copyTreeAnonymized", () => copyTreeAnonymized("/nowhere", "/nowhere", { unparsed: [] })],
    ["mergeTreeAnonymized", () => mergeTreeAnonymized("/nowhere", "/nowhere", { unparsed: [] })],
    [
      "lateReclaimSlug",
      () => lateReclaimSlug({ accountRoot: "/nowhere", slug: "s", captureDir: "/nowhere", report: { unparsed: [] } }),
    ],
    ["anonymizeMeta", () => anonymizeMeta({})],
  ];
  for (const [site, call] of missing) {
    it(`${site} refuses to run without personal values`, () => {
      expect(call).toThrow(`${site} was called without the personal values to scrub`);
    });
  }
});
