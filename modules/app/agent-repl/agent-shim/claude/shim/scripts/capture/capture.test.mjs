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
import path from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import { TOKEN_ENV_VARS } from "./auth.mjs";
import {
  CAPTURE_FLAG,
  CaptureRefusedError,
  EXIT_REFUSED,
  FORBID_VENDOR_CALLS_ENV,
  answersFor,
  assertCaptureAuthorized,
  createInputChannel,
  cwdSlug,
  loadPrompts,
  messageMatches,
  parseArgv,
  patternMatches,
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

  it("gives every scenario either a prompt or a manual procedure", () => {
    const doc = loadPrompts(path.join(HERE, "prompts.json"));
    const orphans = doc.scenarios.filter((s) => !s.prompt && !s.manual);
    expect(orphans).toEqual([]);
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

describe("cwdSlug", () => {
  it("replaces every slash and dot, matching the observed vendor spelling", () => {
    expect(cwdSlug("/Users/x/.config/y")).toBe("-Users-x--config-y");
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
