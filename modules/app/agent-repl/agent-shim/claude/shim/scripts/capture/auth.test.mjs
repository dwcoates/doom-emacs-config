/**
 * Tests for the capture harness's authentication mechanisms.
 *
 * The case that matters most is the one that produced the first failed run:
 * NO mechanism in effect must refuse before any vendor call, not run 75
 * scenarios logged out.
 */
import { mkdtempSync, mkdirSync, readFileSync, writeFileSync, existsSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import {
  ACCOUNT_FIELDS,
  AUTH_CONFIG_ROOT,
  AUTH_INHERITED_TOKEN,
  AUTH_SEED_CREDENTIALS,
  AuthRefusedError,
  CREDENTIALS_FILE,
  TOKEN_ENV_VARS,
  accountFieldsOf,
  defaultAccountRoot,
  findClaudeJson,
  inheritedTokenVar,
  prepareAccountRoot,
  preflightAuth,
  reclaimScratchProject,
  releaseScratchProject,
  resolveAuthMode,
  accountRootEnvFor,
  sdkEnvFor,
  wipeSeededCredentials,
} from "./auth.mjs";

const scratches = [];

/** A throwaway directory, cleaned up after the file's tests. */
function scratch() {
  const dir = mkdtempSync(path.join(tmpdir(), "capture-auth-test-"));
  scratches.push(dir);
  return dir;
}

afterEach(() => {
  // Directories live in the OS temp dir; leaving them is harmless, but the
  // reclaim tests assert on deletion so each test builds its own.
});

/** An account root that looks logged in. */
function loggedInRoot({ credentialBytes = "{\"token\":\"x\"}", withClaudeJson = true } = {}) {
  const root = scratch();
  writeFileSync(path.join(root, CREDENTIALS_FILE), credentialBytes, "utf8");
  if (withClaudeJson) {
    writeFileSync(
      path.join(root, ".claude.json"),
      JSON.stringify({
        oauthAccount: { emailAddress: "operator@example.invalid" },
        hasCompletedOnboarding: true,
        projects: { "/some/private/path": { history: ["secret prompt"] } },
        numStartups: 412,
      }),
      "utf8",
    );
  }
  return root;
}

describe("resolveAuthMode — the failure that produced the empty run", () => {
  it("REFUSES when no mechanism is in effect", () => {
    expect(() => resolveAuthMode({}, {})).toThrow(AuthRefusedError);
  });

  it("explains the logged-out failure it is preventing", () => {
    expect(() => resolveAuthMode({}, {})).toThrow(/Not logged in/);
  });

  it("names all three mechanisms in the refusal so the operator can pick one", () => {
    let message = "";
    try {
      resolveAuthMode({}, {});
    } catch (err) {
      message = err.message;
    }
    expect(message).toContain("--config-root");
    expect(message).toContain("--seed-credentials");
    expect(message).toContain("CLAUDE_CODE_OAUTH_TOKEN");
  });
});

describe("resolveAuthMode — exactly one mechanism", () => {
  it("resolves --config-root", () => {
    expect(resolveAuthMode({ configRoot: "/tmp/root" }, {}).mode).toBe(AUTH_CONFIG_ROOT);
  });

  it("resolves --seed-credentials", () => {
    const root = loggedInRoot();
    expect(resolveAuthMode({ seedCredentials: true, credentialsFrom: root }, {}).mode).toBe(
      AUTH_SEED_CREDENTIALS,
    );
  });

  it("resolves an inherited OAuth token", () => {
    expect(resolveAuthMode({}, { CLAUDE_CODE_OAUTH_TOKEN: "t" }).mode).toBe(AUTH_INHERITED_TOKEN);
  });

  it("resolves an inherited API key", () => {
    expect(resolveAuthMode({}, { ANTHROPIC_API_KEY: "k" }).mode).toBe(AUTH_INHERITED_TOKEN);
  });

  it("REFUSES --config-root together with an inherited token as ambiguous", () => {
    expect(() => resolveAuthMode({ configRoot: "/tmp/r" }, { ANTHROPIC_API_KEY: "k" })).toThrow(
      /mechanisms are in effect at once/,
    );
  });

  it("REFUSES both flags together as ambiguous", () => {
    expect(() => resolveAuthMode({ configRoot: "/tmp/r", seedCredentials: true }, {})).toThrow(
      AuthRefusedError,
    );
  });

  it("tells the operator which variable to unset when a token caused the ambiguity", () => {
    expect(() => resolveAuthMode({ seedCredentials: true }, { ANTHROPIC_API_KEY: "k" })).toThrow(
      /Unset ANTHROPIC_API_KEY/,
    );
  });

  it("treats an EMPTY token variable as not in effect", () => {
    expect(() => resolveAuthMode({}, { ANTHROPIC_API_KEY: "" })).toThrow(AuthRefusedError);
  });

  it("prefers CLAUDE_CODE_OAUTH_TOKEN when both are set", () => {
    expect(inheritedTokenVar({ CLAUDE_CODE_OAUTH_TOKEN: "a", ANTHROPIC_API_KEY: "b" })).toBe(
      "CLAUDE_CODE_OAUTH_TOKEN",
    );
  });

  it("declares both token variables", () => {
    expect(TOKEN_ENV_VARS).toEqual(["CLAUDE_CODE_OAUTH_TOKEN", "ANTHROPIC_API_KEY"]);
  });
});

describe("preflightAuth", () => {
  it("accepts an inherited token without touching the filesystem", () => {
    expect(preflightAuth({ mode: AUTH_INHERITED_TOKEN, tokenVar: "ANTHROPIC_API_KEY" }).source).toBe(
      "$ANTHROPIC_API_KEY",
    );
  });

  it("refuses a --config-root that does not exist", () => {
    expect(() =>
      preflightAuth({ mode: AUTH_CONFIG_ROOT, configRoot: "/no/such/root" }),
    ).toThrow(/does not exist/);
  });

  it("refuses a --config-root that is a file", () => {
    const root = scratch();
    const file = path.join(root, "afile");
    writeFileSync(file, "x", "utf8");
    expect(() => preflightAuth({ mode: AUTH_CONFIG_ROOT, configRoot: file })).toThrow(
      /not a directory/,
    );
  });

  it("accepts an existing --config-root", () => {
    const root = loggedInRoot();
    expect(preflightAuth({ mode: AUTH_CONFIG_ROOT, configRoot: root }).source).toBe(root);
  });

  it("refuses --seed-credentials when there is no credentials file", () => {
    const root = scratch();
    expect(() =>
      preflightAuth({ mode: AUTH_SEED_CREDENTIALS, credentialsFrom: root, claudeJson: null }),
    ).toThrow(/found no \.credentials\.json/);
  });

  it("refuses a ZERO-BYTE credentials file, the real state of this machine's ~/.claude", () => {
    const root = loggedInRoot({ credentialBytes: "" });
    expect(() =>
      preflightAuth({
        mode: AUTH_SEED_CREDENTIALS,
        credentialsFrom: root,
        claudeJson: path.join(root, ".claude.json"),
      }),
    ).toThrow(/ZERO BYTES/);
  });

  it("explains the macOS Keychain when the credentials file is empty", () => {
    const root = loggedInRoot({ credentialBytes: "" });
    expect(() =>
      preflightAuth({
        mode: AUTH_SEED_CREDENTIALS,
        credentialsFrom: root,
        claudeJson: path.join(root, ".claude.json"),
      }),
    ).toThrow(/Keychain/);
  });

  it("refuses --seed-credentials with no .claude.json to onboard from", () => {
    const root = loggedInRoot({ withClaudeJson: false });
    expect(() =>
      preflightAuth({ mode: AUTH_SEED_CREDENTIALS, credentialsFrom: root, claudeJson: null }),
    ).toThrow(/no \.claude\.json/);
  });

  it("accepts a fully populated seed source", () => {
    const root = loggedInRoot();
    expect(
      preflightAuth({
        mode: AUTH_SEED_CREDENTIALS,
        credentialsFrom: root,
        claudeJson: path.join(root, ".claude.json"),
      }).source,
    ).toBe(root);
  });
});

describe("accountFieldsOf — an allowlist, not a copy", () => {
  it("carries the account identity across", () => {
    const fields = accountFieldsOf(JSON.stringify({ oauthAccount: { emailAddress: "a@b.c" } }));
    expect(fields.oauthAccount).toEqual({ emailAddress: "a@b.c" });
  });

  it("carries the onboarding state across", () => {
    expect(accountFieldsOf(JSON.stringify({ hasCompletedOnboarding: true })).hasCompletedOnboarding).toBe(
      true,
    );
  });

  it("does NOT carry the operator's project history", () => {
    const fields = accountFieldsOf(
      JSON.stringify({ oauthAccount: {}, projects: { "/private": { history: ["secret"] } } }),
    );
    expect(fields.projects).toBeUndefined();
  });

  it("does NOT carry telemetry counters", () => {
    expect(accountFieldsOf(JSON.stringify({ numStartups: 412 })).numStartups).toBeUndefined();
  });

  it("omits an allowlisted field that is simply absent", () => {
    expect(Object.keys(accountFieldsOf("{}"))).toEqual([]);
  });

  it("declares oauthAccount and hasCompletedOnboarding in the allowlist", () => {
    expect(ACCOUNT_FIELDS).toContain("oauthAccount");
    expect(ACCOUNT_FIELDS).toContain("hasCompletedOnboarding");
  });
});

describe("prepareAccountRoot", () => {
  it("uses the operator's real root under --config-root", () => {
    const root = loggedInRoot();
    const scratchDir = path.join(scratch(), "config");
    expect(prepareAccountRoot({ mode: AUTH_CONFIG_ROOT, configRoot: root }, scratchDir)).toBe(root);
  });

  it("does not create the scratch config dir under --config-root", () => {
    const root = loggedInRoot();
    const scratchDir = path.join(scratch(), "config");
    prepareAccountRoot({ mode: AUTH_CONFIG_ROOT, configRoot: root }, scratchDir);
    expect(existsSync(scratchDir)).toBe(false);
  });

  it("uses the scratch root under an inherited token", () => {
    const scratchDir = path.join(scratch(), "config");
    expect(prepareAccountRoot({ mode: AUTH_INHERITED_TOKEN, tokenVar: "X" }, scratchDir)).toBe(
      scratchDir,
    );
  });

  it("copies the credential file into the scratch root when seeding", () => {
    const from = loggedInRoot();
    const scratchDir = path.join(scratch(), "config");
    prepareAccountRoot(
      { mode: AUTH_SEED_CREDENTIALS, credentialsFrom: from, claudeJson: path.join(from, ".claude.json") },
      scratchDir,
    );
    expect(readFileSync(path.join(scratchDir, CREDENTIALS_FILE), "utf8")).toBe('{"token":"x"}');
  });

  it("writes only the allowlisted account fields into the scratch .claude.json", () => {
    const from = loggedInRoot();
    const scratchDir = path.join(scratch(), "config");
    prepareAccountRoot(
      { mode: AUTH_SEED_CREDENTIALS, credentialsFrom: from, claudeJson: path.join(from, ".claude.json") },
      scratchDir,
    );
    const seeded = JSON.parse(readFileSync(path.join(scratchDir, ".claude.json"), "utf8"));
    expect(seeded.projects).toBeUndefined();
    expect(seeded.hasCompletedOnboarding).toBe(true);
  });
});

describe("wipeSeededCredentials", () => {
  it("removes the seeded credential so it cannot reach the capture directory", () => {
    const from = loggedInRoot();
    const scratchDir = path.join(scratch(), "config");
    const resolution = {
      mode: AUTH_SEED_CREDENTIALS,
      credentialsFrom: from,
      claudeJson: path.join(from, ".claude.json"),
    };
    prepareAccountRoot(resolution, scratchDir);
    wipeSeededCredentials(resolution, scratchDir);
    expect(existsSync(path.join(scratchDir, CREDENTIALS_FILE))).toBe(false);
  });

  it("removes the seeded .claude.json too", () => {
    const from = loggedInRoot();
    const scratchDir = path.join(scratch(), "config");
    const resolution = {
      mode: AUTH_SEED_CREDENTIALS,
      credentialsFrom: from,
      claudeJson: path.join(from, ".claude.json"),
    };
    prepareAccountRoot(resolution, scratchDir);
    wipeSeededCredentials(resolution, scratchDir);
    expect(existsSync(path.join(scratchDir, ".claude.json"))).toBe(false);
  });

  it("is a no-op for a --config-root run, which must not touch the real root", () => {
    const root = loggedInRoot();
    wipeSeededCredentials({ mode: AUTH_CONFIG_ROOT, configRoot: root }, root);
    expect(existsSync(path.join(root, CREDENTIALS_FILE))).toBe(true);
  });
});

describe("reclaim/release — leaving the operator's account as found", () => {
  it("finds this scenario's own project directory in the real root", () => {
    const root = loggedInRoot();
    const slug = "-tmp-scratch-cwd";
    mkdirSync(path.join(root, "projects", slug), { recursive: true });
    expect(reclaimScratchProject({ mode: AUTH_CONFIG_ROOT }, root, slug)).toBe(
      path.join(root, "projects", slug),
    );
  });

  it("returns null when the vendor wrote nothing", () => {
    const root = loggedInRoot();
    expect(reclaimScratchProject({ mode: AUTH_CONFIG_ROOT }, root, "-nothing")).toBeNull();
  });

  it("reclaims nothing in a seeded run, whose root is already disposable", () => {
    expect(reclaimScratchProject({ mode: AUTH_SEED_CREDENTIALS }, "/x", "-s")).toBeNull();
  });

  it("deletes the reclaimed directory from the operator's root", () => {
    const root = loggedInRoot();
    const slug = "-tmp-scratch-cwd";
    const dir = path.join(root, "projects", slug);
    mkdirSync(dir, { recursive: true });
    releaseScratchProject(dir);
    expect(existsSync(dir)).toBe(false);
  });

  it("leaves the operator's OTHER projects untouched", () => {
    const root = loggedInRoot();
    mkdirSync(path.join(root, "projects", "-real-project"), { recursive: true });
    const dir = path.join(root, "projects", "-tmp-scratch");
    mkdirSync(dir, { recursive: true });
    releaseScratchProject(dir);
    expect(existsSync(path.join(root, "projects", "-real-project"))).toBe(true);
  });

  it("tolerates releasing nothing", () => {
    expect(() => releaseScratchProject(null)).not.toThrow();
  });
});

describe("sdkEnvFor", () => {
  it("passes an inherited token through explicitly", () => {
    expect(sdkEnvFor({ mode: AUTH_INHERITED_TOKEN, tokenVar: "ANTHROPIC_API_KEY" }, { ANTHROPIC_API_KEY: "k" })).toEqual(
      { ANTHROPIC_API_KEY: "k" },
    );
  });

  it("adds nothing for a config-root run", () => {
    expect(sdkEnvFor({ mode: AUTH_CONFIG_ROOT }, {})).toEqual({});
  });

  it("adds nothing for a seeded run", () => {
    expect(sdkEnvFor({ mode: AUTH_SEED_CREDENTIALS }, {})).toEqual({});
  });
});

describe("accountRootEnvFor", () => {
  it("hands a config-root run at the default root NO CLAUDE_CONFIG_DIR", () => {
    const home = scratch();
    const root = path.join(home, ".claude");
    const env = accountRootEnvFor({ mode: AUTH_CONFIG_ROOT }, root, { CLAUDE_CONFIG_DIR: root, KEEP: "1" }, home);
    expect(env).toEqual({ KEEP: "1" });
  });

  it("removes an inherited CLAUDE_CONFIG_DIR at the default root even when it names another root", () => {
    const home = scratch();
    const root = path.join(home, ".claude");
    const env = accountRootEnvFor({ mode: AUTH_CONFIG_ROOT }, root, { CLAUDE_CONFIG_DIR: "/elsewhere" }, home);
    expect("CLAUDE_CONFIG_DIR" in env).toBe(false);
  });

  it("names a config-root run at a non-default root explicitly", () => {
    const home = scratch();
    const root = path.join(home, ".claude-other");
    const env = accountRootEnvFor({ mode: AUTH_CONFIG_ROOT }, root, {}, home);
    expect(env).toEqual({ CLAUDE_CONFIG_DIR: root });
  });

  it("names a seeded scratch root explicitly", () => {
    const home = scratch();
    const dir = scratch();
    expect(accountRootEnvFor({ mode: AUTH_SEED_CREDENTIALS }, dir, {}, home)).toEqual({ CLAUDE_CONFIG_DIR: dir });
  });

  it("names an inherited-token scratch root explicitly", () => {
    const home = scratch();
    const dir = scratch();
    expect(accountRootEnvFor({ mode: AUTH_INHERITED_TOKEN, tokenVar: "ANTHROPIC_API_KEY" }, dir, {}, home)).toEqual({
      CLAUDE_CONFIG_DIR: dir,
    });
  });

  it("does not mutate the environment it was given", () => {
    const home = scratch();
    const given = { CLAUDE_CONFIG_DIR: "/x" };
    accountRootEnvFor({ mode: AUTH_CONFIG_ROOT }, path.join(home, ".claude"), given, home);
    expect(given).toEqual({ CLAUDE_CONFIG_DIR: "/x" });
  });
});

describe("locating .claude.json", () => {
  it("prefers the copy inside the account root", () => {
    const root = loggedInRoot();
    expect(findClaudeJson(root, "/nonexistent-home")).toBe(path.join(root, ".claude.json"));
  });

  it("falls back to the one beside the default root", () => {
    const home = scratch();
    const root = path.join(home, ".claude");
    mkdirSync(root, { recursive: true });
    writeFileSync(path.join(home, ".claude.json"), "{}", "utf8");
    expect(findClaudeJson(root, home)).toBe(path.join(home, ".claude.json"));
  });

  it("reports absence rather than guessing", () => {
    expect(findClaudeJson(scratch(), "/nonexistent-home")).toBeNull();
  });

  it("derives the default account root from the home directory", () => {
    expect(defaultAccountRoot("/home/x")).toBe(path.join("/home/x", ".claude"));
  });
});
