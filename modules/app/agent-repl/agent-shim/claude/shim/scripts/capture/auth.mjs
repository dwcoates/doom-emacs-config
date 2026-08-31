/**
 * HOW THE CAPTURE RUN AUTHENTICATES.
 *
 * WHY THIS MODULE EXISTS: the first real capture run produced nothing. Every
 * scenario got a fresh EMPTY `CLAUDE_CONFIG_DIR` — excellent for isolation,
 * fatal for authentication — so the vendor started logged out and answered
 * "Not logged in · Please run /login" in 33 ms at zero cost, with
 * `apiKeySource: "none"`. Isolation and authentication were in direct conflict
 * and nothing had ever chosen between them.
 *
 * THE RESOLUTION: the scratch CWD stays scratch in every mode — a capture must
 * never run against the operator's real working tree. Only the ACCOUNT ROOT
 * varies, and exactly one of three mechanisms supplies it:
 *
 *   --config-root <dir>   Use an existing account root AS the config dir.
 *                         PRODUCTION-FAITHFUL: the real shim runs against the
 *                         real root, so this is the only mode whose captures
 *                         carry the settings, hooks and CLAUDE.md a real
 *                         session has. The cost is that the vendor writes the
 *                         scenario's transcripts into that root, so the
 *                         harness harvests them into the capture and then
 *                         DELETES them, leaving the account as found.
 *
 *   --seed-credentials    Keep a fresh scratch root, but copy the credential
 *                         and account/onboarding state into it. Maximum
 *                         isolation; the captured session has none of the
 *                         operator's settings, so the vendor emits no
 *                         policy-denial messages and loads no CLAUDE.md.
 *
 *   inherited token       CLAUDE_CODE_OAUTH_TOKEN or ANTHROPIC_API_KEY already
 *                         in the environment, passed through to the SDK. A
 *                         fresh scratch root needs no credential file at all.
 *
 * EXACTLY ONE, ALWAYS, CHECKED BEFORE THE FIRST SCENARIO. Two mechanisms is
 * ambiguous (which one actually authenticated the capture? the golden would not
 * say), zero is the failure that produced the empty run. The preflight resolves
 * and VERIFIES the source before any vendor call, because discovering this
 * after a 75-scenario run is how the first attempt was wasted.
 *
 * Node built-ins only.
 */

import { copyFileSync, existsSync, mkdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";

/** The three mechanisms, spelled once. */
export const AUTH_CONFIG_ROOT = "config_root";
export const AUTH_SEED_CREDENTIALS = "seed_credentials";
export const AUTH_INHERITED_TOKEN = "inherited_token";

/**
 * Environment variables the SDK authenticates from directly, in priority order.
 * Present-and-non-empty counts as a mechanism in effect.
 */
export const TOKEN_ENV_VARS = ["CLAUDE_CODE_OAUTH_TOKEN", "ANTHROPIC_API_KEY"];

/** The credential file inside an account root. */
export const CREDENTIALS_FILE = ".credentials.json";

/**
 * The `.claude.json` fields `--seed-credentials` carries across.
 *
 * An ALLOWLIST, not a copy: `.claude.json` also holds the operator's entire
 * project history, MCP connections and usage telemetry, none of which belongs
 * in a capture. Only the fields that make the scratch root a logged-in,
 * past-onboarding account are taken.
 */
export const ACCOUNT_FIELDS = [
  "oauthAccount",
  "userID",
  "hasCompletedOnboarding",
  "lastOnboardingVersion",
  "customApiKeyResponses",
  "firstStartTime",
  "installMethod",
  "subscriptionNoticeCount",
  "hasAvailableSubscription",
];

/**
 * Thrown when authentication is absent or ambiguous. A refusal, not a failure:
 * the run exits before touching the vendor.
 */
export class AuthRefusedError extends Error {
  constructor(message) {
    super(`capture.mjs REFUSED (authentication): ${message}`);
    this.name = "AuthRefusedError";
  }
}

/** The token variable actually set in this environment, or null. */
export function inheritedTokenVar(env) {
  return TOKEN_ENV_VARS.find((name) => {
    const value = env[name];
    return typeof value === "string" && value !== "";
  }) ?? null;
}

/** The operator's default account root. */
export function defaultAccountRoot(home = homedir()) {
  return path.join(home, ".claude");
}

/**
 * Where `.claude.json` lives for a given account root.
 *
 * With `CLAUDE_CONFIG_DIR` set the vendor keeps it INSIDE the root; for the
 * default `~/.claude` root it sits beside it at `~/.claude.json`. Both are
 * tried rather than guessed, because getting it wrong seeds an onboarding-
 * incomplete account that fails in a different and more confusing way.
 */
export function findClaudeJson(root, home = homedir()) {
  const inside = path.join(root, ".claude.json");
  if (existsSync(inside)) return inside;
  const beside = path.join(home, ".claude.json");
  if (existsSync(beside)) return beside;
  return null;
}

/**
 * Decide which mechanism is in effect. Throws when none or several are.
 *
 * Takes `env` and the parsed options rather than reading globals, so every arm
 * is exercised in-process by the unit tests.
 */
export function resolveAuthMode(opts, env, home = homedir()) {
  const tokenVar = inheritedTokenVar(env);
  const inEffect = [];
  if (typeof opts.configRoot === "string" && opts.configRoot !== "") {
    inEffect.push(AUTH_CONFIG_ROOT);
  }
  if (opts.seedCredentials === true) inEffect.push(AUTH_SEED_CREDENTIALS);
  if (tokenVar !== null) inEffect.push(AUTH_INHERITED_TOKEN);

  if (inEffect.length === 0) {
    throw new AuthRefusedError(
      "no authentication mechanism is in effect, so every scenario would run logged out " +
        '(the first real run captured "Not logged in · Please run /login" for exactly this reason). ' +
        `Pass --config-root <dir> to run against an existing account root, or --seed-credentials to ` +
        `copy the operator's credentials into each scratch root, or export one of ` +
        `${TOKEN_ENV_VARS.join(" / ")}.`,
    );
  }
  if (inEffect.length > 1) {
    throw new AuthRefusedError(
      `${inEffect.length} authentication mechanisms are in effect at once (${inEffect.join(", ")}). ` +
        "Exactly one must be, or the capture cannot say which credential produced it. " +
        (inEffect.includes(AUTH_INHERITED_TOKEN)
          ? `Unset ${tokenVar}, or drop the flag.`
          : "Drop one of the flags."),
    );
  }

  const mode = inEffect[0];
  if (mode === AUTH_CONFIG_ROOT) {
    return { mode, configRoot: path.resolve(opts.configRoot) };
  }
  if (mode === AUTH_SEED_CREDENTIALS) {
    const from = path.resolve(opts.credentialsFrom ?? defaultAccountRoot(home));
    return { mode, credentialsFrom: from, claudeJson: findClaudeJson(from, home) };
  }
  return { mode, tokenVar };
}

/**
 * Verify the resolved mechanism can actually authenticate, BEFORE any scenario.
 *
 * The empty-file check is not paranoia: on macOS the vendor keeps OAuth
 * credentials in the system Keychain and leaves `.credentials.json` as a
 * ZERO-BYTE file. Seeding that file copies nothing, and the run fails exactly
 * the way the first one did — so an empty credential file is refused here, by
 * name, with the Keychain explained.
 */
export function preflightAuth(resolution) {
  if (resolution.mode === AUTH_INHERITED_TOKEN) {
    return { mode: resolution.mode, source: `$${resolution.tokenVar}` };
  }

  if (resolution.mode === AUTH_CONFIG_ROOT) {
    if (!existsSync(resolution.configRoot)) {
      throw new AuthRefusedError(`--config-root ${resolution.configRoot} does not exist.`);
    }
    if (!statSync(resolution.configRoot).isDirectory()) {
      throw new AuthRefusedError(`--config-root ${resolution.configRoot} is not a directory.`);
    }
    return { mode: resolution.mode, source: resolution.configRoot };
  }

  const credentials = path.join(resolution.credentialsFrom, CREDENTIALS_FILE);
  if (!existsSync(credentials)) {
    throw new AuthRefusedError(
      `--seed-credentials found no ${CREDENTIALS_FILE} in ${resolution.credentialsFrom}.`,
    );
  }
  if (statSync(credentials).size === 0) {
    throw new AuthRefusedError(
      `${credentials} is ZERO BYTES, so seeding it would copy no credential and every ` +
        "scenario would run logged out. On macOS the vendor keeps OAuth credentials in the " +
        "system Keychain and leaves this file empty — --seed-credentials cannot work on such " +
        "a machine. Use --config-root <the account root> instead, or export " +
        `${TOKEN_ENV_VARS.join(" / ")}.`,
    );
  }
  if (resolution.claudeJson === null) {
    throw new AuthRefusedError(
      `--seed-credentials found no .claude.json for ${resolution.credentialsFrom}; the scratch ` +
        "root would be an un-onboarded account.",
    );
  }
  return { mode: resolution.mode, source: resolution.credentialsFrom };
}

/** The allowlisted account fields of a `.claude.json`, as an object. */
export function accountFieldsOf(claudeJsonText) {
  const parsed = JSON.parse(claudeJsonText);
  const out = {};
  for (const field of ACCOUNT_FIELDS) {
    if (Object.prototype.hasOwnProperty.call(parsed, field)) out[field] = parsed[field];
  }
  return out;
}

/**
 * Prepare one scenario's account root, returning the directory to hand the SDK
 * as `CLAUDE_CONFIG_DIR`.
 *
 * `scratchConfigDir` is this scenario's own fresh directory; it is used by
 * every mode except `--config-root`, which deliberately uses the real one.
 */
export function prepareAccountRoot(resolution, scratchConfigDir) {
  if (resolution.mode === AUTH_CONFIG_ROOT) return resolution.configRoot;

  mkdirSync(scratchConfigDir, { recursive: true });
  if (resolution.mode === AUTH_SEED_CREDENTIALS) {
    copyFileSync(
      path.join(resolution.credentialsFrom, CREDENTIALS_FILE),
      path.join(scratchConfigDir, CREDENTIALS_FILE),
    );
    writeFileSync(
      path.join(scratchConfigDir, ".claude.json"),
      `${JSON.stringify(accountFieldsOf(readFileSync(resolution.claudeJson, "utf8")), null, 2)}\n`,
      "utf8",
    );
  }
  return scratchConfigDir;
}

/**
 * Remove seeded credential material from a scratch root once the scenario is
 * captured.
 *
 * The scratch root is copied INTO the capture, so a credential left in it would
 * be committed. The anonymizer would redact a recognizable token, but "the
 * walker probably catches it" is not a credential-handling policy — the file is
 * removed outright.
 */
export function wipeSeededCredentials(resolution, scratchConfigDir) {
  if (resolution.mode !== AUTH_SEED_CREDENTIALS) return;
  for (const name of [CREDENTIALS_FILE, ".claude.json"]) {
    rmSync(path.join(scratchConfigDir, name), { force: true });
  }
}

/**
 * Environment additions for the SDK child.
 *
 * The token is passed through EXPLICITLY rather than relied on via inheritance,
 * because the harness builds the child's `env` itself and an implicitly
 * inherited variable would vanish the moment that changed.
 */
export function sdkEnvFor(resolution, env) {
  if (resolution.mode !== AUTH_INHERITED_TOKEN) return {};
  return { [resolution.tokenVar]: env[resolution.tokenVar] };
}

/**
 * After a `--config-root` scenario: take the transcripts the vendor wrote into
 * the operator's real root, and DELETE them from it.
 *
 * The account must be left exactly as found. `projects/<cwd-slug>/` is safe to
 * remove wholesale because the slug encodes this scenario's own mkdtemp cwd,
 * which nothing else has ever written to — a path that cannot collide with any
 * real project of the operator's.
 */
export function reclaimScratchProject(resolution, accountRoot, slug) {
  if (resolution.mode !== AUTH_CONFIG_ROOT) return null;
  const projectDir = path.join(accountRoot, "projects", slug);
  return existsSync(projectDir) ? projectDir : null;
}

/** Delete a reclaimed scratch project directory from the operator's root. */
export function releaseScratchProject(projectDir) {
  if (projectDir === null) return;
  rmSync(projectDir, { recursive: true, force: true });
}
