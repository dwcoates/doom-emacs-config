/**
 * build-identity.ts — THE one reader of what this process IS, as
 * `conversation.v1.SessionRuntime` reports it on every SessionStarted and
 * `conversation.v1.SessionDiagnostics.shim_build` reports on every
 * WatchSession frame (including a session-less shim's opening one).
 *
 * Three values, three different sources, three different failure modes:
 *
 *   - `shim_build_sha`: read from the SPAWN ENV at runtime, never baked into
 *     the bundle. `bin/build-frontend.sh` computes the CONTENT HASH (lowercase
 *     hex SHA-256) of the built `dist/main.js` bundle's bytes once and writes
 *     it to `dist/.built-sha`; the daemon exports that same hash into the
 *     shim's environment when it spawns it, so stamp and reported identity
 *     agree by construction. The daemon's deploy compares the reported hash
 *     against a freshly built bundle's own hash and bounces a shim whose
 *     hash differs — which only works if the value follows the SPAWN, not the
 *     bundle: a shim outlives its daemon, and an esbuild `define` would make a
 *     survivor report the hash of whatever build it was bundled from
 *     regardless of what the daemon that started it said.
 *
 *     `src/main.ts` REFUSES TO START when the variable is unset, so production
 *     never reaches an empty identity. OUTSIDE A SERVING PROCESS — `tsc
 *     --noEmit`, vitest, a `node src/...` run — the env read yields undefined
 *     and that is reported as "" and NOT papered over: the daemon reads an
 *     empty identity as UNKNOWN, which is never a mismatch and therefore never
 *     a bounce. An honest unknown is correct; a fabricated sha would make the
 *     daemon bounce a healthy shim or refuse to bounce a stale one.
 *
 *   - `sdk_version`: the installed `@anthropic-ai/claude-agent-sdk` package
 *     version, read off its own package.json. Always knowable.
 *
 *   - `agent_binary_version`: the version of the Claude Code binary the SDK
 *     bundles. The SDK ships a `manifest.json` declaring it, and that is the
 *     answer whenever it is present. When it is not, the value exists only in
 *     the running session's `system:init` (`claude_code_version`), which
 *     arrives AFTER StartSession begins — so the field is OPTIONAL IN MEMORY
 *     and the engine calls {@link recordAgentBinaryVersion} the moment init
 *     lands. There is deliberately NO "unknown" sentinel: presence, never
 *     sentinels, and `SessionRuntime.agent_binary_version` is non-optional on
 *     the wire, so {@link requireSessionRuntime} refuses to build the message
 *     until the value is real.
 */
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import path from "node:path";
import { bindLog } from "./log.js";

const LOGGER = bindLog({ component: "shim-build-identity", operation: "shim.build-identity" });

/**
 * The build identity the process was SPAWNED with, or "" when nothing set it.
 *
 * A live read of the environment on every call, deliberately: nothing rewrites
 * this variable, but reading it at call time is what keeps the value the
 * daemon exported authoritative rather than anything a build step decided.
 */
export function shimBuildSha(): string {
  return process.env.SHIM_BUILD_SHA ?? "";
}

/**
 * The directory the installed SDK package occupies.
 *
 * Resolved through the package's MAIN entry rather than a subpath: the SDK's
 * `exports` map publishes neither `./package.json` nor `./manifest.json`, so a
 * direct subpath resolve fails with ERR_PACKAGE_PATH_NOT_EXPORTED. Resolving
 * the entry and taking its directory reads the same files without asking the
 * exports map for permission — and resolving is not IMPORTING, so the vendor
 * guard is not involved: nothing here loads or runs vendor code.
 */
function sdkPackageDir(): string {
  const require = createRequire(import.meta.url);
  return path.dirname(require.resolve("@anthropic-ai/claude-agent-sdk"));
}

function readJsonField(file: string, field: string): string | undefined {
  let raw: string;
  try {
    raw = readFileSync(file, "utf8");
  } catch (err) {
    // warn: a defect because an unreadable package manifest leaves runtime identity incomplete.
    LOGGER.warn({ file, cause: err }, `build identity: ${file} is unreadable`);
    return undefined;
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(raw);
  } catch (err) {
    // warn: a defect because malformed package metadata leaves runtime identity incomplete.
    LOGGER.warn({ file, cause: err }, `build identity: ${file} is not valid JSON`);
    return undefined;
  }
  if (typeof parsed !== "object" || parsed === null) {
    // warn: a defect because non-object package metadata leaves runtime identity incomplete.
    LOGGER.warn({ file }, `build identity: ${file} is not a JSON object`);
    return undefined;
  }
  const value = (parsed as Record<string, unknown>)[field];
  if (typeof value !== "string" || value === "") {
    // warn: a defect because required package metadata is absent from runtime identity.
    LOGGER.warn({ file, field }, `build identity: ${file} declares no usable ${field}`);
    return undefined;
  }
  return value;
}

/**
 * The installed SDK's version.
 *
 * Unreadable is a LOUD failure rather than a shrug: `sdk_version` is
 * non-optional on `SessionRuntime`, the shim cannot run without the SDK it is
 * reporting, and a fabricated version would tell the daemon a lie it cannot
 * detect.
 */
export function sdkVersion(): string {
  const file = path.join(sdkPackageDir(), "package.json");
  const version = readJsonField(file, "version");
  if (version === undefined) {
    throw new Error(`shim build identity: cannot read the SDK version from ${file}`);
  }
  return version;
}

/**
 * The version of the agent binary the SDK bundles, as the SDK's own manifest
 * declares it, or absence when it declares none.
 *
 * Absence is normal-and-recoverable, not an error: the session's `system:init`
 * carries `claude_code_version` and the engine records it then.
 */
export function bundledAgentBinaryVersion(): string | undefined {
  return readJsonField(path.join(sdkPackageDir(), "manifest.json"), "version");
}

/**
 * The agent binary version once anything has established it. Starts from the
 * SDK's manifest and is overwritten only by the running session's own report.
 */
let agentBinaryVersion: string | undefined = undefined;
let agentBinaryVersionSource: "manifest" | "session_init" | undefined = undefined;

/**
 * Record the agent binary version the live session reports (`system:init`'s
 * `claude_code_version`).
 *
 * THE SESSION'S REPORT WINS over the manifest: the manifest says what was
 * packaged, the session says what is actually running, and observed behavior
 * outranks a declaration. Called by the engine at StartSession.
 */
export function recordAgentBinaryVersion(version: string): void {
  if (typeof version !== "string" || version === "") {
    throw new Error("shim build identity: the session reported an empty agent binary version");
  }
  if (agentBinaryVersion === version && agentBinaryVersionSource === "session_init") return;
  LOGGER.debug(
    { previous: agentBinaryVersion ?? "", previous_source: agentBinaryVersionSource ?? "", version },
    "recording the agent binary version the live session reports",
  );
  agentBinaryVersion = version;
  agentBinaryVersionSource = "session_init";
}

/** Forget the recorded agent binary version. Test seam; never called in production. */
export function resetAgentBinaryVersionForTest(): void {
  agentBinaryVersion = undefined;
  agentBinaryVersionSource = undefined;
}

/** What is known about this process right now; the binary version may be absent. */
interface ShimRuntimeIdentity {
  readonly shimBuildSha: string;
  readonly sdkVersion: string;
  readonly agentBinaryVersion?: string;
}

/** The identity as currently known, consulting the SDK manifest when nothing was recorded. */
export function runtimeIdentity(): ShimRuntimeIdentity {
  if (agentBinaryVersion === undefined) {
    const bundled = bundledAgentBinaryVersion();
    if (bundled !== undefined) {
      agentBinaryVersion = bundled;
      agentBinaryVersionSource = "manifest";
      LOGGER.debug({ version: bundled }, "adopted the agent binary version from the SDK's bundled manifest");
    }
  }
  return {
    shimBuildSha: shimBuildSha(),
    sdkVersion: sdkVersion(),
    ...(agentBinaryVersion === undefined ? {} : { agentBinaryVersion }),
  };
}

/**
 * The identity with EVERY field populated, for building `SessionRuntime`.
 *
 * Throws while the agent binary version is still unknown. That is the correct
 * answer under the presence rule: `SessionRuntime.agent_binary_version` is
 * non-optional, so there is no legal message to send yet, and "unknown" is a
 * sentinel the contract forbids. The caller waits for `system:init`.
 */
export function requireSessionRuntime(): Required<ShimRuntimeIdentity> {
  const identity = runtimeIdentity();
  if (identity.agentBinaryVersion === undefined) {
    throw new Error(
      "shim build identity: the agent binary version is not established yet (no SDK manifest and no " +
        "system:init report), so SessionRuntime cannot be populated — wait for the session's init",
    );
  }
  return identity as Required<ShimRuntimeIdentity>;
}
