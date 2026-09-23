/**
 * claude-shim — the entrypoint for one daemon-owned Claude session.
 *
 * # The spawn contract, whole
 *
 *   node dist/main.js --listen <uds> --store-socket <uds> --log-fd 3 [--fake]
 *   node dist/main.js --version
 *
 * NOTHING ELSE. Every legacy flag is gone, and an unrecognized one is a startup
 * FAILURE rather than a warning: a daemon that spawns a shim with a flag this
 * build does not understand is a version mismatch, and a shim that shrugged and
 * started anyway would run with the caller's intent silently discarded.
 *
 * Notably absent: `--session-id`, `--cwd`, `--model`, `--permission-mode`,
 * `--resume` and the rewind trio. SESSION FACTS TRAVEL ONLY IN `StartSession`
 * (cross-system contract), so a session's model, mode and vendor binding are
 * rpc arguments now, not spawn arguments. cwd is the process's own working
 * directory, which the daemon sets when it spawns us — passing it as a flag as
 * well gave two sources for one fact, able to disagree. `--claude-bin` is gone
 * with R12: the SDK's own bundled, pinned binary is the engine.
 *
 * # Startup order, and why it is this order
 *
 *   1. parse argv — cheapest, and `--version` must not touch anything;
 *   2. resolve and validate the environment;
 *   3. configure the log on fd 3, so everything after this point is recorded;
 *   4. take the WORKSPACE lock, keyed by cwd;
 *   5. bind the UDS and serve.
 *
 * The workspace lock comes BEFORE the socket because it is the claim that
 * matters: two shims over one workspace means two writers on one transcript, and
 * binding first would leave a window in which a duplicate is reachable. The
 * SESSION lock is NOT taken here — it is keyed by the vendor session id, which
 * does not exist until `StartSession` pre-mints it (fresh) or is handed it
 * (resume) — so `engine/session.ts` takes it, before the SDK is touched.
 *
 * # Signals
 *
 * SIGTERM is the ONE authorized process-level shutdown, and it takes the same
 * graceful path as `KillSession{force:true}`: the engine stands down (every
 * pending permission callback resolved, the query ended, every store write
 * acked) and then the process exits 0. It cannot be an rpc-only path because
 * the daemon may already be dead.
 *
 * SIGINT is REFUSED and logged at error. A shim may be spawned under an
 * attached terminal, and a Ctrl-C there must not end a turn the user is
 * watching.
 *
 * SIGUSR2 is the MANUAL KEEP-ALIVE RESET backdoor (ruled 2026-09-17): it
 * collapses every outstanding keep-alive turn back to the last real record,
 * through the same rewind the per-cycle keep-alive already performs
 * (`engine/session.ts`'s `resetKeepalives`; see `engine/keepalive.ts` for the
 * cadence and rewind this reuses). It is documented in AGENTS.md as the way an
 * operator reclaims context on a live session from the shell. Neither SIGTERM
 * nor SIGINT claims it, and it was free before this change.
 */
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import { realpathSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { configureLog } from "./log.js";
import { MAIN_LIFECYCLE_LOGGER, reportFatal } from "./fatal.js";
import { lockBinaryPath, lockDir, LOCK_DIR_ENV } from "./locks.js";
import { runtimeIdentity } from "./build-identity.js";
import { type Engine } from "./engine/engine.js";
import { createEngine, type CreateQuery, type QuerySpec, type SessionEngine } from "./engine/session.js";
import { createFold } from "./convert/fold.js";
import { createStoreClient } from "./store/client.js";
import {
  createPersistence,
  DEFAULT_RETRY_POLICY,
  type PersistenceRetryPolicy,
} from "./store/persistence.js";
import { createRealQuery, PER_TASK_STOP_AFFORDANCE } from "./sdk/real-query.js";
import { createFakeQuery } from "./fake/index.js";
import { ensureWorkspaceTrusted } from "./trust.js";
import { randomUUID } from "node:crypto";
import { shimRoutes } from "./service/routes.js";
import { serve, type ShimServer } from "./service/server.js";

export {
  MAIN_LIFECYCLE_OPERATION,
  MAIN_FATAL_OPERATION,
  MAIN_LIFECYCLE_LOGGER,
  reportFatal,
} from "./fatal.js";

// ---------------------------------------------------------------------------
// argv
// ---------------------------------------------------------------------------

/** The whole command line. */
export interface CliArgs {
  /** The unix socket to serve shim.v1 on. */
  readonly listen?: string;
  /** The store's unix socket. Falls back to AGENT_REPL_STORE_SOCKET. */
  readonly storeSocket?: string;
  /** The inherited, already-open durable log descriptor. Always 3. */
  readonly logFd?: 3;
  /** Drive the mocked vendor instead of the real SDK. */
  readonly fake: boolean;
  /** Print the version and exit, touching nothing. */
  readonly version: boolean;
}

/** The four accepted flags, plus --version. Anything else is a failure. */
export function parseArgs(argv: readonly string[]): CliArgs {
  let listen: string | undefined;
  let storeSocket: string | undefined;
  let logFd: 3 | undefined;
  let fake = false;
  let version = false;

  for (let index = 0; index < argv.length; index++) {
    const arg = argv[index];
    const next = (): string => {
      const value = argv[++index];
      if (value === undefined) throw new Error(`shim: missing value for ${String(arg)}`);
      return value;
    };
    switch (arg) {
      case "--listen":
        listen = next();
        break;
      case "--store-socket":
        storeSocket = next();
        break;
      case "--log-fd": {
        const value = next();
        // Only fd 3. The daemon inherits exactly one descriptor for the durable
        // sink, and accepting another number would let a caller point the record
        // at whatever happened to be open — including the stderr pipe whose
        // death this design exists to survive.
        if (value !== "3") {
          throw new Error(`shim: invalid --log-fd ${JSON.stringify(value)}; the durable sink is inherited fd 3`);
        }
        logFd = 3;
        break;
      }
      case "--fake":
        fake = true;
        break;
      case "--version":
        version = true;
        break;
      default:
        throw new Error(
          `shim: unknown argument ${JSON.stringify(String(arg))}; the spawn contract is ` +
            "--listen <uds> --store-socket <uds> --log-fd 3 [--fake] [--version]",
        );
    }
  }

  return {
    ...(listen === undefined ? {} : { listen }),
    ...(storeSocket === undefined ? {} : { storeSocket }),
    ...(logFd === undefined ? {} : { logFd }),
    fake,
    version,
  };
}

/** The flags a serving shim cannot start without. */
export function requireServingArgs(
  args: CliArgs,
): asserts args is CliArgs & { listen: string; logFd: 3 } {
  if (args.listen === undefined) throw new Error("shim: --listen <uds> is required");
  if (args.logFd === undefined) throw new Error("shim: --log-fd 3 is required");
}

// ---------------------------------------------------------------------------
// environment
// ---------------------------------------------------------------------------

/** The default state root, shared with every other agent-repl process. */
export const DEFAULT_STATE_DIR_NAME = ".claude-emacs";

/** The env var naming the store socket when `--store-socket` is absent. */
export const STORE_SOCKET_ENV = "AGENT_REPL_STORE_SOCKET";

/** The env var the daemon sets to prove it spawned us. */
export const OWNED_ENV = "AGENT_REPL_OWNED";

/**
 * The daemon's own correlation id for this shim, for LOGGING ONLY.
 *
 * It is never a session fact: `StartSession` remains the one carrier of those.
 * It exists so a daemon log line and a shim log line about the same host
 * session can be joined without either side inferring the other's identity.
 */
export const SESSION_ID_ENV = "AGENT_REPL_SESSION_ID";

/**
 * The keep-alive interval override, honored ONLY under `--fake`.
 *
 * The cadence is fifty-two minutes against subscription billing's ~1-hour
 * cache window (engine/keepalive.ts), and from outside the process there was
 * no way to make one fire — so the two
 * keep-alive obligations could only be declared, never tested. The override
 * exists for that and nothing else, which is why it is refused for a real
 * session: a production shim that took its cadence from the environment could
 * be told to hammer the vendor or to never beat at all.
 */
export const FAKE_KEEPALIVE_INTERVAL_ENV = "AGENT_REPL_FAKE_KEEPALIVE_INTERVAL_MS";

/**
 * Resolve one `--fake`-only millisecond override.
 *
 * The three overrides in this file differ only in which variable they read and
 * what they are called in the log, so the reading, the refusal and the
 * validation live here ONCE rather than drifting across three near-identical
 * copies.
 *
 * Answers `undefined` for "use the module constant" in every rejecting case.
 * An override outside `--fake`, or one that is not a positive whole number of
 * milliseconds, is IGNORED AND REPORTED rather than silently applied or
 * silently dropped.
 */
function resolveFakeMsOverride(
  env: NodeJS.ProcessEnv,
  fake: boolean,
  variable: string,
  outcomePrefix: string,
  subject: string,
): number | undefined {
  const raw = env[variable];
  if (raw === undefined || raw === "") return undefined;
  if (!fake) {
    MAIN_LIFECYCLE_LOGGER.debug(
      { env: variable, value: raw, outcome: `${outcomePrefix}_refused` },
      `the ${subject} override is honored only under --fake; ignoring it for this real session`,
    );
    return undefined;
  }
  const parsed = Number(raw);
  if (!Number.isInteger(parsed) || parsed <= 0) {
    MAIN_LIFECYCLE_LOGGER.debug(
      { env: variable, value: raw, outcome: `${outcomePrefix}_invalid` },
      `the ${subject} override is not a positive whole number of milliseconds; ignoring it`,
    );
    return undefined;
  }
  MAIN_LIFECYCLE_LOGGER.debug(
    { env: variable, value_ms: parsed, outcome: `${outcomePrefix}_applied` },
    `a fake session took its ${subject} from the environment`,
  );
  return parsed;
}

/**
 * Resolve the keep-alive interval for this process.
 *
 * Answers `undefined` for "use the module constant".
 */
export function resolveKeepaliveIntervalMs(
  env: NodeJS.ProcessEnv,
  fake: boolean,
): number | undefined {
  return resolveFakeMsOverride(
    env,
    fake,
    FAKE_KEEPALIVE_INTERVAL_ENV,
    "keepalive_override",
    "keep-alive interval",
  );
}

/**
 * The WATCHER CONCLUSION budget override, honored ONLY under `--fake`.
 *
 * The budget bounds how long a kill's teardown waits for one already-concluded
 * tail to actually end, and it is only ever SPENT by a consumer that stopped
 * pulling — which is exactly the shape of the forced-kill scenarios, and why
 * they were the two slowest tests in the suite. What they assert is that the
 * tail's terminal reaches the consumer BEFORE the process goes, which is an
 * ordering, not a duration.
 *
 * Refused for a real session: a production shim told to give up on a tail
 * immediately would cut streams exactly where a terminal was owed.
 */
export const FAKE_WATCHER_CONCLUSION_BUDGET_ENV = "AGENT_REPL_FAKE_WATCHER_CONCLUSION_BUDGET_MS";

/**
 * Resolve the watcher-conclusion budget for this process.
 *
 * Answers `undefined` for "use the engine's own module constant".
 */
export function resolveWatcherConclusionBudgetMs(
  env: NodeJS.ProcessEnv,
  fake: boolean,
): number | undefined {
  return resolveFakeMsOverride(
    env,
    fake,
    FAKE_WATCHER_CONCLUSION_BUDGET_ENV,
    "watcher_conclusion_budget_override",
    "watcher-conclusion budget",
  );
}

/**
 * The store retry BACKOFF SCHEDULE override, honored ONLY under `--fake`.
 *
 * Only the delays are overridable, and deliberately so. The contractual facts
 * about a store outage are the ones the outage scenarios assert: how many
 * attempts a batch gets, that the buffer is bounded, that exhaustion drops
 * LOUDLY and names the lost keys, and that order survives. None of those is a
 * function of how long the process idles between attempts, so `bufferCapacity`
 * and `maxAttempts` stay fixed at {@link DEFAULT_RETRY_POLICY} and cannot be
 * reached from the environment at all — an override able to shorten the
 * attempt count would weaken exactly the assertions this exists to keep.
 *
 * Refused for a real session: a production shim told to retry with no backoff
 * would hammer a store that is merely restarting.
 */
export const FAKE_STORE_BACKOFF_ENV = "AGENT_REPL_FAKE_STORE_BACKOFF_MS";

/**
 * Resolve the store retry policy for this process.
 *
 * Answers {@link DEFAULT_RETRY_POLICY} unless a `--fake` session supplied a
 * backoff schedule, given as a comma-separated list of non-negative whole
 * milliseconds. This one does NOT go through {@link resolveFakeMsOverride}: it
 * is list-valued, and zero is a legitimate delay here where it is a rejection
 * for every scalar budget.
 */
export function resolveRetryPolicy(
  env: NodeJS.ProcessEnv,
  fake: boolean,
): PersistenceRetryPolicy {
  const raw = env[FAKE_STORE_BACKOFF_ENV];
  if (raw === undefined || raw === "") return DEFAULT_RETRY_POLICY;
  if (!fake) {
    MAIN_LIFECYCLE_LOGGER.debug(
      {
        env: FAKE_STORE_BACKOFF_ENV,
        value: raw,
        outcome: "store_backoff_override_refused",
      },
      "the store backoff override is honored only under --fake; ignoring it for this real session",
    );
    return DEFAULT_RETRY_POLICY;
  }
  const parts = raw.split(",").map((part) => part.trim());
  const parsed = parts.map(Number);
  // An empty slot is checked SEPARATELY because `Number("")` is 0, so a
  // malformed "1,,2" would otherwise be read silently as a valid "1,0,2".
  if (parts.some((part) => part === "") || parsed.some((ms) => !Number.isInteger(ms) || ms < 0)) {
    MAIN_LIFECYCLE_LOGGER.debug(
      {
        env: FAKE_STORE_BACKOFF_ENV,
        value: raw,
        outcome: "store_backoff_override_invalid",
      },
      "the store backoff override is not a comma-separated list of non-negative whole milliseconds; ignoring it",
    );
    return DEFAULT_RETRY_POLICY;
  }
  MAIN_LIFECYCLE_LOGGER.debug(
    {
      env: FAKE_STORE_BACKOFF_ENV,
      backoff_ms: parsed,
      outcome: "store_backoff_override_applied",
    },
    "a fake session took its store retry backoff from the environment",
  );
  // The attempt count and buffer depth are NOT overridable; only the waiting is.
  return { ...DEFAULT_RETRY_POLICY, backoffMs: parsed };
}

/** Everything the process reads from its environment, resolved and checked. */
export interface ShimEnvironment {
  /** The vendor account root the agent binary must read. */
  readonly claudeConfigDir: string;
  /** The one state root every agent-repl process shares. */
  readonly stateDir: string;
  /**
   * The content hash (lowercase hex SHA-256) of the `dist/main.js` bundle the
   * daemon spawned this process from. The daemon states it at spawn time and
   * compares it, at deploy, against the freshly built bundle's own hash.
   */
  readonly shimBuildSha: string;
  /** Where the store is listening. */
  readonly storeSocket: string;
  /** The daemon's correlation id, when it exported one. Logging only. */
  readonly agentReplSessionId?: string;
}

/**
 * Resolve and validate the environment.
 *
 * EVERY REQUIRED VARIABLE IS A REFUSAL, not a default. `CLAUDE_CONFIG_DIR`
 * names which ACCOUNT the session runs as, and guessing it could run a
 * workspace's conversation under the wrong identity. `SHIM_BUILD_SHA` is the
 * content hash (lowercase hex SHA-256) of the bundle the daemon spawned this
 * process from, stated by the daemon at spawn time; the daemon's deploy
 * compares it against a freshly built bundle's own hash to bounce a stale
 * survivor, and defaulting it would make every shim look current.
 * `AGENT_REPL_OWNED=1` is the daemon's own mark — a shim started by hand has
 * no daemon to serve, no session facts coming, and no business taking the
 * workspace lock a real one needs.
 */
export function resolveEnvironment(
  env: NodeJS.ProcessEnv,
  args: CliArgs,
  home: string = os.homedir(),
): ShimEnvironment {
  const claudeConfigDir = env.CLAUDE_CONFIG_DIR;
  if (claudeConfigDir === undefined || claudeConfigDir === "") {
    throw new Error("shim: CLAUDE_CONFIG_DIR is required; it names the account this session runs as");
  }
  const owned = env[OWNED_ENV];
  if (owned !== "1") {
    throw new Error(
      `shim: ${OWNED_ENV}=1 is required (got ${JSON.stringify(owned ?? "")}); ` +
        "a shim serves a daemon that spawned it and refuses to run unowned",
    );
  }
  const buildSha = env.SHIM_BUILD_SHA;
  if (buildSha === undefined || buildSha === "") {
    throw new Error(
      "shim: SHIM_BUILD_SHA is required; it is the content hash of the bundle the daemon spawned, " +
        "which the daemon's deploy compares against a freshly built bundle to bounce a stale survivor",
    );
  }
  // THE FLAG BEATS THE ENV. A caller that stated the socket explicitly meant
  // it; the env exists so a test harness can redirect every process it starts
  // without rewriting each spawn.
  const storeSocket = args.storeSocket ?? env[STORE_SOCKET_ENV] ?? "";
  if (storeSocket === "") {
    throw new Error(
      `shim: the store socket is required; pass --store-socket <uds> or set ${STORE_SOCKET_ENV}`,
    );
  }
  const stateDir =
    env.AGENT_REPL_STATE_DIR === undefined || env.AGENT_REPL_STATE_DIR === ""
      ? path.join(home, DEFAULT_STATE_DIR_NAME)
      : env.AGENT_REPL_STATE_DIR;
  const agentReplSessionId = env[SESSION_ID_ENV];
  return {
    claudeConfigDir,
    stateDir,
    shimBuildSha: buildSha,
    storeSocket,
    ...(agentReplSessionId === undefined || agentReplSessionId === ""
      ? {}
      : { agentReplSessionId }),
  };
}

// ---------------------------------------------------------------------------
// --version
// ---------------------------------------------------------------------------

/** This package's version, read from its own package.json. */
export function packageVersion(): string {
  try {
    const require = createRequire(import.meta.url);
    return (require("../package.json") as { version: string }).version;
  } catch {
    // A bundle relocated away from its package.json still answers, honestly.
    return "unknown";
  }
}

/** The `--version` line. */
export function versionLine(): string {
  return `claude-shim ${packageVersion()}`;
}

// ---------------------------------------------------------------------------
// signals
// ---------------------------------------------------------------------------

/**
 * How long the exit after `KillSession` waits for the wire to go quiet.
 *
 * A LAST RESORT, never the mechanism: the responses' own close events settle
 * the wait in microseconds. This only bounds the pathological case — a stream
 * the teardown somehow failed to conclude — so that a killed shim cannot be
 * kept alive by one wedged consumer.
 */
export const EXIT_QUIET_BUDGET_MS = 5_000;

/**
 * The quiet-drain budget override, honored ONLY under `--fake`.
 *
 * A LAST RESORT bound like the watcher budget above, and overridable for the
 * same reason: what a kill scenario asserts is that the exit WAITS for the wire
 * and then ends anyway, not the particular number of milliseconds it waits for.
 *
 * Refused for a real session: a production shim told to exit instantly would
 * destroy the socket out from under a response still on it.
 */
export const FAKE_EXIT_QUIET_BUDGET_ENV = "AGENT_REPL_FAKE_EXIT_QUIET_BUDGET_MS";

/**
 * Resolve the quiet-drain budget for this process.
 *
 * Answers {@link EXIT_QUIET_BUDGET_MS} unless a `--fake` session overrode it.
 */
export function resolveExitQuietBudgetMs(env: NodeJS.ProcessEnv, fake: boolean): number {
  return (
    resolveFakeMsOverride(
      env,
      fake,
      FAKE_EXIT_QUIET_BUDGET_ENV,
      "exit_quiet_budget_override",
      "quiet-drain budget",
    ) ?? EXIT_QUIET_BUDGET_MS
  );
}

/** What a signal handler set needs to reach. */
export interface SignalTargets {
  /** The session, stood down before the process ends. */
  readonly engine: Engine;
  /** The listener, closed so the socket file does not outlive us. */
  readonly server: Pick<ShimServer, "close">;
  /** How the process ends. Injected so a test observes the code. */
  readonly exit: (code: number) => void;
}

/** The handler set, exposed so a test can invoke it without raising a signal. */
export interface SignalHandlers {
  readonly onSigterm: () => void;
  readonly onSigint: () => void;
  /** The in-progress stand-down, or null while none has begun. */
  standingDown(): Promise<void> | null;
}

/**
 * Own the process-signal boundary.
 *
 * SIGTERM is idempotent: a second one while the first stand-down is in flight is
 * ignored rather than starting a second teardown, because two concurrent
 * teardowns race over the same pending callbacks and store acks.
 */
export function shutdownSignalHandlers(targets: SignalTargets): SignalHandlers {
  let standDown: Promise<void> | null = null;
  return {
    onSigterm(): void {
      if (standDown !== null) {
        MAIN_LIFECYCLE_LOGGER.debug(
          { signal: "SIGTERM", outcome: "shutdown_already_in_flight" },
          "ignored a second SIGTERM: the graceful stand-down is already running",
        );
        return;
      }
      MAIN_LIFECYCLE_LOGGER.info(
        { signal: "SIGTERM", outcome: "graceful_stand_down_started" },
        "received the authorized shutdown signal; standing the session down",
      );
      standDown = (async (): Promise<void> => {
        try {
          const code = await targets.engine.standDown("SIGTERM");
          await targets.server.close();
          const fields = {
            signal: "SIGTERM",
            outcome:
              code === 0 ? "graceful_stand_down_complete" : "stand_down_with_lost_writes",
            exit_code: code,
          };
          const message =
            code === 0
              ? "stood down cleanly"
              : "stood down with writes the store never acked; exiting nonzero";
          if (code === 0) MAIN_LIFECYCLE_LOGGER.info(fields, message);
          else MAIN_LIFECYCLE_LOGGER.error({ ...fields, detail: message }, message);
          targets.exit(code);
        } catch (err) {
          // A failed stand-down is still an exit, but NOT a clean one: reporting
          // 0 here would tell the daemon the session ended in good order when
          // writes may have been lost.
          reportFatal(err);
          targets.exit(1);
        }
      })();
    },
    onSigint(): void {
      // warn: a decision because SIGINT must not terminate a live vendor turn.
      MAIN_LIFECYCLE_LOGGER.warn(
        {
          signal: "SIGINT",
          outcome: "refused_shutdown",
          query_preserved: true,
        },
        "refused SIGINT as a shutdown condition because an attached terminal must not end a live turn",
      );
    },
    standingDown: () => standDown,
  };
}

/** The handler set for the manual keep-alive reset backdoor, exposed so a test can invoke it without raising a signal. */
export interface KeepaliveResetSignalHandlers {
  readonly onSigusr2: () => void;
}

/**
 * SIGUSR2: the manual keep-alive reset backdoor (see the module doc's
 * "Signals" section).
 *
 * `engine.resetKeepalives()` is itself a safe no-op when no keep-alive debt is
 * outstanding or no session is bound, so this handler does no session-presence
 * check of its own — it always fires, and the engine decides whether there is
 * anything to collapse.
 */
export function keepaliveResetSignalHandler(engine: SessionEngine): KeepaliveResetSignalHandlers {
  return {
    onSigusr2(): void {
      MAIN_LIFECYCLE_LOGGER.info(
        { signal: "SIGUSR2", outcome: "keepalive_reset_requested" },
        "received SIGUSR2: resetting all outstanding keep-alive turns back to the last real record",
      );
      void engine.resetKeepalives();
    },
  };
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

/**
 * THE DAEMON'S WORKSPACE ID, read off the socket it told this shim to serve.
 *
 * WHY THE SOCKET IS THE SOURCE. `workspace_id` on a log record is the fleet's
 * grouping key -- `bin/logs.sh --workspace` and the realtest harvest both
 * attribute by it -- so it has to be the DAEMON's minted 16-hex identity and
 * nothing the shim invents. No spawn argument or environment variable carries
 * it: the spawn contract is `--listen`, `--store-socket`, `--log-fd` and
 * `--fake`, and the only contracted env is the account, the owned mark, the
 * build sha, the state root, the store socket and the lock locations. What the
 * daemon DOES hand over is the socket path, and it names that socket after the
 * workspace: `<state>/sock/<workspace id>.sock`, with the rollout generation
 * appended as `.n<gen>` (`daemon/internal/stateroot` mints the first shape,
 * `daemon/internal/workspace/fleet_rollout.go` the second). So the id is read
 * back out of the basename, and `daemon/internal/wsm.IDLength` is why sixteen
 * hex characters is the shape rather than a guess.
 *
 * A REFUSAL, NOT A GUESS. A basename that does not spell one means this build
 * and the daemon disagree about the socket layout, exactly like an unrecognized
 * flag -- and a record filed under a workspace the fleet never heard of is
 * worse than a shim that refuses to start and says why.
 */
export function workspaceIdFromListenSocket(listen: string): string {
  const base = path.basename(listen);
  const stem = base.endsWith(".sock") ? base.slice(0, -".sock".length) : base;
  // The rollout's generation suffix, stripped before the id is read: a
  // replacement shim serves `<id>.n2.sock` for the SAME workspace.
  const candidate = stem.replace(/\.n\d+$/, "");
  if (!/^[0-9a-f]{16}$/.test(candidate)) {
    throw new Error(
      `shim: the --listen socket ${listen} is not named after a workspace id; ` +
        "expected <16 hex characters>[.n<generation>].sock, which is how the daemon names it",
    );
  }
  return candidate;
}

/**
 * The shim's own identity in its log records, before a session exists.
 *
 * `agent_repl_session_id` normally names the daemon's session, but no spawn
 * argument carries one any more (session facts travel only in `StartSession`,
 * which is an rpc that has not arrived yet). So the process names itself, in a
 * form that CORRELATES: it is keyed by THE DAEMON'S OWN workspace id, the same
 * one every record's `workspace_id` carries, so a self-named record joins the
 * daemon's records for the same workspace. It used to be keyed by the shim's
 * md5 prefix, which joined nothing outside this process. The vendor's own id is
 * attached later, through `setClaudeSessionId`, once the SDK reveals it.
 */
export function processIdentity(workspaceId: string): string {
  return `shim-${workspaceId}-${process.pid}`;
}

/** Where the log's correlation id came from, so the record says which it used. */
export interface LogCorrelation {
  readonly agentReplSessionId: string;
  readonly source: "daemon_env" | "self_named";
}

/**
 * The correlation id every log record carries.
 *
 * The daemon's exported id WINS when it exported one, because a joinable record
 * across two processes is worth more than a locally-derived name. Absent it the
 * shim names itself — no daemon id reaches a shim at spawn in every deployment,
 * and a record with no correlation id at all is the one outcome neither side
 * can recover from.
 */
export function logCorrelation(
  environment: ShimEnvironment,
  workspaceId: string,
): LogCorrelation {
  const exported = environment.agentReplSessionId;
  return exported === undefined || exported === ""
    ? { agentReplSessionId: processIdentity(workspaceId), source: "self_named" }
    : { agentReplSessionId: exported, source: "daemon_env" };
}

/**
 * The query factory the engine calls, real or mocked.
 *
 * `--fake` swaps THIS and nothing else: the real shim runs unchanged over the
 * mocked vendor, which is what makes an offline test a test of the shim rather
 * than of a second implementation of it.
 */
export function queryFactory(fake: boolean, environment: ShimEnvironment, cwd: string): CreateQuery {
  // THE WORKSPACE IS TRUSTED BEFORE ANY VENDOR IS CONSTRUCTED, mocked one
  // included. This is the single place a query comes into being, so trust
  // cannot be forgotten on one path and remembered on another; and the mocked
  // vendor takes the same step so the ordering itself is testable offline.
  // An untrusted directory is not refused by the vendor — it is run with the
  // workspace's permission allowlists silently dropped, which is why nothing
  // here treats absence as a failure to start.
  const trusted = (): void => {
    ensureWorkspaceTrusted(environment.claudeConfigDir, cwd);
  };
  if (!fake) {
    return (spec: QuerySpec) => {
      trusted();
      return createRealQuery(
        {
          cwd,
          claudeConfigDir: environment.claudeConfigDir,
          binding: spec.binding,
          permissionMode: spec.permissionMode,
          canUseTool: spec.canUseTool,
          abortController: spec.abortController,
          ...(spec.model === undefined ? {} : { model: spec.model }),
          ...(spec.onStderr === undefined ? {} : { onStderr: spec.onStderr }),
          ...(spec.onChildExit === undefined ? {} : { onChildExit: spec.onChildExit }),
          ...(spec.resumeSessionAt === undefined ? {} : { resumeSessionAt: spec.resumeSessionAt }),
        },
        spec.prompt,
      );
    };
  }
  return (spec: QuerySpec) => {
    trusted();
    return Promise.resolve(
      createFakeQuery(spec.prompt, spec.canUseTool, {
        cwd,
        configDir: environment.claudeConfigDir,
        sessionId:
          spec.binding.kind === "fresh" ? spec.binding.sessionId : spec.binding.resumeSessionId,
        newUuid: () => randomUUID(),
        abortSignal: spec.abortController.signal,
        permissionMode: spec.permissionMode,
        ...(spec.model === undefined ? {} : { model: spec.model }),
        ...(spec.binding.kind === "resume" ? { resume: spec.binding.resumeSessionId } : {}),
        // THE REWIND TARGET REACHES THE MOCK TOO. It was dropped here, so the
        // keep-alive rewind was unobservable on the vendor side: the shim's own
        // log said what it intended, which is not evidence the value arrived.
        ...(spec.resumeSessionAt === undefined ? {} : { resumeSessionAt: spec.resumeSessionAt }),
        // THE SAME DECLARATION THE REAL QUERY MAKES, so the mock's interrupt
        // runs under the posture production runs under.
        perTaskStopAffordance: PER_TASK_STOP_AFFORDANCE,
      }),
    );
  };
}

export async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));

  // `--version` is a dependency-free smoke of the bundle: it loads every static
  // import (the proto stubs, the Connect runtime, @bufbuild/protobuf) and exits
  // before touching a socket, a lock, the log fd, or the SDK.
  if (args.version) {
    process.stdout.write(`${versionLine()}\n`);
    return;
  }

  requireServingArgs(args);
  const environment = resolveEnvironment(process.env, args);
  const cwd = process.cwd();

  // BEFORE THE LOG IS CONFIGURED, because the log cannot state a workspace it
  // has not been told: a refusal here is reported through the pre-logger fatal
  // path, the same as any other spawn-contract disagreement.
  const workspaceId = workspaceIdFromListenSocket(args.listen);
  const correlation = logCorrelation(environment, workspaceId);
  configureLog({
    fd: args.logFd,
    cwd,
    workspaceId,
    agentReplSessionId: correlation.agentReplSessionId,
  });

  const identity = runtimeIdentity();
  MAIN_LIFECYCLE_LOGGER.info(
    {
      workspace_dir: cwd,
      listen_socket: args.listen,
      store_socket: environment.storeSocket,
      state_dir: environment.stateDir,
      claude_config_dir: environment.claudeConfigDir,
      lock_dir: lockDir(),
      lock_dir_overridden: (process.env[LOCK_DIR_ENV] ?? "") !== "",
      // WHICH HOLDER WILL TAKE THE CLAIM. The kernel lock lives in a child
      // process (agent-shim/shim-lock), and a shim pointed at a binary that is
      // not there refuses every session — so the path is stated at startup,
      // where it is readable before the first StartSession rather than only in
      // the failure.
      lock_binary: lockBinaryPath(),
      shim_build_sha: identity.shimBuildSha,
      sdk_version: identity.sdkVersion,
      agent_binary_version: identity.agentBinaryVersion ?? "",
      fake: args.fake,
      agent_repl_session_id_source: correlation.source,
      outcome: "startup_arguments_validated",
    },
    "validated the spawn contract and configured durable logging",
  );

  // NO LOCK IS TAKEN HERE. Startup is parse argv -> configure the log -> bind
  // the socket -> serve, and a shim that has served but has no session is
  // INERT: it holds neither kernel lock. Both claims are made inside
  // StartSession, before the SDK is touched, and held for the process lifetime
  // — so a prelaunched inert shim can sit beside the live shim it is about to
  // replace instead of blocking forever on the live shim's workspace lock,
  // while the daemon's probe still reads a held lock as "a live shim owns this
  // conversation".

  const keepaliveIntervalMs = resolveKeepaliveIntervalMs(process.env, args.fake);
  const exitQuietBudgetMs = resolveExitQuietBudgetMs(process.env, args.fake);
  const watcherConclusionBudgetMs = resolveWatcherConclusionBudgetMs(process.env, args.fake);
  const retry = resolveRetryPolicy(process.env, args.fake);

  // THE SESSION ENGINE, with the real record plane behind it.
  //
  // The record plane is UNNAMED here on purpose: a writer is keyed by the
  // conversation's ORIGINAL vendor session id, which only StartSession learns,
  // and it names itself then (`Persistence.setProducer`). Nothing writes before
  // that, and a write that tried would raise rather than land rows under a
  // placeholder name no replay could absorb against.
  // Filled the moment the listener exists; `KillSession` cannot fire before
  // then, because it arrives over that listener.
  let endProcess: (code: number) => void = (code) => {
    MAIN_LIFECYCLE_LOGGER.error(
      { outcome: "exit_before_serving", exit_code: code, detail: "the listener did not exist when process exit was requested" },
      "a session end was requested before the listener existed; exiting immediately",
    );
    process.exit(code);
  };

  const engine: SessionEngine = createEngine({
    endProcess: (code) => {
      endProcess(code);
    },
    persistence: createPersistence({
      client: createStoreClient(environment.storeSocket),
      nowMs: () => Date.now(),
      retry,
    }),
    fold: createFold(),
    createQuery: queryFactory(args.fake, environment, cwd),
    runtime: { shimBuildSha: identity.shimBuildSha, sdkVersion: identity.sdkVersion },
    env: { stateDir: environment.stateDir, configDir: environment.claudeConfigDir, cwd },
    nowMs: () => Date.now(),
    ...(keepaliveIntervalMs === undefined ? {} : { keepaliveIntervalMs }),
    ...(watcherConclusionBudgetMs === undefined ? {} : { watcherConclusionBudgetMs }),
  });

  const server = await serve(args.listen, shimRoutes(engine));

  // KillSession's own exit. The engine has already torn the session down and
  // built its response; the process may only end once that response — and every
  // stream terminal the teardown produced — is off the wire, which is what
  // `quiet` waits for. Closing first would destroy the socket carrying it.
  let ending = false;
  endProcess = (code): void => {
    if (ending) return;
    ending = true;
    void (async (): Promise<void> => {
      try {
        await server.quiet(exitQuietBudgetMs);
        await server.close();
        const fields = {
          outcome: code === 0 ? "session_killed_exit" : "session_killed_exit_lost_writes",
          exit_code: code,
        };
        const message =
          code === 0
            ? "the session was killed over the wire; the process is ending"
            : "the session was killed with writes the store never acked; exiting nonzero";
        if (code === 0) MAIN_LIFECYCLE_LOGGER.info(fields, message);
        else MAIN_LIFECYCLE_LOGGER.error({ ...fields, detail: message }, message);
      } catch (err) {
        reportFatal(err);
        process.exit(1);
      }
      process.exit(code);
    })();
  };

  // THE SIGNAL HANDLERS GO ON BEFORE THE "SERVING" RECORD, NOT AFTER IT. The
  // record is the shim's announcement that it is ready, and every supervisor
  // waits on it before doing anything to the process — so a shim that
  // announced readiness while node's DEFAULT signal dispositions were still in
  // force could be killed by the very SIGINT it exists to refuse, in the gap
  // between the two statements. Observed as a flake in the SIGINT integration
  // test, which is the only place the gap is reachable at all.
  const handlers = shutdownSignalHandlers({
    engine,
    server,
    exit: (code) => process.exit(code),
  });
  process.on("SIGTERM", handlers.onSigterm);
  process.on("SIGINT", handlers.onSigint);
  const keepaliveResetHandlers = keepaliveResetSignalHandler(engine);
  process.on("SIGUSR2", keepaliveResetHandlers.onSigusr2);

  MAIN_LIFECYCLE_LOGGER.info(
    { listen_socket: args.listen, outcome: "serving" },
    "shim.v1 is being served; the daemon may dial",
  );

  // Nothing else to do: the listener holds the process open, and it is closed
  // only by the stand-down. A shim outlives its daemon by design, so there is
  // deliberately no idle timeout and no stdin to reach EOF.
  await new Promise<never>(() => {});
}

/**
 * Was this module invoked as the program, rather than imported?
 *
 * `import.meta.url` is ALREADY symlink-resolved by the ESM loader, while
 * `process.argv[1]` is whatever the spawner typed. Comparing them raw made a
 * spawn through any symlinked directory (`/var/folders/...` on macOS, which is
 * really `/private/var/folders/...`) compare unequal, and the shim then exited 0
 * having done NOTHING — the worst possible failure, silent and successful.
 */
function invokedAs(argvPath: string): string {
  try {
    return pathToFileURL(realpathSync(argvPath)).href;
  } catch {
    return pathToFileURL(argvPath).href;
  }
}

const isDirectRun =
  process.argv[1] !== undefined && import.meta.url === invokedAs(process.argv[1]);
if (isDirectRun) {
  main().catch((err: unknown) => {
    reportFatal(err);
    process.exit(1);
  });
}
