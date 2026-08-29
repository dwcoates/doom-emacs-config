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
 */
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import { realpathSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { bindLog, configureLog, emergencyStderr } from "./log.js";
import { acquireWorkspaceLock, lockDir, workspaceLockKey, LOCK_DIR_ENV } from "./locks.js";
import { runtimeIdentity } from "./build-identity.js";
import { type Engine } from "./engine/engine.js";
import { createEngine, type CreateQuery, type QuerySpec } from "./engine/session.js";
import { createFold } from "./convert/fold.js";
import { createStoreClient } from "./store/client.js";
import { producerId } from "./store/keys.js";
import { createPersistence } from "./store/persistence.js";
import { createRealQuery } from "./sdk/real-query.js";
import { createFakeQuery } from "./fake/index.js";
import { randomUUID } from "node:crypto";
import { shimRoutes } from "./service/routes.js";
import { serve, type ShimServer } from "./service/server.js";

/** Stable operation labels for entrypoint telemetry and tests. */
export const MAIN_LIFECYCLE_OPERATION = "shim.main.lifecycle";
export const MAIN_FATAL_OPERATION = "shim.main.fatal";

const LIFECYCLE_LOGGER = bindLog({ component: "shim-main", operation: MAIN_LIFECYCLE_OPERATION });
const FATAL_LOGGER = bindLog({ component: "shim-main", operation: MAIN_FATAL_OPERATION });

/** Emit a lifecycle record at info unless the caller identifies an error. */
export function logMainLifecycle(fields: Record<string, unknown>, message: string): void {
  LIFECYCLE_LOGGER.log({ level: "info", ...fields }, message);
}

function fatalCause(err: unknown): string {
  if (err instanceof Error) return err.name.length === 0 ? "Error" : err.name;
  return typeof err;
}

/** Log an unrecoverable entrypoint failure before ending the process. */
export function reportFatal(err: unknown): void {
  const message = `fatal: ${err instanceof Error ? (err.stack ?? err.message) : String(err)}`;
  try {
    FATAL_LOGGER.log(
      {
        level: "error",
        cause: err,
        cause_class: "unrecoverable_entrypoint_failure",
        cause_type: fatalCause(err),
        exit_outcome: "process_exit_1",
      },
      message,
    );
  } catch (logErr) {
    // Reached only before the logger is configured, or when its sink failed.
    emergencyStderr(
      `${message}; logger failure: ${logErr instanceof Error ? logErr.message : String(logErr)}`,
    );
  }
}

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

/** Everything the process reads from its environment, resolved and checked. */
export interface ShimEnvironment {
  /** The vendor account root the agent binary must read. */
  readonly claudeConfigDir: string;
  /** The one state root every agent-repl process shares. */
  readonly stateDir: string;
  /** The build identity the daemon compares against its deploy stamp. */
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
 * workspace's conversation under the wrong identity. `SHIM_BUILD_SHA` is what
 * the daemon compares against its deploy stamp to bounce a stale survivor;
 * defaulting it would make every shim look current. `AGENT_REPL_OWNED=1` is the
 * daemon's own mark — a shim started by hand has no daemon to serve, no session
 * facts coming, and no business taking the workspace lock a real one needs.
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
      "shim: SHIM_BUILD_SHA is required; the daemon compares it against the deploy stamp to bounce a stale survivor",
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
  onSigterm(): void;
  onSigint(): void;
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
        logMainLifecycle(
          { signal: "SIGTERM", outcome: "shutdown_already_in_flight" },
          "ignored a second SIGTERM: the graceful stand-down is already running",
        );
        return;
      }
      logMainLifecycle(
        { signal: "SIGTERM", outcome: "graceful_stand_down_started" },
        "received the authorized shutdown signal; standing the session down",
      );
      standDown = (async (): Promise<void> => {
        try {
          await targets.engine.standDown("SIGTERM");
          await targets.server.close();
          logMainLifecycle(
            { signal: "SIGTERM", outcome: "graceful_stand_down_complete", exit_code: 0 },
            "stood down cleanly",
          );
          targets.exit(0);
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
      logMainLifecycle(
        {
          level: "error",
          signal: "SIGINT",
          outcome: "refused_shutdown",
          query_preserved: true,
        },
        "REFUSED SIGINT as a shutdown condition: an attached terminal's Ctrl-C must not end a live turn",
      );
    },
    standingDown: () => standDown,
  };
}

// ---------------------------------------------------------------------------
// main
// ---------------------------------------------------------------------------

/**
 * The shim's own identity in its log records, before a session exists.
 *
 * `agent_repl_session_id` normally names the daemon's session, but no spawn
 * argument carries one any more (session facts travel only in `StartSession`,
 * which is an rpc that has not arrived yet). So the process names itself, in a
 * form that CORRELATES: the workspace key is the same md5 prefix the workspace
 * lock file and every log record use, so a log line, a lock file and a process
 * can be matched without a session id. The vendor's own id is attached later,
 * through `setClaudeSessionId`, once the SDK reveals it.
 */
export function processIdentity(cwd: string): string {
  return `shim-${workspaceLockKey(cwd)}-${process.pid}`;
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
export function logCorrelation(environment: ShimEnvironment, cwd: string): LogCorrelation {
  const exported = environment.agentReplSessionId;
  return exported === undefined || exported === ""
    ? { agentReplSessionId: processIdentity(cwd), source: "self_named" }
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
  if (!fake) {
    return (spec: QuerySpec) =>
      createRealQuery(
        {
          cwd,
          claudeConfigDir: environment.claudeConfigDir,
          binding: spec.binding,
          permissionMode: spec.permissionMode,
          canUseTool: spec.canUseTool,
          abortController: spec.abortController,
          ...(spec.model === undefined ? {} : { model: spec.model }),
          ...(spec.resumeSessionAt === undefined ? {} : { resumeSessionAt: spec.resumeSessionAt }),
        },
        spec.prompt,
      );
  }
  return (spec: QuerySpec) =>
    Promise.resolve(
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
      }),
    );
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

  const correlation = logCorrelation(environment, cwd);
  configureLog({ fd: args.logFd, cwd, agentReplSessionId: correlation.agentReplSessionId });

  const identity = runtimeIdentity();
  logMainLifecycle(
    {
      workspace_dir: cwd,
      listen_socket: args.listen,
      store_socket: environment.storeSocket,
      state_dir: environment.stateDir,
      claude_config_dir: environment.claudeConfigDir,
      lock_dir: lockDir(),
      lock_dir_overridden: (process.env[LOCK_DIR_ENV] ?? "") !== "",
      shim_build_sha: identity.shimBuildSha,
      sdk_version: identity.sdkVersion,
      agent_binary_version: identity.agentBinaryVersion ?? "",
      fake: args.fake,
      agent_repl_session_id_source: correlation.source,
      outcome: "startup_arguments_validated",
    },
    "validated the spawn contract and configured durable logging",
  );

  // THE WORKSPACE CLAIM, before the socket. Two shims over one workspace means
  // two writers on one transcript; binding first would leave a window in which
  // a duplicate is reachable and already writing.
  const releaseWorkspaceLock = acquireWorkspaceLock(cwd);
  process.on("exit", releaseWorkspaceLock);
  logMainLifecycle(
    { workspace_dir: cwd, workspace_id: workspaceLockKey(cwd), outcome: "workspace_lock_acquired" },
    "exclusive workspace lock acquired",
  );

  // THE SESSION ENGINE, with the real record plane behind it.
  //
  // The producer name is keyed by the workspace until the vendor names a
  // session: write ids are `sha256(producer | coordinates | arm)`, so the name
  // only has to be STABLE for one conversation's writes to share a namespace,
  // and the workspace is the one identity that exists before StartSession.
  const engine: Engine = createEngine({
    persistence: createPersistence({
      client: createStoreClient(environment.storeSocket),
      producer: producerId(`workspace:${workspaceLockKey(cwd)}`),
      nowMs: () => Date.now(),
    }),
    fold: createFold(),
    createQuery: queryFactory(args.fake, environment, cwd),
    runtime: { shimBuildSha: identity.shimBuildSha, sdkVersion: identity.sdkVersion },
    env: { stateDir: environment.stateDir, configDir: environment.claudeConfigDir, cwd },
    nowMs: () => Date.now(),
  });

  const server = await serve(args.listen, shimRoutes(engine));
  logMainLifecycle(
    { listen_socket: args.listen, outcome: "serving" },
    "shim.v1 is being served; the daemon may dial",
  );

  const handlers = shutdownSignalHandlers({
    engine,
    server,
    exit: (code) => process.exit(code),
  });
  process.on("SIGTERM", handlers.onSigterm);
  process.on("SIGINT", handlers.onSigint);

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
