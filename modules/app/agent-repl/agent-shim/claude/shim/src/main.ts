/**
 * claude-repl shim entrypoint.
 *
 * UDS-only process entrypoint for one daemon-owned Claude session.
 *
 * Flags:
 *   --fake                    use the offline scripted query (no API key)
 *   --session-id <id>         override the shim-assigned session id
 *   --permission-mode <mode>  initial permission mode (default: "default")
 *   --cwd <dir>               working directory for the SDK session
 *   --model <model>           model override passed to the SDK
 *   --resume <session>        resume an on-disk claude session
 *   --rewound-from <uuid>     vendor session this resume's transcript was
 *                             truncated FROM (rewind lineage; requires the
 *                             two flags below and --resume)
 *   --rewind-retained-leaf <uuid>
 *                             vendor uuid of the last transcript record the
 *                             truncation retained
 *   --rewind-dropped-turns <ids>
 *                             comma-separated turn ids the truncation dropped,
 *                             in submission order
 *   --claude-bin <path>       claude CLI for the SDK to drive (system
 *                             binary for vterm parity; default: bundled)
 *   --daemon-socket <path>    required UDS endpoint to reach the daemon
 *   --log-fd 3                inherited, already-open durable shim.log sink
 *   --store-socket <path>     shim-store socket (UDS mode; default
 *                             ~/.cache/agent-repl/sock/store.sock)
 *   --version                 print the shim version and exit
 */
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import { realpathSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { bindLog, configureLog, emergencyStderr } from "./uds/log.js";
import {
  UdsSession,
  isQueryTerminationCleanupError,
  isQueryTerminationPersistenceError,
  isUnexpectedSdkStreamTerminationError,
  type UdsQuery,
} from "./uds/uds-session.js";
import { acquireSessionLock } from "./uds/session-lock.js";
import { SessionSource } from "./uds/proto.js";
import { FAKE_COMMANDS, createFakeQuery } from "./fake-query.js";
import { importRealSDK } from "./vendor-guard.js";
import { normalizeOptionalModel } from "./model.js";
import { systemPromptOption } from "./metaprompt.js";
import type {
  SubscriptionUsageQuery,
  SubscriptionUsageResponse,
} from "./subscription-usage.js";
import {
  ModelInfo,
  PermissionMode,
  SlashCommand,
  isPermissionMode,
} from "./protocol.js";
import {
  CanUseToolLike,
  InterruptReceipt,
  QueryLike,
  SdkUserMessageLike,
  SessionDeps,
} from "./session.js";

/** Stable operation labels for shim-entrypoint telemetry queries and tests. */
export const MAIN_LIFECYCLE_OPERATION = "shim.main.lifecycle";
export const MAIN_FATAL_OPERATION = "shim.main.fatal";

/** Normal lifecycle telemetry for the shim entrypoint and owned SDK session. */
const LIFECYCLE_LOGGER = bindLog({ component: "shim-main", operation: MAIN_LIFECYCLE_OPERATION });

/** Unrecoverable process-termination telemetry for the shim entrypoint only. */
const FATAL_LOGGER = bindLog({ component: "shim-main", operation: MAIN_FATAL_OPERATION });

/** Emit a main lifecycle record at info unless the caller identifies an error. */
export function logMainLifecycle(fields: Record<string, unknown>, message: string): void {
  LIFECYCLE_LOGGER.log({ level: "info", ...fields }, message);
}

function fatalCause(err: unknown): string {
  if (err instanceof Error) {
    return err.name.length === 0 ? "Error" : err.name;
  }
  return typeof err;
}

/** Log an unrecoverable entrypoint failure before ending the shim process. */
export function reportFatal(err: unknown): void {
  const message = `fatal: ${err instanceof Error ? err.stack ?? err.message : String(err)}`;
  try {
    FATAL_LOGGER.log({
      level: "error",
      cause: err,
      cause_class: "unrecoverable_entrypoint_failure",
      cause_type: fatalCause(err),
      exit_outcome: "process_exit_1",
    }, message);
  } catch (logErr) {
    // The logger is not configured only during CLI/bootstrap failure, or its sink failed.
    emergencyStderr(`${message}; logger failure: ${logErr instanceof Error ? logErr.message : String(logErr)}`);
  }
}

interface CliArgs {
  fake: boolean;
  sessionId: string;
  permissionMode: PermissionMode;
  cwd?: string;
  model?: string;
  resume?: string;
  /**
   * Rewind lineage: the daemon truncated a transcript under a NEW vendor uuid
   * and respawned this shim with `--resume <new uuid>` plus these three flags.
   *
   * All three arrive together or none do (`validateRewindLineage`), because
   * each alone is an unusable fragment: a previous id with no retained leaf
   * cannot say WHERE the cut fell, and dropped turn ids with no lineage cannot
   * say which seq space they were dropped from. The trio is also meaningless
   * without `--resume`, which names the truncated copy being continued.
   */
  rewoundFrom?: string;
  rewindRetainedLeaf?: string;
  /** Dropped keep-alive turn ids in submission order; order is contractual. */
  rewindDroppedTurns?: string[];
  /** Path to the claude CLI the SDK should drive.
   *
   *  Kept for VERSION PARITY with vterm sessions: the user upgrades their
   *  `claude` independently of our lockfile, so the system binary can lead
   *  the SDK's bundled one. It no longer exists to work around a stale
   *  bundle — since SDK 0.2.113 the SDK spawns a per-platform NATIVE
   *  Claude Code binary (0.3.220 bundles 2.1.220), not a JS `cli.js`, and
   *  that bundle is current enough to resolve the same command set. */
  claudeBin?: string;
  /** UDS-mode listener path (session-<id>.sock). Present => UDS mode. */
  daemonSocket?: string;
  /** Already-open durable shim.log descriptor inherited from the daemon. */
  logFd?: number;
  /** shim-store socket path (UDS mode only). Defaults under ~/.cache. */
  storeSocket?: string;
  /** Print the version and exit (a node-runnable smoke of the bundle). */
  version?: boolean;
}

/** The default shim-store socket, honoring XDG_CACHE_HOME (design §3). */
export function defaultStoreSocket(): string {
  const cache = process.env.XDG_CACHE_HOME ?? path.join(os.homedir(), ".cache");
  return path.join(cache, "agent-repl", "sock", "store.sock");
}

export function parseArgs(argv: string[]): CliArgs {
  const args: CliArgs = {
    fake: false,
    sessionId: randomUUID(),
    permissionMode: "default",
  };
  for (let i = 0; i < argv.length; i++) {
    const arg = argv[i];
    const next = (): string => {
      const v = argv[++i];
      if (v === undefined) throw new Error(`missing value for ${arg}`);
      return v;
    };
    switch (arg) {
      case "--fake":
        args.fake = true;
        break;
      case "--session-id":
        args.sessionId = next();
        break;
      case "--permission-mode": {
        const mode = next();
        if (!isPermissionMode(mode)) {
          throw new Error(`invalid --permission-mode: ${mode}`);
        }
        args.permissionMode = mode;
        break;
      }
      case "--cwd":
        args.cwd = next();
        break;
      case "--model":
        args.model = next();
        break;
      case "--resume":
        args.resume = next();
        break;
      case "--rewound-from":
        args.rewoundFrom = next();
        break;
      case "--rewind-retained-leaf":
        args.rewindRetainedLeaf = next();
        break;
      case "--rewind-dropped-turns":
        args.rewindDroppedTurns = parseDroppedTurns(next());
        break;
      case "--claude-bin":
        args.claudeBin = next();
        break;
      case "--daemon-socket":
        args.daemonSocket = next();
        break;
      case "--log-fd": {
        const value = next();
        if (!/^\d+$/.test(value) || Number(value) !== 3) throw new Error(`invalid --log-fd: ${value}; UDS mode requires inherited fd 3`);
        args.logFd = 3;
        break;
      }
      case "--store-socket":
        args.storeSocket = next();
        break;
      case "--version":
        args.version = true;
        break;
      default:
        throw new Error(`unknown argument: ${arg}`);
    }
  }
  validateRewindLineage(args);
  return args;
}

/**
 * Split `--rewind-dropped-turns` into its ordered turn ids.
 *
 * Order is the CONTRACT (KeepAliveDiscard.dropped_turn_ids is "in submission
 * order"), so the list is never sorted or deduplicated here. An empty element
 * or an empty list is rejected rather than silently dropped: a rewind that
 * dropped no turn is not a rewind, and a blank id would reach the store as an
 * unresolvable reference.
 */
function parseDroppedTurns(value: string): string[] {
  const ids = value.split(",");
  if (ids.some((id) => id.trim() === "")) {
    throw new Error(`invalid --rewind-dropped-turns: ${JSON.stringify(value)}; every comma-separated turn id must be non-empty`);
  }
  return ids;
}

/**
 * Enforce the daemon's rewind-lineage spawn contract before startup proceeds.
 *
 * A PARTIAL set is a loud startup failure, never a silent degrade to "no
 * rewind": the daemon has already retired the previous vendor session by the
 * time it spawns us, so a shim that quietly skipped SessionRewound would leave
 * the lineage unreconstructable from the store forever. Failing here costs one
 * respawn; swallowing it costs the durable record.
 */
export function validateRewindLineage(args: CliArgs): void {
  const present = [
    ["--rewound-from", args.rewoundFrom],
    ["--rewind-retained-leaf", args.rewindRetainedLeaf],
    ["--rewind-dropped-turns", args.rewindDroppedTurns],
  ] as const;
  const supplied = present.filter(([, value]) => value !== undefined);
  if (supplied.length === 0) return;
  if (supplied.length !== present.length) {
    const missing = present.filter(([, value]) => value === undefined).map(([flag]) => flag);
    throw new Error(`incomplete rewind lineage: ${supplied.map(([flag]) => flag).join(", ")} supplied without ${missing.join(", ")}; all three must be present together or all absent`);
  }
  if (args.resume === undefined) {
    throw new Error("rewind lineage requires --resume: the rewound-to vendor session id names the truncated transcript being continued");
  }
  if (args.rewoundFrom === args.resume) {
    throw new Error(`invalid rewind lineage: --rewound-from equals --resume (${args.resume}); a rewind always produces a NEW vendor session id`);
  }
}

/** Validate the daemon's UDS primary-writer spawn contract before startup mutates state. */
export function validateUdsLoggingArgs(args: CliArgs): asserts args is CliArgs & { daemonSocket: string; cwd: string; logFd: 3 } {
  if (args.daemonSocket === undefined) throw new Error("UDS logging validation requires --daemon-socket");
  if (args.cwd === undefined) throw new Error("UDS mode requires --cwd");
  if (args.logFd === undefined) throw new Error("UDS mode requires --log-fd 3");
}

function packageVersion(spec: string): string {
  try {
    const require = createRequire(import.meta.url);
    return (require(spec) as { version: string }).version;
  } catch {
    return "unknown";
  }
}

/**
 * Assemble the SDK query options for a real (non-fake) session.
 * Exported for unit testing (the factory itself needs the live SDK).
 *
 * Interactive-CLI parity is deliberate, NOT the SDK's isolation-mode
 * default:
 * - The `claude_code` system-prompt preset carries the environment
 *   block (cwd, platform, home). Without it the model has no idea what
 *   `~` is and invents paths like /Users/user/... for tilde-phrased
 *   instructions.
 * - settingSources loads the user's settings.json (permission
 *   allowlists, hooks), project settings, and CLAUDE.md — the posture
 *   every vterm-era workflow assumes.
 *
 * The preset additionally carries the session's metaprompt as an
 * `append` (metaprompt.ts), which is how the harness's guidelines reach
 * the agent at all: they are part of the system prompt the SDK re-sends
 * on every request, not a directive injected into the conversation.
 */
export function realQueryOptions(
  args: CliArgs,
  canUseTool: CanUseToolLike,
  abortController?: AbortController,
): Record<string, unknown> {
  const model = normalizeOptionalModel(args.model);
  return {
    canUseTool: canUseTool as never,
    includePartialMessages: true,
    permissionMode: args.permissionMode,
    systemPrompt: systemPromptOption(args.cwd),
    settingSources: ["user", "project", "local"],
    ...(args.claudeBin !== undefined
      ? { pathToClaudeCodeExecutable: args.claudeBin }
      : {}),
    ...(args.cwd !== undefined ? { cwd: args.cwd } : {}),
    ...(model !== undefined ? { model } : {}),
    ...(args.resume !== undefined ? { resume: args.resume } : {}),
    ...(abortController !== undefined ? { abortController } : {}),
  };
}

async function realQueryFactory(
  args: CliArgs,
  prompt: AsyncIterable<SdkUserMessageLike>,
  canUseTool: CanUseToolLike,
  abortController?: AbortController,
): Promise<QueryLike> {
  const sdk = await importRealSDK("realQueryFactory");
  return sdk.query({
    prompt: prompt as never,
    options: realQueryOptions(args, canUseTool, abortController) as never,
  }) as unknown as QueryLike;
}

/**
 * The probe never yields a prompt, so no tool can ever be requested of it.
 * A call here is therefore a broken invariant rather than a permission
 * question, and inventing an answer would only hide that.
 */
const probeCanUseTool: CanUseToolLike = () => {
  throw new Error("shim: the command probe was asked to permit a tool, but it runs no turn");
};

/**
 * SDK options for the throwaway command probe.
 *
 * Derived from {@link realQueryOptions} rather than hand-rolled, because a
 * probe that resolved commands under options the session does not share
 * would offer commands the session cannot invoke. `settingSources` is the
 * sharpest example: without it the CLI resolves the 8 built-ins and none of
 * the user's or project's skills.
 *
 * `resume` is the one option deliberately dropped. Command resolution reads
 * the skill directories and settings, never the transcript, so resuming buys
 * the probe nothing and only points a second process at the live session's
 * transcript.
 */
export function probeQueryOptions(
  args: CliArgs,
  abortController: AbortController,
): Record<string, unknown> {
  const opts = realQueryOptions(args, probeCanUseTool);
  delete opts.resume;
  // The probe's only exit is this controller: aborting it is what SIGTERMs
  // the `claude` child the query spawned. A Query exposes no close() of its
  // own, so without this the shim would leak a process per refresh.
  opts.abortController = abortController;
  return opts;
}

/**
 * Re-resolve the slash-command list by standing up a throwaway query.
 *
 * The prompt iterable never yields, so the CLI completes the init handshake
 * that carries the command list and then simply idles: the probe costs one
 * process spawn and zero model tokens.
 */
async function realProbeCommands(args: CliArgs): Promise<SlashCommand[]> {
  const sdk = await importRealSDK("realProbeCommands");
  const idle = (async function* (): AsyncGenerator<SdkUserMessageLike> {
    await new Promise<never>(() => {});
  })();
  const abortController = new AbortController();
  const probe = sdk.query({
    prompt: idle as never,
    options: probeQueryOptions(args, abortController) as never,
  });
  try {
    return (await probe.supportedCommands()) as SlashCommand[];
  } finally {
    abortController.abort();
  }
}

/**
 * Build the SDK-query factory shared by both transports: a fake scripted query
 * under `--fake`, else the lazily-resolved real SDK query. The factory surface
 * ({@link SessionDeps.createQuery}) is identical to
 * {@link import("./uds/uds-session.js").UdsSessionDeps.createQuery}, so both
 * modes drive the SDK the same way.
 */
export function makeCreateQuery(args: CliArgs): SessionDeps["createQuery"] {
  return (prompt, canUseTool): QueryLike => {
    if (args.fake) {
      return createFakeQuery(prompt, canUseTool, {
        sessionId: args.sessionId,
        newUuid: randomUUID,
        ...(args.resume !== undefined ? { resume: args.resume } : {}),
      });
    }
    return lazyQuery(realQueryFactory(args, prompt, canUseTool));
  };
}

/**
 * Construct the one query owned by a UDS shim session.
 *
 * The streaming SDK has no Query.close() method. Its AbortController is the
 * query's sole lifecycle capability, so only UdsSession receives the abort
 * function and only its intentional shutdown path can invoke it.
 */
export function makeUdsQueryFactory(args: CliArgs): (
  prompt: AsyncIterable<SdkUserMessageLike>,
  canUseTool: CanUseToolLike,
) => UdsQuery {
  let fakeUsageSamples = 0;
  return (prompt, canUseTool): UdsQuery => {
    const abortController = new AbortController();
    if (args.fake) {
      let failDuringBringUp: (() => void) | undefined;
      const query = createFakeQuery(prompt, canUseTool, {
        sessionId: args.sessionId,
        newUuid: randomUUID,
        ...(args.resume !== undefined ? { resume: args.resume } : {}),
        abortSignal: abortController.signal,
        ...(args.resume !== undefined && process.env.AGENT_REPL_E2E_FAIL_RESUMED_FAKE_QUERY === "1"
          ? { onBringUpFailureInjector: (fail: () => void) => { failDuringBringUp = fail; } }
          : {}),
      });
      return {
        query,
        subscriptionUsage: async (): Promise<SubscriptionUsageResponse> => {
          fakeUsageSamples++;
          return {
            subscription_type: "fake-max",
            rate_limits_available: true,
            rate_limits: {
              five_hour: {
                utilization: 10 + fakeUsageSamples / 4,
                resets_at: "2030-01-01T05:00:00.000Z",
              },
            },
          };
        },
        abort: () => abortController.abort(),
        ...(failDuringBringUp !== undefined ? { failDuringBringUp } : {}),
      };
    }
    const queryPromise = realQueryFactory(args, prompt, canUseTool, abortController);
    return {
      query: lazyQuery(queryPromise),
      subscriptionUsage: async (): Promise<SubscriptionUsageResponse> => {
        const query = await queryPromise as QueryLike & SubscriptionUsageQuery;
        return query.usage_EXPERIMENTAL_MAY_CHANGE_DO_NOT_RELY_ON_THIS_API_YET();
      },
      abort: () => abortController.abort(),
    };
  };
}

export async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));

  // `--version` is a node-runnable smoke of the bundle: it loads every static
  // import (including the proto stubs and their bundled @bufbuild/protobuf) and
  // exits before touching a socket or the SDK. It must stay dependency-free.
  if (args.version) {
    process.stdout.write(`claude-shim ${packageVersion("../package.json")}\n`);
    return;
  }

  if (args.daemonSocket === undefined) throw new Error("shim requires --daemon-socket");
  validateUdsLoggingArgs(args);
  configureLog({ fd: args.logFd, cwd: args.cwd, agentReplSessionId: args.sessionId });
  const requestedModel = args.model;
  const normalizedModel = normalizeOptionalModel(requestedModel);
  if (normalizedModel === undefined) {
    delete args.model;
  } else {
    args.model = normalizedModel;
  }
  if (requestedModel !== undefined && normalizedModel !== requestedModel) {
    logMainLifecycle(
      {
        agent_repl_session_id: args.sessionId,
        requested_model: requestedModel,
        effective_model: normalizedModel ?? "",
        outcome: "launch_model_normalized",
      },
      "normalized empty-equivalent launch model before constructing SDK options",
    );
  }
  logMainLifecycle({ agent_repl_session_id: args.sessionId, fake: args.fake, daemon_socket: args.daemonSocket, store_socket: args.storeSocket ?? defaultStoreSocket(), permission_mode: args.permissionMode, model: args.model ?? "", resumed: args.resume !== undefined, rewound_from: args.rewoundFrom ?? "", rewind_retained_leaf: args.rewindRetainedLeaf ?? "", rewind_dropped_turn_count: args.rewindDroppedTurns?.length ?? 0, outcome: "startup_arguments_validated" }, "validated shim startup arguments and configured durable logging");

  // The query factory is synchronous per SessionDeps; pre-resolve the SDK
  // module (dynamic import) before constructing the session. Under
  // AGENT_REPL_FORBID_VENDOR_CALLS this is where a non-fake shim dies: the
  // guard throws, `main()`'s caller prints it and exits nonzero. `--fake`
  // short-circuits before the chokepoint and stays fully offline.
  const sdkModulePromise = args.fake ? null : importRealSDK("main:preresolve");
  logMainLifecycle({ agent_repl_session_id: args.sessionId, query_source: args.fake ? "fake" : "vendor-sdk", outcome: "query_implementation_selected" }, "selecting shim query implementation");
  if (sdkModulePromise) await sdkModulePromise;

  const createQuery = makeUdsQueryFactory(args);

  await runUdsMode(args, createQuery);
}

/**
 * Drive one UDS-mode session (design §8). The UDS server owns lifetime: a
 * daemon disconnect does NOT stop the session or the in-flight turn (reattach,
 * §4.4), and there is no stdin, so stdin-EOF is not a stop path. The explicit
 * stop path is SIGTERM, which cleanly shuts the session down. SIGINT is
 * refused because it is not an authorized reason to end the owned SDK query.
 */
export async function runUdsMode(
  args: CliArgs,
  createQuery: (
    prompt: AsyncIterable<SdkUserMessageLike>,
    canUseTool: CanUseToolLike,
  ) => UdsQuery,
): Promise<void> {
  // Claim the session BEFORE anything else. Uniqueness used to come free from
  // binding session-<id>.sock — a second shim could not exist. Dialling out
  // removes that, and two shims on one conversation means two writers on one
  // transcript, so the claim is now explicit. Holding it is what tells the
  // daemon this session is alive even before we have dialled in.
  //
  // Failure to claim is a refusal to start: another shim owns this session.
  const releaseLock = acquireSessionLock(args.sessionId);
  logMainLifecycle({ agent_repl_session_id: args.sessionId, outcome: "session_lock_acquired" }, "exclusive session lock acquired");
  process.on("exit", releaseLock);

  const session = new UdsSession({
    sessionId: args.sessionId,
    shimVersion: packageVersion("../package.json"),
    protocolVersion: "1",
    udsSocketPath: args.daemonSocket!,
    storeSocketPath: args.storeSocket ?? defaultStoreSocket(),
    // SessionStarted.source: RESUME when respawned to resume an on-disk
    // session, FRESH for a brand-new one (design §5.2 SessionSource).
    sessionSource: args.resume !== undefined ? SessionSource.RESUME : SessionSource.FRESH,
    // The argv posture the query is CONSTRUCTED with. The daemon's
    // DaemonHello.permission_mode overrides it inside the bring-up gate; this
    // is passed so the override is a comparison rather than a guess.
    permissionMode: args.permissionMode,
    queryInstanceId: randomUUID(),
    requestedModel: args.model,
    sdkVersion: packageVersion("@anthropic-ai/claude-agent-sdk/package.json"),
    shimBuildSha: process.env.SHIM_BUILD_SHA ?? "",
    // `--resume <uuid>` IS the vendor session id the store keys events by, so
    // a resumed session can subscribe correctly from its very first Subscribe
    // instead of waiting for the SDK to reveal the uuid.
    ...(args.resume !== undefined ? { storeSessionId: args.resume } : {}),
    // The lineage is validated as a complete trio by parseArgs, so presence of
    // the first field is presence of all three.
    ...(args.rewoundFrom !== undefined
      ? {
        rewindLineage: {
          previousVendorSessionId: args.rewoundFrom,
          retainedLeafUuid: args.rewindRetainedLeaf!,
          droppedTurnIds: args.rewindDroppedTurns!,
        },
      }
      : {}),
    createQuery,
    newRequestId: randomUUID,
  });
  const signals = udsShutdownSignalHandlers(args.sessionId, (reason) => session.shutdown(reason));
  const onSigterm = signals.onSigterm;
  const onSigint = signals.onSigint;
  process.on("SIGTERM", onSigterm);
  process.on("SIGINT", onSigint);
  // exitError, set only on the rethrow path below, is what the `finally` trace
  // reports the process exiting for: a signal-driven shutdown (including one
  // that raced session.start() into throwing) is reported clean, since
  // `signals.stopping()` had already resolved it as intentional.
  let exitError: unknown;
  try {
    await session.start();
    if (signals.stopping() === null) {
      throw new Error("UDS session completed without an intentional shutdown signal");
    }
    await signals.stopping();
  } catch (err) {
    if (signals.stopping() !== null) {
      await signals.stopping();
      return;
    }
    exitError = err;
    throw err;
  } finally {
    logMainLifecycle({
      agent_repl_session_id: args.sessionId,
      ...(exitError === undefined
        ? { outcome: "uds_main_exit_clean" }
        : {
          level: "error",
          outcome: "uds_main_exit_error",
          error: exitError instanceof Error ? exitError.message : String(exitError),
        }),
    }, "runUdsMode exiting");
    process.off("SIGTERM", onSigterm);
    process.off("SIGINT", onSigint);
    releaseLock();
  }
}

/**
 * Own the process-signal boundary for the one live SDK query.
 *
 * SIGTERM is the process-level lifecycle capability used by deliberate shim
 * teardown and hibernation. SIGINT is explicitly refused so an attached
 * terminal cannot turn an interrupt into a second query-ending capability.
 */
export function udsShutdownSignalHandlers(
  sessionId: string,
  shutdown: (reason: "SIGTERM") => Promise<void>,
): {
  onSigterm(): void;
  onSigint(): void;
  stopping(): Promise<void> | null;
} {
  let stopping: Promise<void> | null = null;
  return {
    onSigterm(): void {
      if (stopping !== null) return;
      logMainLifecycle({ agent_repl_session_id: sessionId, signal: "SIGTERM", outcome: "intentional_query_shutdown" }, "received authorized shim shutdown signal");
      stopping = shutdown("SIGTERM");
    },
    onSigint(): void {
      logMainLifecycle({
        level: "error",
        agent_repl_session_id: sessionId,
        signal: "SIGINT",
        outcome: "refused_query_termination",
        query_preserved: true,
      }, "refused unauthorized signal as an SDK query shutdown condition");
    },
    stopping: () => stopping,
  };
}

/** Adapt a Promise<QueryLike> to the synchronous QueryLike surface. */
function lazyQuery(queryPromise: Promise<QueryLike>): QueryLike {
  return {
    [Symbol.asyncIterator](): AsyncIterator<never> {
      let inner: AsyncIterator<unknown> | null = null;
      return {
        next: async (): Promise<IteratorResult<never>> => {
          if (inner === null) {
            inner = (await queryPromise)[Symbol.asyncIterator]();
          }
          return (await inner.next()) as IteratorResult<never>;
        },
      };
    },
    interrupt: async (): Promise<InterruptReceipt | undefined> =>
      (await queryPromise).interrupt(),
    setPermissionMode: async (mode): Promise<void> =>
      (await queryPromise).setPermissionMode(mode),
    setModel: async (model): Promise<void> => (await queryPromise).setModel(model),
    supportedModels: async (): Promise<ModelInfo[]> =>
      (await queryPromise).supportedModels(),
    supportedCommands: async (): Promise<SlashCommand[]> =>
      (await queryPromise).supportedCommands(),
  };
}

/**
 * Was this module invoked as the program, rather than imported?
 *
 * `import.meta.url` is ALREADY symlink-resolved by the ESM loader, while
 * `process.argv[1]` is whatever the spawner typed. Comparing them raw made a
 * spawn through any symlinked directory (`/var/folders/...` on macOS, which is
 * really `/private/var/folders/...`) compare unequal, and the shim then exited
 * 0 having done NOTHING — the worst possible failure, silent and successful.
 * Resolving argv[1] the same way the loader does is what makes the two
 * comparable. realpath is best-effort: an unresolvable argv[1] falls back to
 * the literal path rather than throwing before the fatal handler exists.
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
    if (
      !isUnexpectedSdkStreamTerminationError(err) &&
      !isQueryTerminationPersistenceError(err) &&
      !isQueryTerminationCleanupError(err)
    ) reportFatal(err);
    process.exit(1);
  });
}
