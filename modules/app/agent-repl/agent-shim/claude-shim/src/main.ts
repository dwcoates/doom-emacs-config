/**
 * claude-repl shim entrypoint.
 *
 * stdin:  Layer-1 commands (NDJSON) from the Go daemon.
 * stdout: Layer-1 events (NDJSON) to the Go daemon.
 * stderr: free-form logs, never protocol frames.
 *
 * Flags:
 *   --fake                    use the offline scripted query (no API key)
 *   --session-id <id>         override the shim-assigned session id
 *   --permission-mode <mode>  initial permission mode (default: "default")
 *   --cwd <dir>               working directory for the SDK session
 *   --model <model>           model override passed to the SDK
 *   --resume <session>        resume an on-disk claude session
 *   --claude-bin <path>       claude CLI for the SDK to drive (system
 *                             binary for vterm parity; default: bundled)
 *   --uds-socket <path>       enable UDS mode: listen on <path> for the daemon
 *                             instead of speaking Layer-1 NDJSON over stdio
 *   --store-socket <path>     shim-store socket (UDS mode; default
 *                             ~/.cache/agent-repl/sock/store.sock)
 *   --version                 print the shim version and exit
 */
import { createInterface } from "node:readline";
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import os from "node:os";
import path from "node:path";
import { UdsSession } from "./uds/uds-session.js";
import { SessionSource } from "./uds/proto.js";
import { FAKE_COMMANDS, FAKE_STATUS, createFakeQuery } from "./fake-query.js";
import {
  ModelInfo,
  PermissionMode,
  ShimEvent,
  SlashCommand,
  encodeEvent,
  isPermissionMode,
} from "./protocol.js";
import {
  CanUseToolLike,
  QueryLike,
  SdkUserMessageLike,
  SessionDeps,
  ShimSession,
} from "./session.js";

interface CliArgs {
  fake: boolean;
  sessionId: string;
  permissionMode: PermissionMode;
  cwd?: string;
  model?: string;
  resume?: string;
  /** Path to the claude CLI the SDK should drive (system binary for
   *  version parity with vterm sessions and CLI-era permission modes
   *  like `auto` that the SDK's bundled cli.js predates). */
  claudeBin?: string;
  /** UDS-mode listener path (session-<id>.sock). Present => UDS mode. */
  udsSocket?: string;
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
      case "--claude-bin":
        args.claudeBin = next();
        break;
      case "--uds-socket":
        args.udsSocket = next();
        break;
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
  return args;
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
 */
export function realQueryOptions(
  args: CliArgs,
  canUseTool: CanUseToolLike,
): Record<string, unknown> {
  return {
    canUseTool: canUseTool as never,
    includePartialMessages: true,
    permissionMode: args.permissionMode,
    systemPrompt: { type: "preset", preset: "claude_code" },
    settingSources: ["user", "project", "local"],
    ...(args.claudeBin !== undefined
      ? { pathToClaudeCodeExecutable: args.claudeBin }
      : {}),
    ...(args.cwd !== undefined ? { cwd: args.cwd } : {}),
    ...(args.model !== undefined ? { model: args.model } : {}),
    ...(args.resume !== undefined ? { resume: args.resume } : {}),
  };
}

async function realQueryFactory(
  args: CliArgs,
  prompt: AsyncIterable<SdkUserMessageLike>,
  canUseTool: CanUseToolLike,
): Promise<QueryLike> {
  const sdk = await import("@anthropic-ai/claude-agent-sdk");
  return sdk.query({
    prompt: prompt as never,
    options: realQueryOptions(args, canUseTool) as never,
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
  const sdk = await import("@anthropic-ai/claude-agent-sdk");
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
 * Re-resolve the `/status` snapshot by standing up a throwaway query and
 * reading its init handshake off the stream.
 *
 * Mirrors {@link realProbeCommands}: the idle prompt never yields, so the
 * CLI completes the init handshake — the one carrying every field a
 * `/status` panel reports — and then idles, costing one process spawn and
 * zero model tokens. Unlike the command probe there is no memoized accessor
 * to call: the whole snapshot IS the `system:init` message, so this reads it
 * straight off the probe's stream and returns it.
 */
async function realProbeStatus(args: CliArgs): Promise<unknown> {
  const sdk = await import("@anthropic-ai/claude-agent-sdk");
  const idle = (async function* (): AsyncGenerator<SdkUserMessageLike> {
    await new Promise<never>(() => {});
  })();
  const abortController = new AbortController();
  const probe = sdk.query({
    prompt: idle as never,
    options: probeQueryOptions(args, abortController) as never,
  });
  try {
    for await (const msg of probe as AsyncIterable<{ type?: string; subtype?: string }>) {
      if (msg.type === "system" && msg.subtype === "init") {
        return msg;
      }
    }
    // The stream ended before an init arrived. A probe that cannot answer is
    // surfaced as a rejection (the caller reports it as a command-scoped
    // sdk_throw), never as a silently empty status.
    throw new Error("status probe stream ended before system:init");
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
function makeCreateQuery(args: CliArgs): SessionDeps["createQuery"] {
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

export async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));

  // `--version` is a node-runnable smoke of the bundle: it loads every static
  // import (including the proto stubs and their bundled @bufbuild/protobuf) and
  // exits before touching a socket or the SDK. It must stay dependency-free.
  if (args.version) {
    process.stdout.write(`claude-shim ${packageVersion("../package.json")}\n`);
    return;
  }

  // The query factory is synchronous per SessionDeps; pre-resolve the SDK
  // module (dynamic import) before constructing the session.
  const sdkModulePromise = args.fake
    ? null
    : import("@anthropic-ai/claude-agent-sdk");
  if (sdkModulePromise) await sdkModulePromise;

  const createQuery = makeCreateQuery(args);

  // ── UDS mode (design §8, §4.4) ────────────────────────────────────────────
  // The daemon spawns the shim with `--uds-socket <path>` and CONNECTS to that
  // listener; the shim OWNS lifetime and OUTLIVES the daemon (reattach). This
  // is the path that supersedes stdio once the daemon flips to ShimUDSArgv.
  if (args.udsSocket !== undefined) {
    await runUdsMode(args, createQuery);
    return;
  }

  // ── SUPERSEDED: stdio Layer-1 NDJSON transport ─────────────────────────────
  // This entire branch DIES when the daemon flips its spawn contract from
  // ShimArgv to ShimUDSArgv (daemon/internal/server/agentshim.go): the daemon's
  // consumption cutover is a separate final-assembly task that deletes the
  // stdio path wholesale. It is retained ONLY because the resident daemon and
  // the daemon e2e harness still spawn the shim over stdio; do not remove it
  // until that cutover lands. New behavior belongs in UDS mode above.
  const deps: SessionDeps = {
    sessionId: args.sessionId,
    shimVersion: packageVersion("../package.json"),
    sdkVersion: args.fake
      ? "fake"
      : packageVersion("@anthropic-ai/claude-agent-sdk/package.json"),
    initialPermissionMode: args.permissionMode,
    createQuery,
    probeCommands: (): Promise<SlashCommand[]> =>
      args.fake ? Promise.resolve(FAKE_COMMANDS) : realProbeCommands(args),
    probeStatus: (): Promise<unknown> =>
      args.fake ? Promise.resolve(FAKE_STATUS) : realProbeStatus(args),
    emit: (evt: ShimEvent): void => {
      process.stdout.write(encodeEvent(evt));
    },
    exit: (code: number): void => {
      process.exit(code);
    },
    newRequestId: randomUUID,
  };

  const session = new ShimSession(deps);
  const rl = createInterface({ input: process.stdin, crlfDelay: Infinity });
  rl.on("line", (line) => session.handleLine(line));
  rl.on("close", () => session.handleStdinEnd());
  await session.start();
}

/**
 * Drive one UDS-mode session (design §8). The UDS server owns lifetime: a
 * daemon disconnect does NOT stop the session or the in-flight turn (reattach,
 * §4.4), and there is no stdin, so stdin-EOF is not a stop path. The explicit
 * stop path is SIGTERM/SIGINT, which cleanly shuts the session down.
 */
async function runUdsMode(args: CliArgs, createQuery: SessionDeps["createQuery"]): Promise<void> {
  const session = new UdsSession({
    sessionId: args.sessionId,
    shimVersion: packageVersion("../package.json"),
    protocolVersion: "1",
    udsSocketPath: args.udsSocket!,
    storeSocketPath: args.storeSocket ?? defaultStoreSocket(),
    // SessionStarted.source: RESUME when respawned to resume an on-disk
    // session, FRESH for a brand-new one (design §5.2 SessionSource).
    sessionSource: args.resume !== undefined ? SessionSource.RESUME : SessionSource.FRESH,
    createQuery,
    newRequestId: randomUUID,
  });
  let stopping = false;
  const stop = (sig: string): void => {
    if (stopping) return;
    stopping = true;
    void session.shutdown(sig).finally(() => process.exit(0));
  };
  process.on("SIGTERM", () => stop("SIGTERM"));
  process.on("SIGINT", () => stop("SIGINT"));
  await session.start();
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
    interrupt: async (): Promise<void> => (await queryPromise).interrupt(),
    setPermissionMode: async (mode): Promise<void> =>
      (await queryPromise).setPermissionMode(mode),
    setModel: async (model): Promise<void> => (await queryPromise).setModel(model),
    supportedModels: async (): Promise<ModelInfo[]> =>
      (await queryPromise).supportedModels(),
    supportedCommands: async (): Promise<SlashCommand[]> =>
      (await queryPromise).supportedCommands(),
  };
}

const isDirectRun =
  process.argv[1] !== undefined &&
  import.meta.url === pathToFileURL(process.argv[1]).href;
if (isDirectRun) {
  main().catch((err: unknown) => {
    process.stderr.write(`shim fatal: ${err instanceof Error ? err.stack : String(err)}\n`);
    process.exit(1);
  });
}
