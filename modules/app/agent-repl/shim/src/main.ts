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
 */
import { createInterface } from "node:readline";
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import { createFakeQuery } from "./fake-query.js";
import {
  ModelInfo,
  PermissionMode,
  ShimEvent,
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

export async function main(): Promise<void> {
  const args = parseArgs(process.argv.slice(2));

  // The query factory is synchronous per SessionDeps; pre-resolve the SDK
  // module (dynamic import) before constructing the session.
  const sdkModulePromise = args.fake
    ? null
    : import("@anthropic-ai/claude-agent-sdk");
  if (sdkModulePromise) await sdkModulePromise;

  const deps: SessionDeps = {
    sessionId: args.sessionId,
    shimVersion: packageVersion("../package.json"),
    sdkVersion: args.fake
      ? "fake"
      : packageVersion("@anthropic-ai/claude-agent-sdk/package.json"),
    initialPermissionMode: args.permissionMode,
    createQuery: (prompt, canUseTool): QueryLike => {
      if (args.fake) {
        return createFakeQuery(prompt, canUseTool, {
          sessionId: args.sessionId,
          newUuid: randomUUID,
          ...(args.resume !== undefined ? { resume: args.resume } : {}),
        });
      }
      return lazyQuery(realQueryFactory(args, prompt, canUseTool));
    },
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
