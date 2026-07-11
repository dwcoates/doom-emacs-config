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
 */
import { createInterface } from "node:readline";
import { randomUUID } from "node:crypto";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import { createFakeQuery } from "./fake-query.js";
import { PermissionMode, ShimEvent, encodeEvent } from "./protocol.js";
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
        if (
          mode !== "default" &&
          mode !== "acceptEdits" &&
          mode !== "bypassPermissions" &&
          mode !== "plan"
        ) {
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

async function realQueryFactory(
  args: CliArgs,
  prompt: AsyncIterable<SdkUserMessageLike>,
  canUseTool: CanUseToolLike,
): Promise<QueryLike> {
  const sdk = await import("@anthropic-ai/claude-agent-sdk");
  return sdk.query({
    prompt: prompt as never,
    options: {
      canUseTool: canUseTool as never,
      includePartialMessages: true,
      permissionMode: args.permissionMode,
      ...(args.cwd !== undefined ? { cwd: args.cwd } : {}),
      ...(args.model !== undefined ? { model: args.model } : {}),
      ...(args.resume !== undefined ? { resume: args.resume } : {}),
    },
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
