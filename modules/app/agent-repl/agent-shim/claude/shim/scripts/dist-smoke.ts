/**
 * scripts/dist-smoke.mjs — prove the BUILT bundle is a serving shim.
 *
 * The unit suite exercises `src/`; this exercises `dist/main.js` as the daemon
 * actually runs it — spawned as a process, with the real spawn environment, a
 * real unix socket, a real inherited fd 3, dialed by a real Connect client, and
 * stopped with a real SIGTERM. Every one of those is a place the bundle can
 * differ from the sources: an import esbuild failed to inline, a descriptor the
 * bundle never reads, a signal handler registered too late.
 *
 * It is a SCRIPT rather than a vitest case because it depends on `npm run
 * build` having run, and a suite that fails on a fresh checkout for want of a
 * build teaches people to ignore it.
 *
 * It is TypeScript and bundled before it runs (`npm run smoke`) for one
 * reason: the generated protobuf stubs are `.ts`, so a plain `node` script
 * cannot import them — and hand-writing the wire calls instead would mean the
 * smoke dialed the shim with request bytes nobody generated from the contract.
 *
 * Usage: npm run build && npm run smoke
 */
import { spawn } from "node:child_process";
import { createClient, Code, ConnectError } from "@connectrpc/connect";
import { createConnectTransport } from "@connectrpc/connect-node";
import { create } from "@bufbuild/protobuf";
import { closeSync, mkdirSync, mkdtempSync, openSync, readFileSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";
// The SAME generated descriptors the shim itself serves, through the shim's own
// single import site — so the smoke cannot dial a contract the shim does not.
import { conversationv1, shimv1 } from "../src/proto.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const bundle = path.join(here, "..", "dist", "main.js");
const scratch = mkdtempSync(path.join(os.tmpdir(), "shim-dist-smoke-"));
const listen = path.join(scratch, "shim.sock");
const storeSocket = path.join(scratch, "store.sock");
const logPath = path.join(scratch, "shim.log");
const workspace = path.join(scratch, "workspace");
const lockDir = path.join(scratch, "locks");

const failures: string[] = [];
const check = (what: string, actual: unknown, expected: unknown): void => {
  const ok = JSON.stringify(actual) === JSON.stringify(expected);
  console.log(`${ok ? "ok  " : "FAIL"}  ${what}${ok ? "" : ` — expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`}`);
  if (!ok) failures.push(what);
};

mkdirSync(workspace, { recursive: true });

const logFd = openSync(logPath, "a");
const child = spawn(
  process.execPath,
  [bundle, "--listen", listen, "--store-socket", storeSocket, "--log-fd", "3", "--fake"],
  {
    cwd: workspace,
    // fd 3 is the durable sink the shim writes its record to, inherited exactly
    // the way the daemon passes it.
    // stderr is IGNORED, not inherited: the shim mirrors every record there as
    // well as writing it durably, and inheriting it buries the smoke's own
    // output. The durable copy on fd 3 is the one this script reads, and it is
    // printed in full when a check fails.
    stdio: ["ignore", "inherit", "ignore", logFd],
    env: {
      ...process.env,
      CLAUDE_CONFIG_DIR: path.join(scratch, "account"),
      AGENT_REPL_OWNED: "1",
      SHIM_BUILD_SHA: "dist-smoke",
      AGENT_REPL_STATE_DIR: path.join(scratch, "state"),
      AGENT_REPL_LOCK_DIR: lockDir,
      AGENT_REPL_FORBID_VENDOR_CALLS: "1",
    },
  },
);
closeSync(logFd);

interface Exit {
  readonly code: number | string | null;
  readonly signal: NodeJS.Signals | null;
}
const exited = new Promise<Exit>((resolve) =>
  child.on("exit", (code, signal) => resolve({ code, signal })),
);

/** Wait for the shim to bind, by dialing until a call answers. */
async function awaitServing(client: ReturnType<typeof createClient<typeof shimv1.Shim>>): Promise<boolean> {
  for (let attempt = 0; attempt < 200; attempt++) {
    try {
      await client.getWorkflow(
        create(shimv1.GetWorkflowRequestSchema, {
        work: create(conversationv1.DetachedWorkIdSchema, { value: "probe" }),
      }),
      );
      return true;
    } catch (err) {
      // Unimplemented means the router answered: the shim IS serving.
      if (ConnectError.from(err).code === Code.Unimplemented) return true;
      await new Promise((resolve) => setTimeout(resolve, 25));
    }
  }
  return false;
}

const client = createClient(
  shimv1.Shim,
  createConnectTransport({ httpVersion: "1.1", baseUrl: "http://shim", nodeOptions: { socketPath: listen } }),
);

const serving = await awaitServing(client);
check("the built bundle binds its socket and serves", serving, true);

// A LEGAL request, so it passes validation and reaches the engine — which is
// what "answers Unimplemented" is actually about. An empty request would be
// refused earlier, by validation, and would prove nothing about the engine.
const legalStartSession = create(shimv1.StartSessionRequestSchema, {
  source: {
    case: "fresh",
    value: create(shimv1.StartSessionFreshSchema, {
      model: create(conversationv1.AgentModelSchema, { name: "claude-opus-5" }),
      permissionMode: create(conversationv1.AgentPermissionModeSchema, {
        mode: { case: "default", value: create(conversationv1.AgentPermissionModeDefaultSchema, {}) },
      }),
    }),
  },
});
const startSession = await client
  .startSession(legalStartSession)
  .then((): unknown => "resolved", (err: unknown): unknown => ConnectError.from(err).code);
check("a legal StartSession reaches the engine and answers Unimplemented", startSession, Code.Unimplemented);

const illegalStartSession = await client
  .startSession(create(shimv1.StartSessionRequestSchema, {}))
  .then((): unknown => "resolved", (err: unknown): unknown => ConnectError.from(err).code);
check(
  "an unset request oneof is refused by validation, before the engine",
  illegalStartSession,
  Code.InvalidArgument,
);

const getWorkflow = await client
  .getWorkflow(
    create(shimv1.GetWorkflowRequestSchema, {
      work: create(conversationv1.DetachedWorkIdSchema, { value: "w1" }),
    }),
  )
  .then((): unknown => "resolved", (err: unknown): unknown => ConnectError.from(err).code);
check("GetWorkflow answers Unimplemented (workflow is kicked)", getWorkflow, Code.Unimplemented);

child.kill("SIGTERM");
const outcome = await Promise.race<Exit>([
  exited,
  new Promise<Exit>((resolve) => setTimeout(() => resolve({ code: "timeout", signal: null }), 10_000)),
]);
check("SIGTERM exits 0", outcome.code, 0);

const record = readFileSync(logPath, "utf8").trim().split("\n").filter(Boolean).map((line) => JSON.parse(line) as { operation?: string; context?: Record<string, unknown> });
check(
  "the durable record reached inherited fd 3",
  record.some((entry) => entry.operation === "shim.main.lifecycle"),
  true,
);
check(
  "the record names the resolved lock directory",
  record.some((entry) => entry.context?.lock_dir === lockDir),
  true,
);
check(
  "the stand-down was recorded",
  record.some((entry) => entry.context?.outcome === "graceful_stand_down_complete"),
  true,
);

if (failures.length > 0) {
  console.error(`\nthe shim's durable record (fd 3):\n${readFileSync(logPath, "utf8")}`);
  console.error(`dist smoke FAILED: ${failures.join("; ")}`);
  process.exit(1);
}
console.log("\ndist smoke passed");
