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
 * build teaches people to ignore it. Nothing here imports vitest — the store
 * fake it drives (`test/fakes/store-server.ts`) is deliberately vitest-free for
 * exactly this reason, so bundling it in costs the smoke no dependency.
 *
 * It is TypeScript and bundled before it runs (`npm run smoke`) for one
 * reason: the generated protobuf stubs are `.ts`, so a plain `node` script
 * cannot import them — and hand-writing the wire calls instead would mean the
 * smoke dialed the shim with request bytes nobody generated from the contract.
 *
 * TWO STEPS, and the second is not an afterthought. A session produces store
 * rows from the moment it starts (the mcp-server healths and the fast-mode
 * state among them), and the graceful stand-down WAITS FOR ALL ACKS before
 * exiting — so the exit code is a statement about the record's completeness:
 *   1. WITH a store that answers, SIGTERM exits 0 and the record says the
 *      stand-down completed.
 *   2. With NO store at all, the same SIGTERM exits NONZERO and the record says
 *      it stood down with writes the store never acked. That is the engine
 *      behaving correctly, and asserting it here is what stops a future "fix"
 *      from making a lost record look clean.
 *
 * Usage: npm run build && npm run smoke
 */
import { spawn, type ChildProcess } from "node:child_process";
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
import { startFakeStore, type FakeStore } from "../test/fakes/store-server.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const bundle = path.join(here, "..", "dist", "main.js");

const failures: string[] = [];
const check = (what: string, actual: unknown, expected: unknown): void => {
  const ok = JSON.stringify(actual) === JSON.stringify(expected);
  console.log(`${ok ? "ok  " : "FAIL"}  ${what}${ok ? "" : ` — expected ${JSON.stringify(expected)}, got ${JSON.stringify(actual)}`}`);
  if (!ok) failures.push(what);
};

interface Exit {
  readonly code: number | string | null;
  readonly signal: NodeJS.Signals | null;
}

/** One spawned shim, its scratch tree, and the handles the checks need. */
interface Spawned {
  readonly child: ChildProcess;
  readonly exited: Promise<Exit>;
  readonly logPath: string;
  readonly lockDir: string;
  readonly storeSocket: string;
  readonly client: ReturnType<typeof createClient<typeof shimv1.Shim>>;
}

/** Spawn `dist/main.js` exactly the way the daemon does, in a fresh scratch tree. */
function spawnShim(tag: string): Spawned {
  const scratch = mkdtempSync(path.join(os.tmpdir(), `shim-dist-smoke-${tag}-`));
  // The daemon names this socket after the workspace id, and the shim reads
  // `workspace_id` back off the basename, so the smoke spells it the same way.
  const listen = path.join(scratch, "00000000000000a2.sock");
  const storeSocket = path.join(scratch, "store.sock");
  const logPath = path.join(scratch, "shim.log");
  const workspace = path.join(scratch, "workspace");
  const lockDir = path.join(scratch, "locks");
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
  const exited = new Promise<Exit>((resolve) =>
    child.on("exit", (code, signal) => resolve({ code, signal })),
  );
  const client = createClient(
    shimv1.Shim,
    createConnectTransport({ httpVersion: "1.1", baseUrl: "http://shim", nodeOptions: { socketPath: listen } }),
  );
  return { child, exited, logPath, lockDir, storeSocket, client };
}

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

/** SIGTERM, then the exit — with a ceiling so a wedged stand-down still reports. */
async function sigtermAndWait(shim: Spawned): Promise<Exit> {
  shim.child.kill("SIGTERM");
  return Promise.race<Exit>([
    shim.exited,
    new Promise<Exit>((resolve) => setTimeout(() => resolve({ code: "timeout", signal: null }), 30_000)),
  ]);
}

/** One parsed line of the shim's durable record. */
interface Record_ {
  readonly operation?: string;
  readonly message?: string;
  readonly context?: Readonly<globalThis.Record<string, unknown>>;
}

/** The shim's durable record, parsed. */
function readRecord(logPath: string): Record_[] {
  return readFileSync(logPath, "utf8")
    .trim()
    .split("\n")
    .filter(Boolean)
    .map((line) => JSON.parse(line) as Record_);
}

// ---------------------------------------------------------------------------
// STEP 1 — a store that answers: the whole serving surface, and a clean exit.
// ---------------------------------------------------------------------------
console.log("— step 1: with a store that acks —");
const withStore = spawnShim("acked");
const store: FakeStore = await startFakeStore(withStore.storeSocket);

const serving = await awaitServing(withStore.client);
check("the built bundle binds its socket and serves", serving, true);

// A REAL StartSession over the mocked vendor. The mock has landed, so this
// exercises the whole path the daemon takes: validation, the session lock, the
// pre-minted vendor session id, the query, the vendor's own init, and the
// SessionStarted the daemon reads its readiness and build staleness from.
const started = await withStore.client
  .startSession(legalStartSession)
  .then(
    (response): unknown => response.result.case,
    (err: unknown): unknown => `threw ${String(ConnectError.from(err).code)}`,
  );
check("a legal StartSession over --fake starts a real session", started, "success");

const session = await withStore.client.startSession(legalStartSession).then(
  (response): unknown =>
    response.result.case === "failure" ? response.result.value.cause.case : response.result.case,
  (err: unknown): unknown => `threw ${String(ConnectError.from(err).code)}`,
);
check("a SECOND StartSession is refused as already started", session, "alreadyStarted");

// The readiness signal, and the thing a Go client observes at its first
// Receive: the stream's FIRST frame is diagnostics.
const firstWatchFrame = await (async (): Promise<unknown> => {
  for await (const response of withStore.client.watchSession(create(shimv1.WatchSessionRequestSchema, {}))) {
    return response.frame.case === "update" ? response.frame.value.update.case : response.frame.case;
  }
  return "ended";
})();
check("WatchSession's first frame is diagnostics", firstWatchFrame, "diagnostics");

const illegalStartSession = await withStore.client
  .startSession(create(shimv1.StartSessionRequestSchema, {}))
  .then((): unknown => "resolved", (err: unknown): unknown => ConnectError.from(err).code);
check(
  "an unset request oneof is refused by validation, before the engine",
  illegalStartSession,
  Code.InvalidArgument,
);

const getWorkflow = await withStore.client
  .getWorkflow(
    create(shimv1.GetWorkflowRequestSchema, {
      work: create(conversationv1.DetachedWorkIdSchema, { value: "w1" }),
    }),
  )
  .then((): unknown => "resolved", (err: unknown): unknown => ConnectError.from(err).code);
check("GetWorkflow answers Unimplemented (workflow is kicked)", getWorkflow, Code.Unimplemented);

// The premise of step 2: a started session really does produce store rows.
check("the started session wrote rows the store had to ack", store.writes().length > 0, true);

const ackedExit = await sigtermAndWait(withStore);
check("SIGTERM exits 0", ackedExit.code, 0);

const record = readRecord(withStore.logPath);
check(
  "the durable record reached inherited fd 3",
  record.some((entry) => entry.operation === "shim.main.lifecycle"),
  true,
);
check(
  "the record names the resolved lock directory",
  record.some((entry) => entry.context?.lock_dir === withStore.lockDir),
  true,
);
check(
  "the stand-down was recorded",
  record.some((entry) => entry.context?.outcome === "graceful_stand_down_complete"),
  true,
);
if (failures.length > 0) {
  console.error(`\nstep 1's durable record (fd 3):\n${readFileSync(withStore.logPath, "utf8")}`);
}

await store.close();

// ---------------------------------------------------------------------------
// STEP 2 — no store at all: the SAME stand-down must exit NONZERO and say why.
// ---------------------------------------------------------------------------
console.log("\n— step 2: with no store socket at all —");
const noStore = spawnShim("unacked");
check("the bundle serves with no store reachable", await awaitServing(noStore.client), true);

const noStoreStarted = await noStore.client.startSession(legalStartSession).then(
  (response): unknown => response.result.case,
  (err: unknown): unknown => `threw ${String(ConnectError.from(err).code)}`,
);
check("a session still starts with no store reachable", noStoreStarted, "success");

const unackedExit = await sigtermAndWait(noStore);
check("SIGTERM with unacked writes exits nonzero", unackedExit.code, 1);

const noStoreRecord = readRecord(noStore.logPath);
check(
  "the lost-write stand-down is recorded",
  noStoreRecord.some((entry) => entry.context?.outcome === "stand_down_with_lost_writes"),
  true,
);
check(
  "the record says the store never acked",
  noStoreRecord.some(
    (entry) => entry.message === "stood down with writes the store never acked; exiting nonzero",
  ),
  true,
);

if (failures.length > 0) {
  console.error(`\nstep 2's durable record (fd 3):\n${readFileSync(noStore.logPath, "utf8")}`);
  console.error(`dist smoke FAILED: ${failures.join("; ")}`);
  process.exit(1);
}
console.log("\ndist smoke passed");
