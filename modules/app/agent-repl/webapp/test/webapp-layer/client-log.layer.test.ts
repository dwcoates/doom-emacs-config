/**
 * CLIENT LOG (§2.15) — the browser's record, in the daemon's own log file.
 *
 * This is the one thing the whole logging contract exists for, and until now
 * it was proven only against the FAKE daemon: a record emitted in the page
 * reaching a durable sink nobody in the browser could have written, carrying
 * the identity that joins it to every other runtime's records for the same
 * session.
 *
 * The chain here is entirely real:
 *
 *   log() in the page -> ForwardingLogger -> ClientLog over the real transport
 *     -> real claude-repld -> dlog -> <workspace>/.claude/emacs/webapp.log
 *
 * This file therefore mounts with production's own sink
 * (`bootLayer({ clientLog: true })`, the per-file opt-in whose cost is
 * measured in `setup.ts`), drives a real turn so the workspace HAS a session
 * and a vendor conversation to be identified by, and then reads the daemon's
 * file back off disk.
 *
 * THE CROSS-RUNTIME JOIN IS THE GO DRIVER'S (`e2e/webapplayer_e2e_test.go`,
 * TestWebappLayerClientLog): it takes the record this file plants and matches
 * its promoted `agent_repl_session_id` against the daemon's own records and
 * its `claude_session_id` against the shim's. What is asserted HERE is the
 * half only the page can see — that the page's own record arrived at all,
 * carrying what the page bound.
 */
import { readFileSync } from "node:fs";
import { afterAll, beforeAll, expect, it } from "vitest";

import type { MountedApp } from "../integration/harness";
import { log } from "../../src/log";
import { BOOT_BUDGET_MS, TURN_TEST_MS, awaitDrawn, bootLayer, rows, submit, textOf } from "./drive";
import { realDaemon, workspaceLogPath } from "./real-daemon";

/**
 * The operation the planted record carries.
 *
 * SHARED WITH THE GO DRIVER BY VALUE: `wlClientLogOperation` in
 * `e2e/webapplayer_e2e_test.go` is this same string, and it is how the driver
 * finds the record this page wrote among the boot's own.
 */
const OPERATION = "webapp-layer.client-log-round-trip";
const MESSAGE = "the webapp layer planted a record for the round trip";

/**
 * How long the planted record is given to appear in the daemon's file.
 *
 * REUSED, NOT MINTED: one real unary `ClientLog` round trip plus the daemon's
 * own write, which is strictly less than the turn this same file already
 * drove, so it takes the turn's budget rather than inventing a number.
 */
const FORWARD_BUDGET_MS = 5_000;

/** The whole arrangement: a boot, a real turn, and the planted record. */
const ARRANGE_MS = BOOT_BUDGET_MS + TURN_TEST_MS + FORWARD_BUDGET_MS;

/** One JSONL record of a daemon log sink, as logging-contract.md shapes it. */
interface DaemonLogRecord {
  runtime?: string;
  level?: string;
  operation?: string;
  message?: string;
  pid?: number;
  context?: Record<string, unknown>;
  workspace_id?: string;
  workspace_dir?: string;
  agent_repl_session_id?: string;
  claude_session_id?: string;
}

let app: MountedApp;
let planted: DaemonLogRecord;

/** Every record the workspace's `webapp.log` holds; a missing file is none. */
function webappLog(): DaemonLogRecord[] {
  let text: string;
  try {
    text = readFileSync(workspaceLogPath(realDaemon().workspaceDir, "webapp"), "utf8");
  } catch (err) {
    // ONLY "the daemon has not written this sink yet" reads as no records. Any
    // other failure (a permission fault, a directory where the file should be)
    // is this assertion's own evidence and is raised, never absorbed.
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return [];
    throw err;
  }
  return text
    .split("\n")
    .filter((line) => line.trim() !== "")
    .map((line) => JSON.parse(line) as DaemonLogRecord);
}

/**
 * Settle until `predicate` finds a record in the daemon's `webapp.log`.
 *
 * NEVER A SLEEP. Each round hands the event loop back so the real socket can
 * deliver, and advances the page's own fake clock by the throttle's window so
 * a buffered record is RELEASED rather than waited out — the throttle holds
 * every non-error record for two seconds, which is the page's timer, not real
 * time. The loop ends on the first round the record is there.
 */
async function awaitForwarded(
  what: string,
  predicate: (record: DaemonLogRecord) => boolean,
  budgetMs = FORWARD_BUDGET_MS,
): Promise<DaemonLogRecord> {
  const deadline = Date.now() + budgetMs;
  for (;;) {
    await app.settle();
    const found = webappLog().find(predicate);
    if (found !== undefined) return found;
    if (Date.now() >= deadline) {
      throw new Error(
        `${what} never reached the daemon's webapp.log within ${budgetMs}ms; ` +
          `operations forwarded: [${webappLog().map((r) => r.operation ?? "?").join(", ")}]`,
      );
    }
    await app.tick(2_000);
  }
}

beforeAll(async () => {
  // Arrange — the page mounts with production's ClientLog sink, so every
  // record it emits from here on travels the real rpc.
  app = await bootLayer({ clientLog: true });

  // A REAL TURN FIRST, because the identity is what this file is about: a
  // workspace with no session has no session id, and no vendor conversation
  // has no claude session id, so a record planted before the turn would
  // honestly carry neither.
  const prompt = "webapp layer client log";
  await submit(app, prompt);
  await awaitDrawn(app, "the response row", () =>
    rows(app, "activity", "response").some((row) => textOf(row).includes(`echo: ${prompt}`)),
  );

  // THE PAGE SAYS WHEN IT HAS ITS IDENTITY, AND IT SAYS IT IN THE DAEMON'S
  // OWN LOG. `lifecycle.bindSessionIdentity` binds the identity the daemon
  // pushed and logs that it did; waiting for THAT record to land with the
  // identity promoted is how this file knows the next record it plants will
  // carry it. Nothing here polls the clock or sleeps.
  await awaitForwarded(
    "the page's own record of binding its session identity",
    (record) =>
      record.operation === "lifecycle.session_identity" &&
      (record.agent_repl_session_id ?? "") !== "" &&
      (record.claude_session_id ?? "") !== "",
  );

  // Act — one record, through the page's canonical API. `error` is the level
  // the throttle releases immediately, so the record does not wait on a window
  // this file would then have to advance past.
  log("error", MESSAGE, { operation: OPERATION, context: { planted_by: "client-log.layer" } });

  planted = await awaitForwarded("the planted record", (record) => record.operation === OPERATION);
}, ARRANGE_MS);

afterAll(async () => {
  await app?.stop();
});

it("forwards a record emitted in the page into the daemon's webapp.log", () => {
  // Assert — the message is the page's own, verbatim; nothing but this page
  // could have written it into a file the browser cannot reach.
  expect(planted.message).toBe(MESSAGE);
});

it("files the forwarded record under the webapp runtime", () => {
  // Assert — a forwarded record keeps the SENDING runtime's name, so a webapp
  // line in webapp.log still says webapp (daemon/internal/dlog/record.go).
  expect(planted.runtime).toBe("webapp");
});

it("promotes the page's agent-repl session id to a top-level field", () => {
  // Assert — the identity lives in its own field, never only inside context,
  // which is what makes the record joinable to the daemon's own.
  expect(planted.agent_repl_session_id).toMatch(/\S/);
});

it("promotes the page's claude session id to a top-level field", () => {
  // Assert — the vendor conversation the turn opened, as the page received it.
  expect(planted.claude_session_id).toMatch(/\S/);
});

it("keeps the record's identity out of the context object", () => {
  // Assert — promotion MOVES the identity; a copy left in context is how two
  // readers of the same record end up disagreeing about which is canonical.
  expect(planted.context?.agent_repl_session_id).toBeUndefined();
});

it("keeps the vendor tripwire set while the real sink is installed", () => {
  // Assert — a real ClientLog sink reaches the daemon and nothing else. The
  // standing tripwire is still armed, so a component that reached for a
  // network vendor would find it (the vendor here is the fake SDK inside the
  // real shim, and only that).
  expect(process.env.AGENT_REPL_FORBID_VENDOR_CALLS).toBe("1");
});
