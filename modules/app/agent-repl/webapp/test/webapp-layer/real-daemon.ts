/**
 * THE REAL DAEMON, AS THIS CHILD PROCESS RECEIVES IT.
 *
 * The Go e2e world brings up the quartet and passes four values in the
 * environment (`e2e/WEBAPP-LAYER-SPEC.md` section A). This module is the only
 * place that reads them, so a missing prerequisite is reported once, loudly,
 * naming the invocation that supplies it — never as a mysterious transport
 * failure fifty assertions later, and never as a quiet pass.
 */
import { startAppAgainst, type MountedApp, type HarnessOptions } from "../integration/harness";

/** The gate: set only by the Go driver that owns the daemon's lifecycle. */
const GATE = "AGENT_REPL_WEBAPP_LAYER";
/** `http://<host:port>` read from the daemon's own `daemon.addr`. */
const DAEMON_URL = "AGENT_REPL_E2E_DAEMON_URL";
/** The workspace `RegisterWorkspace` minted for this run. */
const WORKSPACE_ID = "AGENT_REPL_E2E_WORKSPACE_ID";
const WORKSPACE_DIR = "AGENT_REPL_E2E_WORKSPACE_DIR";

const HOW_TO_RUN =
  "the webapp e2e layer is driven by the Go cross-system world, which owns the real " +
  "store/sidecar/daemon/shim lifecycle: run it as " +
  "`go test ./modules/app/agent-repl/e2e -run TestWebappLayer`, not as a bare vitest invocation";

export interface RealDaemon {
  /** The daemon's base url, as the app's transport is pointed at it. */
  readonly baseUrl: string;
  readonly workspaceId: string;
  readonly workspaceDir: string;
}

function required(name: string): string {
  const value = process.env[name];
  if (value === undefined || value === "") {
    throw new Error(`${name} is unset: ${HOW_TO_RUN}`);
  }
  return value;
}

/**
 * The daemon this run was handed.
 *
 * THROWS rather than skips when the gate is absent. A skip would let a run
 * with no daemon report green with zero assertions, which is exactly the
 * failure this layer exists to make impossible.
 */
export function realDaemon(): RealDaemon {
  if (process.env[GATE] !== "1") {
    throw new Error(`${GATE} is not set: ${HOW_TO_RUN}`);
  }
  return {
    baseUrl: required(DAEMON_URL),
    workspaceId: required(WORKSPACE_ID),
    workspaceDir: required(WORKSPACE_DIR),
  };
}

/**
 * Mount the real app against the real daemon, addressed to the real
 * workspace, through the SAME mount path the fake-daemon integration suite
 * uses (`startAppAgainst`). Nothing is stubbed between a component and the
 * wire; the wire's far end is a live `claude-repld`.
 */
export async function startAgainstRealDaemon(
  options: Omit<HarnessOptions, "workspaceId" | "workspaceDir" | "arrange"> = {},
): Promise<MountedApp> {
  const daemon = realDaemon();
  return startAppAgainst(daemon.baseUrl, {
    ...options,
    workspaceId: daemon.workspaceId,
    workspaceDir: daemon.workspaceDir,
  });
}
