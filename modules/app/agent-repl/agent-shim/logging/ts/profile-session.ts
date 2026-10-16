/**
 * The runtime profiling session every TypeScript runtime answers identically.
 *
 * bin/profile-agent-repl.sh opens a session by writing its id into
 * `<root>/active` and closes it by removing that file; a runtime reconciles on
 * SIGWINCH, starting a profile into `<root>/<id>/` or stopping one, and stops
 * by itself after `MAX_SECONDS`. While it listens it holds an armed marker,
 * `<root>/armed/<runtime>-<pid>`. `agent-shim/logging/go/profiling` holds the
 * same answer for Go and `profiling.el` for elisp;
 * `proto/vocab/profile-session.json` is the seam the three are asserted
 * against.
 */
import { join } from "node:path";

/** The environment variable overriding the profiles root. */
export const ROOT_ENV = "AGENT_REPL_PROFILE_ROOT";
/** The profiles root under the user's home. */
export const DEFAULT_ROOT = ".cache/agent-repl/profiles";
/** The file under the root that names the open session. */
export const ACTIVE_FILE = "active";
/** The directory under the root holding one marker per listening runtime. */
export const ARMED_DIR = "armed";
/** The signal a runtime reconciles on. */
export const SIGNAL = "SIGWINCH";
/** How long a profile runs before the runtime stops it by itself. */
export const MAX_SECONDS = 1800;
/**
 * The file in a session's directory naming a log level every runtime raises
 * itself to while it profiles into the session, as a level window closed when
 * the profile stops. Absent, levels are left as they are.
 */
export const LOG_LEVEL_FILE = "log-level";

const SESSION = /^[A-Za-z0-9][A-Za-z0-9._-]{0,62}$/;

/** Whether `id` names a session a runtime may write into. */
export function validSession(id: string): boolean {
  return SESSION.test(id);
}

/** The profiles root: the override when set, else `DEFAULT_ROOT` under home. */
export function profilesRoot(env: Readonly<Record<string, string | undefined>>, home: string): string {
  const override = env[ROOT_ENV];
  if (override) return override;
  if (!home) throw new Error("profiling: no home directory to put the profiles root under");
  return join(home, DEFAULT_ROOT);
}
