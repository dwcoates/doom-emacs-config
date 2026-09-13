/**
 * trust.ts — THE WORKSPACE IS TRUSTED UNDER ITS ACCOUNT ROOT BEFORE THE VENDOR
 * IS SPAWNED.
 *
 * The vendor CLI keeps a per-directory trust decision in
 * `<config_root>/.claude.json` under `projects.<dir>.hasTrustDialogAccepted`.
 * A directory it has never been told to trust is run in a DEGRADED posture,
 * and the only place it says so is one line on the child's stderr:
 *
 *   Ignoring 2 permissions.allow entries from .claude/settings.json: this
 *   workspace has not been trusted. Run Claude Code interactively here once
 *   and accept the trust dialog, or set
 *   projects["<dir>"].hasTrustDialogAccepted: true in <config_root>/.claude.json.
 *
 * That is the vendor prescribing this exact key, so writing it is the
 * supported pre-acceptance and not a guess: the SDK exposes no trust option
 * (`Options` in `sdk.d.ts` has none) and the CLI has no `--trust` flag.
 *
 * WHY THE SHIM AND NOT THE DAEMON: the shim is the process that knows both
 * halves — its account root (`CLAUDE_CONFIG_DIR`) and its cwd — and it is the
 * process that spawns the vendor. Two sources for one fact can disagree.
 *
 * EVERY AGENT-REPL WORKSPACE IS EXPOSED TO THIS. Workspaces are git worktrees
 * agent-repl creates itself, so no human ever opened one interactively to
 * answer the dialog, and their permission allowlists were being dropped
 * silently on every bring-up.
 *
 * THE KEY IS THE REPOSITORY, NOT THE WORKTREE. Grounded against the bundled
 * binary (claude 2.1.220, 2026-09-13): with only the worktree's own path
 * trusted the warning still fires and names the MAIN repository directory;
 * with the main repository trusted it goes away. So the entry this module
 * writes is keyed by {@link trustRoot} — the main worktree of the repository
 * the cwd belongs to, resolved from the `.git` FILE a linked worktree carries,
 * and the cwd itself for everything else.
 */
import { existsSync, readFileSync, renameSync, statSync, writeFileSync, chmodSync } from "node:fs";
import path from "node:path";
import { bindLog } from "./log.js";

const LOGGER = bindLog({ component: "shim-trust", operation: "shim.trust" });

/** The vendor's per-account config file, relative to an account root. */
export const VENDOR_CONFIG_FILE = ".claude.json";

/** The vendor's per-directory trust key. */
export const TRUST_KEY = "hasTrustDialogAccepted";

/** A linked worktree's `.git` file states its git dir on this prefix. */
const GITDIR_PREFIX = "gitdir:";

/** A linked worktree's git dir lives under `<main>/.git/worktrees/<name>`. */
const WORKTREES_SEGMENT = `${path.sep}.git${path.sep}worktrees${path.sep}`;

/**
 * The directory the vendor keys its trust decision by, for a session whose cwd
 * is `cwd`.
 *
 * A LINKED WORKTREE IS THE REPOSITORY'S, NOT ITS OWN. Its `.git` is a file
 * reading `gitdir: <main>/.git/worktrees/<name>`, and the vendor treats the
 * repository's main worktree — `<main>` — as the project. Anything else, an
 * ordinary checkout or a plain directory, is its own root.
 *
 * NO GIT PROCESS IS RUN. The answer is a file's contents, and a shim that
 * shelled out for it would make the spawn path depend on a subprocess that can
 * itself hang, in the one place a hang is already the failure being fixed.
 */
export function trustRoot(cwd: string): string {
  const dotGit = path.join(cwd, ".git");
  let contents: string;
  try {
    if (!statSync(dotGit).isFile()) return cwd;
    contents = readFileSync(dotGit, "utf8");
  } catch {
    // No `.git` at all, or one this process may not read: the cwd is its own
    // root. A trust entry on the cwd is never WRONG, only possibly unused.
    return cwd;
  }
  const line = contents.trim();
  if (!line.startsWith(GITDIR_PREFIX)) return cwd;
  const gitDir = line.slice(GITDIR_PREFIX.length).trim();
  const at = gitDir.indexOf(WORKTREES_SEGMENT);
  if (at < 0) return cwd;
  return gitDir.slice(0, at);
}

/** What {@link ensureWorkspaceTrusted} did, for the caller's own record. */
export type TrustOutcome = "already_trusted" | "granted";

/**
 * Ensure `<configDir>/.claude.json` trusts the workspace `cwd` belongs to.
 *
 * READ-MODIFY-WRITE, NEVER CLOBBER. The file is the account's whole state —
 * history, onboarding, per-project data — so the write parses what is there,
 * touches exactly one key, and re-serializes with the indentation the file
 * already used. A file that is missing is created with just this entry; a file
 * that will not parse is left ALONE and the failure is surfaced, because
 * rewriting an account file we cannot read is how a config gets destroyed.
 *
 * ATOMIC. The new text lands on a temp file beside the original, inherits its
 * mode, and is renamed over it, so a reader (the vendor itself rewrites this
 * file during its own bring-up) sees either the old file or the new one.
 */
export function ensureWorkspaceTrusted(configDir: string, cwd: string): TrustOutcome {
  const root = trustRoot(cwd);
  const file = path.join(configDir, VENDOR_CONFIG_FILE);
  const raw = readVendorConfig(file);
  const parsed = raw === undefined ? {} : parseVendorConfig(file, raw);
  const projects = asRecord(parsed["projects"]) ?? {};
  const project = asRecord(projects[root]) ?? {};
  if (project[TRUST_KEY] === true) {
    LOGGER.debug(
      { workspace_dir: cwd, trust_root: root, config_file: file },
      "the workspace's repository is already trusted under this account root",
    );
    return "already_trusted";
  }
  const next = {
    ...parsed,
    projects: { ...projects, [root]: { ...project, [TRUST_KEY]: true } },
  };
  writeVendorConfig(file, next, indentOf(raw));
  LOGGER.info(
    { workspace_dir: cwd, trust_root: root, config_file: file },
    "granted the workspace's repository folder trust under this account root; " +
      "without it the vendor silently drops the workspace's permission allowlists",
  );
  return "granted";
}

/** The file's text, or absence when the account has no config file yet. */
function readVendorConfig(file: string): string | undefined {
  try {
    return readFileSync(file, "utf8");
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code === "ENOENT") return undefined;
    throw new Error(
      `shim trust: reading ${file} failed: ${err instanceof Error ? err.message : String(err)}`,
      { cause: err },
    );
  }
}

/** The file's object, or a raise naming the file that would have been lost. */
function parseVendorConfig(file: string, raw: string): Record<string, unknown> {
  if (raw.trim() === "") return {};
  let value: unknown;
  try {
    value = JSON.parse(raw);
  } catch (err) {
    throw new Error(
      `shim trust: ${file} is not valid JSON, so the trust entry was not written: ${err instanceof Error ? err.message : String(err)}`,
      { cause: err },
    );
  }
  const record = asRecord(value);
  if (record === undefined) {
    throw new Error(`shim trust: ${file} does not hold a JSON object, so the trust entry was not written`);
  }
  return record;
}

/** The file's own indentation, so a hand-readable config stays hand-readable. */
function indentOf(raw: string | undefined): number {
  if (raw === undefined) return 2;
  const match = /\n(\s+)"/.exec(raw);
  if (match === null) return 0;
  const indent = match[1];
  return indent === undefined || indent.includes("\t") ? 2 : indent.length;
}

/** The whole file, replaced by rename so no reader ever sees a partial one. */
function writeVendorConfig(file: string, value: Record<string, unknown>, indent: number): void {
  const text = `${JSON.stringify(value, undefined, indent)}\n`;
  const temp = `${file}.agent-repl-trust.${process.pid}`;
  writeFileSync(temp, text, "utf8");
  if (existsSync(file)) {
    try {
      chmodSync(temp, statSync(file).mode & 0o7777);
    } catch (err) {
      LOGGER.debug(
        { config_file: file, detail: err instanceof Error ? err.message : String(err) },
        "could not carry the config file's mode onto the replacement; the default mode stands",
      );
    }
  }
  renameSync(temp, file);
}

/** A plain object, or absence for everything else (null and arrays included). */
function asRecord(value: unknown): Record<string, unknown> | undefined {
  if (typeof value !== "object" || value === null || Array.isArray(value)) return undefined;
  return value as Record<string, unknown>;
}
