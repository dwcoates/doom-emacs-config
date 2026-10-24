/**
 * engine/skill-watch.ts — the watch on the directories the vendor loads its
 * skills from.
 *
 * THE VENDOR LOADS ITS SKILLS ONCE, AT ITS PROCESS'S START (measured
 * 2026-10-23, claude-agent-sdk 0.3.280: a skill written into the account's or
 * the worktree's skills directory after the start is not listed by
 * `supportedCommands()` 2s or 10s later). So a skill installed, changed or
 * removed mid-session reaches the catalog only through a new vendor process,
 * and this watch is what tells the session that one is owed
 * (`conversation.v1.SessionSkillsChanged`).
 *
 * WHAT COUNTS AS A CHANGE is what the catalog is made of: an entry of a skills
 * directory appearing or going (a skill installed, removed or relinked), and a
 * skill's own `SKILL.md`, whose frontmatter names and describes it. Any other
 * file inside a skill's directory does not count: some skills keep state there
 * (notes, last-update stamps), and a session must not be replaced every time a
 * skill writes its own state.
 *
 * KERNEL-DRIVEN, NEVER A POLL. Every directory is watched with the platform's
 * own notification (`fs.watch`), non-recursively, and a skill reached through a
 * link is watched AT ITS TARGET, since a watch on the link's directory never
 * sees a change inside the directory it points to. A skills directory that does
 * not exist yet is watched from its nearest existing ancestor, so its creation
 * is seen too.
 *
 * A WATCH THAT FAILS IS A CHANGE. A broken watch may have missed one, and the
 * replacement a change costs is cheap next to a session that silently never
 * learns its skills, so the failure is recorded at ERROR and reported as a
 * change. It is not re-armed here: the change gets this shim replaced, and the
 * replacement arms a watch of its own, where a re-arm loop on a failure that
 * persists (no descriptors left) would spin.
 */
import * as fs from "node:fs";
import * as path from "node:path";
import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-skill-watch", operation: "shim.engine.skill_watch" });

/** The file of a skill that names and describes it in the catalog. */
export const SKILL_FILE = "SKILL.md";

/** One directory watch; `close` ends it, and is safe to call twice. */
export interface DirWatch {
  close(): void;
}

/**
 * Watch DIR, non-recursively. ON_EVENT receives the name of the entry an event
 * concerns, or null when the platform did not say; ON_ERROR receives a failure
 * of the watch after it began. Throws when DIR cannot be watched at all.
 */
export type WatchDir = (dir: string, onEvent: (name: string | null) => void, onError: (err: Error) => void) => DirWatch;

/** The filesystem the watch reads; the real one, or a test's. */
export interface SkillWatchFs {
  readonly watchDir: WatchDir;
  /** PATH with every link resolved, or undefined when it does not exist. */
  resolve(target: string): string | undefined;
  /** The entries of directory DIR, or undefined when DIR is not a directory. */
  entries(dir: string): readonly string[] | undefined;
}

/** The platform's own filesystem and notification. */
export const REAL_SKILL_WATCH_FS: SkillWatchFs = {
  watchDir(dir, onEvent, onError) {
    const watcher = fs.watch(dir, { persistent: false }, (_type, name) => {
      onEvent(name === null ? null : String(name));
    });
    watcher.on("error", onError);
    return { close: () => watcher.close() };
  },
  resolve(target) {
    try {
      return fs.realpathSync(target);
    } catch {
      return undefined;
    }
  },
  entries(dir) {
    try {
      return fs.readdirSync(dir);
    } catch {
      return undefined;
    }
  },
};

/**
 * The skills directories a vendor run as CONFIG_DIR in CWD loads from: the
 * account root's own, then the worktree's.
 */
export function skillDirsOf(configDir: string, cwd: string): string[] {
  return [path.join(configDir, "skills"), path.join(cwd, ".claude", "skills")];
}

/**
 * The watch on every skills directory in ROOTS. ON_CHANGE is called with the
 * path whose change it saw, once per event; the caller decides what a change
 * means.
 */
export class SkillSetWatch {
  private readonly roots: RootWatch[];
  private started = false;

  constructor(roots: readonly string[], fsys: SkillWatchFs, onChange: (changed: string) => void) {
    this.roots = roots.map((root) => new RootWatch(root, fsys, onChange));
  }

  /** Arm every watch. A second call does nothing. */
  start(): void {
    if (this.started) return;
    this.started = true;
    for (const root of this.roots) root.arm();
  }

  /** End every watch. */
  close(): void {
    for (const root of this.roots) root.close();
  }
}

/** The watch on one skills directory, its skills, and its absence. */
class RootWatch {
  /** The watch on the skills directory itself, or on its nearest existing ancestor while it is missing. */
  private anchor: DirWatch | undefined;
  /** Each skill's watch, by its entry name, at its resolved directory. */
  private readonly skills = new Map<string, { target: string; watch: DirWatch }>();
  private closed = false;

  constructor(
    private readonly root: string,
    private readonly fsys: SkillWatchFs,
    private readonly onChange: (changed: string) => void,
  ) {}

  /** Watch the skills directory, or the nearest ancestor it would appear under. */
  arm(): void {
    if (this.closed) return;
    this.anchor?.close();
    this.anchor = undefined;
    if (this.fsys.entries(this.root) !== undefined) {
      this.anchor = this.watch(this.root, (name) => this.onRootEvent(name));
      if (this.anchor === undefined) return;
      LOGGER.debug({ skills_dir: this.root }, "watching a skills directory the vendor loads from");
      this.rescan();
      return;
    }
    this.closeSkills();
    const ancestor = this.nearestExistingAncestor();
    const next = nextComponent(ancestor, this.root);
    this.anchor = this.watch(ancestor, (name) => {
      if (name === null || name === next) this.onAppeared();
    });
    if (this.anchor === undefined) return;
    LOGGER.debug(
      { skills_dir: this.root, watching: ancestor },
      "the skills directory does not exist; watching the ancestor it would appear under",
    );
    // WATCH FIRST, THEN LOOK: the component may have appeared between the look
    // that chose this ancestor and the watch that now stands on it, an
    // appearance no event will ever report (a `mkdir -p` lands every level at
    // once). Anything after the watch began is an event; anything before it is
    // seen here and armed deeper at once, so no appearance falls between the
    // two. Whether it is a change is the caller's to say: an appearance an
    // event reported is (onAppeared), and one while the watch starts is
    // already in what the vendor is about to load.
    if (this.fsys.entries(path.join(ancestor, next)) !== undefined) {
      LOGGER.debug(
        { skills_dir: this.root, appeared: path.join(ancestor, next) },
        "the directory the ancestor's watch waits for appeared before the watch began; watching deeper",
      );
      this.arm();
    }
  }

  close(): void {
    this.closed = true;
    this.anchor?.close();
    this.anchor = undefined;
    this.closeSkills();
  }

  private onAppeared(): void {
    if (this.closed) return;
    this.arm();
    if (this.fsys.entries(this.root) !== undefined) this.onChange(this.root);
  }

  private onRootEvent(name: string | null): void {
    if (this.closed) return;
    if (this.fsys.entries(this.root) === undefined) {
      // THE DIRECTORY ITSELF WENT: every skill it held went with it.
      this.onChange(this.root);
      this.arm();
      return;
    }
    this.onChange(name === null ? this.root : path.join(this.root, name));
    this.rescan();
  }

  /** Watch each skill the directory holds now, at its target, and drop the ones it no longer does. */
  private rescan(): void {
    const present = new Map<string, string>();
    for (const entry of this.fsys.entries(this.root) ?? []) {
      const target = this.fsys.resolve(path.join(this.root, entry));
      if (target !== undefined && this.fsys.entries(target) !== undefined) present.set(entry, target);
    }
    for (const [entry, held] of this.skills) {
      if (present.get(entry) !== held.target) {
        held.watch.close();
        this.skills.delete(entry);
      }
    }
    for (const [entry, target] of present) {
      if (this.skills.has(entry)) continue;
      const watch = this.watch(target, (name) => {
        if (this.closed) return;
        if (name === null || name === SKILL_FILE) this.onChange(path.join(target, SKILL_FILE));
      });
      if (watch !== undefined) this.skills.set(entry, { target, watch });
    }
  }

  private closeSkills(): void {
    for (const held of this.skills.values()) held.watch.close();
    this.skills.clear();
  }

  /**
   * Watch DIR. A directory that cannot be watched, at the start or later, is
   * a change the watch may have missed: recorded at ERROR and reported.
   */
  private watch(dir: string, onEvent: (name: string | null) => void): DirWatch | undefined {
    const failed = (err: Error, when: string): void => {
      if (this.closed) return;
      LOGGER.error(
        { skills_dir: this.root, watched: dir, when, cause: err.message },
        "a watch on the vendor's skills failed; reporting it as a change, which gets the shim replaced",
      );
      this.onChange(dir);
    };
    try {
      return this.fsys.watchDir(dir, onEvent, (err) => failed(err, "after it began"));
    } catch (err) {
      failed(err instanceof Error ? err : new Error(String(err)), "at its start");
      return undefined;
    }
  }

  private nearestExistingAncestor(): string {
    let dir = path.dirname(this.root);
    while (this.fsys.entries(dir) === undefined) {
      const parent = path.dirname(dir);
      if (parent === dir) return dir;
      dir = parent;
    }
    return dir;
  }
}

/** The first path component of DESCENDANT below ANCESTOR. */
function nextComponent(ancestor: string, descendant: string): string {
  const rest = path.relative(ancestor, descendant);
  return rest.split(path.sep)[0] ?? rest;
}
