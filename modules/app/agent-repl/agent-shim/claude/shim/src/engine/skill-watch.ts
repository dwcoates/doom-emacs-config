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
 * A WATCH IS LIVE ONLY ONCE IT HAS SEEN AN EVENT (measured 2026-11-23, node
 * 24 on macOS). `fs.watch` returns before the platform stream behind it has
 * started (libuv starts the FSEvents stream on a thread of its own), and a
 * change made in that gap is never reported: a file written in the same tick
 * as the watch was lost 20 times in 20, and the stream came live 14ms to
 * 3.5s later, the longest under a deploy's load. So the watch proves itself live: it
 * writes probe files into a directory of its own, watched by the same stream,
 * until one is seen (`awaitLive`). Every directory armed before the probe is
 * live then, and a write after it was seen 30 times in 30. What changed in
 * the gap is found by comparing the skills as they stood when the watch armed
 * with how they stand once it is live, and reported as a change.
 *
 * A WATCH THAT FAILS IS A CHANGE. A broken watch may have missed one, and the
 * replacement a change costs is cheap next to a session that silently never
 * learns its skills, so the failure is recorded at ERROR and reported as a
 * change. It is not re-armed here: the change gets this shim replaced, and the
 * replacement arms a watch of its own, where a re-arm loop on a failure that
 * persists (no descriptors left) would spin.
 *
 * THE TEARDOWN ABANDONS THE WATCH, IT DOES NOT CLOSE IT (measured 2026-11-23,
 * node 24.2 on macOS at a load average of 9). Each `FSWatcher.close()` of a
 * directory watch makes libuv rebuild its one FSEvents stream and wait for that
 * stream's thread, with the event loop blocked: closing 29 watches took 2914ms
 * and closing 35 took 6032ms (one close up to 1061ms), against 1ms to arm them.
 * The teardown runs inside KillSession and before the process exits, so those
 * closes made KillSession miss the daemon's bound. Every teardown ends the
 * process, and the exit releases every watch (35 open watches, under 20ms), so
 * `abandon` only stops the reporting and the probe and leaves the platform
 * watches to the exit. The closes made while the session lives (a re-arm, a
 * skill gone) stay: that process goes on and must release what it replaced.
 */
import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import { bindLog } from "../log.js";

const LOGGER = bindLog("shim.engine.skill-watch", { component: "shim-skill-watch", operation: "shim.engine.skill_watch" });

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

/** A directory of the watch's own, which its liveness probe writes into. */
export interface ProbeDir {
  readonly dir: string;
  /** Write one more probe file. */
  write(): void;
  /** Remove the directory and every probe in it. */
  remove(): void;
}

/** The filesystem and clock the watch reads; the real ones, or a test's. */
export interface SkillWatchFs {
  readonly watchDir: WatchDir;
  /** PATH with every link resolved, or undefined when it does not exist. */
  resolve(target: string): string | undefined;
  /** The entries of directory DIR, or undefined when DIR is not a directory. */
  entries(dir: string): readonly string[] | undefined;
  /** When FILE last changed, in ms, or undefined when it does not exist. */
  stamp(file: string): number | undefined;
  /** A fresh directory for the liveness probe. Throws when none can be made. */
  probeDir(): ProbeDir;
  /** Run FN every MS until the answered stop. */
  every(ms: number, fn: () => void): () => void;
  /** Run FN once after MS; answers the cancel. */
  after(ms: number, fn: () => void): () => void;
  /** The clock, in ms. */
  now(): number;
}

/**
 * How often the liveness probe writes another file while it waits. The first
 * write is always lost to the gap, and each later one is a few microseconds of
 * I/O into a directory nobody else reads.
 */
export const PROBE_EVERY_MS = 10;

/**
 * The slowest completed time the watch took to come live, which the bound
 * stands on and its ERROR record names. MEASURED in production on 2026-11-23,
 * as deploys replaced every shim at a load average of 11: 3542ms (20:10Z).
 */
const LIVE_EXPECTED_MS = 3_542;

/**
 * How long the watch may take to come live. MEASURED 2026-11-23 (node 24,
 * macOS): 14-96ms idle and 14-115ms beside one merge gate (load average 12),
 * 30 watches each; up to 660ms inside the full suite itself; 1046-1293ms in
 * production as a deploy replaced every shim at once (four shims, the
 * 0.543.14 rollout); then, between 19:45Z and 20:10Z, as deploys replaced
 * every shim at a load average of 11: 1803ms, 2203ms and 3542ms, and one
 * watch that had not come live within an earlier bound of 4000ms (361 probes
 * written, so the shim's event loop was running and the platform stream was
 * late). The bound is three times the worst completed measurement,
 * LIVE_EXPECTED_MS, rounded down to the hundred. Past it, the watch is
 * recorded at ERROR and reported as a change, as a failed watch is.
 */
export const LIVE_BOUND_MS = 10_600;

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
  stamp(file) {
    try {
      return fs.statSync(file).mtimeMs;
    } catch {
      return undefined;
    }
  },
  probeDir() {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), "agent-repl-skill-probe-"));
    let written = 0;
    return {
      dir,
      write: () => {
        written += 1;
        fs.writeFileSync(path.join(dir, `probe-${written}`), "");
      },
      remove: () => fs.rmSync(dir, { recursive: true, force: true }),
    };
  },
  every(ms, fn) {
    const timer = setInterval(fn, ms);
    timer.unref();
    return () => clearInterval(timer);
  },
  after(ms, fn) {
    const timer = setTimeout(fn, ms);
    timer.unref();
    return () => clearTimeout(timer);
  },
  now: () => performance.now(),
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
  /**
   * Ends the liveness probe while it waits, closing its platform watch only
   * when RELEASE says to; undefined once it has settled.
   */
  private endProbe: ((release: boolean) => void) | undefined;
  private abandoned = false;

  constructor(
    roots: readonly string[],
    private readonly fsys: SkillWatchFs,
    private readonly onChange: (changed: string) => void,
  ) {
    this.roots = roots.map((root) => new RootWatch(root, fsys, onChange));
  }

  /** Arm every watch, then prove it live (`awaitLive`). A second call does nothing. */
  start(): void {
    if (this.started) return;
    this.started = true;
    for (const root of this.roots) root.arm();
    this.awaitLive(this.roots.map((root) => root.inventory()));
  }

  /**
   * End the reporting for good, for the teardown that precedes the process's
   * exit: nothing is reported after it, the probe stops and its directory is
   * removed, and no platform watch is closed, since the exit releases them all
   * (see the file header). A second call does nothing.
   */
  abandon(): void {
    if (this.abandoned) return;
    this.abandoned = true;
    this.endProbe?.(false);
    for (const root of this.roots) root.abandon();
  }

  /**
   * THE WATCH IS LIVE ONCE THE PROBE IS SEEN (see the file header). BEFORE is
   * every root's inventory as it stood when the watch armed; once live, each
   * root whose inventory moved is re-armed and its changes reported. Past
   * LIVE_BOUND_MS, or when the probe cannot run at all, every root is reported
   * as a change, as a failed watch is.
   */
  private awaitLive(before: readonly Inventory[]): void {
    const startedAt = this.fsys.now();
    let probe: ProbeDir;
    let watch: DirWatch | undefined;
    try {
      probe = this.fsys.probeDir();
    } catch (err) {
      this.probeFailed("making its directory", err);
      return;
    }
    let probes = 0;
    const write = (): void => {
      probes += 1;
      try {
        probe.write();
      } catch (err) {
        settle();
        this.probeFailed("writing a probe", err);
      }
    };
    const stops: (() => void)[] = [];
    const settle = (release = true): void => {
      if (this.endProbe === undefined) return;
      this.endProbe = undefined;
      for (const stop of stops) stop();
      if (release) watch?.close();
      probe.remove();
    };
    this.endProbe = settle;
    try {
      watch = this.fsys.watchDir(
        probe.dir,
        () => {
          if (this.endProbe === undefined) return;
          settle();
          LOGGER.info(
            { live_ms: Math.round(this.fsys.now() - startedAt), probes },
            "the watch on the vendor's skills is live",
          );
          this.catchUp(before);
        },
        (err) => {
          if (this.endProbe === undefined) return;
          settle();
          this.probeFailed("watching its directory", err);
        },
      );
    } catch (err) {
      settle();
      this.probeFailed("watching its directory", err);
      return;
    }
    stops.push(this.fsys.after(LIVE_BOUND_MS, () => {
      if (this.endProbe === undefined) return;
      settle();
      LOGGER.error(
        {
          cause: `no probe was seen within ${LIVE_BOUND_MS}ms`,
          bound_ms: LIVE_BOUND_MS,
          expected_ms: LIVE_EXPECTED_MS,
          probes,
          roots: this.roots.map((root) => root.root),
        },
        "the watch on the vendor's skills did not come live within its bound; reporting every skills directory as a change, which gets the shim replaced",
      );
      for (const root of this.roots) this.onChange(root.root);
    }));
    stops.push(this.fsys.every(PROBE_EVERY_MS, write));
    write();
  }

  /** Report what changed between BEFORE and now, root by root, re-arming each root that moved. */
  private catchUp(before: readonly Inventory[]): void {
    this.roots.forEach((root, i) => {
      const changed = changedPaths(before[i] ?? new Map(), root.inventory());
      if (changed.length === 0) return;
      LOGGER.info(
        { skills_dir: root.root, changed },
        "the vendor's skills changed while the watch was coming live; re-arming and reporting it",
      );
      root.arm();
      for (const at of changed) this.onChange(at);
    });
  }

  /** The probe could not run while it was WHEN: recorded at ERROR, and every root reported as a change. */
  private probeFailed(when: string, err: unknown): void {
    this.endProbe = undefined;
    LOGGER.error(
      { when, cause: err instanceof Error ? err.message : String(err), roots: this.roots.map((root) => root.root) },
      "the watch on the vendor's skills could not prove itself live; reporting every skills directory as a change, which gets the shim replaced",
    );
    for (const root of this.roots) this.onChange(root.root);
  }
}

/**
 * A skills directory as the vendor would load it: each path that counts
 * (the directory itself, each entry, each skill's file), by a stamp that
 * moves whenever what it names does.
 */
type Inventory = ReadonlyMap<string, string>;

/** Every path whose stamp differs between BEFORE and AFTER, either side's alone included. */
export function changedPaths(before: Inventory, after: Inventory): string[] {
  const keys = new Set([...before.keys(), ...after.keys()]);
  return [...keys].filter((key) => before.get(key) !== after.get(key)).sort();
}

/** The watch on one skills directory, its skills, and its absence. */
class RootWatch {
  /**
   * The directory as it stands: absent, or present with each entry's
   * resolved target and its skill file's stamp (`Inventory`).
   */
  inventory(): Inventory {
    const held = new Map<string, string>();
    const entries = this.fsys.entries(this.root);
    if (entries === undefined) {
      held.set(this.root, "absent");
      return held;
    }
    held.set(this.root, "present");
    for (const entry of entries) {
      const at = path.join(this.root, entry);
      const target = this.fsys.resolve(at);
      const stamp = target === undefined ? undefined : this.fsys.stamp(path.join(target, SKILL_FILE));
      held.set(at, `${target ?? "unresolved"}:${stamp ?? "no-skill-file"}`);
    }
    return held;
  }

  /** The watch on the skills directory itself, or on its nearest existing ancestor while it is missing. */
  private anchor: DirWatch | undefined;
  /** Each skill's watch, by its entry name, at its resolved directory. */
  private readonly skills = new Map<string, { target: string; watch: DirWatch }>();
  private closed = false;

  constructor(
    readonly root: string,
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

  /** Stop reporting for good, closing no platform watch (see SkillSetWatch.abandon). */
  abandon(): void {
    this.closed = true;
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
    // EVERY EVENT IS RECORDED AS IT ARRIVES, before it is judged, so the log
    // tells a change the platform never reported from one that was reported
    // and not counted.
    const seen = (name: string | null): void => {
      LOGGER.debug({ skills_dir: this.root, watched: dir, entry: name ?? "" }, "a watch on the vendor's skills saw an event");
      onEvent(name);
    };
    try {
      return this.fsys.watchDir(dir, seen, (err) => failed(err, "after it began"));
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
