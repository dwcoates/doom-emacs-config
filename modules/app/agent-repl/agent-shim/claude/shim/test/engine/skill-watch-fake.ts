/**
 * test/engine/skill-watch-fake.ts — an in-memory filesystem for the watch on
 * the vendor's skills (src/engine/skill-watch.ts).
 *
 * Shared by the watch's own suite and by every engine a test builds: an engine
 * built with an empty one arms its watch against nothing and reads no real
 * directory, and a test that needs a change fires one by hand.
 */
import type { DirWatch, SkillWatchFs } from "../../src/engine/skill-watch.js";

/**
 * An in-memory filesystem of directories and links, and a watch whose events
 * the test fires by hand. Nothing touches the real filesystem.
 */
export class FakeSkillWatchFs implements SkillWatchFs {
  /** Every directory, by path, with its entries. */
  readonly dirs = new Map<string, Set<string>>();
  /** Every link, by path, with its target. */
  readonly links = new Map<string, string>();
  /** Every open watch, by directory, with its callbacks. */
  readonly watches = new Map<string, { onEvent: (name: string | null) => void; onError: (err: Error) => void }[]>();
  /** Directories whose watch refuses to start. */
  readonly unwatchable = new Set<string>();
  /**
   * What happens on disk the instant a watch on a directory begins, before it
   * can see anything: a change landing between a look and a watch, which no
   * event reports.
   */
  readonly onWatchBegins = new Map<string, () => void>();

  mkdir(dir: string): void {
    const parts = dir.split("/").filter(Boolean);
    let at = "";
    for (const part of parts) {
      const parent = at === "" ? "/" : at;
      at = `${at}/${part}`;
      if (!this.dirs.has(parent)) this.dirs.set(parent, new Set());
      this.dirs.get(parent)!.add(part);
      if (!this.dirs.has(at)) this.dirs.set(at, new Set());
    }
  }

  link(at: string, target: string): void {
    const slash = at.lastIndexOf("/");
    this.dirs.get(at.slice(0, slash))!.add(at.slice(slash + 1));
    this.links.set(at, target);
  }

  remove(at: string): void {
    const slash = at.lastIndexOf("/");
    this.dirs.get(at.slice(0, slash))?.delete(at.slice(slash + 1));
    this.links.delete(at);
    for (const dir of [...this.dirs.keys()]) if (dir === at || dir.startsWith(`${at}/`)) this.dirs.delete(dir);
  }

  /** Fire NAME's event on DIR's watches. */
  fire(dir: string, name: string | null): void {
    for (const w of this.watches.get(dir) ?? []) w.onEvent(name);
  }

  watching(dir: string): boolean {
    return (this.watches.get(dir) ?? []).length > 0;
  }

  watchDir(dir: string, onEvent: (name: string | null) => void, onError: (err: Error) => void): DirWatch {
    if (this.unwatchable.has(dir)) throw new Error(`EMFILE: too many open files, watch '${dir}'`);
    const begins = this.onWatchBegins.get(dir);
    if (begins !== undefined) {
      this.onWatchBegins.delete(dir);
      begins();
    }
    const entry = { onEvent, onError };
    const list = this.watches.get(dir) ?? [];
    list.push(entry);
    this.watches.set(dir, list);
    return {
      close: () => {
        const now = this.watches.get(dir) ?? [];
        this.watches.set(
          dir,
          now.filter((w) => w !== entry),
        );
      },
    };
  }

  resolve(target: string): string | undefined {
    const linked = this.links.get(target);
    if (linked !== undefined) return this.resolve(linked);
    return this.dirs.has(target) ? target : undefined;
  }

  entries(dir: string): readonly string[] | undefined {
    const held = this.dirs.get(dir);
    return held === undefined ? undefined : [...held];
  }
}
