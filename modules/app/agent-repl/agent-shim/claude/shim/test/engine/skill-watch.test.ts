import { describe, expect, it } from "vitest";
import { changedPaths, LIVE_BOUND_MS, PROBE_EVERY_MS, SkillSetWatch, skillDirsOf } from "../../src/engine/skill-watch.js";
import { FakeSkillWatchFs as FakeFs } from "./skill-watch-fake.js";
import { logRecordsDuring } from "../log-records.js";

const CONFIG = "/home/u/.claude";
const CWD = "/work/ws";
const ROOT = `${CONFIG}/skills`;
const SHARED = "/repo/skills";

/** A filesystem with the account's skills directory holding one linked skill, `alpha`. */
function arrange(): { fsys: FakeFs; changes: string[]; watch: SkillSetWatch } {
  const fsys = new FakeFs();
  fsys.mkdir(ROOT);
  fsys.mkdir(`${SHARED}/alpha`);
  fsys.link(`${ROOT}/alpha`, `${SHARED}/alpha`);
  fsys.mkdir(CWD);
  const changes: string[] = [];
  const watch = new SkillSetWatch([ROOT], fsys, (changed) => changes.push(changed));
  return { fsys, changes, watch };
}

describe("skillDirsOf", () => {
  it("names the account root's skills directory, then the worktree's", () => {
    expect(skillDirsOf(CONFIG, CWD)).toEqual([`${CONFIG}/skills`, `${CWD}/.claude/skills`]);
  });
});

describe("SkillSetWatch", () => {
  it("reports nothing at its start", () => {
    const { changes, watch } = arrange();

    watch.start();

    expect(changes).toEqual([]);
  });

  it("reports a skill installed into a watched directory", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();

    fsys.mkdir(`${SHARED}/beta`);
    fsys.link(`${ROOT}/beta`, `${SHARED}/beta`);
    fsys.fire(ROOT, "beta");

    expect(changes).toEqual([`${ROOT}/beta`]);
  });

  it("records every event it sees at DEBUG, naming the watched directory and the entry", () => {
    const { fsys, watch } = arrange();
    watch.start();

    const records = logRecordsDuring(() => fsys.fire(ROOT, "beta"));

    expect(
      records.some(
        (r) =>
          r.level === "debug" &&
          r.message === "a watch on the vendor's skills saw an event" &&
          r.context["watched"] === ROOT &&
          r.context["entry"] === "beta",
      ),
    ).toBe(true);
  });

  it("records an event that names no entry with an empty entry", () => {
    const { fsys, watch } = arrange();
    watch.start();

    const records = logRecordsDuring(() => fsys.fire(ROOT, null));

    expect(
      records.some((r) => r.message === "a watch on the vendor's skills saw an event" && r.context["entry"] === ""),
    ).toBe(true);
  });

  it("reports a change to a linked skill's SKILL.md, watched at the link's target", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();

    fsys.fire(`${SHARED}/alpha`, "SKILL.md");

    expect(changes).toEqual([`${SHARED}/alpha/SKILL.md`]);
  });

  it("ignores a file a skill writes into its own directory", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();

    fsys.fire(`${SHARED}/alpha`, ".last-update.json");

    expect(changes).toEqual([]);
  });

  it("reports an event the platform names no entry for", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();

    fsys.fire(`${SHARED}/alpha`, null);

    expect(changes).toEqual([`${SHARED}/alpha/SKILL.md`]);
  });

  it("watches a skill installed after its start", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();
    fsys.mkdir(`${SHARED}/beta`);
    fsys.link(`${ROOT}/beta`, `${SHARED}/beta`);
    fsys.fire(ROOT, "beta");

    fsys.fire(`${SHARED}/beta`, "SKILL.md");

    expect(changes).toEqual([`${ROOT}/beta`, `${SHARED}/beta/SKILL.md`]);
  });

  it("stops watching a skill that was removed", () => {
    const { fsys, watch } = arrange();
    watch.start();

    fsys.remove(`${ROOT}/alpha`);
    fsys.fire(ROOT, "alpha");

    expect(fsys.watching(`${SHARED}/alpha`)).toBe(false);
  });

  it("watches a missing skills directory from its nearest existing ancestor, and reports its creation", () => {
    const fsys = new FakeFs();
    fsys.mkdir(CWD);
    const changes: string[] = [];
    const watch = new SkillSetWatch([`${CWD}/.claude/skills`], fsys, (changed) => changes.push(changed));
    watch.start();
    expect(fsys.watching(CWD)).toBe(true);

    fsys.mkdir(`${CWD}/.claude/skills/gamma`);
    fsys.fire(CWD, ".claude");

    expect([changes, fsys.watching(`${CWD}/.claude/skills`), fsys.watching(`${CWD}/.claude/skills/gamma`)]).toEqual([
      [`${CWD}/.claude/skills`],
      true,
      true,
    ]);
  });

  it("reports a skills directory that appeared while its ancestor's watch was being armed", () => {
    // Arrange: `.claude` appears; `.claude/skills` lands between the look and
    // the watch on `.claude`, an appearance no event reports.
    const fsys = new FakeFs();
    fsys.mkdir(CWD);
    const changes: string[] = [];
    const watch = new SkillSetWatch([`${CWD}/.claude/skills`], fsys, (changed) => changes.push(changed));
    watch.start();
    fsys.mkdir(`${CWD}/.claude`);
    fsys.onWatchBegins.set(`${CWD}/.claude`, () => fsys.mkdir(`${CWD}/.claude/skills/gamma`));

    // Act
    fsys.fire(CWD, ".claude");

    // Assert
    expect([changes, fsys.watching(`${CWD}/.claude/skills`), fsys.watching(`${CWD}/.claude/skills/gamma`)]).toEqual([
      [`${CWD}/.claude/skills`],
      true,
      true,
    ]);
  });

  it("watches a skills directory that appeared while the watch was starting, reporting nothing", () => {
    // Arrange: the skills directory lands between the start's look and its
    // watch on the ancestor. The watch starts before the vendor loads its
    // skills, so the vendor loads this one: no change is owed.
    const fsys = new FakeFs();
    fsys.mkdir(CWD);
    const changes: string[] = [];
    const watch = new SkillSetWatch([`${CWD}/.claude/skills`], fsys, (changed) => changes.push(changed));
    fsys.onWatchBegins.set(CWD, () => fsys.mkdir(`${CWD}/.claude/skills`));

    // Act
    watch.start();

    // Assert
    expect([changes, fsys.watching(`${CWD}/.claude/skills`)]).toEqual([[], true]);
  });

  it("ignores an ancestor's event for another entry", () => {
    const fsys = new FakeFs();
    fsys.mkdir(CWD);
    const changes: string[] = [];
    new SkillSetWatch([`${CWD}/.claude/skills`], fsys, (changed) => changes.push(changed)).start();

    fsys.mkdir(`${CWD}/src`);
    fsys.fire(CWD, "src");

    expect(changes).toEqual([]);
  });

  it("reports a watched skills directory that went, and watches for its return", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();

    fsys.remove(ROOT);
    fsys.fire(ROOT, null);

    expect([changes, fsys.watching(CONFIG)]).toEqual([[ROOT], true]);
  });

  it("reports a watch that cannot start as a change, at ERROR", () => {
    const fsys = new FakeFs();
    fsys.mkdir(ROOT);
    fsys.unwatchable.add(ROOT);
    const changes: string[] = [];

    const records = logRecordsDuring(() => new SkillSetWatch([ROOT], fsys, (changed) => changes.push(changed)).start());

    expect(changes).toEqual([ROOT]);
    expect(
      records.some((r) => r.level === "error" && r["operation"] === "shim.engine.skill_watch" && r.context["watched"] === ROOT),
    ).toBe(true);
  });

  it("reports a watch that fails after it began as a change", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();

    for (const w of fsys.watches.get(ROOT) ?? []) w.onError(new Error("watch broke"));

    expect(changes).toEqual([ROOT]);
  });

  it("arms once however often it is started", () => {
    const { fsys, watch } = arrange();

    watch.start();
    watch.start();

    expect(fsys.watches.get(ROOT)?.length).toBe(1);
  });

  it("closes every watch it holds", () => {
    const { fsys, watch } = arrange();
    watch.start();

    watch.close();

    expect([fsys.watching(ROOT), fsys.watching(`${SHARED}/alpha`)]).toEqual([false, false]);
  });

  it("reports nothing after it is closed", () => {
    const { fsys, changes, watch } = arrange();
    watch.start();
    const held = [...(fsys.watches.get(ROOT) ?? [])];
    watch.close();

    for (const w of held) w.onEvent("late");

    expect(changes).toEqual([]);
  });
});

/**
 * THE WATCH PROVES ITSELF LIVE. A platform watch reports nothing until its
 * stream has started, so the watch probes a directory of its own until a
 * probe is seen, then reports whatever changed in the gap.
 */
describe("SkillSetWatch coming live", () => {
  /** ARRANGE's filesystem with its stream not yet live. */
  function arrangeInGap(): ReturnType<typeof arrange> {
    const arranged = arrange();
    arranged.fsys.live = false;
    return arranged;
  }

  it("records that it is live, and how long it took", () => {
    // Arrange
    const { fsys, watch } = arrangeInGap();
    watch.start();
    fsys.goLive();

    // Act
    const records = logRecordsDuring(() => fsys.tick(PROBE_EVERY_MS * 2));

    // Assert
    expect(
      records.find((r) => r.message === "the watch on the vendor's skills is live")?.context,
    ).toMatchObject({ live_ms: PROBE_EVERY_MS, probes: 2 });
  });

  it("reports a skill installed while it was coming live", () => {
    // Arrange
    const { fsys, changes, watch } = arrangeInGap();
    watch.start();
    fsys.mkdir(`${SHARED}/beta`);
    fsys.link(`${ROOT}/beta`, `${SHARED}/beta`);
    fsys.goLive();

    // Act
    fsys.tick(PROBE_EVERY_MS);

    // Assert
    expect(changes).toEqual([`${ROOT}/beta`]);
  });

  it("watches a skill installed while it was coming live", () => {
    // Arrange
    const { fsys, changes, watch } = arrangeInGap();
    watch.start();
    fsys.mkdir(`${SHARED}/beta`);
    fsys.link(`${ROOT}/beta`, `${SHARED}/beta`);
    fsys.goLive();
    fsys.tick(PROBE_EVERY_MS);
    changes.length = 0;

    // Act
    fsys.fire(`${SHARED}/beta`, "SKILL.md");

    // Assert
    expect(changes).toEqual([`${SHARED}/beta/SKILL.md`]);
  });

  it("reports a skill file rewritten while it was coming live", () => {
    // Arrange
    const { fsys, changes, watch } = arrangeInGap();
    watch.start();
    fsys.stamps.set(`${SHARED}/alpha/SKILL.md`, 2);
    fsys.goLive();

    // Act
    fsys.tick(PROBE_EVERY_MS);

    // Assert
    expect(changes).toEqual([`${ROOT}/alpha`]);
  });

  it("reports nothing when nothing changed while it was coming live", () => {
    // Arrange
    const { fsys, changes, watch } = arrangeInGap();
    watch.start();
    fsys.goLive();

    // Act
    fsys.tick(PROBE_EVERY_MS);

    // Assert
    expect(changes).toEqual([]);
  });

  it("removes its probe directory once live", () => {
    // Arrange
    const { fsys, watch } = arrange();

    // Act
    watch.start();

    // Assert
    expect([fsys.probe, fsys.watching("/tmp/agent-repl-skill-probe-1")]).toEqual([undefined, false]);
  });

  it("stops probing once live", () => {
    // Arrange
    const { fsys, watch } = arrange();
    watch.start();
    const written = fsys.probesWritten;

    // Act
    fsys.tick(PROBE_EVERY_MS * 5);

    // Assert
    expect(fsys.probesWritten).toBe(written);
  });

  it("reports every root at ERROR when it is not live within its bound", () => {
    // Arrange
    const { fsys, changes, watch } = arrangeInGap();
    watch.start();

    // Act
    const records = logRecordsDuring(() => fsys.tick(LIVE_BOUND_MS));

    // Assert
    expect([
      changes,
      records.find((r) => r.level === "error")?.context,
      fsys.probe,
    ]).toEqual([[ROOT], expect.objectContaining({ bound_ms: LIVE_BOUND_MS, expected_ms: 1293, roots: [ROOT] }), undefined]);
  });

  it("reports every root at ERROR when its probe directory cannot be made", () => {
    // Arrange
    const { fsys, changes, watch } = arrange();
    fsys.probeUnavailable = true;

    // Act
    const records = logRecordsDuring(() => watch.start());

    // Assert
    expect([changes, records.find((r) => r.level === "error")?.context]).toEqual([
      [ROOT],
      expect.objectContaining({ when: "making its directory", roots: [ROOT] }),
    ]);
  });

  it("reports every root at ERROR when its probe directory cannot be watched", () => {
    // Arrange
    const { fsys, changes, watch } = arrange();
    fsys.unwatchable.add("/tmp/agent-repl-skill-probe-1");

    // Act
    const records = logRecordsDuring(() => watch.start());

    // Assert
    expect([changes, records.find((r) => r.level === "error")?.context, fsys.probe]).toEqual([
      [ROOT],
      expect.objectContaining({ when: "watching its directory" }),
      undefined,
    ]);
  });

  it("reports nothing once closed while coming live", () => {
    // Arrange
    const { fsys, changes, watch } = arrangeInGap();
    watch.start();
    watch.close();

    // Act
    fsys.tick(LIVE_BOUND_MS * 2);

    // Assert
    expect([changes, fsys.probe]).toEqual([[], undefined]);
  });
});

describe("changedPaths", () => {
  it("names a path only the later inventory holds", () => {
    expect(changedPaths(new Map(), new Map([["/a", "x"]]))).toEqual(["/a"]);
  });

  it("names a path only the earlier inventory holds", () => {
    expect(changedPaths(new Map([["/a", "x"]]), new Map())).toEqual(["/a"]);
  });

  it("names a path whose stamp moved", () => {
    expect(changedPaths(new Map([["/a", "x"]]), new Map([["/a", "y"]]))).toEqual(["/a"]);
  });

  it("names nothing for equal inventories", () => {
    expect(changedPaths(new Map([["/a", "x"]]), new Map([["/a", "x"]]))).toEqual([]);
  });
});
