import { describe, expect, it } from "vitest";
import { SkillSetWatch, skillDirsOf } from "../../src/engine/skill-watch.js";
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
