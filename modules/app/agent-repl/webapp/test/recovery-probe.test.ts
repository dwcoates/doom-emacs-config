import { describe, expect, it } from "vitest";
import {
  RECOVERY_PROBE_HOOK,
  RecoveryProbe,
  type ProbeGlobal,
  installHostRecoveryProbeHook,
} from "../src/recovery-probe.js";

/** A probe over a clock the test advances by hand. */
function makeProbe(opts?: { socketOpen?: () => boolean; workspace?: string }) {
  let now = 1_000;
  const probe = new RecoveryProbe({
    now: () => now,
    workspace: () => opts?.workspace ?? "/w",
    socketOpen: opts?.socketOpen ?? (() => true),
  });
  return { probe, advance: (ms: number) => (now += ms), at: () => now };
}

describe("RecoveryProbe", () => {
  it("opens its epoch at construction, so a freshly booted page counts its own frames", () => {
    const { probe, at } = makeProbe();

    const report = probe.report();

    expect(report.epochAtMs).toBe(at());
    expect(report.satisfied).toBe(false);
  });

  it("counts a re-navigated page's evidence without any explicit epoch call", () => {
    const { probe } = makeProbe();

    probe.noteAdopted();
    probe.noteBatch(["session-view"]);

    expect(probe.report().satisfied).toBe(true);
  });

  it("cannot be satisfied by an open socket alone", () => {
    const { probe } = makeProbe({ socketOpen: () => true });
    probe.openEpoch();

    const report = probe.report();

    expect(report.socketOpen).toBe(true);
    expect(report.satisfied).toBe(false);
  });

  it("cannot be satisfied by adoption alone", () => {
    const { probe } = makeProbe();
    probe.openEpoch();

    probe.noteAdopted();

    expect(probe.report().adopted).toBe(true);
    expect(probe.report().satisfied).toBe(false);
  });

  it("cannot be satisfied by content alone", () => {
    const { probe } = makeProbe();
    probe.openEpoch();

    probe.noteBatch(["conversation-items"]);

    expect(probe.report().realDataFrames).toBe(1);
    expect(probe.report().satisfied).toBe(false);
  });

  it("is satisfied by adoption plus content, and stamps when each landed", () => {
    const { probe, advance, at } = makeProbe();
    probe.openEpoch();

    advance(40);
    probe.noteAdopted();
    const adoptedAt = at();
    advance(60);
    probe.noteBatch(["workspace-state"]);
    const dataAt = at();

    const report = probe.report();
    expect(report.satisfied).toBe(true);
    expect(report.adoptedAtMs).toBe(adoptedAt);
    expect(report.firstRealDataAtMs).toBe(dataAt);
  });

  it("does not count chrome effects as real data", () => {
    const { probe } = makeProbe();
    probe.openEpoch();

    const counted = probe.noteBatch(["workspace-roster", "ignored", "progress"]);

    expect(counted).toBe(0);
    expect(probe.report().realDataFrames).toBe(0);
  });

  it("discards the previous attempt's evidence when a new epoch opens", () => {
    const { probe } = makeProbe();
    probe.openEpoch();
    probe.noteAdopted();
    probe.noteBatch(["conversation-page"]);

    probe.openEpoch();

    const report = probe.report();
    expect(report.satisfied).toBe(false);
    expect(report.realDataFrames).toBe(0);
    expect(report.adoptedAtMs).toBe(0);
  });

  it("ignores evidence when the host clock reports no epoch at all", () => {
    const probe = new RecoveryProbe({
      now: () => 0,
      workspace: () => "/w",
      socketOpen: () => true,
    });

    probe.noteAdopted();
    probe.noteBatch(["session-view"]);

    expect(probe.report().satisfied).toBe(false);
  });
});

describe("installHostRecoveryProbeHook", () => {
  it("plants a hook returning the report as JSON the host can parse", () => {
    const { probe } = makeProbe({ workspace: "/ws/a" });
    const target: ProbeGlobal = {};
    probe.openEpoch();
    probe.noteAdopted();
    probe.noteBatch(["conversation-items"]);

    installHostRecoveryProbeHook(target, probe);
    const raw = (target[RECOVERY_PROBE_HOOK] as () => string)();

    expect(JSON.parse(raw)).toMatchObject({ workspace: "/ws/a", satisfied: true });
  });
});
