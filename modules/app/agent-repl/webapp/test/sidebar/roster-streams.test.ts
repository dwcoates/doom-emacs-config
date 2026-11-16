// @vitest-environment jsdom
/**
 * THE CLIENT'S COPY OF THE ROSTER'S FOUR SNAPSHOT STREAMS
 * (src/sidebar/roster-streams.ts): every snapshot a push carries replaces the
 * copy whole, a stream left out is unchanged, and a first push missing one is
 * refused.
 */
import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WatchWorkspaceRosterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_roster_pb";
import { resetLoggingForTests } from "../../src/log.js";
import { MalformedView, UnknownPushArm } from "../../src/rpc/malformed.js";
import { carriedStreams, changedStreams, createRosterIntake, rosterPushOf } from "../../src/sidebar/roster-streams.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { mergeQueueBand, mergingHead, pushOf, queueGroup, repoSection, roster, row } from "./harness.js";

afterEach(() => {
  resetLoggingForTests();
});

/** Every stream as of revision 1. */
const first = () => roster({ revision: 1n, repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] });

describe("createRosterIntake", () => {
  it("holds every stream of a first push that carries them all", () => {
    // Arrange
    const intake = createRosterIntake();
    const streams = first();
    // Act
    const held = intake.apply(pushOf(streams));
    // Assert
    expect([held.workspaces === streams.workspaces, held.mergeQueue === streams.mergeQueue, held.recentlyMerged === streams.recentlyMerged, held.closed === streams.closed]).toEqual([
      true,
      true,
      true,
      true,
    ]);
  });

  it("replaces a stream a later push carries with that push's whole snapshot", () => {
    // Arrange
    const intake = createRosterIntake();
    intake.apply(pushOf(first()));
    const next = roster({ revision: 2n, mergeQueue: mergeQueueBand([queueGroup({ id: "repo-1", queue: { head: mergingHead("w1") } })]) });
    // Act
    const held = intake.apply(pushOf(next, ["merge_queue"]));
    // Assert
    expect(held.mergeQueue).toBe(next.mergeQueue);
  });

  it("keeps the very snapshot of every stream a later push leaves out", () => {
    // Arrange
    const intake = createRosterIntake();
    const streams = first();
    intake.apply(pushOf(streams));
    // Act
    const held = intake.apply(pushOf(roster({ revision: 2n }), ["merge_queue"]));
    // Assert: unchanged, never emptied.
    expect([held.workspaces === streams.workspaces, held.recentlyMerged === streams.recentlyMerged, held.closed === streams.closed]).toEqual([true, true, true]);
  });

  it("answers the push's revision", () => {
    const intake = createRosterIntake();
    intake.apply(pushOf(first()));
    expect(intake.apply(pushOf(roster({ revision: 9n }), ["closed"])).revision).toBe(9n);
  });

  it("refuses a first push missing a stream as a malformed view naming what it lacks", () => {
    // Arrange
    const intake = createRosterIntake();
    // Act
    const act = () => intake.apply(pushOf(first(), ["workspaces", "closed"]));
    // Assert
    expect(act).toThrow(
      new MalformedView(
        "WatchWorkspaceRosterResponse.streams",
        "the first push of a subscription carries every stream; revision 1 lacks merge_queue, recently_merged",
      ),
    );
  });

  it("keeps nothing of a first push it refused", () => {
    const intake = createRosterIntake();
    expect(() => intake.apply(pushOf(first(), ["workspaces"]))).toThrow(MalformedView);
    expect(intake.latest()).toBeNull();
  });

  it("refuses the push after a refused first one too, holding no baseline", () => {
    const intake = createRosterIntake();
    expect(() => intake.apply(pushOf(first(), ["workspaces"]))).toThrow(MalformedView);
    expect(() => intake.apply(pushOf(roster({ revision: 2n }), ["merge_queue"]))).toThrow(MalformedView);
  });

  it("takes the next push after a reset as a first push again", () => {
    // Arrange
    const intake = createRosterIntake();
    intake.apply(pushOf(first()));
    // Act
    intake.reset();
    // Assert
    expect(() => intake.apply(pushOf(roster({ revision: 2n }), ["workspaces"]))).toThrow(MalformedView);
  });

  it("logs each push it applies with the streams it carried", async () => {
    // Arrange
    const intake = createRosterIntake();
    intake.apply(pushOf(first()));
    const capture = captureLogRecords("debug");
    // Act
    intake.apply(pushOf(roster({ revision: 2n }), ["workspaces", "closed"]));
    // Assert
    const record = await forwardedRecord(capture, "sidebar.roster-push");
    expect(record.context).toMatchObject({ revision: "2", carried: ["workspaces", "closed"], baseline: false });
  });
});

describe("carriedStreams", () => {
  it.each([
    { name: "names every stream a whole push carries", only: undefined, want: ["workspaces", "merge_queue", "recently_merged", "closed"] },
    { name: "names only the streams a partial push carries", only: ["recently_merged"] as const, want: ["recently_merged"] },
    { name: "names nothing for a push carrying no stream", only: [] as const, want: [] },
  ])("$name", ({ only, want }) => {
    expect(carriedStreams(pushOf(first(), only))).toEqual(want);
  });
});

describe("changedStreams", () => {
  it("names every stream before anything was drawn", () => {
    expect(changedStreams(null, first())).toEqual(["workspaces", "merge_queue", "recently_merged", "closed"]);
  });

  it("names only the streams whose snapshot was replaced", () => {
    const before = first();
    const after = { ...before, revision: 2n, closed: roster().closed };
    expect(changedStreams(before, after)).toEqual(["closed"]);
  });

  it("names a stream replaced by an equal snapshot, since snapshots are compared by identity", () => {
    const before = first();
    expect(changedStreams(before, { ...before, workspaces: first().workspaces })).toEqual(["workspaces"]);
  });
});

describe("rosterPushOf", () => {
  it("answers the push a streams frame carries", () => {
    const push = pushOf(first());
    expect(rosterPushOf(create(WatchWorkspaceRosterResponseSchema, { push: { case: "streams", value: push } }))).toBe(push);
  });

  it("refuses a frame whose push sets no arm as a malformed view", () => {
    expect(() => rosterPushOf(create(WatchWorkspaceRosterResponseSchema, {}))).toThrow(MalformedView);
  });

  it("skips the ending arm as forward-compat skew, never reaching a draw", () => {
    expect(() => rosterPushOf(create(WatchWorkspaceRosterResponseSchema, { push: { case: "ending", value: {} } }))).toThrow(UnknownPushArm);
  });
});
