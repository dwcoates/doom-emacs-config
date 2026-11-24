// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { RosterColdGateSchema, RosterRepoMergeQueueSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { RepositoryRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  COLD_GATED_NAME_CLASS,
  coldGatedOf,
  markColdGatedName,
  NONE_COLD_GATED,
  sameColdGated,
} from "../../src/sidebar/cold-gate-name.js";
import { drawRosterMergeQueue } from "../../src/sidebar/merge-queue-view.js";
import { drawRosterRow } from "../../src/sidebar/row.js";
import { cascadedValue, inRail } from "../stylesheet.js";
import { mergingHead, row, sidebarContext } from "./harness.js";
import styles from "../../src/styles.css?raw";
import mergeQueueSource from "../../src/sidebar/merge-queue-view.ts?raw";
import rowSource from "../../src/sidebar/row.ts?raw";

/** The repository a test's queue group is drawn for. */
const REPOSITORY = { key: create(RepositoryRefSchema, { id: "repo-1", dir: "/r/repo-1" }), name: "doom" };

/** The painted strike and ink of NAME, mounted in the rail with the real stylesheet. */
function paintOf(drawn: HTMLElement, name: () => Element | null): string[] {
  return inRail(drawn, () => {
    const el = name();
    if (el === null) throw new Error("the drawing has no name");
    return ["text-decoration", "color", "--name-rest"].map((property) => cascadedValue(el, property));
  });
}

describe("markColdGatedName", () => {
  it("adds the shared cold-gate class to the name", () => {
    const name = document.createElement("span");
    markColdGatedName(name);
    expect(name.classList.contains(COLD_GATED_NAME_CLASS)).toBe(true);
  });

  it("keeps the name's own classes", () => {
    const name = document.createElement("span");
    name.className = "sb-mq-name";
    markColdGatedName(name);
    expect([...name.classList]).toEqual(["sb-mq-name", COLD_GATED_NAME_CLASS]);
  });
});

describe("coldGatedOf", () => {
  it("names every workspace a standing gate is on", () => {
    const gates = ["w1", "w2"].map((id) => create(RosterColdGateSchema, { workspace: { id, dir: `/w/${id}` } }));
    expect([...coldGatedOf(gates, "gates")]).toEqual(["w1", "w2"]);
  });

  it("names none while no gate stands", () => {
    expect(coldGatedOf([], "gates").size).toBe(0);
  });

  it("refuses a gate that names no workspace as a malformed view", () => {
    expect(() => coldGatedOf([create(RosterColdGateSchema, {})], "gates")).toThrow(MalformedView);
  });
});

describe("sameColdGated", () => {
  it.each([
    { name: "the same workspaces in another order", a: ["w1", "w2"], b: ["w2", "w1"], want: true },
    { name: "neither naming any", a: [], b: [], want: true },
    { name: "one naming a workspace the other does not", a: ["w1"], b: ["w2"], want: false },
    { name: "one naming more", a: ["w1"], b: ["w1", "w2"], want: false },
  ])("answers $want for $name", ({ a, b, want }) => {
    expect(sameColdGated(new Set(a), new Set(b))).toBe(want);
  });
});

describe("NONE_COLD_GATED", () => {
  it("names no workspace", () => {
    expect(NONE_COLD_GATED.size).toBe(0);
  });
});

// ONE COLD-GATE STYLE (owner request, 2026-11-17): the Open section's row and
// the Merge Queue section's line wear the one class, and the one rule paints
// both alike.
describe("the shared cold-gate style", () => {
  // A VIEWED row: an unviewed one keeps its status tone (owner request,
  // 2026-11-23), so only a viewed row's struck name matches a queue line's.
  const openRow = () => drawRosterRow(row({ id: "w1", status: { case: "coldGate", value: {} }, viewed: true }), sidebarContext(), "R");
  const queueLine = () =>
    drawRosterMergeQueue(create(RosterRepoMergeQueueSchema, { head: mergingHead("w1") }), REPOSITORY, sidebarContext(), "queue", new Set(["w1"]));

  it("is the one class on an Open row's name and a merge queue line's name", () => {
    expect([
      openRow().querySelector(":scope > .row > .name")?.classList.contains(COLD_GATED_NAME_CLASS),
      queueLine().querySelector(".sb-mq-name")?.classList.contains(COLD_GATED_NAME_CLASS),
    ]).toEqual([true, true]);
  });

  it("paints a viewed Open row's name and a merge queue line's name alike", () => {
    const row = openRow();
    const line = queueLine();
    const open = paintOf(row, () => row.querySelector(":scope > .row > .name"));
    const queued = paintOf(line, () => line.querySelector(".sb-mq-name"));
    expect([queued, open[0]]).toEqual([open, "line-through"]);
  });

  it("is drawn by the one helper in both sections", () => {
    expect([rowSource, mergeQueueSource].map((source) => source.includes("markColdGatedName("))).toEqual([true, true]);
  });

  it("is one rule in the stylesheet, keyed on the shared class", () => {
    expect([styles.includes('[data-arm="coldGate"] > .row > .name'), styles.split(`.${COLD_GATED_NAME_CLASS} {`).length - 1]).toEqual([false, 1]);
  });
});
