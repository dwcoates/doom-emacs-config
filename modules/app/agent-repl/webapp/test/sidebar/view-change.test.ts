// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  UpdateSidebarViewResponseSchema,
  type UpdateSidebarViewRequest,
  type UpdateSidebarViewResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { changeView, updateSidebarViewRefusal } from "../../src/sidebar/view-change.js";
import { MERGED_FOLD_KEY } from "../../src/sidebar/context.js";
import { foldViewKey } from "../../src/sidebar/view.js";
import { appContext, sidebarContext } from "./harness.js";

const OK = create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });

/** A context whose daemon records every UpdateSidebarView and answers ANSWER. */
function daemon(answer: () => UpdateSidebarViewResponse) {
  const asked: UpdateSidebarViewRequest[] = [];
  const sc = sidebarContext(
    appContext({
      updateSidebarView: (request) => {
        asked.push(request);
        return answer();
      },
    }),
  );
  return { asked, sc };
}

/** Collapse the recently-merged band: the change every case below asks for. */
const COLLAPSE_MERGED = {
  case: "foldSection",
  value: { section: { case: "recentlyMerged", value: {} }, fold: { case: "collapse", value: {} } },
} as const;

/** The view key the recently-merged band's fold is held under. */
const KEY = foldViewKey(MERGED_FOLD_KEY);

describe("changeView", () => {
  it("paints the asked value before the daemon answers", () => {
    const { sc } = daemon(() => OK);
    const painted: boolean[] = [];
    sc.view.track(KEY, false, (v) => painted.push(v));
    void changeView(sc, { key: KEY, value: true, control: document.createElement("button"), change: COLLAPSE_MERGED });
    expect(painted).toEqual([true]);
  });

  it("sends the change arm it was given", async () => {
    const { asked, sc } = daemon(() => OK);
    sc.view.track(KEY, false, () => undefined);
    await changeView(sc, { key: KEY, value: true, control: document.createElement("button"), change: COLLAPSE_MERGED });
    expect(asked.map((r) => r.change.case)).toEqual(["foldSection"]);
  });

  it("answers true and keeps the hold when the daemon took it", async () => {
    const { sc } = daemon(() => OK);
    sc.view.track(KEY, false, () => undefined);
    const took = await changeView(sc, { key: KEY, value: true, control: document.createElement("button"), change: COLLAPSE_MERGED });
    expect([took, sc.view.track(KEY, false, () => undefined)]).toEqual([true, true]);
  });

  it("repaints from the wire when the daemon refuses", async () => {
    const { sc } = daemon(() =>
      create(UpdateSidebarViewResponseSchema, {
        result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
      }),
    );
    const painted: boolean[] = [];
    sc.view.track(KEY, false, (v) => painted.push(v));
    const took = await changeView(sc, { key: KEY, value: true, control: document.createElement("button"), change: COLLAPSE_MERGED });
    expect([took, painted]).toEqual([false, [true, false]]);
  });

  it("draws the refusal beside the control", async () => {
    const { sc } = daemon(() =>
      create(UpdateSidebarViewResponseSchema, {
        result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } },
      }),
    );
    const host = document.createElement("div");
    const control = document.createElement("button");
    host.appendChild(control);
    sc.view.track(KEY, false, () => undefined);
    await changeView(sc, { key: KEY, value: true, control, change: COLLAPSE_MERGED });
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("unknownRepository");
  });

  it("repaints from the wire when the daemon cannot be reached", async () => {
    const { sc } = daemon(() => {
      throw new Error("down");
    });
    const painted: boolean[] = [];
    sc.view.track(KEY, false, (v) => painted.push(v));
    await changeView(sc, { key: KEY, value: true, control: document.createElement("button"), change: COLLAPSE_MERGED });
    expect(painted).toEqual([true, false]);
  });
});

describe("updateSidebarViewRefusal", () => {
  it.each([
    ["unknownRepository", "the daemon no longer has that repository"],
    ["unknownSubsection", "the daemon no longer has that subsection"],
  ] as const)("words %s", (arm, sentence) => {
    expect(updateSidebarViewRefusal({ case: arm, value: {} } as never)).toBe(sentence);
  });

  it("refuses an arm it does not know as a malformed view", () => {
    expect(() => updateSidebarViewRefusal({ case: "somethingNew" } as never)).toThrow(MalformedView);
  });
});
