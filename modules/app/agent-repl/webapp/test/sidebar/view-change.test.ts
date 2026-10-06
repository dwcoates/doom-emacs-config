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

const GROUPING_TASK = { case: "showGrouping", value: { grouping: { case: "task", value: {} } } } as const;

describe("changeView", () => {
  it("paints the asked value before the daemon answers", () => {
    const { sc } = daemon(() => OK);
    const painted: string[] = [];
    sc.view.track("grouping", "repository", (v) => painted.push(v));
    void changeView(sc, { key: "grouping", value: "task", control: document.createElement("button"), change: GROUPING_TASK });
    expect(painted).toEqual(["task"]);
  });

  it("sends the change arm it was given", async () => {
    const { asked, sc } = daemon(() => OK);
    sc.view.track("grouping", "repository", () => undefined);
    await changeView(sc, { key: "grouping", value: "task", control: document.createElement("button"), change: GROUPING_TASK });
    expect(asked.map((r) => r.change.case)).toEqual(["showGrouping"]);
  });

  it("answers true and keeps the hold when the daemon took it", async () => {
    const { sc } = daemon(() => OK);
    sc.view.track("grouping", "repository", () => undefined);
    const took = await changeView(sc, { key: "grouping", value: "task", control: document.createElement("button"), change: GROUPING_TASK });
    expect([took, sc.view.track("grouping", "repository", () => undefined)]).toEqual([true, "task"]);
  });

  it("repaints from the wire when the daemon refuses", async () => {
    const { sc } = daemon(() =>
      create(UpdateSidebarViewResponseSchema, {
        result: { case: "error", value: { cause: { case: "unknownTask", value: {} } } },
      }),
    );
    const painted: string[] = [];
    sc.view.track("grouping", "repository", (v) => painted.push(v));
    const took = await changeView(sc, { key: "grouping", value: "task", control: document.createElement("button"), change: GROUPING_TASK });
    expect([took, painted]).toEqual([false, ["task", "repository"]]);
  });

  it("draws the refusal beside the control", async () => {
    const { sc } = daemon(() =>
      create(UpdateSidebarViewResponseSchema, {
        result: { case: "error", value: { cause: { case: "unknownTask", value: {} } } },
      }),
    );
    const host = document.createElement("div");
    const control = document.createElement("button");
    host.appendChild(control);
    sc.view.track("grouping", "repository", () => undefined);
    await changeView(sc, { key: "grouping", value: "task", control, change: GROUPING_TASK });
    expect(host.querySelector(".refusal")?.getAttribute("data-arm")).toBe("unknownTask");
  });

  it("repaints from the wire when the daemon cannot be reached", async () => {
    const { sc } = daemon(() => {
      throw new Error("down");
    });
    const painted: string[] = [];
    sc.view.track("grouping", "repository", (v) => painted.push(v));
    await changeView(sc, { key: "grouping", value: "task", control: document.createElement("button"), change: GROUPING_TASK });
    expect(painted).toEqual(["task", "repository"]);
  });
});

describe("updateSidebarViewRefusal", () => {
  it.each([
    ["unknownRepository", "the daemon no longer has that repository"],
    ["unknownTask", "the daemon no longer has that task"],
  ] as const)("words %s", (arm, sentence) => {
    expect(updateSidebarViewRefusal({ case: arm, value: {} } as never)).toBe(sentence);
  });

  it("refuses an arm it does not know as a malformed view", () => {
    expect(() => updateSidebarViewRefusal({ case: "somethingNew" } as never)).toThrow(MalformedView);
  });
});
