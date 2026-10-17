// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
} from "../../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { FeedMergeWorkspaceSwitchSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { WorkspaceRefSchema } from "../../../../proto/gen/ts/workspace/v1/workspace_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import { drawMergeWorkspaceSwitch, switchFacts } from "../../../src/feed/merge/workspace-switch.js";
import { scriptedAppContext } from "../../rpc/app-context.js";
import { RecordingSink, WORKSPACE, mergeRow, rowContext } from "../harness.js";

beforeEach(() => {
  vi.useFakeTimers();
});
afterEach(() => {
  vi.useRealTimers();
});

/** The merge workspace every switch below names. */
const TARGET = create(WorkspaceRefSchema, { id: "ws-merge", dir: "/w/merge" });

/** A row context whose SelectWorkspace succeeds, with the calls it received. */
function selecting() {
  const calls: SelectWorkspaceRequest[] = [];
  const ctx = scriptedAppContext(
    {
      selectWorkspace: (req) => {
        calls.push(req);
        return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    },
    { workspace: WORKSPACE, failures: new RecordingSink() },
  );
  return { rc: rowContext(ctx, mergeRow("m1")), calls };
}

describe("switchFacts", () => {
  it("reads an available head switch to its label and target", () => {
    const facts = switchFacts(
      create(FeedMergeWorkspaceSwitchSchema, {
        label: { text: "open merge workspace" },
        availability: { case: "available", value: { target: TARGET } },
      }),
      "head",
    );
    expect([facts.label, facts.target?.id]).toEqual(["open merge workspace", "ws-merge"]);
  });

  it("reads a removed head switch to no target", () => {
    const facts = switchFacts(
      create(FeedMergeWorkspaceSwitchSchema, {
        label: { text: "merge workspace removed" },
        availability: { case: "removed", value: { removedAtMs: 5n } },
      }),
      "head",
    );
    expect([facts.label, facts.target]).toEqual(["merge workspace removed", null]);
  });

  it("refuses a switch that says neither available nor removed", () => {
    expect(() =>
      switchFacts(create(FeedMergeWorkspaceSwitchSchema, { label: { text: "x" } }), "head"),
    ).toThrow(MalformedView);
  });

  it("refuses an available switch with no target", () => {
    expect(() =>
      switchFacts(
        create(FeedMergeWorkspaceSwitchSchema, { label: { text: "x" }, availability: { case: "available", value: {} } }),
        "head",
      ),
    ).toThrow(MalformedView);
  });
});

describe("drawMergeWorkspaceSwitch", () => {
  it("selects the merge workspace when clicked", async () => {
    const s = selecting();
    const el = drawMergeWorkspaceSwitch({ label: "open merge workspace", target: TARGET }, s.rc);
    el.querySelector<HTMLElement>(".merge-workspace-switch-button")?.click();
    await vi.advanceTimersByTimeAsync(0);
    expect(s.calls.map((c) => c.workspace?.id)).toEqual(["ws-merge"]);
  });

  it("draws the label verbatim on an enabled button", () => {
    const el = drawMergeWorkspaceSwitch({ label: "open merge workspace", target: TARGET }, selecting().rc);
    const button = el.querySelector(".merge-workspace-switch-button");
    expect([button?.textContent, button?.getAttribute("aria-disabled")]).toEqual(["open merge workspace", null]);
  });

  it("draws a removed workspace's switch disabled, and a click selects nothing", async () => {
    const s = selecting();
    const el = drawMergeWorkspaceSwitch({ label: "merge workspace removed", target: null }, s.rc);
    el.querySelector<HTMLElement>(".merge-workspace-switch-button")?.click();
    await vi.advanceTimersByTimeAsync(0);
    expect([el.hasAttribute("data-removed"), el.querySelector(".merge-workspace-switch-button")?.getAttribute("aria-disabled"), s.calls.length]).toEqual([
      true,
      "true",
      0,
    ]);
  });

  it("is drawn as the head's switch", () => {
    const el = drawMergeWorkspaceSwitch({ label: "x", target: TARGET }, selecting().rc);
    expect(el.classList.contains("merge-workspace-switch-head")).toBe(true);
  });
});
