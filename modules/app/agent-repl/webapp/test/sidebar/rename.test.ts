// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SetWorkspaceNameErrorSchema,
  SetWorkspaceNameResponseSchema,
  type SetWorkspaceNameRequest,
  type SetWorkspaceNameResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_set_workspace_name_pb";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import { setWorkspaceNameRefusal } from "../../src/sidebar/rename.js";
import type { RailContext, SidebarContext } from "../../src/sidebar/context.js";
import type { CauseOf } from "../../src/sidebar/verbs.js";
import { appContext, hiddenSection, mergedSection, repoSection, roster, row, sidebarContext } from "./harness.js";

afterEach(() => {
  vi.restoreAllMocks();
  document.body.replaceChildren();
});

/** A sidebar whose daemon answers every rename with ANSWER, recording each. */
function renamed(answer: () => SetWorkspaceNameResponse) {
  const sent: SetWorkspaceNameRequest[] = [];
  const sc = sidebarContext(
    appContext({
      setWorkspaceName: (request) => {
        sent.push(request);
        return answer();
      },
    }),
  );
  return { sc, sent };
}

const taken = () =>
  create(SetWorkspaceNameResponseSchema, { result: { case: "success", value: { name: "better-name", rosterRevision: 1n } } });

/** Draw MADE and answer the drawn pane. */
function mount(made: ReturnType<typeof roster>, sc: RailContext & SidebarContext): HTMLElement {
  const drawn = drawWorkspaceRoster(made, sc);
  document.body.replaceChildren(sc.arrange.status, drawn, sc.arrange.layer);
  return drawn;
}

/** Double-click the name of row ID in PANE. */
function doubleClickName(pane: HTMLElement, id: string): void {
  pane.querySelector(`[data-roster-row='${id}'] .name`)?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
}

describe("renaming a workspace from its row", () => {
  it("opens the name popup on a double-click of the name, holding the name it has", () => {
    const { sc } = renamed(taken);
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "old-name" })] })] }), sc);

    doubleClickName(pane, "ws-1");

    expect(sc.arrange.layer.querySelector<HTMLInputElement>("[data-workspace-popup] [data-workspace-name]")?.value).toBe("old-name");
  });

  it("sends the workspace and the name typed, and closes once the rename is taken", async () => {
    const { sc, sent } = renamed(taken);
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "old-name" })] })] }), sc);
    doubleClickName(pane, "ws-1");
    const input = sc.arrange.layer.querySelector("[data-workspace-name]") as HTMLInputElement;
    input.value = "better-name";

    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    expect([sent.map((r) => [r.workspace?.id, r.name]), sc.arrange.layer.querySelector("[data-workspace-popup]")]).toEqual([
      [["ws-1", "better-name"]],
      null,
    ]);
  });

  it("draws a refusal at the popup and keeps it open", async () => {
    const { sc } = renamed(() =>
      create(SetWorkspaceNameResponseSchema, {
        result: { case: "error", value: { cause: { case: "nameTaken", value: { name: "better-name", holder: { case: "pending", value: {} } } } } },
      }),
    );
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "old-name" })] })] }), sc);
    doubleClickName(pane, "ws-1");
    (sc.arrange.layer.querySelector("[data-workspace-name]") as HTMLInputElement).value = "better-name";

    (sc.arrange.layer.querySelector("[data-workspace-submit]") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));

    expect(sc.arrange.layer.querySelector("[data-workspace-popup] .refusal[data-arm='nameTaken']")).not.toBeNull();
  });

  it.each([
    ["the Recently Merged band", () => roster({ repos: [repoSection({ id: "repo-1" })], merged: mergedSection([row({ id: "ws-1" })]) })],
    ["the Hidden section", () => roster({ repos: [repoSection({ id: "repo-1" })], hidden: hiddenSection([row({ id: "ws-1" })]) })],
  ])("offers no rename in %s, whose click is the band's own gesture", (_where, made) => {
    const { sc } = renamed(taken);
    const pane = mount(made(), sc);

    doubleClickName(pane, "ws-1");

    expect(sc.arrange.layer.querySelector("[data-workspace-popup]")).toBeNull();
  });
});

describe("setWorkspaceNameRefusal", () => {
  it.each([
    ["a slug", { case: "invalidName", value: { name: "Bad", component: "Bad", rule: { case: "slug", value: {} } } }, '"Bad" is not lowercase words joined by hyphens'],
    ["a prefix", { case: "invalidName", value: { name: "A B/x", component: "A B", rule: { case: "prefix", value: {} } } }, 'the prefix "A B" is not letters and digits'],
    ["a workspace's name", { case: "nameTaken", value: { name: "x", holder: { case: "workspace", value: { id: "w" } } } }, "another workspace is already named x, or works on that branch"],
    ["a pending name", { case: "nameTaken", value: { name: "x", holder: { case: "pending", value: {} } } }, "x is being given to a workspace that is being created"],
  ] as const)("words %s", (_name, cause, want) => {
    const error = create(SetWorkspaceNameErrorSchema, { cause: cause as never });
    expect(setWorkspaceNameRefusal(error.cause as CauseOf<typeof error>)).toBe(want);
  });
});
