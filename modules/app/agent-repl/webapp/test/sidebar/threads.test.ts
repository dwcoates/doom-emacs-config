// @vitest-environment jsdom
/**
 * THE SIDEBAR'S APPS: the rail title's switcher and the Chat app's thread
 * list, drawn from the roster and acted on through the daemon.
 */
import { create } from "@bufbuild/protobuf";
import { createRouterTransport, type ServiceImpl } from "@connectrpc/connect";
import { beforeEach, describe, expect, it } from "vitest";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { RequestNewThreadResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_new_thread_pb";
import { SelectWorkspaceResponseSchema, type SelectWorkspaceRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { ShowSidebarAppResponseSchema, type ShowSidebarAppRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_sidebar_app_pb";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import type { WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { mountSidebar } from "../../src/sidebar/sidebar.js";
import { frameQueue, type FrameQueue } from "../frame-queue.js";
import { testAppContext } from "../rpc/app-context.js";
import { NOW, SINK, WORKSPACE, fakeTicker, fakeTimers, repoSection, roster, rosterStream, row } from "./harness.js";

let frames: FrameQueue;
beforeEach(() => {
  frames = frameQueue();
});

/** Mount a rail fed ROSTERS over a daemon answering IMPL. */
function mount(rosters: WorkspaceRoster[], impl: Partial<ServiceImpl<typeof AgentRepl>> = {}): HTMLElement {
  const transport = createRouterTransport(({ service }) => {
    service(AgentRepl, {
      updateSidebarView: () => create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } }),
      ...rosterStream(rosters),
      ...impl,
    });
  });
  const ctx = testAppContext({
    client: createAgentReplClient(transport),
    workspace: WORKSPACE,
    ticker: fakeTicker(NOW),
    failures: SINK,
    composerEnabled: false,
  });
  const host = document.createElement("nav");
  document.body.append(host);
  mountSidebar(host, ctx, { frame: frames.frame, timers: fakeTimers() });
  return host;
}

async function settle(): Promise<void> {
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
  frames.drain();
}

async function click(control: Element | null): Promise<void> {
  if (control === null) throw new Error("no such control");
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await settle();
}

describe("the app switcher", () => {
  it.each([
    ["Workspaces", false],
    ["Chat", true],
  ])("titles the rail %s and marks it in the menu", async (title, chat) => {
    // Arrange / Act
    const host = mount([chat ? roster({ chat: [] }) : roster()]);
    await settle();

    // Assert
    expect(host.querySelector("[data-app-title]")?.textContent).toContain(title);
    expect(host.querySelector(`[data-app-option="${title}"] .sb-app-check`)?.textContent).toBe("✓");
  });

  it("opens on the title, and picking an app asks the daemon to show it", async () => {
    // Arrange
    const shown: ShowSidebarAppRequest[] = [];
    const host = mount([roster()], {
      showSidebarApp: (request) => {
        shown.push(request);
        return create(ShowSidebarAppResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
    });
    await settle();
    const menu = host.querySelector<HTMLElement>("[data-apps-menu]");

    // Act
    await click(host.querySelector("[data-app-title]"));
    const opened = menu?.hidden === false;
    await click(host.querySelector('[data-app-option="Chat"]'));

    // Assert
    expect(opened).toBe(true);
    expect(shown).toHaveLength(1);
    expect(shown[0].app?.app.case).toBe("chat");
    expect(menu?.hidden).toBe(true);
  });
});

describe("the Chat app's body", () => {
  it("lists the threads in place of the workspaces", async () => {
    // Arrange / Act
    const host = mount([
      roster({
        repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "agent work" })] })],
        chat: [
          { id: "t-2", name: "newer", summary: "light", current: true },
          { id: "t-1", name: "older" },
        ],
      }),
    ]);
    await settle();

    // Assert
    const rows = [...host.querySelectorAll("[data-thread-section] [data-roster-row]")];
    expect(rows.map((r) => r.querySelector(".name")?.textContent)).toEqual(["newer", "older"]);
    expect(rows[0].getAttribute("data-current")).toBe("true");
    expect(rows[0].querySelector(".sb-thread-summary")?.textContent).toBe("light");
    expect(host.querySelector('[data-roster-row="ws-1"]')).toBeNull();
  });

  it("selects a thread its row is clicked for", async () => {
    // Arrange
    const selected: SelectWorkspaceRequest[] = [];
    const host = mount([roster({ chat: [{ id: "t-1", name: "why" }] })], {
      selectWorkspace: (request) => {
        selected.push(request);
        return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
    });
    await settle();

    // Act
    await click(host.querySelector('[data-roster-row="t-1"] .row'));

    // Assert
    expect(selected.map((r) => r.workspace?.id)).toEqual(["t-1"]);
  });

  it.each([
    ["asks the editor for a new thread", { case: "success", value: {} }, true],
    ["says so when no editor is connected", { case: "error", value: { cause: { case: "noEditor", value: {} } } }, false],
  ])("the new-thread button %s", async (_name, result, noteHidden) => {
    // Arrange
    let asked = 0;
    const host = mount([roster({ chat: [] })], {
      requestNewThread: () => {
        asked += 1;
        return create(RequestNewThreadResponseSchema, { result: result as never });
      },
    });
    await settle();

    // Act
    await click(host.querySelector("[data-new-thread]"));
    await settle();

    // Assert
    expect(asked).toBe(1);
    expect(host.querySelector<HTMLElement>(".sb-new-thread-note")?.hidden).toBe(noteHidden);
  });

  it("is narrowed by the rail's search", async () => {
    // Arrange
    const host = mount([roster({ chat: [{ id: "t-1", name: "why blue" }, { id: "t-2", name: "rust lifetimes" }] })]);
    await settle();

    // Act
    (host.querySelector("[data-search-toggle]") as HTMLElement).click();
    const input = host.querySelector("[data-search-input]") as HTMLInputElement;
    input.value = "rust";
    input.dispatchEvent(new Event("input"));

    // Assert
    const kept = [...host.querySelectorAll("[data-thread-section] [data-roster-row]:not(.sb-search-miss)")];
    expect(kept.map((r) => r.getAttribute("data-roster-row"))).toEqual(["t-2"]);
  });
});
