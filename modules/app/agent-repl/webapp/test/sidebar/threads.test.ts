// @vitest-environment jsdom
/**
 * THE SIDEBAR'S APPS: the rail title's switcher and the Chat app's thread
 * list, drawn from the roster and acted on through the daemon.
 */
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import { createRouterTransport, type ServiceImpl } from "@connectrpc/connect";
import { beforeEach, describe, expect, it } from "vitest";
import { AgentRepl } from "../../../proto/gen/ts/agentrepl/v1/service_pb";
import { RequestNewThreadResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_request_new_thread_pb";
import { SelectWorkspaceResponseSchema, type SelectWorkspaceRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { ShowSidebarAppResponseSchema, type ShowSidebarAppRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_sidebar_app_pb";
import { ShowChatNodeResponseSchema, type ShowChatNodeRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_show_chat_node_pb";
import { CloseWorkspaceResponseSchema, type CloseWorkspaceRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_workspace_pb";
import { SearchChatsResponseSchema, type SearchChatsRequest } from "../../../proto/gen/ts/agentrepl/v1/endpoint_search_chats_pb";
import { ARCHIVED_HEADING, OPEN_HEADING } from "../../src/sidebar/threads.js";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import type { RosterThreadOutlineSchema, WorkspaceRoster } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { createAgentReplClient } from "../../src/rpc/client.js";
import { mountSidebar } from "../../src/sidebar/sidebar.js";
import { frameQueue, type FrameQueue } from "../frame-queue.js";
import { testAppContext } from "../rpc/app-context.js";
import { NOW, SINK, WORKSPACE, fakeTicker, fakeTimers, repoSection, roster, rosterStream, row } from "./harness.js";

let frames: FrameQueue;
beforeEach(() => {
  frames = frameQueue();
});

let timers: ReturnType<typeof fakeTimers>;

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
  timers = fakeTimers();
  mountSidebar(host, ctx, { frame: frames.frame, timers });
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
    ["Chats", true],
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
    await click(host.querySelector('[data-app-option="Chats"]'));

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

  it("draws the new-thread control as the green \"+\" beside the open threads' heading", async () => {
    // Arrange / Act
    const host = mount([roster({ chat: [{ id: "t-1", name: "why" }] })]);
    await settle();

    // Assert
    const heading = host.querySelector(".sb-threads-open");
    const plus = heading?.querySelector("[data-new-thread]");
    expect([heading?.firstChild?.textContent, plus?.className, plus?.textContent]).toEqual([OPEN_HEADING, "sb-add", "+"]);
  });

  it("draws a thread's menu control after its name, at the row's right edge", async () => {
    // Arrange / Act
    const host = mount([roster({ chat: [{ id: "t-1", name: "why" }] })]);
    await settle();

    // Assert
    const line = host.querySelector('[data-roster-row="t-1"] > .row');
    expect([...(line?.children ?? [])].map((c) => c.className)).toEqual(["name", "sb-more"]);
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

describe("the Chat app's archived threads", () => {
  it("are listed after every open thread, under their heading", async () => {
    // Arrange / Act
    const host = mount([
      roster({
        chat: [
          { id: "t-2", name: "gone", archived: true },
          { id: "t-1", name: "here" },
        ],
      }),
    ]);
    await settle();

    // Assert
    const section = host.querySelector("[data-thread-section]")!;
    // A heading reads as its words, the open one's "+" aside.
    const order = [...section.querySelectorAll(".sb-threads-heading, [data-roster-row]")].map((el) =>
      el.classList.contains("sb-threads-heading") ? el.firstChild?.textContent : el.getAttribute("data-roster-row"),
    );
    expect(order).toEqual([OPEN_HEADING, "t-1", ARCHIVED_HEADING, "t-2"]);
    expect(host.querySelector('[data-roster-row="t-2"]')?.classList.contains("sb-thread-archived")).toBe(true);
  });

  it("draws no archived heading when no thread is archived", async () => {
    // Arrange / Act
    const host = mount([roster({ chat: [{ id: "t-1", name: "here" }] })]);
    await settle();

    // Assert
    expect(host.querySelector(".sb-threads-archived-heading")).toBeNull();
  });
});

describe("a thread row's menu", () => {
  it.each([
    ["an open thread", false, ["close", "nuke"], ["Archive", "Delete…"]],
    ["an archived thread", true, ["open", "nuke"], ["Restore", "Delete…"]],
  ])("offers %s's verbs under the thread's names for them", async (_name, archived, verbs, labels) => {
    // Arrange
    const host = mount([roster({ chat: [{ id: "t-1", name: "why", archived }] })]);
    await settle();

    // Act
    await click(host.querySelector('[data-roster-row="t-1"] .sb-more'));

    // Assert
    const menu = host.querySelector<HTMLElement>('[data-roster-row="t-1"] > .sb-menu')!;
    expect(menu.hidden).toBe(false);
    const items = [...menu.querySelectorAll(":scope > .sb-menu-row > .sb-menu-item")];
    expect(items.map((b) => b.getAttribute("data-verb") ?? "")).toEqual([verbs[0], ""]);
    expect(items.map((b) => b.textContent)).toEqual(labels);
    expect(menu.querySelector('[data-verb="nuke"]')?.textContent).toBe("Delete");
  });

  it("archives the thread by closing its workspace", async () => {
    // Arrange
    const closed: CloseWorkspaceRequest[] = [];
    const host = mount([roster({ chat: [{ id: "t-1", name: "why" }] })], {
      closeWorkspace: (request) => {
        closed.push(request);
        return create(CloseWorkspaceResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    await settle();
    await click(host.querySelector('[data-roster-row="t-1"] .sb-more'));

    // Act
    await click(host.querySelector('[data-roster-row="t-1"] [data-verb="close"]'));

    // Assert
    expect(closed.map((r) => r.workspace?.id)).toEqual(["t-1"]);
  });

  it("does not select the thread when its menu is opened", async () => {
    // Arrange
    let selects = 0;
    const host = mount([roster({ chat: [{ id: "t-1", name: "why" }] })], {
      selectWorkspace: () => {
        selects += 1;
        return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
    });
    await settle();

    // Act
    await click(host.querySelector('[data-roster-row="t-1"] .sb-more'));

    // Assert
    expect(selects).toBe(0);
  });
});

describe("the rail's search in the Chat app", () => {
  /** Type QUERY into the rail's search. */
  function type(host: HTMLElement, query: string): void {
    if (!host.querySelector("[data-search-bar]")?.classList.contains("open")) {
      (host.querySelector("[data-search-toggle]") as HTMLElement).click();
    }
    const input = host.querySelector("[data-search-input]") as HTMLInputElement;
    input.value = query;
    input.dispatchEvent(new Event("input"));
  }

  it("keeps a thread whose chat matches though its name does not, with its snippet", async () => {
    // Arrange
    const asked: SearchChatsRequest[] = [];
    const host = mount([roster({ chat: [{ id: "t-1", name: "why blue" }, { id: "t-2", name: "rust lifetimes" }] })], {
      searchChats: (request) => {
        asked.push(request);
        return create(SearchChatsResponseSchema, {
          result: {
            case: "success",
            value: {
              matches: [
                {
                  thread: { id: "t-1", dir: "/zettel/threads/why blue" },
                  snippet: { parts: [{ part: { case: "plain", value: "Rayleigh " } }, { part: { case: "hit", value: "scattering" } }] },
                },
              ],
            },
          },
        });
      },
    });
    await settle();

    // Act
    type(host, "scattering");
    timers.run();
    await settle();

    // Assert
    expect(asked.map((r) => r.query)).toEqual(["scattering"]);
    const kept = [...host.querySelectorAll("[data-thread-section] [data-roster-row]:not(.sb-search-miss)")];
    expect(kept.map((r) => r.getAttribute("data-roster-row"))).toEqual(["t-1"]);
    const snippet = host.querySelector('[data-roster-row="t-1"] .sb-thread-snippet')!;
    expect(snippet.textContent).toBe("Rayleigh scattering");
    expect(snippet.querySelector("mark")?.textContent).toBe("scattering");
  });

  it("asks nothing until the typing pauses, and only the latest words", async () => {
    // Arrange
    const asked: string[] = [];
    const host = mount([roster({ chat: [{ id: "t-1", name: "why" }] })], {
      searchChats: (request) => {
        asked.push(request.query);
        return create(SearchChatsResponseSchema, { result: { case: "success", value: { matches: [] } } });
      },
    });
    await settle();

    // Act
    type(host, "sc");
    type(host, "scat");
    await settle();
    const beforePause = [...asked];
    timers.run();
    await settle();

    // Assert
    expect(beforePause).toEqual([]);
    expect(asked).toEqual(["scat"]);
  });

  it("does not ask the chats while the Workspaces app is shown", async () => {
    // Arrange
    let asked = 0;
    const host = mount([roster()], {
      searchChats: () => {
        asked += 1;
        return create(SearchChatsResponseSchema, { result: { case: "success", value: { matches: [] } } });
      },
    });
    await settle();

    // Act
    type(host, "scattering");
    timers.run();
    await settle();

    // Assert
    expect(asked).toBe(0);
  });
});

describe("the current thread's outline", () => {
  /** An outline of three runs: the shown one, one on its path, one off it. */
  const OUTLINE: MessageInitShape<typeof RosterThreadOutlineSchema> = {
    breadcrumb: { extent: { case: "elided", value: { first: { node: { id: "n1" }, label: { text: "why?" } }, omitted: 4, last: { node: { id: "n7" }, label: { text: "deeper" } } } } },
    rows: [
      { node: { id: "n8" }, label: { text: "and then?" }, depth: 0, turns: 3, place: { place: { case: "onPath", value: {} } } },
      { node: { id: "n9" }, label: { text: "aside" }, depth: 1, turns: 1, folded: { runs: 5 }, place: { place: { case: "off", value: {} } } },
      { node: { id: "n10" }, label: { text: "here" }, depth: 1, turns: 1, place: { place: { case: "shown", value: {} } } },
    ],
  };

  it("draws a row per run, indented by its depth and marked by its place", async () => {
    // Arrange / Act
    const host = mount([roster({ chat: [{ id: "t-1", name: "why", current: true, outline: OUTLINE }] })]);
    await settle();

    // Assert
    const rows = [...host.querySelectorAll<HTMLElement>('[data-roster-row="t-1"] .sb-outline-row')];
    expect(
      rows.map((r) => [r.getAttribute("data-outline-node"), r.style.getPropertyValue("--outline-depth"), r.className]),
    ).toEqual([
      ["n8", "0", "sb-outline-row on-path"],
      ["n9", "1", "sb-outline-row"],
      ["n10", "1", "sb-outline-row shown"],
    ]);
    expect([rows[0].querySelector(".sb-outline-turns")?.textContent, rows[1].querySelector(".sb-outline-folded")?.textContent]).toEqual(["3", "+5"]);
  });

  it("draws an elided breadcrumb as its first and last crumbs around the count between", async () => {
    // Arrange / Act
    const host = mount([roster({ chat: [{ id: "t-1", name: "why", current: true, outline: OUTLINE }] })]);
    await settle();

    // Assert
    expect(host.querySelector(".sb-outline-crumbs")?.textContent).toBe("why? › … 4 more › deeper");
  });

  it.each([
    ["a row", ".sb-outline-row:nth-of-type(2)", "n8"],
    ["a crumb", ".sb-outline-crumb", "n1"],
  ])("shows the node %s is clicked for", async (_name, selector, want) => {
    // Arrange
    const shown: ShowChatNodeRequest[] = [];
    const host = mount([roster({ chat: [{ id: "t-1", name: "why", current: true, outline: OUTLINE }] })], {
      showChatNode: (request) => {
        shown.push(request);
        return create(ShowChatNodeResponseSchema, { result: { case: "success", value: {} } });
      },
    });
    await settle();

    // Act
    await click(host.querySelector(`[data-thread-outline] ${selector}`));

    // Assert
    expect(shown.map((r) => [r.thread?.id, r.node?.id])).toEqual([["t-1", want]]);
  });

  it("draws nothing under a row the roster gives no outline", async () => {
    // Arrange / Act
    const host = mount([roster({ chat: [{ id: "t-1", name: "why", current: true }] })]);
    await settle();

    // Assert
    expect(host.querySelector("[data-thread-outline]")).toBeNull();
  });
});
