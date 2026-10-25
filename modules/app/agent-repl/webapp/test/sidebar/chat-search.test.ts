// @vitest-environment jsdom
/**
 * THE RAIL SEARCH'S CONTENT HALF: which threads' chats say the typed words,
 * asked once the typing pauses, newest query only, drawn onto the rows.
 */
import { create } from "@bufbuild/protobuf";
import { afterEach, describe, expect, it, vi } from "vitest";
import { SearchChatsResponseSchema, type SearchChatsResponse } from "../../../proto/gen/ts/agentrepl/v1/endpoint_search_chats_pb";
import { log } from "../../src/log.js";
import { CHAT_SNIPPET_CLASS, createChatSearch } from "../../src/sidebar/chat-search.js";
import { SEARCH_MISS_CLASS } from "../../src/sidebar/search.js";
import { appContext, fakeTimers } from "./harness.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** A drawn thread section holding rows IDS, each missed by the name search. */
function drawnSection(...ids: string[]): HTMLElement {
  const root = document.createElement("div");
  const section = document.createElement("div");
  section.setAttribute("data-thread-section", "");
  const rows = document.createElement("div");
  rows.className = "rows";
  for (const id of ids) {
    const ws = document.createElement("div");
    ws.setAttribute("data-roster-row", id);
    ws.classList.add(SEARCH_MISS_CLASS);
    rows.append(ws);
  }
  section.append(rows);
  root.append(section);
  return root;
}

/** A success answering THREAD with one hit. */
function answer(thread: string, hit: string): SearchChatsResponse {
  return create(SearchChatsResponseSchema, {
    result: {
      case: "success",
      value: { matches: [{ thread: { id: thread, dir: `/z/${thread}` }, snippet: { parts: [{ part: { case: "hit", value: hit } }] } }] },
    },
  });
}

/** Let the answered call's promise settle. */
async function settle(): Promise<void> {
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

/** A chat search over ROOT whose daemon answers ANSWERS in turn. */
function searchOver(root: HTMLElement, answers: Array<() => SearchChatsResponse | Promise<SearchChatsResponse>>) {
  const timers = fakeTimers();
  const asked: string[] = [];
  let applied = 0;
  const search = createChatSearch({
    ctx: appContext({
      searchChats: (request) => {
        asked.push(request.query);
        const next = answers.shift();
        if (next === undefined) throw new Error("an unexpected search");
        return next();
      },
    }),
    timers,
    root: () => root,
    applied: () => {
      applied += 1;
    },
  });
  return { search, timers, asked, applied: () => applied };
}

describe("createChatSearch", () => {
  it("draws a match's snippet onto its row and keeps it", async () => {
    // Arrange
    const root = drawnSection("t-1", "t-2");
    const s = searchOver(root, [() => answer("t-1", "sky")]);

    // Act
    s.search.changed("sky", true);
    s.timers.run();
    await settle();

    // Assert
    const hit = root.querySelector('[data-roster-row="t-1"]')!;
    expect(hit.classList.contains(SEARCH_MISS_CLASS)).toBe(false);
    expect(hit.querySelector(`.${CHAT_SNIPPET_CLASS} mark`)?.textContent).toBe("sky");
    expect(root.querySelector('[data-roster-row="t-2"]')?.classList.contains(SEARCH_MISS_CLASS)).toBe(true);
    expect(s.applied()).toBe(1);
  });

  it("drops an answer for a query the bar no longer holds", async () => {
    // Arrange: the first answer is held until the query has changed.
    const root = drawnSection("t-1");
    let release: (r: SearchChatsResponse) => void = () => {};
    const s = searchOver(root, [() => new Promise((resolve) => (release = resolve))]);
    s.search.changed("sky", true);
    s.timers.run();
    await settle();

    // Act
    s.search.changed("", true);
    release(answer("t-1", "sky"));
    await settle();

    // Assert
    expect(root.querySelector(`.${CHAT_SNIPPET_CLASS}`)).toBeNull();
    expect(s.applied()).toBe(0);
  });

  it.each([
    ["a blank query", "   ", true],
    ["the Workspaces app shown", "sky", false],
  ])("asks nothing for %s", async (_name, query, chat) => {
    // Arrange
    const s = searchOver(drawnSection("t-1"), []);

    // Act
    s.search.changed(query, chat);
    s.timers.run();
    await settle();

    // Assert
    expect(s.asked).toEqual([]);
  });

  it("clears the last query's snippets when the query changes", async () => {
    // Arrange
    const root = drawnSection("t-1");
    const s = searchOver(root, [() => answer("t-1", "sky")]);
    s.search.changed("sky", true);
    s.timers.run();
    await settle();

    // Act
    s.search.changed("", true);
    s.search.apply(root);

    // Assert
    expect(root.querySelector(`.${CHAT_SNIPPET_CLASS}`)).toBeNull();
  });

  it("asks once for a query repeated by a redraw", async () => {
    // Arrange
    const s = searchOver(drawnSection("t-1"), [() => answer("t-1", "sky")]);

    // Act
    s.search.changed("sky", true);
    s.search.changed("sky", true);
    s.timers.run();
    await settle();

    // Assert
    expect(s.asked).toEqual(["sky"]);
  });

  it("records a refused search at info and draws nothing", async () => {
    // Arrange
    const info = vi.spyOn(log, "info");
    const root = drawnSection("t-1");
    const s = searchOver(root, [
      () => create(SearchChatsResponseSchema, { result: { case: "error", value: { cause: { case: "graphFailed", value: { detail: "disk" } } } } }),
    ]);

    // Act
    s.search.changed("sky", true);
    s.timers.run();
    await settle();

    // Assert
    expect(info).toHaveBeenCalledWith("SearchChats was refused: graphFailed", expect.objectContaining({ operation: "sidebar.chat-search.refused" }));
    expect(root.querySelector(`.${CHAT_SNIPPET_CLASS}`)).toBeNull();
  });

  it("asks nothing once disposed", async () => {
    // Arrange
    const s = searchOver(drawnSection("t-1"), []);
    s.search.changed("sky", true);

    // Act
    s.search.dispose();
    s.timers.run();
    await settle();

    // Assert
    expect(s.asked).toEqual([]);
  });
});
