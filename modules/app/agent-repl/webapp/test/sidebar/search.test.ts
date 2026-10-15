// @vitest-environment jsdom
import { afterEach, describe, expect, it } from "vitest";
import { drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import {
  SEARCH_EMPTY_CLASS,
  SEARCH_MISS_CLASS,
  applySearch,
  createRailSearch,
  matchesSearch,
  searchPattern,
} from "../../src/sidebar/search.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import {
  hiddenSection,
  mergedBand,
  mergedGroup,
  mergedSection,
  repoSection,
  roster,
  row,
  sidebarContext,
} from "./harness.js";

/** Whether the drawn row for ID is hidden by the search. */
function missed(root: ParentNode, id: string): boolean[] {
  return [...root.querySelectorAll(`[data-roster-row='${id}']`)].map((el) => el.classList.contains(SEARCH_MISS_CLASS));
}

/** A drawn roster, ready to search. */
function drawn(init: Parameters<typeof roster>[0]): HTMLElement {
  return drawWorkspaceRoster(roster(init), sidebarContext());
}

describe("searchPattern", () => {
  it("answers null for a blank query", () => {
    expect(searchPattern("")).toBeNull();
  });

  it("answers null for a query of whitespace alone", () => {
    expect(searchPattern("  \t ")).toBeNull();
  });
});

describe("matchesSearch", () => {
  it.each([
    ["matches a word found anywhere in the name", "side", "fix-the-sidebar", true],
    ["treats whitespace as a wildcard between words", "fix side", "fix-the-sidebar", true],
    ["keeps the words in order", "side fix", "fix-the-sidebar", false],
    ["ignores case", "SIDE", "fix-the-sidebar", true],
    ["reads a word's regexp characters literally", "a.b", "axb", false],
    ["treats a run of whitespace as one wildcard", "fix   bar", "fix-the-sidebar", true],
    ["matches every name on a blank query", "", "anything", true],
    ["refuses a name missing a word", "fix zzz", "fix-the-sidebar", false],
  ])("%s", (_case, query, name, expected) => {
    // Act
    const matched = matchesSearch(name, query);
    // Assert
    expect(matched).toBe(expected);
  });
});

describe("applySearch", () => {
  it("hides a repository section's row the query does not match", () => {
    // Arrange
    const root = drawn({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" })] })] });
    // Act
    applySearch(root, "beta");
    // Assert
    expect(missed(root.querySelector(".sb-pane") as Element, "ws-1")).toEqual([true]);
  });

  it("keeps a repository section's row the query matches", () => {
    const root = drawn({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" })] })] });
    applySearch(root, "alp");
    expect(missed(root.querySelector(".sb-pane") as Element, "ws-1")).toEqual([false]);
  });

  it("narrows Hidden Workspaces", () => {
    const root = drawn({ hidden: hiddenSection([row({ id: "ws-h", name: "alpha" })]) });
    applySearch(root, "beta");
    expect(missed(root, "ws-h")).toEqual([true]);
  });

  it("narrows Recently Merged", () => {
    const root = drawn({ merged: mergedSection([row({ id: "ws-m", name: "alpha" })], 1, false) });
    applySearch(root, "beta");
    expect(missed(root, "ws-m")).toEqual([true]);
  });

  it("keeps a row whose nested child matches, so the child keeps its family", () => {
    const root = drawn({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-p", name: "parent", children: [row({ id: "ws-c", name: "child" })] })] })],
    });
    applySearch(root, "child");
    const pane = root.querySelector(".sb-pane") as Element;
    expect([missed(pane, "ws-p"), missed(pane, "ws-c")]).toEqual([[false], [false]]);
  });

  it("hides a nested child the query does not match under a parent it does", () => {
    const root = drawn({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-p", name: "parent", children: [row({ id: "ws-c", name: "child" })] })] })],
    });
    applySearch(root, "parent");
    expect(missed(root.querySelector(".sb-pane") as Element, "ws-c")).toEqual([true]);
  });

  it("hides a section the query leaves no row in", () => {
    const root = drawn({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" })] })] });
    applySearch(root, "beta");
    expect(root.querySelector(".repo-section")?.classList.contains(SEARCH_EMPTY_CLASS)).toBe(true);
  });

  it("keeps a section the query leaves a row in", () => {
    const root = drawn({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" }), row({ id: "ws-2", name: "beta" })] })],
    });
    applySearch(root, "beta");
    expect(root.querySelector(".repo-section")?.classList.contains(SEARCH_EMPTY_CLASS)).toBe(false);
  });

  it("hides a merged group the query leaves no row in", () => {
    const root = drawn({
      merged: mergedBand(
        [
          mergedGroup({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" })] }),
          mergedGroup({ id: "repo-2", rows: [row({ id: "ws-2", name: "beta" })] }),
        ],
        2,
        false,
      ),
    });
    applySearch(root, "beta");
    const group = root.querySelector(".merged-group[data-repository='repo-1']");
    expect(group?.classList.contains(SEARCH_EMPTY_CLASS)).toBe(true);
  });

  it("keeps a section with no rows on a blank query", () => {
    const root = drawn({ repos: [repoSection({ id: "repo-1", rows: [] })] });
    applySearch(root, "");
    expect(root.querySelector(".repo-section")?.classList.contains(SEARCH_EMPTY_CLASS)).toBe(false);
  });

  it("shows every row again on a blank query", () => {
    const root = drawn({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" })] })] });
    applySearch(root, "beta");
    applySearch(root, "");
    expect(root.querySelectorAll(`.${SEARCH_MISS_CLASS}, .${SEARCH_EMPTY_CLASS}`).length).toBe(0);
  });

  it("answers how many drawn rows it kept and hid", () => {
    const root = drawn({
      repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1", name: "alpha" }), row({ id: "ws-2", name: "beta" })] })],
    });
    expect(applySearch(root, "beta")).toEqual({ kept: 1, missed: 1 });
  });

  it("refuses a row drawn without its name, and logs it", async () => {
    // Arrange
    const capture = captureLogRecords();
    const root = drawn({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "ws-1" })] })] });
    root.querySelector(".sb-pane [data-roster-row='ws-1'] .name")?.remove();
    // Act
    expect(() => applySearch(root, "x")).toThrow(/ws-1 has no name/);
    // Assert
    const record = await forwardedRecord(capture, "sidebar.search.malformed");
    expect([record.level.case, record.context]).toMatchObject(["error", { workspace: "ws-1" }]);
  });
});

describe("createRailSearch", () => {
  afterEach(() => {
    document.body.replaceChildren();
  });

  /** A search on the page, with every query it was told. */
  function mounted(): { search: ReturnType<typeof createRailSearch>; told: string[]; input: HTMLInputElement } {
    const told: string[] = [];
    const search = createRailSearch((query) => told.push(query));
    document.body.append(search.toggle, search.bar);
    const input = search.bar.querySelector("[data-search-input]") as HTMLInputElement;
    return { search, told, input };
  }

  function type(input: HTMLInputElement, value: string): void {
    input.value = value;
    input.dispatchEvent(new Event("input"));
  }

  it("starts closed", () => {
    const { search } = mounted();
    expect([search.isOpen(), search.bar.classList.contains("open")]).toEqual([false, false]);
  });

  it("opens on the toggle and focuses its input", () => {
    const { search, input } = mounted();
    search.toggle.click();
    expect([search.isOpen(), document.activeElement === input]).toEqual([true, true]);
  });

  it("lights the toggle while open", () => {
    const { search } = mounted();
    search.toggle.click();
    expect(search.toggle.classList.contains("active")).toBe(true);
  });

  it("tells every typed query", () => {
    const { search, told, input } = mounted();
    search.toggle.click();
    type(input, "fix side");
    expect([told, search.query()]).toEqual([["fix side"], "fix side"]);
  });

  it("clears the query when the toggle closes it, and says so", () => {
    const { search, told, input } = mounted();
    search.toggle.click();
    type(input, "fix");
    search.toggle.click();
    expect([search.isOpen(), input.value, told]).toEqual([false, "", ["fix", ""]]);
  });

  it("says nothing on closing with no query", () => {
    const { search, told } = mounted();
    search.toggle.click();
    search.toggle.click();
    expect(told).toEqual([]);
  });

  it("closes on Escape", () => {
    const { search, input } = mounted();
    search.toggle.click();
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(search.isOpen()).toBe(false);
  });

  it("keeps the input out of the tab order while closed", () => {
    const { input } = mounted();
    expect(input.tabIndex).toBe(-1);
  });
});
