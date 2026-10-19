// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  ArrangeSidebarResponseSchema,
  type ArrangeSidebarRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_arrange_sidebar_pb";
import { disposeWorkspaceRoster, drawWorkspaceRoster } from "../../src/sidebar/roster.js";
import { DRAG_GHOST_CLASS, DRAG_SLOT_CLASS, REARRANGED_ATTRIBUTE } from "../../src/sidebar/drag.js";
import type { RailContext, SidebarContext } from "../../src/sidebar/context.js";
import { frameQueue } from "../frame-queue.js";
import { queryAll } from "../../src/dom.js";
import { appContext, hiddenSection, primaryPointer, repoSection, roster, row, sidebarContext } from "./harness.js";

/** Every ArrangeSidebar request the daemon received, and a context over it. */
function arranged(): { sc: RailContext & SidebarContext; sent: ArrangeSidebarRequest[] } {
  const sent: ArrangeSidebarRequest[] = [];
  const sc = sidebarContext(
    appContext({
      arrangeSidebar: (request) => {
        sent.push(request);
        return create(ArrangeSidebarResponseSchema, { result: { case: "success", value: { rosterRevision: 1n } } });
      },
    }),
  );
  return { sc, sent };
}

/** Draw ROSTER into the document, the rail's layer beside it; answers the open repository grouping. */
function mount(made: ReturnType<typeof roster>, sc: RailContext & SidebarContext): HTMLElement {
  const drawn = drawWorkspaceRoster(made, sc);
  document.body.replaceChildren(sc.arrange.status, drawn, sc.arrange.layer);
  return drawn.querySelector(".sb-sections") as HTMLElement;
}

const ROW_HEIGHT = 20;

/**
 * Lay LIST out at TOP: each item a ROW_HEIGHT band in its current place, so
 * a slot that travels moves the bands under it, as a browser lays them out.
 */
function layOut(list: HTMLElement, top: number): void {
  const box = (y: number, height: number): DOMRect => ({ top: y, bottom: y + height, height, left: 0, right: 200, width: 200, x: 0, y, toJSON: () => ({}) });
  Object.defineProperty(list, "getBoundingClientRect", {
    configurable: true,
    value: () => box(top, Math.max(ROW_HEIGHT, list.children.length * ROW_HEIGHT)),
  });
  for (const item of [...list.children] as HTMLElement[]) {
    Object.defineProperty(item, "getBoundingClientRect", {
      configurable: true,
      value: () => box(top + [...list.children].indexOf(item) * ROW_HEIGHT, ROW_HEIGHT),
    });
  }
}

const pointer = primaryPointer;

/** Press HANDLE at FROM, drag it to TO, and release. */
async function drag(handle: Element, from: number, to: number): Promise<void> {
  pointer("pointerdown", handle, from);
  pointer("pointermove", window, from + 1);
  pointer("pointermove", window, to);
  pointer("pointerup", window, to);
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

function rowLine(pane: HTMLElement, id: string): HTMLElement {
  return pane.querySelector(`[data-roster-row='${id}'] > .row`) as HTMLElement;
}

beforeEach(() => {
  vi.stubGlobal("requestAnimationFrame", () => 0);
});
afterEach(() => {
  vi.unstubAllGlobals();
  document.body.replaceChildren();
});

describe("a repository's subsections", () => {
  it("draw no box for a repository with none", () => {
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" })] }), sidebarContext());
    expect(pane.querySelector(".sb-subsections")).toBeNull();
  });

  it("draw below the section's own rows, each with its name, count and rows", () => {
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", label: "later", rows: [row({ id: "ws-1" })] }] })] }),
      sidebarContext(),
    );
    const sub = pane.querySelector(".repo-section > .sb-subsections > .sb-subsection") as HTMLElement;
    expect([
      sub.getAttribute("data-subsection"),
      sub.querySelector(".sb-subsection-label")?.textContent,
      sub.querySelector(".sb-count")?.textContent,
      sub.querySelector(":scope > .rows > [data-roster-row='ws-1']") !== null,
    ]).toEqual(["sub-1", "later", "(1)", true]);
  });

  it("draw a slot to drop into when empty", () => {
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sidebarContext());
    expect(pane.querySelector(".sb-subsection > .rows")?.children.length).toBe(0);
    expect(pane.querySelector(".sb-subsection > .sb-subsection-empty")).not.toBeNull();
  });

  it("delete through the header's remove control", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sc);
    (pane.querySelector("[data-subsection-delete='sub-1']") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect(sent.map((r) => r.change.case)).toEqual(["deleteSubsection"]);
  });

  it("rename through a popup opened by a double-click on the name", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1", label: "old" }] })] }), sc);
    pane.querySelector(".sb-subsection-label")?.dispatchEvent(new MouseEvent("dblclick", { bubbles: true }));
    const input = sc.arrange.layer.querySelector("[data-subsection-name]") as HTMLInputElement;
    expect(input.value).toBe("old");
    input.value = "new";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    const change = sent[0]?.change;
    expect(change?.case === "renameSubsection" ? [change.value.subsection?.id, change.value.name] : null).toEqual(["sub-1", "new"]);
  });
});

describe("the new-subsection popup", () => {
  function opened(): { sc: RailContext & SidebarContext; sent: ArrangeSidebarRequest[]; input: HTMLInputElement } {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    (pane.querySelector("[data-subsection-create='repo-1']") as HTMLElement).click();
    return { sc, sent, input: sc.arrange.layer.querySelector("[data-subsection-name]") as HTMLInputElement };
  }

  it("creates the subsection in its repository with the name typed, and closes", async () => {
    const { sc, sent, input } = opened();
    input.value = "  later ";
    (sc.arrange.layer.querySelector("[data-subsection-submit]") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    const change = sent[0]?.change;
    expect(change?.case === "createSubsection" ? [change.value.repository?.id, change.value.name] : null).toEqual(["repo-1", "later"]);
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("sends nothing for a blank name", async () => {
    const { sc, sent, input } = opened();
    input.value = "   ";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect([sent.length, sc.arrange.layer.querySelector("[data-subsection-popup]") !== null]).toEqual([0, true]);
  });

  it("closes on Escape", () => {
    const { sc, input } = opened();
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("closes on a press outside it", () => {
    const { sc } = opened();
    document.body.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup]")).toBeNull();
  });

  it("keeps the popup open when the subsection is refused", async () => {
    const sc = sidebarContext(
      appContext({
        arrangeSidebar: () =>
          create(ArrangeSidebarResponseSchema, { result: { case: "error", value: { cause: { case: "unknownRepository", value: {} } } } }),
      }),
    );
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" })] }), sc);
    (pane.querySelector("[data-subsection-create='repo-1']") as HTMLElement).click();
    (sc.arrange.layer.querySelector("[data-subsection-name]") as HTMLInputElement).value = "later";
    (sc.arrange.layer.querySelector("[data-subsection-submit]") as HTMLElement).click();
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect(sc.arrange.layer.querySelector("[data-subsection-popup] .refusal[data-arm='unknownRepository']")).not.toBeNull();
  });
});

describe("dragging a workspace row", () => {
  it("places it before the row it was dropped above", async () => {
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" }), row({ id: "c" })] })] }),
      sc,
    );
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    await drag(rowLine(pane, "c"), 50, 5);

    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace"
        ? [change.value.workspace?.id, change.value.into.case, change.value.at.case, change.value.at.case === "before" ? change.value.at.value.id : null]
        : null,
    ).toEqual(["c", "noSubsection", "before", "a"]);
  });

  it("places it last in the empty subsection it was dropped into", async () => {
    const { sc, sent } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })], subsections: [{ id: "sub-1" }] })] }),
      sc,
    );
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);
    layOut(pane.querySelector(".sb-subsection > .rows") as HTMLElement, 100);

    await drag(rowLine(pane, "a"), 10, 110);

    const change = sent[0]?.change;
    expect(
      change?.case === "placeWorkspace" ? [change.value.workspace?.id, change.value.into.case === "subsection" ? change.value.into.value.id : null, change.value.at.case] : null,
    ).toEqual(["a", "sub-1", "end"]);
  });

  it("sends nothing when dropped where it started, and lets the hold go", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    await drag(rowLine(pane, "a"), 10, 8);

    expect([sent.length, sc.arrange.hold.holding()]).toEqual([0, false]);
  });

  it("does not lift under the threshold, so a click still selects", async () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })] })] }), sc);
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 12);
    expect([sc.arrange.layer.querySelector(`.${DRAG_GHOST_CLASS}`), sc.arrange.hold.holding()]).toEqual([null, false]);
    pointer("pointerup", window, 12);
  });

  it("lifts past the threshold: a copy follows the pointer and the row becomes its slot", () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);
    pointer("pointerdown", rowLine(pane, "a"), 10);
    pointer("pointermove", window, 20);
    const ghost = sc.arrange.layer.querySelector(`.${DRAG_GHOST_CLASS}`);
    expect([
      ghost !== null,
      ghost?.hasAttribute("data-roster-row"),
      pane.querySelector("[data-roster-row='a']")?.classList.contains(DRAG_SLOT_CLASS),
      sc.arrange.hold.holding(),
    ]).toEqual([true, false, true, true]);
    pointer("pointerup", window, 20);
  });

  it("puts the row back on Escape and sends nothing", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    const list = pane.querySelector(".repo-section > .rows") as HTMLElement;
    layOut(list, 0);
    pointer("pointerdown", rowLine(pane, "a"), 5);
    pointer("pointermove", window, 35);
    window.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
    expect([[...list.children].map((el) => el.getAttribute("data-roster-row")), sent.length, sc.arrange.hold.holding()]).toEqual([
      ["a", "b"],
      0,
      false,
    ]);
  });

  it("swallows the click the release leaves behind", async () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);
    await drag(rowLine(pane, "b"), 30, 2);
    const clicked = vi.fn();
    document.body.addEventListener("click", clicked);
    rowLine(pane, "b").dispatchEvent(new MouseEvent("click", { bubbles: true }));
    expect(clicked).not.toHaveBeenCalled();
    document.body.removeEventListener("click", clicked);
  });

  it("leaves a press on the row's menu control to the control", () => {
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })] })] }), sc);
    const menu = pane.querySelector("[data-roster-row='a'] > .row .sb-more") as HTMLElement;
    pointer("pointerdown", menu, 10);
    pointer("pointermove", window, 40);
    expect(sc.arrange.hold.holding()).toBe(false);
    pointer("pointerup", window, 40);
  });
});

describe("dragging near the Open region's edge", () => {
  it("scrolls the Open region toward the pointer, one step a frame, and stops when the drag ends", () => {
    // Arrange: the Open section's rows region is the box that scrolls.
    const frames = frameQueue();
    vi.stubGlobal("requestAnimationFrame", (fn: () => void) => frames.frame(fn));
    const { sc } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    const scroller = pane;
    Object.defineProperty(scroller, "getBoundingClientRect", {
      value: () => ({ top: 0, bottom: 200, height: 200, left: 0, right: 200, width: 200, x: 0, y: 0, toJSON: () => ({}) }),
    });
    scroller.scrollTop = 0;
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 100);

    // Act: hold the drag in the bottom edge band, and run one frame.
    pointer("pointerdown", rowLine(pane, "a"), 105);
    pointer("pointermove", window, 195);
    const scrolledBy = (): number => scroller.scrollTop;
    frames.run();
    const afterOne = scrolledBy();
    pointer("pointerup", window, 195);
    frames.drain();

    // Assert: jsdom keeps scrollTop as written; one frame moved one step, and
    // the frame after the release moved nothing.
    expect([afterOne, scrolledBy()]).toEqual([8, 8]);
  });
});

describe("dragging a subsection or a repository", () => {
  it("moves a subsection before the one it was dropped above", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-a" }, { id: "sub-b" }] })] }), sc);
    layOut(pane.querySelector(".sb-subsections") as HTMLElement, 0);

    await drag(pane.querySelector("[data-subsection='sub-b'] > .sb-subsection-head") as Element, 30, 2);

    const change = sent[0]?.change;
    expect(
      change?.case === "moveSubsection" ? [change.value.subsection?.id, change.value.to.case === "before" ? change.value.to.value.id : null] : null,
    ).toEqual(["sub-b", "sub-a"]);
  });

  it("moves a repository after every other when dropped below them", async () => {
    const { sc, sent } = arranged();
    const pane = mount(roster({ repos: [repoSection({ id: "repo-1" }), repoSection({ id: "repo-2" })] }), sc);
    layOut(pane, 0);

    await drag(pane.querySelector(".repo-section > .repo-head") as Element, 5, 39);

    const change = sent[0]?.change;
    expect(change?.case === "moveRepository" ? [change.value.repository?.id, change.value.to.case] : null).toEqual(["repo-1", "end"]);
  });
});

describe("what is not a grip", () => {
  it("leaves a nested row to move with its parent", () => {
    // Arrange
    const { sc } = arranged();
    const pane = mount(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a", children: [row({ id: "kid" })] }), row({ id: "b" })] })] }),
      sc,
    );
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    // Act
    pointer("pointerdown", rowLine(pane, "kid"), 25);
    pointer("pointermove", window, 60);

    // Assert
    expect([sc.arrange.hold.holding(), sc.arrange.layer.querySelector(`.${DRAG_GHOST_CLASS}`)]).toEqual([false, null]);
    pointer("pointerup", window, 60);
  });

  it("leaves a hidden workspace's row alone", () => {
    // Arrange
    const { sc } = arranged();
    const drawn = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1" })], hidden: hiddenSection([row({ id: "h" })]) }), sc);
    document.body.replaceChildren(drawn, sc.arrange.layer);

    // Act
    pointer("pointerdown", drawn.querySelector("[data-roster-row='h'] > .row") as Element, 10);
    pointer("pointermove", window, 40);

    // Assert
    expect(sc.arrange.hold.holding()).toBe(false);
    pointer("pointerup", window, 40);
  });
});

describe("a drag across pushes", () => {
  it("drags a row a later push redrew in place", async () => {
    // Arrange: the second push changes row b alone, which is patched in place.
    const { sc, sent } = arranged();
    mount(roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] }), sc);
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b", current: true })] })] }),
      sc,
    );
    const pane = drawn.querySelector(".sb-sections") as HTMLElement;
    layOut(pane.querySelector(".repo-section > .rows") as HTMLElement, 0);

    // Act
    await drag(rowLine(pane, "b"), 30, 2);

    // Assert
    const change = sent[0]?.change;
    expect(change?.case === "placeWorkspace" ? change.value.workspace?.id : null).toBe("b");
  });

  it("redraws a section a drag rearranged, though the next push leaves it as it was", () => {
    // Arrange: a drag that lifts the row and is cancelled marks its section.
    const { sc } = arranged();
    const made = roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] })] });
    const pane = mount(made, sc);
    const section = pane.querySelector(".repo-section") as HTMLElement;
    layOut(section.querySelector(":scope > .rows") as HTMLElement, 0);
    pointer("pointerdown", rowLine(pane, "a"), 5);
    pointer("pointermove", window, 35);
    window.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    const marked = section.hasAttribute(REARRANGED_ATTRIBUTE);

    // Act
    const drawn = drawWorkspaceRoster(made, sc);

    // Assert
    const redrawn = drawn.querySelector(".repo-section") as HTMLElement;
    expect([marked, redrawn === section, redrawn.hasAttribute(REARRANGED_ATTRIBUTE)]).toEqual([true, false, false]);
  });

  it("redraws a subsection's changed row alone, keeping its section", () => {
    // Arrange
    const sc = sidebarContext();
    const first = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "sub-1", rows: [row({ id: "b" })] }] })] }),
      sc,
    );
    const section = first.querySelector(".repo-section");
    const a = first.querySelector("[data-roster-row='a']");

    // Act
    const drawn = drawWorkspaceRoster(
      roster({
        repos: [repoSection({ id: "repo-1", rows: [row({ id: "a" })], subsections: [{ id: "sub-1", rows: [row({ id: "b", current: true })] }] })],
      }),
      sc,
    );

    // Assert
    expect([
      drawn.querySelector(".repo-section") === section,
      drawn.querySelector("[data-roster-row='a']") === a,
      drawn.querySelector(".sb-subsection [data-roster-row='b']")?.hasAttribute("data-current"),
    ]).toEqual([true, true, true]);
    disposeWorkspaceRoster(sc);
  });

  it("redraws a section whose subsections changed", () => {
    // Arrange
    const sc = sidebarContext();
    const first = drawWorkspaceRoster(roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }] })] }), sc);
    const section = first.querySelector(".repo-section");

    // Act
    const drawn = drawWorkspaceRoster(
      roster({ repos: [repoSection({ id: "repo-1", subsections: [{ id: "sub-1" }, { id: "sub-2" }] })] }),
      sc,
    );

    // Assert
    expect([drawn.querySelector(".repo-section") === section, drawn.querySelectorAll(".sb-subsection").length]).toEqual([false, 2]);
    disposeWorkspaceRoster(sc);
  });
});

describe("a drag stays inside its repository", () => {
  it("files a row dropped over another repository's rows into its own repository", async () => {
    // Arrange: repo-2's rows sit right below repo-1's, under the drop point.
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [
          repoSection({ id: "repo-1", rows: [row({ id: "a" }), row({ id: "b" })] }),
          repoSection({ id: "repo-2", rows: [row({ id: "x" }), row({ id: "y" })] }),
        ],
      }),
      sc,
    );
    const [own, other] = queryAll<HTMLElement>(pane, ".repo-section > .rows");
    layOut(own, 0);
    layOut(other, 100);

    // Act: drag a far below its own list, over repo-2's rows.
    await drag(rowLine(pane, "a"), 5, 130);

    // Assert: the row never entered repo-2's list, and the placement names
    // a's own repository's list.
    const change = sent[0]?.change;
    expect([
      [...other.children].map((el) => el.getAttribute("data-roster-row")),
      change?.case === "placeWorkspace" ? [change.value.into.case, change.value.at.case] : null,
    ]).toEqual([["x", "y"], ["noSubsection", "end"]]);
  });

  it("keeps a subsection among its own repository's subsections", async () => {
    // Arrange
    const { sc, sent } = arranged();
    const pane = mount(
      roster({
        repos: [
          repoSection({ id: "repo-1", subsections: [{ id: "sub-a" }, { id: "sub-b" }] }),
          repoSection({ id: "repo-2", subsections: [{ id: "sub-x" }] }),
        ],
      }),
      sc,
    );
    const [own, other] = queryAll<HTMLElement>(pane, ".sb-subsections");
    layOut(own, 0);
    layOut(other, 100);

    // Act: drag sub-a over repo-2's subsections.
    await drag(pane.querySelector("[data-subsection='sub-a'] > .sb-subsection-head") as Element, 5, 110);

    // Assert
    const change = sent[0]?.change;
    expect([
      queryAll(other, ".sb-subsection").map((el) => el.getAttribute("data-subsection")),
      change?.case === "moveSubsection" ? [change.value.subsection?.id, change.value.to.case] : null,
    ]).toEqual([["sub-x"], ["sub-a", "end"]]);
  });
});
