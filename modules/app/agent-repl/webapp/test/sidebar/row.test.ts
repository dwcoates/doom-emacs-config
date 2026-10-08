// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { UpdateSidebarViewResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_update_sidebar_view_pb";
import { RosterRowSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import {
  HOVER_CLOSE_GRACE_MS,
  HOVER_OPEN_DELAY_MS,
  drawRosterRow,
  drawRosterRowWhen,
  drawStatusMark,
  expandVisibleRows,
  placeOpenRowDetails,
  placeRowDetail,
  toggleRowMenu,
} from "../../src/sidebar/row.js";
import { REVEAL_MARGIN_PX } from "../../src/topbar/clamp.js";
import { ROSTER_ARM_CLASS, ROSTER_STATUS_CASES } from "../../src/sidebar/tones.js";
import { WorkspaceRefSchema } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import {
  NOW,
  appContext,
  fakeTicker,
  fakeTimers,
  row,
  sidebarContext,
} from "./harness.js";
import { createSidebarView } from "../../src/sidebar/view.js";

const SELECT_OK = create(SelectWorkspaceResponseSchema, {
  result: { case: "success", value: {} },
});

/** The row line: what the pointer and the keyboard focus are read from. */
const lineOf = (drawn: HTMLElement): HTMLElement =>
  drawn.querySelector(":scope > .row") as HTMLElement;

/** This row's own detail panel. */
const panelOf = (drawn: HTMLElement): HTMLElement =>
  drawn.querySelector(":scope > .detail") as HTMLElement;

/** The status dot: the panel's one pointer trigger (owner request, 2026-10-02). */
const dotOf = (drawn: HTMLElement): HTMLElement =>
  lineOf(drawn).querySelector(".st") as HTMLElement;

function hoverIn(drawn: HTMLElement): void {
  dotOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
}

function hoverOut(drawn: HTMLElement): void {
  dotOf(drawn).dispatchEvent(new MouseEvent("mouseleave"));
}

async function click(control: Element): Promise<void> {
  (control as HTMLElement).dispatchEvent(new MouseEvent("click", { bubbles: true }));
  await new Promise((resolve) => globalThis.setTimeout(resolve, 0));
}

describe("the row's identity and hooks", () => {
  it("is addressed by the workspace id the daemon minted", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.getAttribute("data-roster-row")).toBe("ws-1");
  });

  it("states its status arm", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "thinking", value: {} } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.getAttribute("data-arm")).toBe("thinking");
  });

  it("draws the display name", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", name: "the rail" }), sidebarContext(), "R");
    expect(drawn.querySelector(".name")?.textContent).toBe("the rail");
  });

  it("offers exactly one SelectWorkspace click target", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelectorAll(":scope > [data-select]").length).toBe(1);
  });
});

describe("the status mark", () => {
  it.each([...ROSTER_STATUS_CASES])("paints %s the shared vocabulary's color", (arm) => {
    const mark = drawStatusMark(arm, "R.status");
    expect(mark.classList.contains(ROSTER_ARM_CLASS[arm])).toBe(true);
  });

  it.each([...ROSTER_STATUS_CASES])("names %s's glyph", (arm) => {
    expect(drawStatusMark(arm, "R.status").hasAttribute("data-glyph")).toBe(true);
  });

  it("draws a failed turn end as a turquoise dot", () => {
    const mark = drawStatusMark("turnFailed", "R.status");
    expect(mark.classList.contains("tone-turquoise")).toBe(true);
    expect(mark.getAttribute("data-glyph")).toBe("dot");
  });

  it("draws a merge as the recycle glyph rather than a lifecycle dot", () => {
    expect(drawStatusMark("merging", "R.status").getAttribute("data-glyph")).toBe("recycle");
  });

  it("draws a question mark for a perspective-less workspace", () => {
    expect(drawStatusMark("inactive", "R.status").textContent).toBe("?");
  });

  it("draws no character for a workspace with no session", () => {
    expect(drawStatusMark("none", "R.status").textContent).toBe("");
  });

  it("draws no check for a landed merge", () => {
    expect(drawStatusMark("merged", "R.status").textContent).toBe("");
  });

  it("draws no disc for a landed merge either: its box is a glyph box", () => {
    expect(drawStatusMark("merged", "R.status").classList.contains("st-glyph")).toBe(true);
  });

  it("breathes while a turn is in flight", () => {
    expect(drawStatusMark("thinking", "R.status").classList.contains("breathes")).toBe(true);
  });

  it("spins while the merge queue is on this run", () => {
    expect(drawStatusMark("merging", "R.status").classList.contains("spins")).toBe(true);
  });
});

describe("the highlight and the receded styling", () => {
  it("highlights the row the RESOLVER called current", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", current: true }), sidebarContext(), "R");
    expect(drawn.getAttribute("data-current")).toBe("true");
  });

  it("does not highlight a row the resolver left uncurrent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", current: false }), sidebarContext(), "R");
    expect(drawn.hasAttribute("data-current")).toBe(false);
  });

  it("recedes a closed workspace", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", closed: true }), sidebarContext(), "R");
    expect(drawn.getAttribute("data-closed")).toBe("true");
  });

  it("leaves an open workspace unreceded", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", closed: false }), sidebarContext(), "R");
    expect(drawn.hasAttribute("data-closed")).toBe(false);
  });
});

describe("the attention marker", () => {
  it("marks the row when the daemon set it", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", attention: true }), sidebarContext(), "R");
    expect(drawn.hasAttribute("data-attention")).toBe(true);
  });

  it("draws no marker when it is absent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".sb-attn")).toBeNull();
  });

  it("starts the blink lit, on the cadence the message specifies", () => {
    const sc = sidebarContext();
    const drawn = drawRosterRow(row({ id: "ws-1", attention: true }), sc, "R");
    // The MARKER blinks, not the row: `data-blink` is painted on the element
    // the hook contract points at, `[data-roster-row] [data-attention]`.
    expect(drawn.querySelector(".sb-attn")?.getAttribute("data-blink")).toBe("on");
  });
});

describe("the priority badge", () => {
  it("draws the resolver's composed label verbatim", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", priority: "P0.5" }), sidebarContext(), "R");
    expect(drawn.querySelector(".sb-prio")?.textContent).toBe("P0.5");
  });

  it("addresses the badge by that label", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", priority: "P1" }), sidebarContext(), "R");
    expect(drawn.querySelector("[data-priority='P1']")).not.toBeNull();
  });

  it("draws no badge for an unprioritized workspace", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".sb-prio")).toBeNull();
  });
});

describe("the when-column", () => {
  it("is empty when the daemon has nothing to show", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".when")?.textContent).toBe("");
  });

  it("reads the nearest second when an activity age is sampled just short of one", () => {
    // Arrange + Act: the stamp does not share the shared ticker's phase.
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "active", value: { atMs: BigInt(NOW - 4920) } } }),
      sidebarContext(),
      "R",
    );
    // Assert: five real seconds ago reads 5s, not the lagging 4s.
    expect(drawn.querySelector(".when")?.textContent).toBe("5s");
  });

  it("ticks an activity age off the SHARED clock", () => {
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "active", value: { atMs: BigInt(NOW - 60_000) } } }),
      sc,
      "R",
    );
    ticker.tick(NOW + 120_000);
    expect(drawn.querySelector(".when")?.textContent).toBe("3m");
  });

  it("registers an activity age's tick as a teardown the next push runs", () => {
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    drawRosterRow(
      row({ id: "ws-1", when: { case: "active", value: { atMs: BigInt(NOW) } } }),
      sc,
      "R",
    );
    expect(sc.disposers.length).toBe(1);
  });

  it("states the arm the daemon chose for last activity", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "active", value: { atMs: BigInt(NOW - 180_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.getAttribute("data-when")).toBe("active");
  });

  it("draws last activity as a relative age", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "active", value: { atMs: BigInt(NOW - 180_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("3m");
  });

  it("draws a never-active workspace's creation time as a bare relative age", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "created", value: { atMs: BigInt(NOW - 3_600_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("1h");
  });

  it("names a settled merge in its own words", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "merged", value: { atMs: BigInt(NOW - 3_600_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("merged 1h");
  });

});

describe("the detail panel", () => {
  it("draws the branch when the line is present", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail")?.textContent).toContain("feat/rail");
  });

  it("draws the parent branch when the line is present", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { parentBranch: { name: "master" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail")?.textContent).toContain("master");
  });

  it("draws the summary when the line is present", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { summary: { text: "porting the rail" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail .summary")?.textContent).toBe("porting the rail");
  });

  it("omits an absent line rather than drawing it blank", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".detail .summary")).toBeNull();
  });

  it("draws no list at all when every line is absent", () => {
    const drawn = drawRosterRow(row({ id: "ws-1", detail: {} }), sidebarContext(), "R");
    expect(drawn.querySelector(".detail dl")).toBeNull();
  });

  it("is closed until the row is expanded", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("opens on hover, once the intent delay has passed", () => {
    // ARRANGE
    vi.useFakeTimers();
    try {
      const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
      // ACT
      hoverIn(drawn);
      vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
      // ASSERT
      expect(drawn.classList.contains("open")).toBe(true);
    } finally {
      vi.useRealTimers();
    }
  });

  it("remembers the expansion on this page, keyed by the workspace id", () => {
    // ARRANGE
    vi.useFakeTimers();
    try {
      const sc = sidebarContext();
      const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
      // ACT
      hoverIn(drawn);
      vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
      // ASSERT
      expect([...sc.openDetails]).toEqual(["ws-1"]);
    } finally {
      vi.useRealTimers();
    }
  });

  it("draws an expansion this page remembers open on the next push", () => {
    const sc = sidebarContext(appContext(), fakeTimers(), createSidebarView(), new Set(["ws-1"]));
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("never asks the daemon about a detail, which is this page's own", () => {
    // ARRANGE
    vi.useFakeTimers();
    const asked: string[] = [];
    try {
      const sc = sidebarContext(
        appContext({
          updateSidebarView: (request) => {
            asked.push(request.change.case ?? "unset");
            return create(UpdateSidebarViewResponseSchema, { result: { case: "success", value: {} } });
          },
        }),
      );
      const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
      // ACT
      hoverIn(drawn);
      vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
      // ASSERT
      expect(asked).toEqual([]);
    } finally {
      vi.useRealTimers();
    }
  });

  it("carries no chevron at all", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".chev")).toBeNull();
  });
});

describe("the family", () => {
  it("nests a child under its parent", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", children: [row({ id: "ws-2" })] }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".kids [data-roster-row='ws-2']")).not.toBeNull();
  });

  it("nests a grandchild one generation further", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", children: [row({ id: "ws-2", children: [row({ id: "ws-3" })] })] }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".kids .kids [data-roster-row='ws-3']")).not.toBeNull();
  });

  it("draws no family box for a childless row", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.querySelector(".kids")).toBeNull();
  });

  it("does not draw a closed child", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", children: [row({ id: "ws-2", closed: true })] }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector("[data-roster-row='ws-2']")).toBeNull();
  });

  it("hoists a live grandchild up in a closed child's place", () => {
    const drawn = drawRosterRow(
      row({
        id: "ws-1",
        children: [row({ id: "ws-2", closed: true, children: [row({ id: "ws-3" })] })],
      }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".kids [data-roster-row='ws-3']")).not.toBeNull();
  });

  it("draws no family box when every child is closed and leaves no descendant", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", children: [row({ id: "ws-2", closed: true })] }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".kids")).toBeNull();
  });
});

describe("expandVisibleRows", () => {
  it("keeps an open row", () => {
    const visible = expandVisibleRows([row({ id: "ws-1", closed: false })], "R");
    expect(visible.map((v) => v.row.workspace?.workspace?.id)).toEqual(["ws-1"]);
  });

  it("drops a closed row", () => {
    const visible = expandVisibleRows([row({ id: "ws-1", closed: true })], "R");
    expect(visible).toEqual([]);
  });

  it("hoists a closed row's live child into its place", () => {
    const visible = expandVisibleRows(
      [row({ id: "ws-1", closed: true, children: [row({ id: "ws-2" })] })],
      "R",
    );
    expect(visible.map((v) => v.row.workspace?.workspace?.id)).toEqual(["ws-2"]);
  });

  it("gives a hoisted row the message path it was found at", () => {
    const visible = expandVisibleRows(
      [row({ id: "ws-1", closed: true, children: [row({ id: "ws-2" })] })],
      "R",
    );
    expect(visible[0]?.path).toBe("R[0].children[0]");
  });
});

describe("the row click", () => {
  it("calls SelectWorkspace and nothing else", async () => {
    const calls: string[] = [];
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          calls.push(request.workspace?.id ?? "");
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(calls).toEqual(["ws-1"]);
  });

  it("echoes the served ref rather than rebuilding one", async () => {
    let dir = "";
    const sc = sidebarContext(
      appContext({
        selectWorkspace: (request) => {
          dir = request.workspace?.dir ?? "";
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1", dir: "/w/one" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(dir).toBe("/w/one");
  });

  it("says a refusal at the row itself", async () => {
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () =>
          create(SelectWorkspaceResponseSchema, {
            result: {
              case: "error",
              value: { cause: { case: "unknownWorkspace", value: {} } },
            },
          }),
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(drawn.querySelector(":scope > .sb-refusal")?.textContent).toBe(
      "the daemon does not know this workspace",
    );
  });

  it("labels the row's refusal with the cause's own arm", async () => {
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () =>
          create(SelectWorkspaceResponseSchema, {
            result: {
              case: "error",
              value: { cause: { case: "notYetAdopted", value: {} } },
            },
          }),
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector("[data-select]") as Element);
    expect(drawn.querySelector(":scope > .sb-refusal")?.getAttribute("data-arm")).toBe(
      "notYetAdopted",
    );
  });
});

describe("the verb menu", () => {
  it("is drawn with the row, hidden until it is asked for", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(true);
  });

  it("opens on the row's own control", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".sb-more") as Element);
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(false);
  });

  it("opens DOWNWARD, under the row line, so it cannot clip off the top", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".sb-more") as Element);
    expect(drawn.querySelector(".row")?.nextElementSibling?.classList.contains("sb-menu")).toBe(
      true,
    );
  });

  it("closes on a second click rather than stacking a second menu", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".sb-more") as Element);
    await click(drawn.querySelector(".sb-more") as Element);
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(true);
    expect(drawn.querySelectorAll(".sb-menu")).toHaveLength(1);
  });

  it("opens on a right-click of the row", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    drawn
      .querySelector(".row")
      ?.dispatchEvent(new MouseEvent("contextmenu", { bubbles: true }));
    expect((drawn.querySelector(".sb-menu") as HTMLElement).hidden).toBe(false);
  });

  it("does not select the workspace when the control is clicked", async () => {
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () => {
          calls += 1;
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector(".sb-more") as Element);
    expect(calls).toBe(0);
  });
});

describe("a malformed row", () => {
  it("refuses a row with no lifecycle at all", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no workspace", () => {
    const malformed = create(RosterRowSchema, {
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no name", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row that does not state its highlight", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row that does not state its receded styling", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      when: {},
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no when-column box", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      detail: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a row with no detail box", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-1", dir: "/w" } },
      name: { text: "one" },
      status: { case: "ready", value: {} },
      current: { current: false },
      closed: { closed: false },
      when: {},
    });
    expect(() => drawRosterRow(malformed, sidebarContext(), "R")).toThrow(MalformedView);
  });

  it("refuses a child that is itself malformed", () => {
    const malformed = create(RosterRowSchema, {
      workspace: { workspace: { id: "ws-2", dir: "/w" } },
      name: { text: "two" },
      current: { current: false },
      closed: { closed: false },
      when: {},
      detail: {},
    });
    const parent = row({ id: "ws-1", children: [malformed] });
    expect(() => drawRosterRow(parent, sidebarContext(), "R")).toThrow(MalformedView);
  });
});

describe("a when-column arm this build has never heard of", () => {
  it("is a malformed view, never a blank column", () => {
    expect(() =>
      drawRosterRowWhen(
        { shown: { case: "aFutureArm", value: {} } } as never,
        sidebarContext(),
        "R.when",
      ),
    ).toThrow(new MalformedView("R.when.shown", "arm 'aFutureArm' is not one this build can draw"));
  });
});

describe("the row menu toggle", () => {
  const TARGET = {
    sc: sidebarContext(),
    workspace: create(WorkspaceRefSchema, { id: "ws-1", dir: "/w/one" }),
    name: "one",
  };

  it("reveals the menu the row owns", () => {
    const ws = document.createElement("div");
    const menu = document.createElement("div");
    menu.className = "sb-menu";
    menu.hidden = true;
    ws.appendChild(menu);
    toggleRowMenu(ws, TARGET);
    expect(menu.hidden).toBe(false);
  });

  it("leaves a nested row's menu alone, because the menu must be this row's own", () => {
    const ws = document.createElement("div");
    const child = document.createElement("div");
    const menu = document.createElement("div");
    menu.className = "sb-menu";
    menu.hidden = true;
    child.appendChild(menu);
    ws.appendChild(child);
    toggleRowMenu(ws, TARGET);
    expect(menu.hidden).toBe(true);
  });
});

describe("the detail panel opens on hover", () => {
  /**
   * Owner ruling, 2026-09-14: there is no chevron; hovering the row does what
   * opening the chevron did. NO REAL TIMERS — the two delays are advanced.
   */
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("opens nothing on a resting row", () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("does not open when the pointer rests on the row's name", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    (lineOf(drawn).querySelector(".name") as HTMLElement).dispatchEvent(new MouseEvent("mouseenter"));
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS * 10);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("does not open when the pointer rests on the row line itself", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    lineOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS * 10);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("opens once the pointer has rested on the status dot for the intent delay", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    hoverIn(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("does not open on a pass-through shorter than the intent delay", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT: the pointer crosses the row on its way somewhere else.
    hoverIn(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS - 1);
    hoverOut(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS + HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("closes once the pointer has left the row and the grace has run out", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
    // ACT
    hoverOut(drawn);
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("is still open inside the grace, so the pointer can travel to the panel", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
    // ACT
    hoverOut(drawn);
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS - 1);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("stays open while the pointer is inside the panel", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
    hoverOut(drawn);
    // ACT: the pointer arrives in the panel before the grace runs out.
    panelOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS * 10);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("closes when the pointer leaves the panel too", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    hoverIn(drawn);
    vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
    hoverOut(drawn);
    panelOf(drawn).dispatchEvent(new MouseEvent("mouseenter"));
    // ACT
    panelOf(drawn).dispatchEvent(new MouseEvent("mouseleave"));
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("opens at once when the keyboard focus reaches the row", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    // ACT
    lineOf(drawn).dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("closes again when the focus leaves the row entirely", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    lineOf(drawn).dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ACT
    lineOf(drawn).dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: document.body }),
    );
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(false);
  });

  it("keeps the panel open while the focus moves BETWEEN the row's own controls", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    const line = lineOf(drawn);
    line.dispatchEvent(new FocusEvent("focusin", { bubbles: true }));
    // ACT
    line.dispatchEvent(
      new FocusEvent("focusout", { bubbles: true, relatedTarget: line.querySelector(".sb-more") }),
    );
    vi.advanceTimersByTime(HOVER_CLOSE_GRACE_MS);
    // ASSERT
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("still selects the workspace when the row itself is clicked", async () => {
    // ARRANGE
    vi.useRealTimers();
    let calls = 0;
    const sc = sidebarContext(
      appContext({
        selectWorkspace: () => {
          calls += 1;
          return SELECT_OK;
        },
      }),
    );
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    // ACT
    await click(lineOf(drawn));
    // ASSERT
    expect(calls).toBe(1);
  });
});

describe("the detail panel leaves the rail and stays inside the window", () => {
  // jsdom reports every rect as zero, so the rects this placement reads are
  // staged on the prototype and the window's size is stubbed — the same
  // arrangement the topbar reveal suite uses for the same reason.
  const rects = new Map<Element, DOMRect>();
  let original: typeof Element.prototype.getBoundingClientRect;

  const rect = (init: { left: number; top: number; width: number; height: number }): DOMRect =>
    ({
      left: init.left,
      top: init.top,
      right: init.left + init.width,
      bottom: init.top + init.height,
      width: init.width,
      height: init.height,
      x: init.left,
      y: init.top,
      toJSON: () => ({}),
    });

  beforeEach(() => {
    rects.clear();
    // Captured to be ASSIGNED back in afterEach, never called off the reference.
    // eslint-disable-next-line @typescript-eslint/unbound-method -- see above
    original = Element.prototype.getBoundingClientRect;
    Element.prototype.getBoundingClientRect = function staged(this: Element): DOMRect {
      return rects.get(this) ?? rect({ left: 0, top: 0, width: 0, height: 0 });
    };
    vi.stubGlobal("innerWidth", 1000);
    vi.stubGlobal("innerHeight", 800);
  });

  afterEach(() => {
    Element.prototype.getBoundingClientRect = original;
    vi.unstubAllGlobals();
  });

  /** An expanded row whose line and panel have the staged rects given. */
  function expandedRow(line: DOMRect, panel: DOMRect): HTMLElement {
    const drawn = drawRosterRow(
      row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
      sidebarContext(appContext(), fakeTimers(), createSidebarView(), new Set(["ws-1"])),
      "R",
    );
    rects.set(drawn.querySelector(":scope > .row") as Element, line);
    rects.set(drawn.querySelector(":scope > .detail") as Element, panel);
    return drawn;
  }

  it("hangs the panel directly under the row's own line", () => {
    // ARRANGE: a rail-width row at the window's left edge.
    const drawn = expandedRow(
      rect({ left: 8, top: 100, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: the line's own left, and its bottom.
    expect([panelOf(drawn).style.left, panelOf(drawn).style.top]).toEqual(["8px", "124px"]);
  });

  it("slides a panel that would run off the right edge back inside the window", () => {
    // ARRANGE: a row near the right edge, with a panel wider than what is left.
    const drawn = expandedRow(
      rect({ left: 880, top: 100, width: 110, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: held one margin clear of the right edge, never cut off by it.
    expect(panelOf(drawn).style.left).toBe(`${1000 - REVEAL_MARGIN_PX - 320}px`);
  });

  it("caps a panel near the bottom edge at the height the window leaves it", () => {
    // ARRANGE: a row low in a short window, with a tall panel.
    const drawn = expandedRow(
      rect({ left: 8, top: 700, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 400 }),
    );
    // ACT
    placeRowDetail(drawn);
    // ASSERT: it scrolls inside itself rather than running past the bottom.
    expect(panelOf(drawn).style.maxHeight).toBe(`${800 - 724 - REVEAL_MARGIN_PX}px`);
  });

  it("places a row that was drawn already expanded, once it is on the page", () => {
    // ARRANGE
    const drawn = expandedRow(
      rect({ left: 8, top: 40, width: 190, height: 24 }),
      rect({ left: 0, top: 0, width: 320, height: 90 }),
    );
    const host = document.createElement("div");
    host.appendChild(drawn);
    // ACT
    placeOpenRowDetails(host);
    // ASSERT
    expect(panelOf(drawn).style.top).toBe("64px");
  });

  it("leaves a closed row's panel unplaced", () => {
    // ARRANGE
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    const host = document.createElement("div");
    host.appendChild(drawn);
    // ACT
    placeOpenRowDetails(host);
    // ASSERT
    expect(panelOf(drawn).style.top).toBe("");
  });

  it("places the panel the moment the hover opens it", () => {
    // ARRANGE: a resting row, expanded by the hover rather than by the draw.
    vi.useFakeTimers();
    try {
      const drawn = drawRosterRow(
        row({ id: "ws-1", detail: { branch: { name: "feat/rail" } } }),
        sidebarContext(),
        "R",
      );
      rects.set(
        drawn.querySelector(":scope > .row") as Element,
        rect({ left: 8, top: 200, width: 190, height: 24 }),
      );
      rects.set(
        drawn.querySelector(":scope > .detail") as Element,
        rect({ left: 0, top: 0, width: 320, height: 90 }),
      );
      // ACT
      hoverIn(drawn);
      vi.advanceTimersByTime(HOVER_OPEN_DELAY_MS);
      // ASSERT
      expect(panelOf(drawn).style.top).toBe("224px");
    } finally {
      vi.useRealTimers();
    }
  });
});

describe("the row's display mode", () => {
  it("greys the NAME when the daemon carries the viewed marker", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", viewed: true }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(true);
  });

  it("leaves the NAME alone when it carries none", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(false);
  });

  it("marks the whole row PARTIAL for the hook contract", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", viewed: true }), sidebarContext(), "R");

    // Assert.
    expect(drawn.getAttribute("data-viewed")).toBe("true");
  });

  it("leaves the status dot's tone untouched in partial mode", () => {
    // Arrange: the mode says what the user has SEEN, never what the workspace
    // is doing — the dot is what says that.
    const sc = sidebarContext();

    // Act.
    const full = drawRosterRow(row({ id: "ws-1", status: { case: "thinking", value: {} } }), sc, "R");
    const partial = drawRosterRow(
      row({ id: "ws-2", status: { case: "thinking", value: {} }, viewed: true }),
      sc,
      "R",
    );

    // Assert.
    const dot = (el: HTMLElement): string =>
      (el.querySelector(":scope > .row > .st") as HTMLElement).className;
    expect(dot(partial)).toBe(dot(full));
  });

  it("draws PARTIAL on the push that changes the status when the marker is set", () => {
    // Arrange: the row was last drawn FULL at idle_async.
    const sc = sidebarContext();
    drawRosterRow(row({ id: "ws-1", status: { case: "idleAsync", value: {} } }), sc, "R");

    // Act: the detached work ended and the daemon says the result is read.
    const drawn = drawRosterRow(
      row({ id: "ws-1", status: { case: "done", value: {} }, viewed: true }),
      sc,
      "R",
    );

    // Assert: the daemon resolves the marker with the status, so the page
    // draws what the wire says.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(true);
  });
});

describe("the reviving shimmer", () => {
  it("shimmers the NAME while the daemon carries the reviving marker", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", reviving: true }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("reviving")).toBe(true);
  });

  it("leaves the NAME still once the marker is gone", () => {
    // Arrange, Act: the daemon lowered the marker when the revival ended.
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Assert.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("reviving")).toBe(false);
  });

  it("marks the whole row reviving for the hook contract", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(row({ id: "ws-1", reviving: true }), sidebarContext(), "R");

    // Assert.
    expect(drawn.getAttribute("data-reviving")).toBe("true");
  });

  it("leaves the status dot's tone untouched while reviving", () => {
    // Arrange: the marker is not a status arm.
    const sc = sidebarContext();

    // Act.
    const still = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    const reviving = drawRosterRow(row({ id: "ws-2", reviving: true }), sc, "R");

    // Assert.
    const dot = (el: HTMLElement): string =>
      (el.querySelector(":scope > .row > .st") as HTMLElement).className;
    expect(dot(reviving)).toBe(dot(still));
  });

  it("keeps the viewed mode alongside the shimmer", () => {
    // Arrange, Act.
    const drawn = drawRosterRow(
      row({ id: "ws-1", viewed: true, reviving: true }),
      sidebarContext(),
      "R",
    );

    // Assert: the two markers are independent.
    const name = drawn.querySelector(":scope > .row > .name") as HTMLElement;
    expect(name.classList.contains("viewed")).toBe(true);
  });
});

describe("the durable last-selected instant", () => {
  it("draws a row carrying it exactly as the same row without it", () => {
    // Arrange: the instant is an ordering fact for clients, never drawn.
    const plain = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");

    // Act.
    const stamped = drawRosterRow(
      row({ id: "ws-1", lastSelectedAtMs: 1756400000000n }),
      sidebarContext(),
      "R",
    );

    // Assert.
    expect(stamped.outerHTML).toBe(plain.outerHTML);
  });
});
