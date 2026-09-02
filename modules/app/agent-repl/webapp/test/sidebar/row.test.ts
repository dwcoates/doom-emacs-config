// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { SelectWorkspaceResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import { RosterRowSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { MalformedView } from "../../src/rpc/malformed.js";
import { drawRosterRow, drawStatusMark } from "../../src/sidebar/row.js";
import { ROSTER_ARM_CLASS, ROSTER_STATUS_CASES } from "../../src/sidebar/tones.js";
import {
  NOW,
  appContext,
  fakeTicker,
  memoryPrefs,
  row,
  sidebarContext,
} from "./harness.js";

const SELECT_OK = create(SelectWorkspaceResponseSchema, {
  result: { case: "success", value: {} },
});

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

  it("draws a merge as the recycle glyph rather than a lifecycle dot", () => {
    expect(drawStatusMark("merging", "R.status").getAttribute("data-glyph")).toBe("recycle");
  });

  it("draws a question mark for a perspective-less workspace", () => {
    expect(drawStatusMark("inactive", "R.status").textContent).toBe("?");
  });

  it("draws no character for a workspace with no session", () => {
    expect(drawStatusMark("none", "R.status").textContent).toBe("");
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

  it("states the arm the daemon chose for a last selection", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "lastSelected", value: { atMs: BigInt(NOW - 180_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.getAttribute("data-when")).toBe("lastSelected");
  });

  it("draws a last selection as a relative age", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "lastSelected", value: { atMs: BigInt(NOW - 180_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("3m");
  });

  it("reads the nearest second when a tick samples just short of one", () => {
    // Arrange + Act: the stamp does not share the shared ticker's phase.
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "lastSelected", value: { atMs: BigInt(NOW - 4920) } } }),
      sidebarContext(),
      "R",
    );
    // Assert: five real seconds ago reads 5s, not the lagging 4s.
    expect(drawn.querySelector(".when")?.textContent).toBe("5s");
  });

  it("names a settled merge in its own words", () => {
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "merged", value: { atMs: BigInt(NOW - 3_600_000) } } }),
      sidebarContext(),
      "R",
    );
    expect(drawn.querySelector(".when")?.textContent).toBe("merged 1h");
  });

  it("ticks the age off the SHARED clock", () => {
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    const drawn = drawRosterRow(
      row({ id: "ws-1", when: { case: "lastSelected", value: { atMs: BigInt(NOW - 60_000) } } }),
      sc,
      "R",
    );
    ticker.tick(NOW + 120_000);
    expect(drawn.querySelector(".when")?.textContent).toBe("3m");
  });

  it("registers its tick as a teardown the next push runs", () => {
    const ticker = fakeTicker();
    const sc = sidebarContext(appContext({}, ticker));
    drawRosterRow(
      row({ id: "ws-1", when: { case: "lastSelected", value: { atMs: BigInt(NOW) } } }),
      sc,
      "R",
    );
    expect(sc.disposers.length).toBe(1);
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

  it("opens on the chevron", async () => {
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(), "R");
    await click(drawn.querySelector(".chev") as Element);
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("remembers the expansion, keyed by the workspace id", async () => {
    const prefs = memoryPrefs();
    const sc = sidebarContext(appContext(), prefs);
    const drawn = drawRosterRow(row({ id: "ws-1" }), sc, "R");
    await click(drawn.querySelector(".chev") as Element);
    expect(prefs.state.expanded["ws-1"]).toBe(true);
  });

  it("draws a remembered expansion open on the next push", () => {
    const prefs = memoryPrefs({ expanded: { "ws-1": true } });
    const drawn = drawRosterRow(row({ id: "ws-1" }), sidebarContext(appContext(), prefs), "R");
    expect(drawn.classList.contains("open")).toBe(true);
  });

  it("does not select the workspace when the chevron is clicked", async () => {
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
    await click(drawn.querySelector(".chev") as Element);
    expect(calls).toBe(0);
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
