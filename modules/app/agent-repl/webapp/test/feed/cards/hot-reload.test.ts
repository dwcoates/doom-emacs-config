// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create, type MessageInitShape } from "@bufbuild/protobuf";
import {
  FeedHotReloadSchema,
  FeedHotReloadStateSchema,
  type FeedHotReload,
} from "../../../../proto/gen/ts/frontend/v1/feed_pb";
import { MalformedView } from "../../../src/rpc/malformed.js";
import {
  HOT_RELOAD_WORDS,
  SELECTED_SERVICE_ATTRIBUTE,
  buildsText,
  drawFeedHotReload,
  drawRow,
} from "../../../src/feed/cards/hot-reload.js";
import { mergeBadge } from "../../../src/feed/merge/merge.js";
import { harness, mergeRow, rowContext } from "../harness.js";
import { oneofArms } from "../../arms.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1_000_000);
});
afterEach(() => {
  vi.useRealTimers();
});

type StateInit = MessageInitShape<typeof FeedHotReloadStateSchema>;

const running = (words: string, startedAtMs = 940_000n): StateInit => ({
  state: { case: "running", value: { words: { text: words }, startedAtMs } },
});

/** A live card with the protobufs step, a store and a shim replacing two workspaces. */
function card(): FeedHotReload {
  return create(FeedHotReloadSchema, {
    head: { label: { text: "hot-reloading" } },
    state: { case: "live", value: { startedAtMs: 900_000n } },
    steps: [{ name: { text: "protobufs" }, state: { state: { case: "done", value: { words: { text: "generated" }, startedAtMs: 900_000n, endedAtMs: 903_000n } } } }],
    services: [
      {
        name: { text: "store" },
        state: { state: { case: "unchanged", value: { words: { text: "already running the fresh build" } } } },
        about: { text: "Keeps every conversation's records." },
        steps: [],
        instances: [],
      },
      {
        name: { text: "shim" },
        state: running("replacing"),
        summary: { text: "1 of 2 replaced" },
        about: { text: "Hosts each workspace's Claude session." },
        builds: { previous: { shortHash: "4f2a1c9" }, fresh: { shortHash: "9be5151" } },
        steps: [
          { name: { text: "build" }, state: { state: { case: "done", value: { words: { text: "built" }, startedAtMs: 900_000n, endedAtMs: 909_000n } } } },
          { name: { text: "replace" }, state: running("replacing") },
        ],
        instances: [
          { name: { text: "fix-footer" }, state: { state: { case: "done", value: { words: { text: "replaced" }, startedAtMs: 950_000n, endedAtMs: 952_000n } } } },
          { name: { text: "deploy-mediator" }, state: { state: { case: "waiting", value: { words: { text: "waits for its turn to end" }, startedAtMs: 940_000n } } } },
        ],
      },
    ],
  });
}

/** Draw MSG, as a redraw of PREVIOUS when given. */
function draw(msg: FeedHotReload, previous?: HTMLElement): HTMLElement {
  const rc = rowContext(harness().ctx, mergeRow("h1"));
  return drawFeedHotReload(msg, previous === undefined ? rc : { ...rc, previous });
}

/** Click the service row NAME in EL. */
function select(el: HTMLElement, name: string): void {
  const row = el.querySelector<HTMLElement>(`.hot-reload-service[data-service="${name}"]`);
  if (row === null) throw new Error(`no service row ${name}`);
  row.click();
}

describe("drawFeedHotReload: the frame", () => {
  it("draws the daemon's label in the merge head, with the reloading badge", () => {
    const el = draw(card());
    expect(el.querySelector(".merge-label")?.textContent).toBe("hot-reloading");
    expect(el.querySelector(".merge-badge")?.outerHTML).toBe(mergeBadge(HOT_RELOAD_WORDS.live, "is-live").outerHTML);
  });

  it("refuses a card whose head is unset", () => {
    const msg = card();
    msg.head = undefined;
    expect(() => draw(msg)).toThrow(MalformedView);
  });
});

describe("drawFeedHotReload: the list view", () => {
  it("draws the shared steps, then one selectable row per service, in served order", () => {
    const el = draw(card());
    expect([...el.querySelectorAll(".hot-reload-list > .hot-reload-row .hot-reload-name")].map((n) => n.textContent)).toEqual([
      "protobufs",
      "store",
      "shim",
    ]);
    expect(el.querySelector(".hot-reload-service")?.getAttribute("role")).toBe("button");
  });

  it("draws a service's words and its summary", () => {
    const el = draw(card());
    expect(el.querySelector('[data-service="shim"] .hot-reload-words')?.textContent).toBe("replacing · 1 of 2 replaced");
  });

  it("draws a service with no summary by its words alone", () => {
    const el = draw(card());
    expect(el.querySelector('[data-service="store"] .hot-reload-words')?.textContent).toBe("already running the fresh build");
  });
});

describe("drawFeedHotReload: a service's detail", () => {
  it("swaps the list for the selected service's detail, recording the selection", () => {
    const el = draw(card());
    select(el, "shim");
    expect(el.querySelector(".hot-reload-list")).toBeNull();
    expect(el.querySelector(".hot-reload-detail-head .hot-reload-name")?.textContent).toBe("shim");
    expect(el.getAttribute(SELECTED_SERVICE_ATTRIBUTE)).toBe("shim");
  });

  it("draws what reloading the service means and its builds", () => {
    const el = draw(card());
    select(el, "shim");
    expect(el.querySelector(".hot-reload-about")?.textContent).toBe("Hosts each workspace's Claude session.");
    expect(el.querySelector(".hot-reload-builds")?.textContent).toBe("build 4f2a1c9 → 9be5151");
  });

  it("draws the service's steps, then its running copies", () => {
    const el = draw(card());
    select(el, "shim");
    expect([...el.querySelectorAll(".hot-reload-steps .hot-reload-name")].map((n) => n.textContent)).toEqual(["build", "replace"]);
    expect([...el.querySelectorAll(".hot-reload-instances .hot-reload-row")].map((r) => r.getAttribute("data-state"))).toEqual([
      "done",
      "waiting",
    ]);
  });

  it("draws no copies section for a service with none", () => {
    const el = draw(card());
    select(el, "store");
    expect(el.querySelector(".hot-reload-instances")).toBeNull();
    expect(el.querySelector(".hot-reload-builds")).toBeNull();
  });

  it("goes back to the list from the back control", () => {
    const el = draw(card());
    select(el, "shim");
    el.querySelector<HTMLElement>("[data-hot-reload-back]")?.click();
    expect(el.querySelector(".hot-reload-list")).not.toBeNull();
    expect(el.hasAttribute(SELECTED_SERVICE_ATTRIBUTE)).toBe(false);
  });

  it("keeps the selected service's detail across a push", () => {
    const first = draw(card());
    select(first, "shim");
    const next = draw(card(), first);
    expect(next.querySelector(".hot-reload-detail-head .hot-reload-name")?.textContent).toBe("shim");
  });

  it("returns to the list when a push no longer lists the selected service", () => {
    const first = draw(card());
    select(first, "shim");
    const msg = card();
    msg.services = msg.services.slice(0, 1);
    const next = draw(msg, first);
    expect(next.querySelector(".hot-reload-list")).not.toBeNull();
    expect(next.hasAttribute(SELECTED_SERVICE_ATTRIBUTE)).toBe(false);
  });
});

describe("buildsText", () => {
  it("names both builds", () => {
    expect(buildsText({ previous: { shortHash: "a" }, fresh: { shortHash: "b" } } as never, "p")).toBe("build a → b");
  });

  it("names the fresh build alone when nothing stood before", () => {
    expect(buildsText({ fresh: { shortHash: "b" } } as never, "p")).toBe("build b");
  });
});

describe("drawRow: every state arm", () => {
  const rc = () => rowContext(harness().ctx, mergeRow("r1"));
  const cases: { arm: string; state: StateInit; glyph: string; clock: string | null }[] = [
    { arm: "scheduled", state: { state: { case: "scheduled", value: { words: { text: "scheduled" } } } }, glyph: "○", clock: null },
    { arm: "running", state: running("building"), glyph: "●", clock: "1m" },
    { arm: "waiting", state: { state: { case: "waiting", value: { words: { text: "waits" }, startedAtMs: 940_000n } } }, glyph: "●", clock: "1m" },
    { arm: "done", state: { state: { case: "done", value: { words: { text: "built" }, startedAtMs: 0n + 1n, endedAtMs: 5_001n } } }, glyph: "✓", clock: "5s" },
    { arm: "unchanged", state: { state: { case: "unchanged", value: { words: { text: "current" } } } }, glyph: "✓", clock: null },
    { arm: "failed", state: { state: { case: "failed", value: { words: { text: "broke" }, startedAtMs: 1n, endedAtMs: 2_001n } } }, glyph: "✗", clock: "2s" },
    { arm: "skipped", state: { state: { case: "skipped", value: { words: { text: "stopped" } } } }, glyph: "–", clock: null },
  ];
  for (const c of cases) {
    it(`draws ${c.arm} with its glyph and clock`, () => {
      const row = drawRow("x", create(FeedHotReloadStateSchema, c.state), null, rc(), "p");
      expect([row.getAttribute("data-state"), row.querySelector(".hot-reload-glyph")?.textContent]).toEqual([c.arm, c.glyph]);
      expect(row.querySelector(".hot-reload-clock")?.textContent ?? null).toBe(c.clock);
    });
  }

  it("refuses a row whose state is unset", () => {
    expect(() => drawRow("x", undefined, null, rc(), "p")).toThrow(MalformedView);
  });

  it("refuses a row whose state sets no arm", () => {
    expect(() => drawRow("x", create(FeedHotReloadStateSchema, {}), null, rc(), "p")).toThrow(MalformedView);
  });

  it("holds to the schema: every arm of FeedHotReloadState is drawn", () => {
    expect([...oneofArms(FeedHotReloadStateSchema, "state")].sort()).toEqual(cases.map((c) => c.arm).sort());
  });
});
