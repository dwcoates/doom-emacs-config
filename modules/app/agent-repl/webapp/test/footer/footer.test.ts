// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { captureLogRecords, type LogCapture } from "../log-capture.js";
import { create } from "@bufbuild/protobuf";
import { WatchFooterResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import {
  FooterExpandedFocusSchema,
  FooterStripSchema,
  type FooterStatus,
  type FooterView,
} from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { TICKING_ATTRIBUTE } from "../../src/feed/ticking.js";
import { compactionProgress } from "../../src/footer/progress.js";
import {
  buildWatchFooterRequest,
  mountFooter,
  type FooterHandle,
  panelStorageKey,
  readSelection,
  writeSelection,
} from "../../src/footer/footer.js";
import {
  WORKSPACE,
  expanded,
  footerView,
  harness,
  pushView,
  quietActivity,
  strip,
  type Harness,
} from "./harness.js";
import {
  clearClientFailures,
  reportClientFailure,
} from "../../src/rpc/link.js";
import STYLESHEET from "../../src/styles.css?raw";
import { cascadedValue, installStylesheet } from "../stylesheet.js";
import { enduringLine } from "./enduring-line.js";

const NOW = 1_800_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
  window.localStorage.clear();
});
afterEach(() => {
  // EVERY MOUNT IS DISPOSED, because a footer subscribes to the page-wide
  // client verdict (`src/rpc/link.ts`) and an undisposed one from an earlier
  // test would still be redrawing -- off whatever view it last held, malformed
  // ones included -- when a later test reports a failure. Production disposes
  // its one footer; the suite does the same.
  while (mounted.length > 0) mounted.pop()?.dispose();
  vi.useRealTimers();
});

/** Every footer this file mounted, disposed after the test that mounted it. */
const mounted: FooterHandle[] = [];

async function settle(): Promise<void> {
  for (let i = 0; i < 40; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** Mount the footer into a fresh host. */
function mount(
  h: Harness = harness(),
  selectDetachedWork: () => Promise<boolean> = async () => true,
) {
  const host = document.createElement("div");
  document.body.replaceChildren(host);
  const footer = mountFooter(host, h.ctx, {
    selectDetachedWork,
  });
  mounted.push(footer);
  return { host, h, footer };
}

/** One live agent row, so the agents panel has something to draw and does not
 * fold away. The daemon ships the chip count and the expanded rows together,
 * so a chip count rides with a matching row. */
const AGENT_ROW = {
  work: { value: "work-1" },
  jump: { target: { case: "entry" as const, value: { value: "bubble-1" } } },
  label: { text: "Explore" },
  tokens: { text: "0 tok" },
  runtime: { startedAtMs: BigInt(NOW) },
  state: { case: "running" as const, value: {} },
};

/** A view whose agents chip and expanded panel both carry `count` live agents. */
function withAgents(count = 1): ReturnType<typeof footerView> {
  const rows = Array.from({ length: count }, (_unused, i) => ({
    ...AGENT_ROW,
    work: { value: `work-${i + 1}` },
    jump: {
      target: { case: "entry" as const, value: { value: `bubble-${i + 1}` } },
    },
  }));
  return footerView({
    strip: strip({ liveWork: { agents: { count } } }),
    expanded: expanded({ agents: rows }),
  });
}

describe("buildWatchFooterRequest", () => {
  it("addresses the stream to this page's workspace", () => {
    expect(buildWatchFooterRequest(harness().ctx).workspace).toEqual(WORKSPACE);
  });
});

/** A thinking status whose activity is the daemon's compaction line. */
function compactingStatus(text: string): FooterStatus["status"] {
  return {
    case: "working",
    value: {
      substatus: { case: "compacting", value: {} },
      activity: {
        tier: {
          case: "salient",
          value: {
            at: { atMs: BigInt(NOW) },
            kind: { case: "compaction", value: { text } },
          },
        },
      },
    },
  } as unknown as FooterStatus["status"];
}

// THE COMPACTION LINE IS PUBLISHED PAGE-WIDE (owner's report, 2026-09-14): the
// cold-gate card waits on the compaction its own click started, and the daemon
// composes the only sentence there is for it on THIS stream.
describe("mountFooter: the compaction line it publishes", () => {
  it("publishes nothing before any push", async () => {
    // Arrange / Act
    mount();
    await settle();
    // Assert
    expect(compactionProgress()).toBeNull();
  });

  it("publishes the compaction line a push carried", async () => {
    // Arrange
    const { h } = mount();
    await settle();
    // Act
    h.tail.push(
      pushView(
        footerView({
          strip: strip({ status: compactingStatus("compacting · 412 of 900") }),
        }),
      ),
    );
    await settle();
    // Assert
    expect(compactionProgress()).toBe("compacting · 412 of 900");
  });

  it("publishes nothing for a push whose activity is some other arm", async () => {
    // Arrange
    const { h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({
          strip: strip({ status: compactingStatus("compacting · 412 of 900") }),
        }),
      ),
    );
    await settle();
    // Act
    h.tail.push(pushView(footerView()));
    await settle();
    // Assert
    expect(compactionProgress()).toBeNull();
  });

  it("drops the line when the footer is disposed", async () => {
    // Arrange
    const { h, footer } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({
          strip: strip({ status: compactingStatus("compacting · 412 of 900") }),
        }),
      ),
    );
    await settle();
    // Act
    footer.dispose();
    // Assert
    expect(compactionProgress()).toBeNull();
  });
});

describe("mountFooter: the standing stream", () => {
  it("marks the host so the integration suite can find the component", () => {
    const { host } = mount();
    expect(host.getAttribute("data-component")).toBe("footer");
  });

  it("opens WatchFooter on the page's workspace", async () => {
    const { h } = mount();
    await settle();
    expect(h.calls.watchFooter[0]?.workspace?.id).toBe("ws-1");
  });

  it("draws the strip on the first push", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-strip")).not.toBeNull();
  });

  it("REPLACES the view whole on a later push", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ liveWork: { agents: { count: 2 } } }) }),
      ),
    );
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelectorAll(".footer-chip")).toHaveLength(0);
  });

  it("draws no expanded section while nothing is selected", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-expanded")).toBeNull();
  });

  it("skips a push whose status sets no arm and files it as undecodable", async () => {
    const { h } = mount();
    await settle();
    h.tail.push(
      create(WatchFooterResponseSchema, {
        footer: {
          strip: create(FooterStripSchema, {
            status: {},
            clock: {},
            tokens: { input: { text: "0 in" } },
            liveWork: {},
          }),
          expanded: expanded(),
        },
      }),
    );
    await settle();
    expect(h.sink.reported).toContain("frameUndecodable");
  });

  it("keeps the stream open after an unreadable push", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(
      create(WatchFooterResponseSchema, {
        footer: {
          strip: create(FooterStripSchema, {
            status: {},
            clock: {},
            tokens: {},
            liveWork: {},
          }),
          expanded: expanded(),
        },
      }),
    );
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-strip")).not.toBeNull();
  });
});

describe("mountFooter: the panel selection", () => {
  it("opens a panel on a chip click, with NO round trip", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    expect(
      host.querySelector('.footer-expanded[data-panel="agents"]'),
    ).not.toBeNull();
    expect(h.calls.watchFooter).toHaveLength(1);
  });

  it("closes the open panel when its chip is clicked again", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    const click = (): void =>
      host
        .querySelector<HTMLElement>('[data-chip="agents"]')
        ?.dispatchEvent(new MouseEvent("click")) as never;
    click();
    expect(
      host.querySelector('.footer-expanded[data-panel="agents"]'),
    ).not.toBeNull();
    click();
    expect(host.querySelector(".footer-expanded")).toBeNull();
  });

  it("folds the agents panel away when the last agent resolves", async () => {
    // Arrange: the agents panel is open with a live agent.
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    expect(
      host.querySelector('.footer-expanded[data-panel="agents"]'),
    ).not.toBeNull();

    // Act: every agent resolves — the chip is unset and the rows are empty.
    h.tail.push(pushView(footerView()));
    await settle();

    // Assert: the expanded region collapses whole; no "stop all" is left behind.
    expect(host.querySelector(".footer-expanded")).toBeNull();
    expect(host.querySelector(".footer-stop-all")).toBeNull();
    expect(host.querySelector(".footer-divider")).toBeNull();
  });

  it("SURVIVES a push: a redraw must not close what the reader opened", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    h.tail.push(pushView(withAgents(2)));
    await settle();
    expect(
      host.querySelector('.footer-expanded[data-panel="agents"]'),
    ).not.toBeNull();
  });

  it("remembers the open panel per workspace", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ liveWork: { shells: { count: 1 } } }) }),
      ),
    );
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="shells"]')
      ?.dispatchEvent(new MouseEvent("click"));
    expect(window.localStorage.getItem(panelStorageKey("ws-1"))).toBe("shells");
  });

  it("forgets the panel when it is closed", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ liveWork: { shells: { count: 1 } } }) }),
      ),
    );
    await settle();
    const chip = host.querySelector<HTMLElement>('[data-chip="shells"]');
    chip?.dispatchEvent(new MouseEvent("click"));
    host
      .querySelector<HTMLElement>('[data-chip="shells"]')
      ?.dispatchEvent(new MouseEvent("click"));
    expect(window.localStorage.getItem(panelStorageKey("ws-1"))).toBeNull();
  });

  it("restores the remembered panel on the first push after a reload", async () => {
    window.localStorage.setItem(panelStorageKey("ws-1"), "crons");
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(
      host.querySelector('.footer-expanded[data-panel="crons"]'),
    ).not.toBeNull();
  });

  it("draws the empty panel's line for a remembered panel with nothing in it", async () => {
    window.localStorage.setItem(panelStorageKey("ws-1"), "crons");
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector("[data-empty]")?.textContent).toBe(
      "nothing scheduled",
    );
  });
});

/** VIEW carrying the daemon's focus on PANEL under GENERATION. */
function focused(
  view: FooterView,
  panel: "agents" | "shells" | "monitors" | "mergeTests",
  generation: bigint,
): FooterView {
  view.focus = create(FooterExpandedFocusSchema, {
    generation,
    panel: { case: panel, value: {} },
  });
  return view;
}

/** A view with a live agent, shell and monitor, so any focusable panel can be open. */
function withLiveWork(): FooterView {
  const view = withAgents();
  view.strip = strip({
    liveWork: {
      agents: { count: 1 },
      shells: { count: 1 },
      monitors: { count: 1 },
    },
  });
  return view;
}

/** Whichever panel the expanded section is drawing, or null when it is closed. */
function openPanel(host: HTMLElement): string | null {
  return (
    host
      .querySelector<HTMLElement>(".footer-expanded")
      ?.getAttribute("data-panel") ?? null
  );
}

/** A view whose merge is testing: the 🧪 chip set and one suite in the panel. */
function withMergeTests(rows = 1): FooterView {
  return footerView({
    strip: strip({ liveWork: { mergeTests: { finished: 0, total: rows } } }),
    expanded: expanded({
      mergeTests: Array.from({ length: rows }, (_unused, i) => ({
        name: { text: `suite-${i + 1}` },
        state: { state: { case: "waiting" as const, value: {} } },
      })),
    }),
  });
}

// THE MERGE TESTS PANEL (merge-landing.md, Landed change 1): the daemon focuses
// it when the merge begins testing, and it folds away when testing ends.
describe("mountFooter: the merge tests panel", () => {
  it("opens the closed section on the merge tests panel for a new testing focus", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withMergeTests(), "mergeTests", 1n)));
    await settle();
    expect(openPanel(host)).toBe("mergeTests");
  });

  it("marks the 🧪 chip selected when the testing focus opens its panel", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withMergeTests(), "mergeTests", 1n)));
    await settle();
    expect(
      host
        .querySelector('[data-chip="mergeTests"]')
        ?.getAttribute("data-selected"),
    ).toBe("true");
  });

  it("moves an open panel onto the merge tests panel when testing begins", async () => {
    const { host, h } = mount();
    await settle();
    const view = withLiveWork();
    h.tail.push(pushView(view));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    const testing = focused(withMergeTests(), "mergeTests", 1n);
    h.tail.push(pushView(testing));
    await settle();
    expect(openPanel(host)).toBe("mergeTests");
  });

  it("closes the section when testing ends and the panel empties", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withMergeTests(), "mergeTests", 1n)));
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-expanded")).toBeNull();
  });

  it("draws no divider once the emptied merge tests panel has folded", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withMergeTests(), "mergeTests", 1n)));
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-divider")).toBeNull();
  });

  it("reopens the merge tests panel for the next round's focus", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withMergeTests(), "mergeTests", 1n)));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="mergeTests"]')
      ?.dispatchEvent(new MouseEvent("click"));
    h.tail.push(pushView(focused(withMergeTests(2), "mergeTests", 2n)));
    await settle();
    expect(openPanel(host)).toBe("mergeTests");
  });

  it("remembers the merge tests panel as a click's would be", async () => {
    const { h } = mount();
    await settle();
    h.tail.push(pushView(focused(withMergeTests(), "mergeTests", 1n)));
    await settle();
    expect(window.localStorage.getItem(panelStorageKey(WORKSPACE.id))).toBe(
      "mergeTests",
    );
  });
});

// THE DAEMON'S FOCUS (owner's requirement): when detached work starts, the
// daemon names the panel to open, and the page applies each generation ONCE.
describe("mountFooter: the daemon's focus", () => {
  it.each(["agents", "shells", "monitors"] as const)(
    "opens the closed section on the %s panel for a generation it has not applied",
    async (panel) => {
      // Arrange
      const { host, h } = mount();
      await settle();

      // Act
      h.tail.push(pushView(focused(withLiveWork(), panel, 1n)));
      await settle();

      // Assert
      expect(openPanel(host)).toBe(panel);
    },
  );

  it("moves an open panel onto the focused one", async () => {
    // Arrange: the reader has the shells panel open.
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withLiveWork()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="shells"]')
      ?.dispatchEvent(new MouseEvent("click"));

    // Act
    h.tail.push(pushView(focused(withLiveWork(), "agents", 1n)));
    await settle();

    // Assert
    expect(openPanel(host)).toBe("agents");
  });

  it("leaves a reader's click standing across a push of the SAME generation", async () => {
    // Arrange: generation 1 opened agents, then the reader picked shells.
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withLiveWork(), "agents", 1n)));
    await settle();
    expect(openPanel(host), "the arrangement did not apply the focus").toBe(
      "agents",
    );
    host
      .querySelector<HTMLElement>('[data-chip="shells"]')
      ?.dispatchEvent(new MouseEvent("click"));

    // Act
    h.tail.push(pushView(focused(withLiveWork(), "agents", 1n)));
    await settle();

    // Assert
    expect(openPanel(host)).toBe("shells");
  });

  it("leaves a reader's close standing across a push of the SAME generation", async () => {
    // Arrange: generation 1 opened agents, then the reader closed it.
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withLiveWork(), "agents", 1n)));
    await settle();
    expect(openPanel(host), "the arrangement did not apply the focus").toBe(
      "agents",
    );
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));

    // Act
    h.tail.push(pushView(focused(withLiveWork(), "agents", 1n)));
    await settle();

    // Assert
    expect(openPanel(host)).toBeNull();
  });

  it("overrides a reader's click with a LATER generation", async () => {
    // Arrange: generation 1 opened agents, then the reader picked shells.
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(focused(withLiveWork(), "agents", 1n)));
    await settle();
    expect(openPanel(host), "the arrangement did not apply the focus").toBe(
      "agents",
    );
    host
      .querySelector<HTMLElement>('[data-chip="shells"]')
      ?.dispatchEvent(new MouseEvent("click"));

    // Act
    h.tail.push(pushView(focused(withLiveWork(), "agents", 2n)));
    await settle();

    // Assert
    expect(openPanel(host)).toBe("agents");
  });

  it("remembers the focused panel as a click's would be", async () => {
    // Arrange
    const { h } = mount();
    await settle();

    // Act
    h.tail.push(pushView(focused(withLiveWork(), "shells", 1n)));
    await settle();

    // Assert
    expect(window.localStorage.getItem(panelStorageKey("ws-1"))).toBe("shells");
  });

  it("skips a push whose focus names no panel and files it as undecodable", async () => {
    // Arrange
    const { h } = mount();
    await settle();
    const view = withLiveWork();
    view.focus = create(FooterExpandedFocusSchema, { generation: 1n });

    // Act
    h.tail.push(pushView(view));
    await settle();

    // Assert
    expect(h.sink.reported).toContain("frameUndecodable");
  });
});

describe("the persisted selection, read and written behind try/catch", () => {
  it("answers null when nothing was stored", () => {
    expect(readSelection(harness().ctx)).toBeNull();
  });

  it("round-trips a stored panel", () => {
    const { ctx } = harness();
    writeSelection(ctx, "tasks");
    expect(readSelection(ctx)).toBe("tasks");
  });

  it("discards a stored value that is not a panel name", () => {
    const { ctx } = harness();
    window.localStorage.setItem(panelStorageKey("ws-1"), "workflow");
    expect(readSelection(ctx)).toBeNull();
  });

  it("survives a browser that throws on the storage accessor", () => {
    const { ctx } = harness();
    const getItem = vi
      .spyOn(Storage.prototype, "getItem")
      .mockImplementation(() => {
        throw new Error("site data is blocked");
      });
    expect(readSelection(ctx)).toBeNull();
    getItem.mockRestore();
  });

  it("survives a storage that throws on write", () => {
    const { ctx } = harness();
    const setItem = vi
      .spyOn(Storage.prototype, "setItem")
      .mockImplementation(() => {
        throw new Error("quota exceeded");
      });
    expect(() => writeSelection(ctx, "tokens")).not.toThrow();
    setItem.mockRestore();
  });
});

describe("mountFooter: onStatus", () => {
  it("announces the status arm on every push", async () => {
    const { footer, h } = mount();
    const seen: string[] = [];
    footer.onStatus((arm) => seen.push(arm));
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    h.tail.push(
      pushView(
        footerView({
          strip: strip({
            status: {
              case: "merging",
              value: {
                substatus: { case: "merge", value: {} },
                activity: quietActivity("merging"),
              },
            } as never,
          }),
        }),
      ),
    );
    await settle();
    expect(seen).toEqual(["idle", "merging"]);
  });

  it("announces the substatus arm the push drew beside the status", async () => {
    // Arrange
    const { footer, h } = mount();
    const seen: [string, string | undefined][] = [];
    footer.onStatus((arm, sub) => seen.push([arm, sub]));
    await settle();

    // Act
    h.tail.push(
      pushView(
        footerView({
          strip: strip({
            status: {
              case: "vendorFault",
              value: {
                substatus: { case: "apiRetrying", value: {} },
                activity: quietActivity("vendorFault"),
              },
            } as never,
          }),
        }),
      ),
    );
    await settle();

    // Assert
    expect(seen).toEqual([["vendorFault", "apiRetrying"]]);
  });

  it("tells a LATE subscriber the current arm immediately", async () => {
    const { footer, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    const seen: string[] = [];
    footer.onStatus((arm) => seen.push(arm));
    expect(seen).toEqual(["idle"]);
  });

  it("says nothing to a subscriber before the first push", async () => {
    const { footer } = mount();
    const seen: string[] = [];
    footer.onStatus((arm) => seen.push(arm));
    await settle();
    expect(seen).toEqual([]);
  });

  it("stops announcing to an unsubscribed listener", async () => {
    const { footer, h } = mount();
    const seen: string[] = [];
    const off = footer.onStatus((arm) => seen.push(arm));
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    off();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(seen).toEqual(["idle"]);
  });
});

describe("mountFooter: dispose", () => {
  it("empties the host", async () => {
    const { host, footer, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    footer.dispose();
    expect(host.children).toHaveLength(0);
  });

  it("drops every clock the footer started", async () => {
    const { host, footer, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) }),
      ),
    );
    await settle();
    footer.dispose();
    expect(host.querySelectorAll(`[${TICKING_ATTRIBUTE}]`)).toHaveLength(0);
  });

  it("draws nothing more after disposal", async () => {
    const { host, footer, h } = mount();
    await settle();
    footer.dispose();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-strip")).toBeNull();
  });

  it("is idempotent", async () => {
    const { footer } = mount();
    await settle();
    footer.dispose();
    expect(() => footer.dispose()).not.toThrow();
  });
});

describe("mountFooter: a stop's own answer outlives the push it caused", () => {
  // THE VIEW IS DRAWN WHOLE ON EVERY PUSH, and a stop is the one thing on the
  // footer whose answer is NOT pushed: the note, the refusal and the confirm
  // challenge are the click's own, drawn at the control. A stop always causes
  // the next push -- the live set it just emptied is in the view -- so a
  // control rebuilt per draw loses its answer within milliseconds. Measured in
  // the G51 playbook: the daemon answered `interrupted_detached count=3` and
  // the footer that came back carried a bare "stop all", which is the only
  // place that count is ever stated.

  it("keeps the SAME turn-stop element across a redraw", async () => {
    // Arrange: a live turn, so the strip mounts the stop.
    const { host, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) }),
      ),
    );
    await settle();
    const before = host.querySelector(".footer-stop-turn");
    // Act: another push, which redraws the whole view.
    h.tail.push(
      pushView(
        footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 2000) }) }),
      ),
    );
    await settle();
    // Assert
    expect(host.querySelector(".footer-stop-turn")).toBe(before);
  });

  it("carries whatever the stop drew at the control through that redraw", async () => {
    // Arrange
    const { host, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) }),
      ),
    );
    await settle();
    const note = document.createElement("span");
    note.className = "footer-stop-note";
    note.setAttribute("data-stop-outcome", "interruptedDetached");
    note.textContent = "stopped 3 agents";
    host.querySelector(".footer-stop-turn")?.appendChild(note);
    // Act
    h.tail.push(
      pushView(
        footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 2000) }) }),
      ),
    );
    await settle();
    // Assert
    expect(host.querySelector(".footer-stop-note")?.textContent).toBe(
      "stopped 3 agents",
    );
  });

  it("drops the controls with the footer, so nothing outlives the mount", async () => {
    // Arrange
    const { host, footer, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) }),
      ),
    );
    await settle();
    // Act
    footer.dispose();
    // Assert
    expect(host.querySelector(".footer-stop-turn")).toBeNull();
  });
});

/**
 * THE DOCK'S ORDER (owner ruling, 2026-09-13): the strip on top, one divider,
 * the expanded section under it. These pin the ORDER and the PARTITION, which
 * is what the ruling settles; every panel's own content is `expanded.test.ts`.
 */
describe("mountFooter: the strip on top, the expanded section under it", () => {
  /** Mount, push a view with an agent, and open the agents panel. */
  async function openPanel(): Promise<HTMLElement> {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    return host;
  }

  it("draws the expanded panel as a FOLLOWING sibling of the strip", async () => {
    // Arrange / Act
    const host = await openPanel();
    // Assert
    const dock = host.querySelector(".pfooter");
    const children = [...(dock?.children ?? [])];
    expect(
      children.indexOf(host.querySelector(".footer-expanded") as Element),
    ).toBeGreaterThan(
      children.indexOf(host.querySelector(".footer-strip") as Element),
    );
  });

  it("puts the divider BETWEEN the strip and the expanded panel", async () => {
    // Arrange / Act
    const host = await openPanel();
    // Assert
    const dock = host.querySelector(".pfooter");
    const named = [".footer-strip", ".footer-divider", ".footer-expanded"];
    expect(
      [...(dock?.children ?? [])].map(
        (el) => named.find((sel) => el.matches(sel)) ?? el.className,
      ),
    ).toEqual(named);
  });

  it("draws NO divider while the footer is closed: one section needs no partition", async () => {
    // Arrange
    const { host, h } = mount();
    await settle();
    // Act
    h.tail.push(pushView(footerView()));
    await settle();
    // Assert
    expect(host.querySelector(".footer-divider")).toBeNull();
  });

  it("hangs the divider off the DOCK, not off the padded expanded section", async () => {
    // Arrange / Act
    const host = await openPanel();
    // Assert
    expect(
      host.querySelector(".footer-divider")?.parentElement?.className,
    ).toBe("pfooter");
  });

  it("paints the divider one step darker than the rows' own delimiter", async () => {
    // Arrange
    const teardown = installStylesheet();
    // Act
    const host = await openPanel();
    // Assert
    try {
      expect(
        cascadedValue(
          host.querySelector(".footer-divider") as Element,
          "border-top-color",
        ),
      ).toBe("var(--border-strong)");
    } finally {
      teardown();
    }
  });

  it("takes its darker token from the row delimiter's own, one step down", () => {
    // Arrange / Act / Assert: same hue, derived — never a second unrelated grey.
    expect(STYLESHEET).toMatch(
      /--border-strong:\s*color-mix\(in srgb, var\(--border\) 85%, var\(--fg\)\);/,
    );
  });

  it("insets the divider on NEITHER side, so it spans the dock edge to edge", async () => {
    // Arrange
    const teardown = installStylesheet();
    // Act
    const host = await openPanel();
    // Assert
    const divider = host.querySelector(".footer-divider") as Element;
    try {
      expect(
        ["margin-left", "margin-right", "padding-left", "padding-right"].map(
          (p) => cascadedValue(divider, p),
        ),
      ).toEqual(["0px", "0px", "0px", "0px"]);
    } finally {
      teardown();
    }
  });

  it("measures the divider at the dock's own full width", async () => {
    // Arrange: jsdom lays nothing out, so the dock's width is stubbed and the
    // divider -- a plain in-flow block with no inset -- is measured against it.
    const teardown = installStylesheet();
    const host = await openPanel();
    const dock = host.querySelector(".pfooter") as HTMLElement;
    const divider = host.querySelector(".footer-divider") as HTMLElement;
    const DOCK_WIDTH = 640;
    Object.defineProperty(dock, "clientWidth", {
      value: DOCK_WIDTH,
      configurable: true,
    });
    // Act: the width a no-inset in-flow block takes is its container's, less
    // whatever the cascade insets it by -- which the rule pins at zero.
    const inset = [
      "margin-left",
      "margin-right",
      "padding-left",
      "padding-right",
    ]
      .map((p) => Number.parseFloat(cascadedValue(divider, p)))
      .reduce((a, b) => a + b, 0);
    // Assert
    try {
      expect(dock.clientWidth - inset).toBe(DOCK_WIDTH);
    } finally {
      teardown();
    }
  });
});

describe("mountFooter: the client's own verdict overlays the daemon's view", () => {
  afterEach(() => {
    clearClientFailures();
  });

  it("draws the status disconnected under a unary transport failure", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-status")?.textContent).toBe(
      "disconnected",
    );
  });

  it("draws the substatus daemon unreachable under a unary transport failure", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-substatus")?.textContent).toBe(
      "daemon unreachable",
    );
  });

  it("draws the failing call's own line as the activity", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(
      host.querySelector(".footer-activity-client-verdict")?.textContent,
    ).toBe("AnswerColdGate: unavailable");
  });

  it("draws a source_ended subscription's own line as the activity", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure(
      "subscription_source_ended",
      "the footer-1 page subscription ended (source_ended)",
    );
    expect(
      host.querySelector(".footer-activity-client-verdict")?.textContent,
    ).toBe("the footer-1 page subscription ended (source_ended)");
  });

  it("names the reporting site on the dock, for the integration suite", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure(
      "stream_ended",
      "WatchFooter stream ended (producer_ended)",
    );
    expect(
      host.querySelector(".pfooter")?.getAttribute("data-client-verdict"),
    ).toBe("stream_ended");
  });

  it("does NOT let a daemon push override a standing verdict", async () => {
    const { host, h } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-status")?.textContent).toBe(
      "disconnected",
    );
  });

  it("restores the daemon's view on the next push once the verdict is cleared", async () => {
    const { host, h } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    clearClientFailures();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-status")?.textContent).toBe("idle");
  });

  it("redraws the daemon's last view the moment the verdict is cleared", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    clearClientFailures();
    expect(host.querySelector(".footer-status")?.textContent).toBe("idle");
  });

  it("does NOT close the composer gate, the retry being what lifts the verdict", async () => {
    const { footer, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    const seen: string[] = [];
    footer.onStatus((statusCase) => seen.push(statusCase));
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(seen).toEqual(["idle"]);
  });

  it("keeps the daemon's last tokens cell beside the client's status cells", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(
      pushView(
        footerView({
          strip: strip({ tokens: { input: { text: "9.9k in" } } }),
        }),
      ),
    );
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-tokens")?.textContent).toContain(
      "9.9k in",
    );
  });

  it("files an unreadable last view rather than throwing at whoever reported", async () => {
    // ARRANGE: a push whose status sets no arm. It is skipped at the push, and
    // the view it left behind cannot be redrawn either.
    const { h } = mount();
    await settle();
    h.tail.push(
      create(WatchFooterResponseSchema, {
        footer: {
          strip: create(FooterStripSchema, {
            status: {},
            clock: {},
            tokens: { input: { text: "0 in" } },
            liveWork: {},
          }),
          expanded: expanded(),
        },
      }),
    );
    await settle();
    h.sink.reported.length = 0;
    // ACT: clearing a verdict redraws, and the redraw hits the same view.
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    clearClientFailures();
    // ASSERT
    expect(h.sink.reported).toContain("frameUndecodable");
  });

  it("stops drawing verdicts once disposed", async () => {
    const { host, footer } = mount();
    await settle();
    footer.dispose();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-status")).toBeNull();
  });
});

/**
 * THE DID-NOTHING PATH (owner report, 2026-09-23): a live subagent row whose
 * token count kept updating was clicked and showed neither a selection nor a
 * notice. The footer redraws whole on every push, and the notice used to be
 * appended to the ROW ELEMENT that was clicked -- after the reveal's round trip,
 * by which time a push had already replaced it. The notice is now the mount's
 * state, painted by every draw.
 */
describe("mountFooter: a row's click outcome outlives the pushes around it", () => {
  /** Mount with a selection the test answers by hand, and open the agents panel. */
  async function openWithPendingSelect() {
    let answer: (reached: boolean) => void = () => undefined;
    const pending = new Promise<boolean>((resolve) => {
      answer = resolve;
    });
    const { host, h } = mount(harness(), () => pending);
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    return { host, h, answer };
  }

  it("draws the notice on the LIVE row when a push landed while the reveal was in flight", async () => {
    // Arrange: the click is in flight.
    const { host, h, answer } = await openWithPendingSelect();
    host
      .querySelector<HTMLElement>(".footer-row-jump")
      ?.dispatchEvent(new MouseEvent("click"));
    // Act: a token update pushes the footer, then the reveal misses.
    h.tail.push(pushView(withAgents()));
    await settle();
    answer(false);
    await settle();
    // Assert
    expect(
      host.querySelector(".footer-row-jump .footer-row-unreachable")
        ?.textContent,
    ).toBe("not on screen");
  });

  it("keeps the notice standing across the next push", async () => {
    // Arrange
    const { host, h, answer } = await openWithPendingSelect();
    host
      .querySelector<HTMLElement>(".footer-row-jump")
      ?.dispatchEvent(new MouseEvent("click"));
    answer(false);
    await settle();
    // Act
    h.tail.push(pushView(withAgents()));
    await settle();
    // Assert
    expect(host.querySelector(".footer-row-unreachable")?.textContent).toBe(
      "not on screen",
    );
  });

  it("drops the notice once the row leaves the view", async () => {
    // Arrange
    const { host, h, answer } = await openWithPendingSelect();
    host
      .querySelector<HTMLElement>(".footer-row-jump")
      ?.dispatchEvent(new MouseEvent("click"));
    answer(false);
    await settle();
    // Act: the row goes, then comes back.
    h.tail.push(pushView(withAgents(0)));
    await settle();
    h.tail.push(pushView(withAgents()));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    // Assert
    expect(host.querySelector(".footer-row-unreachable")).toBeNull();
  });
});

describe("mountFooter: the expanded section's scroll is the reader's", () => {
  /** Mount, push a view with SIX agents (past the cap), and open the panel. */
  async function openBusyPanel() {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(withAgents(6)));
    await settle();
    host
      .querySelector<HTMLElement>('[data-chip="agents"]')
      ?.dispatchEvent(new MouseEvent("click"));
    return { host, h };
  }

  it("keeps the SAME section element across a push", async () => {
    // Arrange
    const { host, h } = await openBusyPanel();
    const before = host.querySelector(".footer-expanded");
    // Act
    h.tail.push(pushView(withAgents(6)));
    await settle();
    // Assert
    expect(host.querySelector(".footer-expanded")).toBe(before);
  });

  it("never detaches the section or the dock on a push", async () => {
    // Arrange
    const { host, h } = await openBusyPanel();
    const section = host.querySelector(".footer-expanded");
    const dock = host.querySelector(".pfooter");
    const removed: Node[] = [];
    const observer = new MutationObserver((records) => {
      for (const record of records) removed.push(...record.removedNodes);
    });
    observer.observe(host, { childList: true, subtree: true });
    // Act
    h.tail.push(pushView(withAgents(6)));
    await settle();
    observer.disconnect();
    // Assert
    expect(removed).not.toContain(section);
    expect(removed).not.toContain(dock);
  });

  it("keeps the reader's scroll position across a push", async () => {
    // Arrange
    const { host, h } = await openBusyPanel();
    const section = host.querySelector<HTMLElement>(".footer-expanded");
    if (section === null) throw new Error("no section");
    section.scrollTop = 35;
    // Act
    h.tail.push(pushView(withAgents(6)));
    await settle();
    // Assert
    expect(section.scrollTop).toBe(35);
  });

  it("scrolls past four rows under the real stylesheet", async () => {
    // Arrange
    const uninstall = installStylesheet();
    const { host } = await openBusyPanel();
    const section = host.querySelector(".footer-expanded");
    if (section === null) throw new Error("no section");
    // Act
    const overflow = cascadedValue(section, "overflow-y");
    const rows = (section as HTMLElement).style.getPropertyValue(
      "--pfooter-sheet-rows",
    );
    uninstall();
    // Assert
    expect({ overflow, rows }).toEqual({ overflow: "auto", rows: "4" });
  });

  it("caps the section's height at the markup's row count in the stylesheet", () => {
    // Assert: the ceiling is rows x row height, with the count read from the markup.
    expect(STYLESHEET).toMatch(
      /\.pfooter-sheet \{[^}]*max-height: calc\(var\(--pfooter-row-h\) \* var\(--pfooter-sheet-rows\)\);/,
    );
  });
});

// ---- the transient's expiry: the one re-render the footer schedules --------

/** The status arms whose cell carries the unpinned tiers these tests push. */
type TransientStatus = "idle" | "working" | "background";

/** A view whose cell, under STATUS, carries a hook transient lapsing at EXPIRESATMS. */
function transientView(
  name: string,
  expiresAtMs: number,
  enduring: Record<string, unknown> = {},
  status: TransientStatus = "idle",
): ReturnType<typeof footerView> {
  return footerView({
    strip: strip({
      status: {
        case: status,
        value: {
          ...(status === "working"
            ? { substatus: { case: "thinking", value: {} } }
            : {}),
          activity: {
            tier: {
              case: "unpinned",
              value: {
                transient: {
                  at: { atMs: BigInt(NOW) },
                  expiry: { expiresAtMs: BigInt(expiresAtMs) },
                  kind: { case: "hook", value: { name } },
                },
                enduring: enduringLine(enduring),
              },
            },
          },
        },
      } as never,
    }),
  });
}

/** The drawn activity cell's tier. */
function tierOf(host: HTMLElement): string | null | undefined {
  return host.querySelector(".footer-activity")?.getAttribute("data-tier");
}

describe("mountFooter: the transient's expiry", () => {
  it("draws the pushed transient while it is live", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(transientView("fmt", NOW + 10_000)));
    await settle();
    expect(host.querySelector(".footer-activity-hook")?.textContent).toBe(
      "fmt",
    );
  });

  it("redraws the enduring line at the expiry instant with no push", async () => {
    // Arrange
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(transientView("fmt", NOW + 10_000)));
    await settle();
    // Act
    await vi.advanceTimersByTimeAsync(10_000);
    // Assert
    expect(tierOf(host)).toBe("enduring");
  });

  // THE QUIET TIER IS RETIRED (owner ruling, 2026-10-01): under the statuses
  // that once carried a quiet-stretch line, a lapsed transient gives way to
  // the enduring line and nothing else.
  it.each(["working", "background"] as const)(
    "redraws the enduring line at the expiry instant under %s",
    async (status) => {
      // Arrange
      const { host, h } = mount();
      await settle();
      h.tail.push(pushView(transientView("fmt", NOW + 10_000, {}, status)));
      await settle();
      // Act
      await vi.advanceTimersByTimeAsync(10_000);
      // Assert
      expect(tierOf(host)).toBe("enduring");
    },
  );

  it("keeps the transient drawn until the instant arrives", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(transientView("fmt", NOW + 10_000)));
    await settle();
    await vi.advanceTimersByTimeAsync(9_000);
    expect(tierOf(host)).toBe("transient");
  });

  it("replaces the pending re-render when a newer transient is pushed", async () => {
    // Arrange: the first transient would lapse at +10s.
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(transientView("first", NOW + 10_000)));
    await settle();
    // Act: a newer one, pushed at +5s, lapses at +15s.
    await vi.advanceTimersByTimeAsync(5_000);
    h.tail.push(pushView(transientView("second", NOW + 15_000)));
    await settle();
    await vi.advanceTimersByTimeAsync(5_000);
    // Assert: the first instant passed and the newer transient still stands.
    expect(host.querySelector(".footer-activity-hook")?.textContent).toBe(
      "second",
    );
  });

  it("cancels the pending re-render when a push carries no transient", async () => {
    const { h } = mount();
    await settle();
    h.tail.push(pushView(transientView("fmt", NOW + 10_000)));
    await settle();
    const before = vi.getTimerCount();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(vi.getTimerCount()).toBe(before - 1);
  });

  it("cancels the pending re-render on dispose", async () => {
    const { h, footer } = mount();
    await settle();
    h.tail.push(pushView(transientView("fmt", NOW + 10_000)));
    await settle();
    const before = vi.getTimerCount();
    footer.dispose();
    // The dispose also drops the stream's and the clocks' timers; the expiry
    // is gone with them, so nothing is left to fire at the instant.
    expect(vi.getTimerCount()).toBeLessThan(before);
    await vi.advanceTimersByTimeAsync(10_000);
    expect(vi.getTimerCount()).toBe(0);
  });

  it("files an enduring line that cannot be drawn at the expiry, rather than throwing", async () => {
    // ARRANGE: the enduring usage's reset instant does not fit a JS number,
    // which only the expiry's redraw reaches.
    const { h } = mount();
    await settle();
    h.tail.push(
      pushView(
        transientView("fmt", NOW + 10_000, {
          usage: {
            session: {
              newsworthy: false,
              utilization: 0.1,
              resetsAtS: 2n ** 62n,
            },
          },
        }),
      ),
    );
    await settle();
    h.sink.reported.length = 0;
    // ACT
    await vi.advanceTimersByTimeAsync(10_000);
    // ASSERT
    expect(h.sink.reported).toContain("frameUndecodable");
  });
});

describe("mountFooter: the status the strip draws is recorded", () => {
  afterEach(() => {
    clearClientFailures();
  });

  /** The footer.drawn-status records a capture holds, in order. */
  async function drawnStatuses(capture: LogCapture) {
    capture.logger.flush();
    await Promise.resolve();
    return capture.sent
      .filter((record) => record.operation === "footer.drawn-status")
      .map((record) => record.context as Record<string, unknown>);
  }

  it("records the daemon's arm the strip draws", async () => {
    // Arrange
    const capture = captureLogRecords();
    const { host, h } = mount();
    await settle();
    // Act
    h.tail.push(pushView(footerView()));
    await settle();
    // Assert
    const drawnArm = host.querySelector<HTMLElement>(".footer-status")?.dataset.arm;
    expect(await drawnStatuses(capture)).toEqual([
      expect.objectContaining({ arm: drawnArm, source: "daemon" }),
    ]);
  });

  it("records the client's verdict when it stands over the daemon's view", async () => {
    // Arrange
    const capture = captureLogRecords();
    mount();
    await settle();
    // Act
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    // Assert
    const records = await drawnStatuses(capture);
    expect(records.at(-1)).toEqual(
      expect.objectContaining({ arm: "disconnected", substatus: "unary_transport", source: "client_verdict" }),
    );
  });
});
