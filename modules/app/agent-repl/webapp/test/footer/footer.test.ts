// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  WatchFooterResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_footer_pb";
import { FooterStripSchema } from "../../../proto/gen/ts/frontend/v1/footer_pb";
import { TICKING_ATTRIBUTE } from "../../src/feed/ticking.js";
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
  strip,
  type Harness,
} from "./harness.js";
import {
  clearClientFailures,
  reportClientFailure,
} from "../../src/rpc/link.js";
import STYLESHEET from "../../src/styles.css?raw";
import { cascadedValue, installStylesheet } from "../stylesheet.js";

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
function mount(h: Harness = harness()) {
  const host = document.createElement("div");
  document.body.replaceChildren(host);
  const footer = mountFooter(host, h.ctx, { revealRow: async () => true });
  mounted.push(footer);
  return { host, h, footer };
}

describe("buildWatchFooterRequest", () => {
  it("addresses the stream to this page's workspace", () => {
    expect(buildWatchFooterRequest(harness().ctx).workspace).toEqual(WORKSPACE);
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
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { agents: { count: 2 } } }) })));
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
          strip: create(FooterStripSchema, { status: {}, clock: {}, tokens: {}, liveWork: {} }),
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
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { agents: { count: 1 } } }) })));
    await settle();
    host.querySelector<HTMLElement>('[data-chip="agents"]')?.dispatchEvent(new MouseEvent("click"));
    expect(host.querySelector('.footer-expanded[data-panel="agents"]')).not.toBeNull();
    expect(h.calls.watchFooter).toHaveLength(1);
  });

  it("closes the open panel when its chip is clicked again", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { agents: { count: 1 } } }) })));
    await settle();
    const click = (): void =>
      host
        .querySelector<HTMLElement>('[data-chip="agents"]')
        ?.dispatchEvent(new MouseEvent("click")) as never;
    click();
    click();
    expect(host.querySelector(".footer-expanded")).toBeNull();
  });

  it("SURVIVES a push: a redraw must not close what the reader opened", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { agents: { count: 1 } } }) })));
    await settle();
    host.querySelector<HTMLElement>('[data-chip="agents"]')?.dispatchEvent(new MouseEvent("click"));
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { agents: { count: 2 } } }) })));
    await settle();
    expect(host.querySelector('.footer-expanded[data-panel="agents"]')).not.toBeNull();
  });

  it("remembers the open panel per workspace", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { shells: { count: 1 } } }) })));
    await settle();
    host.querySelector<HTMLElement>('[data-chip="shells"]')?.dispatchEvent(new MouseEvent("click"));
    expect(window.localStorage.getItem(panelStorageKey("ws-1"))).toBe("shells");
  });

  it("forgets the panel when it is closed", async () => {
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { shells: { count: 1 } } }) })));
    await settle();
    const chip = host.querySelector<HTMLElement>('[data-chip="shells"]');
    chip?.dispatchEvent(new MouseEvent("click"));
    host.querySelector<HTMLElement>('[data-chip="shells"]')?.dispatchEvent(new MouseEvent("click"));
    expect(window.localStorage.getItem(panelStorageKey("ws-1"))).toBeNull();
  });

  it("restores the remembered panel on the first push after a reload", async () => {
    window.localStorage.setItem(panelStorageKey("ws-1"), "crons");
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector('.footer-expanded[data-panel="crons"]')).not.toBeNull();
  });

  it("draws the empty panel's line for a remembered panel with nothing in it", async () => {
    window.localStorage.setItem(panelStorageKey("ws-1"), "crons");
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector("[data-empty]")?.textContent).toBe("nothing scheduled");
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
    const setItem = vi.spyOn(Storage.prototype, "setItem").mockImplementation(() => {
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
          strip: strip({ status: { case: "merging", value: { substatus: { case: "merge", value: {} } } } as never }),
        }),
      ),
    );
    await settle();
    expect(seen).toEqual(["idle", "merging"]);
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
    h.tail.push(pushView(footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) })));
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
    h.tail.push(pushView(footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) })));
    await settle();
    const before = host.querySelector(".footer-stop-turn");
    // Act: another push, which redraws the whole view.
    h.tail.push(pushView(footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 2000) }) })));
    await settle();
    // Assert
    expect(host.querySelector(".footer-stop-turn")).toBe(before);
  });

  it("carries whatever the stop drew at the control through that redraw", async () => {
    // Arrange
    const { host, h } = mount();
    await settle();
    h.tail.push(pushView(footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) })));
    await settle();
    const note = document.createElement("span");
    note.className = "footer-stop-note";
    note.setAttribute("data-stop-outcome", "interruptedDetached");
    note.textContent = "stopped 3 agents";
    host.querySelector(".footer-stop-turn")?.appendChild(note);
    // Act
    h.tail.push(pushView(footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 2000) }) })));
    await settle();
    // Assert
    expect(host.querySelector(".footer-stop-note")?.textContent).toBe("stopped 3 agents");
  });

  it("drops the controls with the footer, so nothing outlives the mount", async () => {
    // Arrange
    const { host, footer, h } = mount();
    await settle();
    h.tail.push(pushView(footerView({ strip: strip({ turnStartedAtMs: BigInt(NOW - 1000) }) })));
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
    h.tail.push(pushView(footerView({ strip: strip({ liveWork: { agents: { count: 1 } } }) })));
    await settle();
    host.querySelector<HTMLElement>('[data-chip="agents"]')?.dispatchEvent(new MouseEvent("click"));
    return host;
  }

  it("draws the expanded panel as a FOLLOWING sibling of the strip", async () => {
    // Arrange / Act
    const host = await openPanel();
    // Assert
    const dock = host.querySelector(".pfooter");
    const children = [...(dock?.children ?? [])];
    expect(children.indexOf(host.querySelector(".footer-expanded") as Element)).toBeGreaterThan(
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
      [...(dock?.children ?? [])].map((el) => named.find((sel) => el.matches(sel)) ?? el.className),
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
    expect(host.querySelector(".footer-divider")?.parentElement?.className).toBe("pfooter");
  });

  it("paints the divider one step darker than the rows' own delimiter", async () => {
    // Arrange
    const teardown = installStylesheet();
    // Act
    const host = await openPanel();
    // Assert
    try {
      expect(cascadedValue(host.querySelector(".footer-divider") as Element, "border-top-color"))
        .toBe("var(--border-strong)");
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
        ["margin-left", "margin-right", "padding-left", "padding-right"].map((p) =>
          cascadedValue(divider, p),
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
    Object.defineProperty(dock, "clientWidth", { value: DOCK_WIDTH, configurable: true });
    // Act: the width a no-inset in-flow block takes is its container's, less
    // whatever the cascade insets it by -- which the rule pins at zero.
    const inset = ["margin-left", "margin-right", "padding-left", "padding-right"]
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
    expect(host.querySelector(".footer-status")?.textContent).toBe("disconnected");
  });

  it("draws the substatus daemon unreachable under a unary transport failure", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-substatus")?.textContent).toBe("daemon unreachable");
  });

  it("draws the failing call's own line as the activity", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-activity-client-verdict")?.textContent).toBe(
      "AnswerColdGate: unavailable",
    );
  });

  it("draws a source_ended subscription's own line as the activity", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("subscription_source_ended", "the footer-1 page subscription ended (source_ended)");
    expect(host.querySelector(".footer-activity-client-verdict")?.textContent).toBe(
      "the footer-1 page subscription ended (source_ended)",
    );
  });

  it("names the reporting site on the dock, for the integration suite", async () => {
    const { host } = mount();
    await settle();
    reportClientFailure("stream_ended", "WatchFooter stream ended (producer_ended)");
    expect(host.querySelector(".pfooter")?.getAttribute("data-client-verdict")).toBe("stream_ended");
  });

  it("does NOT let a daemon push override a standing verdict", async () => {
    const { host, h } = mount();
    await settle();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    h.tail.push(pushView(footerView()));
    await settle();
    expect(host.querySelector(".footer-status")?.textContent).toBe("disconnected");
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

  it("publishes disconnected to the composer gate while the verdict stands", async () => {
    const { footer } = mount();
    await settle();
    const seen: string[] = [];
    footer.onStatus((statusCase) => seen.push(statusCase));
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(seen).toEqual(["disconnected"]);
  });

  it("stops drawing verdicts once disposed", async () => {
    const { host, footer } = mount();
    await settle();
    footer.dispose();
    reportClientFailure("unary_transport", "AnswerColdGate: unavailable");
    expect(host.querySelector(".footer-status")).toBeNull();
  });
});
