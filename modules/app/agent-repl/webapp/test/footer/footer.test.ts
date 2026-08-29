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

const NOW = 1_800_000_000_000;

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
  window.localStorage.clear();
});
afterEach(() => {
  vi.useRealTimers();
});

async function settle(): Promise<void> {
  for (let i = 0; i < 40; i += 1) await vi.advanceTimersByTimeAsync(0);
}

/** Mount the footer into a fresh host. */
function mount(h: Harness = harness()) {
  const host = document.createElement("div");
  document.body.replaceChildren(host);
  return { host, h, footer: mountFooter(host, h.ctx, { revealRow: async () => true }) };
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
