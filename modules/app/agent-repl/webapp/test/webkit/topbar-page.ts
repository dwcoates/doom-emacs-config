/**
 * THE PAGE THE WEBKIT TOPBAR TEST DRIVES (topbar.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit over the
 * REAL stylesheet, drawing the REAL topbar cells, so what is measured is what
 * the webview paints rather than markup a test wrote to look like it.
 */
import { create } from "@bufbuild/protobuf";
import { TopbarPersistentWifiSchema, TopbarViewSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import type { TopbarContext } from "../../src/topbar/context.js";
import { drawTopbarPersistentWifi } from "../../src/topbar/persistent-wifi.js";
import { mountRevealLayer } from "../../src/topbar/reveal.js";
import { drawTopbarView } from "../../src/topbar/topbar.js";
import { appContext } from "../topbar/fixtures.js";

/** What the test calls, on `window.topbarPage`. */
export interface TopbarPage {
  /**
   * Draw the persistent-wifi chip, joined and with the mode on, OFFSET css px
   * right of a whole-pixel edge, so the chip lands at a fractional position.
   */
  drawWifi(offset: number): void;
  /**
   * Draw the whole strip in a `#topbar` header, with a session (every
   * session-scoped control drawn) or without one (their dashes drawn), and
   * measure it.
   */
  drawStrip(session: boolean): StripMeasure;
  /** Every record the page wrote, as `level operation`. */
  records(): string[];
}

/** One right-hand cell: its class, and its visible box's content edges. */
export interface CellMeasure {
  cell: string;
  left: number;
  right: number;
  fontSize: string;
}

/** What one drawn strip measured, in CSS px. */
export interface StripMeasure {
  /** The account label's computed font size. */
  accountFontSize: string;
  /** The right group's cells, in order. */
  cells: CellMeasure[];
}

declare global {
  interface Window {
    topbarPage: TopbarPage;
  }
}

/** The page's host, created on first use. */
function host(): HTMLElement {
  let el = document.getElementById("host");
  if (el === null) {
    el = document.createElement("div");
    el.id = "host";
    document.body.append(el);
  }
  return el;
}

/**
 * The webapp's own logger, at DEBUG; its console line is kept for the test to
 * read, and the forwarding sink accepts and drops.
 */
const written: string[] = [];
setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    (level, line) => {
      const { operation } = JSON.parse(line) as { operation: string };
      written.push(`${level} ${operation}`);
    },
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-topbar-page" });

/** The context every drawn cell is handed; nothing here clicks, so no rpc is scripted. */
function topbarContext(): TopbarContext {
  return { ctx: appContext(), reveals: mountRevealLayer(host()), openLogin: () => undefined, localFailures: () => [] };
}

/** The strip's view: one of every right-hand cell, warnings included. */
function stripView(session: boolean) {
  return create(TopbarViewSchema, {
    title: { text: "DWC/fix" },
    sessionLine: { text: "session abc" },
    account: { state: { case: "loggedIn", value: { email: "dodge.w.coates@gmail.com" } } },
    connectivity: { tone: "green", glyph: "●", title: "connected" },
    ...(session
      ? {
          modelSelector: {
            options: [{ model: { name: "opus" }, displayName: "Opus 4.7" }],
            selected: { model: { name: "opus" }, displayName: "Opus 4.7" },
          },
          permissionModePicker: { current: { mode: "auto", displayName: "auto" }, options: [] },
        }
      : {}),
    context: { text: "142.3k", breakdown: { sections: [] } },
    warnings: { warnings: [{ line: { text: "something is wrong" } }] },
    persistentWifi: {
      wifi: { case: "joined", value: {} },
      mode: { case: "on", value: {} },
      tooltip: { text: "persistent wifi on" },
    },
  });
}

/**
 * The box a reader sees for one right-hand cell: the button inside a
 * control's wrap, or the cell itself when it holds none. Its CONTENT edges,
 * inside padding and border, are where the cell's text or glyph is drawn.
 */
function measureCell(cell: Element): CellMeasure {
  const box = cell.querySelector("ar-button") ?? cell;
  const r = box.getBoundingClientRect();
  const st = getComputedStyle(box);
  const px = (v: string): number => Number.parseFloat(v);
  return {
    cell: cell.className.split(" ")[0] ?? "",
    left: r.left + px(st.borderLeftWidth) + px(st.paddingLeft),
    right: r.right - px(st.borderRightWidth) - px(st.paddingRight),
    fontSize: st.fontSize,
  };
}

window.topbarPage = {
  records: () => [...written],
  drawStrip(session) {
    const header = document.createElement("header");
    header.id = "topbar";
    host().replaceChildren(header);
    const strip = document.createElement("div");
    strip.className = "topbar-strip";
    header.append(strip);
    const tc: TopbarContext = {
      ctx: appContext(),
      reveals: mountRevealLayer(header),
      openLogin: () => undefined,
      localFailures: () => [],
    };
    strip.append(drawTopbarView(stripView(session), tc));
    const account = header.querySelector(".topbar-account");
    const right = header.querySelector(".topbar-right");
    if (account === null || right === null) throw new Error("the strip drew no account or no right group");
    return {
      accountFontSize: getComputedStyle(account).fontSize,
      cells: [...right.children].map(measureCell),
    };
  },
  drawWifi(offset) {
    const chip = drawTopbarPersistentWifi(
      create(TopbarPersistentWifiSchema, {
        wifi: { case: "joined", value: {} },
        mode: { case: "on", value: {} },
        tooltip: { text: "persistent wifi on" },
      }),
      topbarContext(),
    );
    const at = document.createElement("div");
    at.style.paddingLeft = `${20 + offset}px`;
    at.style.paddingTop = "10px";
    at.append(chip);
    host().replaceChildren(at);
  },
};
