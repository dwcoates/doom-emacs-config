/**
 * THE PAGE THE WEBKIT TOPBAR TEST DRIVES (topbar.webkit.test.ts).
 *
 * Bundled by that test into one script and run in headless WebKit over the
 * REAL stylesheet, drawing the REAL topbar cells, so what is measured is what
 * the webview paints rather than markup a test wrote to look like it.
 */
import { create } from "@bufbuild/protobuf";
import { TopbarPersistentWifiSchema } from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import type { TopbarContext } from "../../src/topbar/context.js";
import { drawTopbarPersistentWifi } from "../../src/topbar/persistent-wifi.js";
import { mountRevealLayer } from "../../src/topbar/reveal.js";
import { appContext } from "../topbar/fixtures.js";

/** What the test calls, on `window.topbarPage`. */
export interface TopbarPage {
  /**
   * Draw the persistent-wifi chip, joined and with the mode on, OFFSET css px
   * right of a whole-pixel edge, so the chip lands at a fractional position.
   */
  drawWifi(offset: number): void;
  /** Every record the page wrote, as `level operation`. */
  records(): string[];
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

window.topbarPage = {
  records: () => [...written],
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
