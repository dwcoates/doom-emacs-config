// @vitest-environment jsdom
/**
 * THE BOOT, under jsdom.
 *
 * `src/main.ts` is a composition root whose whole product is an ORDER: the
 * shell before the address, the address before the transport, the transport
 * before the logger, the logger before anything that can log, adoption before
 * any stream, the lifecycle before any view, and the mounts in index.html's
 * own order. None of that is observable from any other module, so this file
 * runs the real boot and asserts each step's observable outcome.
 *
 * WHAT IS SUBSTITUTED AND WHY. Only the two ends: the wire (`transport` and
 * `client`, because a unit run has no daemon) and the mounts (each component
 * has its own suite; what main.ts owns is WHICH host each one gets and WHEN).
 * `shell.ts`, `page-address.ts`, `workspace-ref.ts`, `context.ts`, `clock.ts`,
 * `log.ts`, the client-local failures and the topbar's MOUNT (its warning
 * chip) are the app's own throughout, so a failure listed here is the one the
 * browser would show. Only the topbar's `watch` -- its stream -- is recorded.
 *
 * WHY EVERY TEST RE-IMPORTS THE MODULE. `main.ts` boots itself at import time
 * -- that top-level `void boot()` IS the production entry point, and running
 * it is the point of this file -- so each test arranges its page and its
 * mocks, then imports a fresh copy through `vi.resetModules()`. Nothing in
 * `src/` is changed to make that possible.
 *
 * The unit run is un-isolated: this file installs no fake clock, unstubs every
 * global it stubs, and leaves the page empty.
 */
import { createControl } from "../src/control.js";
import { afterEach, beforeAll, beforeEach, describe, expect, test, vi, type Mock } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import type { SubmitPromptCommandPanel } from "../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";

/** One `ClientLog` call the installed logger forwarded. */
type ClientLogCall = { record: { message: string; context?: Record<string, unknown> } };

/**
 * EVERYTHING ONE BOOT RECORDS, AND ONLY THAT BOOT.
 *
 * A boot is not cancellable -- `main.ts` ends in `void boot()` -- and its
 * import can outlive the test that started it (a timed-out test's import keeps
 * running). So nothing a boot writes may land in state another test reads:
 * `bootMain` captures the record current when it starts, every substitution
 * it registers writes into THAT record, and every spy lives on it rather than
 * at file level. A straggler records into its own dead test's record and can
 * never reach the next one's `clientLogs` -- which is how a timed-out boot's
 * "the webapp booted" once failed the log-level test that ran after it.
 */
interface BootRecord {
  /** Every mount, in the order the boot called it. */
  order: string[];
  /** Whether the mocked `ClientLog` rejects every record. */
  clientLogFails: boolean;
  /** The `ClientLog` calls the installed logger forwarded. */
  clientLogs: ClientLogCall[];
  /** The rejection `main.ts` handed to `queueMicrotask` to re-raise. */
  rethrown: (() => void)[];
  /** What `adoptAtBoot` does when the boot reaches it. */
  adopt: () => Promise<void>;
  /** The composer gate the boot built, so the footer's status can be read off it. */
  gateSet: Mock;
  /** The footer's status subscriber, captured so a test can drive it. */
  onFooterStatus: ((statusCase: string) => void) | null;
  /** The dev-mode composer's panel callback, captured the same way. */
  onComposerPanel: ((panel: SubmitPromptCommandPanel) => void) | null;
  /** The topbar's `openLogin`, and the login handle's `open` it must reach. */
  openLogin: ((control: HTMLElement) => void) | null;
  loginOpen: Mock;
  /** The footer's `selectDetachedWork`, and the feed handle's it must reach. */
  footerSelectDetachedWork: ((id: unknown) => void) | null;
  feedSelectDetachedWork: Mock;
  /** The tray's `promptHeld`, and the feed handle's it must reach. */
  trayPromptHeld: ((turn: string) => void) | null;
  feedPromptHeld: Mock;
  /** The last `mountFeed` deps, for the composer-factory contract. */
  feedDeps: { composerFactory?: unknown } | null;
  /** The context the boot built, so its stream's fate can be read off it. */
  bootedContext: import("../src/rpc/context.js").AppContext | null;
  /** The element `drawCommandPanel` hands back. */
  drawnPanel: HTMLElement | null;
  /** Each component's mount, recorded. */
  mounts: {
    sidebar: Mock;
    topbar: Mock;
    feed: Mock;
    holdTray: Mock;
    footer: Mock;
    composer: Mock;
    login: Mock;
    newsDigest: Mock;
  };
}

function freshRecord(): BootRecord {
  return {
    order: [],
    clientLogFails: false,
    clientLogs: [],
    rethrown: [],
    adopt: async () => {},
    gateSet: vi.fn(),
    onFooterStatus: null,
    onComposerPanel: null,
    openLogin: null,
    loginOpen: vi.fn(),
    footerSelectDetachedWork: null,
    feedSelectDetachedWork: vi.fn(),
    trayPromptHeld: null,
    feedPromptHeld: vi.fn(),
    feedDeps: null,
    bootedContext: null,
    drawnPanel: null,
    mounts: {
      sidebar: vi.fn(),
      topbar: vi.fn(),
      feed: vi.fn(),
      holdTray: vi.fn(),
      footer: vi.fn(),
      composer: vi.fn(),
      login: vi.fn(),
      newsDigest: vi.fn(),
    },
  };
}

/** The current test's record: arranged by the test, written by its boot. */
let page: BootRecord = freshRecord();
/** `console.error`, for the documented pre-logger emergency path. */
let consoleError: ReturnType<typeof vi.spyOn>;
/**
 * The boot the current test started, until it has finished booting (or
 * failing). `afterEach` awaits it, so no boot outlives its own test into the
 * next one's globals (`queueMicrotask`, `crypto`, the page, the address).
 */
let inFlight: Promise<void> | null = null;

/** The real index.html body, so the shell resolved here is the shipped one. */
function installShell(): void {
  const html = readFileSync(resolve(process.cwd(), "index.html"), "utf8");
  const body = /<body[^>]*>([\s\S]*)<\/body>/i.exec(html);
  if (!body) throw new Error("index.html has no <body>: main.test.ts cannot boot the real shell");
  // The module script tag goes: this file imports main.ts itself, and jsdom
  // would otherwise try to fetch the bundle over http.
  document.body.innerHTML = body[1].replace(/<script[\s\S]*?<\/script>/gi, "");
}

/** Point the page at a workspace (or, with "", at nothing). */
function addressPage(search: string): void {
  window.history.replaceState({}, "", `/${search}`);
}

/**
 * Register every substitution, then run the boot by importing the entry.
 *
 * The boot is recorded in `inFlight` BEFORE it is awaited, so a test that
 * times out while awaiting it still leaves `afterEach` a handle to wait on.
 */
function bootMain(): Promise<void> {
  const boot = runBoot(page);
  inFlight = boot;
  return boot;
}

async function runBoot(rec: BootRecord): Promise<void> {
  vi.resetModules();
  // The SAME log module instance main.ts is about to configure: the static
  // import in this file belongs to the pre-reset registry and would never see
  // the logger the boot installs.
  const logging = await import("../src/log.js");
  const freshLog = logging.log;
  // test/setup.ts installed a logger into the PRE-RESET module instance, and
  // the canonical `log` methods throw without one -- the fresh graph gets the same quiet default
  // so the boot's own first `shell.resolve` record has somewhere to go. The
  // boot replaces it with the ClientLog-forwarding one, which is the only
  // logger that reaches the mocked client.
  logging.resetLoggingForTests();
  logging.setLogger(new logging.ForwardingLogger(async () => "accepted", () => {}));
  logging.bindLogContext({ connection_id: "test-connection" });

  // THE REAL CONTEXT, CAPTURED. `createAppContext` opens the page's one stream,
  // so whether a failed boot stops dialing is a fact about the object it
  // returns — not something a stub could answer.
  vi.doMock("../src/rpc/context.js", async () => {
    const actual = await vi.importActual<typeof import("../src/rpc/context.js")>(
      "../src/rpc/context.js",
    );
    return {
      ...actual,
      createAppContext: (init: Parameters<typeof actual.createAppContext>[0]) => {
        rec.bootedContext = actual.createAppContext(init);
        return rec.bootedContext;
      },
    };
  });
  vi.doMock("../src/rpc/transport.js", () => ({
    createDaemonTransport: vi.fn(() => ({ transport: true })),
  }));
  vi.doMock("../src/rpc/client.js", () => ({
    createAgentReplClient: vi.fn(() => ({
      clientLog: (request: ClientLogCall) => {
        rec.clientLogs.push(request);
        // A SINK THAT REJECTS is how the daemon being unreachable reaches the
        // logger, and `clientLogSink` is the one place that failure becomes
        // something the footer can draw.
        return rec.clientLogFails
          ? Promise.reject(new Error("no route to the daemon"))
          : Promise.resolve({});
      },
      watchPage: async function* (_request: unknown, options: { signal: AbortSignal }) {
        await new Promise<void>((resolve) => {
          options.signal.addEventListener("abort", () => resolve(), { once: true });
        });
      },
    })),
  }));
  vi.doMock("../src/lifecycle/lifecycle.js", () => ({
    adoptAtBoot: vi.fn(async () => {
      rec.order.push("adopt");
      await rec.adopt();
    }),
    startLifecycle: vi.fn(() => {
      rec.order.push("lifecycle");
      return { dispose: vi.fn() };
    }),
  }));
  vi.doMock("../src/sidebar/sidebar.js", () => ({
    mountSidebar: rec.mounts.sidebar.mockImplementation((host: HTMLElement) => {
      rec.order.push("sidebar");
      // The logger must already be forwarding by the first mount: a component
      // that logs during its own mount is the case this proves.
      freshLog.error("the sidebar mounted", { operation: "main.test.sidebar-mounted" });
      return { dispose: vi.fn(), host };
    }),
  }));
  vi.doMock("../src/topbar/topbar.js", async () => {
    // THE REAL MOUNT, A RECORDED WATCH. Mounting is what draws the warning
    // chip, and the chip is where a failed boot shows; the stream is a
    // component suite's business, so only WHEN it starts is recorded here.
    const actual = await vi.importActual<typeof import("../src/topbar/topbar.js")>(
      "../src/topbar/topbar.js",
    );
    return {
      ...actual,
      mountTopbar: rec.mounts.topbar.mockImplementation(
        (host: HTMLElement, deps: Parameters<typeof actual.mountTopbar>[1]) => {
          rec.order.push("topbar-mount");
          const handle = actual.mountTopbar(host, deps);
          return {
            ...handle,
            watch: (_ctx: unknown, watchDeps: { openLogin: (control: HTMLElement) => void }) => {
              rec.order.push("topbar");
              rec.openLogin = watchDeps.openLogin;
            },
          };
        },
      ),
    };
  });
  vi.doMock("../src/feed/feed.js", () => ({
    mountFeed: rec.mounts.feed.mockImplementation(
      (_host: HTMLElement, _ctx, deps: BootRecord["feedDeps"]) => {
        rec.order.push("feed");
        rec.feedDeps = deps;
        return {
          dispose: vi.fn(),
          selectDetachedWork: rec.feedSelectDetachedWork,
          promptHeld: rec.feedPromptHeld,
        };
      },
    ),
  }));
  vi.doMock("../src/feed/renderers.js", () => ({
    createRowRenderers: vi.fn(() => ({})),
  }));
  vi.doMock("../src/tray/tray.js", () => ({
    mountHoldTray: rec.mounts.holdTray.mockImplementation(
      (_host: HTMLElement, _ctx, deps: { promptHeld: (turn: string) => void }) => {
        rec.order.push("holdTray");
        rec.trayPromptHeld = deps.promptHeld;
        return { dispose: vi.fn() };
      },
    ),
  }));
  vi.doMock("../src/footer/footer.js", () => ({
    mountFooter: rec.mounts.footer.mockImplementation(
      (_host: HTMLElement, _ctx, deps: { selectDetachedWork: (id: unknown) => void }) => {
        rec.order.push("footer");
        rec.footerSelectDetachedWork = deps.selectDetachedWork;
        return {
          dispose: vi.fn(),
          onStatus: (fn: (statusCase: string) => void) => {
            rec.onFooterStatus = fn;
          },
        };
      },
    ),
  }));
  vi.doMock("../src/composer/composer.js", () => ({
    createComposerGate: vi.fn(() => ({ set: rec.gateSet, state: () => "open" })),
    mountComposer: rec.mounts.composer.mockImplementation(
      (_host: HTMLElement, _ctx, opts: { onPanel?: (panel: SubmitPromptCommandPanel) => void }) => {
        rec.order.push("composer");
        if (opts.onPanel !== undefined) rec.onComposerPanel = opts.onPanel;
        return { dispose: vi.fn() };
      },
    ),
  }));
  vi.doMock("../src/login/login.js", () => ({
    mountLoginOverlay: rec.mounts.login.mockImplementation(() => {
      rec.order.push("login");
      return { dispose: vi.fn(), open: rec.loginOpen };
    }),
  }));
  vi.doMock("../src/news-digest/news-digest.js", () => ({
    mountNewsDigest: rec.mounts.newsDigest.mockImplementation(() => {
      rec.order.push("newsDigest");
      return { dispose: vi.fn(), apply: vi.fn() };
    }),
  }));
  vi.doMock("../src/panels/panels.js", () => ({
    drawCommandPanel: vi.fn(() => {
      const el = document.createElement("div");
      el.setAttribute("data-panel", "drawn");
      rec.drawnPanel = el;
      return el;
    }),
  }));

  await import("../src/main.js");
  // The boot's one asynchronous boundary is `await adoptAtBoot`. A macrotask
  // turn runs after every microtask the resumed boot queues, so the page is
  // finished booting -- or finished failing -- when this returns.
  await new Promise<void>((r) => setTimeout(r, 0));
  await new Promise<void>((r) => setTimeout(r, 0));
}

/** Arrange a fresh page: a fresh record, the shipped shell, a workspace address. */
function arrangePage(): void {
  page = freshRecord();
  consoleError = vi.spyOn(console, "error").mockImplementation(() => {});
  // CAPTURED, NOT RUN. `main.ts` re-raises a boot rejection out of a
  // microtask so the browser reports it uncaught; under vitest that would
  // fail the file rather than be asserted on, so the callback is held and the
  // test invokes it to prove the throw is still the same one. Bound to THIS
  // record: a straggling boot could only ever re-raise into its own.
  const rec = page;
  vi.stubGlobal("queueMicrotask", (fn: () => void) => {
    rec.rethrown.push(fn);
  });
  installShell();
  addressPage("?workspace=ws-1&dir=/tmp/ws-1");
}

/**
 * Wait out the test's boot, then take the page down.
 *
 * AWAITED FIRST, because a test that timed out left its boot running: its
 * context, its mounts and its re-raise would otherwise land after the globals
 * below were restored, in whatever test came next. Only once no boot is in
 * flight is the one it opened stopped.
 */
async function teardownPage(): Promise<void> {
  const boot = inFlight;
  inFlight = null;
  try {
    await boot;
  } finally {
    // A successful boot owns one standing page stream. Stop it at the same
    // boundary production uses so no retry or iterator survives into the next
    // fresh module graph.
    page.bootedContext?.quiesce();
    consoleError.mockRestore();
    vi.unstubAllGlobals();
    vi.resetModules();
    document.body.replaceChildren();
    addressPage("");
  }
}

/**
 * THE FILE'S COLD BOOT IS PAID ONCE, IN A `beforeAll`, UNDER ITS OWN BOUND.
 *
 * The first `import("../src/main.js")` in a worker is the first time the
 * composition root's graph is fetched and compiled: vite-node pulls each module
 * over an rpc to the main vitest process, one `await` per import, and V8
 * compiles each on first evaluation. PROFILED (a `--cpu-prof` of both
 * processes): the boot itself settles in ~2ms and neither awaits anything
 * serially nor arms a real timer; the cold cost is the worker IDLE on those
 * module-fetch round trips, plus protobuf-es decoding the generated
 * descriptors and jsdom parsing the imported stylesheet. Every later import in
 * the file re-evaluates the same graph from vite-node's transform cache. So
 * the cold import is ~10x a warm one, and at load it is the round trips that
 * stretch: MEASURED, cold 0.65-1.3s idle, up to 3.15s over 20 runs at a load
 * average of ~112 (`yes` x16 plus two looping unit suites), and 5.59s once on
 * the contended host; warm 55-250ms idle, up to 536ms at ~112 and 1.45s on
 * that same contended run.
 *
 * It used to land on whichever test ran first, and at a load average of ~110
 * it crossed that test's bound. 15s is ~2.7x the slowest cold import seen.
 */
const COLD_BOOT_TIMEOUT_MS = 15_000;

/**
 * ONE WARM BOOT: a fresh evaluation of the whole mocked graph plus its settle.
 * ~3x the 1.45s slowest warm test measured above. It is a per-site bound
 * rather than a raised global because nothing else in the unit suite imports
 * its subject at run time; the 850ms global stays sized for what it covers.
 */
const BOOT_TIMEOUT_MS = 4_500;
beforeAll(async () => {
  arrangePage();
  try {
    await bootMain();
  } finally {
    await teardownPage();
  }
}, COLD_BOOT_TIMEOUT_MS);

beforeEach(arrangePage);
afterEach(teardownPage, BOOT_TIMEOUT_MS);
describe("the boot", { timeout: BOOT_TIMEOUT_MS }, () => {
  test("mounts every component on the shell element that names it", async () => {
    await bootMain();

    expect(page.mounts.sidebar.mock.calls[0]?.[0]).toBe(document.getElementById("ws-sidebar"));
    expect(page.mounts.topbar.mock.calls[0]?.[0]).toBe(document.getElementById("topbar"));
    expect(page.mounts.feed.mock.calls[0]?.[0]).toBe(document.getElementById("feed"));
    expect(page.mounts.holdTray.mock.calls[0]?.[0]).toBe(document.getElementById("hold-tray"));
    expect(page.mounts.footer.mock.calls[0]?.[0]).toBe(document.getElementById("footer"));
    expect(page.mounts.login.mock.calls[0]?.[0]).toBe(document.getElementById("login-overlay"));
    expect(page.mounts.newsDigest.mock.calls[0]?.[0]).toBe(document.getElementById("news-digest"));
  });

  test("mounts the news digest overlay before the lifecycle whose stream feeds it", async () => {
    await bootMain();

    expect(page.order.indexOf("newsDigest")).toBeLessThan(page.order.indexOf("lifecycle"));
  });

  test("has the ClientLog-forwarding logger installed before the first mount", async () => {
    await bootMain();

    expect(page.clientLogs.map((call) => call.record.message)).toContain("the sidebar mounted");
  });

  test("configures the logger from the page-delivered log level", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&log_level=error");

    await bootMain();

    expect(page.clientLogs.map((call) => call.record.message)).not.toContain("the webapp booted");
    expect(page.clientLogs.map((call) => call.record.message)).toContain("the sidebar mounted");
  });

  test("adopts the workspace before it starts the lifecycle", async () => {
    await bootMain();

    expect(page.order.indexOf("adopt")).toBeLessThan(page.order.indexOf("lifecycle"));
  });

  test("starts the lifecycle before it mounts any view", async () => {
    await bootMain();

    expect(page.order.indexOf("lifecycle")).toBeLessThan(page.order.indexOf("sidebar"));
  });

  test("mounts the login overlay before the topbar that opens it starts watching", async () => {
    await bootMain();

    expect(page.order.indexOf("login")).toBeLessThan(page.order.indexOf("topbar"));
  });

  test("mounts the topbar before adoption, so its warning chip can show a failed boot", async () => {
    await bootMain();

    expect(page.order.indexOf("topbar-mount")).toBeLessThan(page.order.indexOf("adopt"));
  });

  test("starts the topbar's stream only after adoption", async () => {
    await bootMain();

    expect(page.order.indexOf("adopt")).toBeLessThan(page.order.indexOf("topbar"));
  });

  test("mounts the feed before the footer whose jump rows reveal its rows", async () => {
    await bootMain();

    expect(page.order.indexOf("feed")).toBeLessThan(page.order.indexOf("footer"));
  });

  test("reveals and mounts the composer when the address asks for one", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");

    await bootMain();

    expect(document.getElementById("composer")?.hidden).toBe(false);
    expect(page.mounts.composer.mock.calls[0]?.[0]).toBe(document.getElementById("composer"));
  });

  test("leaves the composer hidden and unmounted in production", async () => {
    await bootMain();

    expect(document.getElementById("composer")?.hidden).toBe(true);
    expect(page.mounts.composer).not.toHaveBeenCalled();
  });

  test("gives the feed a per-bubble composer factory only in dev mode", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");

    await bootMain();

    expect(page.feedDeps?.composerFactory).toBeTypeOf("function");
  });

  test("routes the topbar's account control to the login overlay it mounted", async () => {
    await bootMain();
    const control = createControl();

    page.openLogin?.(control);

    expect(page.loginOpen).toHaveBeenCalledWith(control);
  });

  test("routes the footer's jump row to the feed it mounted", async () => {
    await bootMain();

    page.footerSelectDetachedWork?.({ value: "row-1" });

    expect(page.feedSelectDetachedWork).toHaveBeenCalledWith({ value: "row-1" });
  });

  test("routes the hold tray's first-drawn held prompt to the feed it mounted", async () => {
    await bootMain();

    page.trayPromptHeld?.("turn-1");

    expect(page.feedPromptHeld).toHaveBeenCalledWith("turn-1");
  });

  test("mints a page identity without crypto.randomUUID", async () => {
    // An old webview or a non-secure origin exposes no `crypto.randomUUID`,
    // and an unidentifiable page's records are still worth more than a page
    // that refuses to boot over its own identifier.
    vi.stubGlobal("crypto", {});

    await bootMain();

    const booted = page.clientLogs.find((call) => call.record.message === "the webapp booted");
    expect(booted?.record.context?.connection_id).toMatch(/^web-/);
  });

  test("builds a per-bubble composer on the host the feed hands it", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");
    await bootMain();
    const bubbleHost = document.createElement("div");

    (page.feedDeps?.composerFactory as (host: HTMLElement, feed: unknown) => unknown)(bubbleHost, {
      value: "row-1",
    });

    expect(page.mounts.composer).toHaveBeenCalledWith(bubbleHost, expect.anything(), expect.anything());
  });

  test("gives the feed no per-bubble composer factory in production", async () => {
    await bootMain();

    expect(page.feedDeps?.composerFactory).toBeUndefined();
  });

  // A MERGE IN FLIGHT HOLDS WHAT IS SUBMITTED (owner ruling, 2026-10-01), so
  // its composer stays open and the prompt waits in the tray for the merge.
  test("keeps the gate open while the workspace is merging", async () => {
    await bootMain();

    page.onFooterStatus?.("merging");

    expect(page.gateSet).toHaveBeenCalledWith("open", undefined);
  });

  // A STOPPED MERGE NO LONGER HOLDS THE SESSION (owner ruling, 2026-09-28): a
  // failed or landed merge is over, so neither may close the composer the way
  // a merge in flight does.
  test.each([["mergeFailed"], ["merged"]])(
    "keeps the gate open when the footer reports the stopped merge's %s arm",
    async (arm) => {
      await bootMain();

      page.onFooterStatus?.(arm);

      expect(page.gateSet).toHaveBeenCalledWith("open", undefined);
    },
  );

  // THE COMPOSER INVARIANT (owner rulings, 2026-09-28, 2026-10-01 and
  // 2026-10-02): the gate is closed exactly when the footer's color is blue
  // (an agent-repl or network fault, a close); purple, a merge in flight, and
  // turquoise, a vendor fault, hold their prompts and leave it open. One row per arm
  // the rulings place on either side of the line.
  test.each([
    ["agentReplFault", "closed"],
    ["networkFault", "closed"],
    ["closing", "closed"],
    ["vendorFault", "open"],
    ["merging", "open"],
    ["turnFailed", "open"],
    ["degraded", "open"],
    ["idle", "open"],
    ["working", "open"],
  ])("sets the gate %s -> %s by the footer's color", async (arm, want) => {
    await bootMain();

    page.onFooterStatus?.(arm);

    expect(page.gateSet).toHaveBeenCalledWith(want, want === "closed" ? arm : undefined);
  });

  test("draws a dev-mode command panel beside the composer that asked for it", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");
    await bootMain();

    page.onComposerPanel?.({} as SubmitPromptCommandPanel);

    expect(page.drawnPanel?.parentElement).toBe(document.getElementById("composer"));
  });

  test("replaces a stale panel rather than stacking a second one", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");
    await bootMain();
    const stale = document.createElement("div");
    stale.setAttribute("data-panel", "stale");
    document.getElementById("composer")?.append(stale);

    page.onComposerPanel?.({} as SubmitPromptCommandPanel);

    expect(stale.parentElement).toBeNull();
    expect(document.querySelectorAll("#composer > [data-panel]")).toHaveLength(1);
  });
});

/** The evidence lines the topbar's warning chip shows for the client-local ARM. */
function chipEvidence(arm: string): string[] {
  document.querySelector<HTMLElement>("#topbar .topbar-warning-chip")?.click();
  document
    .querySelector<HTMLElement>(`#topbar [data-reveal] [data-local][data-arm="${arm}"]`)
    ?.click();
  return [...document.querySelectorAll("#topbar [data-reveal] .topbar-warning-detail-line")].map(
    (line) => line.textContent ?? "",
  );
}

describe("a boot that fails", { timeout: BOOT_TIMEOUT_MS }, () => {
  test("files boot_failed in the topbar's warning chip when adoption never completes", async () => {
    page.adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(chipEvidence("bootFailed")).toEqual(["cause: Error: adoption refused"]);
  });

  test("draws no failure overlay for a failed boot", async () => {
    page.adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(document.querySelector("#failure-overlay, .failure-card")).toBeNull();
  });

  test("re-raises the adoption failure after it has been drawn", async () => {
    page.adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(page.rethrown).toHaveLength(1);
    expect(() => page.rethrown[0]?.()).toThrow("adoption refused");
  });

  test("stops the page's one stream when the boot fails", async () => {
    // THE PAGE STREAM IS OPENED SEVERAL STEPS ABOVE ADOPTION, so a boot that
    // fails at adoption used to leave it reopening on backoff forever against
    // a daemon the page had already given up on — a dead page still holding a
    // connection, burying its own `boot_failed` card under a reopen loop's
    // error records. Caught as a timeout in the isolated coverage run.
    page.adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(page.bootedContext?.isQuiesced()).toBe(true);
  });

  test("leaves the page's one stream running when the boot succeeds", async () => {
    // The other side of it: quiescing is the FAILURE path's act, and a healthy
    // page that stopped dialing would draw nothing ever again.
    await bootMain();

    expect(page.bootedContext?.isQuiesced()).toBe(false);
  });

  test("words a non-Error rejection in the chip as the value itself", async () => {
    page.adopt = () => Promise.reject("the daemon hung up");

    await bootMain();

    expect(chipEvidence("bootFailed")).toEqual(["cause: the daemon hung up"]);
  });

  test("re-raises the shell's missing mount point by name", async () => {
    document.getElementById("footer")?.remove();

    await bootMain();

    expect(() => page.rethrown[0]?.()).toThrow("the page shell is missing #footer");
  });

  test("reports an address failure through the pre-overlay emergency path", async () => {
    addressPage("?dir=/tmp/ws-1");

    await bootMain();

    expect(consoleError).toHaveBeenCalledWith(
      expect.stringContaining("the webapp failed to boot before it could report anything"),
    );
  });

  test("draws no warning chip for an address failure, having no topbar yet", async () => {
    addressPage("?dir=/tmp/ws-1");

    await bootMain();

    expect(document.querySelector("#topbar .topbar-warnings")).toBeNull();
  });
});

describe("the ClientLog sink and the client's link verdict", { timeout: BOOT_TIMEOUT_MS }, () => {
  test("reports a ClientLog that could not be forwarded", async () => {
    // ARRANGE: the daemon refuses every record this page tries to file.
    page.clientLogFails = true;

    // ACT
    await bootMain();

    // ASSERT: read out of the BOOT'S OWN module graph, which `vi.resetModules`
    // made a fresh one -- the statically imported copy is another page.
    const link = await import("../src/rpc/link.js");
    expect(link.standingClientFailure()).toEqual({
      kind: "client_log_failed",
      substatus: "daemon unreachable",
      activity: "ClientLog forwarding failed",
    });
    link.clearClientFailures();
  });

  test("reports nothing while the sink is accepting records", async () => {
    await bootMain();

    const link = await import("../src/rpc/link.js");
    expect(link.standingClientFailure()).toBeNull();
  });
});

describe("the harness", { timeout: BOOT_TIMEOUT_MS }, () => {
  test("a boot that outlives its test records into its own record, not the next test's", async () => {
    // ARRANGE: a boot held at adoption, the way a timed-out test's boot is
    // still running when the next test arranges its page.
    const straggler = page;
    let reachedAdoption: () => void = () => {};
    const atAdoption = new Promise<void>((resolve) => {
      reachedAdoption = resolve;
    });
    let releaseAdoption: () => void = () => {};
    straggler.adopt = () => {
      reachedAdoption();
      return new Promise<void>((resolve) => {
        releaseAdoption = resolve;
      });
    };
    const boot = bootMain();
    await atAdoption;
    page = freshRecord();

    // ACT: the straggler resumes and mounts everything after adoption.
    releaseAdoption();
    await boot;
    straggler.bootedContext?.quiesce();

    // ASSERT
    expect(straggler.clientLogs.map((call) => call.record.message)).toContain("the sidebar mounted");
    expect(page.clientLogs).toEqual([]);
    expect(page.order).toEqual([]);
  });

  test("every block in this file runs its tests under the warm-boot bound", () => {
    // EVERY TEST HERE BOOTS, so every block carries BOOT_TIMEOUT_MS. One block
    // that left it off ran its warm boots under the 850ms unit global, which
    // is under the 1.45s warm boot measured on the contended host, and timed
    // out only when the full suite set loaded the machine.
    const source = readFileSync(resolve(process.cwd(), "test/main.test.ts"), "utf8");
    const blocks = source.split("\n").filter((line) => line.startsWith("describe("));

    expect(blocks.filter((line) => !line.includes("{ timeout: BOOT_TIMEOUT_MS }"))).toEqual([]);
  });
});
