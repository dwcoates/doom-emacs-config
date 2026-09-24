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
import { afterEach, beforeEach, describe, expect, test, vi } from "vitest";
import { readFileSync } from "node:fs";
import { resolve } from "node:path";

import type { SubmitPromptCommandPanel } from "../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";

/** Every mount, in the order the boot called it. */
let order: string[] = [];
/** Whether the mocked `ClientLog` rejects every record. */
let clientLogFails = false;
/** The `ClientLog` calls the installed logger forwarded. */
let clientLogs: { record: { message: string; context?: Record<string, unknown> } }[] = [];
/** The rejection `main.ts` handed to `queueMicrotask` to re-raise. */
let rethrown: (() => void)[] = [];
/** `console.error`, for the documented pre-logger emergency path. */
let consoleError: ReturnType<typeof vi.spyOn>;

/** What `adoptAtBoot` does when the boot reaches it. */
let adopt: () => Promise<void> = async () => {};
/** The composer gate the boot built, so the footer's status can be read off it. */
const gateSet = vi.fn();
/** The footer's status subscriber, captured so a test can drive it. */
let onFooterStatus: ((statusCase: string) => void) | null = null;
/** The dev-mode composer's panel callback, captured the same way. */
let onComposerPanel: ((panel: SubmitPromptCommandPanel) => void) | null = null;
/** The topbar's `openLogin`, and the login handle's `open` it must reach. */
let openLogin: ((control: HTMLElement) => void) | null = null;
const loginOpen = vi.fn();
/** The footer's `selectDetachedWork`, and the feed handle's it must reach. */
let footerSelectDetachedWork: ((id: unknown) => void) | null = null;
const feedSelectDetachedWork = vi.fn();
/** The tray's `promptHeld`, and the feed handle's it must reach. */
let trayPromptHeld: ((turn: string) => void) | null = null;
const feedPromptHeld = vi.fn();
/** The last `mountFeed` deps, for the composer-factory contract. */
let feedDeps: { composerFactory?: unknown } | null = null;
/** The context the boot built, so its stream's fate can be read off it. */
let bootedContext: import("../src/rpc/context.js").AppContext | null = null;
/** `log` out of the freshly-imported graph -- the instance main.ts installs into. */
let freshLog: typeof import("../src/log.js").log;
/** The element `drawCommandPanel` hands back. */
let drawnPanel: HTMLElement | null = null;

const mounts = {
  sidebar: vi.fn(),
  topbar: vi.fn(),
  feed: vi.fn(),
  holdTray: vi.fn(),
  footer: vi.fn(),
  composer: vi.fn(),
  login: vi.fn(),
};

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

/** Register every substitution, then run the boot by importing the entry. */
async function bootMain(): Promise<void> {
  vi.resetModules();
  // The SAME log module instance main.ts is about to configure: the static
  // import in this file belongs to the pre-reset registry and would never see
  // the logger the boot installs.
  const logging = await import("../src/log.js");
  freshLog = logging.log;
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
        bootedContext = actual.createAppContext(init);
        return bootedContext;
      },
    };
  });
  vi.doMock("../src/rpc/transport.js", () => ({
    createDaemonTransport: vi.fn(() => ({ transport: true })),
  }));
  vi.doMock("../src/rpc/client.js", () => ({
    createAgentReplClient: vi.fn(() => ({
      clientLog: (request: { record: { message: string; context?: Record<string, unknown> } }) => {
        clientLogs.push(request);
        // A SINK THAT REJECTS is how the daemon being unreachable reaches the
        // logger, and `clientLogSink` is the one place that failure becomes
        // something the footer can draw.
        return clientLogFails
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
      order.push("adopt");
      await adopt();
    }),
    startLifecycle: vi.fn(() => {
      order.push("lifecycle");
      return { dispose: vi.fn() };
    }),
  }));
  vi.doMock("../src/sidebar/sidebar.js", () => ({
    mountSidebar: mounts.sidebar.mockImplementation((host: HTMLElement) => {
      order.push("sidebar");
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
      mountTopbar: mounts.topbar.mockImplementation(
        (host: HTMLElement, deps: Parameters<typeof actual.mountTopbar>[1]) => {
          order.push("topbar-mount");
          const handle = actual.mountTopbar(host, deps);
          return {
            ...handle,
            watch: (_ctx: unknown, watchDeps: { openLogin: (control: HTMLElement) => void }) => {
              order.push("topbar");
              openLogin = watchDeps.openLogin;
            },
          };
        },
      ),
    };
  });
  vi.doMock("../src/feed/feed.js", () => ({
    mountFeed: mounts.feed.mockImplementation((_host: HTMLElement, _ctx, deps: typeof feedDeps) => {
      order.push("feed");
      feedDeps = deps;
      return { dispose: vi.fn(), selectDetachedWork: feedSelectDetachedWork, promptHeld: feedPromptHeld };
    }),
  }));
  vi.doMock("../src/feed/renderers.js", () => ({
    createRowRenderers: vi.fn(() => ({})),
  }));
  vi.doMock("../src/tray/tray.js", () => ({
    mountHoldTray: mounts.holdTray.mockImplementation(
      (_host: HTMLElement, _ctx, deps: { promptHeld: (turn: string) => void }) => {
        order.push("holdTray");
        trayPromptHeld = deps.promptHeld;
        return { dispose: vi.fn() };
      },
    ),
  }));
  vi.doMock("../src/footer/footer.js", () => ({
    mountFooter: mounts.footer.mockImplementation(
      (_host: HTMLElement, _ctx, deps: { selectDetachedWork: (id: unknown) => void }) => {
        order.push("footer");
        footerSelectDetachedWork = deps.selectDetachedWork;
        return {
          dispose: vi.fn(),
          onStatus: (fn: (statusCase: string) => void) => {
            onFooterStatus = fn;
          },
        };
      },
    ),
  }));
  vi.doMock("../src/composer/composer.js", () => ({
    createComposerGate: vi.fn(() => ({ set: gateSet, state: () => "open" })),
    mountComposer: mounts.composer.mockImplementation(
      (_host: HTMLElement, _ctx, opts: { onPanel?: (panel: SubmitPromptCommandPanel) => void }) => {
        order.push("composer");
        if (opts.onPanel !== undefined) onComposerPanel = opts.onPanel;
        return { dispose: vi.fn() };
      },
    ),
  }));
  vi.doMock("../src/login/login.js", () => ({
    mountLoginOverlay: mounts.login.mockImplementation(() => {
      order.push("login");
      return { dispose: vi.fn(), open: loginOpen };
    }),
  }));
  vi.doMock("../src/panels/panels.js", () => ({
    drawCommandPanel: vi.fn(() => {
      const el = document.createElement("div");
      el.setAttribute("data-panel", "drawn");
      drawnPanel = el;
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

beforeEach(() => {
  order = [];
  bootedContext = null;
  clientLogs = [];
  clientLogFails = false;
  rethrown = [];
  adopt = async () => {};
  onFooterStatus = null;
  openLogin = null;
  footerSelectDetachedWork = null;
  trayPromptHeld = null;
  loginOpen.mockClear();
  feedSelectDetachedWork.mockClear();
  feedPromptHeld.mockClear();
  onComposerPanel = null;
  feedDeps = null;
  drawnPanel = null;
  gateSet.mockClear();
  for (const fn of Object.values(mounts)) fn.mockClear();
  consoleError = vi.spyOn(console, "error").mockImplementation(() => {});
  // CAPTURED, NOT RUN. `main.ts` re-raises a boot rejection out of a
  // microtask so the browser reports it uncaught; under vitest that would
  // fail the file rather than be asserted on, so the callback is held and the
  // test invokes it to prove the throw is still the same one.
  vi.stubGlobal("queueMicrotask", (fn: () => void) => {
    rethrown.push(fn);
  });
  installShell();
  addressPage("?workspace=ws-1&dir=/tmp/ws-1");
});

afterEach(() => {
  // A successful boot owns one standing page stream. Stop it at the same
  // boundary production uses so no retry or iterator survives into the next
  // fresh module graph.
  bootedContext?.quiesce();
  consoleError.mockRestore();
  vi.unstubAllGlobals();
  vi.resetModules();
  document.body.replaceChildren();
  addressPage("");
});

// These full-graph boot cases repeatedly reached 1.5s under the isolated
// Istanbul coverage workers while completing normally. Their five-second
// local bound keeps the 850ms unit-test default intact and still detects a
// boot that stops making progress.
const coverageBootTimeoutMS = 5_000;

describe("the boot", { timeout: coverageBootTimeoutMS }, () => {
  test("mounts every component on the shell element that names it", async () => {
    await bootMain();

    expect(mounts.sidebar.mock.calls[0]?.[0]).toBe(document.getElementById("ws-sidebar"));
    expect(mounts.topbar.mock.calls[0]?.[0]).toBe(document.getElementById("topbar"));
    expect(mounts.feed.mock.calls[0]?.[0]).toBe(document.getElementById("feed"));
    expect(mounts.holdTray.mock.calls[0]?.[0]).toBe(document.getElementById("hold-tray"));
    expect(mounts.footer.mock.calls[0]?.[0]).toBe(document.getElementById("footer"));
    expect(mounts.login.mock.calls[0]?.[0]).toBe(document.getElementById("login-overlay"));
  });

  test("has the ClientLog-forwarding logger installed before the first mount", async () => {
    await bootMain();

    expect(clientLogs.map((call) => call.record.message)).toContain("the sidebar mounted");
  });

  test("configures the logger from the page-delivered log level", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&log_level=error");

    await bootMain();

    expect(clientLogs.map((call) => call.record.message)).not.toContain("the webapp booted");
    expect(clientLogs.map((call) => call.record.message)).toContain("the sidebar mounted");
  });

  test("adopts the workspace before it starts the lifecycle", async () => {
    await bootMain();

    expect(order.indexOf("adopt")).toBeLessThan(order.indexOf("lifecycle"));
  });

  test("starts the lifecycle before it mounts any view", async () => {
    await bootMain();

    expect(order.indexOf("lifecycle")).toBeLessThan(order.indexOf("sidebar"));
  });

  test("mounts the login overlay before the topbar that opens it starts watching", async () => {
    await bootMain();

    expect(order.indexOf("login")).toBeLessThan(order.indexOf("topbar"));
  });

  test("mounts the topbar before adoption, so its warning chip can show a failed boot", async () => {
    await bootMain();

    expect(order.indexOf("topbar-mount")).toBeLessThan(order.indexOf("adopt"));
  });

  test("starts the topbar's stream only after adoption", async () => {
    await bootMain();

    expect(order.indexOf("adopt")).toBeLessThan(order.indexOf("topbar"));
  });

  test("mounts the feed before the footer whose jump rows reveal its rows", async () => {
    await bootMain();

    expect(order.indexOf("feed")).toBeLessThan(order.indexOf("footer"));
  });

  test("reveals and mounts the composer when the address asks for one", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");

    await bootMain();

    expect(document.getElementById("composer")?.hidden).toBe(false);
    expect(mounts.composer.mock.calls[0]?.[0]).toBe(document.getElementById("composer"));
  });

  test("leaves the composer hidden and unmounted in production", async () => {
    await bootMain();

    expect(document.getElementById("composer")?.hidden).toBe(true);
    expect(mounts.composer).not.toHaveBeenCalled();
  });

  test("gives the feed a per-bubble composer factory only in dev mode", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");

    await bootMain();

    expect(feedDeps?.composerFactory).toBeTypeOf("function");
  });

  test("routes the topbar's account control to the login overlay it mounted", async () => {
    await bootMain();
    const control = document.createElement("button");

    openLogin?.(control);

    expect(loginOpen).toHaveBeenCalledWith(control);
  });

  test("routes the footer's jump row to the feed it mounted", async () => {
    await bootMain();

    footerSelectDetachedWork?.({ value: "row-1" });

    expect(feedSelectDetachedWork).toHaveBeenCalledWith({ value: "row-1" });
  });

  test("routes the hold tray's first-drawn held prompt to the feed it mounted", async () => {
    await bootMain();

    trayPromptHeld?.("turn-1");

    expect(feedPromptHeld).toHaveBeenCalledWith("turn-1");
  });

  test("mints a page identity without crypto.randomUUID", async () => {
    // An old webview or a non-secure origin exposes no `crypto.randomUUID`,
    // and an unidentifiable page's records are still worth more than a page
    // that refuses to boot over its own identifier.
    vi.stubGlobal("crypto", {});

    await bootMain();

    const booted = clientLogs.find((call) => call.record.message === "the webapp booted");
    expect(booted?.record.context?.connection_id).toMatch(/^web-/);
  });

  test("builds a per-bubble composer on the host the feed hands it", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");
    await bootMain();
    const bubbleHost = document.createElement("div");

    (feedDeps?.composerFactory as (host: HTMLElement, feed: unknown) => unknown)(bubbleHost, {
      value: "row-1",
    });

    expect(mounts.composer).toHaveBeenCalledWith(bubbleHost, expect.anything(), expect.anything());
  });

  test("gives the feed no per-bubble composer factory in production", async () => {
    await bootMain();

    expect(feedDeps?.composerFactory).toBeUndefined();
  });

  test("closes the gate in the footer's own word when the workspace is merging", async () => {
    await bootMain();

    onFooterStatus?.("merging");

    expect(gateSet).toHaveBeenCalledWith("closed", "merging");
  });

  test("opens the gate when the footer reports a status that is not one of the three", async () => {
    await bootMain();

    onFooterStatus?.("running");

    expect(gateSet).toHaveBeenCalledWith("open", undefined);
  });

  test("draws a dev-mode command panel beside the composer that asked for it", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");
    await bootMain();

    onComposerPanel?.({} as SubmitPromptCommandPanel);

    expect(drawnPanel?.parentElement).toBe(document.getElementById("composer"));
  });

  test("replaces a stale panel rather than stacking a second one", async () => {
    addressPage("?workspace=ws-1&dir=/tmp/ws-1&composer=1");
    await bootMain();
    const stale = document.createElement("div");
    stale.setAttribute("data-panel", "stale");
    document.getElementById("composer")?.append(stale);

    onComposerPanel?.({} as SubmitPromptCommandPanel);

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

describe("a boot that fails", { timeout: coverageBootTimeoutMS }, () => {
  test("files boot_failed in the topbar's warning chip when adoption never completes", async () => {
    adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(chipEvidence("bootFailed")).toEqual(["cause: Error: adoption refused"]);
  });

  test("draws no failure overlay for a failed boot", async () => {
    adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(document.querySelector("#failure-overlay, .failure-card")).toBeNull();
  });

  test("re-raises the adoption failure after it has been drawn", async () => {
    adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(rethrown).toHaveLength(1);
    expect(() => rethrown[0]?.()).toThrow("adoption refused");
  });

  test("stops the page's one stream when the boot fails", async () => {
    // THE PAGE STREAM IS OPENED SEVERAL STEPS ABOVE ADOPTION, so a boot that
    // fails at adoption used to leave it reopening on backoff forever against
    // a daemon the page had already given up on — a dead page still holding a
    // connection, burying its own `boot_failed` card under a reopen loop's
    // error records. Caught as a timeout in the isolated coverage run.
    adopt = () => Promise.reject(new Error("adoption refused"));

    await bootMain();

    expect(bootedContext?.isQuiesced()).toBe(true);
  });

  test("leaves the page's one stream running when the boot succeeds", async () => {
    // The other side of it: quiescing is the FAILURE path's act, and a healthy
    // page that stopped dialing would draw nothing ever again.
    await bootMain();

    expect(bootedContext?.isQuiesced()).toBe(false);
  });

  test("words a non-Error rejection in the chip as the value itself", async () => {
    adopt = () => Promise.reject("the daemon hung up");

    await bootMain();

    expect(chipEvidence("bootFailed")).toEqual(["cause: the daemon hung up"]);
  });

  test("re-raises the shell's missing mount point by name", async () => {
    document.getElementById("footer")?.remove();

    await bootMain();

    expect(() => rethrown[0]?.()).toThrow("the page shell is missing #footer");
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

describe("the ClientLog sink and the client's link verdict", () => {
  test("reports a ClientLog that could not be forwarded", async () => {
    // ARRANGE: the daemon refuses every record this page tries to file.
    clientLogFails = true;

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
