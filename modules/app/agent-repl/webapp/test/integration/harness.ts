/**
 * THE HARNESS — the whole app, under jsdom, against a real daemon.
 *
 * It boots the way production boots: the real `index.html` becomes the
 * document, `shellElements` resolves the mount points by id (so a renamed id
 * fails here exactly as it would in the browser), and every component is
 * mounted through its own published signature. Nothing is stubbed between the
 * component and the wire — the transport, the codec, the streams and the
 * refusals are all the app's own.
 *
 * The one substitution is Node's `fetch`: jsdom's window has none, and the
 * transport needs one to reach the fake daemon. That is a capability the
 * environment is missing, not a seam in the app. It is handed an undici agent
 * pinned to that daemon's unix socket, because a loopback port per daemon
 * exhausted the ephemeral range under parallel runs (see fake-daemon.ts).
 *
 * TIME. Fake timers drive every clock, so a "quiet for N s" or a countdown is
 * asserted by advancing time rather than waiting for it. `shouldAdvanceTime`
 * is on because the HTTP round trip to the fake is real I/O that would
 * otherwise never complete; `settle()` is how a test waits for the DOM instead
 * of sleeping.
 */
import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { transferableAbortController } from "node:util";
import { Agent } from "undici";
import { vi } from "vitest";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";

import { shellElements, type ShellElements } from "../../src/shell";
import { createDaemonTransport } from "../../src/rpc/transport";
import { createAgentReplClient } from "../../src/rpc/client";
import { createAppContext, type AppContext } from "../../src/rpc/context";
import { workspaceRef } from "../../src/rpc/workspace-ref";
import { createTicker } from "../../src/clock";
import {
  ForwardingLogger,
  bindLogContext,
  setLogger,
  type ClientLogLevel,
} from "../../src/log";
import { createLocalFailures } from "../../src/failure/local";
import { bootFailed } from "../../src/failure/sink";
import { mountFeed, type FeedHandle } from "../../src/feed/feed";
import { createRowRenderers } from "../../src/feed/renderers";
import { mountFooter, type FooterHandle } from "../../src/footer/footer";
import { mountTopbar } from "../../src/topbar/topbar";
import { mountSidebar } from "../../src/sidebar/sidebar";
import { mountHoldTray } from "../../src/tray/tray";
import { mountComposer, createComposerGate } from "../../src/composer/composer";
import { drawCommandPanel } from "../../src/panels/panels";
import { forgetOwnTurns } from "../../src/composer/own-turns";
import { mountLoginOverlay, type LoginHandle } from "../../src/login/login";
import type { TerminalFactory } from "../../src/login/terminal";
import { adoptAtBoot, startLifecycle } from "../../src/lifecycle/lifecycle";
import type { SubmitPromptCommandPanel } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";

import { createFakeDaemon, type FakeDaemon } from "./fake-daemon";
import { WORKSPACE_ID, WORKSPACE_DIR } from "./fixtures";

/** Where the page's clock starts: just after the fixtures' own timestamps. */
export const HARNESS_EPOCH_MS = 10_000;

/**
 * How many drain rounds `settle()` gives the DOM before it calls it a fault.
 *
 * MEASURED, LEFT AS-IS: instrumented across all 13 integration files, the
 * slowest convergence in a healthy run took 24 rounds (in
 * refusals.integration.test.ts). 60 is already a ~2.5x margin over that; the
 * usual "~3x the observed max" rule would put this at 72, which is LOOSER
 * than the current cap, so it stays — never loosen a bound to hit a formula.
 */
const SETTLE_ROUND_CAP = 60;
/** How many consecutive quiet rounds mean the DOM has actually settled. */
const SETTLE_STABLE_ROUNDS = 4;
/** How much markup the non-convergence diagnostic quotes. */
const SETTLE_DIAGNOSTIC_LIMIT = 2000;

/**
 * Every request whose response head has not landed yet, each as a promise
 * that resolves (never rejects) when it does. `settle()` treats a non-empty
 * set as "not settled" and waits on it rather than spinning.
 */
interface InFlight {
  heads: Set<Promise<void>>;
}

interface Handle {
  dispose(): void;
}

/**
 * NODE'S FETCH, TAUGHT TO ACCEPT JSDOM'S AbortSignal.
 *
 * jsdom installs its own `AbortController`/`AbortSignal` over Node's, and
 * undici refuses a signal that is not an instance of its own class
 * ("RequestInit: Expected signal (\"AbortSignal {}\") to be an instance of
 * AbortSignal"). Every stream the app opens carries one, so without this every
 * `Watch*` request threw before it left the page, `watchStream` read that as a
 * transport failure, and the whole app sat behind a `daemon_unreachable` card
 * with no view ever drawn.
 *
 * The bridge is a capability the environment is missing, exactly like `fetch`
 * itself — not a seam in the app. `node:util`'s transferable controller is a
 * genuine Node one, so the app's abort still aborts the real request, and a
 * cancelled watch still closes the socket the fake daemon is holding.
 */
function fetchAcceptingJsdomSignals(
  underlying: typeof globalThis.fetch,
  inFlight: InFlight,
  dispatcher: Agent | undefined,
): typeof globalThis.fetch {
  return (input, init) => {
    const signal = init?.signal;
    const bridged = ((): RequestInit | undefined => {
      // THE DISPATCHER IS WHAT MAKES THE URL REACH ANYTHING. The fake
      // daemon's `baseUrl` is an identity, not an address (`*.invalid`); this
      // agent is pinned to that daemon's unix socket, so every request the app
      // forms against that origin lands on that listener and no ephemeral port
      // is ever consumed.
      // A REAL DAEMON NEEDS NO DISPATCHER. The e2e layer's base url is a
      // reachable loopback origin (the daemon writes it to daemon.addr), so
      // the init is left alone and Node's fetch resolves it itself.
      const routed = (dispatcher === undefined ? { ...init } : { ...init, dispatcher }) as RequestInit;
      if (signal === undefined || signal === null) return routed;
      const bridge = transferableAbortController();
      const abort = (): void => bridge.abort(signal.reason);
      if (signal.aborted) abort();
      else signal.addEventListener("abort", abort, { once: true });
      return { ...routed, signal: bridge.signal };
    })();
    // COUNTED SO `settle()` CANNOT RETURN MID-ROUND-TRIP. A request is in
    // flight until its RESPONSE HEAD lands, which for a standing stream is the
    // accept (the server flushes headers there) rather than the stream's end —
    // so a watch never holds settle open, and a call that has not answered yet
    // always does.
    // The head is also HELD AS A PROMISE, so a settle round that finds a
    // request outstanding can wait for the answer instead of spinning: it
    // resolves (never rejects) the moment the response head lands or the
    // request fails, and leaves the set at the same moment.
    let landed!: () => void;
    const head = new Promise<void>((resolve) => {
      landed = resolve;
    });
    inFlight.heads.add(head);
    const settled = (): void => {
      inFlight.heads.delete(head);
      landed();
    };
    return underlying(input, bridged).then(
      (response) => {
        settled();
        return response;
      },
      (err: unknown) => {
        settled();
        throw err;
      },
    );
  };
}

/**
 * THE MOUNTED APP: every query and control that is about the PAGE.
 *
 * Split out of `Harness` so the identical mount can be driven against a
 * daemon this harness did not start — the real `claude-repld` the Go e2e
 * world spawns (see `e2e/WEBAPP-LAYER-SPEC.md`). Everything a test asserts on
 * lives here; only the three fake-daemon-scripting members `Harness` adds do
 * not, so no existing integration file changes.
 */
export interface MountedApp {
  readonly ctx: AppContext;
  readonly shell: ShellElements;
  readonly feed: FeedHandle;
  readonly footer: FooterHandle;
  readonly login: LoginHandle;
  /** Every command panel the composer has been handed, in order. */
  readonly panels: SubmitPromptCommandPanel[];

  /** Advance microtasks and timers until the DOM stops changing. */
  settle(): Promise<void>;
  /** Advance fake time by `ms` and settle. */
  tick(ms: number): Promise<void>;
  /**
   * Dispose every mount while LEAVING the daemon up, so a test can observe
   * what the app's own cancellation does to the server's live streams.
   */
  disposeMounts(): Promise<void>;
  /** Dispose every mount, stop every daemon, restore real timers. */
  stop(): Promise<void>;
  /**
   * Install production's log sink NOW, on a harness booted without it.
   *
   * The wiring is main.ts's own, identical to what `clientLog: true` installs;
   * only the moment differs. A case that asserts about a record IT emits wants
   * production's sink but not the boot's own diagnostics, each of which is its
   * own unary round trip to the fake. Booting quiet and installing after keeps
   * unrelated diagnostic traffic out of the assertion entirely.
   *
   * `clientLog: true` still exists for the cases that ARE about the boot's own
   * diagnostics: those need the sink standing before the first mount draws.
   */
  installClientLogSink(): void;

  // --- queries, keyed on the DOM hooks contract (preamble §5) -------------
  $(selector: string): HTMLElement | null;
  $$(selector: string): HTMLElement[];
  /** The element for one feed row, or null when the row is not drawn. */
  row(id: string): HTMLElement | null;
  /** Every drawn feed row, in document order, by their FeedId values. */
  rowIds(container?: HTMLElement): string[];
  /** A feed container: the root feed, or a sub-feed by its bubble's FeedId. */
  feedContainer(id?: string): HTMLElement | null;
  /** Trimmed text of the first match, or undefined when it is not drawn. */
  text(selector: string): string | undefined;
  /** Trimmed text of every match, in document order. */
  texts(selector: string): string[];
  /** Click the first match; throws by selector when nothing matches. */
  click(selector: string): Promise<void>;
  /** Click an element directly (for elements found by a richer query). */
  clickElement(element: HTMLElement): Promise<void>;
  /** The client-local failure arms the topbar's warning chip currently lists. */
  failureArms(): string[];
  /** The refusal arms currently drawn anywhere, with their host selectors. */
  refusalArms(): string[];
}

/** The mounted app plus the fake daemon this harness started for it. */
export interface Harness extends MountedApp {
  /** The daemon the app is talking to. */
  readonly fake: FakeDaemon;
  /** A second daemon, started only by `startSecondDaemon` (the transfer case). */
  secondFake?: FakeDaemon;
  /** Boot a SECOND fake daemon, for the transfer/adopt case. */
  startSecondDaemon(): Promise<FakeDaemon>;
}

/** The body of the real index.html, so the shell under test is the shipped one. */
function installShell(doc: Document): void {
  // RESOLVED OFF THE PROJECT ROOT, not off `import.meta.url`. Under the jsdom
  // environment the module's own url is an http one (jsdom's document base),
  // and `fileURLToPath` refuses it — vitest runs from `webapp/`, so the shell
  // is found the same way `npm run build` finds it.
  const html = readFileSync(resolve(process.cwd(), "index.html"), "utf8");
  const body = /<body[^>]*>([\s\S]*)<\/body>/i.exec(html);
  if (!body) throw new Error("index.html has no <body>: the harness cannot boot the real shell");
  // Drop the module script tag: the harness mounts components itself rather
  // than letting main.ts boot, so that a test can script the daemon first.
  doc.body.innerHTML = body[1].replace(/<script[\s\S]*?<\/script>/gi, "");
}

/**
 * A TERMINAL FOR JSDOM, the one environment substitution besides `fetch`.
 *
 * xterm.js is a browser bundle: it reads `self` at import time, measures the
 * device pixel ratio through `window.matchMedia`, and paints through a canvas
 * 2d context. jsdom has none of the three, so the real factory throws before
 * the overlay has written a byte — the terminal cannot run here for the same
 * reason `fetch` cannot, and for no reason that lives in the app.
 *
 * So the harness supplies the same `LoginTerminalView` the overlay is written
 * against, backed by the DOM: bytes are appended as text (which is what the
 * suite reads back), a keydown on the host is reported as keystrokes, and
 * `fit()` answers a fixed geometry. Everything the tests actually assert —
 * which rpcs are called, which arms are sent, when the stream is cancelled —
 * is the app's own.
 */
const jsdomTerminalFactory: TerminalFactory = async (host) => {
  const decoder = new TextDecoder();
  const encoder = new TextEncoder();
  const screen = document.createElement("pre");
  screen.className = "login-term-screen";
  host.replaceChildren(screen);
  const listeners: ((data: Uint8Array) => void)[] = [];
  const onKeydown = (event: KeyboardEvent): void => {
    // One key, one report: the pty sees the bytes, never a key name.
    const bytes = encoder.encode(event.key === "Enter" ? "\r" : event.key);
    for (const fn of listeners) fn(bytes);
  };
  host.addEventListener("keydown", onKeydown);
  return {
    write: (data) => {
      screen.textContent = (screen.textContent ?? "") + decoder.decode(data);
    },
    onData: (fn) => {
      listeners.push(fn);
    },
    fit: () => ({ rows: 24, cols: 100 }),
    focus: () => {},
    dispose: () => {
      host.removeEventListener("keydown", onKeydown);
      listeners.length = 0;
      screen.remove();
    },
  };
};

export interface HarnessOptions {
  /** Dev mode: mount the root composer (`&composer=1`). Default false. */
  composer?: boolean;
  /** The workspace the page is addressed to. */
  workspaceId?: string;
  workspaceDir?: string;
  /** The page-delivered `AGENT_REPL_LOG_LEVEL`. */
  logLevel?: ClientLogLevel;
  /** Script the daemon before anything mounts (the cold-open case). */
  arrange?(fake: FakeDaemon): void;
  /**
   * Install PRODUCTION'S OWN LOG SINK: one `ClientLog` call per record, as
   * main.ts wires it (`clientLogSink`, deliberately NOT through `callUnary`,
   * with the identity bound before anything draws).
   *
   * OFF BY DEFAULT, because every mount logs and a suite that is not about
   * logging would then read its own diagnostics back out of the daemon's call
   * log. The console function is a no-op so the suite's output stays clean;
   * the forwarding half is the app's own.
   *
   * Reach for this ONLY when the case is about the boot's own diagnostics. A
   * case that only wants the sink for a record IT emits should boot quiet and
   * call `installClientLogSink()` instead.
   */
  clientLog?: boolean;
}

/** Where the app's transport points, and how a request gets routed there. */
interface Endpoint {
  /** The transport's base url. */
  readonly baseUrl: string;
  /**
   * A unix socket the base url's origin is really served on, when it is an
   * identity rather than an address (the in-process fake). Absent for a real
   * daemon, whose base url is a reachable loopback origin.
   */
  readonly socketPath?: string;
}

/**
 * The client-local failure arms the topbar's warning chip lists, read off the
 * document: the chip's `data-local-arms` hook, so no reveal has to be opened
 * to say which failures the page is showing.
 */
export function chipFailureArms(): string[] {
  const chip = document.querySelector<HTMLElement>("#topbar .topbar-warnings");
  return (chip?.dataset.localArms ?? "").split(" ").filter((arm) => arm !== "");
}

/**
 * Everything the warning chip shows for the client-local failure ARM: its row
 * in the chip's list and, when the row opens one, the detail behind it — the
 * headline and the evidence, as the reader would see them.
 */
export async function chipFailureText(app: MountedApp, arm: string): Promise<string> {
  await app.click("#topbar .topbar-warning-chip");
  const row = app.$(`#topbar [data-reveal] [data-local][data-arm="${arm}"]`);
  if (row === null) throw new Error(`the warning chip lists no ${arm} failure`);
  const listed = row.textContent ?? "";
  if (row.tagName !== "BUTTON") return listed;
  await app.clickElement(row);
  return `${listed}\n${app.$("#topbar [data-reveal]")?.textContent ?? ""}`;
}

/**
 * Mount the app against a daemon SOMEONE ELSE started.
 *
 * The Go e2e world owns the real quartet's lifecycle and hands its daemon's
 * loopback address to the vitest child that calls this (see
 * `e2e/WEBAPP-LAYER-SPEC.md`). Everything past the endpoint is identical to
 * `startHarness`: the same shell, the same mounts, the same boot order.
 */
export async function startAppAgainst(
  baseUrl: string,
  options: HarnessOptions = {},
): Promise<MountedApp> {
  if (options.arrange !== undefined) {
    throw new Error(
      "startAppAgainst cannot arrange: there is no fake daemon to script, the daemon is real",
    );
  }
  return mountApp({ baseUrl }, options);
}

export async function startHarness(options: HarnessOptions = {}): Promise<Harness> {
  const fake = createFakeDaemon();
  const { baseUrl, socketPath } = await fake.start();
  const app = await mountApp({ baseUrl, socketPath }, options, fake);
  const harness: Harness = {
    ...app,
    fake,
    async startSecondDaemon() {
      const second = createFakeDaemon();
      await second.start();
      harness.secondFake = second;
      return second;
    },
    async stop() {
      await app.stop();
      await harness.secondFake?.stop();
    },
  };
  return harness;
}

// pageSerial names each mounted app's page. A page id addresses ONE stream on
// the daemon, and a second stream for one id is refused, so two mounts in one
// file must not share one.
let pageSerial = 0;

async function mountApp(
  endpoint: Endpoint,
  options: HarnessOptions,
  fake?: FakeDaemon,
): Promise<MountedApp> {
  // CAPTURED BEFORE THE CLOCK IS FAKED. `settle()` needs a way to hand the
  // event loop back to Node so the loopback round trip to the fake daemon can
  // land; every scheduling primitive on the page is about to become fake, so
  // the real one is taken now. This is NOT a sleep and never waits for a
  // duration: it is a zero-length yield, the only thing that lets real socket
  // I/O make progress between two microtask drains.
  const yieldToIo = ((): (() => Promise<void>) => {
    const realSetImmediate = globalThis.setImmediate;
    return () => new Promise<void>((resolve) => realSetImmediate(() => resolve()));
  })();

  // THE CLOCK STARTS WHERE THE FIXTURES LIVE. Every wire timestamp in
  // `fixtures.ts` is a small absolute epoch value (a queue at 3 s, a last
  // progress at 2 s), because a fixture that hard-codes "now" would rot. So
  // the page's clock is put ten seconds after the epoch rather than at the
  // real wall clock, where those same timestamps would read as fifty years
  // ago and every countdown would already have expired.
  vi.useFakeTimers({ shouldAdvanceTime: true, now: HARNESS_EPOCH_MS });

  const { baseUrl, socketPath } = endpoint;
  const dispatcher = socketPath === undefined ? undefined : new Agent({ connect: { socketPath } });
  // `stop()` is idempotent (a test may stop explicitly and the afterEach stops
  // again), and undici throws on a second close, so the close is claimed once.
  let stopped = false;
  let dispatcherClosed = false;
  const closeDispatcher = async (): Promise<void> => {
    if (dispatcher === undefined || dispatcherClosed) return;
    dispatcherClosed = true;
    await dispatcher.close();
  };
  if (fake !== undefined) options.arrange?.(fake);

  installShell(document);
  const shell = shellElements(document);

  // jsdom's window carries no fetch; Node's global one reaches loopback.
  const inFlight: InFlight = { heads: new Set() };
  const transport = createDaemonTransport(baseUrl, {
    fetch: fetchAcceptingJsdomSignals(globalThis.fetch, inFlight, dispatcher),
  });
  const client = createAgentReplClient(transport);
  const ticker = createTicker();
  // MAIN.TS'S OWN ORDER: the topbar is mounted before any stream, so its
  // warning chip can list a failure that stops one from ever opening.
  const failures = createLocalFailures();
  const topbar = mountTopbar(shell.topbar, { failures });
  const composerEnabled = options.composer === true;

  // THE REAL PAGE MUX, on purpose. This harness drives a REAL daemon, so its
  // page attaches `WatchPage` and every watch it mounts rides that one stream —
  // which is the only place in the suites where the multiplexing itself is
  // exercised end to end. Each mounted app takes its own page id, so two
  // harnesses in one file cannot collide on the daemon's page registry.
  const ctx = createAppContext({
    client,
    workspace: workspaceRef(options.workspaceId ?? WORKSPACE_ID, options.workspaceDir ?? WORKSPACE_DIR),
    ticker,
    failures,
    composerEnabled,
    page: `harness-page-${(pageSerial += 1)}`,
  });

  // MAIN.TS'S OWN SINK, in main.ts's own order: the identity is bound and the
  // forwarding logger installed BEFORE the first component draws, so a record
  // emitted during boot travels the same path a record emitted later does.
  let clientLogger: ForwardingLogger | undefined;
  const installClientLogSink = (): void => {
    bindLogContext({
      connection_id: "harness-connection",
      workspace_id: ctx.workspace.id,
      workspace_dir: ctx.workspace.dir,
    });
    clientLogger = new ForwardingLogger(
      async (record) => {
        await client.clientLog({ workspace: ctx.workspace, record });
        return "accepted";
      },
      () => {},
      {},
      options.logLevel ?? "info",
    );
    setLogger(clientLogger);
  };
  if (options.clientLog === true) installClientLogSink();

  const panels: SubmitPromptCommandPanel[] = [];
  const gate = createComposerGate();
  const handles: Handle[] = [failures, topbar];

  /**
   * PRODUCTION'S OWN PANEL SINK (main.ts): the answer to a slash command is
   * DRAWN in the composer's area, replacing whatever panel stood there — a
   * panel answers one submission, not the conversation. The harness keeps the
   * panels it was handed as well, so a test can assert the callback and the
   * drawing separately.
   */
  const showPanel = (panel: SubmitPromptCommandPanel): void => {
    panels.push(panel);
    for (const stale of shell.composer.querySelectorAll(":scope > [data-panel]")) stale.remove();
    shell.composer.append(drawCommandPanel(panel, ctx));
  };

  // PRODUCTION'S OWN BOOT ORDER, and it is load-bearing: adoption comes before
  // any stream (a daemon still finishing its rendezvous refuses every
  // per-workspace rpc), and the lifecycle comes IMMEDIATELY after it, before
  // the first view mounts, so the `transferring_away` move hook is registered
  // before any refusal can carry that arm back.
  // MAIN.TS'S OWN BOOT PATH. A terminal adoption refusal throws
  // `AdoptionFailed`, main mints `boot_failed` from it, and NOTHING more is
  // mounted over a workspace this page could not adopt. The harness mirrors
  // that: it files the same failure, stops the daemon it started (no Harness is returned to
  // stop it later), and lets the throw reach the test.
  try {
    await adoptAtBoot(ctx);
  } catch (err) {
    // The topbar is LEFT MOUNTED on purpose: its warning chip is the only
    // account of the failed boot a test can read.
    failures.report(bootFailed(err instanceof Error ? err.message : String(err)));
    await fake?.stop();
    await closeDispatcher();
    throw err;
  }
  handles.push(startLifecycle(ctx, { drainBannerHost: shell.drainBanner }));

  const feed = mountFeed(shell.feed, ctx, {
    renderers: createRowRenderers(ctx),
    composerFactory: composerEnabled
      ? (host, bubble) => mountComposer(host, ctx, { feed: bubble, gate, onPanel: showPanel })
      : undefined,
  });
  handles.push(feed);

  const footer = mountFooter(shell.footer, ctx, { selectDetachedWork: (id: FeedId) => feed.selectDetachedWork(id) });
  handles.push(footer);
  // The per-bubble composers close on exactly these statuses (R7).
  footer.onStatus((statusCase) =>
    gate.set(
      statusCase === "merging" || statusCase === "closing" || statusCase === "disconnected"
        ? "closed"
        : "open",
    ),
  );

  const login = mountLoginOverlay(shell.loginOverlay, ctx, {
    terminalFactory: jsdomTerminalFactory,
  });
  handles.push(login);
  topbar.watch(ctx, { openLogin: (control) => login.open(control) });
  handles.push(mountSidebar(shell.sidebar, ctx));
  handles.push(mountHoldTray(shell.holdTray, ctx));

  if (composerEnabled) {
    shell.composer.hidden = false;
    handles.push(mountComposer(shell.composer, ctx, { gate, onPanel: showPanel }));
  }

  const $ = (selector: string): HTMLElement | null => document.querySelector<HTMLElement>(selector);
  const $$ = (selector: string): HTMLElement[] => [...document.querySelectorAll<HTMLElement>(selector)];

  /**
   * Wait for the DOM to stop changing.
   *
   * A push travels an HTTP stream, a promise chain and possibly a ticker
   * before it becomes DOM, and no single flush covers all three. Settling on
   * "the markup stopped changing across two consecutive drains" waits for the
   * observable thing a test asserts on, and never sleeps a fixed interval.
   */
  /**
   * The markup, with THE CLOCKS MASKED.
   *
   * `shouldAdvanceTime` is on, so real seconds pass while `settle()` drains —
   * and a page holding a ticking row (a permission's "waiting 0s", a tool
   * call's "quiet for N", a cold gate's lapse) rewrites that one string every
   * real second. Under load a drain round can take longer than the gap between
   * two ticks, and a settle that compares raw markup then never sees four quiet
   * rounds in a row and fails a page that is in fact idle.
   *
   * Every such element marks itself `data-ticking` (src/feed/ticking.ts), so
   * their text is blanked in the comparison and nowhere else: a test that
   * asserts a clock moved still reads the live DOM, and any OTHER change — a
   * redraw, a new row, an attribute — still counts as the DOM moving.
   */
  const quietMarkup = (): string => {
    const clone = document.body.cloneNode(true) as HTMLElement;
    for (const clock of clone.querySelectorAll("[data-ticking]")) clock.textContent = "";
    return clone.innerHTML;
  };

  /**
   * A MUTATION OBSERVER IN FRONT OF THE MARKUP DIFF, purely as a fast path.
   *
   * The diff above is the definition of "the DOM moved" and stays the
   * definition — a component that redraws itself wholesale into byte-identical
   * markup (the sidebar, the footer and the tray all do, once a real second,
   * because they hold a relative time) has NOT moved as far as a test is
   * concerned, and only comparing the markup can tell you that.
   *
   * But it is O(the whole page) and it ran on every round of every settle:
   * profiled across the suite that was 6.8ms a round and HALF of all the time
   * the integration tests spent, nearly all of it answering "no" on a page
   * that nothing had touched.
   *
   * A `MutationObserver` answers "did anything touch the page at all" at O(the
   * changes), and when the answer is no the markup CANNOT have changed, so the
   * serialization is skipped and the previous string still stands. The diff
   * runs only on the rounds where something really did mutate. The verdict is
   * identical to the old one on every round; only the cost differs.
   */
  let touched = false;
  const observer = new MutationObserver(() => {
    touched = true;
  });
  observer.observe(document.body, {
    subtree: true,
    childList: true,
    attributes: true,
    characterData: true,
  });
  // NOT pushed onto `handles`: those are disposed by `disposeMounts()`, which
  // then settles to observe what the app's own teardown does. The observer has
  // to still be watching for that, so it is disconnected only by `stop()`.

  let lastMarkup = quietMarkup();
  /** Whether the masked markup differs from the last time this was asked. */
  const markupMoved = (): boolean => {
    // `takeRecords()` drains what the observer has queued but not yet
    // delivered, so a round never misses a change the NEXT round would hear
    // about — and a delivered callback has already set the flag.
    const pending = observer.takeRecords().length > 0;
    if (!touched && !pending) return false;
    touched = false;
    const current = quietMarkup();
    const moved = current !== lastMarkup;
    lastMarkup = current;
    return moved;
  };

  const settle = async (): Promise<void> => {
    let stable = 0;
    for (let round = 0; round < SETTLE_ROUND_CAP; round += 1) {
      await vi.advanceTimersByTimeAsync(0);
      await yieldToIo();
      for (let flush = 0; flush < 5; flush += 1) await Promise.resolve();
      // AN OUTSTANDING REQUEST IS WAITED FOR, NOT SPUN ON. A round used to
      // cost ~7ms of serialization, which made the round cap a wall-clock
      // allowance of ~400ms as a side effect, and a loopback unary under
      // parallel load always answered inside it. With the observer fast path
      // a quiet round costs ~1ms, sixty of them ~60ms, and a unary that took
      // longer — measured: the ClientLog flush in client-log.integration.test.ts,
      // 3 runs in 6 under a sibling suite's load — exhausted the cap on a page
      // that was merely waiting for its answer. So the round blocks on the
      // first head to land; the cap counts DOM churn, never the network.
      if (inFlight.heads.size > 0) await Promise.race(inFlight.heads);
      // An unanswered request is a change that has not happened YET, so a
      // quiet DOM with one outstanding is not settled — it is early.
      stable = !markupMoved() && inFlight.heads.size === 0 ? stable + 1 : 0;
      if (stable >= SETTLE_STABLE_ROUNDS) return;
    }
    // NON-CONVERGENCE IS A FAULT, NEVER A QUIET RETURN. A settle that gave up
    // silently is how a page that never stopped redrawing became "the element
    // was not drawn" fifty assertions later; throwing here names the real
    // fault at the moment it happens.
    throw new Error(
      `the DOM never stopped changing after ${SETTLE_ROUND_CAP} settle rounds; ` +
        `failure arms: [${harnessFailureArms().join(", ")}]; ` +
        `last markup: ${document.body.innerHTML.slice(0, SETTLE_DIAGNOSTIC_LIMIT)}`,
    );
  };

  const harnessFailureArms = (): string[] => chipFailureArms();

  const harness: MountedApp = {
    ctx,
    shell,
    feed,
    footer,
    login,
    panels,

    settle,
    installClientLogSink,
    async tick(ms) {
      await vi.advanceTimersByTimeAsync(ms);
      await settle();
    },
    async disposeMounts() {
      for (const handle of [...handles].reverse()) handle.dispose();
      handles.length = 0;
      await settle();
    },
    async stop() {
      if (stopped) return;
      stopped = true;
      for (const handle of [...handles].reverse()) handle.dispose();
      // Draining clears the logger's timer and settle holds the socket open
      // until every resulting ClientLog response head lands. Without both,
      // a later test's clock can flush this page's records after its fake
      // daemon has already stopped.
      if (clientLogger !== undefined) {
        clientLogger.flush();
        await settle();
        // Stream cancellation below can itself log. Detach production's sink
        // only after it is drained so teardown diagnostics cannot address a
        // fake daemon that teardown has already stopped.
        setLogger(new ForwardingLogger(async () => "accepted", () => {}));
      }
      // A daemon this mount did not start is the caller's to stop; the fake
      // one it did start is stopped here.
      await fake?.stop();
      // The agent holds this daemon's sockets open; a run leaves ~1600 of them
      // otherwise, and the next test's daemon is a different socket anyway.
      await closeDispatcher();
      // A FRESH BROWSER PROFILE PER TEST. The webview-local preferences (R14 —
      // the open footer panel, the sidebar grouping, folds) live in
      // `localStorage`, which jsdom shares across every test in a file. Left
      // behind, one test's click decides what the NEXT test's page opens with,
      // which is a dependency between tests and not a fact about the app.
      // The page's claim on the turns IT submitted dies with the page, the
      // same way it does on a reload (R14: nothing is persisted).
      observer.disconnect();
      forgetOwnTurns();
      try {
        window.localStorage.clear();
      } catch {
        // A jsdom without storage is fine: there is then nothing to clear.
      }
      vi.useRealTimers();
    },

    $,
    $$,
    row: (id) => $(`[data-feed-row="${id}"]`),
    rowIds: (container) =>
      [...(container ?? document.body).querySelectorAll<HTMLElement>("[data-feed-row]")].map(
        (el) => el.dataset.feedRow ?? "",
      ),
    feedContainer: (id) => $(`[data-feed="${id ?? "root"}"]`),
    text: (selector) => $(selector)?.textContent?.trim(),
    texts: (selector) => $$(selector).map((el) => el.textContent?.trim() ?? ""),
    async click(selector) {
      const element = $(selector);
      if (!element) throw new Error(`nothing to click at ${selector}`);
      element.click();
      await settle();
    },
    async clickElement(element) {
      element.click();
      await settle();
    },
    failureArms: () => chipFailureArms(),
    refusalArms: () => $$(".refusal[data-arm]").map((el) => el.dataset.arm ?? ""),
  };

  await settle();
  return harness;
}
