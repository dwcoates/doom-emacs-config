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
 * transport needs one to reach loopback. That is a capability the environment
 * is missing, not a seam in the app.
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
import { vi } from "vitest";
import type { FeedId } from "../../../proto/gen/ts/frontend/v1/feed_pb";

import { shellElements, type ShellElements } from "../../src/shell";
import { createDaemonTransport } from "../../src/rpc/transport";
import { createAgentReplClient } from "../../src/rpc/client";
import { createAppContext, type AppContext } from "../../src/rpc/context";
import { workspaceRef } from "../../src/rpc/workspace-ref";
import { createTicker } from "../../src/clock";
import { mountFailureOverlay } from "../../src/failure/overlay";
import { mountFeed, type FeedHandle } from "../../src/feed/feed";
import { createRowRenderers } from "../../src/feed/renderers";
import { mountFooter, type FooterHandle } from "../../src/footer/footer";
import { mountTopbar } from "../../src/topbar/topbar";
import { mountSidebar } from "../../src/sidebar/sidebar";
import { mountHoldTray } from "../../src/tray/tray";
import { mountComposer, createComposerGate } from "../../src/composer/composer";
import { mountLoginOverlay, type LoginHandle } from "../../src/login/login";
import { adoptAtBoot, startLifecycle } from "../../src/lifecycle/lifecycle";
import type { SubmitPromptCommandPanel } from "../../../proto/gen/ts/agentrepl/v1/endpoint_submit_prompt_pb";

import { createFakeDaemon, type FakeDaemon } from "./fake-daemon";
import { WORKSPACE_ID, WORKSPACE_DIR } from "./fixtures";

/** How many drain rounds `settle()` gives the DOM before it calls it a fault. */
const SETTLE_ROUND_CAP = 60;
/** How many consecutive quiet rounds mean the DOM has actually settled. */
const SETTLE_STABLE_ROUNDS = 4;
/** How much markup the non-convergence diagnostic quotes. */
const SETTLE_DIAGNOSTIC_LIMIT = 2000;

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
  inFlight: { count: number },
): typeof globalThis.fetch {
  return (input, init) => {
    const signal = init?.signal;
    const bridged = ((): RequestInit | undefined => {
      if (signal === undefined || signal === null) return init ?? undefined;
      const bridge = transferableAbortController();
      const abort = (): void => bridge.abort(signal.reason);
      if (signal.aborted) abort();
      else signal.addEventListener("abort", abort, { once: true });
      return { ...init, signal: bridge.signal };
    })();
    // COUNTED SO `settle()` CANNOT RETURN MID-ROUND-TRIP. A request is in
    // flight until its RESPONSE HEAD lands, which for a standing stream is the
    // accept (the server flushes headers there) rather than the stream's end —
    // so a watch never holds settle open, and a call that has not answered yet
    // always does.
    inFlight.count += 1;
    const settled = (): void => {
      inFlight.count -= 1;
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

export interface Harness {
  /** The daemon the app is talking to. */
  readonly fake: FakeDaemon;
  /** A second daemon, started only by `startSecondDaemon` (the transfer case). */
  secondFake?: FakeDaemon;
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
  /** Boot a SECOND fake daemon, for the transfer/adopt case. */
  startSecondDaemon(): Promise<FakeDaemon>;
  /**
   * Dispose every mount while LEAVING the daemon up, so a test can observe
   * what the app's own cancellation does to the server's live streams.
   */
  disposeMounts(): Promise<void>;
  /** Dispose every mount, stop every daemon, restore real timers. */
  stop(): Promise<void>;

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
  /** The failure overlay's currently drawn arms. */
  failureArms(): string[];
  /** The refusal arms currently drawn anywhere, with their host selectors. */
  refusalArms(): string[];
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

export interface HarnessOptions {
  /** Dev mode: mount the root composer (`&composer=1`). Default false. */
  composer?: boolean;
  /** The workspace the page is addressed to. */
  workspaceId?: string;
  workspaceDir?: string;
  /** Script the daemon before anything mounts (the cold-open case). */
  arrange?(fake: FakeDaemon): void;
}

export async function startHarness(options: HarnessOptions = {}): Promise<Harness> {
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

  vi.useFakeTimers({ shouldAdvanceTime: true });

  const fake = createFakeDaemon();
  const { baseUrl } = await fake.start();
  options.arrange?.(fake);

  installShell(document);
  const shell = shellElements(document);

  // jsdom's window carries no fetch; Node's global one reaches loopback.
  const inFlight = { count: 0 };
  const transport = createDaemonTransport(baseUrl, {
    fetch: fetchAcceptingJsdomSignals(globalThis.fetch, inFlight),
  });
  const client = createAgentReplClient(transport);
  const ticker = createTicker();
  const failures = mountFailureOverlay(shell.failureOverlay);
  const composerEnabled = options.composer === true;

  const ctx = createAppContext({
    client,
    workspace: workspaceRef(options.workspaceId ?? WORKSPACE_ID, options.workspaceDir ?? WORKSPACE_DIR),
    ticker,
    failures,
    composerEnabled,
  });

  const panels: SubmitPromptCommandPanel[] = [];
  const gate = createComposerGate();
  const handles: Handle[] = [failures];

  // PRODUCTION'S OWN BOOT ORDER, and it is load-bearing: adoption comes before
  // any stream (a daemon still finishing its rendezvous refuses every
  // per-workspace rpc), and the lifecycle comes IMMEDIATELY after it, before
  // the first view mounts, so the `transferring_away` move hook is registered
  // before any refusal can carry that arm back.
  await adoptAtBoot(ctx);
  handles.push(startLifecycle(ctx, { drainBannerHost: shell.drainBanner }));

  const feed = mountFeed(shell.feed, ctx, {
    renderers: createRowRenderers(ctx),
    composerFactory: composerEnabled
      ? (host, bubble) =>
          mountComposer(host, ctx, { feed: bubble, gate, onPanel: (p) => panels.push(p) })
      : undefined,
  });
  handles.push(feed);

  const footer = mountFooter(shell.footer, ctx, { revealRow: (id: FeedId) => feed.revealRow(id) });
  handles.push(footer);
  // The per-bubble composers close on exactly these statuses (R7).
  footer.onStatus((statusCase) =>
    gate.set(
      statusCase === "merging" || statusCase === "closing" || statusCase === "disconnected"
        ? "closed"
        : "open",
    ),
  );

  const login = mountLoginOverlay(shell.loginOverlay, ctx);
  handles.push(login);
  handles.push(mountTopbar(shell.topbar, ctx, { openLogin: () => login.open() }));
  handles.push(mountSidebar(shell.sidebar, ctx));
  handles.push(mountHoldTray(shell.holdTray, ctx));

  if (composerEnabled) {
    shell.composer.hidden = false;
    handles.push(mountComposer(shell.composer, ctx, { gate, onPanel: (p) => panels.push(p) }));
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
  const settle = async (): Promise<void> => {
    let previous = "";
    let stable = 0;
    for (let round = 0; round < SETTLE_ROUND_CAP; round += 1) {
      await vi.advanceTimersByTimeAsync(0);
      await yieldToIo();
      for (let flush = 0; flush < 5; flush += 1) await Promise.resolve();
      const current = document.body.innerHTML;
      // An unanswered request is a change that has not happened YET, so a
      // quiet DOM with one outstanding is not settled — it is early.
      stable = current === previous && inFlight.count === 0 ? stable + 1 : 0;
      previous = current;
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

  const harnessFailureArms = (): string[] =>
    $$('[data-component="failure-overlay"] [data-arm]').map((el) => el.dataset.arm ?? "");

  const harness: Harness = {
    fake,
    ctx,
    shell,
    feed,
    footer,
    login,
    panels,

    settle,
    async tick(ms) {
      await vi.advanceTimersByTimeAsync(ms);
      await settle();
    },
    async startSecondDaemon() {
      const second = createFakeDaemon();
      await second.start();
      harness.secondFake = second;
      return second;
    },
    async disposeMounts() {
      for (const handle of [...handles].reverse()) handle.dispose();
      handles.length = 0;
      await settle();
    },
    async stop() {
      for (const handle of [...handles].reverse()) handle.dispose();
      await fake.stop();
      await harness.secondFake?.stop();
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
    failureArms: () =>
      $$('[data-component="failure-overlay"] [data-arm]').map((el) => el.dataset.arm ?? ""),
    refusalArms: () => $$(".refusal[data-arm]").map((el) => el.dataset.arm ?? ""),
  };

  await settle();
  return harness;
}
