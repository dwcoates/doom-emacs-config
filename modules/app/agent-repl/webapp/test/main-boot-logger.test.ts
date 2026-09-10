// @vitest-environment jsdom
/**
 * THE BOOT WITH NOTHING SUBSTITUTED, AND WITH NO LOGGER ALREADY INSTALLED.
 *
 * `main.test.ts` covers the boot's order and its wiring, and it substitutes
 * the two ends to do so: the wire, and every mount. That is the right shape
 * for what it asserts, and it is exactly why it cannot see the defect this
 * file exists for.
 *
 * THE DEFECT. `log()` REFUSES to emit without an installed sink -- `emit`
 * throws "the webapp logger is not installed" rather than discarding the
 * record -- and two of the boot's own steps log as their first statement:
 * `shellElements` announces the shell it is resolving, and
 * `mountFailureOverlay` announces its mount. Both ran BEFORE `setLogger`, so
 * every real page threw out of `boot` at the one moment `overlay` was still
 * null: the catch reported through `console.error`, and THE PAGE CAME UP
 * EMPTY AND SILENT -- no stream opened, no card drawn, nothing said.
 *
 * Observed in the e2e sandbox against the real daemon before it was fixed:
 * the bundle served and evaluated, the shell's four mount points sat
 * untouched, and a plain `fetch` from inside that same page answered
 * `DaemonHealth` 200 -- so nothing about the environment was wrong.
 *
 * TWO THINGS HID IT FROM EVERY SUITE IN THIS PACKAGE, and this file undoes
 * both:
 *
 *   - `test/setup.ts` installs a logger before EVERY test, reproducing the
 *     invariant `boot` is itself responsible for establishing. So each test
 *     here takes it back OUT, which is the only way to run the boot against
 *     the state a real page hands it.
 *   - `main.test.ts` mocks the mounts, so `mountFailureOverlay` never
 *     reaches its own log line. Nothing here is mocked but the wire.
 *
 * The page's own shell comes from `index.html` itself, so this cannot drift
 * from the file the daemon serves.
 */
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { resetLoggingForTests } from "../src/log.js";
import { shellHTML } from "./shell-html.js";

/**
 * THIS FILE'S OWN TEST BOUND, because one test here pays for the whole graph.
 *
 * The 850ms unit global is sized for tests that import nothing at run time.
 * Every test here does `await import("../src/main.js")` INSIDE its body — that
 * top-level `void boot()` is the production entry point, so importing it is
 * how a page is booted — and whichever test runs first compiles the entire
 * module graph on that import. Measured over three runs: the first test 661,
 * 678 and 692ms; the second and third 68-87ms each, doing identical work. So
 * the cost is the first import, not the boot, and it lands on whichever test
 * the runner happens to order first — which the shuffled run reorders.
 *
 * 3x the 692ms healthy maximum. This is a per-site bound, deliberately not a
 * raised global: nothing else in the unit suite imports its subject at run
 * time, and the global stays sized for what it covers.
 */
const BOOT_IMPORT_TIMEOUT_MS = 2100;

/** The page address a workspace-addressed page is loaded with. */
const PAGE_ADDRESS = "/?workspace=w-boot-logger&dir=/tmp/w-boot-logger";

/**
 * How long the boot is given to reach its failure card.
 *
 * The boot is asynchronous -- `main.ts` ends in `void boot()` -- so the
 * import returns before the failure path has run. Everything between here
 * and the card is one rejected fetch and a few microtasks, so this is a
 * ceiling on a hang rather than a duration anything is expected to take.
 */
const BOOT_SETTLE_MS = 2000;

/** The boot's failure card, once it exists. */
async function awaitBootFailureCard(): Promise<Element> {
  const deadline = Date.now() + BOOT_SETTLE_MS;
  for (;;) {
    const card = document.querySelector('#failure-overlay .failure-card[data-arm="bootFailed"]');
    if (card !== null) return card;
    if (Date.now() > deadline) {
      throw new Error(
        `the boot drew no failure card within ${BOOT_SETTLE_MS}ms; ` +
          `the overlay holds ${document.querySelector("#failure-overlay")?.innerHTML ?? "<no overlay>"}`,
      );
    }
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
}

/**
 * THE RE-RAISE, CAUGHT INSTEAD OF LEAKED.
 *
 * `main.ts` ends in `void boot().catch(err => queueMicrotask(() => { throw err }))`,
 * and that rethrow is deliberate: it re-raises a boot failure as the uncaught
 * error a browser reports, which is the loudness the old synchronous throw
 * had. Both tests here drive the boot to failure ON PURPOSE, so both provoke
 * it — and under vitest an uncaught error is a FAILED RUN, however many
 * assertions passed. `npm test` exited 1 with 3759 tests green.
 *
 * The answer is not to silence it. A test that provokes a re-raise owns it, so
 * the microtask the re-raise is scheduled on is taken over here and the error
 * it carries is captured and ASSERTED — strictly more coverage than the
 * escape, and not one line of production changed. Every other caller's
 * callback still runs on a real microtask.
 */
let reRaised: unknown[] = [];

function captureTheReRaise(): void {
  const realQueueMicrotask = globalThis.queueMicrotask.bind(globalThis);
  reRaised = [];
  globalThis.queueMicrotask = (callback: () => void): void => {
    realQueueMicrotask(() => {
      try {
        callback();
      } catch (err) {
        reRaised.push(err);
      }
    });
  };
}

// Compile the composition root's dependency graph during file collection,
// outside every behavioral timeout. The timed import below still evaluates a
// fresh real entry point; this only keeps Vite's transform cost from consuming
// that bound when the coverage run is contending with its worker pool.
await Promise.all([
  import("../src/clock.js"),
  import("../src/composer/composer.js"),
  import("../src/feed/renderers.js"),
  import("../src/feed/feed.js"),
  import("../src/footer/footer.js"),
  import("../src/lifecycle/lifecycle.js"),
  import("../src/login/login.js"),
  import("../src/panels/panels.js"),
  import("../src/sidebar/sidebar.js"),
  import("../src/topbar/topbar.js"),
  import("../src/tray/tray.js"),
  import("../src/failure/sink.js"),
  import("../src/failure/overlay.js"),
  import("../src/rpc/client.js"),
  import("../src/rpc/context.js"),
  import("../src/rpc/page-address.js"),
  import("../src/rpc/transport.js"),
  import("../src/rpc/workspace-ref.js"),
  import("../src/shell.js"),
]);

describe("the webapp's boot against a real page", () => {
  let realFetch: typeof globalThis.fetch;
  let realQueueMicrotask: typeof globalThis.queueMicrotask;
  let consoleError: ReturnType<typeof vi.spyOn>;

  beforeEach(() => {
    realQueueMicrotask = globalThis.queueMicrotask;
    captureTheReRaise();
    // OUT, not replaced. A page has no logger; installing one is `boot`'s job.
    resetLoggingForTests();
    document.body.innerHTML = shellHTML();
    window.history.replaceState(null, "", PAGE_ADDRESS);
    realFetch = globalThis.fetch;
    // A transport that refuses drives the boot straight to its failure path,
    // which is where the guarantee this file exists for is visible. The
    // subject is what the boot does AROUND its first rpc, not the rpc.
    globalThis.fetch = vi.fn(async () => {
      throw new TypeError("the daemon is not answering in this test");
    });
    consoleError = vi.spyOn(console, "error").mockImplementation(() => {});
    vi.resetModules();
  });

  afterEach(() => {
    globalThis.queueMicrotask = realQueueMicrotask;
    globalThis.fetch = realFetch;
    consoleError.mockRestore();
    document.body.innerHTML = "";
  });

  it("draws its failure rather than coming up empty and silent", async () => {
    // `main.ts` boots itself at import time; that top-level call IS the
    // production entry point, so importing it is how a page is booted.
    await import("../src/main.js");
    const card = await awaitBootFailureCard();
    expect(card.textContent).toContain("AdoptWebWorkspace");
    expect(document.querySelector("#failure-overlay")?.hasAttribute("data-empty")).toBe(false);
  }, BOOT_IMPORT_TIMEOUT_MS);

  it("never fails on the logger's own guard", async () => {
    // The specific regression, named. A boot that fails for ANY reason
    // satisfies the test above; this says the reason is never the guard --
    // which is what a reordering would reintroduce, and which the buggy
    // build reported through exactly this console path.
    await import("../src/main.js");
    await awaitBootFailureCard();
    const said = consoleError.mock.calls.flat().join(" ");
    expect(said).not.toContain("the webapp logger is not installed");
  }, BOOT_IMPORT_TIMEOUT_MS);

  it("re-raises the failure it drew, rather than ending quietly", async () => {
    // The other half of "drew its failure": the card is what the READER sees,
    // and this is what the BROWSER sees. A boot that drew the card and then
    // returned normally would leave a page that looks broken to a person and
    // healthy to every error reporter pointed at it.
    await import("../src/main.js");
    await awaitBootFailureCard();
    // The re-raise is scheduled on a microtask after the card is drawn.
    await Promise.resolve();
    expect(reRaised.map((err) => String(err))).toEqual([
      expect.stringContaining("AdoptWebWorkspace") as unknown as string,
    ]);
  }, BOOT_IMPORT_TIMEOUT_MS);
});
