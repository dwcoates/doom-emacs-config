// @vitest-environment jsdom
//
// The interceptor resolves clicks through real `closest` walks over rendered
// markup and asserts on `defaultPrevented`, so it needs a real DOM.
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  OPEN_EXTERNAL_PATH,
  externalAnchorFor,
  installExternalLinkInterceptor,
  makeExternalOpener,
} from "../src/external-link.js";

/** Render HTML into a fresh container attached to the document. */
function mount(html: string): HTMLElement {
  const host = document.createElement("div");
  host.innerHTML = html;
  document.body.appendChild(host);
  return host;
}

/** Document-level listeners to unregister after each test. */
const cleanups: Array<() => void> = [];

/**
 * Install the interceptor on `document` and remember its remover.
 *
 * The interceptor listens on the DOCUMENT, so one left installed would still
 * be cancelling clicks in the NEXT test — and, because it cancels first, every
 * later test would see an already-cancelled event and claim nothing.
 */
function install(open: (url: string) => Promise<void>): () => void {
  const remove = installExternalLinkInterceptor(document, open);
  cleanups.push(remove);
  return remove;
}

/**
 * Dispatch a primary-button click on EL, hand the in-flight event to PROBE,
 * and cancel it.
 *
 * The trailing cancellation is jsdom hygiene rather than part of any
 * assertion: jsdom cannot perform a real navigation and logs a "Not
 * implemented" warning for every uncancelled anchor click. Probing DURING
 * dispatch is what lets a test read `defaultPrevented` exactly as production
 * code would see it while still suppressing the navigation afterwards.
 *
 * The probe listens in the CAPTURE phase on `document`, registered after
 * anything the test installed, so an interceptor under test has already run
 * by the time PROBE reads the event.
 */
function probeClick<T>(
  el: Element,
  probe: (e: MouseEvent) => T,
  init: MouseEventInit = {},
): T {
  let result!: T;
  const listener = (evt: Event): void => {
    result = probe(evt as MouseEvent);
    evt.preventDefault();
  };
  document.addEventListener("click", listener, true);
  try {
    el.dispatchEvent(
      new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, ...init }),
    );
  } finally {
    document.removeEventListener("click", listener, true);
  }
  return result;
}

/** What `externalAnchorFor` makes of a click on EL. */
function claimFor(el: Element, init: MouseEventInit = {}): HTMLAnchorElement | null {
  return probeClick(el, externalAnchorFor, init);
}

/** Whether a click on EL was cancelled by whatever ran before the probe. */
function cancelledOn(el: Element, init: MouseEventInit = {}): boolean {
  return probeClick(el, (e) => e.defaultPrevented, init);
}

afterEach(() => {
  while (cleanups.length) cleanups.pop()!();
  document.body.innerHTML = "";
  vi.restoreAllMocks();
});

describe("externalAnchorFor", () => {
  it("claims an https anchor", () => {
    // Arrange.
    const host = mount(`<a href="https://example.com/x">link</a>`);

    // Act.
    const claimed = claimFor(host.querySelector("a")!);

    // Assert.
    expect(claimed?.getAttribute("href")).toBe("https://example.com/x");
  });

  it("claims a click on an element nested inside the anchor", () => {
    // Arrange.
    const host = mount(`<a href="https://example.com/x"><code>inner</code></a>`);

    // Act.
    const claimed = claimFor(host.querySelector("code")!);

    // Assert.
    expect(claimed?.getAttribute("href")).toBe("https://example.com/x");
  });

  it("claims a plain http anchor", () => {
    // Arrange.
    const host = mount(`<a href="http://example.com/">link</a>`);

    // Act.
    const claimed = claimFor(host.querySelector("a")!);

    // Assert.
    expect(claimed).not.toBeNull();
  });

  it("leaves an in-page fragment anchor alone", () => {
    // Arrange.
    const host = mount(`<a href="#section">jump</a>`);

    // Act.
    const claimed = claimFor(host.querySelector("a")!);

    // Assert.
    expect(claimed).toBeNull();
  });

  it("leaves a non-anchor click alone", () => {
    // Arrange.
    const host = mount(`<button>send</button>`);

    // Act.
    const claimed = claimFor(host.querySelector("button")!);

    // Assert.
    expect(claimed).toBeNull();
  });

  it("leaves a modified click to the platform", () => {
    // Arrange.
    const host = mount(`<a href="https://example.com/x">link</a>`);

    // Act.
    const claimed = claimFor(host.querySelector("a")!, { metaKey: true });

    // Assert.
    expect(claimed).toBeNull();
  });

  it("leaves a non-primary button click alone", () => {
    // Arrange.
    const host = mount(`<a href="https://example.com/x">link</a>`);

    // Act.
    const claimed = claimFor(host.querySelector("a")!, { button: 1 });

    // Assert.
    expect(claimed).toBeNull();
  });

  it("leaves an already-cancelled click alone", () => {
    // Arrange. The canceller is registered on `document` in the capture phase
    // BEFORE the probe, so the event reaches the predicate already cancelled.
    const host = mount(`<a href="https://example.com/x">link</a>`);
    const canceller = (e: Event): void => {
      e.preventDefault();
    };
    document.addEventListener("click", canceller, true);
    cleanups.push(() => {
      document.removeEventListener("click", canceller, true);
    });

    // Act.
    const claimed = claimFor(host.querySelector("a")!);

    // Assert.
    expect(claimed).toBeNull();
  });
});

describe("installExternalLinkInterceptor", () => {
  it("cancels the click so the webview cannot navigate", () => {
    // Arrange.
    const host = mount(`<a href="https://example.com/x">link</a>`);
    install(async () => {});

    // Act.
    const cancelled = cancelledOn(host.querySelector("a")!);

    // Assert.
    expect(cancelled).toBe(true);
  });

  it("hands the anchor's href to the opener", () => {
    // Arrange.
    const opened: string[] = [];
    const host = mount(`<a href="https://example.com/x">link</a>`);
    install(async (url) => {
      opened.push(url);
    });

    // Act.
    cancelledOn(host.querySelector("a")!);

    // Assert.
    expect(opened).toEqual(["https://example.com/x"]);
  });

  it("does not cancel or open a fragment link", () => {
    // Arrange.
    const opened: string[] = [];
    const host = mount(`<a href="#section">jump</a>`);
    install(async (url) => {
      opened.push(url);
    });

    // Act.
    const cancelled = cancelledOn(host.querySelector("a")!);

    // Assert.
    expect(cancelled).toBe(false);
    expect(opened).toEqual([]);
  });

  it("still cancels the click when the opener rejects", async () => {
    // Arrange.
    const host = mount(`<a href="https://example.com/x">link</a>`);
    install(() => Promise.reject(new Error("daemon down")));

    // Act.
    const cancelled = cancelledOn(host.querySelector("a")!);
    await Promise.resolve();

    // Assert.
    expect(cancelled).toBe(true);
  });

  it("stops claiming clicks once removed", () => {
    // Arrange.
    const opened: string[] = [];
    const host = mount(`<a href="https://example.com/x">link</a>`);
    const remove = install(async (url) => {
      opened.push(url);
    });

    // Act.
    remove();
    const cancelled = cancelledOn(host.querySelector("a")!);

    // Assert.
    expect(cancelled).toBe(false);
    expect(opened).toEqual([]);
  });
});

describe("makeExternalOpener", () => {
  it("POSTs the url to the daemon's open-external route", async () => {
    // Arrange.
    const calls: Array<{ url: string; init?: RequestInit }> = [];
    const open = makeExternalOpener("http://d:1", async (url, init) => {
      calls.push({ url, init });
      return new Response("{}", { status: 200 });
    });

    // Act.
    await open("https://example.com/x");

    // Assert.
    expect(calls[0]?.url).toBe(`http://d:1${OPEN_EXTERNAL_PATH}`);
    expect(calls[0]?.init?.method).toBe("POST");
    expect(calls[0]?.init?.body).toBe(JSON.stringify({ url: "https://example.com/x" }));
  });

  it("rejects when the daemon refuses the url", async () => {
    // Arrange.
    const open = makeExternalOpener("http://d:1", async () => new Response("nope", { status: 400 }));

    // Act/Assert.
    await expect(open("https://example.com/x")).rejects.toThrow("400");
  });
});
