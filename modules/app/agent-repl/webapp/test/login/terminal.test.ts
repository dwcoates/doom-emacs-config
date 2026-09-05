// @vitest-environment jsdom
//
// The xterm binding. The real bundle THROWS ON IMPORT under the runner
// ("self is not defined"), so the three modules `xtermFactory` reaches for are
// mocked at file scope and the wrapper's OWN logic is what is asserted: the
// addon loaded before the host is opened, the keystroke encoding, the
// post-fit geometry, and the fact that nothing is imported until it is called.
import { beforeEach, describe, expect, it, vi } from "vitest";

/** What the mocked constructors hand back, plus the lazy-import counter. */
const state = vi.hoisted(() => ({
  imported: 0,
  term: null as null | FakeTerm,
  fit: null as null | FakeFit,
  options: null as null | Record<string, unknown>,
}));

interface FakeTerm {
  calls: string[];
  rows: number;
  cols: number;
  opened: HTMLElement | null;
  addons: unknown[];
  written: Uint8Array[];
  dataHandler: ((chunk: string) => void) | null;
  focused: number;
  disposed: number;
  loadAddon(addon: unknown): void;
  open(host: HTMLElement): void;
  write(data: Uint8Array): void;
  onData(fn: (chunk: string) => void): void;
  focus(): void;
  dispose(): void;
}

interface FakeFit {
  fitCalls: number;
  fit(): void;
}

vi.mock("@xterm/xterm", () => {
  state.imported += 1;
  return {
    Terminal: function Terminal(options: Record<string, unknown>) {
      state.options = options;
      return state.term;
    } as unknown as new (options: unknown) => unknown,
  };
});

vi.mock("@xterm/addon-fit", () => {
  state.imported += 1;
  return {
    FitAddon: function FitAddon() {
      return state.fit;
    } as unknown as new () => unknown,
  };
});

vi.mock("@xterm/xterm/css/xterm.css", () => {
  state.imported += 1;
  return {};
});

import { xtermFactory } from "../../src/login/terminal.js";

/**
 * Captured at FILE LOAD, before any test body has run: importing the wrapper
 * must not have pulled the xterm bundle in. Read at load rather than inside a
 * test so the assertion holds whatever order the tests are shuffled into.
 */
const importsAtModuleLoad = state.imported;

function fakeTerm(): FakeTerm {
  const term: FakeTerm = {
    calls: [],
    rows: 10,
    cols: 20,
    opened: null,
    addons: [],
    written: [],
    dataHandler: null,
    focused: 0,
    disposed: 0,
    loadAddon(addon) {
      term.calls.push("loadAddon");
      term.addons.push(addon);
    },
    open(host) {
      term.calls.push("open");
      term.opened = host;
    },
    write(data) {
      term.written.push(data);
    },
    onData(fn) {
      term.dataHandler = fn;
    },
    focus() {
      term.focused += 1;
    },
    dispose() {
      term.disposed += 1;
    },
  };
  return term;
}

/** A fit addon that RESIZES the terminal, so "after the fit" is observable. */
function fakeFit(term: FakeTerm): FakeFit {
  const fit: FakeFit = {
    fitCalls: 0,
    fit() {
      fit.fitCalls += 1;
      term.rows = 41;
      term.cols = 111;
    },
  };
  return fit;
}

let host: HTMLElement;

beforeEach(() => {
  const term = fakeTerm();
  state.term = term;
  state.fit = fakeFit(term);
  state.options = null;
  host = document.createElement("div");
  document.body.replaceChildren(host);
});

describe("xtermFactory", () => {
  it("imports nothing until it is called, so a page that never logs in never pays", () => {
    // ARRANGE / ACT: the file's own static import of the wrapper, at load.
    // ASSERT
    expect(importsAtModuleLoad).toBe(0);
  });

  it("imports the terminal, the addon and the stylesheet when it IS called", async () => {
    // ARRANGE / ACT
    await xtermFactory(host);
    // ASSERT: exactly the three modules, each imported once (the registry
    // memoizes them, so this holds however many tests ran first).
    expect(state.imported).toBe(3);
  });

  it("opens the terminal on the host element it was handed", async () => {
    // ARRANGE / ACT
    await xtermFactory(host);
    // ASSERT
    expect(state.term!.opened).toBe(host);
  });

  it("loads the fit addon into the terminal", async () => {
    // ARRANGE / ACT
    await xtermFactory(host);
    // ASSERT
    expect(state.term!.addons).toEqual([state.fit]);
  });

  it("loads the addon BEFORE opening, so the first fit has a sized host", async () => {
    // ARRANGE / ACT
    await xtermFactory(host);
    // ASSERT
    expect(state.term!.calls).toEqual(["loadAddon", "open"]);
  });

  it("writes the exact bytes it was handed to the terminal", async () => {
    // ARRANGE
    const view = await xtermFactory(host);
    const data = new Uint8Array([27, 91, 65]);
    // ACT
    view.write(data);
    // ASSERT
    expect(state.term!.written).toEqual([data]);
  });

  it("encodes a keystroke chunk to UTF-8 bytes rather than passing the string", async () => {
    // ARRANGE: a multi-byte character, so a pass-through would be visible.
    const view = await xtermFactory(host);
    const seen: number[][] = [];
    view.onData((data) => seen.push(Array.from(data)));
    // ACT
    state.term!.dataHandler!("é");
    // ASSERT
    expect(seen).toEqual([[0xc3, 0xa9]]);
  });

  it("reports the geometry AFTER fitting, not the geometry it had before", async () => {
    // ARRANGE: the fake fit resizes 10x20 to 41x111.
    const view = await xtermFactory(host);
    // ACT
    const size = view.fit();
    // ASSERT
    expect(size).toEqual({ rows: 41, cols: 111 });
  });

  it("builds the terminal with the overlay's own options", async () => {
    // ARRANGE / ACT
    await xtermFactory(host);
    // ASSERT: the blinking cursor a login TUI needs, at the overlay's size.
    expect([state.options?.cursorBlink, state.options?.fontSize]).toEqual([true, 13]);
  });

  it("asks the addon to fit exactly once per fit", async () => {
    // ARRANGE
    const view = await xtermFactory(host);
    // ACT
    view.fit();
    // ASSERT
    expect(state.fit!.fitCalls).toBe(1);
  });

  it("focus reaches the terminal", async () => {
    // ARRANGE
    const view = await xtermFactory(host);
    // ACT
    view.focus();
    // ASSERT
    expect(state.term!.focused).toBe(1);
  });

  it("dispose reaches the terminal", async () => {
    // ARRANGE
    const view = await xtermFactory(host);
    // ACT
    view.dispose();
    // ASSERT
    expect(state.term!.disposed).toBe(1);
  });
});
