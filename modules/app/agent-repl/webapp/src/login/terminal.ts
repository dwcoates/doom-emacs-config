/**
 * The xterm.js binding, kept apart from `login.ts` on purpose.
 *
 * xterm ships as a browser bundle that THROWS ON IMPORT under the test runner
 * ("self is not defined"), so every decision worth asserting on — the verbs,
 * the frames, the refusals, the overlay's lifecycle — lives in `login.ts` where
 * it can be tested, and this file is the thin glue that cannot be. It is loaded
 * lazily for the same reason: a page that never logs in never pays for the
 * bundle, and a suite that never opens the overlay never imports it.
 *
 * NOTHING HERE INTERPRETS THE TERMINAL. Bytes go to the screen and keystrokes
 * come back, exactly as they did through the vterm this replaces.
 */

/** The slice of a terminal the overlay drives. */
export interface LoginTerminalView {
  /** Write pty bytes to the screen. */
  write(data: Uint8Array): void;
  /** Report the reader's keystrokes as bytes. */
  onData(fn: (data: Uint8Array) => void): void;
  /** Fit to the host and answer the resulting geometry. */
  fit(): { rows: number; cols: number };
  focus(): void;
  dispose(): void;
}

/** How the overlay obtains a terminal; replaced in tests by a fake. */
export type TerminalFactory = (host: HTMLElement) => Promise<LoginTerminalView>;

/** The real one: xterm plus the fit addon, imported only when opened. */
export const xtermFactory: TerminalFactory = async (host) => {
  const [{ Terminal }, { FitAddon }] = await Promise.all([
    import("@xterm/xterm"),
    import("@xterm/addon-fit"),
  ]);
  // xterm renders nothing legible without its own stylesheet; it travels with
  // the only module that needs it.
  await import("@xterm/xterm/css/xterm.css");

  const term = new Terminal({
    cursorBlink: true,
    fontSize: 13,
    fontFamily: 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace',
    theme: { background: "#11151c", foreground: "#c5c8c6" },
  });
  const fit = new FitAddon();
  term.loadAddon(fit);
  term.open(host);

  const encoder = new TextEncoder();
  return {
    write: (data) => term.write(data),
    onData: (fn) => {
      term.onData((chunk) => fn(encoder.encode(chunk)));
    },
    fit: () => {
      fit.fit();
      return { rows: term.rows, cols: term.cols };
    },
    focus: () => term.focus(),
    dispose: () => term.dispose(),
  };
};
