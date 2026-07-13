/**
 * The xterm.js binding for the login terminal.
 *
 * Kept apart from login.ts on purpose. xterm ships as a browser bundle that
 * throws on import under the test runner ("self is not defined"), so every
 * decision worth asserting on — the wire frames, the notices, the URLs —
 * lives in login.ts where it can be tested, and this file is the thin glue
 * that cannot be.
 *
 * Nothing here interprets the terminal. Bytes go out to the screen and
 * keystrokes come back, exactly as they did through the vterm.
 */

import { FitAddon } from "@xterm/addon-fit";
import { Terminal } from "@xterm/xterm";
// xterm renders nothing legible without its own stylesheet. Imported beside
// the component that needs it rather than from main.ts, so the dependency
// travels with the only module that has it.
import "@xterm/xterm/css/xterm.css";

import {
  decodeTerminalChunk,
  encodeKeystrokes,
  loginTerminalUrl,
  resizeFrame,
} from "./login.js";

/** A live login terminal. */
export interface LoginTerminal {
  /** Tear down the socket and the renderer. Idempotent. */
  dispose(): void;
}

/** What `attachLoginTerminal` needs from its surroundings. */
export interface LoginTerminalDeps {
  /** Opens the terminal socket. Defaults to the global WebSocket. */
  openSocket?: (url: string) => WebSocket;
  /** Called when the login child goes away (the socket closed). */
  onClosed?: () => void;
}

/**
 * Mount the login terminal into HOST and wire it to the daemon.
 *
 * Binary frames carry the pty in and the keystrokes out. The one text frame
 * is the geometry report, which is load-bearing rather than cosmetic: the
 * TUI hard-wraps at the column count and the OAuth URL runs ~350 characters.
 */
export function attachLoginTerminal(
  host: HTMLElement,
  wsBase: string,
  sessionId: string,
  deps: LoginTerminalDeps = {},
): LoginTerminal {
  const open = deps.openSocket ?? ((url: string) => new WebSocket(url));

  const term = new Terminal({
    cursorBlink: true,
    fontSize: 13,
    fontFamily:
      'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace',
    theme: { background: "#11151c", foreground: "#c5c8c6" },
  });
  const fit = new FitAddon();
  term.loadAddon(fit);
  term.open(host);

  const socket = open(loginTerminalUrl(wsBase, sessionId));
  socket.binaryType = "arraybuffer";

  const reportSize = (): void => {
    // Only once the socket is up: sending into a CONNECTING socket throws.
    // The daemon starts the child wide enough to keep the URL on one line
    // until the first real report lands, so nothing is lost by waiting.
    if (socket.readyState !== WebSocket.OPEN) return;
    socket.send(resizeFrame(term.rows, term.cols));
  };

  socket.addEventListener("open", () => {
    fit.fit();
    reportSize();
  });

  socket.addEventListener("message", (e: MessageEvent) => {
    const chunk = decodeTerminalChunk(e.data);
    if (chunk !== null) term.write(chunk);
  });

  socket.addEventListener("close", () => deps.onClosed?.());

  term.onData((chunk) => {
    if (socket.readyState !== WebSocket.OPEN) return;
    socket.send(encodeKeystrokes(chunk));
  });

  const onWindowResize = (): void => {
    fit.fit();
    reportSize();
  };
  window.addEventListener("resize", onWindowResize);

  // Focus so the user can answer the TUI's prompts without hunting for a
  // click target — the whole point is that this behaves like the terminal
  // it replaced.
  term.focus();

  let disposed = false;
  return {
    dispose(): void {
      if (disposed) return;
      disposed = true;
      window.removeEventListener("resize", onWindowResize);
      socket.close();
      term.dispose();
    },
  };
}
