/**
 * The login overlay: a full-screen terminal over the daemon's login pty.
 *
 * WHY A TERMINAL AND NOT A FORM. The login is a full-screen TUI gated behind
 * stateful prompts (theme onboarding on a fresh config root, folder trust on an
 * untrusted cwd) before it ever reaches OAuth. Nothing can reliably scrape
 * that, which is why the login used to be exiled to an Emacs vterm where a
 * human could read it. The webapp keeps the human and moves the terminal: the
 * daemon owns the pty, this end renders the bytes, and no code anywhere parses
 * the TUI. It survives a CLI redesign for exactly that reason.
 *
 * PER-ACCOUNT IDEMPOTENT: `OpenLogin` on a login already running JOINS it, and
 * the attach replays the scrollback first, so a second viewer sees the whole
 * screen rather than whatever arrives next.
 *
 * ESCAPE DOES NOT CLOSE IT. The TUI needs the key — the reader is inside a
 * terminal, and a modal that eats Escape would make the login unusable at the
 * first prompt that wants it. The close button is the only exit, and it calls
 * `CloseLogin` (closing an absent login is success).
 *
 * WHEN THE `closed` FRAME ARRIVES the overlay closes and NOTHING IS RE-PROBED:
 * the account chip is a field of the topbar view and updates on the topbar's
 * own push. There is no account state on this end to refresh.
 */
import { createControl } from "../control.js";
import { CloseLoginResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_login_pb";
import { OpenLoginResponseSchema } from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_login_pb";
import type { LoginTerminalOutput } from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_login_terminal_pb";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import {
  drawTransportRefusal,
  drawTypedRefusal,
  drawUnreadableRefusal,
  type SentenceTable,
} from "../rpc/refuse.js";
import { connectLoginLink, type LoginLink } from "./link.js";
import { xtermFactory, type LoginTerminalView, type TerminalFactory } from "./terminal.js";

/** What every mount answers with. */
export interface Handle {
  dispose(): void;
}

export interface LoginHandle extends Handle {
  /**
   * Open (or join) the login and raise the overlay.
   *
   * CONTROL is the element that asked for it — the topbar's account chip. A
   * refusal renders AT the clicked control, and the control that made this
   * call belongs to another component, so it is handed in rather than guessed
   * at. Without one the overlay's own header states the refusal, which is
   * where it belongs when nothing else asked.
   */
  open(control?: HTMLElement): void;
}

export interface LoginOverlayDeps {
  /** How a terminal is built; the test fake replaces the xterm bundle. */
  terminalFactory?: TerminalFactory;
  /** The link; injected so a test can script the stream without a transport. */
  link?: LoginLink;
}

/** The causes only OpenLogin can answer with. */
export const OPEN_LOGIN_CAUSES = {
  spawnFailed: (value: { detail: string }) => `the login terminal could not start: ${value.detail}`,
} as unknown as SentenceTable;

/** The causes only SendLoginInput can answer with. */
export const SEND_LOGIN_INPUT_CAUSES = {
  noLoginOpen: () => "no login terminal is open for this workspace",
} as unknown as SentenceTable;

/** CloseLogin has no arms of its own; the four cross-cutting ones are all. */
export const CLOSE_LOGIN_CAUSES = {} as SentenceTable;

/**
 * Mount the overlay on HOST. It ships hidden and costs nothing until opened.
 */
export function mountLoginOverlay(
  host: HTMLElement,
  ctx: AppContext,
  deps: LoginOverlayDeps = {},
): LoginHandle {
  log.debug("mounting the login overlay", { operation: "login.mount" });
  const terminalFactory = deps.terminalFactory ?? xtermFactory;
  const link = deps.link ?? connectLoginLink(ctx);

  host.hidden = true;

  const panel = document.createElement("div");
  panel.className = "login-panel";

  const header = document.createElement("div");
  header.className = "login-header";
  const title = document.createElement("span");
  title.className = "login-title";
  title.textContent = "account login";
  const account = document.createElement("span");
  // THE ACCOUNT ROOT IS NAMED because the two-account split going wrong is
  // most expensive exactly here: logging the wrong root in is invisible after.
  account.className = "login-account";
  const close = createControl();
  close.className = "login-close";
  close.setAttribute("data-login-close", "");
  close.textContent = "close";
  header.append(title, account, close);

  const term = document.createElement("div");
  term.className = "login-term";
  term.setAttribute("data-login-term", "");

  panel.append(header, term);
  host.replaceChildren(panel);

  /** Everything the CURRENT session owns; null while the overlay is closed. */
  let session: {
    controller: AbortController;
    terminal: LoginTerminalView;
    onWindowResize: () => void;
  } | null = null;
  /** Guards a second open while the first is still starting. */
  let opening = false;

  const teardown = (): void => {
    if (session === null) return;
    window.removeEventListener("resize", session.onWindowResize);
    session.controller.abort();
    session.terminal.dispose();
    session = null;
  };

  const hide = (): void => {
    teardown();
    host.hidden = true;
  };

  close.addEventListener("click", () => {
    void closeLogin(ctx, header, hide);
  });

  const start = async (control?: HTMLElement): Promise<void> => {
    if (session !== null || opening) {
      // Per-account idempotent daemon-side, and idempotent here too: a second
      // click while one login is up must not build a second terminal over it.
      log.debug("a login overlay is already open; the click joins it", {
        operation: "login.open-already",
      });
      return;
    }
    opening = true;
    try {
      // The refusal goes to whatever made the call: the chip that was
      // clicked, or the overlay's own header when nothing named itself.
      const configDir = await openLogin(ctx, control ?? header);
      if (configDir === null) return;
      account.textContent = configDir;
      host.hidden = false;

      const terminal = await terminalFactory(term);
      const controller = new AbortController();
      const onWindowResize = (): void => {
        const size = terminal.fit();
        void report(ctx, header, link.sendResize(size.rows, size.cols));
      };
      session = { controller, terminal, onWindowResize };

      terminal.onData((data) => {
        void report(ctx, header, link.sendKeystrokes(data));
      });
      window.addEventListener("resize", onWindowResize);
      // The daemon starts the child wide enough to keep the OAuth URL on one
      // line; this is the viewer's own geometry layered on top of that.
      onWindowResize();
      terminal.focus();

      void pump(ctx, link, controller, terminal, hide);
    } finally {
      opening = false;
    }
  };

  return {
    open(control?: HTMLElement): void {
      void start(control);
    },
    dispose(): void {
      log.debug("disposing the login overlay", { operation: "login.dispose" });
      teardown();
      host.hidden = true;
      host.replaceChildren();
    },
  };
}

/**
 * Ask the daemon to open (or join) the login. Answers the account root, or null
 * when it refused — the refusal is already stated in the header.
 */
export async function openLogin(ctx: AppContext, header: HTMLElement): Promise<string | null> {
  log.info("opening the account login", { operation: "login.open" });
  let response;
  try {
    response = await callUnary(
      ctx,
      "OpenLogin",
      (client) => client.openLogin({ workspace: ctx.workspace }),
      OpenLoginResponseSchema,
    );
  } catch {
    drawTransportRefusal(header);
    return null;
  }
  try {
    const result = requireCase(response.result, "OpenLoginResponse.result");
    switch (result.case) {
      case "success":
        return result.value.configDir;
      case "error":
        drawTypedRefusal(header, "OpenLoginError.cause", "OpenLogin", result.value.cause, OPEN_LOGIN_CAUSES);
        return null;
      default: {
        const other: { case: string } = result;
        return unreachableArm("OpenLoginResponse.result", other.case);
      }
    }
  } catch (err) {
    if (!drawUnreadableRefusal(ctx, header, "login.open-malformed-refusal", err)) throw err;
    return null;
  }
}

/** Ask the daemon to end the login, then close the overlay either way. */
export async function closeLogin(
  ctx: AppContext,
  header: HTMLElement,
  hide: () => void,
): Promise<void> {
  log.info("closing the account login", { operation: "login.close" });
  let response;
  try {
    response = await callUnary(
      ctx,
      "CloseLogin",
      (client) => client.closeLogin({ workspace: ctx.workspace }),
      CloseLoginResponseSchema,
    );
  } catch {
    // The overlay comes down regardless: the reader asked to be rid of it, and
    // leaving a terminal up over a daemon that cannot be reached helps nobody.
    drawTransportRefusal(header);
    hide();
    return;
  }
  try {
    const result = requireCase(response.result, "CloseLoginResponse.result");
    switch (result.case) {
      case "success":
        hide();
        return;
      case "error":
        // STATED, AND STILL CLOSED. The refusal is about the daemon's own
        // record of the login, not about this overlay's right to go away.
        drawTypedRefusal(
          header,
          "CloseLoginError.cause",
          "CloseLogin",
          result.value.cause,
          CLOSE_LOGIN_CAUSES,
        );
        hide();
        return;
      default: {
        const other: { case: string } = result;
        return unreachableArm("CloseLoginResponse.result", other.case);
      }
    }
  } catch (err) {
    if (!drawUnreadableRefusal(ctx, header, "login.close-malformed-refusal", err)) throw err;
    hide();
  }
}

/**
 * Drive the output stream into the terminal until it concludes.
 *
 * THE `closed` FRAME IS THE END, and it is a legitimate one — the login child
 * exited. A stream that ends any OTHER way is a transport failure and is
 * reported as `control_plane_failed`: the reader is staring at a terminal that
 * has silently stopped updating, which is the one state a login must never be
 * left in.
 */
export async function pump(
  ctx: AppContext,
  link: LoginLink,
  controller: AbortController,
  terminal: LoginTerminalView,
  hide: () => void,
): Promise<void> {
  let concluded = false;
  try {
    for await (const frame of link.attach(controller.signal)) {
      if (handleFrame(frame, terminal)) {
        concluded = true;
        break;
      }
    }
  } catch (err) {
    if (controller.signal.aborted) return;
    const cause = err instanceof Error ? err.message : String(err);
    log.error(`the login terminal stream failed: ${cause}`, {
      operation: "login.stream-failed",
      context: { cause },
    });
    ctx.failures.report(controlPlaneFailed("login terminal", cause));
    hide();
    return;
  }
  if (controller.signal.aborted) return;
  if (!concluded) {
    // The producer ended without saying the child exited. That is not a login
    // that finished; it is a link that went away mid-login.
    log.error("the login terminal stream ended without a closed frame", {
      operation: "login.stream-ended-early",
    });
    ctx.failures.report(controlPlaneFailed("login terminal", "the stream ended without a closed frame"));
  }
  hide();
}

/** One frame. Answers whether it was the terminal one. */
export function handleFrame(frame: LoginTerminalOutput, terminal: LoginTerminalView): boolean {
  const output = requireCase(frame.output, "LoginTerminalOutput.output");
  switch (output.case) {
    case "bytes":
      terminal.write(output.value.data);
      return false;
    case "closed":
      log.info("the login child exited; closing the overlay", {
        operation: "login.child-exited",
      });
      return true;
    default: {
      const other: { case: string } = output;
      return unreachableArm("LoginTerminalOutput.output", other.case);
    }
  }
}

/**
 * Await one `SendLoginInput` and state its refusal in the header.
 *
 * Keystrokes are fire-and-forget from the terminal's point of view, but a
 * refusal must not be: input that never reached the pty leaves the reader
 * typing into a screen that does not answer.
 */
export async function report(
  ctx: AppContext,
  header: HTMLElement,
  pending: Promise<{ result: { case?: string; value?: unknown } }>,
): Promise<void> {
  let response;
  try {
    response = await pending;
  } catch {
    drawTransportRefusal(header);
    return;
  }
  try {
    const result = requireCase(response.result, "SendLoginInputResponse.result");
    switch (result.case) {
      case "success":
        return;
      case "error":
        drawTypedRefusal(
          header,
          "SendLoginInputError.cause",
          "SendLoginInput",
          (result.value as { cause: { case?: string; value?: unknown } }).cause,
          SEND_LOGIN_INPUT_CAUSES,
        );
        return;
      default:
        return unreachableArm("SendLoginInputResponse.result", result.case);
    }
  } catch (err) {
    if (!drawUnreadableRefusal(ctx, header, "login.input-malformed-refusal", err)) throw err;
  }
}
