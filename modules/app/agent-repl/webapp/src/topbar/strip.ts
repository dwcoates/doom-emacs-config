/**
 * The strip's left and center: the account chip, the connectivity glyph, and
 * the title.
 *
 * THE ACCOUNT LABEL IS THE WARNING (topbar.proto). A session whose config root
 * is logged out cannot run a turn, so `logged_out` draws the words "logged out"
 * in the warning register rather than a blank — a blank reads as "loading",
 * which is the one thing it is not.
 *
 * THE CELL'S CLICK IS THE LOGIN OPTIONS, in both arms (owner ruling,
 * 2026-09-13). It used to open the session line when logged in, which is a
 * different fact and usually an empty one, and the login overlay when logged
 * out; the dropdown `./account.ts` draws answers both — every root the daemon
 * knows, and picking one switches the workspace and then opens that root's
 * login when it has none. The session line stays on the TITLE, which carries
 * it in every account state.
 *
 * THE CONNECTIVITY INDICATOR IS FULLY RESOLVED: the daemon sends the glyph
 * character, the tooltip, and a tone NAME from the shared vocabulary. This end
 * validates the tone against `render-colors.json#topbar_tones` and paints the
 * class — it never maps a connectivity state to a color of its own, because the
 * footer paints the same condition from the same file and the two must agree.
 *
 * THE TITLE IS PRE-COMPOSED. The daemon decides whether a branch is worth
 * showing; this end never concatenates identity fragments.
 */
import { createControl } from "../control.js";
import type {
  TopbarAccount,
  TopbarConnectivity,
  TopbarSessionLine,
  TopbarTitle,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { log } from "../log.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { toneClass, topbarTone } from "../vocab.js";
import type { TopbarContext } from "./context.js";

/** Mark an element as a reveal's anchor, so the layer's outside-click spares it. */
export function asAnchor(element: HTMLElement, name: string): HTMLElement {
  element.setAttribute("data-reveal-anchor", name);
  return element;
}

/**
 * The account chip. THE ARM IS THE STATE.
 *
 * It takes no context: the cell's CLICK is the options dropdown's
 * (`bindAccountReveal`), so the drawing has nothing to call.
 */
export function drawTopbarAccount(u: TopbarAccount): HTMLElement {
  const state = requireCase(u.state, "TopbarAccount.state");
  log.debug("drawing the topbar account", {
    operation: "topbar.account",
    context: { arm: state.case },
  });

  const button = createControl();
  button.className = "topbar-account";
  button.setAttribute("data-arm", state.case);
  button.classList.add(`arm-${state.case}`);

  switch (state.case) {
    case "loggedIn":
      button.textContent = state.value.email;
      button.title = state.value.email;
      return button;
    case "loggedOut":
      // THE LABEL IS THE WARNING. The warning CLASS carries it for the eye:
      // the chip is drawn in the warning register (brief: "logged out ... in
      // the WARNING state — orange"), and the class is what the stylesheet
      // paints from. The remedy is one row down: the cell's dropdown offers
      // this root among the others, and choosing it opens its login.
      button.classList.add("topbar-account-warn");
      button.textContent = "logged out";
      button.title = "this session's account root has no login; click to choose an account";
      return button;
    default: {
      const other: { case: string } = state;
      return unreachableArm("TopbarAccount.state", other.case);
    }
  }
}

/**
 * Wire the TITLE's session-line reveal.
 *
 * The session line names the session the title names, and the title is present
 * in every account state — a logged-out session still has one. So the title
 * opens the same reveal the logged-in chip does: the account chip is the
 * identity door, the title is the session's own.
 */
export function bindTitleSessionReveal(
  title: HTMLElement,
  line: TopbarSessionLine | undefined,
  tc: TopbarContext,
): void {
  if (line === undefined) return;
  asAnchor(title, "session");
  const body = (): HTMLElement => drawTopbarSessionLine(line);
  tc.reveals.register("session", "session", body);
  title.addEventListener("click", () => {
    tc.reveals.toggle("session", "session", body);
  });
}

/** The session identity line, drawn verbatim. */
export function drawTopbarSessionLine(u: TopbarSessionLine): HTMLElement {
  const element = document.createElement("div");
  element.className = "topbar-session-line";
  element.textContent = u.text;
  return element;
}

/**
 * The connectivity glyph: the producer's character, its tooltip, and the tone
 * class the shared vocabulary gives that tone name.
 */
export function drawTopbarConnectivity(u: TopbarConnectivity): HTMLElement {
  const color = topbarTone(u.tone);
  log.debug("drawing the topbar connectivity glyph", {
    operation: "topbar.connectivity",
    context: { tone: u.tone },
  });
  const element = document.createElement("span");
  element.className = `topbar-connectivity ${toneClass(color)}`;
  element.setAttribute("data-tone", u.tone);
  // The glyph rides an attribute as well as the text, so a reader of the DOM
  // sees WHICH glyph was served rather than having to compare rendered text.
  element.setAttribute("data-glyph", u.glyph);
  element.title = u.title;
  element.textContent = u.glyph;
  return element;
}

/** The title, pre-composed by the daemon and drawn verbatim. */
export function drawTopbarTitle(u: TopbarTitle): HTMLElement {
  const element = document.createElement("div");
  element.className = "topbar-title";
  element.title = u.text;
  element.textContent = u.text;
  return element;
}
