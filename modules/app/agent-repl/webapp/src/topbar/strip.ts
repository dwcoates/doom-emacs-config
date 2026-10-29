/**
 * The strip's left and center: the account chip, the connectivity glyph, and
 * the heading (the summary over the workspace's name).
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
 * login when it has none. The session line stays on the HEADING, which carries
 * it in every account state.
 *
 * THE CONNECTIVITY INDICATOR IS FULLY RESOLVED: the daemon sends the glyph
 * character, the tooltip, and a tone NAME from the shared vocabulary. This end
 * validates the tone against `render-colors.json#topbar_tones` and paints the
 * class — it never maps a connectivity state to a color of its own, because the
 * footer paints the same condition from the same file and the two must agree.
 *
 * THE TITLE IS THE SUMMARY, DRAWN VERBATIM, AND THE NAME IS ITS OWN LINE. The
 * daemon states the summary only when one exists and never folds the
 * workspace's name into it; this end never concatenates identity fragments and
 * never stands the name in for a missing summary.
 *
 * THE SUMMARY OPENS THE WORKSPACE'S SHOWN LINKS, when the title carries them,
 * and the daemon's arm says how: one link is opened directly, through the one
 * external-link component, so the click is OpenExternal (the Chrome profile of
 * the session's account) and never a navigation; several drop a menu under the
 * heading whose rows are that same component. Either click stops propagating,
 * so the heading's session-line reveal does not also open. With no summary
 * there is no title, so nothing is clickable.
 */
import { createControl } from "../control.js";
import type {
  TopbarAccount,
  TopbarConnectivity,
  TopbarSessionLine,
  TopbarTitle,
  TopbarTitleLinkMenu,
  TopbarWorkspaceName,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { renderExternalLink } from "../link.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
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
 * Wire the HEADING's session-line reveal.
 *
 * The session line names the session the heading names, and the heading is
 * present in every account state — a logged-out session still has one, and a
 * workspace with no summary yet still draws its name line. So the heading opens
 * the same reveal the logged-in chip does: the account chip is the identity
 * door, the heading is the session's own.
 */
export function bindHeadingSessionReveal(
  heading: HTMLElement,
  line: TopbarSessionLine | undefined,
  tc: TopbarContext,
): void {
  if (line === undefined) return;
  asAnchor(heading, "session");
  const body = (): HTMLElement => drawTopbarSessionLine(line);
  tc.reveals.register("session", "session", body);
  heading.addEventListener("click", () => {
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

/**
 * The heading: the summary on the strip's band, and the workspace's name on the
 * line beneath it. The heading box hugs its lines and each is centered in it,
 * so the name's midpoint is the summary's midpoint rather than the strip's.
 * With no summary (the daemon left the title absent) only the name is drawn,
 * and the stylesheet sets it in the summary's place on the band.
 */
export function drawTopbarHeading(
  title: TopbarTitle | undefined,
  name: TopbarWorkspaceName,
  tc: TopbarContext,
): HTMLElement {
  const heading = document.createElement("div");
  heading.className = "topbar-heading";
  if (title !== undefined) heading.append(drawTopbarTitle(title, tc));
  heading.append(drawTopbarWorkspaceName(name));
  return heading;
}

/**
 * One heading line of daemon text, drawn verbatim. The strip ellipsis-clips
 * every heading line, so the whole text also rides the tooltip.
 */
export function drawHeadingLine(className: string, text: string): HTMLElement {
  const element = document.createElement("div");
  element.className = className;
  element.title = text;
  element.textContent = text;
  return element;
}

/** The summary's menu's name in the strip's one reveal layer. */
export const LINKS_REVEAL = "links";

/**
 * The title: the workspace's generated summary, drawn verbatim. With one shown
 * link the summary's text is that link, and the tooltip names its label and
 * url beneath the whole summary. With several, a click on the summary drops
 * their menu.
 */
export function drawTopbarTitle(u: TopbarTitle, tc: TopbarContext): HTMLElement {
  const element = drawHeadingLine("topbar-title", u.text);
  if (u.links === undefined) return element;
  element.classList.add("topbar-title-linked");
  const opens = requireCase(u.links.opens, "TopbarTitleLinks.opens");
  switch (opens.case) {
    case "single":
      element.title = `${u.text}\n${opens.value.label}\n${opens.value.url}`;
      element.replaceChildren(renderExternalLink(tc.ctx, { text: u.text, url: opens.value.url }));
      return element;
    case "menu":
      bindTitleLinkMenu(element, opens.value, tc);
      return element;
    default: {
      const other: { case: string } = opens;
      return unreachableArm("TopbarTitleLinks.opens", other.case);
    }
  }
}

/**
 * Wire the summary's click to the menu of its shown links. The click is
 * claimed, so the heading's session-line reveal does not also toggle.
 */
export function bindTitleLinkMenu(element: HTMLElement, menu: TopbarTitleLinkMenu, tc: TopbarContext): void {
  element.classList.add("topbar-title-menu");
  asAnchor(element, LINKS_REVEAL);
  const body = (): HTMLElement => drawTitleLinkMenu(menu, tc);
  // Registered as it is drawn, so a push arriving while the menu is open
  // re-opens it with THIS push's links rather than the previous push's.
  tc.reveals.register(LINKS_REVEAL, LINKS_REVEAL, body);
  element.addEventListener("click", (event) => {
    event.stopPropagation();
    tc.reveals.toggle(LINKS_REVEAL, LINKS_REVEAL, body);
  });
}

/**
 * The summary's menu: one row per shown link, in the served order (most
 * recently set first), each the label drawn as its link with the url beneath
 * it as the row's dim second line, the way a model's description sits under
 * its name. A click anywhere in a row closes the menu as it opens the link.
 */
export function drawTitleLinkMenu(menu: TopbarTitleLinkMenu, tc: TopbarContext): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-title-link-options list-rows";
  log.debug("drawing the summary's link menu", {
    operation: "topbar.title-link-menu",
    context: { items: menu.items.length },
  });
  for (const item of menu.items) {
    const row = document.createElement("div");
    row.className = "topbar-title-link-option";
    row.title = item.url;
    const link = renderExternalLink(tc.ctx, { text: item.label, url: item.url });
    link.addEventListener("click", () => tc.reveals.close());
    // The whole row opens its link: a click on the row's padding or its url
    // line is relayed to the label's own click, so the one open path (and its
    // refusal drawing) is the link's. A click on the label itself already
    // opened it.
    row.addEventListener("click", (event) => {
      if (link.contains(event.target as Node)) return;
      link.click();
    });
    const url = document.createElement("span");
    url.className = "topbar-title-link-url";
    url.textContent = item.url;
    row.append(link, url);
    list.append(row);
  }
  return list;
}

/** The workspace's name, drawn verbatim on the heading's second line. */
export function drawTopbarWorkspaceName(u: TopbarWorkspaceName): HTMLElement {
  return drawHeadingLine("topbar-workspace-name", u.text);
}
