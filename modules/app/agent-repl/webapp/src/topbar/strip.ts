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
 * NEITHER LINE CHANGES WITH STATE (owner ruling, 2026-11-25). The summary is
 * always the summary's purple and never underlined; the name is always the
 * foreground white. A hover on the summary shows the workspace's long summary
 * beneath it (`TopbarTitle.description`).
 *
 * THE NAME OPENS THE WORKSPACE'S SHOWN LINKS, when it carries them, and is
 * then the one underlined line. The daemon's arm says how: one link is opened
 * directly, through the one external-link component, so the click is
 * OpenExternal (the Chrome profile of the session's account) and never a
 * navigation; several drop a menu under the heading whose rows are that same
 * component. Either click stops propagating, so the heading's session-line
 * reveal does not also open.
 */
import { createControl } from "../control.js";
import type {
  TopbarAccount,
  TopbarConnectivity,
  TopbarSessionLine,
  TopbarTitle,
  TopbarTitleDescription,
  TopbarWorkspaceNameLinkMenu,
  TopbarWorkspaceName,
} from "../../../proto/gen/ts/frontend/v1/topbar_pb";
import { renderExternalLink } from "../link.js";
import { drawLinkBadge, drawLinkMarks } from "./link-badge.js";
import { drawLinkIcon } from "./link-icon.js";
import { createLogger } from "../log.js";
import { requireCase, unreachableArm } from "../rpc/strict.js";
import { toneClass, topbarTone } from "../vocab.js";
import { drawNamePrefix } from "../name-prefix.js";
import type { TopbarContext } from "./context.js";

const log = createLogger("webapp.topbar.strip");

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
  heading.append(drawTopbarWorkspaceName(name, tc));
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

/** The name's link menu's name in the strip's one reveal layer. */
export const LINKS_REVEAL = "links";

/** The summary's hover popup's name in the strip's one reveal layer. */
export const DESCRIPTION_REVEAL = "description";

/** A menu row whose link the workspace's work made: drawn on a lighter ground, its label in the summary's purple. */
export const OWNED_LINK_CLASS = "is-owned";
/** A menu row that carries a pull request badge, which reserves its margin. */
export const BADGED_LINK_CLASS = "has-badge";
/** A link-menu row's first line: its label and the marks beside it. */
export const LINK_HEAD_CLASS = "topbar-name-link-head";

/**
 * The title: the workspace's short summary, drawn verbatim, always in the
 * summary's purple. With a description, a hover on it shows the long summary
 * beneath it, and the popup replaces the line's own tooltip.
 */
export function drawTopbarTitle(u: TopbarTitle, tc: TopbarContext): HTMLElement {
  const element = drawHeadingLine("topbar-title", u.text);
  if (u.description !== undefined) bindTitleDescription(element, u.description, tc);
  return element;
}

/**
 * Wire the summary's hover to its long summary: the popup opens while the
 * pointer rests on the summary and closes when it leaves. It is registered as
 * it is drawn, so a push arriving while it is open re-opens it with THIS
 * push's text.
 */
export function bindTitleDescription(element: HTMLElement, description: TopbarTitleDescription, tc: TopbarContext): void {
  element.classList.add("topbar-title-described");
  element.removeAttribute("title");
  asAnchor(element, DESCRIPTION_REVEAL);
  const body = (): HTMLElement => drawTitleDescription(description);
  tc.reveals.register(DESCRIPTION_REVEAL, DESCRIPTION_REVEAL, body);
  element.addEventListener("pointerenter", () => {
    tc.reveals.open(DESCRIPTION_REVEAL, DESCRIPTION_REVEAL, body);
  });
  element.addEventListener("pointerleave", () => {
    if (tc.reveals.current() === DESCRIPTION_REVEAL) tc.reveals.close();
  });
}

/** The summary's hover popup: the long summary, drawn verbatim. */
export function drawTitleDescription(u: TopbarTitleDescription): HTMLElement {
  const element = document.createElement("div");
  element.className = "topbar-title-description";
  element.textContent = u.text;
  return element;
}

/**
 * Wire the name's click to the menu of its shown links. The click is
 * claimed, so the heading's session-line reveal does not also toggle.
 */
export function bindNameLinkMenu(element: HTMLElement, menu: TopbarWorkspaceNameLinkMenu, tc: TopbarContext): void {
  element.classList.add("topbar-name-menu");
  asAnchor(element, LINKS_REVEAL);
  const body = (): HTMLElement => drawNameLinkMenu(menu, tc);
  // Registered as it is drawn, so a push arriving while the menu is open
  // re-opens it with THIS push's links rather than the previous push's.
  tc.reveals.register(LINKS_REVEAL, LINKS_REVEAL, body);
  element.addEventListener("click", (event) => {
    event.stopPropagation();
    tc.reveals.toggle(LINKS_REVEAL, LINKS_REVEAL, body);
  });
}

/**
 * The name's link menu: one row per shown link, in the served order (most
 * recently set first), each the label drawn as its link with the url beneath
 * it as the row's dim second line, the way a model's description sits under
 * its name. A click anywhere in a row closes the menu as it opens the link.
 * The daemon serves the owned links first; an owned row is tinted and its
 * label drawn in the summary's purple, a row whose host has a known service
 * leads with that service's icon (never tinted), the label is followed by its
 * "owned" and "subscribed" marks, and a pull request's row carries its
 * state's badge in its right margin.
 */
export function drawNameLinkMenu(menu: TopbarWorkspaceNameLinkMenu, tc: TopbarContext): HTMLElement {
  const list = document.createElement("div");
  list.className = "topbar-name-link-options list-rows";
  log.debug("drawing the name's link menu", {
    operation: "topbar.name-link-menu",
    context: { items: menu.items.length },
  });
  for (const item of menu.items) {
    const row = document.createElement("div");
    row.className = "topbar-name-link-option";
    row.title = item.url;
    const link = renderExternalLink(tc.ctx, { text: item.label, url: item.url });
    link.addEventListener("click", () => tc.reveals.close());
    const icon = drawLinkIcon(item.icon);
    if (icon !== null) link.prepend(icon);
    // The daemon serves owned rows first and flags them: the flag only tints.
    row.classList.toggle(OWNED_LINK_CLASS, item.owned);
    // The whole row opens its link: a click on the row's padding or its url
    // line is relayed to the label's own click, so the one open path (and its
    // refusal drawing) is the link's. A click on the label itself already
    // opened it.
    row.addEventListener("click", (event) => {
      if (link.contains(event.target as Node)) return;
      link.click();
    });
    // The row's first line: the label, then its marks beside it.
    const head = document.createElement("div");
    head.className = LINK_HEAD_CLASS;
    head.append(link, ...drawLinkMarks(item));
    const url = document.createElement("span");
    url.className = "topbar-name-link-url";
    url.textContent = item.url;
    row.append(head, url);
    const badge = drawLinkBadge(item.badge);
    if (badge !== null) {
      row.classList.add(BADGED_LINK_CLASS);
      row.append(badge);
    }
    list.append(row);
  }
  return list;
}

/**
 * The workspace's name, drawn verbatim on the heading's second line, after
 * the marks the daemon puts before it (`name-prefix.ts`), always in the
 * foreground white. With shown links it is underlined and its click opens
 * them: one directly, with the tooltip naming its label and url beneath the
 * name, several through their menu. The tooltip is otherwise the name alone.
 */
export function drawTopbarWorkspaceName(u: TopbarWorkspaceName, tc: TopbarContext): HTMLElement {
  const line = drawHeadingLine("topbar-workspace-name", u.text);
  if (u.links !== undefined) {
    line.classList.add("topbar-name-linked");
    const opens = requireCase(u.links.opens, "TopbarWorkspaceNameLinks.opens");
    switch (opens.case) {
      case "single": {
        line.title = `${u.text}\n${opens.value.label}\n${opens.value.url}`;
        line.replaceChildren(renderExternalLink(tc.ctx, { text: u.text, url: opens.value.url }));
        line.addEventListener("click", (event) => event.stopPropagation());
        break;
      }
      case "menu":
        bindNameLinkMenu(line, opens.value, tc);
        break;
      default: {
        const other: { case: string } = opens;
        return unreachableArm("TopbarWorkspaceNameLinks.opens", other.case);
      }
    }
  }
  const prefix = drawNamePrefix(u.prefix, "TopbarWorkspaceName.prefix");
  if (prefix !== undefined) line.prepend(prefix);
  return line;
}
