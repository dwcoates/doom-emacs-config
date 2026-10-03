/**
 * The shell: the page's mount points, resolved once.
 *
 * `index.html` ships an empty layout whose ids name where each component
 * draws. Every component then owns its host outright -- it renders its whole
 * view into it on each push and cleans it up on dispose -- so the ONE thing
 * the boot has to get right is handing each mount the element it belongs to.
 *
 * That resolution happens here, once, eagerly. A missing id is a broken build
 * (an edited `index.html`, a stale bundle served against a newer boot), not a
 * runtime condition to cope with: resolving lazily would let the page come up
 * healthy and lose one component silently, so this throws by NAME on the first
 * id it cannot find and the boot fails where the fault is.
 */

import { log } from "./log.js";

/** Every element the boot mounts a component on. */
export interface ShellElements {
  /** The workspaces rail, left of the main column. Ships hidden. */
  sidebar: HTMLElement;
  /** The thin top strip. */
  topbar: HTMLElement;
  /** The standing drain/restart banner under the topbar. */
  drainBanner: HTMLElement;
  /** The scrolling region holding the feed and the hold tray. */
  feedScroll: HTMLElement;
  /** The conversation surface -- the root feed. */
  feed: HTMLElement;
  /** The held-prompt tray at the feed's tail, inside the scroll zone. */
  holdTray: HTMLElement;
  /** The status footer, docked below the scroll zone. */
  footer: HTMLElement;
  /** The docked gate banner's slot, below the footer. Ships hidden. */
  gateDock: HTMLElement;
  /** The dev-mode composer. Ships hidden; production runs composer-less. */
  composer: HTMLElement;
  /** The full-screen login terminal overlay. Ships hidden. */
  loginOverlay: HTMLElement;
  /** The news digest overlay over the feed's box. Ships hidden. */
  newsDigest: HTMLElement;
}

/**
 * The shell's ids, in the order they are resolved. Declaration order is the
 * page's own top-to-bottom order, so the id a failure names is the first one
 * missing as the document reads, which is where a truncated or edited shell
 * usually breaks.
 */
const SHELL_IDS: ReadonlyArray<readonly [keyof ShellElements, string]> = [
  ["sidebar", "ws-sidebar"],
  ["topbar", "topbar"],
  ["drainBanner", "drain-banner"],
  ["feedScroll", "feed-scroll"],
  ["feed", "feed"],
  ["holdTray", "hold-tray"],
  ["footer", "footer"],
  ["gateDock", "gate-dock"],
  ["composer", "composer"],
  ["loginOverlay", "login-overlay"],
  ["newsDigest", "news-digest"],
];

/**
 * Resolve every mount point in DOC. Throws an Error naming the first missing
 * id -- the element is not defaulted, stubbed, or created, because a shell
 * that has to build its own mount points cannot say what else the document is
 * missing.
 */
export function shellElements(doc: Document): ShellElements {
  log.debug("resolving the page shell", {
    operation: "shell.resolve",
    context: { mount_points: SHELL_IDS.length },
  });
  const resolved: Partial<ShellElements> = {};
  for (const [key, id] of SHELL_IDS) {
    const element = doc.getElementById(id);
    if (element === null) {
      // The one branch that selects a materially different outcome, and the
      // boot's first possible failure: logged where the fault is, by name,
      // before the throw carries it up to the pre-overlay emergency path.
      log.error(`the page shell is missing #${id}`, {
        operation: "shell.missing-mount-point",
        context: { id, key },
      });
      throw new Error(`the page shell is missing #${id}`);
    }
    resolved[key] = element;
  }
  return resolved as ShellElements;
}
