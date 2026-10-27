/**
 * presence — WHETHER ANYONE CAN SEE THIS PAGE, from both of the things that
 * decide it.
 *
 * A workspace page lives in an iframe of Emacs's one host web view
 * (host/protocol.ts). Two facts decide whether the reader can see it:
 *
 *   - the HOST shows it: only the page of the workspace the user stands in is
 *     shown; every other page is held, live, behind it. The host tells the page
 *     (embed.ts). A page NOT in a host (opened on its own, as tests and dev mode
 *     do) is shown by definition;
 *   - the DOCUMENT is visible: WebKit reports the whole web view hidden when
 *     Emacs is not on screen.
 *
 * A page is SEEN when both say yes. Everything that used to read
 * `document.visibilityState` to decide whether a reader can see the page
 * (the rail's unseen hold, the auto-collapse on leaving, the rail's wall-clock
 * phase) reads this instead, because inside the host every page's document is
 * visible whenever the host is, shown or not.
 */
import { createListeners } from "./listeners.js";
import { perDocument } from "./per-document.js";

/** What changed when a listener is told. */
export type PresenceCause = "visibility" | "host";

/** Told when the page's seen state may have changed. */
type PresenceListener = (seen: boolean, cause: PresenceCause) => void;

/** The page's presence, as its readers see it. */
export interface Presence {
  /** Whether the reader can see the page now. */
  seen(): boolean;
  /** Whether the host shows the page (always, for a page in no host). */
  shown(): boolean;
  /** Tell LISTENER on every change of either fact; answers the unsubscribe. */
  onChange(listener: PresenceListener): () => void;
  /** Whether the page sits in the webhost (its iframe), rather than on its own. */
  readonly embedded: boolean;
  /**
   * When the user's switch that made the current show began (wall-clock Unix
   * milliseconds), or null when it was not a switch the user made in Emacs, or
   * the page is hidden. Set before listeners hear the show.
   */
  switchStartedAt(): number | null;
  /**
   * When the reload that loaded this page began (wall-clock Unix
   * milliseconds), or null when no reload loaded it.
   */
  reloadStartedAt(): number | null;
}

/** The presence plus the one writer the host link holds. */
export interface PresenceSource extends Presence {
  /**
   * The host showed (true) or hid (false) this page; a show the user's switch
   * made carries when that switch began (switchStartedAt).
   */
  setShown(shown: boolean, switchStartedAtMs?: number | null): void;
  /** The host said a reload that began at STARTEDATMS loaded this page. */
  setReloadStartedAt(startedAtMs: number): void;
  /** Stop listening to the document. */
  dispose(): void;
}

/**
 * The presence of the page in DOC. EMBEDDED is whether the page sits in a host:
 * an embedded page is not shown until the host says it is.
 */
export function createPresence(doc: Document, embedded: boolean): PresenceSource {
  let hostShown = !embedded;
  let switchStartedAtMs: number | null = null;
  let reloadStartedAtMs: number | null = null;
  const listeners = createListeners<Parameters<PresenceListener>>();
  const visible = (): boolean => doc.visibilityState === "visible";
  const seen = (): boolean => hostShown && visible();
  const tell = (cause: PresenceCause): void => {
    const now = seen();
    listeners.notify(now, cause);
  };
  const onVisibility = (): void => tell("visibility");
  doc.addEventListener("visibilitychange", onVisibility);
  return {
    embedded,
    seen,
    shown: () => hostShown,
    switchStartedAt: () => switchStartedAtMs,
    reloadStartedAt: () => reloadStartedAtMs,
    onChange(listener) {
      return listeners.add(listener);
    },
    setShown(shown, startedAtMs = null) {
      if (shown === hostShown) return;
      hostShown = shown;
      switchStartedAtMs = shown ? startedAtMs : null;
      tell("host");
    },
    setReloadStartedAt(startedAtMs) {
      reloadStartedAtMs = startedAtMs;
    },
    dispose() {
      doc.removeEventListener("visibilitychange", onVisibility);
      listeners.clear();
    },
  };
}

/** The presence of each page, keyed by its document: one per page. */
const presenceOf = perDocument((doc) => createPresence(doc, pageIsEmbedded(window)));

/** Whether this page runs inside another page's iframe (the host's). */
export function pageIsEmbedded(win: Window): boolean {
  return win.parent !== win;
}

/** The presence of this page: one per page (per document), made on first use. */
export function pagePresence(): PresenceSource {
  return presenceOf(document);
}
