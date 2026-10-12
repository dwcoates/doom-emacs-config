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
}

/** The presence plus the one writer the host link holds. */
export interface PresenceSource extends Presence {
  /** The host showed (true) or hid (false) this page. */
  setShown(shown: boolean): void;
  /** Stop listening to the document. */
  dispose(): void;
}

/**
 * The presence of the page in DOC. EMBEDDED is whether the page sits in a host:
 * an embedded page is not shown until the host says it is.
 */
export function createPresence(doc: Document, embedded: boolean): PresenceSource {
  let hostShown = !embedded;
  const listeners = new Set<PresenceListener>();
  const visible = (): boolean => doc.visibilityState === "visible";
  const seen = (): boolean => hostShown && visible();
  const tell = (cause: PresenceCause): void => {
    const now = seen();
    for (const listener of [...listeners]) listener(now, cause);
  };
  const onVisibility = (): void => tell("visibility");
  doc.addEventListener("visibilitychange", onVisibility);
  return {
    seen,
    shown: () => hostShown,
    onChange(listener) {
      listeners.add(listener);
      return () => {
        listeners.delete(listener);
      };
    },
    setShown(shown) {
      if (shown === hostShown) return;
      hostShown = shown;
      tell("host");
    },
    dispose() {
      doc.removeEventListener("visibilitychange", onVisibility);
      listeners.clear();
    },
  };
}

let pageSource: PresenceSource | null = null;

/** Whether this page runs inside another page's iframe (the host's). */
export function pageIsEmbedded(win: Window): boolean {
  return win.parent !== win;
}

/** The presence of this page: one per page, made on first use. */
export function pagePresence(): PresenceSource {
  pageSource ??= createPresence(document, pageIsEmbedded(window));
  return pageSource;
}
