/**
 * quiet — WHETHER PERSISTENT WIFI'S LID-CLOSED QUIET STATE STANDS for this
 * document: the lid is closed, nobody can see any page, and the battery goes
 * to agent work. Emacs hears it from the daemon and tells the webhost and the
 * sidebar (`setQuiet`); the webhost tells every page (host/protocol.ts
 * `quiet`).
 *
 * While it stands the page does no work for a screen nobody sees: the shared
 * clock stops (clock.ts) and the page reads as unseen (presence.ts), so
 * everything that animates or samples only while seen stops too. The daemon
 * resolves no view for the page meanwhile; when the state ends it hands each
 * stream the current state in one step, and the clock steps at once.
 */
import { createListeners } from "./listeners.js";
import { perDocument } from "./per-document.js";

/** The quiet state as its readers see it. */
export interface Quiet {
  /** Whether the quiet state stands now. */
  quiet(): boolean;
  /** Tell LISTENER every change; answers the unsubscribe. */
  onChange(listener: (quiet: boolean) => void): () => void;
}

/** The quiet state plus the one writer Emacs's messages reach. */
export interface QuietSource extends Quiet {
  /** The quiet state began (true) or ended (false). A repeat is no change. */
  set(quiet: boolean): void;
}

/** A quiet state that starts not standing. */
export function createQuiet(): QuietSource {
  let standing = false;
  const listeners = createListeners<[boolean]>();
  return {
    quiet: () => standing,
    onChange: (listener) => listeners.add(listener),
    set(quiet) {
      if (quiet === standing) return;
      standing = quiet;
      listeners.notify(quiet);
    },
  };
}

/** The quiet state of each document: one per page, made on first use. */
export const pageQuiet = perDocument(() => createQuiet());
