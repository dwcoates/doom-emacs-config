/**
 * The boot of Emacs's two web views: each document's logger first, then its
 * code, then its start.
 *
 * THE BOOT IS RECORDED FROM THE ENTRY BUNDLE ON. Each document's code arrives
 * by a dynamic import (main.ts), and its logger used to be installed only once
 * that code had started. So a document whose import never settled, whose
 * import failed, or whose start threw left no record at all: on 2026-10-31 a
 * merge gate's host page loaded, held both of Emacs's tabs for their pages,
 * and never created an iframe, and nothing told those stopping points apart.
 * Here the document's connection and logger are made in the entry bundle,
 * before the import (document.ts `startWebhostRuntime`), and every stage of
 * the boot and its failure is recorded through them.
 */
import { log } from "../log.js";
import { startWebhostRuntime, type WebhostDocument, type WebhostRuntime } from "./document.js";

/** What the boot may have injected, for a suite that drives it. */
export interface BootWebhostDeps {
  /** Begin the document and install its logger; the real one dials the daemon. */
  startRuntime?: (document: WebhostDocument, win: Window) => WebhostRuntime;
}

/** The cause of ERR, as a record carries it. */
function causeOf(err: unknown): string {
  return err instanceof Error ? `${err.name}: ${err.message}` : String(err);
}

/**
 * Boot DOCUMENT in WIN: install its logger, load its code (LOAD), and start it
 * (START, handed the code and the runtime). Answers what START answered.
 *
 * QUEUED is how many calls Emacs had queued for the document when its entry
 * ran, which the first record carries. A FAILED STAGE IS RECORDED HERE, ONCE,
 * and rethrown: the boot has nothing to fall back to, and main.ts re-raises
 * the rejection as the page's uncaught error.
 */
export async function bootWebhostDocument<Code, Started>(
  document: WebhostDocument,
  win: Window,
  queued: () => number,
  load: () => Promise<Code>,
  start: (code: Code, runtime: WebhostRuntime) => Started,
  deps: BootWebhostDeps = {},
): Promise<Started> {
  const runtime = (deps.startRuntime ?? startWebhostRuntime)(document, win);
  log.info("the document's entry ran; importing its code", {
    operation: "webhost.boot-importing",
    context: { queued: queued() },
  });
  let code: Code;
  try {
    code = await load();
  } catch (err) {
    const cause = causeOf(err);
    log.error(`the document's code failed to import: ${cause}`, {
      operation: "webhost.boot-import-failed",
      context: { cause },
    });
    throw err;
  }
  log.info("the document's code imported; starting it", {
    operation: "webhost.boot-imported",
    context: { queued: queued() },
  });
  try {
    return start(code, runtime);
  } catch (err) {
    const cause = causeOf(err);
    log.error(`the document failed to start: ${cause}`, {
      operation: "webhost.boot-failed",
      context: { cause },
    });
    throw err;
  }
}
