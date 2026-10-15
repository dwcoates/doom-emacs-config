/**
 * THE ONE LOGGER A WEBKIT TEST PAGE INSTALLS: the webapp's own
 * `ForwardingLogger`, which every page module that drives the real modules
 * writes through, under the page's own `connection_id`. Its sink accepts
 * every record and goes nowhere; a page that reads back what was written
 * hands WRITTEN, and each record lands there as `level operation`.
 */
import { ForwardingLogger, bindLogContext, setLogger, type ClientLogLevel } from "../../src/log.js";

/** How a page's logger is installed. */
export interface PageLoggerOptions {
  /** The page's threshold; DEBUG when omitted. */
  level?: ClientLogLevel;
  /** Where each written record lands, as `level operation`; nowhere when omitted. */
  written?: string[];
}

/** The `connection_id` the logger of the page named PAGE writes under. */
export function pageConnectionId(page: string): string {
  return `webkit-${page}-page`;
}

/** Install the page logger of the page named PAGE (its file's `<page>-page.ts`). */
export function installPageLogger(page: string, opts: PageLoggerOptions = {}): void {
  const written = opts.written;
  setLogger(
    new ForwardingLogger(
      () => Promise.resolve("accepted"),
      written === undefined
        ? () => undefined
        : (level, line) => {
            const { operation } = JSON.parse(line) as { operation: string };
            written.push(`${level} ${operation}`);
          },
      {},
      opts.level ?? "debug",
    ),
  );
  bindLogContext({ connection_id: pageConnectionId(page) });
}
