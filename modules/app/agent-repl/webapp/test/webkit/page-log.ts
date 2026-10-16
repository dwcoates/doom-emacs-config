/**
 * THE LOGGER EVERY WEBKIT PAGE INSTALLS before it mounts anything.
 *
 * Production installs the ClientLog-forwarding logger before any component
 * draws, and `log` refuses a record with no bound identity, so a page without
 * one fails inside the logger rather than inside the code under test. This is
 * the webapp's own logger at LEVEL, its identity bound to the page's NAME, with
 * a forwarding sink that accepts and drops. Its console line, written for
 * every record before the forwarding throttle, is kept as `level operation`
 * for the test to read.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";

/** Install the page logger for the page called NAME. Answers a reader of every record written so far. */
export function installPageLogger(name: string, level: "debug" | "info" = "debug"): () => string[] {
  const written: string[] = [];
  setLogger(
    new ForwardingLogger(
      () => Promise.resolve("accepted"),
      (recordLevel, line) => {
        const { operation } = JSON.parse(line) as { operation: string };
        written.push(`${recordLevel} ${operation}`);
      },
      {},
      level,
    ),
  );
  bindLogContext({ connection_id: `webkit-${name}-page` });
  return () => [...written];
}
