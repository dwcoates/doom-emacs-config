/**
 * The page shell, taken from `index.html` itself.
 *
 * A test that wants to run `boot` needs the mount points `shellElements`
 * resolves, and there is exactly one right source for them: the file the
 * daemon actually serves. A hand-written copy would be a SECOND spelling of
 * the shell, and the first thing it would do is stop failing when a mount
 * point is renamed in one place and not the other — which is precisely the
 * failure `shellElements` exists to catch.
 */
import indexHTML from "../index.html?raw";

/** The body's own markup from `index.html`, mount points and all. */
export function shellHTML(): string {
  const open = indexHTML.indexOf("<body>");
  const close = indexHTML.lastIndexOf("</body>");
  if (open < 0 || close < 0) {
    throw new Error("index.html has no <body>, so the page shell cannot be read from it");
  }
  const body = indexHTML.slice(open + "<body>".length, close);
  // The entry script is deliberately dropped: a test arranges a page around
  // `boot` and calls it itself, and leaving the tag in would ask jsdom to
  // fetch and run the application under test.
  return body.replace(/<script\b[\s\S]*?<\/script>/g, "");
}
