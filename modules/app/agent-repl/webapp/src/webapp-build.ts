/**
 * webapp-build — the build THIS PAGE IS RUNNING, read from its own shell.
 *
 * Every process reports its build when it connects (see
 * `proto/src/agentrepl/v1/endpoint_watch_web_workspace.proto`,
 * `WatchWebWorkspaceRequest.webapp_build`), so the daemon can push
 * `reload_webapp` to exactly the webviews whose build differs from the one it
 * just built. The build is the CONTENT HASH Vite gives the entry bundle: the
 * `<hash>` in `assets/index-<hash>.js`.
 *
 * THE PATTERN IS SHARED WITH THE WRITER, NOT REINVENTED HERE. Once built,
 * `dist/index.html` carries the entry as
 * `<script type="module" crossorigin src="/assets/index-<hash>.js">`, and
 * `bin/build-frontend.sh`'s `write_webapp_build_id` reads the same tag with
 * the same `src="/assets/index-([A-Za-z0-9_-]+)\.js"` shape. This module reads
 * it back out of the running page's own DOM rather than the file, because the
 * page has no access to the file it was served from — only to the document it
 * became.
 *
 * A PAGE WITH NO SUCH TAG CANNOT STATE ITS BUILD, and that is a LOUD failure:
 * `webapp_build` is REQUIRED on the wire, and a watch opened with an empty one
 * is a request the daemon would refuse outright. So this throws rather than
 * answering an empty string, and logs through the one canonical logger first,
 * at error, so the failure is on record even though nothing here can recover
 * from it.
 */
import { log } from "./log.js";

/** The same shape `write_webapp_build_id` reads out of the built `index.html`. */
const ENTRY_SCRIPT_SRC_PATTERN = /^\/assets\/index-([A-Za-z0-9_-]+)\.js$/;

/** Thrown when DOC carries no built entry `<script>` tag to read a build from. */
export class WebappBuildUnknown extends Error {
  constructor() {
    super(
      "this page carries no built entry <script type=\"module\"> tag; its webapp build is unknown",
    );
    this.name = "WebappBuildUnknown";
  }
}

/**
 * Read the entry hash from DOC's own module script tag.
 *
 * Throws `WebappBuildUnknown` — logged at error first — when no `<script
 * type="module">` on the page carries a `src` matching the built entry's
 * shape, which is the ordinary case for a page whose shell was never actually
 * built (a dev shell, a malformed serve).
 */
export function readWebappBuild(doc: Document = document): string {
  for (const script of doc.querySelectorAll('script[type="module"]')) {
    const src = script.getAttribute("src");
    if (src === null) continue;
    const match = ENTRY_SCRIPT_SRC_PATTERN.exec(src);
    if (match !== null) return match[1];
  }
  log.error(
    "this page has no built entry script tag; cannot state its webapp build",
    { operation: "webapp-build.missing" },
  );
  throw new WebappBuildUnknown();
}
