/**
 * The page module the Recently Merged fit's WebKit suite bundles: the REAL
 * `fitMergedSection`, under the webapp's own logger (which the module writes
 * through), exposed on `window.MergedFit`.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { fitMergedSection } from "../../src/sidebar/merged-fit.js";

setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    () => undefined,
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-merged-fit-page" });

export { fitMergedSection };
