/**
 * The page module the band fit's WebKit suite bundles: the REAL `fitBands`,
 * under the webapp's own logger (which the module writes through), exposed on
 * `window.BandFit`.
 */
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { fitBands } from "../../src/sidebar/band-fit.js";

setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    () => undefined,
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-band-fit-page" });

export { fitBands };
