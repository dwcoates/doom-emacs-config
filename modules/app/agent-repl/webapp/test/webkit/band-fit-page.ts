/**
 * The page module the band fit's WebKit suite bundles: the REAL `fitBands`,
 * under the webapp's own logger (which the module writes through), exposed on
 * `window.BandFit`.
 */
import { installPageLogger } from "./page-logger";
import { fitBands } from "../../src/sidebar/band-fit.js";

installPageLogger("band-fit");

export { fitBands };
