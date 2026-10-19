/**
 * The page module the band fit's WebKit suite bundles: the REAL `fitBands`
 * and `paintBandMore`,
 * under the webapp's own logger (which the module writes through), exposed on
 * `window.BandFit`.
 */
import { installPageLogger } from "./page-log";
import { fitBands, paintBandMore } from "../../src/sidebar/band-fit.js";

installPageLogger("band-fit");

export { fitBands, paintBandMore };
