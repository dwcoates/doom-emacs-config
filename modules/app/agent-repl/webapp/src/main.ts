/**
 * PLACEHOLDER BOOT.
 *
 * The real boot -- page address, transport, client, app context, and the mount
 * of every component -- lands with the rpc core and replaces this file whole.
 * Until then the entry point does the one thing that must be true before any
 * of that: it resolves the shell, so a broken `index.html` fails here, loudly
 * and by name, rather than somewhere inside a component's first draw.
 */
import "./styles.css";
import { shellElements } from "./shell.js";

shellElements(document);
