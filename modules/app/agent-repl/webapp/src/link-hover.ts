/**
 * The hover popup a prose link shows: the link's destination, alone.
 *
 * A markdown anchor's text can say anything, so hovering it shows the real
 * http(s) url in a small popup under the link. Nothing but the url is drawn.
 * One delegated pair of listeners over the scroll zone serves every bubble,
 * and one popup element is reused for every link.
 */

const LINKABLE_SCHEME = /^https?:\/\//i;

/** Gap in pixels between the link's bottom edge and the popup. */
const POPUP_GAP = 4;

export function installLinkHoverPopup(root: HTMLElement): () => void {
  const popup = document.createElement("div");
  popup.className = "link-hover-popup";
  popup.hidden = true;
  popup.setAttribute("role", "tooltip");
  document.body.appendChild(popup);

  const hide = (): void => {
    popup.hidden = true;
  };
  const anchorOf = (event: Event): HTMLAnchorElement | null => {
    const target = event.target;
    if (!(target instanceof Element)) return null;
    const anchor = target.closest("a[href]");
    if (!(anchor instanceof HTMLAnchorElement)) return null;
    return LINKABLE_SCHEME.test(anchor.getAttribute("href") ?? "") ? anchor : null;
  };
  const onOver = (event: Event): void => {
    const anchor = anchorOf(event);
    if (anchor === null) return;
    popup.textContent = anchor.getAttribute("href");
    popup.hidden = false;
    const rect = anchor.getBoundingClientRect();
    popup.style.left = `${Math.max(0, rect.left)}px`;
    popup.style.top = `${rect.bottom + POPUP_GAP}px`;
  };
  const onOut = (event: Event): void => {
    if (anchorOf(event) !== null) hide();
  };

  root.addEventListener("mouseover", onOver);
  root.addEventListener("mouseout", onOut);
  root.addEventListener("click", hide, true);
  return () => {
    root.removeEventListener("mouseover", onOver);
    root.removeEventListener("mouseout", onOut);
    root.removeEventListener("click", hide, true);
    popup.remove();
  };
}
