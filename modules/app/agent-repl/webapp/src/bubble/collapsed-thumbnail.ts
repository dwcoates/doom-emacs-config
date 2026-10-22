/**
 * collapsed-thumbnail — AN IMAGE A COLLAPSED BUBBLE DRAWS SMALL.
 *
 * A collapsed prompt bubble in the feed shows each image among its blocks as a
 * small thumbnail, no wider than about a dozen characters (owner request,
 * 2026-10-22), so the reader sees a picture is there and roughly what it is.
 * Opening the bubble (expand.ts's one toggle) draws it at the full thumbnail
 * height (`.prompt-block-image`).
 *
 * A bubble in the ellipsis mode (a held prompt, src/feed/bubble-more.ts)
 * collapses to ONE line, which no picture fits, so there the stylesheet hides
 * the image until the card is opened, as it does an expanded-only block
 * (src/bubble/expanded-only.ts).
 */

/** The class every image a collapsed bubble draws small wears. */
export const BUBBLE_COLLAPSED_THUMBNAIL_CLASS = "bubble-collapsed-thumbnail";

/** Mark EL as an image a collapsed bubble draws small, and answer it. */
export function collapsedThumbnail<T extends HTMLElement>(el: T): T {
  el.classList.add(BUBBLE_COLLAPSED_THUMBNAIL_CLASS);
  return el;
}
