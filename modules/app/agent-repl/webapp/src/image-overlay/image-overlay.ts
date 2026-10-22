/**
 * image-overlay — ONE IMAGE DRAWN LARGE OVER THE FEED (frontend.v1
 * FeedImageOverlay).
 *
 * The daemon holds each workspace's overlay and pushes it on the root feed's
 * watch; this component draws exactly what the push says. A click on an image
 * in Emacs's input window or anywhere in the feed asks the daemon to open it
 * (feed-image.ts); a click on the overlay, or Escape, asks to close it.
 *
 * IT COVERS THE FEED'S STREAMING AREA, JUST SHY OF IT (owner ruling,
 * 2026-10-22): the fixed host follows #feed-scroll's rectangle (followBox),
 * and its padding keeps the frame inside the column the rows span, so the rows
 * stay visible around its edge. The image is as large as fits the frame,
 * aspect kept; one that fails to load is drawn as the feed's "Image no longer
 * available" note (drawFeedImageBlock).
 */
import type { FeedImageOverlay } from "../../../proto/gen/ts/frontend/v1/feed_image_overlay_pb";
import { closeFeedImage } from "../feed/feed-image.js";
import { drawFeedImageBlock, type FeedImageClasses } from "../feed/rows/blocks.js";
import { log } from "../log.js";
import { followBox } from "../overlay/follow-box.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";

/** The class the overlay's frame wears: the box the image is drawn in. */
export const IMAGE_OVERLAY_FRAME_CLASS = "image-overlay-frame";

/** The classes the overlay's image and its lost-image note wear. */
export const IMAGE_OVERLAY_CLASSES: FeedImageClasses = {
  image: "image-overlay-image",
  missing: "image-overlay-missing",
};

/** What the page holds of the overlay. */
export interface ImageOverlayHandle {
  /** Draw the overlay the daemon pushed, whole. */
  readonly apply: (overlay: FeedImageOverlay) => void;
  /** Take the overlay down for good (the page is leaving). */
  readonly dispose: () => void;
}

/** What the overlay draws over. */
export interface ImageOverlayDeps {
  /** The feed's scroll box: the rectangle the overlay covers. */
  readonly feedScroll: HTMLElement;
}

/** Mount the image overlay into HOST, hidden until the daemon opens one. */
export function mountImageOverlay(host: HTMLElement, ctx: AppContext, deps: ImageOverlayDeps): ImageOverlayHandle {
  log.debug("mounting the image overlay", { operation: "image-overlay.mount" });
  host.hidden = true;
  host.replaceChildren();
  const box = followBox(host, deps.feedScroll);

  const close = (cause: string): void => {
    if (host.hidden) return;
    log.info(`the image overlay closes on ${cause}`, { operation: "image-overlay.close", context: { cause } });
    void guardMalformed(ctx, "image-overlay.close", closeFeedImage(ctx));
  };
  const onClick = (event: MouseEvent): void => {
    event.preventDefault();
    event.stopPropagation();
    close("a click");
  };
  const onKeydown = (event: KeyboardEvent): void => {
    if (event.key !== "Escape" || host.hidden) return;
    event.preventDefault();
    close("Escape");
  };
  host.addEventListener("click", onClick);
  document.addEventListener("keydown", onKeydown);

  const show = (src: string, alt: string, frame: HTMLElement): void => {
    host.replaceChildren(frame);
    host.hidden = false;
    box.follow();
    log.info("an image is open; drawing it over the feed", {
      operation: "image-overlay.shown",
      context: { src, alt },
    });
  };
  const hide = (): void => {
    if (host.hidden) return;
    host.hidden = true;
    host.replaceChildren();
    log.info("no image is open; taking the overlay down", { operation: "image-overlay.closed" });
  };

  return {
    apply(overlay: FeedImageOverlay): void {
      const state = requireCase(overlay.state, "FeedImageOverlay.state");
      switch (state.case) {
        case "open": {
          const image = requireMessage(state.value.image, "FeedImageOverlayOpen.image");
          const frame = document.createElement("div");
          frame.className = IMAGE_OVERLAY_FRAME_CLASS;
          frame.appendChild(drawFeedImageBlock(image, IMAGE_OVERLAY_CLASSES));
          show(image.src, image.alt, frame);
          return;
        }
        case "closed":
          hide();
          return;
        default: {
          const other: { case: string } = state;
          return unreachableArm("FeedImageOverlay.state", other.case);
        }
      }
    },
    dispose(): void {
      log.debug("disposing the image overlay", { operation: "image-overlay.dispose" });
      box.dispose();
      host.removeEventListener("click", onClick);
      document.removeEventListener("keydown", onKeydown);
      host.hidden = true;
      host.replaceChildren();
    },
  };
}
