/**
 * feed-image — THE WEBAPP'S CALLS OF `OpenFeedImage` AND `CloseFeedImage`, and
 * the click that opens an image.
 *
 * The daemon holds each workspace's image overlay (frontend.v1
 * FeedImageOverlay) and pushes it on the root feed's watch; the webapp only
 * asks. A click on any image the feed drew -- a prompt bubble's, a held card's,
 * a tool card's -- asks to open it, echoing the image exactly as it was served
 * (`drawn`). The overlay's own click and Escape ask to close it
 * (image-overlay.ts). Neither acts on the answer: the push is what draws.
 *
 * A REFUSED OR FAILED CALL is an error the reader must see: it is filed on the
 * topbar's warning chip (`control_plane_failed`) and logged, as every verb is.
 */
import { create } from "@bufbuild/protobuf";
import { ConnectError } from "@connectrpc/connect";
import {
  OpenFeedImageRequestSchema,
  OpenFeedImageResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_open_feed_image_pb";
import {
  CloseFeedImageRequestSchema,
  CloseFeedImageResponseSchema,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_close_feed_image_pb";
import { controlPlaneFailed } from "../failure/sink.js";
import { log } from "../log.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { callFailure, refusalOf } from "../rpc/refuse.js";
import { requireCase } from "../rpc/strict.js";
import { callUnary } from "../rpc/unary.js";
import { FEED_IMAGE_CLASS } from "./rows/blocks.js";


/** What the chip's `request` evidence names each call as. */
export const OPEN_IMAGE_REQUEST = "open the image";
export const CLOSE_IMAGE_REQUEST = "close the image";

/** OpenFeedImage's own refusal arms, in the reader's words. */
const OPEN_FEED_IMAGE_REFUSALS = {
  unservable: (value: { detail: string }) => `the image cannot be shown (${value.detail})`,
  notDrawn: (value: { src: string }) => `the feed did not draw that image (${value.src})`,
};

/**
 * Ask the daemon to open the drawn image SRC (ALT its alt text) over this
 * page's feed. Answers once the daemon has answered and any failure has been
 * filed; nothing is thrown for a refusal or a transport failure.
 */
export async function openDrawnImage(ctx: AppContext, src: string, alt: string): Promise<void> {
  let response;
  try {
    response = await callUnary(
      ctx,
      "OpenFeedImage",
      (client) =>
        client.openFeedImage(
          create(OpenFeedImageRequestSchema, {
            workspace: ctx.workspace,
            image: { case: "drawn", value: { image: { src, alt } } },
          }),
        ),
      OpenFeedImageResponseSchema,
    );
  } catch (err) {
    if (!(err instanceof ConnectError)) throw err;
    ctx.failures.report(controlPlaneFailed(OPEN_IMAGE_REQUEST, callFailure(err).text));
    return;
  }
  const result = requireCase(response.result, "OpenFeedImageResponse.result");
  if (result.case === "success") {
    log.debug("the daemon opened the image; its push draws the overlay", {
      operation: "feed.open-image-answered",
      context: { src },
    });
    return;
  }
  const said = refusalOf(result.value.cause, OPEN_FEED_IMAGE_REFUSALS, "OpenFeedImageError.cause");
  log.error(`the daemon refused to ${OPEN_IMAGE_REQUEST}: ${said.text}`, {
    operation: "feed.open-image-refused",
    context: { src, arm: said.arm },
  });
  ctx.failures.report(controlPlaneFailed(OPEN_IMAGE_REQUEST, said.text));
}

/**
 * Ask the daemon to close the image open over this page's feed. Answers once
 * the daemon has answered and any failure has been filed.
 */
export async function closeFeedImage(ctx: AppContext): Promise<void> {
  let response;
  try {
    response = await callUnary(
      ctx,
      "CloseFeedImage",
      (client) => client.closeFeedImage(create(CloseFeedImageRequestSchema, { workspace: ctx.workspace })),
      CloseFeedImageResponseSchema,
    );
  } catch (err) {
    if (!(err instanceof ConnectError)) throw err;
    ctx.failures.report(controlPlaneFailed(CLOSE_IMAGE_REQUEST, callFailure(err).text));
    return;
  }
  const result = requireCase(response.result, "CloseFeedImageResponse.result");
  if (result.case === "success") {
    log.debug("the daemon closed the image; its push takes the overlay down", {
      operation: "feed.close-image-answered",
    });
    return;
  }
  const said = refusalOf(result.value.cause, {}, "CloseFeedImageError.cause");
  log.error(`the daemon refused to ${CLOSE_IMAGE_REQUEST}: ${said.text}`, {
    operation: "feed.close-image-refused",
    context: { arm: said.arm },
  });
  ctx.failures.report(controlPlaneFailed(CLOSE_IMAGE_REQUEST, said.text));
}

/**
 * Arm the click that opens an image, on the scroll BOX: every image the feed
 * draws inside it (the rows, the held tray, an expanded sub-feed) opens on a
 * click. It listens in the CAPTURE phase and stops the click there, so the
 * bubble the image sits in does not also take it as a select or an expand.
 * Answers the uninstall.
 */
export function installImageOpen(box: HTMLElement, ctx: AppContext): () => void {
  const onClick = (event: MouseEvent): void => {
    const image = event.target instanceof Element ? event.target.closest(`img.${FEED_IMAGE_CLASS}`) : null;
    if (image === null || !box.contains(image)) return;
    event.preventDefault();
    event.stopPropagation();
    // THE SOURCE AS SERVED: the attribute, never the resolved `.src`, which
    // the browser has made absolute and the daemon never served.
    const src = image.getAttribute("src") ?? "";
    const alt = image.getAttribute("alt") ?? "";
    log.info("a click on an image asks to open it over the feed", {
      operation: "feed.image-click",
      context: { src, alt },
    });
    void guardMalformed(ctx, "feed.image-open", openDrawnImage(ctx, src, alt));
  };
  box.addEventListener("click", onClick, { capture: true });
  return () => {
    box.removeEventListener("click", onClick, { capture: true });
  };
}
