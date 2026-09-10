/**
 * MalformedView — the one refusal the renderer raises when a received view
 * cannot be drawn as written.
 *
 * The webapp is a STATELESS RENDERER of server-resolved views: it derives
 * nothing, so there is never a second reading of a frame it cannot read. An
 * unset oneof, an unset non-optional message field, an arm this build has no
 * case for, or an unknown field on the wire are all the SAME condition — the
 * view says something this renderer cannot honestly draw — and all of them
 * land here rather than defaulting into a plausible-looking box.
 *
 * WHY A PATH AND A DETAIL RATHER THAN A SENTENCE. The `path` is where in the
 * message tree the refusal happened ("WatchFooterResponse.footer.status"), so
 * whoever debugs it can go straight to the producer's field; the `detail` is
 * what was wrong there. Both ride the failure overlay's frame_undecodable card
 * and the log record verbatim, which is why they are separate fields and not
 * one pre-joined string.
 */
export class MalformedView extends Error {
  readonly path: string;
  readonly detail: string;

  constructor(path: string, detail: string) {
    super(`malformed view at ${path}: ${detail}`);
    this.name = "MalformedView";
    this.path = path;
    this.detail = detail;
  }
}

/** Whether an unknown thrown value is this renderer's own refusal. */
export function isMalformedView(err: unknown): err is MalformedView {
  return err instanceof MalformedView;
}
