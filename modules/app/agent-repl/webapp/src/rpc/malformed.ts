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
 * what was wrong there. Both ride the warning chip's frame_undecodable row
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

/**
 * UnknownPushArm — the one refusal that is FORWARD-COMPAT SKEW rather than a
 * contract violation: a newer daemon set an arm of a push's TOP-LEVEL oneof
 * that this bundle's draw switch has no case for.
 *
 * WHY IT IS ITS OWN TYPE. Every other MalformedView is a real defect — a
 * malformed message, an unknown wire field, a required field unset, an unknown
 * arm on some nested component — and stays loud (the `frame_undecodable` card).
 * This one is benign version skew: right after a deploy that adds a push arm, a
 * webview still on the old bundle receives a frame carrying it, and skipping
 * that one frame quietly is correct — the stream keeps running and a reload
 * fully resolves it. Distinguishing it by TYPE (thrown only by
 * `unreachablePushArm` from a push envelope's top-level oneof switch) is what
 * lets the stream skip it quietly without ever reading the human sentence, so
 * no genuine malformation is ever quietened by accident.
 *
 * It remains a MalformedView so any code that does not care about the
 * distinction still treats it as the refusal it is; the stream pipeline checks
 * `isUnknownPushArm` FIRST to peel the skew case off before the loud path.
 */
export class UnknownPushArm extends MalformedView {
  readonly arm: string;

  constructor(path: string, arm: string) {
    super(path, `arm '${arm}' is not one this build can draw`);
    this.name = "UnknownPushArm";
    this.arm = arm;
  }
}

/** Whether a refusal is the benign forward-compat skew of an unknown push arm. */
export function isUnknownPushArm(err: unknown): err is UnknownPushArm {
  return err instanceof UnknownPushArm;
}
