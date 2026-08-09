/**
 * local-failure — the webapp's OWN classifier, and the only one it is allowed
 * to have.
 *
 * A fact is classified by the process that is first to hold both the failure
 * and its cause. The daemon holds every conversation-plane fact, so everything
 * on that plane arrives already classified and this end renders it without
 * inspecting it. What the daemon definitionally CANNOT report is its own
 * unreachability — that is this file's whole subject.
 *
 * # The partition is now the ARM NUMBER, not a string prefix
 *
 * `errors.proto` splits the vocabulary by field number: the daemon mints every
 * arm below 100, and a frontend mints the arms from 100 up and ONLY those. The
 * eight client-local arms are exactly the functions below, and there is no
 * ninth — a failure this end wants to report that has no arm up there is a
 * failure this end has no standing to classify.
 *
 * That replaced a reserved `client.` string prefix on a free-text `error_type`.
 * The prefix made a violation detectable by inspecting a string; the split by
 * number makes most of the same violations UNREPRESENTABLE, because these
 * builders can only construct the arms they name.
 *
 * EVERY ONE OF THEM IS MACHINERY (blue). Nothing about the account is ever
 * implicated by a transport fault, so a vendor-side local failure would be this
 * frontend guessing at something only the daemon can see.
 */

import { create } from "@bufbuild/protobuf";
import { FailureKindSchema } from "../../proto/gen/ts/agentshim/frontend/v1/errors_pb";
import type { FailureCardLifecycle, FailureCardView, FailureKind } from "./frontend-proto.js";
import type { FailureCardItem } from "./store.js";

/**
 * The `FailureKind` arms this frontend may mint — the client-local band of the
 * vocabulary, spelled once.
 *
 * The keys are the generated arm names, so a renamed arm fails the build here
 * rather than producing a card the shared side table has no entry for.
 */
export const CLIENT_FAILURE_ARMS = [
  "daemonUnreachable",
  "workspaceGone",
  "bootFailed",
  "controlPlaneFailed",
  "frameUndecodable",
  "staleBundle",
  "commandUnsent",
  "commandRejectionUnclassified",
] as const;
export type ClientFailureArm = (typeof CLIENT_FAILURE_ARMS)[number];

/** Whether an arm belongs to the band a frontend is allowed to mint. */
export function isClientArm(arm: string): boolean {
  return (CLIENT_FAILURE_ARMS as readonly string[]).includes(arm);
}

/**
 * Mint a locally-classified card.
 *
 * `uuid` is derived from the arm so a repeated report of the SAME condition
 * reconciles onto one card instead of stacking. A reconnect loop that appended
 * a card per attempt would bury the feed under its own alarm.
 */
function clientCard(
  kind: FailureKind,
  message: string,
  detail: string,
  lifecycle: FailureCardLifecycle,
  uuid: string,
): FailureCardItem {
  const view: FailureCardView = { kind, message, detail, lifecycle };
  return { kind: "failure", uuid, view };
}

/** The uuid a client-minted card for ARM reconciles on. */
export function clientFailureUuid(arm: ClientFailureArm, discriminator = ""): string {
  return discriminator === "" ? `local:${arm}` : `local:${arm}:${discriminator}`;
}

/**
 * Classify a WebSocket close as the daemon-unreachable failure.
 *
 * The close code and reason are carried as the arm's OWN typed evidence rather
 * than flattened into prose, because they are the only thing distinguishing a
 * daemon that restarted (a clean 1000/1001 from a shutting-down server) from a
 * network drop (an abnormal 1006 the browser synthesizes with no reason at
 * all). Reporting both as one thing is what made "reconnecting…" the webapp's
 * answer to every transport fault.
 *
 * WINDOW-SHAPED and RETRACTED rather than resolved — see
 * `CONNECTIVITY_WINDOW_KINDS`.
 */
export function daemonUnreachableFailure(code: number, reason: string): FailureCardItem {
  const kind = create(FailureKindSchema, {
    kind: { case: "daemonUnreachable", value: { closeCode: code, closeReason: reason } },
  });
  const message =
    code === 1000 || code === 1001
      ? "the daemon closed the connection; reconnecting"
      : "lost the connection to the daemon; reconnecting";
  return clientCard(
    kind,
    message,
    reason === "" ? `close=${code}` : `close=${code} ${reason}`,
    { case: "open" },
    clientFailureUuid("daemonUnreachable"),
  );
}

/**
 * The RESOLVED twin of the above, stamped when the socket comes back.
 *
 * It is a RETRACTION, not a card: `daemonUnreachable` is one of the
 * connectivity windows the store takes down on resolution, so handing this to
 * `addFailure` removes the "lost the connection" card rather than settling it
 * in place. Its message and stamp are what the store's trace records the
 * removal by.
 */
export function daemonReachableFailure(atMs: number): FailureCardItem {
  const kind = create(FailureKindSchema, {
    kind: { case: "daemonUnreachable", value: { closeCode: 0, closeReason: "" } },
  });
  return clientCard(
    kind,
    "reconnected to the daemon",
    "",
    { case: "resolved", resolvedAtMs: atMs },
    clientFailureUuid("daemonUnreachable"),
  );
}

/**
 * Classify the definitive "not listed" answer to the existence probe.
 *
 * TERMINAL: unlike a dropped connection, there is nothing to come back. The arm
 * names the WORKSPACE and not a session, because a rendering frontend holds no
 * session vocabulary — the fence answers the only question it ever had.
 */
export function workspaceGoneFailure(workspace: string): FailureCardItem {
  const kind = create(FailureKindSchema, { kind: { case: "workspaceGone", value: {} } });
  return clientCard(
    kind,
    "this workspace no longer exists on the daemon",
    workspace === "" ? "" : `workspace=${workspace}`,
    { case: "terminal" },
    clientFailureUuid("workspaceGone"),
  );
}

/**
 * Classify a failed control-plane call as the frontend's own failure.
 *
 * `what` names the action in the USER'S terms ("the account switch"), not the
 * endpoint's — a card reading "POST /accounts/switch failed" explains nothing
 * to the person who clicked a menu item. It is also the uuid discriminator, so
 * two different failed actions are two cards while a retried one reconciles
 * onto its own; keying every control-plane failure alike would let a failed
 * login overwrite a failed remediation.
 */
export function controlPlaneFailure(what: string, err: unknown): FailureCardItem {
  const cause = causeText(err);
  const kind = create(FailureKindSchema, {
    kind: { case: "controlPlaneFailed", value: { what, cause } },
  });
  return clientCard(
    kind,
    `${what} failed`,
    cause,
    { case: "terminal" },
    clientFailureUuid("controlPlaneFailed", what),
  );
}

/**
 * Classify a frame this end could not decode.
 *
 * The message is GENERIC on purpose: the reader is being told that a frame was
 * dropped, and the decoder's own complaint is evidence, not prose. The frame
 * head rides the arm beside the cause, so the debugger gets the bytes and the
 * user gets the sentence.
 *
 * One uuid for every occurrence: a daemon emitting a shape this build cannot
 * read will emit it repeatedly, and a card per frame would bury the feed under
 * the same fact.
 */
export function frameUndecodableFailure(err: unknown, frameHead: string): FailureCardItem {
  const cause = causeText(err);
  const kind = create(FailureKindSchema, {
    kind: { case: "frameUndecodable", value: { cause, frameHead } },
  });
  return clientCard(
    kind,
    "a message from the daemon could not be read and was skipped",
    frameHead === "" ? cause : `${cause} — frame head: ${frameHead}`,
    { case: "terminal" },
    clientFailureUuid("frameUndecodable"),
  );
}

/**
 * Classify a refused version-skew reload.
 *
 * The message names the CONSEQUENCE rather than the mechanism: the reader does
 * not care about snapshot leases, only that what they are looking at is not the
 * live state and reloading did not fix it. Deliberately terminal — offering a
 * self-clearing version of this would hide a page that is silently wrong.
 */
export function staleBundleFailure(detail: string): FailureCardItem {
  const kind = create(FailureKindSchema, {
    kind: { case: "staleBundle", value: { detail } },
  });
  return clientCard(
    kind,
    "this page cannot read the daemon's state and reloading did not fix it; restart the view",
    detail,
    { case: "terminal" },
    clientFailureUuid("staleBundle"),
  );
}

/** Classify a frontend that could not boot at all. */
export function bootFailedFailure(err: unknown): FailureCardItem {
  const cause = causeText(err);
  const kind = create(FailureKindSchema, { kind: { case: "bootFailed", value: { cause } } });
  return clientCard(
    kind,
    "the interface could not start",
    cause,
    { case: "terminal" },
    clientFailureUuid("bootFailed"),
  );
}

/**
 * Classify a command that never left this page.
 *
 * NOT the same fact as a refusal: nothing was decided, so the operation can
 * simply be retried once the socket is back.
 */
export function commandUnsentFailure(command: string): FailureCardItem {
  const kind = create(FailureKindSchema, { kind: { case: "commandUnsent", value: { command } } });
  return clientCard(
    kind,
    `${command} was not sent: the connection to the daemon is down`,
    "",
    { case: "terminal" },
    clientFailureUuid("commandUnsent", command),
  );
}

/**
 * Classify a refusal the daemon declined to name.
 *
 * Legitimately classified here — the daemon decided the refusal but carried no
 * `FailureKind` with it, and somebody has to say so, or the refusal reaches the
 * user through nothing at all. The daemon's own prose is carried VERBATIM as
 * the evidence; this end invents no reading of it, and says outright that it
 * could not classify the refusal rather than picking a kind on the daemon's
 * behalf.
 */
export function commandRejectionUnclassifiedFailure(
  command: string,
  daemonReason: string,
): FailureCardItem {
  const kind = create(FailureKindSchema, {
    kind: { case: "commandRejectionUnclassified", value: { command, daemonReason } },
  });
  // THE DAEMON'S OWN WORDS LEAD when it gave any. It decided this refusal, and
  // the sentence it wrote is the closest thing to an account there is; a
  // composed "<command> was refused" is the fallback for a refusal that came
  // with nothing at all, not a replacement for prose the daemon supplied.
  return clientCard(
    kind,
    daemonReason === "" ? `${command} was refused` : daemonReason,
    `command=${command}`,
    { case: "terminal" },
    clientFailureUuid("commandRejectionUnclassified", command),
  );
}

/** A rejection's own words, whatever shape it arrived in. */
function causeText(err: unknown): string {
  return err instanceof Error ? err.message : String(err);
}
