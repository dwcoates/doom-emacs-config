/**
 * async-bubble — `agentshim.frontend.v1` DETACHED WORK, decoded.
 *
 * async-bubble.proto models detached work as what it IS rather than as a notice
 * that something is happening elsewhere: a detached agent is a live, growing,
 * recursively spawning conversation; a Workflow run is a live table of journal
 * rows; a backgrounded shell command is a live byte spool; and a spawn whose
 * tool the daemon does not recognize is a first-class kind that says so.
 *
 * The daemon owns the whole apparatus. It classifies a detaching tool call,
 * MINTS the bubble's id, folds the work's output, and pushes typed updates
 * addressed to that id. This module's entire job is to read those pushes
 * faithfully and loudly. It derives no identity, parses no transcript, and
 * infers no kind — every one of those is a daemon fact that arrives on the
 * wire.
 *
 * WHAT THIS MODULE REFUSES TO DO, and why each refusal is structural:
 *
 * - It never invents a bubble id. `AsyncBubble.id` is "never empty" on the
 *   contract, so an empty one is a malformed frame, not a bubble to name after
 *   something else.
 * - It never picks a kind. Exactly one `kind` arm is set; zero or two is a
 *   producer fault. An unrecognized TOOL already has its own arm
 *   (`unclassified`), so there is nothing left for a fallback to cover.
 * - It never resolves a settled bubble's outcome from its exit code. The
 *   daemon resolves `outcome` FROM `shell_exit` and puts both on the wire
 *   precisely so that this end does not make that mapping.
 *
 * DECODE TABLES ARE ANCHORED (invariant I5). Every key set below is built with
 * {@link generatedFieldSet} against the committed `proto/gen/ts` stub for that
 * message, so a field added, renamed or removed in the .proto breaks THIS build
 * rather than producing a decoder that silently ignores part of the wire. Arm
 * name sets are typed against the generated oneof's own `case` union for the
 * same reason.
 */

import {
  AsyncAgentBubbleSchema,
  AsyncAgentUpdateSchema,
  AsyncBubbleDeltaSchema,
  AsyncBubbleSchema,
  AsyncBubbleUpdateSchema,
  AsyncFoldSchema,
  AsyncLiveSchema,
  AsyncLivenessSchema,
  AsyncLivenessUpdateSchema,
  AsyncMergeBubbleSchema,
  AsyncOutcomeDoneSchema,
  AsyncOutcomeErrorSchema,
  AsyncOutcomeKilledSchema,
  AsyncOutputAppendSchema,
  AsyncOutputSpoolSchema,
  AsyncSettledSchema,
  AsyncShellBubbleSchema,
  AsyncShellExitSchema,
  AsyncSkillBodyResolvedSchema,
  AsyncSkillBubbleSchema,
  AsyncSkillUpdateSchema,
  AsyncUnclassifiedBubbleSchema,
  AsyncWorkflowJournalRowSchema,
  AsyncWorkflowJournalSchema,
  AsyncWorkflowJournalUpdateSchema,
  AsyncWorkflowStepDoneSchema,
  AsyncWorkflowStepFailedSchema,
  AsyncWorkflowStepRunningSchema,
  type AsyncBubble as GeneratedAsyncBubble,
  type AsyncBubbleUpdate as GeneratedAsyncBubbleUpdate,
  type AsyncLiveness as GeneratedAsyncLiveness,
  type AsyncSettled as GeneratedAsyncSettled,
  type AsyncSkillUpdate as GeneratedAsyncSkillUpdate,
  type AsyncWorkflowJournalRow as GeneratedAsyncWorkflowJournalRow,
} from "../../proto/gen/ts/agentshim/frontend/v1/async-bubble_pb";
import { unwrapAgentEmission, type UnwrappedEmission } from "./agent-emission.js";
import {
  ensureArray,
  ensureObject,
  generatedFieldSet,
  int64OrZero,
  num,
  offset,
  oneof,
  rejectUnknown,
  str,
  type Obj,
} from "./proto-scalars.js";

// --- render vocabulary ------------------------------------------------------

/** A generated oneof's arm keys, with protobuf-es's "nothing set" arm dropped. */
type ArmKeys<Oneof extends { case: string | undefined }> = Exclude<Oneof["case"], undefined>;

/**
 * Tail-cap accounting for a bubble whose folded content is capped.
 *
 * The cap is a resolved DAEMON fact, not per-frontend policy, so that two
 * frontends cannot silently disagree about what the user is being shown: a fold
 * that drops its oldest entries and says nothing is indistinguishable from a
 * complete one. `droppedBefore === 0` means the fold is complete and no
 * "earlier entries" notice is drawn.
 */
export interface AsyncFold {
  droppedBefore: number;
  tailCap: number;
}

/** A process's exit status, as the shell reports it (128+N for a signal). */
export interface AsyncShellExit {
  code: number;
}

/** HOW a settled bubble finished. Exactly one of these is always the case. */
export type AsyncOutcome =
  | { case: "done" }
  | { case: "error"; message: string }
  | { case: "killed"; reason: string };

/** The work has finished, one way or another. */
export interface AsyncSettled {
  settledAtMs: number;
  /**
   * The exit status, for work that IS a process. ABSENT for work with no exit
   * status of its own (an agent, a workflow), and absence is the only reading
   * of "this work did not exit, it concluded".
   */
  shellExit?: AsyncShellExit;
  outcome: AsyncOutcome;
}

/** The work is still running. */
export interface AsyncLive {
  /**
   * When the daemon last saw this work produce anything; 0 means nothing since
   * launch. A "quiet for a while" affordance, NOT a liveness verdict — a silent
   * agent is still a live agent.
   */
  lastActivityMs: number;
}

/**
 * Live-or-settled, as arms rather than a flag plus an optional outcome, so that
 * "settled" and "settled with what outcome" are one indivisible fact.
 */
export type AsyncLiveness =
  | { case: "live"; value: AsyncLive }
  | { case: "settled"; value: AsyncSettled };

/** A verbatim byte spool and its delivery cursor. */
export interface AsyncOutputSpool {
  /** Everything spooled so far, verbatim — deliberately unparsed. */
  text: string;
  /**
   * Bytes delivered so far. ALSO the sequencing check on an append: an append
   * whose `fromOffset` does not equal this is a gap (invariant I4).
   */
  throughOffset: number;
}

/** One logged step of a Workflow run's journal. */
export interface AsyncWorkflowJournalRow {
  label: string;
  detail: string;
  status: ArmKeys<GeneratedAsyncWorkflowJournalRow["status"]>;
}

/** A detached agent: a whole conversation happening elsewhere. */
export interface AsyncAgentBubble {
  /** The conversation so far, in EXACTLY the vocabulary the top-level feed uses. */
  emissions: UnwrappedEmission[];
  fold: AsyncFold;
}

/** A Workflow run's journal: the step log a Workflow launch writes. */
export interface AsyncWorkflowJournal {
  rows: AsyncWorkflowJournalRow[];
  fold: AsyncFold;
}

/** A backgrounded shell command: an opaque byte spool with a command line. */
export interface AsyncShellBubble {
  command: string;
  output: AsyncOutputSpool;
}

/**
 * A merge run: the conversation a merge drives through this workspace's own
 * session, folded exactly like a detached agent's.
 *
 * DAEMON-AUTHORED — no tool spawns it. It carries the SAME shape as
 * {@link AsyncAgentBubble} deliberately, so a frontend's renderer for a
 * response bubble, a thinking block or a tool card is the same code here.
 */
export interface AsyncMergeBubble {
  /** The merge conversation so far, in the top-level feed's vocabulary. */
  emissions: UnwrappedEmission[];
  fold: AsyncFold;
}

/**
 * A skill invocation as detached-style work: the invocation card the feed used
 * to render flat, now a bubble that owns its window.
 *
 * DAEMON-AUTHORED. The daemon classifies the Skill tool call, resolves the
 * skill file's contents as the bubble's own `body` — the SKILL's content, not a
 * response of the conversation — and folds the session's subsequent emissions
 * here until the user takes the session back, the same temporal membership
 * {@link AsyncMergeBubble} uses.
 */
export interface AsyncSkillBubble {
  /** The skill's name as invoked, verbatim. */
  skillName: string;
  /** The invocation's arguments, verbatim; empty when none were given. */
  args: string;
  /** The skill file's contents, empty until resolution delivers them. */
  body: string;
  /** The window's conversation so far, in the top-level feed's vocabulary. */
  emissions: UnwrappedEmission[];
  fold: AsyncFold;
}

/** A spawn whose tool the daemon does not recognize — an ARM, not a fallback. */
export interface AsyncUnclassifiedBubble {
  /** The tool that spawned the work, verbatim as the agent named it. */
  toolName: string;
  output: AsyncOutputSpool;
}

/** WHAT KIND of work a bubble is, carrying that kind's folded content. */
export type AsyncBubbleKind =
  | { case: "agent"; value: AsyncAgentBubble }
  | { case: "journal"; value: AsyncWorkflowJournal }
  | { case: "shell"; value: AsyncShellBubble }
  | { case: "unclassified"; value: AsyncUnclassifiedBubble }
  | { case: "merge"; value: AsyncMergeBubble }
  | { case: "skill"; value: AsyncSkillBubble };

/** The kind discriminators a bubble may carry. */
export type AsyncBubbleKindCase = AsyncBubbleKind["case"];

/** One piece of detached work, and the unit every later update is addressed to. */
export interface AsyncBubble {
  /** THE ROUTING HANDLE. Never empty; matched, never derived. */
  id: string;
  /**
   * The workspace this detached work belongs to (contract amendment 2).
   *
   * It is the bubble's SCOPING key, not its routing key: the daemon filters a
   * connect snapshot's bubbles by it, and refuses to publish one that names no
   * workspace, because the snapshot has no other key to route a bubble by. On
   * this end the registry still routes strictly by `id` (invariant I2), so the
   * field is carried rather than dispatched on — it exists so a bubble restated
   * in a snapshot and a bubble pushed in a delta are scoped by one rule.
   */
  workspace: string;
  /** The tool_use id of the call that spawned this work; empty when none did. */
  originToolUseId: string;
  /** The bubble this one was spawned FROM; empty at the top level. */
  parentBubbleId: string;
  /** The collapsed fold's face; empty means the client shows the id. */
  label: string;
  startedAtMs: number;
  liveness: AsyncLiveness;
  kind: AsyncBubbleKind;
}

/** New output from a detached agent. */
export interface AsyncAgentUpdate {
  emissions: UnwrappedEmission[];
  /** The fold accounting AFTER this update — restated, never deltaed. */
  fold: AsyncFold;
}

/** New steps in a Workflow run's journal. Rows are APPEND-ONLY. */
export interface AsyncWorkflowJournalUpdate {
  rows: AsyncWorkflowJournalRow[];
  fold: AsyncFold;
}

/** New bytes on a spool, at the offset they START at. */
export interface AsyncOutputAppend {
  text: string;
  /**
   * MUST equal the spool's current `throughOffset`; anything else is a gap.
   * Carried explicitly so a gap is DETECTABLE at all — a bare append cannot
   * tell a lost chunk from a quiet one.
   */
  fromOffset: number;
}

/**
 * One incremental push to a skill bubble.
 *
 * Body resolution and emission appends are different events with different
 * lifetimes, so they are ARMS rather than co-set fields.
 */
export type AsyncSkillUpdateArm =
  /** The skill file's contents, resolved — replaces the bubble's `body` whole. */
  | { case: "body"; value: string }
  /** Emissions appended to the window, exactly as a detached agent's. */
  | { case: "emissions"; value: AsyncAgentUpdate };

/** WHAT changed on a bubble. The arm MUST match the bubble's kind. */
export type AsyncBubbleUpdateArm =
  | { case: "agent"; value: AsyncAgentUpdate }
  | { case: "journal"; value: AsyncWorkflowJournalUpdate }
  | { case: "shell"; value: AsyncOutputAppend }
  | { case: "unclassified"; value: AsyncOutputAppend }
  | { case: "liveness"; value: AsyncLiveness }
  | { case: "merge"; value: AsyncAgentUpdate }
  | { case: "skill"; value: AsyncSkillUpdateArm };

/** The update arm discriminators. `liveness` is the kind-independent one. */
export type AsyncBubbleUpdateCase = AsyncBubbleUpdateArm["case"];

/** One incremental push to one bubble: the id routes it, the arm types it. */
export interface AsyncBubbleUpdate {
  /** Which bubble this lands on. Never empty; the ONLY routing input. */
  bubbleId: string;
  update: AsyncBubbleUpdateArm;
}

/** The async push frame: bubbles that opened, and updates to bubbles open. */
export interface AsyncBubbleDelta {
  workspace: string;
  /** Bubbles opening for the first time, or re-delivered in full after a resync. */
  opened: AsyncBubble[];
  /** Incremental pushes to bubbles already open, in order. */
  updates: AsyncBubbleUpdate[];
  throughSeq: number;
  /** The opaque staleness fence: compared BYTE-WISE, never parsed. */
  fence: string;
}

// --- kind ↔ update-arm correspondence ---------------------------------------

/**
 * Which bubble kind each KIND-SPECIFIC update arm addresses.
 *
 * This table IS invariant I2's kind-mismatch check. It is written once, here,
 * beside the two vocabularies it relates, so that "a journal update addressed
 * to a shell bubble" has exactly one answer in this codebase rather than one
 * per call site. `liveness` is deliberately absent: it is kind-INDEPENDENT and
 * lands on any bubble, which is why it cannot be spelled as a mapping.
 *
 * `shell` and `unclassified` map to their own kinds and to nothing else even
 * though both carry the SAME payload message. The arm names which kind the
 * update is for; the two kinds differ in what they ARE, not in how their output
 * arrives. `merge` and `agent` are the same pairing for the same reason: a merge
 * run's emissions arrive precisely as a detached agent's do.
 */
export const UPDATE_ARM_KIND: Readonly<Record<Exclude<AsyncBubbleUpdateCase, "liveness">, AsyncBubbleKindCase>> = {
  agent: "agent",
  journal: "journal",
  shell: "shell",
  unclassified: "unclassified",
  merge: "merge",
  skill: "skill",
};

// --- anchored key sets ------------------------------------------------------

/**
 * CONTRACT AMENDMENT 2 landed `AsyncBubble.workspace = 7`, and regenerating the
 * stubs turned this line into the build failure it was designed to produce —
 * the anchor named the missing key rather than letting a decoder silently drop
 * a field the daemon had started sending. `workspace` is spelled here now that
 * the generated stub actually has it, which is what invariant I5 requires.
 */
const BUBBLE_KEYS = generatedFieldSet<keyof typeof AsyncBubbleSchema.field>()("id", "workspace", "originToolUseId", "parentBubbleId", "label", "startedAtMs", "liveness", "agent", "journal", "shell", "unclassified", "merge", "skill");
const AGENT_BUBBLE_KEYS = generatedFieldSet<keyof typeof AsyncAgentBubbleSchema.field>()("emissions", "fold");
const JOURNAL_KEYS = generatedFieldSet<keyof typeof AsyncWorkflowJournalSchema.field>()("rows", "fold");
const SHELL_BUBBLE_KEYS = generatedFieldSet<keyof typeof AsyncShellBubbleSchema.field>()("command", "output");
const UNCLASSIFIED_BUBBLE_KEYS = generatedFieldSet<keyof typeof AsyncUnclassifiedBubbleSchema.field>()("toolName", "output");
const MERGE_BUBBLE_KEYS = generatedFieldSet<keyof typeof AsyncMergeBubbleSchema.field>()("emissions", "fold");
const SKILL_BUBBLE_KEYS = generatedFieldSet<keyof typeof AsyncSkillBubbleSchema.field>()("skillName", "args", "body", "emissions", "fold");
const SPOOL_KEYS = generatedFieldSet<keyof typeof AsyncOutputSpoolSchema.field>()("text", "throughOffset");
const JOURNAL_ROW_KEYS = generatedFieldSet<keyof typeof AsyncWorkflowJournalRowSchema.field>()("label", "detail", "running", "done", "failed");
const LIVENESS_KEYS = generatedFieldSet<keyof typeof AsyncLivenessSchema.field>()("live", "settled");
const LIVE_KEYS = generatedFieldSet<keyof typeof AsyncLiveSchema.field>()("lastActivityMs");
const SETTLED_KEYS = generatedFieldSet<keyof typeof AsyncSettledSchema.field>()("settledAtMs", "shellExit", "done", "error", "killed");
const SHELL_EXIT_KEYS = generatedFieldSet<keyof typeof AsyncShellExitSchema.field>()("code");
const OUTCOME_ERROR_KEYS = generatedFieldSet<keyof typeof AsyncOutcomeErrorSchema.field>()("message");
const OUTCOME_KILLED_KEYS = generatedFieldSet<keyof typeof AsyncOutcomeKilledSchema.field>()("reason");
const FOLD_KEYS = generatedFieldSet<keyof typeof AsyncFoldSchema.field>()("droppedBefore", "tailCap");
const UPDATE_KEYS = generatedFieldSet<keyof typeof AsyncBubbleUpdateSchema.field>()("bubbleId", "agent", "journal", "shell", "unclassified", "liveness", "merge", "skill");
const AGENT_UPDATE_KEYS = generatedFieldSet<keyof typeof AsyncAgentUpdateSchema.field>()("emissions", "fold");
const SKILL_UPDATE_KEYS = generatedFieldSet<keyof typeof AsyncSkillUpdateSchema.field>()("body", "emissions");
const SKILL_BODY_RESOLVED_KEYS = generatedFieldSet<keyof typeof AsyncSkillBodyResolvedSchema.field>()("contents");
const JOURNAL_UPDATE_KEYS = generatedFieldSet<keyof typeof AsyncWorkflowJournalUpdateSchema.field>()("rows", "fold");
const OUTPUT_APPEND_KEYS = generatedFieldSet<keyof typeof AsyncOutputAppendSchema.field>()("text", "fromOffset");
const LIVENESS_UPDATE_KEYS = generatedFieldSet<keyof typeof AsyncLivenessUpdateSchema.field>()("liveness");
const DELTA_KEYS = generatedFieldSet<keyof typeof AsyncBubbleDeltaSchema.field>()("workspace", "opened", "updates", "throughSeq", "fence");

/**
 * The EMPTY marker messages, each anchored to its own generated stub.
 *
 * Their key sets are empty on purpose and checked against the generated field
 * list: a field ADDED to one of them fails this build rather than being
 * silently ignored, which is the guarantee `REVIVAL_HOLD_FIELDS` gives in
 * `proto-names.ts`. They are separate constants rather than one shared
 * `EMPTY_KEY_SET` precisely so each message's emptiness is asserted
 * independently.
 */
const STEP_RUNNING_KEYS = generatedFieldSet<keyof typeof AsyncWorkflowStepRunningSchema.field>()();
const STEP_DONE_KEYS = generatedFieldSet<keyof typeof AsyncWorkflowStepDoneSchema.field>()();
const STEP_FAILED_KEYS = generatedFieldSet<keyof typeof AsyncWorkflowStepFailedSchema.field>()();
const OUTCOME_DONE_KEYS = generatedFieldSet<keyof typeof AsyncOutcomeDoneSchema.field>()();

/** Each journal step status arm → the anchored key set proving it is empty. */
const JOURNAL_STATUS_KEYS: Readonly<Record<AsyncWorkflowJournalRow["status"], ReadonlySet<string>>> = {
  running: STEP_RUNNING_KEYS,
  done: STEP_DONE_KEYS,
  failed: STEP_FAILED_KEYS,
};

/** The bubble `kind` arm keys, typed against the generated oneof. */
const BUBBLE_KIND_ARMS = ["agent", "journal", "shell", "unclassified", "merge", "skill"] as const satisfies readonly ArmKeys<GeneratedAsyncBubble["kind"]>[];
/** The update arm keys, typed against the generated oneof. */
const UPDATE_ARMS = ["agent", "journal", "shell", "unclassified", "liveness", "merge", "skill"] as const satisfies readonly ArmKeys<GeneratedAsyncBubbleUpdate["update"]>[];
/** The skill update's own arm keys, typed against the generated oneof. */
const SKILL_UPDATE_ARMS = ["body", "emissions"] as const satisfies readonly ArmKeys<GeneratedAsyncSkillUpdate["update"]>[];
/** The liveness state arms, typed against the generated oneof. */
const LIVENESS_ARMS = ["live", "settled"] as const satisfies readonly ArmKeys<GeneratedAsyncLiveness["state"]>[];
/** The settled outcome arms, typed against the generated oneof. */
const OUTCOME_ARMS = ["done", "error", "killed"] as const satisfies readonly ArmKeys<GeneratedAsyncSettled["outcome"]>[];
/** The journal row status arms, typed against the generated oneof. */
const JOURNAL_STATUS_ARMS = ["running", "done", "failed"] as const satisfies readonly ArmKeys<GeneratedAsyncWorkflowJournalRow["status"]>[];

// --- decoders ---------------------------------------------------------------

function decodeFold(v: unknown, ctx: string): AsyncFold {
  if (v === undefined || v === null) return { droppedBefore: 0, tailCap: 0 };
  const o = ensureObject(v, ctx);
  rejectUnknown(o, FOLD_KEYS, ctx);
  const fold: AsyncFold = {
    droppedBefore: num(o, "droppedBefore", ctx),
    tailCap: num(o, "tailCap", ctx),
  };
  if (fold.droppedBefore < 0) {
    throw new Error(`frontend-proto: ${ctx}.droppedBefore must not be negative`);
  }
  if (fold.tailCap < 0) {
    throw new Error(`frontend-proto: ${ctx}.tailCap must not be negative`);
  }
  return fold;
}

function decodeSpool(v: unknown, ctx: string): AsyncOutputSpool {
  // An empty body is a bubble that has produced nothing YET — a real state on
  // this contract ("a newly opened bubble carries an empty body"), not a
  // missing field. Its cursor is 0, which is the offset the first append must
  // carry.
  if (v === undefined || v === null) return { text: "", throughOffset: 0 };
  const o = ensureObject(v, ctx);
  rejectUnknown(o, SPOOL_KEYS, ctx);
  return { text: str(o, "text", ctx), throughOffset: offset(o, "throughOffset", ctx) };
}

function decodeShellExit(v: unknown, ctx: string): AsyncShellExit {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, SHELL_EXIT_KEYS, ctx);
  return { code: num(o, "code", ctx) };
}

function decodeOutcome(o: Obj, ctx: string): AsyncOutcome {
  const arm = oneof(o, OUTCOME_ARMS, `${ctx}.outcome`);
  switch (arm) {
    case "done":
      // An EMPTY message: being set is the entire assertion.
      rejectUnknown(ensureObject(o.done, `${ctx}.done`), OUTCOME_DONE_KEYS, `${ctx}.done`);
      return { case: "done" };
    case "error": {
      const e = ensureObject(o.error, `${ctx}.error`);
      rejectUnknown(e, OUTCOME_ERROR_KEYS, `${ctx}.error`);
      // Empty when the source reported failure without a reason — never filled
      // with a manufactured one.
      return { case: "error", message: str(e, "message", `${ctx}.error`) };
    }
    default: {
      const k = ensureObject(o.killed, `${ctx}.killed`);
      rejectUnknown(k, OUTCOME_KILLED_KEYS, `${ctx}.killed`);
      return { case: "killed", reason: str(k, "reason", `${ctx}.killed`) };
    }
  }
}

function decodeSettled(v: unknown, ctx: string): AsyncSettled {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, SETTLED_KEYS, ctx);
  const settled: AsyncSettled = {
    settledAtMs: int64OrZero(o, "settledAtMs", ctx),
    outcome: decodeOutcome(o, ctx),
  };
  // Absent shell_exit is "this work did not exit, it concluded" — a fact, not a
  // missing field, so it is never defaulted to 0 (which would read as a clean
  // exit).
  if (o.shellExit !== undefined && o.shellExit !== null) {
    settled.shellExit = decodeShellExit(o.shellExit, `${ctx}.shellExit`);
  }
  return settled;
}

/** Decode an `AsyncLiveness`. A bubble is ALWAYS one of live or settled. */
export function decodeLiveness(v: unknown, ctx: string): AsyncLiveness {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, LIVENESS_KEYS, ctx);
  const arm = oneof(o, LIVENESS_ARMS, `${ctx}.state`);
  if (arm === "live") {
    const live = ensureObject(o.live, `${ctx}.live`);
    rejectUnknown(live, LIVE_KEYS, `${ctx}.live`);
    return { case: "live", value: { lastActivityMs: num(live, "lastActivityMs", `${ctx}.live`) } };
  }
  return { case: "settled", value: decodeSettled(o.settled, `${ctx}.settled`) };
}

function decodeJournalRow(v: unknown, ctx: string): AsyncWorkflowJournalRow {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, JOURNAL_ROW_KEYS, ctx);
  const status = oneof(o, JOURNAL_STATUS_ARMS, `${ctx}.status`) as AsyncWorkflowJournalRow["status"];
  // Each status is an EMPTY marker message; a field on one is a wire the
  // renderer does not know how to draw.
  rejectUnknown(ensureObject(o[status], `${ctx}.${status}`), JOURNAL_STATUS_KEYS[status], `${ctx}.${status}`);
  return { label: str(o, "label", ctx), detail: str(o, "detail", ctx), status };
}

function decodeEmissions(v: unknown, ctx: string): UnwrappedEmission[] {
  if (v === undefined || v === null) return [];
  return ensureArray(v, ctx).map((e, i) => unwrapAgentEmission(e, `${ctx}[${i}]`));
}

function decodeJournalRows(v: unknown, ctx: string): AsyncWorkflowJournalRow[] {
  if (v === undefined || v === null) return [];
  return ensureArray(v, ctx).map((r, i) => decodeJournalRow(r, `${ctx}[${i}]`));
}

function decodeBubbleKind(o: Obj, ctx: string): AsyncBubbleKind {
  const arm = oneof(o, BUBBLE_KIND_ARMS, `${ctx}.kind`);
  switch (arm) {
    case "agent": {
      const a = ensureObject(o.agent, `${ctx}.agent`);
      rejectUnknown(a, AGENT_BUBBLE_KEYS, `${ctx}.agent`);
      return {
        case: "agent",
        value: {
          emissions: decodeEmissions(a.emissions, `${ctx}.agent.emissions`),
          fold: decodeFold(a.fold, `${ctx}.agent.fold`),
        },
      };
    }
    case "journal": {
      const j = ensureObject(o.journal, `${ctx}.journal`);
      rejectUnknown(j, JOURNAL_KEYS, `${ctx}.journal`);
      return {
        case: "journal",
        value: {
          rows: decodeJournalRows(j.rows, `${ctx}.journal.rows`),
          fold: decodeFold(j.fold, `${ctx}.journal.fold`),
        },
      };
    }
    case "shell": {
      const s = ensureObject(o.shell, `${ctx}.shell`);
      rejectUnknown(s, SHELL_BUBBLE_KEYS, `${ctx}.shell`);
      return {
        case: "shell",
        value: {
          command: str(s, "command", `${ctx}.shell`),
          output: decodeSpool(s.output, `${ctx}.shell.output`),
        },
      };
    }
    case "merge": {
      // The SAME shape as `agent`, decoded by the same rules: a merge run is a
      // conversation, and its emissions are the feed's own vocabulary.
      const m = ensureObject(o.merge, `${ctx}.merge`);
      rejectUnknown(m, MERGE_BUBBLE_KEYS, `${ctx}.merge`);
      return {
        case: "merge",
        value: {
          emissions: decodeEmissions(m.emissions, `${ctx}.merge.emissions`),
          fold: decodeFold(m.fold, `${ctx}.merge.fold`),
        },
      };
    }
    case "skill": {
      const s = ensureObject(o.skill, `${ctx}.skill`);
      rejectUnknown(s, SKILL_BUBBLE_KEYS, `${ctx}.skill`);
      return {
        case: "skill",
        value: {
          skillName: str(s, "skillName", `${ctx}.skill`),
          args: str(s, "args", `${ctx}.skill`),
          body: str(s, "body", `${ctx}.skill`),
          emissions: decodeEmissions(s.emissions, `${ctx}.skill.emissions`),
          fold: decodeFold(s.fold, `${ctx}.skill.fold`),
        },
      };
    }
    default: {
      const u = ensureObject(o.unclassified, `${ctx}.unclassified`);
      rejectUnknown(u, UNCLASSIFIED_BUBBLE_KEYS, `${ctx}.unclassified`);
      return {
        case: "unclassified",
        value: {
          toolName: str(u, "toolName", `${ctx}.unclassified`),
          output: decodeSpool(u.output, `${ctx}.unclassified.output`),
        },
      };
    }
  }
}

/** Decode one `AsyncBubble`. */
export function decodeAsyncBubble(v: unknown, ctx: string): AsyncBubble {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, BUBBLE_KEYS, ctx);
  const id = str(o, "id", ctx);
  if (id === "") {
    // "Never empty" on the contract: a detachment the daemon cannot attribute
    // to a tool call is a daemon fault surfaced as a failure card, not a bubble
    // with a blank id. Accepting one here would mint an unroutable bubble.
    throw new Error(`frontend-proto: ${ctx}.id is empty — a bubble's routing handle is never empty`);
  }
  if (o.liveness === undefined || o.liveness === null) {
    throw new Error(`frontend-proto: ${ctx}.liveness is absent — a bubble is always live or settled`);
  }
  return {
    id,
    workspace: str(o, "workspace", ctx),
    originToolUseId: str(o, "originToolUseId", ctx),
    parentBubbleId: str(o, "parentBubbleId", ctx),
    label: str(o, "label", ctx),
    startedAtMs: int64OrZero(o, "startedAtMs", ctx),
    liveness: decodeLiveness(o.liveness, `${ctx}.liveness`),
    kind: decodeBubbleKind(o, ctx),
  };
}

function decodeOutputAppend(v: unknown, ctx: string): AsyncOutputAppend {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, OUTPUT_APPEND_KEYS, ctx);
  return { text: str(o, "text", ctx), fromOffset: offset(o, "fromOffset", ctx) };
}

function decodeAgentUpdate(v: unknown, ctx: string): AsyncAgentUpdate {
  const a = ensureObject(v, ctx);
  rejectUnknown(a, AGENT_UPDATE_KEYS, ctx);
  return {
    emissions: decodeEmissions(a.emissions, `${ctx}.emissions`),
    fold: decodeFold(a.fold, `${ctx}.fold`),
  };
}

function decodeSkillUpdateArm(v: unknown, ctx: string): AsyncSkillUpdateArm {
  const s = ensureObject(v, ctx);
  rejectUnknown(s, SKILL_UPDATE_KEYS, ctx);
  const arm = oneof(s, SKILL_UPDATE_ARMS, `${ctx}.update`);
  if (arm === "body") {
    const b = ensureObject(s.body, `${ctx}.body`);
    rejectUnknown(b, SKILL_BODY_RESOLVED_KEYS, `${ctx}.body`);
    return { case: "body", value: str(b, "contents", `${ctx}.body`) };
  }
  return { case: "emissions", value: decodeAgentUpdate(s.emissions, `${ctx}.emissions`) };
}

function decodeUpdateArm(o: Obj, ctx: string): AsyncBubbleUpdateArm {
  const arm = oneof(o, UPDATE_ARMS, `${ctx}.update`);
  switch (arm) {
    case "agent":
      return { case: "agent", value: decodeAgentUpdate(o.agent, `${ctx}.agent`) };
    case "merge":
      // The SAME payload as `agent`: a merge run's emissions arrive exactly as
      // a detached agent's do, so the arm names the kind and nothing else.
      return { case: "merge", value: decodeAgentUpdate(o.merge, `${ctx}.merge`) };
    case "skill":
      return { case: "skill", value: decodeSkillUpdateArm(o.skill, `${ctx}.skill`) };
    case "journal": {
      const j = ensureObject(o.journal, `${ctx}.journal`);
      rejectUnknown(j, JOURNAL_UPDATE_KEYS, `${ctx}.journal`);
      return {
        case: "journal",
        value: {
          rows: decodeJournalRows(j.rows, `${ctx}.journal.rows`),
          fold: decodeFold(j.fold, `${ctx}.journal.fold`),
        },
      };
    }
    case "shell":
      return { case: "shell", value: decodeOutputAppend(o.shell, `${ctx}.shell`) };
    case "unclassified":
      return { case: "unclassified", value: decodeOutputAppend(o.unclassified, `${ctx}.unclassified`) };
    default: {
      const l = ensureObject(o.liveness, `${ctx}.liveness`);
      rejectUnknown(l, LIVENESS_UPDATE_KEYS, `${ctx}.liveness`);
      if (l.liveness === undefined || l.liveness === null) {
        throw new Error(`frontend-proto: ${ctx}.liveness carries no liveness`);
      }
      return { case: "liveness", value: decodeLiveness(l.liveness, `${ctx}.liveness.liveness`) };
    }
  }
}

/** Decode one `AsyncBubbleUpdate`. */
export function decodeAsyncBubbleUpdate(v: unknown, ctx: string): AsyncBubbleUpdate {
  const o = ensureObject(v, ctx);
  rejectUnknown(o, UPDATE_KEYS, ctx);
  const bubbleId = str(o, "bubbleId", ctx);
  if (bubbleId === "") {
    // The id is the ONLY thing an update carries to say where it lands
    // (invariant I2). An empty one is not routable by any means this frontend
    // is allowed to use, so it is refused rather than matched by some other
    // evidence.
    throw new Error(`frontend-proto: ${ctx}.bubbleId is empty — an update names its bubble or it is unroutable`);
  }
  return { bubbleId, update: decodeUpdateArm(o, ctx) };
}

/** Decode one `AsyncBubbleDelta`. */
export function decodeAsyncBubbleDelta(v: unknown): AsyncBubbleDelta {
  const ctx = "AsyncBubbleDelta";
  const o = ensureObject(v, ctx);
  rejectUnknown(o, DELTA_KEYS, ctx);
  const delta: AsyncBubbleDelta = {
    workspace: str(o, "workspace", ctx),
    opened: (o.opened === undefined || o.opened === null
      ? []
      : ensureArray(o.opened, `${ctx}.opened`)
    ).map((b, i) => decodeAsyncBubble(b, `${ctx}.opened[${i}]`)),
    updates: (o.updates === undefined || o.updates === null
      ? []
      : ensureArray(o.updates, `${ctx}.updates`)
    ).map((u, i) => decodeAsyncBubbleUpdate(u, `${ctx}.updates[${i}]`)),
    throughSeq: num(o, "throughSeq", ctx),
    fence: str(o, "fence", ctx),
  };
  if (delta.fence === "") {
    // The fence is how a client tells a current push from a stale one. A push
    // without one cannot be gated at all, so it is refused rather than adopted
    // ungated.
    throw new Error(`frontend-proto: ${ctx} missing required \`fence\``);
  }
  return delta;
}
