/**
 * convert/vendor-process.ts — the session's VENDOR PROCESS as a standing level
 * (conversation.v1.SessionVendorProcess).
 *
 * WHY IT IS STATED AT ALL. Another Claude session's record of a message from
 * this one names its sender by the kernel-verified pid of the vendor process
 * that connected, so the daemon maps that pid back to this workspace through
 * this level. The pid is the vendor child the shim spawned: the vendor names
 * its messaging socket after it (`/tmp/cc-socks/<pid>.sock`).
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../log.js";
import { conversationv1 } from "../proto.js";

const LOGGER = bindLog({ component: "shim-convert-vendor-process", operation: "shim.convert.vendor_process" });

/**
 * The level for a child that has a pid. `startedAtMs` is read AFTER the spawn
 * returned the pid, so it is never earlier than the process's true start, and a
 * consumer that refuses events older than it can never pin an older process's
 * event on this one.
 */
export function vendorProcessRunning(pid: number, startedAtMs: number): conversationv1.SessionVendorProcess {
  if (!Number.isInteger(pid) || pid <= 0) {
    const detail = `vendor-process: a running vendor process has a positive pid, got ${pid}`;
    LOGGER.error({ pid, detail }, detail);
    throw new Error(detail);
  }
  return create(conversationv1.SessionVendorProcessSchema, {
    state: {
      case: "running",
      value: create(conversationv1.SessionVendorProcessRunningSchema, { pid, startedAtMs: BigInt(startedAtMs) }),
    },
  });
}

/** The level once the running child exited and no next one runs yet. */
export function vendorProcessEnded(): conversationv1.SessionVendorProcess {
  return create(conversationv1.SessionVendorProcessSchema, {
    state: { case: "ended", value: create(conversationv1.SessionVendorProcessEndedSchema, {}) },
  });
}

/**
 * Whether an exit ends the level standing now. Only the CURRENT child's exit
 * does: a replaced child's late exit names a pid the level no longer states,
 * and ending the level on it would erase the child that replaced it.
 */
export function exitEndsLevel(level: conversationv1.SessionVendorProcess | undefined, exitedPid: number | undefined): boolean {
  return level?.state.case === "running" && exitedPid !== undefined && level.state.value.pid === exitedPid;
}

/** The session update carrying the level. */
export function vendorProcessUpdate(level: conversationv1.SessionVendorProcess): conversationv1.SessionUpdate {
  return create(conversationv1.SessionUpdateSchema, { update: { case: "vendorProcess", value: level } });
}
