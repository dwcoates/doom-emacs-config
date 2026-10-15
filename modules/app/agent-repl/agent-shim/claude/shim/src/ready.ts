/**
 * The readiness pipe: the shim's word to the daemon that it is listening.
 *
 * The daemon forks the shim with the write end of a pipe on fd 5 and waits on
 * its read end for ONE byte, which the shim writes once its listen socket
 * accepts, and then closes. The daemon dials a shim it knows is listening
 * instead of polling for the socket; a shim that dies first closes the pipe
 * with its death, so a death is never waited out. A shim that closes the pipe
 * without the byte broke the spawn contract, and the daemon fails the
 * bring-up loudly for it.
 */
import { closeSync, writeSync } from "node:fs";

/** The only descriptor the daemon hands the readiness pipe on. */
export const READY_FD = 5;

/** Whether the daemon heard the report, or had already gone. */
export type ReadyReport = "reported" | "daemon_gone";

/** The two descriptor calls the report makes, injectable for a test. */
export interface ReadyIo {
  readonly write: (fd: number, byte: Uint8Array) => number;
  readonly close: (fd: number) => void;
}

const nodeIo: ReadyIo = {
  write: (fd, byte) => writeSync(fd, byte),
  close: (fd) => {
    closeSync(fd);
  },
};

/**
 * Write the one readiness byte to FD and close it. A daemon that died after
 * opening the spawn gate leaves no reader (EPIPE): that is "daemon_gone", and
 * the shim serves on for a successor to adopt. Any other failure throws.
 */
export function reportReady(fd: number, io: ReadyIo = nodeIo): ReadyReport {
  let outcome: ReadyReport = "reported";
  try {
    io.write(fd, Uint8Array.of(1));
  } catch (err) {
    if ((err as NodeJS.ErrnoException).code !== "EPIPE") {
      io.close(fd);
      throw err;
    }
    outcome = "daemon_gone";
  }
  io.close(fd);
  return outcome;
}
