/**
 * The readiness pipe: one byte once the shim listens, then closed.
 */
import { describe, expect, it } from "vitest";
import { READY_FD, reportReady, type ReadyIo } from "../src/ready.js";

/** A recording ReadyIo whose write throws WRITE_ERROR when given. */
function recordingIo(writeError?: NodeJS.ErrnoException): ReadyIo & { calls: string[] } {
  const calls: string[] = [];
  return {
    calls,
    write: (fd, byte) => {
      calls.push(`write ${String(fd)} ${Array.from(byte).join(",")}`);
      if (writeError !== undefined) throw writeError;
      return byte.length;
    },
    close: (fd) => {
      calls.push(`close ${String(fd)}`);
    },
  };
}

/** An errno-shaped error with CODE. */
function errno(code: string): NodeJS.ErrnoException {
  return Object.assign(new Error(code), { code });
}

describe("reportReady", () => {
  it("writes one byte to the readiness descriptor and closes it", () => {
    const io = recordingIo();
    expect(reportReady(READY_FD, io)).toBe("reported");
    expect(io.calls).toEqual(["write 5 1", "close 5"]);
  });

  it("answers daemon_gone when the daemon left no reader, and still closes", () => {
    const io = recordingIo(errno("EPIPE"));
    expect(reportReady(READY_FD, io)).toBe("daemon_gone");
    expect(io.calls).toEqual(["write 5 1", "close 5"]);
  });

  it("throws any other write failure, after closing", () => {
    const io = recordingIo(errno("EBADF"));
    expect(() => reportReady(READY_FD, io)).toThrow("EBADF");
    expect(io.calls).toEqual(["write 5 1", "close 5"]);
  });
});
