/**
 * The spawn gate: the daemon's word that this process is on record.
 *
 * The daemon forks the shim with the read end of a pipe on fd 4 and writes one
 * byte to it only once it has made the shim's pid durable
 * (`wsm.SetSpawnedShimPID`). Until then the shim binds nothing. A daemon killed
 * in between closes the write end with its death, so the shim reads EOF and
 * exits without binding: a shim that ever serves is ALWAYS one whose pid a
 * successor daemon can find, and a successor never meets a starting shim it has
 * no record of (two shims racing for one session socket).
 */
import { createReadStream } from "node:fs";

/** The only descriptor the daemon hands the gate on. */
export const SPAWN_GATE_FD = 4;

/** Whether the daemon opened the gate, or died before it could. */
export type SpawnGate = "opened" | "abandoned";

/**
 * Wait on the gate at FD: one byte is the opening, EOF a daemon that died
 * before recording this process. A read error rejects.
 */
export function awaitSpawnGate(fd: number): Promise<SpawnGate> {
  return new Promise((resolve, reject) => {
    const stream = createReadStream("", { fd, autoClose: true });
    let settled = false;
    const settle = (outcome: SpawnGate): void => {
      if (settled) return;
      settled = true;
      stream.destroy();
      resolve(outcome);
    };
    stream.once("data", () => settle("opened"));
    stream.once("end", () => settle("abandoned"));
    stream.once("error", (err) => {
      if (settled) return;
      settled = true;
      reject(err);
    });
  });
}
