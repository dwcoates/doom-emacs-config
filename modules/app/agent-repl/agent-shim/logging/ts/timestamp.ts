/**
 * The parts of the agent-repl logging contract that every TypeScript runtime
 * must answer identically.
 *
 * The shim and the webapp are separate packages with separate sinks, but their
 * records are read together, so the representation lives here rather than in
 * each package. `agent-shim/logging/go` holds the same answer for Go and
 * `agent-repl--log-timestamp-format` in `core.el` holds it for elisp;
 * `proto/vocab/log-timestamp.json` is the seam the three are asserted against.
 */

function pad(value: number, width: number): string {
  return String(value).padStart(width, "0");
}

/**
 * Renders an instant as RFC 3339 in the machine's local zone, on a 24-hour
 * clock, with fixed-width microseconds and an explicit numeric offset.
 *
 * Local zone rather than UTC because an operator reads these logs on the
 * machine that wrote them. Fixed-width fractional digits because a record
 * landing on a whole second would otherwise sort out of order against its
 * neighbors. JavaScript resolves instants only to milliseconds, so the last
 * three microsecond digits are always zero rather than a narrower field.
 */
export function logTimestamp(at: Date = new Date()): string {
  const offsetMinutes = -at.getTimezoneOffset();
  const sign = offsetMinutes < 0 ? "-" : "+";
  const offset = Math.abs(offsetMinutes);
  return (
    `${pad(at.getFullYear(), 4)}-${pad(at.getMonth() + 1, 2)}-${pad(at.getDate(), 2)}` +
    `T${pad(at.getHours(), 2)}:${pad(at.getMinutes(), 2)}:${pad(at.getSeconds(), 2)}` +
    `.${pad(at.getMilliseconds(), 3)}000` +
    `${sign}${pad(Math.floor(offset / 60), 2)}:${pad(offset % 60, 2)}`
  );
}
