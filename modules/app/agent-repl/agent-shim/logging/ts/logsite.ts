/**
 * A log call's SITE, as every TypeScript runtime derives it: the key that
 * names one log call across every record it writes and every commit that
 * keeps it, and the template that doubles as its summary
 * (logging-contract.md "Log sites"). `agent-shim/logging/go/logsite` is the
 * same answer for Go, and both pin one key for one input.
 *
 * A SITE KEY is the first 12 hex characters of the sha256 of the record's
 * runtime, the call's scope and its template, joined by NUL bytes. Go keys a
 * call by the function it is written in; TypeScript has no reliable caller at
 * run time (and the webapp ships minified), so a module names its logger's
 * SCOPE statically when it builds it, and that scope stands in for the
 * function. Nothing dynamic takes part in a key: a template is a string
 * literal whose placeholders ({name}) are filled from the call's named values
 * only when the message is rendered.
 */

/** How many hex characters of the sha256 a key keeps. */
export const SITE_KEY_LENGTH = 12;

/**
 * Whether string type T holds a hole: a template literal type such as
 * `row ${string}`, which is what a template literal with a run-time value in
 * it is typed as, alone or as one arm of a union.
 */
type HasHole<T extends string> = true extends (T extends unknown ? ({} extends Record<T, 1> ? true : false) : never) ? true : false;

/**
 * T when T is a string LITERAL type (or a union of them), never when it is
 * `string` or holds a run-time hole. A parameter typed `Literal<T>` therefore
 * refuses a value built at run time: a template is the site's summary and
 * part of its key.
 */
export type Literal<T extends string> = string extends T ? never : HasHole<T> extends true ? never : T;

/**
 * The placeholder names of the template literal type T, as a union: each
 * `{name}`, with a doubled brace (`{{`, `}}`) read as a literal brace. A
 * malformed template is refused at run time (`placeholders`) and by each
 * runtime's static scan, not here.
 */
export type Placeholders<T extends string, Found extends string = never> =
  T extends `${string}{${infer Tail}`
    ? Tail extends `{${infer After}`
      ? Placeholders<After, Found>
      : Tail extends `${infer Name}}${infer After}`
        ? Placeholders<After, Found | Name>
        : Found
    : Found;

/** The named values of template T: exactly one per placeholder, no others. */
export type LogValues<T extends string> = { readonly [K in Placeholders<T>]: unknown };

/**
 * The rest parameter a log method takes after template T: nothing when T
 * names no placeholder, else its named values. So a missing or an extra value
 * is a type error at the call.
 */
export type LogValuesArg<T extends string> = [Placeholders<T>] extends [never] ? [] : [values: LogValues<T>];

/** A placeholder's name: a lowercase identifier. */
const PLACEHOLDER_NAME = /^[a-z_][a-z0-9_]*$/;

/** One parsed piece of a template: literal text, or a placeholder's name. */
export type TemplatePart = { literal: string } | { name: string };

/**
 * Parse TEMPLATE into its literal runs and placeholders, in order. A template
 * whose braces do not parse throws, naming the fault: an unclosed or empty
 * placeholder, a name that is not a lowercase identifier, or a lone closing
 * brace. The same rules as Go's `logsite.Placeholders`.
 */
export function parseTemplate(template: string): TemplatePart[] {
  const parts: TemplatePart[] = [];
  let run = "";
  for (let i = 0; i < template.length; i++) {
    const c = template[i];
    if (c === "{" && template[i + 1] === "{") {
      run += "{";
      i++;
    } else if (c === "}" && template[i + 1] === "}") {
      run += "}";
      i++;
    } else if (c === "{") {
      const end = template.indexOf("}", i + 1);
      if (end < 0) {
        throw new Error(`logsite: the template ${JSON.stringify(template)} opens a placeholder at ${i} that never closes (a literal brace is written {{)`);
      }
      const name = template.slice(i + 1, end);
      if (!PLACEHOLDER_NAME.test(name)) {
        throw new Error(`logsite: the template ${JSON.stringify(template)} names the placeholder {${name}}, which is not a lowercase identifier (a literal brace is written {{)`);
      }
      if (run.length > 0) parts.push({ literal: run });
      run = "";
      parts.push({ name });
      i = end;
    } else if (c === "}") {
      throw new Error(`logsite: the template ${JSON.stringify(template)} has a lone closing brace at ${i} (a literal brace is written }})`);
    } else {
      run += c;
    }
  }
  if (run.length > 0) parts.push({ literal: run });
  return parts;
}

/** TEMPLATE's placeholder names, in order of first appearance, each once. */
export function placeholders(template: string): string[] {
  const names: string[] = [];
  for (const part of parseTemplate(template)) {
    if ("name" in part && !names.includes(part.name)) names.push(part.name);
  }
  return names;
}

/**
 * How a named value reads inside a rendered message: a string as it is, an
 * error by its message, a plain object or array as JSON, and anything else
 * as `String` spells it (so a number reads as a template literal reads it).
 */
export function valueText(value: unknown): string {
  if (typeof value === "string") return value;
  if (value instanceof Error) return value.message;
  if (value !== null && typeof value === "object") {
    const encoded = JSON.stringify(value) as string | undefined;
    return encoded ?? String(value);
  }
  return String(value);
}

/**
 * The site key of the call written as TEMPLATE in SCOPE of RUNTIME: the first
 * `SITE_KEY_LENGTH` hex of sha256(runtime NUL scope NUL template).
 */
export function siteKey(runtime: string, scope: string, template: string): string {
  return sha256Hex(`${runtime}\u0000${scope}\u0000${template}`).slice(0, SITE_KEY_LENGTH);
}

/** Whether KEY has a site key's form: `SITE_KEY_LENGTH` lowercase hex. */
export function validSiteKey(key: string): boolean {
  return /^[0-9a-f]{12}$/.test(key);
}

/** A scope's form: dotted lowercase segments ("shim.engine.turn"). */
const SCOPE_FORM = /^[a-z][a-z0-9-]*(\.[a-z0-9][a-z0-9_-]*)+$/;

/** One site of a logger: its key, its template and the template's parts. */
export interface LogSite {
  readonly key: string;
  readonly template: string;
  readonly parts: readonly TemplatePart[];
  readonly names: readonly string[];
}

/**
 * The sites of one logger's scope, each derived once: a call's first record
 * parses its template and hashes its key, and every later record of the call
 * reads them back from the table. So a record costs one map lookup, never a
 * hash.
 */
export class SiteTable {
  private readonly sites = new Map<string, LogSite>();
  readonly runtime: string;
  readonly scope: string;

  constructor(runtime: string, scope: string) {
    this.runtime = runtime;
    this.scope = scope;
    if (!SCOPE_FORM.test(scope)) {
      throw new Error(`logsite: the scope ${JSON.stringify(scope)} is not dotted lowercase segments ("shim.engine.turn")`);
    }
  }

  /** The site written as TEMPLATE in this scope; a malformed template throws. */
  site(template: string): LogSite {
    const known = this.sites.get(template);
    if (known !== undefined) return known;
    if (template.trim() === "") throw new Error("logsite: a template is never blank: it is the site's summary");
    const parts = parseTemplate(template);
    const names: string[] = [];
    for (const part of parts) if ("name" in part && !names.includes(part.name)) names.push(part.name);
    const site: LogSite = { key: siteKey(this.runtime, this.scope, template), template, parts, names };
    this.sites.set(template, site);
    return site;
  }
}

/**
 * SITE's template rendered from VALUES. A placeholder VALUES holds no value
 * for, or a value no placeholder names, throws: the compiler checks a literal
 * call's values, and this is the same check for anything that got past it.
 */
export function renderSite(site: LogSite, values: Readonly<Record<string, unknown>>): string {
  for (const name of Object.keys(values)) {
    if (!site.names.includes(name)) {
      throw new Error(`logsite: the template ${JSON.stringify(site.template)} names no {${name}}, but the call passed a value for it`);
    }
  }
  let rendered = "";
  for (const part of site.parts) {
    if ("literal" in part) {
      rendered += part.literal;
    } else {
      if (!Object.prototype.hasOwnProperty.call(values, part.name)) {
        throw new Error(`logsite: the template ${JSON.stringify(site.template)} names {${part.name}}, which the call's values do not hold`);
      }
      rendered += valueText(values[part.name]);
    }
  }
  return rendered;
}

/**
 * TEMPLATE rendered from VALUES, with `renderSite`'s checks, for a message's
 * text wanted outside its record (an error it is also thrown as).
 */
export function renderTemplate(template: string, values: Readonly<Record<string, unknown>>): string {
  const parts = parseTemplate(template);
  return renderSite({ key: "", template, parts, names: placeholders(template) }, values);
}

// ---------------------------------------------------------------------------
// SHA-256, synchronously. Web Crypto's digest is asynchronous, and a log call
// must render and key its record before it returns, so the webapp cannot use
// it; node's createHash is not in a browser. Each site is hashed once
// (SiteTable), so this runs once per call site per page or process.
// ---------------------------------------------------------------------------

const K = new Uint32Array([
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
]);

/** The lowercase hex sha256 of TEXT's UTF-8 bytes. */
export function sha256Hex(text: string): string {
  const message = new TextEncoder().encode(text);
  const bitLength = message.length * 8;
  const padded = new Uint8Array((((message.length + 8) >> 6) + 1) * 64);
  padded.set(message);
  padded[message.length] = 0x80;
  const view = new DataView(padded.buffer);
  view.setUint32(padded.length - 8, Math.floor(bitLength / 0x100000000));
  view.setUint32(padded.length - 4, bitLength >>> 0);
  const h = new Uint32Array([0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19]);
  const w = new Uint32Array(64);
  for (let offset = 0; offset < padded.length; offset += 64) {
    for (let i = 0; i < 16; i++) w[i] = view.getUint32(offset + i * 4);
    for (let i = 16; i < 64; i++) {
      const a = w[i - 15], b = w[i - 2];
      const s0 = ((a >>> 7) | (a << 25)) ^ ((a >>> 18) | (a << 14)) ^ (a >>> 3);
      const s1 = ((b >>> 17) | (b << 15)) ^ ((b >>> 19) | (b << 13)) ^ (b >>> 10);
      w[i] = (w[i - 16] + s0 + w[i - 7] + s1) >>> 0;
    }
    let [a, b, c, d, e, f, g, hh] = h;
    for (let i = 0; i < 64; i++) {
      const s1 = ((e >>> 6) | (e << 26)) ^ ((e >>> 11) | (e << 21)) ^ ((e >>> 25) | (e << 7));
      const ch = (e & f) ^ (~e & g);
      const t1 = (hh + s1 + ch + K[i] + w[i]) >>> 0;
      const s0 = ((a >>> 2) | (a << 30)) ^ ((a >>> 13) | (a << 19)) ^ ((a >>> 22) | (a << 10));
      const maj = (a & b) ^ (a & c) ^ (b & c);
      const t2 = (s0 + maj) >>> 0;
      hh = g; g = f; f = e; e = (d + t1) >>> 0;
      d = c; c = b; b = a; a = (t1 + t2) >>> 0;
    }
    h[0] += a; h[1] += b; h[2] += c; h[3] += d; h[4] += e; h[5] += f; h[6] += g; h[7] += hh;
  }
  let hex = "";
  for (const word of h) hex += word.toString(16).padStart(8, "0");
  return hex;
}
