/**
 * engine/api-reachability.ts — whether the vendor's API host can be reached,
 * asked without spending a token.
 *
 * THE PROBE IS ONE REQUEST THE API DOES NOT BILL: a `HEAD` of the configured
 * base URL (DNS resolve, TCP connect, TLS handshake and an HTTP answer), or —
 * behind a proxy — a `CONNECT` through that proxy to the API's host. ANY HTTP
 * answer from the API is "reachable": a 404 or a 405 is still the API
 * answering. Only failing to get an answer at all is "unreachable".
 *
 * IT PROBES WHAT THE VENDOR IS CONFIGURED FOR. `ANTHROPIC_BASE_URL` when set
 * (the vendor child inherits this process's environment), the public API
 * otherwise; `HTTPS_PROXY`/`HTTP_PROXY` (either case) and `NO_PROXY` decide
 * whether the request goes direct. A base URL that does not parse is answered
 * "unreachable" with the reason, never replaced by the default.
 */
import * as http from "node:http";
import * as https from "node:https";
import { existsSync } from "node:fs";
import { bindLog } from "../log.js";
import type { ProbeAnswer, ReachabilityProbe } from "./network-resume.js";

const LOGGER = bindLog({ component: "shim-engine-api-reachability", operation: "shim.engine.api_reachability" });

/** The public API, when nothing configures another. */
export const DEFAULT_API_BASE_URL = "https://api.anthropic.com";

/**
 * How long one probe waits for an answer. Under the loop's five-second beat,
 * so a probe settles before the next beat would have fired.
 */
export const PROBE_TIMEOUT_MS = 4_000;

/** Where a probe goes: the API, and the proxy it goes through, if any. */
export type ReachabilityTarget =
  | { readonly kind: "direct"; readonly url: URL }
  | { readonly kind: "proxied"; readonly url: URL; readonly proxy: URL }
  | { readonly kind: "unresolvable"; readonly detail: string };

/** The environment variables the target is read from. */
type Env = Readonly<Record<string, string | undefined>>;

function envValue(env: Env, ...names: string[]): string | undefined {
  for (const name of names) {
    const value = env[name];
    if (value !== undefined && value.trim() !== "") return value.trim();
  }
  return undefined;
}

/**
 * Whether `NO_PROXY` exempts a host.
 *
 * The common reading: a comma- or space-separated list whose entries are `*`
 * (everything), a bare host (that host and its subdomains), or a `.suffix`
 * (the subdomains). A `:port` on an entry is ignored.
 */
export function noProxyExempts(noProxy: string | undefined, host: string): boolean {
  if (noProxy === undefined) return false;
  const hostname = host.toLowerCase();
  for (const raw of noProxy.split(/[\s,]+/)) {
    const entry = raw.trim().toLowerCase().replace(/:\d+$/, "");
    if (entry === "") continue;
    if (entry === "*") return true;
    const bare = entry.startsWith(".") ? entry.slice(1) : entry;
    if (hostname === bare || hostname.endsWith(`.${bare}`)) return true;
  }
  return false;
}

/** Resolve where a probe goes, from the environment the vendor also reads. */
export function resolveReachabilityTarget(env: Env): ReachabilityTarget {
  const configured = envValue(env, "ANTHROPIC_BASE_URL") ?? DEFAULT_API_BASE_URL;
  let url: URL;
  try {
    url = new URL(configured);
  } catch {
    return { kind: "unresolvable", detail: `ANTHROPIC_BASE_URL ${JSON.stringify(configured)} is not a URL` };
  }
  if (url.protocol !== "https:" && url.protocol !== "http:") {
    return { kind: "unresolvable", detail: `the API base URL ${JSON.stringify(configured)} is not http(s)` };
  }
  if (noProxyExempts(envValue(env, "NO_PROXY", "no_proxy"), url.hostname)) return { kind: "direct", url };
  const proxyRaw =
    url.protocol === "https:"
      ? envValue(env, "HTTPS_PROXY", "https_proxy")
      : envValue(env, "HTTP_PROXY", "http_proxy");
  if (proxyRaw === undefined) return { kind: "direct", url };
  let proxy: URL;
  try {
    proxy = new URL(proxyRaw);
  } catch {
    return { kind: "unresolvable", detail: `the proxy ${JSON.stringify(proxyRaw)} is not a URL` };
  }
  return { kind: "proxied", url, proxy };
}

/**
 * Record, once per process, which host the probe will ask.
 *
 * A target that cannot be resolved is an ERROR here and only here: every probe
 * of it then answers "unreachable" with the same reason, so a wait gives up
 * loudly rather than probing a host the vendor does not use.
 */
export function recordReachabilityTarget(target: ReachabilityTarget): void {
  switch (target.kind) {
    case "unresolvable":
      LOGGER.error(
        { detail: target.detail },
        "the API host the vendor is configured for cannot be resolved; every network-resume probe will answer unreachable",
      );
      return;
    case "direct":
      LOGGER.debug({ api_origin: target.url.origin }, "the network-resume probe asks the API host directly");
      return;
    case "proxied":
      LOGGER.debug(
        { api_origin: target.url.origin, proxy: target.proxy.host },
        "the network-resume probe asks the API host through the configured proxy",
      );
      return;
  }
}

/** The two requests a probe can make; injected so no suite touches a network. */
export interface ProbeTransport {
  /** `HEAD` the URL; resolves with the status of whatever answered, rejects on no answer. */
  head(url: URL, timeoutMs: number): Promise<number>;
  /** `CONNECT host:port` through the proxy; resolves with the proxy's status, rejects on no answer. */
  connect(proxy: URL, host: string, port: number, timeoutMs: number): Promise<number>;
}

function defaultPort(url: URL): number {
  if (url.port !== "") return Number(url.port);
  return url.protocol === "https:" ? 443 : 80;
}

/** Node's own `http`/`https`: the transport a real session probes with. */
export const NODE_TRANSPORT: ProbeTransport = {
  head(url, timeoutMs) {
    return new Promise((resolve, reject) => {
      const lib = url.protocol === "https:" ? https : http;
      const request = lib.request(url, { method: "HEAD", timeout: timeoutMs }, (response) => {
        response.resume();
        resolve(response.statusCode ?? 0);
      });
      request.on("timeout", () => {
        request.destroy(new Error(`no answer within ${String(timeoutMs)}ms`));
      });
      request.on("error", reject);
      request.end();
    });
  },
  connect(proxy, host, port, timeoutMs) {
    return new Promise((resolve, reject) => {
      const lib = proxy.protocol === "https:" ? https : http;
      const headers: Record<string, string> = { host: `${host}:${String(port)}` };
      if (proxy.username !== "") {
        const credentials = `${decodeURIComponent(proxy.username)}:${decodeURIComponent(proxy.password)}`;
        headers["proxy-authorization"] = `Basic ${Buffer.from(credentials).toString("base64")}`;
      }
      const request = lib.request({
        host: proxy.hostname,
        port: defaultPort(proxy),
        method: "CONNECT",
        path: `${host}:${String(port)}`,
        headers,
        timeout: timeoutMs,
      });
      request.on("connect", (response, socket) => {
        socket.destroy();
        resolve(response.statusCode ?? 0);
      });
      request.on("timeout", () => {
        request.destroy(new Error(`the proxy gave no answer within ${String(timeoutMs)}ms`));
      });
      request.on("error", reject);
      request.end();
    });
  },
};

/**
 * A probe of the target.
 *
 * NEVER THROWS: every way of not getting an answer is the answer
 * "unreachable", with the reason.
 */
export function createReachabilityProbe(
  target: ReachabilityTarget,
  transport: ProbeTransport = NODE_TRANSPORT,
  timeoutMs: number = PROBE_TIMEOUT_MS,
): ReachabilityProbe {
  return async (): Promise<ProbeAnswer> => {
    switch (target.kind) {
      case "unresolvable":
        return { reachable: false, detail: target.detail };
      case "direct":
        try {
          const status = await transport.head(target.url, timeoutMs);
          return { reachable: true, detail: `${target.url.origin} answered HEAD with ${String(status)}` };
        } catch (err) {
          return { reachable: false, detail: `${target.url.origin}: ${describe(err)}` };
        }
      case "proxied":
        try {
          const port = defaultPort(target.url);
          const status = await transport.connect(target.proxy, target.url.hostname, port, timeoutMs);
          // THE PROXY ANSWERED, BUT ONLY A 2xx MEANS IT REACHED THE API. A 502
          // or 504 is the proxy saying the API host is what it could not reach.
          const reachable = status >= 200 && status < 300;
          return {
            reachable,
            detail: `the proxy ${target.proxy.host} answered CONNECT ${target.url.hostname}:${String(port)} with ${String(status)}`,
          };
        } catch (err) {
          return { reachable: false, detail: `the proxy ${target.proxy.host}: ${describe(err)}` };
        }
    }
  };
}

/**
 * The mocked vendor's probe: no network at all.
 *
 * With no gate the API is always reachable; with a gate path it is reachable
 * exactly while that path exists, so a suite can hold an outage open and end
 * it with one file.
 */
export function createFakeReachabilityProbe(gatePath: string | undefined): ReachabilityProbe {
  return () => {
    if (gatePath === undefined) return Promise.resolve({ reachable: true, detail: "the mocked vendor is always reachable" });
    const reachable = existsSync(gatePath);
    return Promise.resolve({
      reachable,
      detail: reachable ? `the fake reachability gate ${gatePath} exists` : `the fake reachability gate ${gatePath} is absent`,
    });
  };
}

function describe(err: unknown): string {
  if (err instanceof Error) {
    const code = (err as { code?: unknown }).code;
    return typeof code === "string" && !err.message.includes(code) ? `${code}: ${err.message}` : err.message;
  }
  return String(err);
}
