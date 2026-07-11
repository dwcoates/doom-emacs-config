#!/usr/bin/env node
// smoke.mjs — real-SDK smoke driver for claude-repld (plan step 2).
// Creates a fake:false session on the target daemon, drives the §2 flows,
// and prints a PASS/FAIL line per check. Node >=22 (native WebSocket).
//
// Usage: node smoke.mjs http://127.0.0.1:8788

const base = process.argv[2] ?? "http://127.0.0.1:8788";
const wsBase = base.replace(/^http/, "ws");
const MODEL = "haiku"; // cheap alias, resolved by the CLI
const CWD = "/tmp/agent-repl-smoke";

const results = [];
function check(name, ok, detail = "") {
  results.push({ name, ok });
  console.log(`${ok ? "PASS" : "FAIL"} - ${name}${detail ? ` :: ${detail}` : ""}`);
}

function timeout(ms, why) {
  return new Promise((_, rej) => setTimeout(() => rej(new Error(`timeout: ${why}`)), ms));
}

// --- create session ---------------------------------------------------------
const createResp = await fetch(`${base}/sessions`, {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify({ fake: false, cwd: CWD, model: MODEL }),
});
check("POST /sessions (fake:false)", createResp.ok, String(createResp.status));
const { session_id } = await createResp.json();
console.log(`session: ${session_id}`);

// --- attach WS ---------------------------------------------------------------
const frames = [];
const waiters = [];
const ws = new WebSocket(`${wsBase}/sessions/${session_id}/stream`);
ws.addEventListener("message", (ev) => {
  const frame = JSON.parse(ev.data);
  frames.push(frame);
  for (const w of [...waiters]) w();
});
function waitFor(pred, why, ms = 90000) {
  const scan = () => frames.find(pred);
  const hit = scan();
  if (hit) return Promise.resolve(hit);
  return Promise.race([
    new Promise((resolve) => {
      const w = () => {
        const h = scan();
        if (h) {
          waiters.splice(waiters.indexOf(w), 1);
          resolve(h);
        }
      };
      waiters.push(w);
    }),
    timeout(ms, why),
  ]);
}
await new Promise((resolve, reject) => {
  ws.addEventListener("open", resolve);
  ws.addEventListener("error", (e) => reject(new Error(`ws error: ${e.message}`)));
});

const hello = await waitFor((f) => f.type === "hello", "hello");
check("hello frame", !!hello, `model=${hello.model} cwd=${hello.cwd} mode=${hello.permission_mode}`);

function send(obj) {
  ws.send(JSON.stringify(obj));
}

// --- turn 1: plain text ------------------------------------------------------
send({ type: "user-message", request_id: "smoke-t1", content: "Reply with exactly the word: pong" });
await waitFor((f) => f.type === "user-turn" && f.request_id === "smoke-t1", "user-turn echo");
check("user-turn broadcast", true);
const t1start = await waitFor((f) => f.type === "text-start", "text-start (auth/streaming)", 120000);
check("text streaming started (auth works)", !!t1start);
const t1end = await waitFor((f) => f.type === "text-end", "text-end");
check("text-end", /pong/i.test(t1end.final_text), JSON.stringify(t1end.final_text).slice(0, 80));
const t1res = await waitFor((f) => f.type === "result", "result 1");
check("result subtype success", t1res.subtype === "success", `${t1res.subtype} cost=$${t1res.total_cost_usd}`);

// --- turn 2: read-only tool (no prompt expected: default rules allow Read) ------
send({
  type: "user-message",
  request_id: "smoke-t2",
  content: "Read the file marker.txt in the current directory using the Read tool and tell me the answer number it contains. Do not use any other tool.",
});
const toolStart = await waitFor((f) => f.type === "tool-use-start", "tool-use-start (Read)", 120000);
check("tool-use-start for Read without prompt", toolStart.tool_name === "Read", toolStart.tool_name);
const toolResult = await waitFor((f) => f.type === "tool-use-result", "tool-use-result", 120000);
check("tool-use-result ok", !toolResult.is_error);
const t2end = await waitFor((f) => f.type === "text-end" && frames.indexOf(f) > frames.indexOf(toolResult), "post-tool text");
check("tool answer mentions 42", /42/.test(t2end.final_text), JSON.stringify(t2end.final_text).slice(0, 80));
await waitFor((f) => f.type === "result" && frames.indexOf(f) > frames.indexOf(t2end), "result 2");

// --- turn 3: Bash tool + allow (prompt expected) ---------------------------------
send({
  type: "user-message",
  request_id: "smoke-t3",
  content: "Run exactly `cat marker.txt` with the Bash tool and repeat its output.",
});
const permReq = await waitFor((f) => f.type === "permission-request", "permission-request (Bash)", 120000);
check("permission-request", !!permReq.request_id && permReq.tool_name === "Bash", `tool=${permReq.tool_name}`);
check("tool_use_id non-empty (SDK toolUseID plumb)", typeof permReq.tool_use_id === "string" && permReq.tool_use_id.length > 0, permReq.tool_use_id);
send({ type: "permission-decision", request_id: permReq.request_id, decision: { behavior: "allow" } });
const resolved = await waitFor((f) => f.type === "permission-resolved" && f.request_id === permReq.request_id, "permission-resolved");
check("permission-resolved allow", resolved.decision === "allow");
const bashResult = await waitFor(
  (f) => f.type === "tool-use-result" && frames.indexOf(f) > frames.indexOf(permReq),
  "bash tool-use-result",
  120000,
);
check("allowed Bash ran", !bashResult.is_error);
const res3 = await waitFor(
  (f) => f.type === "result" && frames.indexOf(f) > frames.indexOf(bashResult),
  "result 3",
  120000,
);
check("allow round-trip completes turn", res3.subtype === "success", res3.subtype);

// --- turn 4: Bash tool + deny -----------------------------------------------------
send({
  type: "user-message",
  request_id: "smoke-t4",
  content: "Run `ls -la /` with the Bash tool. If the tool is denied, reply exactly: denied-ok",
});
const permReq4 = await waitFor(
  (f) => f.type === "permission-request" && frames.indexOf(f) > frames.indexOf(res3),
  "permission-request 4",
  120000,
);
send({
  type: "permission-decision",
  request_id: permReq4.request_id,
  decision: { behavior: "deny", message: "smoke denial" },
});
const res4 = await waitFor(
  (f) => f.type === "result" && frames.indexOf(f) > frames.indexOf(permReq4),
  "result 4",
  120000,
);
check("deny round-trip completes turn", !!res4, `subtype=${res4.subtype}`);
// Observable deny effect: the model saw the denial and produced the
// scripted acknowledgement. (SDK left permission_denials empty on this
// path — the spec field is optional; recorded as an SDK behavior note.)
const denyText = frames.filter((f) => f.type === "text-end" && frames.indexOf(f) > frames.indexOf(permReq4)).at(-1);
check("model acknowledged the denial", !!denyText && /denied-ok/i.test(denyText.final_text), JSON.stringify(denyText?.final_text ?? "").slice(0, 80));

// --- permission mode round-trip -------------------------------------------------
send({ type: "set-permission-mode", request_id: "smoke-mode", mode: "acceptEdits" });
const modeChanged = await waitFor((f) => f.type === "permission-mode-changed", "permission-mode-changed");
check("permission-mode-changed", modeChanged.mode === "acceptEdits", modeChanged.mode);

// --- teardown -------------------------------------------------------------------
const del = await fetch(`${base}/sessions/${session_id}`, { method: "DELETE" });
check("DELETE /sessions", del.ok, String(del.status));
ws.close();

const failed = results.filter((r) => !r.ok);
console.log(`\n=== SMOKE ${failed.length === 0 ? "PASS" : "FAIL"} (${results.length - failed.length}/${results.length}) ===`);
process.exit(failed.length === 0 ? 0 : 1);
