package vitestmeasure

import (
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// testrun/vitest's setup files are driven here by node, their own runtime,
// beside a stub "vitest" that hands their hooks to the script: no vitest
// runs.

const stubVitest = `export const hooks = { before: [], after: [] };
export function beforeEach(f) { hooks.before.push(f); }
export function afterEach(f) { hooks.after.push(f); }
`

// setupDir copies the setup files beside the stub, answering the directory.
func setupDir(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	stub := filepath.Join(dir, "node_modules", "vitest")
	if err := os.MkdirAll(stub, 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(stub, "package.json"), []byte(`{"name": "vitest", "type": "module", "main": "index.mjs"}`), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(stub, "index.mjs"), []byte(stubVitest), 0o644); err != nil {
		t.Fatal(err)
	}
	for _, f := range []string{"redundancy-trace.mjs", "redundancy-skip.mjs", "redundancy-names.mjs"} {
		b, err := os.ReadFile(filepath.Join("..", "..", "..", "vitest", f))
		if err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(dir, f), b, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return dir
}

func runNode(t *testing.T, dir, script string, env ...string) (string, error) {
	t.Helper()
	path := filepath.Join(dir, "drive.mjs")
	if err := os.WriteFile(path, []byte(script), 0o644); err != nil {
		t.Fatal(err)
	}
	cmd := exec.Command("node", path)
	cmd.Dir = dir
	cmd.Env = append(os.Environ(), env...)
	out, err := cmd.CombinedOutput()
	return string(out), err
}

func TestTraceRecordsTheFilesATestReads(t *testing.T) {
	// Arrange: one test reads a source through each wrapped read, a second
	// reads nothing, and a read between tests is no test's.
	dir := setupDir(t)
	trace := filepath.Join(dir, "trace.jsonl")
	for _, f := range []string{"a.ts", "b.ts", "c.ts", "between.ts"} {
		if err := os.WriteFile(filepath.Join(dir, f), nil, 0o644); err != nil {
			t.Fatal(err)
		}
	}
	script := `import { hooks } from "vitest";
import { readFileSync, promises } from "node:fs";
import fs from "node:fs";
globalThis.__VITEST_COVERAGE__ = {};
await import("./redundancy-trace.mjs");
const task = (name) => ({ name, type: "test", file: { filepath: "/p/t.test.ts" }, suite: { type: "file" }, result: { state: "pass" } });
const run = async (name, body) => {
  for (const f of hooks.before) f({ task: task(name) });
  await body();
  for (const f of hooks.after) f({ task: task(name) });
};
await run("reads", async () => {
  readFileSync("a.ts", "utf8");
  await promises.readFile(new URL("./b.ts", import.meta.url));
  await new Promise((r) => fs.readFile("c.ts", r));
});
readFileSync("between.ts");
await run("reads nothing", async () => {});
`

	// Act
	out, err := runNode(t, dir, script, TraceEnv+"="+trace)

	// Assert
	if err != nil {
		t.Fatalf("node: %v\n%s", err, out)
	}
	tests, err := readTrace(trace)
	if err != nil {
		t.Fatal(err)
	}
	if len(tests) != 2 {
		t.Fatalf("%d traced tests, want 2", len(tests))
	}
	real, err := filepath.EvalSymlinks(dir)
	if err != nil {
		t.Fatal(err)
	}
	got := map[string]bool{}
	for _, r := range tests[0].Read {
		rr, err := filepath.EvalSymlinks(r)
		if err != nil {
			t.Fatal(err)
		}
		got[rr] = true
	}
	for _, f := range []string{"a.ts", "b.ts", "c.ts"} {
		if !got[filepath.Join(real, f)] {
			t.Errorf("the first test's reads %v lack %s", tests[0].Read, f)
		}
	}
	if len(got) != 3 || len(tests[1].Read) != 0 {
		t.Fatalf("reads = %v and %v, want a.ts, b.ts and c.ts, then none", tests[0].Read, tests[1].Read)
	}
}

func TestSkipSkipsTheListedTestsAlone(t *testing.T) {
	// Arrange
	dir := setupDir(t)
	list := filepath.Join(dir, "skip.json")
	b, err := json.Marshal([]skipped{{File: "/p/t.test.ts", Name: "outer > reads"}})
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(list, b, 0o644); err != nil {
		t.Fatal(err)
	}
	script := `import { hooks } from "vitest";
await import("./redundancy-skip.mjs");
const outer = { name: "outer", type: "suite", suite: { type: "file" } };
for (const name of ["reads", "runs"]) {
  const ctx = { task: { name, type: "test", file: { filepath: "/p/t.test.ts" }, suite: outer }, skip: () => console.log("skipped " + name) };
  for (const f of hooks.before) f(ctx);
}
`

	// Act
	out, err := runNode(t, dir, script, SkipEnv+"="+list)

	// Assert
	if err != nil {
		t.Fatalf("node: %v\n%s", err, out)
	}
	if strings.TrimSpace(out) != "skipped reads" {
		t.Fatalf("output %q, want outer > reads skipped alone", out)
	}
}

func TestSkipRefusesAMissingList(t *testing.T) {
	// Arrange
	dir := setupDir(t)

	// Act
	out, err := runNode(t, dir, `await import("./redundancy-skip.mjs");`, SkipEnv+"=")

	// Assert
	if err == nil || !strings.Contains(out, "AGENT_REPL_REDUNDANCY_SKIP names no skip list") {
		t.Fatalf("err = %v, out = %s", err, out)
	}
}
