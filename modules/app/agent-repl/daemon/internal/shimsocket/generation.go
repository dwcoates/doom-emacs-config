package shimsocket

import (
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
)

// GenerationSuffix is what a relaunch's fresh socket path is minted with:
// `<workspace>.sock` becomes `<workspace>.nN.sock`, N counting the shims one
// daemon has spawned for that workspace (workspace.Fleet.freshSocketPath).
const GenerationSuffix = ".n"

// NewestLive resolves the socket path a workspace's CURRENT shim is listening
// on, given the base path the state root's layout names.
//
// IT EXISTS BECAUSE THE GENERATION COUNTER IS NOT DURABLE. A relaunch moves a
// workspace's shim onto `<base>.nN.sock` and the counter that minted N lives in
// the session fleet's memory, so the next daemon's boot knows only the BASE
// path — and dials a path the surviving shim has not held since the relaunch.
// The lock still reads HELD (the process is alive), so the redial ladder never
// stops, which is the ten-hour accept wedge of pid 31984.
//
// The candidates are every generation that EXISTS on disk, newest first, with
// the base path last: a generation is only ever bumped, so a higher N is a
// later shim. The first candidate a probe calls LIVE is the answer. When none
// is live the BASE path is answered with its own probe result, which is what
// every caller's stale-clear and spawn path is already written against.
//
// The probe is taken as an argument rather than called directly so a caller's
// own injected probe — boot's seam, the fleet's — decides liveness, and one
// resolution cannot disagree with the probe the same caller then reports.
func NewestLive(probe func(string) (State, error), base string) (string, State, error) {
	baseState, baseErr := probe(base)
	if baseState == StateLive {
		return base, baseState, baseErr
	}
	for _, candidate := range generations(base) {
		state, err := probe(candidate)
		if state == StateLive {
			return candidate, state, err
		}
	}
	return base, baseState, baseErr
}

// generations names the `<base>.nN.sock` paths that exist beside base, newest
// N first. A directory that cannot be read yields none: the base path is then
// the only candidate, which is the answer a daemon with no relaunch history
// wants anyway.
func generations(base string) []string {
	found := numberedGenerations(base)
	paths := make([]string, 0, len(found))
	for _, g := range found {
		paths = append(paths, g.path)
	}
	return paths
}

// generation is one `<base>.nN.sock` path on disk and its N.
type generation struct {
	n    int
	path string
}

// numberedGenerations is generations with each N kept, newest first.
func numberedGenerations(base string) []generation {
	dir := filepath.Dir(base)
	stem := strings.TrimSuffix(filepath.Base(base), ".sock")
	if stem == filepath.Base(base) {
		// Not a `.sock` path at all; there is no generation spelling for it.
		return nil
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		return nil
	}
	var found []generation
	prefix := stem + GenerationSuffix
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasPrefix(name, prefix) || !strings.HasSuffix(name, ".sock") {
			continue
		}
		digits := strings.TrimSuffix(strings.TrimPrefix(name, prefix), ".sock")
		n, convErr := strconv.Atoi(digits)
		if convErr != nil || n <= 0 {
			continue
		}
		found = append(found, generation{n: n, path: filepath.Join(dir, name)})
	}
	sort.Slice(found, func(i, j int) bool { return found[i].n > found[j].n })
	return found
}

// NextGeneration mints the socket path of a workspace's NEXT shim generation:
// `<base>.nN.sock` with N one past both `after` (the caller's own counter) and
// every generation that exists beside base on disk. It answers the path and N.
//
// THE DISK IS READ BECAUSE THE COUNTER IS NOT DURABLE. A successor adopts a
// shim a predecessor already relaunched onto `.n1.sock`, and its own counter
// starts at zero, so a counter-only mint handed the replacement the very
// socket the running shim held: the replacement refused to bind and died at
// once (deploy 2026-09-24T18:27:44, three workspaces).
func NextGeneration(base string, after int) (string, int) {
	n := after
	if found := numberedGenerations(base); len(found) > 0 && found[0].n > n {
		n = found[0].n
	}
	n++
	return strings.TrimSuffix(base, ".sock") + GenerationSuffix + strconv.Itoa(n) + ".sock", n
}
