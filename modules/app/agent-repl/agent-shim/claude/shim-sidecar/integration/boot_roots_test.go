package integration

import (
	"strings"
	"testing"

	"agentrepl/shim-claude-sidecar/internal/discover"
)

// SUBJECT — the boot record names the roots THE SIDECAR ACTUALLY GLOBS.
//
// `--config-roots` and `--spool-root` are taken verbatim off the command line,
// but discovery symlink-resolves every root before it globs and every `path` a
// later record carries is spelled the resolved way. On macOS that is the whole
// difference between `/tmp` and `/private/tmp`, and between `/var/folders/...`
// and `/private/var/folders/...`. A boot record printing the flag's spelling
// therefore names a root that PREFIXES NONE of the thousands of paths logged
// beneath it, which is exactly the join an operator opens this record to make.
//
// The tree here lives under t.TempDir(), so on macOS the two spellings really
// do differ and the subject is a live discrimination rather than a tautology;
// on a platform where they coincide it degrades to asserting the resolved
// spelling, which is still the contract.

// TestTheBootRecordNamesTheRootsAsGlobbed drives one boot and checks each root
// the record owes against the spelling discovery resolved it to.
func TestTheBootRecordNamesTheRootsAsGlobbed(t *testing.T) {
	t.Parallel()
	// Arrange.
	ctx, cancel := testContext(t)
	defer cancel()
	fake := startFakeStore(t)
	tree := newVendorTree(t)
	opts := defaultSidecarOptions(t, fake.Socket, tree)

	// Act.
	startSidecar(t, opts)
	start := awaitLog(ctx, t, opts.LogPath, "the process start record", func(r logRecord) bool {
		return r.Operation == "start"
	})

	// Assert.
	for _, tc := range []struct {
		name string
		flag string
	}{
		{name: "config root", flag: tree.Root},
		{name: "spool root", flag: tree.SpoolRoot},
	} {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			resolved := discover.Normalize(tc.flag)
			if !strings.Contains(start.Message, resolved) {
				t.Fatalf("the start record does not name the %s as discovery globs it.\n  want the resolved spelling: %s\n  the flag was passed as:     %s\n  record: %s",
					tc.name, resolved, tc.flag, start.Message)
			}
		})
	}
}
