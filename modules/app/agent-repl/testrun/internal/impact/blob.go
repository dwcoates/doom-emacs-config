// Package impact is the test runner's test impact selection, in SHADOW MODE:
// it records which files each top-level test of a coverage-recorded suite
// depended on, keyed on the CONTENT of those files, and says which tests a
// later tree would let a run skip, without ever skipping one.
//
// A record says: this test, run on files with exactly these git blob hashes
// (and these non-file facts: the toolchain, the build tags), ended in this
// outcome. A later tree matches a record when every file the record names has
// the same blob hash in it. Only a passing record whose every dependency
// matches makes a test one that WOULD be skipped (Decide).
//
// The store (Store) is shared by every workspace on the host, beside the
// run history, and is never the workspace_test_cases rerun table.
package impact

import (
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"os"
	"strconv"
)

// BlobHashBytes is the git blob hash of content: the SHA-1 of "blob <size>\x00"
// followed by the content, as `git hash-object` answers it. It is computed in
// process, so no run spawns git for it.
func BlobHashBytes(content []byte) string {
	h := sha1.New()
	h.Write([]byte("blob " + strconv.Itoa(len(content)) + "\x00"))
	h.Write(content)
	return hex.EncodeToString(h.Sum(nil))
}

// BlobHash is the git blob hash of the file at path.
func BlobHash(path string) (string, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("impact: hash %s: %w", path, err)
	}
	return BlobHashBytes(content), nil
}
