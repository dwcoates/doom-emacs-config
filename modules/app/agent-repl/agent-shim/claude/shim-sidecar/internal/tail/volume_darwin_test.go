package tail

import (
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/sys/unix"
)

// TestAFilesIdentityNamesItsVolumeByMountPoint pins the spelling that survives
// a reboot: darwin's st_dev moves at every mount, the mount point does not.
func TestAFilesIdentityNamesItsVolumeByMountPoint(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "t.jsonl")
	writeFile(t, path, `{"a":1}`+"\n")
	var fs unix.Statfs_t
	if err := unix.Statfs(path, &fs); err != nil {
		t.Fatalf("statfs: %v", err)
	}
	mount := unix.ByteSliceToString(fs.Mntonname[:])

	// Act.
	id, err := Identity(path)

	// Assert.
	if err != nil {
		t.Fatalf("identity: %v", err)
	}
	if !strings.HasPrefix(id, mount+":") {
		t.Fatalf("identity %q does not name the volume by its mount point %q", id, mount)
	}
}
