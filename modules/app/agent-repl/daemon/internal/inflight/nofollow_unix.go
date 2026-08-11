//go:build unix

package inflight

import "syscall"

// syscallNoFollow refuses to open the manifest through a symlink.
//
// The logging contract forbids a runtime from following a workspace-provided
// link as a durable sink, and the manifest is exactly such a sink: it lives
// inside the workspace, where anything could have replaced it with a link
// pointing somewhere else. ELOOP here is a loud failure, which is the correct
// outcome — a manifest that cannot be written safely must not be written at all.
const syscallNoFollow = syscall.O_NOFOLLOW
