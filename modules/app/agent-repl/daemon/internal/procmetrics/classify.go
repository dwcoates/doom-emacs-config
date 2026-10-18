package procmetrics

import (
	"errors"
	"path/filepath"
	"sort"
	"strings"
)

// The process label's values.
const (
	ProcessDaemon      = "daemon"
	ProcessDaemonTools = "daemon_tools"
	ProcessStore       = "store"
	ProcessSidecar     = "sidecar"
	ProcessShim        = "shim"
	ProcessShimTools   = "shim_tools"
	ProcessEmacs       = "emacs"
	// processWebKitPrefix prefixes a WebKit process's lowercased role:
	// webkit_webcontent, webkit_networking, webkit_gpu.
	processWebKitPrefix = "webkit_"
)

// The binaries' process names (buildreport.ServiceStore and ServiceSidecar
// name the same binaries).
const (
	storeName   = "shim-store"
	sidecarName = "shim-claude-sidecar"
	nodeName    = "node"
	webkitName  = "com.apple.WebKit."
	// launchdPID is the parent of every launchd-managed service: a store or
	// sidecar a test started has its test as parent and is not ours.
	launchdPID = 1
)

// Group is a set of processes measured together. Workspace is set for a
// shim and its tools only.
type Group struct {
	Process   string
	Workspace string
}

// classification is one scrape's answer: each agent-repl process's group,
// and the argv reads that failed for a reason other than the process exiting.
type classification struct {
	groups map[int]Group
	errs   []error
}

// classify sorts the host's processes into agent-repl's groups (see the
// package comment for the rules). self is the daemon, sockDir the directory
// its shims listen in.
func classify(ids map[int]Identity, self int, sockDir string, args func(int) ([]string, error)) classification {
	out := classification{groups: map[int]Group{}}
	emacs := 0
	if r := ids[self].Responsible; r != self && r > 0 && strings.EqualFold(ids[r].Name, "emacs") {
		emacs = r
	}
	children := map[int][]int{}
	pids := make([]int, 0, len(ids))
	for pid, id := range ids {
		pids = append(pids, pid)
		children[id.PPID] = append(children[id.PPID], pid)
	}
	sort.Ints(pids)
	var shims []int
	for _, pid := range pids {
		id := ids[pid]
		switch {
		case pid == self:
			out.groups[pid] = Group{Process: ProcessDaemon}
		case pid == emacs:
			out.groups[pid] = Group{Process: ProcessEmacs}
		case id.PPID == launchdPID && id.Name == storeName:
			out.groups[pid] = Group{Process: ProcessStore}
		case id.PPID == launchdPID && id.Name == sidecarName:
			out.groups[pid] = Group{Process: ProcessSidecar}
		case emacs != 0 && id.Responsible == emacs && strings.HasPrefix(id.Name, webkitName):
			out.groups[pid] = Group{Process: processWebKitPrefix + strings.ToLower(strings.TrimPrefix(id.Name, webkitName))}
		case id.Name == nodeName:
			argv, err := args(pid)
			if err != nil {
				if !errors.Is(err, ErrGone) {
					out.errs = append(out.errs, err)
				}
				continue
			}
			if ws, ok := shimWorkspace(argv, sockDir); ok {
				out.groups[pid] = Group{Process: ProcessShim, Workspace: ws}
				shims = append(shims, pid)
			}
		}
	}
	// A SHIM'S DESCENDANTS ARE ITS TOOLS, the daemon's other descendants are
	// the daemon's: shims first, so a shim the daemon spawned keeps its own.
	for _, shim := range shims {
		claimDescendants(out.groups, children, shim, Group{Process: ProcessShimTools, Workspace: out.groups[shim].Workspace})
	}
	claimDescendants(out.groups, children, self, Group{Process: ProcessDaemonTools})
	return out
}

// claimDescendants puts every unclassified descendant of root in group,
// without descending into an already classified one (a shim under the
// daemon).
func claimDescendants(groups map[int]Group, children map[int][]int, root int, group Group) {
	for _, child := range children[root] {
		if _, classified := groups[child]; classified {
			continue
		}
		groups[child] = group
		claimDescendants(groups, children, child, group)
	}
}

// shimWorkspace answers the workspace a shim's argv serves: the name of the
// socket it listens on in sockDir, `<workspace>[.nN].sock`.
func shimWorkspace(argv []string, sockDir string) (string, bool) {
	for i := 0; i+1 < len(argv); i++ {
		if argv[i] != "--listen" {
			continue
		}
		socket := argv[i+1]
		if filepath.Dir(socket) != filepath.Clean(sockDir) || !strings.HasSuffix(socket, ".sock") {
			return "", false
		}
		ws, _, _ := strings.Cut(filepath.Base(socket), ".")
		return ws, ws != ""
	}
	return "", false
}
