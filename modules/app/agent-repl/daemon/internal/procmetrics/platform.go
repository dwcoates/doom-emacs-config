package procmetrics

import "strings"

// Platform is what tells one platform's kernel records apart: which figures
// its kernel measures at all, and how agent-repl's services and Emacs's
// WebKit processes show in its process table. A Reader answers its own
// platform's (Reader.Platform), so classification and the collector never
// branch on the platform they were built for.
type Platform struct {
	// MeasuresEnergy is whether Usage.EnergyJoules is the kernel's own
	// attribution; a kernel that attributes no energy to a process has no
	// energy series at all, rather than a series of zeros.
	MeasuresEnergy bool
	// MeasuresMemoryPressure is whether Host.MemoryPressure is the kernel's
	// own level, likewise.
	MeasuresMemoryPressure bool
	// ServiceParent answers whether ppid is the service manager that runs the
	// store and the sidecar: a store or sidecar a test started has its test
	// as parent and is not ours.
	ServiceParent func(ids map[int]Identity, ppid int) bool
	// WebContentName is the name of a WebKit web content process, every
	// workspace page's host (webfootprint).
	WebContentName string
	// WebKitRole answers, for a process Emacs runs WebKit in, its role (the
	// key of webkitRoles) and true; a WebKit process of a role this package
	// does not know answers "" and true. Any other process answers false.
	WebKitRole func(ids map[int]Identity, id Identity, emacs int) (string, bool)
}

// Darwin is macOS: launchd (pid 1) runs the services, and WebKit's processes
// are XPC services macOS holds Emacs responsible for, named
// "com.apple.WebKit.<role>[.<variant>]".
var Darwin = Platform{
	MeasuresEnergy:         true,
	MeasuresMemoryPressure: true,
	WebContentName:         darwinWebKitPrefix + "WebContent",
	ServiceParent:          func(_ map[int]Identity, ppid int) bool { return ppid == launchdPID },
	WebKitRole: func(_ map[int]Identity, id Identity, emacs int) (string, bool) {
		if id.Responsible != emacs || !strings.HasPrefix(id.Name, darwinWebKitPrefix) {
			return "", false
		}
		role, _, _ := strings.Cut(strings.TrimPrefix(id.Name, darwinWebKitPrefix), ".")
		if _, known := webkitRoles[role]; !known {
			return "", true
		}
		return role, true
	},
}

// Linux is a freedesktop host: `systemd --user' runs the services (its
// process is named systemd, like the system's own manager), and WebKitGTK's
// processes are Emacs's descendants (through bubblewrap when WebKitGTK
// sandboxes them), named for their executables. Linux attributes no energy to
// a process, and its pressure stall figures are no level.
var Linux = Platform{
	WebContentName: "WebKitWebProcess",
	ServiceParent:  func(ids map[int]Identity, ppid int) bool { return ids[ppid].Name == systemdName },
	WebKitRole: func(ids map[int]Identity, id Identity, emacs int) (string, bool) {
		if !strings.HasPrefix(id.Name, linuxWebKitPrefix) || !descendsFrom(ids, id.PID, emacs) {
			return "", false
		}
		return linuxWebKitRoles[id.Name], true
	},
}

const (
	// launchdPID is launchd's pid.
	launchdPID = 1
	// systemdName is the service manager's process name on Linux.
	systemdName        = "systemd"
	darwinWebKitPrefix = "com.apple.WebKit."
	linuxWebKitPrefix  = "WebKit"
)

// linuxWebKitRoles maps WebKitGTK's executables to their roles.
var linuxWebKitRoles = map[string]string{
	"WebKitWebProcess":     "WebContent",
	"WebKitNetworkProcess": "Networking",
	"WebKitGPUProcess":     "GPU",
}

// descendsFrom answers whether pid is a descendant of ancestor in ids.
// ancestor 0 is no process, so nothing descends from it.
func descendsFrom(ids map[int]Identity, pid, ancestor int) bool {
	if ancestor == 0 {
		return false
	}
	seen := map[int]bool{}
	for p := ids[pid].PPID; p != 0 && !seen[p]; p = ids[p].PPID {
		if p == ancestor {
			return true
		}
		seen[p] = true
	}
	return false
}
