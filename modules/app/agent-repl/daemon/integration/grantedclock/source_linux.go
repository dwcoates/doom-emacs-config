package grantedclock

// Source answers the stall this binary's bounds are kept on. On linux it is
// the container's (CgroupCPUStall): the only place these suites run on linux
// is the e2e sandbox, whose cgroup keeps the union of every process's stall.
func Source() (Stall, error) {
	return CgroupCPUStall, nil
}
