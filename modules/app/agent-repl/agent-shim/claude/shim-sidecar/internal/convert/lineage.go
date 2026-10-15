package convert

// lineage.go — a set of records descending from known roots by parentUuid.
//
// Records arrive in file order and a parent is always written before its
// child, so one pass decides descent: a record whose parent is in the set
// joins it. The keep-alive rule (keepalive.go) and a compaction boundary
// awaiting its summary (contextcut.go) both ask this one question.

// lineage is the uuids of its roots and of every record that descends from one.
type lineage map[string]struct{}

// newLineage is a lineage holding ROOTS.
func newLineage(roots ...string) lineage {
	l := lineage{}
	for _, root := range roots {
		l.add(root)
	}
	return l
}

// has reports whether UUID is in the lineage. The empty uuid never is.
func (l lineage) has(uuid string) bool {
	if uuid == "" {
		return false
	}
	_, ok := l[uuid]
	return ok
}

// add puts UUID in the lineage. The empty uuid names no record and is never
// added.
func (l lineage) add(uuid string) {
	if uuid != "" {
		l[uuid] = struct{}{}
	}
}

// extend adds UUID when PARENT is in the lineage, and reports whether it was.
func (l lineage) extend(uuid, parent string) bool {
	if !l.has(parent) {
		return false
	}
	l.add(uuid)
	return true
}
