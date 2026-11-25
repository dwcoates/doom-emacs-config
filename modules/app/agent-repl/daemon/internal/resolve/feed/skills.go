package feed

import (
	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/skillrefs"
	"agentrepl/logging/logsite"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE TEXT'S SKILL LINKS. An inline code span naming one of the
// workspace's skills draws as a link to that skill's file (skillrefs.Linkify),
// the file the workspace's standing catalog says its session resolves the
// name to (conversation.v1.SessionCommandCatalogEntry.skill_file). It has two
// edges, as a prompt's known commands do (commands.go):
//
//   - DRAWN with the text's other links, by linkSkills, from the catalog in
//     hand (Deps.Commands);
//   - REDRAWN when the session states a catalog, by relinkSkills, so a row
//     drawn before any catalog was stated (a replay at daemon start), or
//     before the context reading named the skills' files, gains its links
//     then. A link once drawn stays: the redraw reads the rows as drawn, and
//     a drawn link is no longer a code span.

// linkSkills rewrites every drawn markdown of PARSED, workspace WS's text,
// with its skill names linked. Only the text's end can still grow while it is
// ARRIVING.
func (r *resolver) linkSkills(ws ids.WorkspaceID, parsed *frontendv1.ParsedProse, arriving bool) {
	files := r.skillFiles(ws)
	eachDrawnMarkdown(parsed, true, func(markdown string, ends bool) string {
		return skillrefs.Linkify(markdown, files, r.deps.Home, arriving && ends)
	})
}

// skillFiles answers the skill files workspace WS's standing catalog names,
// none while no catalog is known.
func (r *resolver) skillFiles(ws ids.WorkspaceID) skillrefs.Files {
	if r.deps.Commands == nil {
		return func(string) (string, bool) { return "", false }
	}
	catalog, _ := r.deps.Commands(ws)
	return func(name string) (string, bool) {
		for _, entry := range catalog.GetCommands() {
			if entry.GetName() == name && entry.SkillFile != nil {
				return entry.GetSkillFile(), true
			}
		}
		return "", false
	}
}

// relinkSkills re-publishes every row of the workspace, on every feed, whose
// drawn text gains a skill link from the catalog just stated. A row whose text
// names no newly known skill is left as it stands.
func (r *resolver) relinkSkills(s *wsState) {
	relinked := 0
	for key, f := range s.feeds {
		for _, id := range f.order {
			row, ok := r.snapshotRow(s, f.rows[id], unclonable{
				operation: "daemon.feed.skills_relink_unclonable",
				message:   logsite.Msg("a row whose skill names were to be linked could not be cloned; it was not re-pushed"),
				context:   dlog.Context{"feed": f.key, "row": id},
			})
			if !ok {
				continue
			}
			eachParsedProse(row.ProtoReflect(), func(parsed *frontendv1.ParsedProse) {
				r.linkSkills(s.id, parsed, false)
			})
			if proto.Equal(row, f.rows[id]) {
				continue
			}
			r.upsert(s, placement{feed: s.feedAddrs[key]}, row, !f.nonDurable[id])
			relinked++
		}
	}
	r.logger(s.id).Debug("daemon.feed.skills_relinked",
		"the session stated its command catalog; every drawn row naming one of its skills was linked to the skill's file",
		dlog.Context{"rows": relinked})
}

// eachParsedProse calls VISIT with every frontend.v1.ParsedProse M holds, at
// any depth, so a redraw reaches the text of every kind of row without naming
// each kind.
func eachParsedProse(m protoreflect.Message, visit func(*frontendv1.ParsedProse)) {
	if parsed, ok := m.Interface().(*frontendv1.ParsedProse); ok {
		visit(parsed)
		return
	}
	m.Range(func(fd protoreflect.FieldDescriptor, v protoreflect.Value) bool {
		if fd.Kind() != protoreflect.MessageKind && fd.Kind() != protoreflect.GroupKind {
			return true
		}
		switch {
		case fd.IsList():
			list := v.List()
			for i := 0; i < list.Len(); i++ {
				eachParsedProse(list.Get(i).Message(), visit)
			}
		case fd.IsMap():
			if fd.MapValue().Kind() == protoreflect.MessageKind {
				v.Map().Range(func(_ protoreflect.MapKey, mv protoreflect.Value) bool {
					eachParsedProse(mv.Message(), visit)
					return true
				})
			}
		default:
			eachParsedProse(v.Message(), visit)
		}
		return true
	})
}
