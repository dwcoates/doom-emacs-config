package reuse

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
)

// addScriptGraph places a script suite's SCRIPT and every script of DIRS it
// reaches by name: a script names another by its file name, as `source
// "$here/lib-worktrees.sh"` or `"$BIN/land-direct.sh"` do, so each file of
// DIRS whose name a placed script's text holds is placed too, and read in
// its turn. Each directory's listing is placed, so a glob over it still
// sees every name. The graph OVER-PLACES rather than under: a name a script
// only mentions is placed all the same, and a script reached by a name its
// text builds at run time is a read its profile refuses, loudly (2026-11-13:
// fifteen harnesses that each declared the whole of bin/ re-ran on a change
// to one script none of them reads).
func addScriptGraph(d Deps, set *InputSet, script string, dirs []string) error {
	byName := map[string][]string{}
	for _, dir := range dirs {
		rel, _, err := RepoPath(d, dir)
		if err != nil {
			return err
		}
		if err := set.AddDir(rel); err != nil {
			return err
		}
		root := filepath.Join(d.Repo, filepath.FromSlash(rel))
		err = filepath.WalkDir(root, func(abs string, e fs.DirEntry, err error) error {
			if err != nil {
				return fmt.Errorf("reuse: walk the script directory %s: %w", rel, err)
			}
			fileRel, err := filepath.Rel(d.Repo, abs)
			if err != nil {
				return err
			}
			fileRel = filepath.ToSlash(fileRel)
			if d.Ignored.Has(fileRel) {
				if e.IsDir() {
					return filepath.SkipDir
				}
				return nil
			}
			if e.IsDir() {
				if fileRel != rel {
					return set.AddDir(fileRel)
				}
				return nil
			}
			// A SCRIPT IS A REGULAR FILE, or a link to one: a link to a
			// directory (a skill linked into .claude/skills/) names no script.
			if !e.Type().IsRegular() {
				info, err := os.Stat(abs)
				if err != nil {
					return fmt.Errorf("reuse: stat %s: %w", fileRel, err)
				}
				if !info.Mode().IsRegular() {
					return nil
				}
			}
			byName[e.Name()] = append(byName[e.Name()], fileRel)
			return nil
		})
		if err != nil {
			return err
		}
	}
	start, _, err := RepoPath(d, script)
	if err != nil {
		return err
	}
	names := make([]string, 0, len(byName))
	for name := range byName {
		names = append(names, name)
	}
	sort.Strings(names)
	placed := map[string]bool{start: true}
	queue := []string{start}
	for len(queue) > 0 {
		next := queue[0]
		queue = queue[1:]
		text, err := os.ReadFile(filepath.Join(d.Repo, filepath.FromSlash(next)))
		if err != nil {
			return fmt.Errorf("reuse: read the script %s: %w", next, err)
		}
		for _, name := range names {
			if !strings.Contains(string(text), name) {
				continue
			}
			for _, rel := range byName[name] {
				if placed[rel] {
					continue
				}
				placed[rel] = true
				if err := set.AddFile(rel); err != nil {
					return err
				}
				queue = append(queue, rel)
			}
		}
	}
	return nil
}
