package wsm

// RepositoryWithID answers the repository in repos whose id is id. It is the
// ONE lookup of a workspace record's repository by the id the record names,
// over a list the caller has already read with ListRepositories, so a caller
// that needs the list for anything else pays for one read, not two.
func RepositoryWithID(repos []Repository, id RepoID) (Repository, bool) {
	for _, repo := range repos {
		if repo.ID == id {
			return repo, true
		}
	}
	return Repository{}, false
}

// RepositoryAt answers the repository in repos whose main checkout is dir. dir
// must already be in the registry's spelling (dirpath.Canonical); this
// compares, it does not normalize.
func RepositoryAt(repos []Repository, dir string) (Repository, bool) {
	for _, repo := range repos {
		if repo.Dir == dir {
			return repo, true
		}
	}
	return Repository{}, false
}
