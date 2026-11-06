// identity.go reads the identities commits carry and the user's global one, for
// the merge queue's identity check before a landing (merge/identity.go).
package gitclient

import (
	"context"
	"strings"

	"agent-repld/internal/dlog"
)

// Identity is a git identity: a name and an email.
type Identity struct {
	Name  string
	Email string
}

// String is the identity as git prints one: "Name <email>".
func (i Identity) String() string { return i.Name + " <" + i.Email + ">" }

// CommitIdentity is one commit's author and committer.
type CommitIdentity struct {
	// SHA is the full commit sha.
	SHA string
	// Subject is the first line of the message.
	Subject   string
	Author    Identity
	Committer Identity
}

// identityFormat is the one identity template: sha, subject, author name and
// email, committer name and email.
const identityFormat = "%H" + fieldSep + "%s" + fieldSep + "%an" + fieldSep + "%ae" + fieldSep + "%cn" + fieldSep + "%ce"

// GlobalIdentity reads `git config --global user.name` and `user.email`. A key
// that is not set is an ORDINARY ANSWER, its field left empty (git's exit 1);
// whether an empty field is acceptable is the caller's to judge. Any other
// failure is git's and fails loudly.
func (c *client) GlobalIdentity(ctx context.Context, dir string) (Identity, error) {
	const operation = "daemon.gitclient.global_identity"
	var id Identity
	for _, field := range []struct {
		key  string
		into *string
	}{{"user.name", &id.Name}, {"user.email", &id.Email}} {
		in, err := c.runRaw(ctx, operation, dir, "config", "--global", field.key)
		if err != nil {
			return Identity{}, err
		}
		switch in.exitCode {
		case 0:
			*field.into = strings.TrimRight(in.stdout, "\n")
		case 1:
		default:
			c.log.Global().Error(operation, "git exited nonzero", in.logContext())
			return Identity{}, in.fail()
		}
	}
	return id, nil
}

// CommitIdentities lists the author and committer of every commit reachable
// from tip and not from base, oldest first, merges INCLUDED: everything a
// fast-forward of base to tip would bring in.
func (c *client) CommitIdentities(ctx context.Context, dir, base, tip string) ([]CommitIdentity, error) {
	const operation = "daemon.gitclient.commit_identities"
	out, err := c.run(ctx, operation, dir,
		"rev-list", "--reverse", "--format="+identityFormat, "--no-commit-header", "--end-of-options", base+".."+tip)
	if err != nil {
		return nil, err
	}
	records, err := c.formattedRecords(operation, dir, out, 6, dlog.Context{"base": base, "tip": tip})
	if err != nil || records == nil {
		return nil, err
	}
	commits := make([]CommitIdentity, 0, len(records))
	for _, fields := range records {
		commits = append(commits, CommitIdentity{
			SHA:       fields[0],
			Subject:   fields[1],
			Author:    Identity{Name: fields[2], Email: fields[3]},
			Committer: Identity{Name: fields[4], Email: fields[5]},
		})
	}
	return commits, nil
}
