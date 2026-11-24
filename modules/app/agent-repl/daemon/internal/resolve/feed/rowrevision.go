package feed

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"

	"google.golang.org/protobuf/proto"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// A ROW'S REVISION IS ITS CONTENT IN ONE VALUE (FeedRow.revision). A reader
// that opens a feed it already holds hands back the revision of every row it
// drew (agentrepl.v1.FeedResume), and the open leaves out each row whose
// revision is unchanged, so a reattach that finds nothing newer sends and
// draws nothing.
//
// IT IS STATED ON THE ONE WRITE PATH (`upsert`), after every other stamp, so
// it covers everything the publication says; and it is computed from the
// row's bytes alone, so two daemons publishing equal rows publish equal
// revisions, which is what lets a successor answer a page the incumbent drew.

// revisionBytes is how many bytes of the sha256 a revision keeps: 16 bytes
// make a collision between two publications of one row unreachable.
const revisionBytes = 16

// stampRevision states ROW's revision: the digest of its deterministic
// encoding with the revision itself left out.
func stampRevision(row *frontendv1.FeedRow) error {
	row.Revision = nil
	encoded, err := proto.MarshalOptions{Deterministic: true}.Marshal(row)
	if err != nil {
		return fmt.Errorf("encode the row to digest its revision: %w", err)
	}
	sum := sha256.Sum256(encoded)
	row.Revision = &frontendv1.FeedRowRevision{Digest: hex.EncodeToString(sum[:revisionBytes])}
	return nil
}
