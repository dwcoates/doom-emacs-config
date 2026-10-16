package integration

// SUBJECT — WHERE the keep-alive marker sits in a prompt.
//
// The marker is a PREFIX rule, and the reason it has to be is that the marker's
// own text is ordinary prose: a person quoting it while asking about keep-alive
// prompts must not have their turn silently withheld from their own feed. The
// two placements are therefore the two edges of one rule, and they must be
// driven separately — a subject that only proves the opening marker withholds a
// turn is equally satisfied by a reader matching the marker ANYWHERE.
