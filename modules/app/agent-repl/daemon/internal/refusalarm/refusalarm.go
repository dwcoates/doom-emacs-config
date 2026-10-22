// Package refusalarm holds the one spelling of a typed refusal field that
// names an arm of a oneof INSIDE the refusal's own error arm, so every verb
// package and the rpc layer share it rather than each declaring its own.
package refusalarm

// Nested is a refusal field value naming an arm of a oneof inside the
// `<Rpc>Error` arm message (the unexpected increment's `reason`, the bubble
// refusal's `kind`). The refusal keys it by the oneof's own name, and the rpc
// layer sets that arm by name. The nested arms are empty messages, so the arm
// NAME is the whole value.
type Nested string
