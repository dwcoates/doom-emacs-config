package wsm

// selfCloseDDL is the layout-63 addition: close_when_turn_ends, the mark a
// workspace that asked to close itself carried while its asking turn ran. The
// standing close request replaced it (closerequest.go): layout 215 carries
// every mark into the request and layout 216 retires the column.
const selfCloseDDL = `
ALTER TABLE workspaces ADD COLUMN close_when_turn_ends INTEGER;
`
