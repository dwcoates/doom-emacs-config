package feed

import (
	"sort"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/reflect/protoreflect"
)

// changedFieldsDepth bounds how far changedFields descends: deep enough to
// name the stamp a republication changed (row.activity.response.interim), and
// no deeper, so the walk stays a handful of comparisons per republication.
const changedFieldsDepth = 4

// changedFields names, as sorted dotted paths, the fields that differ between
// two versions of one message: set in one and not the other, or set to
// different values. A field that differs below the depth bound is named at the
// bound. Two equal messages answer nothing.
func changedFields(before, after proto.Message) []string {
	var out []string
	diffMessages(before.ProtoReflect(), after.ProtoReflect(), "", changedFieldsDepth, &out)
	sort.Strings(out)
	return out
}

func diffMessages(before, after protoreflect.Message, prefix string, depth int, out *[]string) {
	seen := map[protoreflect.FieldNumber]bool{}
	visit := func(fd protoreflect.FieldDescriptor) {
		if seen[fd.Number()] {
			return
		}
		seen[fd.Number()] = true
		path := prefix + string(fd.Name())
		inBefore, inAfter := before.Has(fd), after.Has(fd)
		if !inBefore && !inAfter {
			return
		}
		if inBefore != inAfter {
			*out = append(*out, path)
			return
		}
		b, a := before.Get(fd), after.Get(fd)
		if fd.Message() != nil && !fd.IsList() && !fd.IsMap() && depth > 1 {
			diffMessages(b.Message(), a.Message(), path+".", depth-1, out)
			return
		}
		if !fieldValuesEqual(fd, b, a) {
			*out = append(*out, path)
		}
	}
	before.Range(func(fd protoreflect.FieldDescriptor, _ protoreflect.Value) bool { visit(fd); return true })
	after.Range(func(fd protoreflect.FieldDescriptor, _ protoreflect.Value) bool { visit(fd); return true })
}

// fieldValuesEqual compares one field's two values: lists element by element,
// maps key by key, messages by proto.Equal, scalars by value.
func fieldValuesEqual(fd protoreflect.FieldDescriptor, b, a protoreflect.Value) bool {
	switch {
	case fd.IsList():
		bl, al := b.List(), a.List()
		if bl.Len() != al.Len() {
			return false
		}
		for i := 0; i < bl.Len(); i++ {
			if !singularEqual(fd, bl.Get(i), al.Get(i)) {
				return false
			}
		}
		return true
	case fd.IsMap():
		bm, am := b.Map(), a.Map()
		if bm.Len() != am.Len() {
			return false
		}
		equal := true
		bm.Range(func(k protoreflect.MapKey, v protoreflect.Value) bool {
			other := am.Get(k)
			if !am.Has(k) || !singularEqual(fd.MapValue(), v, other) {
				equal = false
			}
			return equal
		})
		return equal
	default:
		return singularEqual(fd, b, a)
	}
}

// singularEqual compares one element's two values.
func singularEqual(fd protoreflect.FieldDescriptor, b, a protoreflect.Value) bool {
	if fd.Message() != nil {
		return proto.Equal(b.Message().Interface(), a.Message().Interface())
	}
	return b.Equal(a)
}
