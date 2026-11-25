/**
 * The paint vocabulary's inventory (proto/vocab/paint-classes.json): every
 * syntax and ANSI paint name, which the paint mapping's tests and the
 * stylesheet's paint rules (rules/feed/cards/paint.rules.test.ts) both walk.
 */
import paintClasses from "../../../../proto/vocab/paint-classes.json";

const SYNTAX: readonly string[] = paintClasses.syntax;
const ANSI: readonly string[] = paintClasses.ansi;

/** Every paint name the vocabulary carries, syntax first. */
export const PAINT_INVENTORY: readonly string[] = [...SYNTAX, ...ANSI];
