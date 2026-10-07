// Types for the part of anonymize.mjs the TypeScript tests import.

/** What a home directory becomes in a recording. */
export declare const HOME_TOKEN: "${HOME}";

/** The vendor's project-directory spelling of an absolute path. */
export declare function pathSlug(p: string): string;

/** A recording's text with the home token expanded to `home`. */
export declare function expandHome(text: string, home: string): string;
