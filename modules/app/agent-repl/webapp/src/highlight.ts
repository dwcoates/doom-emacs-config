/**
 * Shared highlight.js setup: one core instance with a lean language
 * set (registered once), plus the escape/highlight helpers the
 * markdown renderer and the tool-result renderers build on. hljs
 * emits its own escaped HTML, so highlightCode output is safe to
 * inject wherever escaped text is.
 */

import hljs from "highlight.js/lib/core";
import bash from "highlight.js/lib/languages/bash";
import c from "highlight.js/lib/languages/c";
import cpp from "highlight.js/lib/languages/cpp";
import css from "highlight.js/lib/languages/css";
import diff from "highlight.js/lib/languages/diff";
import go from "highlight.js/lib/languages/go";
import java from "highlight.js/lib/languages/java";
import javascript from "highlight.js/lib/languages/javascript";
import json from "highlight.js/lib/languages/json";
import lisp from "highlight.js/lib/languages/lisp";
import markdown from "highlight.js/lib/languages/markdown";
import python from "highlight.js/lib/languages/python";
import ruby from "highlight.js/lib/languages/ruby";
import rust from "highlight.js/lib/languages/rust";
import shell from "highlight.js/lib/languages/shell";
import sql from "highlight.js/lib/languages/sql";
import typescript from "highlight.js/lib/languages/typescript";
import xml from "highlight.js/lib/languages/xml";
import yaml from "highlight.js/lib/languages/yaml";

hljs.registerLanguage("bash", bash);
hljs.registerLanguage("c", c);
hljs.registerLanguage("cpp", cpp);
hljs.registerLanguage("css", css);
hljs.registerLanguage("diff", diff);
hljs.registerLanguage("go", go);
hljs.registerLanguage("java", java);
hljs.registerLanguage("javascript", javascript);
hljs.registerLanguage("json", json);
hljs.registerLanguage("lisp", lisp);
hljs.registerLanguage("markdown", markdown);
hljs.registerLanguage("python", python);
hljs.registerLanguage("ruby", ruby);
hljs.registerLanguage("rust", rust);
hljs.registerLanguage("shell", shell);
hljs.registerLanguage("sql", sql);
hljs.registerLanguage("typescript", typescript);
hljs.registerLanguage("xml", xml);
hljs.registerLanguage("yaml", yaml);

export function escapeHtml(s: string): string {
  return s
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

/** File extension (lowercased, no dot) → registered hljs language. */
const EXT_LANGUAGES: Record<string, string> = {
  bash: "bash",
  sh: "bash",
  zsh: "bash",
  c: "c",
  h: "c",
  cc: "cpp",
  cpp: "cpp",
  cxx: "cpp",
  hh: "cpp",
  hpp: "cpp",
  hxx: "cpp",
  css: "css",
  diff: "diff",
  patch: "diff",
  go: "go",
  java: "java",
  cjs: "javascript",
  js: "javascript",
  jsx: "javascript",
  mjs: "javascript",
  json: "json",
  el: "lisp",
  lisp: "lisp",
  markdown: "markdown",
  md: "markdown",
  py: "python",
  rb: "ruby",
  rs: "rust",
  sql: "sql",
  cts: "typescript",
  mts: "typescript",
  ts: "typescript",
  tsx: "typescript",
  htm: "xml",
  html: "xml",
  svg: "xml",
  xml: "xml",
  yaml: "yaml",
  yml: "yaml",
};

/**
 * Language for a file path's extension, or null when the extension is
 * unknown or absent. Explicit table (no hljs alias probing) so the
 * mapping stays deterministic.
 */
export function languageForPath(filePath: string): string | null {
  const base = filePath.slice(filePath.lastIndexOf("/") + 1);
  const dot = base.lastIndexOf(".");
  if (dot <= 0) return null; // extensionless (or a dotfile like .zshrc)
  return EXT_LANGUAGES[base.slice(dot + 1).toLowerCase()] ?? null;
}

/**
 * Escaped, highlighted HTML for CODE when LANGUAGE is registered
 * (aliases resolve through hljs), plain escaped text otherwise.
 * hljs.highlight escapes its input itself, so BOTH branches emit only
 * escaped text.
 */
export function highlightCode(code: string, language: string): string {
  const known = language !== "" && hljs.getLanguage(language) !== undefined;
  return known ? hljs.highlight(code, { language }).value : escapeHtml(code);
}
