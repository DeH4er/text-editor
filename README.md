# text-editor


### Goals:
 * Provide a modal editor for programmers
 * Multicursors are citizens of the editor. The use shouldn't be as complicated as in Kakoune
 * Universal command palette (like intelij shift + shift)
 * Modifiers are evil. F-X keys are evil.  Editor should work with only english alphabet + digits in normal mode


### Non-Goals:
 * Haskell implementation of vim
 * Haskell implementation of kakoune
 * Haskell implementation of IDE
 * IDE


## Better movement/editing [v0.0.1]
 * Move by a word forward/backward (vim: w, b)
 * Move by half a screen (vim: ctrl d, ctrl u)
 * Delete a word (vim: dw, db)
 * Delete a line (vim: dd)
 * Creation/removing of cursors


## Ready for basic editing [Done]
 * Save to a particular file from editor
 * Open particular file in editor
 * UI follows a cursor
 * Tests


## Proof of concept [Done]
 * Open file
 * Edit content
 * Save file
 * Close without saving
