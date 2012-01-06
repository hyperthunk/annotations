# TODO List

## Functionality

- implement `around` advice (added Nov/Dec 2011)
- tighten up the behaviour support
- provide an API for instrumenting code at runtime

## Refactoring

- remove duplication in `annotations_pt`
- a lot of code in `annotations_pt` moves data around (e.g., between tuple elements) unnecessarily
- avoid making multiple passes through the AST during `parse_transform/2`
