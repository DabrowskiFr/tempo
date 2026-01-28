# Section Outline for the Tempo Technical Report

This file is the working checklist for drafting the report. Each section has objective, questions, sources, expected artifacts, and confidence level for claims.

## Abstract
- Objective: summarize tempo, its purpose, and main outcomes in 5–8 lines, with lineage.
- Questions: what is tempo? how does it relate to reactive control? what is the key result?
- Sources: README, lib/tempo.ml, tests.
- Artifacts: none.
- Paper style: include contribution-style phrasing and one positioning sentence.
- Confidence: to verify.

## Introduction
- Objective: position tempo in reactive control lineage and state contributions.
- Questions: how does tempo build on Esterel, FairThreads, ReactiveML? what is in/out of scope?
- Sources: README, design notes if any, lib/tempo.ml, external references.
- Artifacts: 3–5 bullet contributions, short lineage paragraph.
- Paper style: end with clear contributions list.
- Confidence: to verify.

## Background: Reactive Control and Boussinot
- Objective: explain the reactive control approach and Boussinot's model.
- Questions: what is reactive control? how do FairThreads structure concurrency?
- Sources: external references, README if it cites them.
- Artifacts: concise summary of the control model.
- Paper style: cite original sources and define terms before use.
- Confidence: to verify.

## ReactiveML and Synchronous Reactive Context
- Objective: connect ReactiveML to the synchronous reactive tradition and FRP.
- Questions: what is synchronous reactive programming? how does FRP differ?
- Sources: external references.
- Artifacts: comparison bullets or table.
- Paper style: avoid survey tone; focus on what's needed for tempo.
- Confidence: to verify.

## Core Constructs and Properties
- Objective: present the common constructs and synchronous properties.
- Questions: what are signals, emissions, suspension, guards, preemption? what is determinism here?
- Sources: external references, lib/tempo.ml for mapping.
- Artifacts: glossary table and property list.
- Paper style: include formal definitions if used later.
- Confidence: to verify.

## Algebraic Effects in OCaml
- Objective: present algebraic effects and handlers as background.
- Questions: how do effects capture continuations? why OCaml 5 matters?
- Sources: external references (effects/handlers, Eff, Multicore OCaml).
- Artifacts: short conceptual summary with citations.
- Paper style: keep it concise and relevant to tempo.
- Confidence: to verify.

## Tempo Overview
- Objective: describe tempo's goals and positioning relative to Boussinot/ReactiveML.
- Questions: what is new due to OCaml 5.3 effects? what remains similar?
- Sources: lib/tempo.ml, README.
- Artifacts: summary bullets of design choices.
- Paper style: state contributions and novelty explicitly.
- Confidence: to verify.

## Public API
- Objective: detail types, functions, and invariants.
- Questions: what are the main entry points? expected behaviors? errors?
- Sources: lib/tempo.ml, interface files if any.
- Artifacts: API summary table (type, role, notes).
- Paper style: align API narrative with the formal model.
- Confidence: to verify.

## Usage and Examples
- Objective: show minimal and advanced usage patterns.
- Questions: what is the smallest useful example? what is a real scenario?
- Sources: README, tests, examples if any.
- Artifacts: 1 minimal and 1 advanced example.
- Paper style: keep examples short; move extended listings to appendix.
- Confidence: to verify.

## Implementation with Algebraic Effects (OCaml 5.3)
- Objective: explain the effect-based scheduler and execution model.
- Questions: which effects are used? how are preemption and suspension encoded?
- Sources: lib/tempo.ml, related modules in lib/.
- Artifacts: control-flow diagram or pseudo-code.
- Paper style: separate model from implementation details.
- Confidence: to verify.

## Performance
- Objective: report complexity and measured performance.
- Questions: time/space complexity? what benchmarks exist?
- Sources: tests, benchmarks (if any), code analysis.
- Artifacts: benchmark table with setup and results.
- Paper style: document measurement setup and environment.
- Confidence: to verify.

## Limitations and Trade-offs
- Objective: list known limitations and rationale.
- Questions: edge cases? design trade-offs? future risks?
- Sources: README, code comments, issues if available.
- Artifacts: explicit list of limitations with rationale.
- Paper style: keep limitations concrete and evidence-based.
- Confidence: to verify.

## Related Work
- Objective: compare with Esterel, FairThreads, ReactiveML, and FRP.
- Questions: what is reused or inspired? what differs due to effects?
- Sources: external references to cite.
- Artifacts: comparison table with 3–4 items.
- Paper style: focus on differences relevant to contributions.
- Confidence: to verify.

## Conclusion
- Objective: recap findings and propose next steps.
- Questions: what was delivered? what remains to improve?
- Sources: synthesized from all sections.
- Artifacts: short bullet list of next steps.
- Paper style: restate contributions and future research directions.
- Confidence: to verify.

## Appendix
- Objective: hold extended details or data.
- Questions: what content is too detailed for main sections?
- Sources: raw data, extended examples, tables.
- Artifacts: extended tables, longer code listings.
- Paper style: keep appendices referenced from main text.
- Confidence: to verify.
