# Writing Guide for the Tempo Technical Report

Purpose: define how to write the report so it stays consistent, technical, verifiable, and suitable as a draft for scientific papers.

## Goals
- Position tempo in the lineage of reactive control languages.
- Explain how tempo revisits Boussinot/ReactiveML with OCaml 5.3 algebraic effects.
- Make the design decisions explicit and justified.
- Provide accurate usage guidance and limitations.

## Audience
- OCaml developers and engineers who need timing or scheduling primitives.
- Readers who want design trade-offs and formal properties, not marketing.
- Researchers familiar with synchronous reactive and FRP.

## Style
- Use short, precise sentences and formal academic tone.
- Prefer facts over opinions; justify choices with constraints.
- Keep code snippets minimal and focused; place longer snippets in the appendix.
- Use present tense for current behavior; past tense for experiments.
- Avoid marketing language; describe behavior and trade-offs.
- Use citations for lineage and claims that rely on prior work.

## Scientific-paper readiness
- Favor clear contributions and problem statements.
- Define terms before use and keep notation consistent.
- Separate claims, evidence, and interpretation.
- Prefer neutral voice; avoid informal phrasing.

## Structure and formatting
- Follow acmart defaults; keep sections concise and scannable.
- Use figures and tables only when they add clarity (e.g., module map).
- Use inline references to files and modules when describing behavior.

## Content checklist per section
- Introduction: lineage (Esterel, FairThreads, ReactiveML), scope, and contributions.
- Background: reactive control approach, Boussinot model, and synchronous reactive notions.
- Reactive Constructs: signals, emission, suspension, guards, preemption, determinism.
- Tempo Overview: goals, positioning, and design summary.
- Public API: types, invariants, and main functions with behavior notes.
- Usage and Examples: minimal example, then a more advanced scenario.
- Implementation: OCaml 5.3 algebraic effects, scheduling model, key mechanisms.
- Performance: time and space complexity, plus benchmark setup.
- Limitations: known issues, edge cases, and future work.
- Related Work: compare to Esterel, FairThreads, ReactiveML, and FRP.
- Conclusion: summary and next steps.

## Data and evidence
- Prefer measured numbers over estimates.
- If a claim is unverified, label it clearly as a hypothesis.
- Tag claims with one of: Verified, Assumption, To Measure.

## Source references
- Reference files in the repo (README, lib code, tests).
- Cite external papers or libraries only when relevant.
- Keep citations minimal and connected to concrete claims.
## External references to include
- Esterel: synchronous reactive control lineage.
- FairThreads (Boussinot): cooperative concurrency model.
- ReactiveML: reactive control and synchronization in ML.
- FRP: position tempo relative to dataflow reactive paradigms.

## Terminology
- Define core terms once (e.g., clock, timer, scheduler) and reuse them.
- Keep naming consistent with the codebase.

## Practical workflow
- Fill sections in order; keep TODO markers until content is verified.
- After each section, add a short validation note if applicable.
- Keep a running list of open questions for follow-up.
