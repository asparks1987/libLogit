# Burndown Plan

This document outlines the planned burndown from the current prototype to the first multi-language release of libLogit.

## Assumptions
- Six 1-week iterations (Sprint 0 through Sprint 5) to reach the v1 milestone.
- Total scope sized at 60 story points (SP) based on the prioritized backlog.
- Velocity expectation: 10 SP/week once the team is at speed (after Sprint 0 ramp-up).
- Dependencies with external teams (e.g., infrastructure) may shift remote transport work by up to one sprint.

## Sprint Milestones
| Sprint | Focus | Exit Criteria |
|--------|-------|----------------|
| Sprint 0 | Foundation & Planning | P0 issues ready-to-start, config schema draft reviewed, scaffolding stories estimated |
| Sprint 1 | Schema & Package Restructure | Config schema validated, Python package reorganized, smoke tests passing |
| Sprint 2 | Console/File Sinks | Config-driven console/file sinks demonstrated, documentation updated |
| Sprint 3 | Streaming API & Remote Interface | Python streaming API prototype merged, remote transport interface ADR accepted |
| Sprint 4 | Remote Transport + Hardening | HTTP stub implemented, retry/buffering decisions documented, integration tests added |
| Sprint 5 | Polish & Release Prep | CONTRIBUTING in place, language expansion ADR approved, release notes + packaging dry run |

## Planned Burndown
| Week (Sprint) | Planned Remaining SP | Notes |
|---------------|----------------------|-------|
| 0 | 60 | Baseline; backlog refined |
| 1 | 48 | Schema + package restructure complete |
| 2 | 36 | Console/file sinks integrated |
| 3 | 24 | Streaming API + remote interface delivered |
| 4 | 12 | Remote transport + hardening |
| 5 | 0 | Release candidate ready |

## Target Burndown Chart
    SP
    60 |##############################
    54 |##########################
    48 |######################
    42 |####################
    36 |##################
    30 |###############
    24 |###########
    18 |########
    12 |#####
     6 |###
     0 +------------------------------> Week
        0    1    2    3    4    5

## Monitoring Guidance
- Track actual completed SP each sprint and plot alongside the target line to spot scope creep or velocity dips early.
- When actual burndown stays above target for 2 consecutive sprints, trigger a scope triage (defer P1/P2 items or add capacity).
- Capture learnings in sprint retrospectives and update this plan if new work emerges (e.g., additional language bindings).
