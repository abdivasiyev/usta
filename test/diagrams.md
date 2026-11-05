# Mermaid examples

One diagram of every type usta draws, for the tests: each reads and lays
out, and its layout is kept under test/golden/diagrams.

## architecture

```mermaid
architecture-beta
  group app(cloud)[usta]
  service db(database)[Cache] in app
  service sync(server)[Sync] in app
  db:L -- R:sync
```

## block

```mermaid
block-beta
  columns 3
  a["Forge"] b["Sync"] c["Cache"]
  a --> b
  b --> c
```

## c4

```mermaid
C4Context
  title usta
  Person(dev, "Developer")
  System(usta, "usta", "Desktop client")
  System_Ext(gh, "GitHub")
  Rel(dev, usta, "Uses")
  Rel(usta, gh, "Syncs", "HTTPS")
```

## class

```mermaid
classDiagram
  class Forge {
    +String host
    +pullRequests() List
  }
  class GitHub
  Forge <|-- GitHub
  Forge "1" --> "*" Repository : has
```

## entity

```mermaid
erDiagram
  ACCOUNT ||--o{ REPOSITORY : syncs
  REPOSITORY ||--|{ PULL_REQUEST : has
  PULL_REQUEST {
    int number
    string title
  }
```

## flowchart

```mermaid
---
title: Review flow
---
flowchart LR
  A[Open pull request] --> B{Checks pass?}
  B -- yes --> C([Review])
  B -. no .-> D[(Fix it)]
  D ==> B
  subgraph Merge
    C --> E{{Merge}}
  end
```

## gantt

```mermaid
gantt
  title Beta
  dateFormat YYYY-MM-DD
  section Build
  Library split :a1, 2026-11-05, 1d
  Tests         :after a1, 3d
  section Ship
  Release       :2026-11-10, 1d
```

## gitgraph

```mermaid
gitGraph
  commit
  branch tests
  checkout tests
  commit
  commit
  checkout main
  merge tests
  commit
```

## journey

```mermaid
journey
  title Reviewing a pull request
  section Inbox
    Open usta: 5: Me
    Find the request: 4: Me
  section Review
    Read the diff: 3: Me, Reviewer
```

## kanban

```mermaid
kanban
  todo[To do]
    t1[GitLab]
  doing[Doing]
    t2[Tests]
  done[Done]
    t3[Beta]
```

## mindmap

```mermaid
mindmap
  root((usta))
    Inbox
      Notifications
    Pull requests
      Diffs
      Checks
```

## packet

```mermaid
packet-beta
  0-15: "Source port"
  16-31: "Destination port"
  32-63: "Sequence number"
```

## pie

```mermaid
pie title Languages
  "Haskell" : 92
  "C" : 6
  "Nix" : 2
```

## quadrant

```mermaid
quadrantChart
  title Effort and value
  x-axis Low effort --> High effort
  y-axis Low value --> High value
  quadrant-1 Plan
  quadrant-2 Do now
  quadrant-3 Skip
  quadrant-4 Maybe
  Tests: [0.3, 0.9]
  GitLab: [0.8, 0.6]
```

## radar

```mermaid
radar-beta
  title Skills
  axis a["Speed"], b["Keys"], c["Offline"]
  curve u["usta"]{4, 5, 5}
  curve w["web"]{3, 2, 1}
  max 5
```

## requirement

```mermaid
requirementDiagram
  requirement offline {
    id: 1
    text: works offline
    risk: medium
    verifymethod: test
  }
  element cache {
    type: sqlite
  }
  cache - satisfies -> offline
```

## sankey

```mermaid
sankey-beta
Inbox,Reviewed,20
Inbox,Done,10
Reviewed,Merged,15
```

## sequence

```mermaid
sequenceDiagram
  participant U as usta
  participant F as Forge
  U->>F: GET /notifications
  F-->>U: 304 Not Modified
  Note over U,F: nothing changed
  loop every minute
    U->>F: sync
  end
```

## state

```mermaid
stateDiagram-v2
  [*] --> Open
  Open --> Draft : convert
  Draft --> Open : ready
  Open --> Merged
  Open --> Closed
  Merged --> [*]
```

## timeline

```mermaid
timeline
  title usta
  2026-10 : Scaffold
  2026-11 : Beta : Tests
```

## treemap

```mermaid
treemap-beta
"src"
  "UI": 120
  "Forge": 40
"test"
  "unit": 30
```

## xychart

```mermaid
xychart-beta
  title "Pull requests"
  x-axis [jan, feb, mar, apr]
  y-axis "Count" 0 --> 50
  bar [10, 20, 35, 42]
  line [8, 18, 30, 40]
```

## zenuml

```mermaid
zenuml
  Alice->Bob: Hello
  Bob->Alice: Hi
```
