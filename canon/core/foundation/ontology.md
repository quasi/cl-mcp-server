# Domain Ontology

Entity relationships in the CL-MCP-Server domain.

---

## Entity Relationship Overview

```dot
digraph CL_MCP_Server {
    rankdir=TB;
    node [shape=box];

    subgraph cluster_protocol {
        label="MCP Protocol Layer";
        MCPServer [label="MCP Server"];
        Transport;
        Tool;
    }

    subgraph cluster_evaluation {
        label="Evaluation Layer";
        Session;
        Evaluation;
        Package;
    }

    subgraph cluster_results {
        label="Result Layer";
        Result;
        Condition;
        Output;
    }

    MCPServer -> Transport [label="uses"];
    MCPServer -> Tool [label="exposes"];
    MCPServer -> Session [label="maintains"];

    Session -> Package [label="current"];
    Session -> Evaluation [label="contains"];

    Evaluation -> Result [label="produces"];
    Evaluation -> Condition [label="may signal"];
    Evaluation -> Output [label="captures"];

    Tool -> Evaluation [label="triggers"];
}
```

## Communication Flow

```dot
digraph Communication {
    rankdir=LR;
    node [shape=box];

    Claude [shape=ellipse];
    stdin [shape=parallelogram];
    MCPServer [label="MCP Server"];
    Evaluator [label="Code Evaluator"];
    stdout [shape=parallelogram];

    Claude -> stdin [label="JSON-RPC request"];
    stdin -> MCPServer [label="parse"];
    MCPServer -> Evaluator [label="evaluate"];
    Evaluator -> MCPServer [label="result/condition"];
    MCPServer -> stdout [label="JSON-RPC response"];
    stdout -> Claude [label="return"];
}
```

## Cardinality Summary

| Entity A | Relationship | Entity B | Cardinality |
|----------|--------------|----------|-------------|
| MCP Server | maintains | Session | 1:1 |
| MCP Server | uses | Transport | 1:1 |
| MCP Server | exposes | Tool | 1:N |
| Session | contains | Evaluation | 1:N |
| Session | has current | Package | 1:1 |
| Evaluation | produces | Result | 1:1 |
| Evaluation | may signal | Condition | 1:0..N |
| Evaluation | captures | Output | 1:1 |
