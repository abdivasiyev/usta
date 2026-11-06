#!/usr/bin/env python3
"""Cuts GitHub's API descriptions down to what usta's writes use, for
test/Usta/Contract/WritesTests.hs. GitHub's whole REST description is
14 MB and its GraphQL schema 1.5 MB; these keep a few hundred KB.

  curl -sL -o /tmp/api.json https://raw.githubusercontent.com/github/rest-api-description/main/descriptions/api.github.com/api.github.com.json
  curl -sL -o /tmp/schema.graphql https://docs.github.com/public/fpt/schema.docs.graphql
  USTA_DUMP_WRITES=/tmp/writes.txt cabal test usta-test --test-options='-p Writes'
  python3 test/contract/trim-github.py /tmp/api.json /tmp/schema.graphql /tmp/writes.txt

The writes' list comes from a run with the whole descriptions in place.
"""
import json
import re
import sys

api_file, schema_file, writes_file = sys.argv[1:4]
api = json.load(open(api_file))


def fits(template, path):
    t = [s for s in template.split("/") if s]
    a = [s for s in path.split("/") if s]
    for i, s in enumerate(t):
        if i >= len(a):
            return False
        hole = s.startswith("{")
        if i == len(t) - 1 and hole and len(a) > len(t):
            return True
        if not hole and s != a[i]:
            return False
    return len(t) == len(a)


writes = set()
for line in open(writes_file):
    forge, method, path = line.split()
    if forge == "github" and path != "/graphql":
        writes.add((method.lower(), path))

paths = {}
for method, path in writes:
    found = [t for t in api["paths"] if fits(t, path) and method in api["paths"][t]]
    found.sort(key=lambda t: -len([s for s in t.split("/") if s and not s.startswith("{")]))
    t = found[0]
    item = paths.setdefault(t, {k: v for k, v in api["paths"][t].items() if k == "parameters"})
    item[method] = api["paths"][t][method]

# Every component the kept operations refer to, and theirs in turn.
components, todo = {}, [paths]
while todo:
    value = todo.pop()
    if isinstance(value, dict):
        ref = value.get("$ref")
        if isinstance(ref, str) and ref.startswith("#/components/"):
            _, _, kind, name = ref.split("/", 3)
            if name not in components.setdefault(kind, {}):
                components[kind][name] = api["components"][kind][name]
                todo.append(components[kind][name])
        todo.extend(value.values())
    elif isinstance(value, list):
        todo.extend(value)

# Answers are not checked: leave them out.
for item in paths.values():
    for method, op in item.items():
        if method != "parameters":
            op.pop("responses", None)
            op.pop("x-github", None)

json.dump({"openapi": api["openapi"], "info": api["info"], "paths": paths, "components": components},
          open("test/contract/github-openapi.json", "w"), indent=1, sort_keys=True)

# GraphQL: the mutations usta sends, their inputs, and the enums in them.
schema = open(schema_file).read()
source = open("src/Usta/Forge/GitHub.hs").read() + open("src/Usta/Forge/GitHub/Api.hs").read()
mutations = sorted(set(re.findall(r'"(\w+)" ""', source)) | set(re.findall(r'pullMutate repo n "(\w+)"', source))
                   | set(re.findall(r'\{ (\w+)\(input:', source)))
blocks = re.split(r"\n(?=(?:\"\"\"\n(?:.*\n)*?\"\"\"\n)?(?:type|input|enum|interface|union|scalar|directive) )", schema)


def block(kind, name):
    for b in blocks:
        if re.search(rf"^{kind} {name}\b", b, re.M):
            return b
    raise SystemExit(f"no {kind} {name}")


mutation_lines = block("type", "Mutation").split("\n")
kept_fields = []
for m in mutations:
    start = next((i for i, l in enumerate(mutation_lines) if l.startswith(f"  {m}(")), None)
    if start is None:
        raise SystemExit(f"no mutation {m}")
    end = next(i for i in range(start, len(mutation_lines)) if mutation_lines[i].startswith("  ): "))
    # Its description, the lines between the two '  """' just above it.
    first = start
    if start > 0 and mutation_lines[start - 1] == '  """':
        first = max(i for i in range(start - 1) if mutation_lines[i] == '  """')
    kept_fields.append("\n".join(mutation_lines[first:end + 1]) + "\n")
out = ["type Mutation {\n" + "\n".join(kept_fields) + "}\n"]
seen, todo = set(), [re.search(r"input: (\w+)!", f).group(1) for f in kept_fields]
while todo:
    name = todo.pop()
    if name in seen:
        continue
    seen.add(name)
    for kind in ("input", "enum"):
        try:
            b = block(kind, name)
        except SystemExit:
            continue
        out.append(b.strip() + "\n")
        if kind == "input":
            todo.extend(re.findall(r"^  \w+: \[?(\w+)", b, re.M))
open("test/contract/github-schema.graphql", "w").write("\n".join(out))
print(f"{len(paths)} paths, {sum(len(v) for v in components.values())} components, {len(mutations)} mutations: {', '.join(mutations)}")
