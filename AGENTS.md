# Merl - Erlang Web Framework

## Cursor Cloud specific instructions

### Erlang Version Requirement

This codebase uses Erlang's experimental **packages** feature (dotted module names like `merl.router`, `merl.state_hash`) and the old-style `-import(Module).` syntax, both of which were removed in Erlang R16. The project requires **Erlang/OTP R14B04** or earlier, provided via a Docker image.

### Running Commands

All Erlang commands must be run inside the `erlang-r14` Docker container:

```bash
sudo docker run --rm -v /workspace:/workspace -v /espec:/espec -w /workspace erlang-r14 <command>
```

### Build

```bash
sudo docker run --rm -v /workspace:/workspace -v /espec:/espec -w /workspace erlang-r14 bash -c "./scripts/clean && erl -make all"
```

### Run Tests

```bash
sudo docker run --rm -v /workspace:/workspace -v /espec:/espec -w /workspace erlang-r14 erl -noshell -s espec_suite main -s init stop
```

### Known Issue: `lists:flat_length/1`

The router module calls `lists:flat_length/1` which was removed in R14 (renamed to `lists:flatlength/1`). This causes 16 of 36 specs to fail at runtime. The state_hash and controller specs (8/8) all pass. This is a pre-existing code issue, not an environment problem.

### espec Testing Framework

The project depends on a custom espec BDD framework located at `/espec` (mounted as a sibling directory). The espec implementation provides `?describe`, `?before`, `?bdd` macros and an `espec_suite` runner. It is NOT the `lucaspiller/espec` or `antonmi/espec` projects.

### Docker Daemon

Docker must be running (`sudo dockerd` in background). The `erlang-r14` image is built from `ubuntu:12.04` with Erlang R14B04 from the Ubuntu Precise repository.
