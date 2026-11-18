# purescript-hylograph-d3-kernel

Force kernel for Hylograph built on `d3-force`'s force functions.

## Overview

Low-level bindings to the force functions of `d3-force` (`forceManyBody`, `forceCollide`, `forceLink`, `forceCenter`, `forceX`, `forceY`, `forceRadial`), with `d3-selection` and `d3-drag` for dragging nodes. It does not use `d3.forceSimulation`: the tick loop, alpha decay and velocity integration are written here, so the caller owns the node data.

This is the kernel-per-package arrangement of the same code that `hylograph-simulation` carries internally (`Hylograph.ForceEngine.*`); `hylograph-simulation` does not depend on this package. `hylograph-simulation-halogen` and several demos do. For a new project, start from `hylograph-simulation`.

The PureScript registry cannot declare npm dependencies, so a project that bundles this library has to install them itself:

```bash
npm install d3-force d3-selection d3-drag
```

## Installation

```bash
spago install hylograph-d3-kernel
```

## Modules

- `Hylograph.Kernel.D3.Core` - FFI to the `d3-force` force functions
- `Hylograph.Kernel.D3.Setup` - Force configuration
- `Hylograph.Kernel.D3.Simulation` - Simulation control
- `Hylograph.Kernel.D3.Events` - Event handling
- `Hylograph.Kernel.D3.Links` - Link force support
- `Hylograph.Kernel.D3.Types` - Type definitions
- `Hylograph.Kernel.D3.Adapter` - Kernel adapter interface
- `Hylograph.Kernel.D3.SimulationGroup` - Grouped simulations

## Part of Hylograph

- **hylograph-d3-kernel** - D3 physics kernel (this package)
- **hylograph-wasm-kernel** - Rust/WASM physics kernel (alternative with no D3 force code)
- **hylograph-simulation** - High-level simulation API (carries its own copy of these bindings)
- **hylograph-simulation-core** - Kernel-agnostic types

## License

MIT
