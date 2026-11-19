# purescript-hylograph-d3-kernel

Force kernel for Hylograph built on `d3-force`'s force functions.

> **Deprecated, 2026-11-19.** Use
> [hylograph-simulation](https://github.com/afcondon/purescript-hylograph-simulation)
> 1.0 or later. Its default kernel,
> [hylograph-forces](https://github.com/afcondon/purescript-hylograph-forces),
> is these same seven forces in PureScript, held to d3-force 3.0.0 bit for
> bit, with six more that d3 does not have, and needs no npm packages.
> `hylograph-simulation-halogen` 0.6.0 and later no longer depends on this
> package. Version 0.1.0 stays on the registry and still works; there will
> be no further releases.
>
> Moving over: `Hylograph.Kernel.D3.Setup` is `Hylograph.ForceEngine.Setup`
> and `Hylograph.Kernel.D3.Simulation` is `Hylograph.ForceEngine.Simulation`,
> with the changes listed in hylograph-simulation's README (nodes without a
> position start on d3's spiral; a link to a missing node is an error;
> `velocityDecay` is `velocityRetention`). There is no simulation group
> there: step several `Hylograph.Simulation.Stepped` handles from one
> animation loop. Two faults in this package that the move also removes: a
> group never restarts once it has come to rest, and links set after a
> setup is applied are ignored.

## Overview

Low-level bindings to the force functions of `d3-force` (`forceManyBody`, `forceCollide`, `forceLink`, `forceCenter`, `forceX`, `forceY`, `forceRadial`), with `d3-selection` and `d3-drag` for dragging nodes. It does not use `d3.forceSimulation`: the tick loop, alpha decay and velocity integration are written here, so the caller owns the node data.

This was the kernel-per-package arrangement of code that `hylograph-simulation` carried internally until its 1.0, which has no D3 in it.

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
