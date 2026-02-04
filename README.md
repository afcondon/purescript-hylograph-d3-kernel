# purescript-hylograph-d3-kernel

D3.js force simulation kernel for Hylograph.

## Overview

Low-level bindings to D3.js force simulation. This package provides the D3 physics engine implementation that powers `hylograph-simulation`. Most users should use `hylograph-simulation` directly rather than this package.

## Installation

```bash
spago install hylograph-d3-kernel
```

## Modules

- `Hylograph.Kernel.D3.Core` - Core D3 force simulation FFI
- `Hylograph.Kernel.D3.Setup` - Force configuration
- `Hylograph.Kernel.D3.Simulation` - Simulation control
- `Hylograph.Kernel.D3.Events` - Event handling
- `Hylograph.Kernel.D3.Links` - Link force support
- `Hylograph.Kernel.D3.Types` - Type definitions
- `Hylograph.Kernel.D3.Adapter` - Kernel adapter interface
- `Hylograph.Kernel.D3.SimulationGroup` - Grouped simulations

## Part of Hylograph

- **hylograph-d3-kernel** - D3 physics kernel (this package)
- **hylograph-wasm-kernel** - WASM physics kernel (faster alternative)
- **hylograph-simulation** - High-level simulation API
- **hylograph-simulation-core** - Kernel-agnostic types

## License

MIT
