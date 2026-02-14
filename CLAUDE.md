# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Ada-Orbit-Vision is a geospatial data processing and physics simulation application written in Ada. It combines orbital mechanics (SGP4 satellite propagation), real-time image processing (Earth texture extraction from satellite viewpoints), and collision detection — all leveraging Ada's native task-based parallelism. Documentation is in French.

## Build Commands

Ada project using **Alire** (package manager) and **GNAT** (compiler).

```bash
alr build                    # Build the project
alr run                      # Build and run
alr clean                    # Clean build artifacts
gnatmake -O3 -gnatn ...     # Release build with full optimization (for benchmarks)
```

Build output: executables in `bin/`, objects in `obj/<profile>/`.

## Architecture

Three parallel subsystems communicate through a central protected object:

```
[TLE Input] → Satellite_Tracker tasks (1 per satellite) → Ground_Station (protected object)
                                                                  ↓
[Blue Marble Image] → Texture_Extractor → Transformations → Filtered View → UI
                                                                  ↑
                                          Alert_System task ← Ground_Station (collision check)
```

### Concurrency Model (core design principle)

This project is designed around **massive Ada task parallelism** — not a single main loop:

- **`task type Satellite_Tracker`** — One task instance per tracked satellite. Each independently runs SGP4 propagation and deposits position vectors into the shared Ground_Station.
- **`protected type Ground_Station`** — Central thread-safe data store. Tracker tasks write positions; the UI and Alert_System read them. No manual locking — Ada's protected object semantics prevent race conditions and deadlocks.
- **`Alert_System` task** — High-priority dedicated task that monitors Ground_Station for collision proximity (Euclidean distance in 3D below threshold).
- Image filtering (Sobel/Canny) should be parallelized via dedicated tasks or `pragma Parallel_Loops` where supported.

### Precision Strategy

Use Ada **fixed-point types** (not floating-point) for orbital coordinates to avoid drift in long simulations:
```ada
type Coordinate is delta 10.0**(-6) range -20_000_000.0 .. 20_000_000.0;
```

## Source Layout

```
src/
├── common/         -- Shared types, physical constants, fixed-point definitions, logging
├── engine/         -- Orbital mechanics: SGP4 propagation, collision detection
├── vision/         -- Image loading, perspective transforms, edge filters (Sobel/Canny)
├── concurrency/    -- Task types, protected objects, alert system
└── ui/             -- Native SDL2 display layer
```

- `ada_orbit_vision.gpr` — GNAT project file
- `alire.toml` — Alire manifest and dependencies

## Source Modules (by layer)

### 1. Common (`src/common/`)
- **`common-physics.ads`** — Fixed-point coordinate types, vector records (position + velocity), physical constants (Earth radius, GM, J2)
- **`common-logging.ads/.adb`** — Centralized task-safe logging system. Uses a protected object so any task can log without race conditions. Supports four severity levels (Debug, Info, Warning, Error) and outputs timestamped messages to a log file and/or standard error.

### 2. Engine (`src/engine/`)
- **`engine-sgp4.ads/.adb`** — SGP4 orbital propagation. Input: TLE (Two-Line Element set). Output: state vector (X, Y, Z, Xdot, Ydot, Zdot) at a given epoch.
- **`engine-tle_parser.ads/.adb`** — Parses NORAD two-line element sets from fixed-width text format into Ada records.
- **`engine-collision.ads/.adb`** — 3D Euclidean distance between satellite pairs; threshold-based alert trigger.

### 3. Vision (`src/vision/`)
- **`vision-texture_extractor.ads/.adb`** — Loads Blue Marble image, extracts sub-matrices via direct memory access based on satellite ground-track coordinates.
- **`vision-transformations.ads/.adb`** — Spherical-to-planar projection matrices (Earth surface → satellite camera view).
- **`vision-filters.ads/.adb`** — Parallelized Sobel/Canny edge detection for coastline and structure recognition on extracted textures.

### 4. Concurrency (`src/concurrency/`)
- **`concurrency-satellite_manager.ads/.adb`** — Declares `task type Satellite_Tracker`; each instance wraps one satellite's SGP4 loop.
- **`concurrency-ground_station.ads/.adb`** — `protected type Ground_Station` with entries for writing positions and reading snapshots.
- **`concurrency-alert_system.ads/.adb`** — Dedicated high-priority task consuming Ground_Station data for collision detection.

### 5. UI (`src/ui/`)
- **`ui-display.ads/.adb`** — SDL2 window management: initialization, event loop, shutdown.
- **`ui-renderer.ads/.adb`** — Draws satellite trajectories, Earth texture views, collision alert overlays onto the SDL2 surface.

## Dependencies (Alire)

- **SDLAda** — Ada bindings to SDL2 for native window rendering
- Standard GNAT libraries: `Ada.Containers`, `Ada.Real_Time`, `Ada.Numerics`
- System libraries (linked via GPR): `libSDL2`, `libSDL2_image`

## Style & Compiler Conventions

Follows the same strict rules as the sibling `adsb_track` project:
- **3-space indentation** (`-gnaty3`)
- All warnings enabled (`-gnatwa`)
- All validity checks enabled (`-gnatVa`)
- UTF-8 encoding (`-gnatW8`)
- Enforced casing rules for keywords, attributes, pragmas, and identifiers
- No trailing blanks, no tabs
- Symbolic traceback enabled via binder switch `-Es`

## Logging

The project uses a centralized logging system (`Common.Logging`) that all subsystems must use for diagnostic output:

- **Task-safe** — The logger uses an Ada protected object internally; any task can call `Log` without synchronization concerns
- **Severity levels** — `Debug`, `Info`, `Warning`, `Error`. The minimum displayed level is configurable at initialization
- **Timestamped** — Every log line is prefixed with an `Ada.Calendar` timestamp and the emitting module name
- **Dual output** — Logs write to a file (`logs/ada_orbit_vision.log` by default) and optionally to `Standard_Error` for console visibility
- **No `Ada.Text_IO.Put_Line` for diagnostics** — All runtime diagnostic output must go through `Common.Logging.Log`, never through direct `Put_Line` calls, to guarantee task-safety and consistent formatting
- **Log file directory** — The `logs/` directory is created automatically at initialization if it does not exist

## Key Constraints

- TLE data format is the standard NORAD two-line element set — parsing must handle the fixed-width column format exactly
- SGP4 accuracy depends on epoch freshness; stale TLEs degrade prediction quality
- Blue Marble source images can be very large (21600x10800 px) — texture extraction must use direct memory access, not full-image copies
- Collision detection runs at higher priority than rendering to ensure timely alerts
- Fixed-point arithmetic is mandatory for orbital state vectors to avoid floating-point drift across long propagation intervals

## Installation & Desktop Launch

The app is fully native (SDL2, no web component). An install script handles everything:

```bash
./install.sh
```

What it does:
1. Checks system dependencies (`alr`, `gnat`, `libSDL2`, `libSDL2_image`)
2. Builds the project in release mode (`alr build --release`)
3. Copies the binary to `~/.local/bin/ada_orbit_vision`
4. Copies `assets/` and `data/` to `~/.local/share/ada_orbit_vision/`
5. Creates a `.desktop` entry in `~/.local/share/applications/` so the app appears in the system application menu and can be launched by double-clicking

The `.desktop` file expects an icon at `assets/icon.png` — provide a 256x256 PNG.

Requires `~/.local/bin` to be in `$PATH` (standard on most Linux desktops).
