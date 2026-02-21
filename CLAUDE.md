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

## Plan d'action — Interface interactive

### Phase 0 : Corrections prerequis (satellites invisibles)

Les satellites ne s'affichent pas actuellement. Deux bugs identifies :

**Bug 0.1 — TLE rejetés : retour chariot Windows (`\r`)**
- `data/gp.txt` a des fins de lignes `\r\n` (Windows). Sur Linux, `Get_Line` conserve le `\r`, donnant des lignes de 70 caractères au lieu de 69.
- Le parseur TLE exige `Line_1'Length = 69` → tous les TLE sont rejetés → 0 satellites chargés.
- **Correction** : dans `Load_TLEs` (`ada_orbit_vision.adb`), passer `Line_1 (1 .. 69)` et `Line_2 (1 .. 69)` au parseur (on sait déjà que `Len >= 69`). Idem pour `Line_0` : strip du `\r` via un helper `Strip_CR`.
- Fichiers touchés : `src/ada_orbit_vision.adb`

**Bug 0.2 — Max_Satellites = 20 en dur**
- Le tableau `Trackers` est fixé à 20 entrées. `gp.txt` contient ~14 000 satellites.
- **Correction** : remplacer le tableau fixe par un `Ada.Containers.Vectors` de `Tracker_Access`, sans limite arbitraire. Ou augmenter à un nombre raisonnable (ex: 200) pour un premier test.
- Fichiers touchés : `src/ada_orbit_vision.adb`

### Phase 1 : Bouton plein écran (toggle fullscreen)

**Objectif** : touche F11 ou bouton SDL pour basculer entre fenêtré et plein écran.

**Étapes** :
1. **`ui-display.ads/.adb`** — Ajouter `procedure Toggle_Fullscreen` qui appelle `SDL_SetWindowFullscreen` avec le flag `SDL_WINDOW_FULLSCREEN_DESKTOP`. Stocker l'état courant (`Is_Fullscreen : Boolean`). Après le toggle, re-lire les dimensions fenêtre via `SDL_GetWindowSize` et mettre à jour `Win_W`/`Win_H`.
2. **`ada_orbit_vision.adb`** — Dans la boucle d'événements, intercepter `Evt_Key_Down` avec scancode F11 (SDL_SCANCODE_F11 = 58). Appeler `UI.Display.Toggle_Fullscreen`.
3. **`ui-display.adb`** — Étendre `Poll_Event` pour remonter le scancode dans `UI_Event.Key` (actuellement toujours 0).

**Imports SDL2 nécessaires** :
- `SDL_SetWindowFullscreen (Window, Flags)` — pour le toggle
- `SDL_GetWindowSize (Window, W_ptr, H_ptr)` — pour récupérer les nouvelles dimensions

### Phase 2 : Panel latéral — liste des satellites

**Objectif** : un volet rétractable sur le côté droit listant les satellites trackés, avec nom, ID NORAD, et état.

**Contrainte SDL2** : SDL2 n'a pas de widgets natifs. Deux approches :
- **(A) Rendu SDL2 pur** — Dessiner le panel manuellement (rectangles, texte via SDL_ttf). Plus cohérent, pas de dépendance externe.
- **(B) Overlay texte simple** — Sans SDL_ttf, afficher chaque satellite comme un rectangle coloré + son ID NORAD à côté (pas de vrai texte, juste des indicateurs visuels).

**Approche retenue** : **(A) avec SDL_ttf** — Ajouter `libSDL2_ttf` aux dépendances pour le rendu texte. Essentiel pour un panel lisible.

**Étapes** :
1. **Dépendances** — Ajouter `-lSDL2_ttf` dans `ada_orbit_vision.gpr` (section Linker). Vérifier dans `install.sh` la présence de `libSDL2_ttf`. Embarquer une police monospace (ex: `assets/fonts/DejaVuSansMono.ttf`).
2. **`ui-display.ads/.adb`** — Initialiser SDL_ttf (`TTF_Init`) dans `Initialize`, `TTF_Quit` dans `Shutdown`. Charger la police et exposer un handle via `Get_Font`.
3. **`ui-renderer.ads/.adb`** — Nouveau module ou nouvelles procédures :
   - `Draw_Panel (Rend, Snapshots, W, H, Panel_Width, Selected_Id)` — Dessine un rectangle semi-transparent à droite, puis pour chaque satellite : rectangle coloré + nom (tronqué) + ID NORAD. Le satellite sélectionné est mis en surbrillance (fond plus clair ou bordure).
   - `Panel_Width` : constante (ex: 280 px) ou pourcentage de la largeur.
   - État du panel (ouvert/fermé) : variable module-level `Panel_Open : Boolean`, toggle via touche Tab ou bouton.
4. **`ada_orbit_vision.adb`** — Passer `Panel_Open` et `Selected_Sat_Id` au renderer. Ajuster la zone de carte : si panel ouvert, la map occupe `W - Panel_Width` pixels en largeur. Les projections `Lat_Lon_To_Pixel` doivent utiliser la largeur effective.

**Données affichées par satellite** :
- Nom (24 chars max, tronqué à ~20 pour le panel)
- ID NORAD (5 chiffres)
- Altitude approximative (calculable depuis `Norm(Position) - Earth_Radius_Km`)
- Indicateur de couleur (même couleur que le point sur la carte)

### Phase 3 : Sélection et surbrillance d'un satellite

**Objectif** : cliquer sur un satellite (sur la carte ou dans le panel) pour le sélectionner. Le satellite sélectionné est mis en surbrillance partout.

**Étapes** :
1. **État de sélection** — Variable `Selected_Sat : Sat_Id` (0 = aucun) dans le module principal ou dans un état UI dédié.
2. **Clic sur la carte** — Dans `Poll_Event`, remonter les coordonnées souris (`Mouse_X`, `Mouse_Y`) pour `Evt_Mouse_Click` (déjà prévu dans `UI_Event` mais non rempli). Dans la boucle principale, pour chaque satellite visible, tester si le clic est dans un rayon de ~10 px du point satellite. Si oui, `Selected_Sat := Snap.Id`.
3. **Clic dans le panel** — Calculer quel satellite est sous le curseur en fonction de `Mouse_Y` et de la position de scroll du panel. Mapper `Y → index → Sat_Id`.
4. **Rendu surbrillance carte** — Dans `Draw_Satellites`, si `Snap.Id = Selected_Sat` : dessiner un cercle plus grand (12px au lieu de 6), avec une bordure blanche pulsante (varier l'alpha avec le temps pour un effet de pulsation). Afficher le nom au-dessus du point.
5. **Rendu surbrillance panel** — Dans `Draw_Panel`, le satellite sélectionné a un fond distinct (ex: bleu foncé au lieu de gris).

### Phase 4 : Zone de vision satellite (footprint sur la carte)

**Objectif** : quand un satellite est sélectionné, afficher sur la carte la zone qu'il « voit » (son empreinte au sol / footprint).

**Calcul du footprint** :
- L'angle de vue dépend de l'altitude : `half_angle = arccos(R_earth / (R_earth + altitude))`
- Le footprint au sol est un cercle centré sur le nadir (ground track point) de rayon angulaire `half_angle`.
- Sur la projection équirectangulaire, ce cercle devient une ellipse déformée (surtout aux hautes latitudes).

**Étapes** :
1. **`engine-collision.ads/.adb`** ou nouveau **`engine-footprint.ads/.adb`** — Fonction `Compute_Footprint_Radius (Altitude_Km : Long_Float) return Long_Float` retournant le rayon angulaire en radians. Fonction `Footprint_Polygon (Lat, Lon, Radius : Long_Float; N_Points : Positive) return array of (Lat, Lon)` retournant N points sur le contour du footprint (cercle en coordonnées sphériques → polygone lat/lon).
2. **`ui-renderer.adb`** — Nouvelle procédure `Draw_Footprint (Rend, Lat, Lon, Radius, W, H, Color)`. Convertit les points du polygone en pixels via `Lat_Lon_To_Pixel`, puis dessine le contour avec `SDL_RenderDrawLine` (polygone fermé) et un remplissage semi-transparent.
3. **`ada_orbit_vision.adb`** — Si `Selected_Sat /= 0`, calculer le footprint et appeler `Draw_Footprint` entre `Draw_Earth_Background` et `Draw_Satellites` (pour que les points satellites soient par-dessus).
4. **Rendu** : contour blanc ou jaune (2px), remplissage semi-transparent (rgba 255, 255, 100, 40). Le footprint doit gérer le wrapping aux bords de la carte (longitude ±180°).

### Ordre d'implémentation recommandé

```
Phase 0 (bugs)  →  Phase 1 (fullscreen)  →  Phase 2 (panel)  →  Phase 3 (sélection)  →  Phase 4 (footprint)
   ↑ bloquant         indépendant              dépend de         dépend de 2               dépend de 3
   pour tout                                    SDL_ttf
```

Phase 0 est un prérequis absolu — sans satellites visibles, rien d'autre n'a de sens.
Phases 1 et 2 sont indépendantes et peuvent être développées en parallèle.
Phases 3 et 4 sont séquentielles (la sélection doit exister avant le footprint).

### État d'avancement

| Phase | Statut | Notes |
|-------|--------|-------|
| 0 — Corrections TLE | **FAIT** | `Strip_CR` + `Max_Satellites=200` + vecteur dynamique |
| 1 — Fullscreen (F11) | **FAIT** | `Toggle_Maximize` via `SDL_MaximizeWindow`/`SDL_RestoreWindow`, scancode remonté dans `Poll_Event` |
| 2 — Panel latéral | **FAIT** | SDL_ttf intégré (`-lSDL2_ttf`), police DejaVu Sans Mono embarquée (`assets/fonts/`), `Draw_Panel` avec liste satellites (nom, NORAD ID, altitude, indicateur couleur), toggle via Tab, zone carte ajustée (`W - Panel_Width`) |
| 3 — Sélection satellite | **FAIT** | Clic carte + panel, `Find_Sat_At_Map_Click`, `Find_Sat_At_Panel_Click`, surbrillance `Draw_Satellites` / `Draw_Panel` |
| 4 — Footprint | **FAIT** | `Engine.Footprint` (Half_Angle_Rad, Compute_Polygon), `Draw_Footprint` (fan fill + contour, gestion antimeridien) |

**Fichiers modifiés en Phases 3 & 4** :
- `src/engine/engine-footprint.ads/.adb` — nouveau module : `Half_Angle_Rad`, `Compute_Polygon` (sphérique)
- `src/ui/ui-renderer.ads` — `Draw_Satellites` (+`Selected_Sat`, +`Font`), `Draw_Panel` (+`Selected_Sat`), `Draw_Footprint`, `Find_Sat_At_Map_Click`, `Find_Sat_At_Panel_Click`, import `SDL_RenderDrawRect`
- `src/ui/ui-renderer.adb` — `with Engine.Footprint`, implémentations des 5 procédures/fonctions, palette `Sat_Colours` factorisée, couleurs texte constantes
- `src/ada_orbit_vision.adb` — `Selected_Sat`, `Pending_Click*`, gestion `Evt_Mouse_Click`, appel `Draw_Footprint`, `Station.Get_Position`, paramètres `Selected_Sat`/`Font` aux appels renderer

**Fichiers modifiés en Phase 2** :
- `ada_orbit_vision.gpr` — ajout `-lSDL2_ttf` aux switches linker
- `install.sh` — ajout check `libSDL2_ttf`
- `assets/fonts/DejaVuSansMono.ttf` — police monospace embarquée (licence Bitstream Vera)
- `src/ui/ui-display.ads` — `Get_Font`, `SDL_SCANCODE_TAB`, imports TTF (TTF_Init, TTF_Quit, TTF_OpenFont, TTF_CloseFont)
- `src/ui/ui-display.adb` — init TTF + chargement police dans `Initialize`, cleanup dans `Shutdown`, accesseur `Get_Font`
- `src/ui/ui-renderer.ads` — `Draw_Panel` (Rend, Font, Snapshots, W, H, Panel_Width)
- `src/ui/ui-renderer.adb` — `Render_Text` helper (TTF_RenderUTF8_Blended + SDL texture), `Draw_Panel` (fond semi-transparent, bordure, titre, liste scrollable, compteur), imports SDL_SetRenderDrawBlendMode / SDL_SetTextureBlendMode
- `src/ada_orbit_vision.adb` — `Panel_Open` + Tab toggle, `Map_W` (largeur effective carte), appel `Draw_Panel`
