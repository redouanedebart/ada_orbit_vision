# Todo — Ada Orbit Vision

## Engine (`src/engine/`)

### `engine-sgp4.adb` — Propagate (Kepler solver)
- [ ] Implement Newton-Raphson iterative Kepler equation solver in `Propagate`
- [ ] Calculate true anomaly from eccentric anomaly
- [ ] Compute final position/velocity vectors from orbital elements
- [ ] Currently raises `Program_Error` at line ~209

### `engine-collision.adb` — Is_Collision
- [ ] Implement `Is_Collision` (delegate to `Evaluate(...).Is_Alert`)
- [ ] Currently raises `Program_Error` at line ~40

---

## Concurrency (`src/concurrency/`)

### `concurrency-ground_station.adb` — Get_Position
- [ ] Implement `Get_Position`: lookup by `Sat_Id` in map, set `Found` and `Snap`
- [ ] Currently raises `Program_Error` at line ~34

### `concurrency-satellite_manager.adb` — Snapshot builder
- [ ] Build `Satellite_Snapshot` record (Id, Name, Name_Len, State) from TLE + SGP4 result
- [ ] Feed snapshot to `Ground_Station.Station.Update_Position`
- [ ] Currently raises `Program_Error` at line ~56

---

## Vision (`src/vision/`) — Not started

### `vision.ads` — Root package
- [ ] Declare root package with shared image types (Pixel, Image_Buffer, dimensions)

### `vision-texture_extractor.ads/.adb` — Blue Marble loader
- [ ] Define `Texture` record (pixel buffer, width, height)
- [ ] `Load_Image(Path)` — load Blue Marble PNG/BMP via SDL2_image into memory buffer
- [ ] `Extract_Region(Texture, Lat, Lon, Size)` — extract sub-matrix by ground-track coordinates using direct memory access (no full-image copy)

### `vision-transformations.ads/.adb` — Projection math
- [ ] `Lat_Lon_To_Pixel(Lat, Lon, Img_W, Img_H)` — geographic to pixel mapping
- [ ] `Satellite_View_Transform(Sat_Pos, Ground_Point)` — spherical-to-planar projection matrix (Earth surface to satellite camera plane)
- [ ] Support equirectangular projection (Blue Marble native format)

### `vision-filters.ads/.adb` — Edge detection
- [ ] `Sobel(Src, Dst)` — Sobel gradient operator (3x3 convolution, Gx + Gy)
- [ ] `Canny(Src, Dst, Low, High)` — Canny edge detector (Gaussian blur, gradient, non-max suppression, hysteresis thresholding)
- [ ] Parallelize filter loops via Ada tasks or `pragma Parallel_Loops`

---

## UI (`src/ui/`) — Not started

### `ui.ads` — Root package
- [ ] Declare root package

### `ui-display.ads/.adb` — SDL2 window management
- [ ] `Initialize(Title, W, H)` — create SDL2 window + renderer
- [ ] `Poll_Events` / event loop — handle quit, keyboard, mouse
- [ ] `Shutdown` — destroy renderer/window, call `SDL_Quit`

### `ui-renderer.ads/.adb` — Drawing
- [ ] `Draw_Earth_Texture(Renderer, Texture)` — blit extracted Earth view
- [ ] `Draw_Satellite_Tracks(Renderer, Snapshots)` — plot satellite positions/trajectories
- [ ] `Draw_Collision_Alert(Renderer, Alert_Info)` — overlay collision warning
- [ ] `Present(Renderer)` — flip buffer

---

## Main (`src/ada_orbit_vision.adb`) — Stub

- [ ] Load TLE data from file(s) in `data/`
- [ ] Instantiate `Satellite_Tracker` tasks (one per satellite)
- [ ] Start `Alert_Monitor` task
- [ ] Initialize SDL2 display via `UI.Display`
- [ ] Run main event/render loop
- [ ] Graceful shutdown: stop all tasks, finalize logging, close SDL2

---

## Data & Assets

- [ ] Add sample TLE files to `data/` (ISS, Starlink, etc.)
- [ ] Add Blue Marble image to `assets/` (or document download instructions)
- [ ] Add `assets/icon.png` (256x256) for desktop entry
