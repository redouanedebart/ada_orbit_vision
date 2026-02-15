--  Ada_Orbit_Vision — Point d'entree principal (stub)
with Common.Logging;
with Engine.TLE_Parser;
pragma Unreferenced (Engine.TLE_Parser);

procedure Ada_Orbit_Vision is
begin
   Common.Logging.Initialize;
   Common.Logging.Log_Info ("Main", "Ada Orbit Vision demarre");
   Common.Logging.Finalize;
end Ada_Orbit_Vision;
