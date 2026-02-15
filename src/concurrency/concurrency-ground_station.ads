--  Concurrency.Ground_Station -- Depot de donnees thread-safe
--
--  Objet protege central : les taches Satellite_Tracker y
--  deposent les positions, l'Alert_System et le renderer
--  y lisent des snapshots. Pas de verrou manuel : la
--  semantique Ada des protected objects garantit l'absence
--  de race conditions.

with Ada.Containers.Ordered_Maps;
with Common.Physics;
with Engine.TLE_Parser;

package Concurrency.Ground_Station is

   --  Identifiant = numero de catalogue NORAD
   subtype Sat_Id is Engine.TLE_Parser.Catalog_Number;

   --  Snapshot d'un satellite a un instant donne
   type Satellite_Snapshot is record
      Id       : Sat_Id := 1;
      Name     : String (1 .. 24) := (others => ' ');
      Name_Len : Natural := 0;
      State    : Common.Physics.State_Vector;
   end record;

   --  Map ordonnee : Id -> Snapshot
   package Snapshot_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Sat_Id,
      Element_Type => Satellite_Snapshot);

   --  Objet protege principal
   protected Station is

      --  Deposer ou mettre a jour la position d'un sat
      procedure Update_Position
        (Snap : Satellite_Snapshot);

      --  Lire la position d'un satellite particulier.
      --  Found = False si le sat n'est pas dans le depot.
      procedure Get_Position
        (Id    : Sat_Id;
         Snap  : out Satellite_Snapshot;
         Found : out Boolean);

      --  Obtenir un snapshot complet de tous les sats
      function Get_All return Snapshot_Maps.Map;

      --  Nombre de satellites actuellement suivis
      function Count return Natural;

      --  Vider le depot
      procedure Clear;

   private
      Data : Snapshot_Maps.Map;
   end Station;

end Concurrency.Ground_Station;
