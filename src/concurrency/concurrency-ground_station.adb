--  Concurrency.Ground_Station -- Implementation

with Common.Logging;

package body Concurrency.Ground_Station is

   protected body Station is

      -------------------------------------------------------
      --  Update_Position
      -------------------------------------------------------

      procedure Update_Position
        (Snap : Satellite_Snapshot)
      is
         use Snapshot_Maps;
         C : constant Cursor := Data.Find (Snap.Id);
      begin
         if C = No_Element then
            Data.Insert (Snap.Id, Snap);
            Common.Logging.Log_Debug
              ("Ground_Station",
               "Nouveau satellite enregistre :"
               & Snap.Id'Image);
         else
            Data.Replace_Element (C, Snap);
         end if;
      end Update_Position;

      -------------------------------------------------------
      --  Get_Position
      -------------------------------------------------------
      --  ================================================
      --  =  TODO :                                      =
      --  =                                               =
      --  =  Chercher Id dans Data via Data.Find (Id).   =
      --  =  Si le curseur /= No_Element :               =
      --  =    Found := True;                            =
      --  =    Snap  := Element (curseur);               =
      --  =  Sinon :                                     =
      --  =    Found := False;                           =
      --  =                                               =
      --  =  Types utiles :                              =
      --  =    Snapshot_Maps.Cursor                      =
      --  =    Snapshot_Maps.No_Element                  =
      --  =    Snapshot_Maps.Element (C)                 =
      --  ================================================

      procedure Get_Position
        (Id    : Sat_Id;
         Snap  : out Satellite_Snapshot;
         Found : out Boolean)
      is
         pragma Unreferenced (Id);
      begin
         Found := False;
         Snap := Satellite_Snapshot'
           (Id       => 1,
            Name     => (others => ' '),
            Name_Len => 0,
            State    => <>);
         raise Program_Error
           with "Get_Position non implemente";
      end Get_Position;

      -------------------------------------------------------
      --  Get_All
      -------------------------------------------------------

      function Get_All return Snapshot_Maps.Map is
      begin
         return Data;
      end Get_All;

      -------------------------------------------------------
      --  Count
      -------------------------------------------------------

      function Count return Natural is
      begin
         return Natural (Data.Length);
      end Count;

      -------------------------------------------------------
      --  Clear
      -------------------------------------------------------

      procedure Clear is
      begin
         Data.Clear;
         Common.Logging.Log_Info
           ("Ground_Station",
            "Depot vide");
      end Clear;

   end Station;

end Concurrency.Ground_Station;
