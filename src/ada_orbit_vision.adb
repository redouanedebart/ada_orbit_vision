--  Ada_Orbit_Vision -- Point d'entree principal
--
--  Sequence de demarrage :
--  1. Initialiser le logger
--  2. Charger les TLE depuis data/sample.tle
--  3. Creer la fenetre SDL2
--  4. Lancer une tache Satellite_Tracker par satellite
--  5. Lancer la tache Alert_Monitor
--  6. Boucle principale : events -> snapshot -> rendu
--  7. Arret propre de toutes les taches
--  8. Fermeture SDL2 et logger

with System;
with Ada.Text_IO;
with Ada.Directories;
with Common.Logging;
with Common.Physics;
with Engine.TLE_Parser;
with Engine.SGP4;
with Engine.Collision;
with Concurrency.Ground_Station;
with Concurrency.Satellite_Manager;
with Concurrency.Alert_System;
with UI.Display;
with UI.Renderer;
with Vision.Transformations;
with Vision.Texture_Extractor;
with Vision.Filters;
pragma Unreferenced (Engine.SGP4);
pragma Unreferenced (Engine.Collision);
pragma Unreferenced (Vision.Texture_Extractor);
pragma Unreferenced (Vision.Filters);
pragma Unreferenced (Vision.Transformations);

procedure Ada_Orbit_Vision is

   --  Chemins des fichiers de donnees
   TLE_Path     : constant String := "data/gp.txt";
   Texture_Path : constant String := "assets/blue_marble.jpg";

   --  Parametres de rafraichissement
   Refresh_Interval : aliased Duration := 2.0;
   Check_Interval   : aliased Duration := 3.0;
   Threshold        : aliased Common.Physics.Coordinate :=
     Common.Physics.Default_Collision_Threshold_Km;

   --  Nombre maximum de satellites supportes
   Max_Satellites : constant := 20;

   --  Tableau de taches tracker
   Trackers : array (1 .. Max_Satellites)
     of Concurrency.Satellite_Manager.Tracker_Access;
   Num_Trackers : Natural := 0;

   --  Moniteur d'alertes
   Monitor : Concurrency.Alert_System.Monitor_Access;

   -------------------------------------------------------
   --  Load_TLEs : lit le fichier TLE et lance les taches
   -------------------------------------------------------

   procedure Load_TLEs is
      use Ada.Text_IO;
      File   : File_Type;
      Line_0 : String (1 .. 200);
      Line_1 : String (1 .. 200);
      Line_2 : String (1 .. 200);
      Len_0  : Natural;
      Len_1  : Natural;
      Len_2  : Natural;
   begin
      if not Ada.Directories.Exists (TLE_Path) then
         Common.Logging.Log_Warning
           ("Main",
            "Fichier TLE absent : " & TLE_Path);
         return;
      end if;

      Open (File, In_File, TLE_Path);

      while not End_Of_File (File) loop
         --  Lire le bloc de 3 lignes
         Get_Line (File, Line_0, Len_0);

         --  Ignorer les lignes vides et commentaires
         if Len_0 = 0
           or else Line_0 (1) = '#'
         then
            goto Continue;
         end if;

         if End_Of_File (File) then
            exit;
         end if;
         Get_Line (File, Line_1, Len_1);

         if End_Of_File (File) then
            exit;
         end if;
         Get_Line (File, Line_2, Len_2);

         --  Verifier que Line_1 commence par '1'
         --  et Line_2 par '2'
         if Len_1 >= 69
           and then Len_2 >= 69
           and then Line_1 (1) = '1'
           and then Line_2 (1) = '2'
         then
            declare
               TLE : Engine.TLE_Parser.TLE_Record;
            begin
               TLE := Engine.TLE_Parser.Parse
                 (Line_0 (1 .. Len_0),
                  Line_1 (1 .. Len_1),
                  Line_2 (1 .. Len_2));

               if Num_Trackers < Max_Satellites then
                  Num_Trackers := Num_Trackers + 1;
                  Trackers (Num_Trackers) := new
                    Concurrency.Satellite_Manager
                      .Satellite_Tracker
                        (Refresh_Interval'Unchecked_Access);
                  Trackers (Num_Trackers).Start (TLE);

                  Common.Logging.Log_Info
                    ("Main",
                     "Tracker lance pour NORAD"
                     & TLE.Catalog_Id'Image);
               end if;
            exception
               when Engine.TLE_Parser.TLE_Format_Error =>
                  Common.Logging.Log_Warning
                    ("Main",
                     "TLE invalide, ignore : "
                     & Line_0 (1 .. Len_0));
            end;
         end if;

         <<Continue>>
      end loop;

      Close (File);

      Common.Logging.Log_Info
        ("Main",
         "Charge"
         & Num_Trackers'Image
         & " satellite(s)");
   end Load_TLEs;

   -------------------------------------------------------
   --  Boucle principale
   -------------------------------------------------------

   Has_Alert : Boolean := False;

begin
   --  1. Logger
   Common.Logging.Initialize;
   Common.Logging.Log_Info
     ("Main", "Ada Orbit Vision demarre");

   --  2. Charger les TLE
   Load_TLEs;

   --  3. Fenetre SDL2
   UI.Display.Initialize
     ("Ada Orbit Vision", 1024, 768);

   --  3b. Texture terrestre (fond de carte)
   UI.Renderer.Load_Earth_Texture
     (UI.Display.Get_Renderer, Texture_Path);

   --  4. Alert Monitor
   Monitor := new
     Concurrency.Alert_System.Alert_Monitor
       (Check_Interval'Unchecked_Access,
        Threshold'Unchecked_Access);
   Monitor.Start;

   --  5. Boucle principale
   Main_Loop :
   loop
      --  Evenements SDL2
      declare
         Evt : constant UI.Display.UI_Event :=
           UI.Display.Poll_Event;
      begin
         case Evt.Kind is
            when UI.Display.Evt_Quit =>
               exit Main_Loop;
            when others =>
               null;
         end case;
      end;

      --  Lire le snapshot complet
      declare
         use Concurrency.Ground_Station;
         Sats : constant Snapshot_Maps.Map :=
           Station.Get_All;
         W : constant Positive :=
           UI.Display.Get_Width;
         H : constant Positive :=
           UI.Display.Get_Height;
         Rend : constant System.Address :=
           UI.Display.Get_Renderer;
      begin
         --  Fond bleu fonce (fallback si pas de texture)
         UI.Renderer.Clear
           (Rend,
            (R => 10, G => 15, B => 40, A => 255));

         --  Texture terrestre en fond de carte
         UI.Renderer.Draw_Earth_Background (Rend);

         --  Dessiner les satellites
         UI.Renderer.Draw_Satellites
           (Rend, Sats, W, H);

         --  Alerte si collision detectee
         Has_Alert := False;
         declare
            use Snapshot_Maps;
            use Common.Physics;
            C1 : Cursor := Sats.First;
            C2 : Cursor;
         begin
            while C1 /= No_Element
              and then not Has_Alert
            loop
               C2 := Next (C1);
               while C2 /= No_Element
                 and then not Has_Alert
               loop
                  if Distance
                    (Element (C1).State.Position,
                     Element (C2).State.Position)
                    < Threshold
                  then
                     Has_Alert := True;
                  end if;
                  C2 := Next (C2);
               end loop;
               C1 := Next (C1);
            end loop;
         end;

         if Has_Alert then
            UI.Renderer.Draw_Alert_Overlay (Rend, W);
         end if;

         --  Presenter
         UI.Renderer.Present (Rend);
      end;

      --  Limiter la boucle (~30 fps via vsync)
      delay 0.016;
   end loop Main_Loop;

   --  6. Arret des taches
   Common.Logging.Log_Info
     ("Main", "Arret en cours...");

   for I in 1 .. Num_Trackers loop
      Trackers (I).Stop;
   end loop;

   Monitor.Stop;

   --  7. Fermeture SDL2
   UI.Renderer.Destroy_Earth_Texture;
   UI.Display.Shutdown;

   --  8. Logger
   Common.Logging.Log_Info
     ("Main", "Ada Orbit Vision arrete");
   Common.Logging.Finalize;
end Ada_Orbit_Vision;
