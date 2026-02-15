--  Engine.TLE_Parser -- Implementation du parseur TLE NORAD

with Ada.Calendar.Arithmetic;
with Ada.Numerics;
with Ada.Strings;
with Ada.Strings.Fixed;
with Common.Logging;

package body Engine.TLE_Parser is

   package SF renames Ada.Strings.Fixed;

   ----------------------------------------------------------
   --  Utilitaires internes
   ----------------------------------------------------------

   function Trim (S : String) return String is
   begin
      return SF.Trim (S, Ada.Strings.Both);
   end Trim;

   ----------------------------------------------------------
   --  To_Classification
   ----------------------------------------------------------

   function To_Classification
     (C : Character) return Classification
   is
   begin
      case C is
         when 'U'    => return Unclassified;
         when 'C'    => return Classified;
         when 'S'    => return Secret;
         when others =>
            Common.Logging.Log_Warning
              ("TLE_Parser",
               "Classification inconnue '"
               & C & "', defaut : Unclassified");
            return Unclassified;
      end case;
   end To_Classification;

   ----------------------------------------------------------
   --  Parse_Implicit_Decimal
   ----------------------------------------------------------
   --  Decode le format exponentiel implicite des TLE.
   --
   --  Exemples :
   --    " 12345-6"  =>  0.12345 * 10^(-6)
   --    "-12345-6"  => -0.12345 * 10^(-6)
   --    " 00000+0"  =>  0.0
   --
   --  Structure (8 caracteres) :
   --    Col 1     : signe mantisse (' ' ou '-')
   --    Col 2..6  : chiffres mantisse (point implicite)
   --    Col 7     : signe exposant ('+' ou '-')
   --    Col 8     : chiffre exposant

   function Parse_Implicit_Decimal
     (S : String) return Long_Float
   is
      Trimmed  : constant String := Trim (S);
      Sign     : Long_Float := 1.0;
      Mantissa : Long_Float;
      Exp_Sign : Long_Float := 1.0;
      Exp_Val  : Natural;
      Idx      : Positive;
   begin
      if Trimmed'Length = 0 then
         return 0.0;
      end if;

      Idx := Trimmed'First;

      --  Signe de la mantisse
      if Trimmed (Idx) = '-' then
         Sign := -1.0;
         Idx := Idx + 1;
      elsif Trimmed (Idx) = '+' then
         Idx := Idx + 1;
      end if;

      --  Position du signe d'exposant
      declare
         Exp_Pos  : Natural := 0;
         Mant_Str : constant String :=
           "0." & Trimmed (Idx .. Trimmed'Last);
      begin
         for I in Idx + 1 .. Trimmed'Last loop
            if Trimmed (I) = '+'
              or else Trimmed (I) = '-'
            then
               Exp_Pos := I;
               exit;
            end if;
         end loop;

         if Exp_Pos = 0 then
            Mantissa := Long_Float'Value (Mant_Str);
            return Sign * Mantissa;
         end if;

         --  Mantisse : "0." & chiffres
         Mantissa := Long_Float'Value
           ("0." & Trimmed (Idx .. Exp_Pos - 1));

         --  Exposant
         if Trimmed (Exp_Pos) = '-' then
            Exp_Sign := -1.0;
         end if;

         Exp_Val := Natural'Value
           (Trimmed (Exp_Pos + 1 .. Trimmed'Last));

         declare
            Exp_Int : constant Integer :=
              Integer (Exp_Sign * Long_Float (Exp_Val));
         begin
            return Sign * Mantissa * (10.0 ** Exp_Int);
         end;
      end;
   end Parse_Implicit_Decimal;

   ----------------------------------------------------------
   --  Parse_Epoch
   ----------------------------------------------------------
   --  Convertit l'epoque TLE en Ada.Calendar.Time.
   --
   --  Convention NORAD :
   --    - Annee 2 chiffres :
   --        0..56  => 2000..2056
   --        57..99 => 1957..1999
   --    - Jour fractionnaire :
   --        1.0 = 1er janvier 00:00:00 UTC
   --
   --  Annee 2D -> 4 chiffres, puis Jan_1 + jours + secondes.

   function Parse_Epoch
     (Year_2D  : Natural;
      Day_Frac : Long_Float) return Ada.Calendar.Time
   is
      use Ada.Calendar;
      use Ada.Calendar.Arithmetic;

      Year        : Year_Number;
      Jan_1       : Time;
      Whole_Days  : Integer;
      Frac        : Long_Float;
      Day_Seconds : Duration;
   begin
      if Year_2D <= 56 then
         Year := Year_Number (2000 + Year_2D);
      else
         Year := Year_Number (1900 + Year_2D);
      end if;

      Jan_1 := Time_Of (Year, 1, 1, 0.0);
      Whole_Days :=
        Integer (Long_Float'Floor (Day_Frac)) - 1;
      Frac :=
        Day_Frac - Long_Float (Whole_Days + 1);
      Day_Seconds := Duration (Frac * 86_400.0);

      return Jan_1
        + Day_Count (Whole_Days)
        + Day_Seconds;
   end Parse_Epoch;

   ----------------------------------------------------------
   --  Valid_Checksum
   ----------------------------------------------------------
   --  Verifie le checksum modulo 10 d'une ligne TLE
   --  (69 caracteres).
   --
   --  Algorithme :
   --    - Parcourir caracteres 1 a 68
   --    - Chiffre ('0'..'9') : ajouter sa valeur
   --    - '-' : ajouter 1
   --    - Autres : ajouter 0
   --    - Checksum attendu = caractere en position 69
   --    - Valide si (somme mod 10) = checksum
   --
   --  Somme modulo 10 des 68 premiers caracteres.

   function Valid_Checksum (Line : String) return Boolean
   is
      Sum      : Natural := 0;
      Expected : Natural;
      C        : Character;
   begin
      if Line'Length < 2 then
         return False;
      end if;

      for I in Line'First .. Line'Last - 1 loop
         C := Line (I);
         if C in '0' .. '9' then
            Sum := Sum
              + Character'Pos (C)
              - Character'Pos ('0');
         elsif C = '-' then
            Sum := Sum + 1;
         end if;
      end loop;

      --  Dernier caractere = checksum attendu
      C := Line (Line'Last);
      if C not in '0' .. '9' then
         return False;
      end if;

      Expected :=
        Character'Pos (C) - Character'Pos ('0');

      return (Sum mod 10) = Expected;
   end Valid_Checksum;

   ----------------------------------------------------------
   --  Parse -- Point d'entree principal
   ----------------------------------------------------------

   function Parse
     (Line_0 : String;
      Line_1 : String;
      Line_2 : String) return TLE_Record
   is
      use Common.Physics;

      Result : TLE_Record;

      Deg_To_Rad : constant Long_Float :=
        Ada.Numerics.Pi / 180.0;
   begin
      --  Validations de format
      if Line_1'Length /= 69 then
         raise TLE_Format_Error
           with "Ligne 1 : longueur"
                & Line_1'Length'Image
                & " au lieu de 69";
      end if;

      if Line_2'Length /= 69 then
         raise TLE_Format_Error
           with "Ligne 2 : longueur"
                & Line_2'Length'Image
                & " au lieu de 69";
      end if;

      if Line_1 (Line_1'First) /= '1' then
         raise TLE_Format_Error
           with "Ligne 1 doit commencer par '1'";
      end if;

      if Line_2 (Line_2'First) /= '2' then
         raise TLE_Format_Error
           with "Ligne 2 doit commencer par '2'";
      end if;

      --  Verification des checksums
      if not Valid_Checksum (Line_1) then
         raise TLE_Format_Error
           with "Checksum invalide sur ligne 1";
      end if;

      if not Valid_Checksum (Line_2) then
         raise TLE_Format_Error
           with "Checksum invalide sur ligne 2";
      end if;

      --  Ligne 0 -- Nom du satellite
      if Line_0'Length > 0 then
         declare
            Trimmed_Name : constant String :=
              Trim (Line_0);
            Len : constant Natural :=
              Natural'Min (Trimmed_Name'Length, 24);
         begin
            Result.Name (1 .. Len) :=
              Trimmed_Name (Trimmed_Name'First
                .. Trimmed_Name'First + Len - 1);
            Result.Name_Length := Len;
         end;
      end if;

      --  Ligne 1 -- Identification et epoque
      --  Positions (indice 1-based) :
      --    3..7   : Numero de catalogue
      --    8      : Classification
      --    10..17 : Designation internationale
      --    19..20 : Annee d'epoque (2 chiffres)
      --    21..32 : Jour fractionnaire d'epoque
      --    34..43 : Derivee 1ere mouvement moyen / 2
      --    45..52 : Derivee 2nde (notation implicite)
      --    54..61 : BStar (notation implicite)
      --    65..68 : Numero du jeu d'elements

      declare
         L : constant String (1 .. 69) := Line_1;
      begin
         Result.Catalog_Id :=
           Catalog_Number'Value (Trim (L (3 .. 7)));
         Result.Object_Class :=
           To_Classification (L (8));
         Result.Intl_Designator := L (10 .. 17);

         --  Epoque
         declare
            Epoch_Year : constant Natural :=
              Natural'Value (Trim (L (19 .. 20)));
            Epoch_Day : constant Long_Float :=
              Long_Float'Value (Trim (L (21 .. 32)));
         begin
            Result.Epoch :=
              Parse_Epoch (Epoch_Year, Epoch_Day);
         end;

         --  Derivee premiere du mouvement moyen
         Result.Mean_Motion_Dot :=
           Long_Float'Value (Trim (L (34 .. 43)));

         --  Derivee seconde (exponentiel implicite)
         Result.Mean_Motion_DDot :=
           Parse_Implicit_Decimal (L (45 .. 52));

         --  BStar (exponentiel implicite)
         Result.BStar :=
           Parse_Implicit_Decimal (L (54 .. 61));

         --  Numero du jeu d'elements
         Result.Element_Set_Number :=
           Natural'Value (Trim (L (65 .. 68)));
      end;

      --  Ligne 2 -- Elements kepleriens
      --  Positions (1-based) :
      --    9..16  : Inclinaison (degres)
      --    18..25 : RAAN (degres)
      --    27..33 : Excentricite (point implicite)
      --    35..42 : Argument du perigee (degres)
      --    44..51 : Anomalie moyenne (degres)
      --    53..63 : Mouvement moyen (rev/jour)
      --    64..68 : Numero de revolution

      declare
         L : constant String (1 .. 69) := Line_2;

         Inc_Deg  : Long_Float;
         Raan_Deg : Long_Float;
         Ecc      : Long_Float;
         Aop_Deg  : Long_Float;
         Ma_Deg   : Long_Float;
         Mm       : Long_Float;
      begin
         Inc_Deg :=
           Long_Float'Value (Trim (L (9 .. 16)));
         Raan_Deg :=
           Long_Float'Value (Trim (L (18 .. 25)));

         --  Excentricite : "0001756" => 0.0001756
         Ecc :=
           Long_Float'Value ("0." & L (27 .. 33));

         Aop_Deg :=
           Long_Float'Value (Trim (L (35 .. 42)));
         Ma_Deg :=
           Long_Float'Value (Trim (L (44 .. 51)));
         Mm :=
           Long_Float'Value (Trim (L (53 .. 63)));

         --  Conversion fixed-point + radians
         Result.Elements := Keplerian_Elements'
           (Inclination    =>
              Angle (Inc_Deg * Deg_To_Rad),
            RAAN           =>
              Angle (Raan_Deg * Deg_To_Rad),
            Eccentricity   =>
              Coordinate (Ecc),
            Arg_Of_Perigee =>
              Angle (Aop_Deg * Deg_To_Rad),
            Mean_Anomaly   =>
              Angle (Ma_Deg * Deg_To_Rad),
            Mean_Motion    =>
              Coordinate (Mm));

         --  Numero de revolution
         if L (64 .. 68) /= "     " then
            Result.Rev_Number :=
              Natural'Value (Trim (L (64 .. 68)));
         end if;
      end;

      Common.Logging.Log_Info
        ("TLE_Parser",
         "TLE parse : "
         & Trim (Result.Name
             (1 .. Result.Name_Length))
         & " (NORAD"
         & Result.Catalog_Id'Image & ")");

      return Result;
   end Parse;

end Engine.TLE_Parser;
