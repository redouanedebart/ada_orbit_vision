--  UI.Display -- Implementation

with Common.Logging;

package body UI.Display is

   use type System.Address;

   --  Etat interne du module
   Win_Ptr  : System.Address := System.Null_Address;
   Rend_Ptr : System.Address := System.Null_Address;
   Win_W    : Positive := 1024;
   Win_H    : Positive := 768;

   --  Constantes SDL2
   SDL_WINDOWPOS_CENTERED : constant := 16#2FFF0000#;
   SDL_WINDOW_SHOWN       : constant := 16#00000004#;
   SDL_RENDERER_ACCELERATED : constant := 16#00000002#;
   SDL_RENDERER_PRESENTVSYNC : constant := 16#00000004#;

   --  Buffer brut pour SDL_Event (56 octets suffisent)
   --  Le type d'evenement est un Uint32 au debut
   type Uint32 is mod 2 ** 32
     with Size => 32;

   type Event_Buffer is record
      Evt_Type : Uint32 := 0;
      Padding  : String (1 .. 60) := (others => ' ');
   end record
     with Convention => C, Size => 512;

   --  Codes d'evenements SDL2
   SDL_QUIT_EVENT            : constant Uint32 := 16#100#;
   SDL_KEYDOWN_EVENT         : constant Uint32 := 16#300#;
   SDL_MOUSEBUTTONDOWN_EVENT : constant Uint32 := 16#401#;

   ----------------------------------------------------------
   --  Initialize
   ----------------------------------------------------------

   procedure Initialize
     (Title  : String;
      Width  : Positive := 1024;
      Height : Positive := 768)
   is
      use Interfaces.C;
      Ret : int;
   begin
      Win_W := Width;
      Win_H := Height;

      Ret := SDL_Init (SDL_INIT_VIDEO);
      if Ret /= 0 then
         Common.Logging.Log_Error
           ("Display",
            "Echec SDL_Init :" & int'Image (Ret));
         return;
      end if;

      Win_Ptr := SDL_CreateWindow
        (To_C (Title),
         int (SDL_WINDOWPOS_CENTERED),
         int (SDL_WINDOWPOS_CENTERED),
         int (Width),
         int (Height),
         SDL_WINDOW_SHOWN);

      if Win_Ptr = System.Null_Address then
         Common.Logging.Log_Error
           ("Display", "Echec SDL_CreateWindow");
         return;
      end if;

      Rend_Ptr := SDL_CreateRenderer
        (Win_Ptr,
         -1,
         SDL_RENDERER_ACCELERATED
           or SDL_RENDERER_PRESENTVSYNC);

      if Rend_Ptr = System.Null_Address then
         Common.Logging.Log_Error
           ("Display", "Echec SDL_CreateRenderer");
         return;
      end if;

      Common.Logging.Log_Info
        ("Display",
         "Fenetre creee :"
         & Width'Image & "x" & Height'Image);
   end Initialize;

   ----------------------------------------------------------
   --  Poll_Event
   ----------------------------------------------------------

   function Poll_Event return UI_Event
   is
      use Interfaces.C;
      Buf    : Event_Buffer;
      Ret    : int;
      Result : UI_Event :=
        (Kind => Evt_None, others => <>);
   begin
      Ret := SDL_PollEvent (Buf'Address);
      if Ret = 0 then
         return Result;
      end if;

      if Buf.Evt_Type = SDL_QUIT_EVENT then
         Result.Kind := Evt_Quit;
      elsif Buf.Evt_Type = SDL_KEYDOWN_EVENT then
         Result.Kind := Evt_Key_Down;
      elsif Buf.Evt_Type = SDL_MOUSEBUTTONDOWN_EVENT
      then
         Result.Kind := Evt_Mouse_Click;
      end if;

      return Result;
   end Poll_Event;

   ----------------------------------------------------------
   --  Shutdown
   ----------------------------------------------------------

   procedure Shutdown is
   begin
      if Rend_Ptr /= System.Null_Address then
         SDL_DestroyRenderer (Rend_Ptr);
         Rend_Ptr := System.Null_Address;
      end if;

      if Win_Ptr /= System.Null_Address then
         SDL_DestroyWindow (Win_Ptr);
         Win_Ptr := System.Null_Address;
      end if;

      SDL_Quit_C;

      Common.Logging.Log_Info
        ("Display", "SDL2 ferme proprement");
   end Shutdown;

   ----------------------------------------------------------
   --  Accesseurs
   ----------------------------------------------------------

   function Get_Renderer return System.Address is
   begin
      return Rend_Ptr;
   end Get_Renderer;

   function Get_Width return Positive is
   begin
      return Win_W;
   end Get_Width;

   function Get_Height return Positive is
   begin
      return Win_H;
   end Get_Height;

end UI.Display;
