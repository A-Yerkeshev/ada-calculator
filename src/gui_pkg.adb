with Gtk.Main;
with Gtk; use Gtk;
with Gtk.Window; use Gtk.Window;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Handlers; use Gtk.Handlers;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Grid; use Gtk.Grid;
with Gtk.Button; use Gtk.Button;
with Glib; use Glib;
with Gtk.GEntry; use Gtk.GEntry;
with Gtk.Box; use Gtk.Box;
with Gtk.Label; use Gtk.Label;
with Ada.Text_IO; use Ada.Text_IO;

package body gui_pkg with SPARK_Mode is
   type Matrix is array (Gint range <>, Gint range <>) of Character;

   Window : Gtk_Window;
   Window_Height : constant Gint := 300;
   Window_Width : constant Gint := 300;
   Buttons_Grid : Gtk_Grid;
   VBox : Gtk_Box;
   Input_Field : Gtk.GEntry.Gtk_Entry;
   Padding : constant Integer := 5;
   Result_Label : Gtk_Label;
   Max_Result_Length : constant Integer := 20;
   Button_Labels : constant Matrix := (('1', '2', '3', '+', '-'),
                                       ('4', '5', '6', '*', '/'),
                                       ('7', '8', '9', 'C', '<'),
                                       ('.', '0', '=', ' ', ' ')
                                      );

   --  Event listener for closing window
   package Close_Handler is new Gtk.Handlers.Callback
     (Widget_Type => Gtk_Widget_Record);

   procedure Close_Window (Widget : access Gtk_Widget_Record'Class)
     with Global => null
   is
      pragma Unreferenced (Widget);
   begin
      Gtk.Main.Main_Quit;
   end Close_Window;

   --  Event listener for button clicks
   package Button_Click_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Button_Record, Character);

   procedure Handle_Button_Click
     (Button_Access : access Gtk_Button_Record'Class; Char : Character)
     with Global => null,
     Pre => (for some I in Button_Labels'Range (1) =>
               (for some J in Button_Labels'Range (2) =>
                    Char = Button_Labels (I, J)))
   is
      pragma Unreferenced (Button_Access);
      Current_Input : constant String := Input_Field.Get_Text;
   begin
      case Char is
      when '=' =>
         Call_Calculate (Current_Input);
      when 'C' =>
         Input_Field.Set_Text ("");
      when '<' =>
         Input_Field.Set_Text
           (Current_Input (Current_Input'First .. Current_Input'Last - 1));
      when '.' =>
         --  Don't allow more than one dot in a single number
         declare
            Num_Start : Positive := Current_Input'First;
         begin
            for I in reverse Current_Input'Range loop
               if Current_Input (I) in '+' | '-' | '*' | '/' then
                  Num_Start := I + 1;
               end if;
            end loop;

            for C of Current_Input (Num_Start .. Current_Input'Last) loop
               if C = '.' then
                  return;
               end if;
            end loop;

            Input_Field.Set_Text (Current_Input & Char);
         end;
      when others =>
         Input_Field.Set_Text (Current_Input & Char);
      end case;
   end Handle_Button_Click;

   --  Result formatter
   --  (AI-generated)
   function Format_Result (Result : Float) return String is
      package FIO is new Float_IO (Float);

      Buffer : String (1 .. Max_Result_Length);
      First, Last : Positive;
   begin
      --  Force fixed decimal notation (no exponent)
      FIO.Put
        (To   => Buffer,
         Item => Result,
         Exp  => 0);

      First := Buffer'First;
      Last := Buffer'Last;

      --  Trim trailing spaces
      while Last >= First and then Buffer (Last) = ' ' loop
         Last := Last - 1;
      end loop;

      --  Trim trailing zeros
      while Last >= First and then Buffer (Last) = '0' loop
         Last := Last - 1;
      end loop;

      --  Trim dot if needed
      if Last >= First and then Buffer (Last) = '.' then
         Last := Last - 1;
      end if;

      --  Trim leading spaces
      while First <= Buffer'Last and then Buffer (First) = ' ' loop
         First := First + 1;
      end loop;

      return Buffer (First .. Last);
   end Format_Result;

   procedure Update_Result (Result : Float) is
   begin
      Result_Label.Set_Text ("= " & Format_Result (Result));
   end Update_Result;

   --  Main GUI program
   procedure Open_Main_Window is
   begin
      --  Initialize window
      Gtk.Main.Init;

      Window := Gtk_Window_New (Window_Toplevel);
      Window.Set_Title ("Ada calculator");
      Window.Set_Default_Size (Window_Width, Window_Height);

      --  Register handler for closing window
      Close_Handler.Connect
        (Widget => Window,
         Name => "destroy",
         Marsh => Close_Handler.To_Marshaller (Close_Window'Access));

      --  Define layout
      Gtk.Box.Gtk_New
        (VBox, Orientation => Orientation_Vertical, Spacing => Gint (Padding));
      Gtk.GEntry.Gtk_New (Input_Field);
      Gtk.Label.Gtk_New (Result_Label, "= 0");
      Result_Label.Set_Xalign (0.0);
      Gtk.Grid.Gtk_New (Buttons_Grid);
      Buttons_Grid.Set_Row_Spacing (Guint (Padding));
      Buttons_Grid.Set_Column_Spacing (Guint (Padding));

      for Row in Button_Labels'Range (1) loop
         for Col in Button_Labels'Range (2) loop
            if Button_Labels (Row, Col) /= ' ' then
               declare
                  Button : Gtk_Button;
                  Char : constant Character := Button_Labels (Row, Col);
               begin
                  Gtk.Button.Gtk_New
                    (Button => Button,
                     Label => "" & Char);
                  Buttons_Grid.Attach
                    (Child => Button,
                     Left => Col,
                     Top => Row,
                     Width => 1,
                     Height => 1);
                  Button_Click_Handler.Connect
                    (Widget => Button,
                     Name => "clicked",
                     Marsh => Button_Click_Handler.To_Marshaller
                       (Handle_Button_Click'Access),
                     User_Data => Char);
               end;
            end if;
         end loop;
      end loop;

      VBox.Pack_Start (Input_Field,
                       Expand => False,
                       Fill => True,
                       Padding => Guint (Padding));
      VBox.Pack_Start (Result_Label,
                       Expand => False,
                       Fill => True,
                       Padding => Guint (Padding));
      VBox.Pack_Start (Buttons_Grid,
                       Expand => True,
                       Fill => True,
                       Padding => Guint (Padding));
      VBox.Set_Margin_Top (Gint (Padding));
      VBox.Set_Margin_Bottom (Gint (Padding));
      VBox.Set_Margin_Left (Gint (Padding));
      VBox.Set_Margin_Right (Gint (Padding));
      Window.Add (VBox);

      --  Launch window
      Window.Show_All;
      Gtk.Main.Main;
   end Open_Main_Window;
end gui_pkg;
