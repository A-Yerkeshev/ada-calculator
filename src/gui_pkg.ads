generic
   with procedure Call_Calculate (User_Input : String);
package gui_pkg with SPARK_Mode is
   procedure Open_Main_Window with Global => null;
   procedure Update_Result (Result : Float)
     with Global => null;
end gui_pkg;
