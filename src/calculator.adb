with gui_pkg;
with calculator_fop; use calculator_fop;
--  with calculator_oop; use calculator_oop;

procedure Calculator with SPARK_Mode is
   Result : Float := 0.0;

   procedure Call_Calculate (User_Input : String);

   package gui is new gui_pkg (Call_Calculate);

   procedure Call_Calculate (User_Input : String) is
      task Calculate_Task;

      task body Calculate_Task is
      begin
         Calculate (Result, User_Input);
         gui.Update_Result (Result);
      end Calculate_Task;
   begin
      null;
   end Call_Calculate;
begin
   gui.Open_Main_Window;
end Calculator;
