package calculator_fop with SPARK_Mode is
   procedure Calculate (Result : in out Float; User_Input : String)
     with Global => null,
     Pre => User_Input'Length /= 0;
end calculator_fop;
