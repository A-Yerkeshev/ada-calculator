--  Calculator implemented in function-oriented way

with user_input; use user_input;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;

package body calculator_fop with SPARK_Mode is

   procedure Calculate (Result : in out Float; User_Input : String) is
      Calc_Idx, Calc_End_Idx : Positive;
      Invalid_Input : exception;

      type Token_Class is (Number_Token_Class,
                           Operator_Token_Class,
                           Placeholder_Token_Class);
      type Token (Class : Token_Class := Placeholder_Token_Class) is record
         case Class is
         when Number_Token_Class =>
            Number : Float;
         when Operator_Token_Class =>
            Operator : Valid_Operator;
         when Placeholder_Token_Class =>
            null;
         end case;
      end record;
      subtype Number_Token is Token (Class => Number_Token_Class);
      subtype Operator_Token is Token (Class => Operator_Token_Class);
      type Tokens_Array is array (User_Input'Range) of Token;

      Parsed_Input : Tokens_Array;
      First_Token : Token;

      function Parse_Input return Tokens_Array
        with Global => User_Input
      is
         Res : Tokens_Array;
         Idx : Positive range Res'Range := Res'First;
         Number_Str : String (User_Input'Range) := (others => ' ');
         NIdx : Positive range User_Input'Range := 1;
      begin
         for I in User_Input'Range loop
            --  Raise error on invalid character
            if User_Input (I) not in Permitted_Input_Char then
               raise Invalid_Input with "Invalid input character: " &
                 User_Input (I);
            end if;

            if User_Input (I) in Valid_Digit_Char | '.' then
               --  If the character is digit or dot -
               --    1. append it to the number string
               --    2. increment number string index
               Number_Str (NIdx) := User_Input (I);
               NIdx := NIdx + 1;
            elsif User_Input (I) in Valid_Operator | ' ' then
               --  If the character is operator -
               --    1. convert number string to number token
               --    2. add number token to the result
               --    3. create operator token
               --    4. add operator token to the result
               --    5. reset variables
               if NIdx > 1 then
                  Res (Idx) := (Class => Number_Token_Class,
                                Number => Float'Value (Number_Str));
                  Idx := Idx + 1;
               end if;
               Res (Idx) := (Class => Operator_Token_Class,
                             Operator => User_Input (I));
               Idx := Idx + 1;
               Number_Str := (others => ' ');
               NIdx := 1;
            end if;
         end loop;

         --  Create token for the last number
         if NIdx > 1 then
            Res (Idx) := (Class => Number_Token_Class,
                          Number => Float'Value (Number_Str));
         end if;

         return Res;
      end Parse_Input;

      procedure Perform_Operation (Result : in out Float;
                                   O_Token : Operator_Token;
                                   N_Token : Number_Token)
        with Global => null,
        Pre => O_Token.Operator in Valid_Operator and then not
        (O_Token.Operator = '/' and then N_Token.Number = 0.0),
        Depends => (Result => (Result, O_Token, N_Token))
      is
         Number : constant Float := N_Token.Number;
      begin
         case O_Token.Operator is
         when '+' =>
            Result := Result + Number;
         when '-' =>
            Result := Result - Number;
         when '*' =>
            Result := Result * Number;
         when '/' =>
            Result := Result / Number;
         end case;
      end Perform_Operation;

      procedure Debug_Tokens_Array (Parsed_Input : Tokens_Array)
        with Global => null
      is
      begin
         Put_Line ("Tokens array:");
         for T of Parsed_Input loop
            case T.Class is
            when Number_Token_Class =>
               Ada.Float_Text_IO.Put (Item => T.Number,
                                      Fore => 0,
                                      Aft => 2,
                                      Exp => 0);
            when Operator_Token_Class =>
               Put (T.Operator);
            when others =>
               null;
            end case;
            Put (" | ");
         end loop;
         Put_Line ("End tokens array");
      end Debug_Tokens_Array;
   begin
      Parsed_Input := Parse_Input;
      Calc_Idx := Parsed_Input'First;
      Calc_End_Idx := Calc_Idx;
      First_Token := Parsed_Input (Calc_Idx);

      if First_Token.Class = Number_Token_Class then
         Result := First_Token.Number;
         Calc_Idx := Calc_Idx + 1;
      end if;

      while Calc_End_Idx < Parsed_Input'Last
        and then Parsed_Input
          (Calc_End_Idx + 1).Class /= Placeholder_Token_Class loop
         Calc_End_Idx := Calc_End_Idx + 1;
      end loop;

      while Calc_Idx < Calc_End_Idx loop
         declare
            O_Token : constant Operator_Token := Parsed_Input (Calc_Idx);
            N_Token : constant Number_Token   := Parsed_Input (Calc_Idx + 1);
            Temp : Float := N_Token.Number;
            Next_Idx : Positive := Calc_Idx + 2;
         begin
            --  If + or -, resolve all following * and / first
            if O_Token.Operator in '+' | '-' then
               while Next_Idx < Calc_End_Idx loop
                  exit when Parsed_Input (Next_Idx).Operator not in '*' | '/';

                  declare
                     Mul_Op : constant Operator_Token :=
                       Parsed_Input (Next_Idx);
                     Mul_Num : constant Number_Token :=
                       Parsed_Input (Next_Idx + 1);
                  begin
                     if Mul_Op.Operator = '*' then
                        Temp := Temp * Mul_Num.Number;
                     else
                        Temp := Temp / Mul_Num.Number;
                     end if;
                  end;

                  Next_Idx := Next_Idx + 2;
               end loop;

               --  Apply the + or - with the collapsed value
               if O_Token.Operator = '+' then
                  Result := Result + Temp;
               else
                  Result := Result - Temp;
               end if;

               Calc_Idx := Next_Idx;

            else
               --  For * and / at the beginning
               Perform_Operation (Result, O_Token, N_Token);
               Calc_Idx := Calc_Idx + 2;
            end if;
         end;
      end loop;

   end Calculate;

end calculator_fop;
