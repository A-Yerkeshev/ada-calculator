--  Calculator implemented in object-oriented way

with user_input; use user_input;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body calculator_oop is

   procedure Calculate (Result : in out Float; User_Input : String) is
      Calc_Idx, Calc_End_Idx : Positive;
      Invalid_Input : exception;

      type Token is abstract tagged null record;
      type Number_Token is new Token with
         record
            Number : Float;
         end record;
      type Operator_Token is new Token with
         record
            Operator : Valid_Operator;
         end record;
      type Placeholder_Token is new Token with null record;
      type Token_Access is access all Token'Class;
      type Tokens_Array is array (User_Input'Range) of Token_Access;

      Parsed_Input : Tokens_Array;
      First_Token : Token_Access;

      procedure Free is new Ada.Unchecked_Deallocation (Object => Token'Class,
                                                        Name => Token_Access);

      function Parse_Input return Tokens_Array is
         Res : Tokens_Array := (others => new Placeholder_Token);
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
                  Res (Idx) :=
                    new Number_Token'(Number => Float'Value (Number_Str));
                  Idx := Idx + 1;
               end if;
               Res (Idx) := new Operator_Token'(Operator => User_Input (I));
               Idx := Idx + 1;
               Number_Str := (others => ' ');
               NIdx := 1;
            end if;
         end loop;

         --  Create token for the last number
         if NIdx > 1 then
            Res (Idx) :=
              new Number_Token'(Number => Float'Value (Number_Str));
         end if;

         return Res;
      end Parse_Input;

      procedure Perform_Operation (Result : in out Float;
                                   O_Token : Operator_Token;
                                   N_Token : Number_Token) is
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

      procedure Debug_Tokens_Array (Parsed_Input : Tokens_Array) is
      begin
         Put_Line ("Tokens array:");
         for T of Parsed_Input loop
            if T.all in Number_Token then
               Ada.Float_Text_IO.Put (Item => Number_Token (T.all).Number,
                                      Fore => 0,
                                      Aft => 2,
                                      Exp => 0);
            elsif T.all in Operator_Token then
               Put (Operator_Token (T.all).Operator);
            else
               null;
            end if;
            Put (" | ");
         end loop;
         Put_Line ("End tokens array");
      end Debug_Tokens_Array;
   begin
      Parsed_Input := Parse_Input;
      Calc_Idx := Parsed_Input'First;
      Calc_End_Idx := Calc_Idx;
      First_Token := Parsed_Input (Calc_Idx);

      if First_Token.all in Number_Token then
         Result := Number_Token (First_Token.all).Number;
         Calc_Idx := Calc_Idx + 1;
      end if;

      while Calc_End_Idx < Parsed_Input'Last
        and then Parsed_Input
          (Calc_End_Idx + 1).all not in Placeholder_Token loop
         Calc_End_Idx := Calc_End_Idx + 1;
      end loop;

      while Calc_Idx < Calc_End_Idx loop
         declare
            O_Token : constant Operator_Token := Operator_Token
              (Parsed_Input (Calc_Idx).all);
            N_Token : constant Number_Token   := Number_Token
              (Parsed_Input (Calc_Idx + 1).all);
            Temp : Float := N_Token.Number;
            Next_Idx : Positive := Calc_Idx + 2;
         begin
            --  If + or -, resolve all following * and / first
            if O_Token.Operator in '+' | '-' then
               while Next_Idx < Calc_End_Idx loop
                  exit when Operator_Token
                    (Parsed_Input (Next_Idx).all).Operator not in '*' | '/';

                  declare
                     Mul_Op : constant Operator_Token :=
                       Operator_Token (Parsed_Input (Next_Idx).all);
                     Mul_Num : constant Number_Token :=
                       Number_Token (Parsed_Input (Next_Idx + 1).all);
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

      --  Delete tokens
      for T of Parsed_Input loop
         Free (T);
      end loop;
   end Calculate;

end calculator_oop;
