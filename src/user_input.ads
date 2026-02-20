package user_input is

   subtype Valid_Digit_Char is Character range '0' .. '9';
   subtype Valid_Operator is Character
     with Static_Predicate => Valid_Operator in '+' | '-' | '*' | '/';
   subtype Permitted_Input_Char is Character
     with Static_Predicate =>
       Permitted_Input_Char in Valid_Digit_Char
       or else Permitted_Input_Char in Valid_Operator
       or else Permitted_Input_Char in '.' | ' ';

end user_input;
