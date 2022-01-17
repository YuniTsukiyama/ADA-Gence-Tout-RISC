package body Label is

   overriding function "=" (Left, Right : Label) return Boolean is
   begin
      return Left.Symbol = Right.Symbol;
   end "=";

end Label;
