program pre_revprop_change;

{$APPTYPE CONSOLE}

begin
  if (ParamStr(5)='M') and (ParamStr(4)='svn:log')
    then Halt(0);
  WriteLn(ErrOutput, 'Changing revision properties other than svn:log is prohibited');
  Halt(1);
end.