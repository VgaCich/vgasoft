program unload;

uses windows, Messages;

begin
  PostMessage(FindWindow('hkhdclass', ''), WM_DESTROY, 0, 0);
end.
