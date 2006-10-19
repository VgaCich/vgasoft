unit MakeVPK;

interface

function VPKNew(Name: PChar): Boolean; stdcall; external 'MakeVPK.dll';
function VPKAddFile(Name: PChar; FileType: Byte; var FileData; FileSize: Cardinal): Boolean; stdcall; external 'MakeVPK.dll';
function VPKClose: Boolean; stdcall; external 'MakeVPK.dll';  

implementation

end.
