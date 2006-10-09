const
  FTE_DIR=0;
  FTE_NRV=1;
  FTE_LZMA=2;
  FTE_STORE=3;
  PakID=$4B505356;

type
  TPakHdr=packed record
    ID, Adler32, EntriesCount, FTPackedSize, FTSize: Cardinal;
  end;
  TPakFTEntry=packed record
    NameSize: Byte;
    case EntryType: Byte of
      FTE_DIR: (EntriesCount: Cardinal);
      FTE_NRV, FTE_LZMA, FTE_STORE: (Offset: Cardinal);
  end;

