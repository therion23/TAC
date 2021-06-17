Program Therions_something_long;

{$IFDEF MSDOS}
{$M $2000, $60000, $A0000}
{$ENDIF}

{$IFDEF OS2}
{$M $4000}
{$ENDIF}

Uses
  
{$IFDEF OS2}
  use32,
{$ENDIF}
  
{$IFDEF MSDOS}
  searches,
{$ENDIF}
  
  DOS;

Type
  
{$IFDEF OS2}
  superdb = Array [1..98280] Of Byte;
{$ENDIF}
  
  lintdb = Array [1..16380] Of LongInt;
  
  worddb = Array [1..16380] Of Word;
  
  listdb = Array [1..5000] Of String [12];
  
  klstdb = Array [1..256] Of Array [1..10] Of String [20];
  
  Buffer = Array [1..65520] Of Char;
  
  cpybuf = Array [1..4096] Of Byte;
  
Const
  
  crc_32_tab: Array [0..255] Of LongInt = (
  $00000000, $77073096, $ee0e612c, $990951ba,
  $076dc419, $706af48f, $e963a535, $9e6495a3,
  $0edb8832, $79dcb8a4, $e0D5e91e, $97D2D988,
  $09b64c2b, $7eb17cbd, $e7b82D07, $90bf1D91,
  $1db71064, $6ab020f2, $f3b97148, $84be41de,
  $1adad47D, $6ddde4eb, $f4D4b551, $83D385c7,
  $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec,
  $14015c4f, $63066cd9, $fa0f3D63, $8D080df5,
  $3b6e20c8, $4c69105e, $D56041e4, $a2677172,
  $3c03e4D1, $4b04D447, $D20D85fd, $a50ab56b,
  $35b5a8fa, $42b2986c, $dbbbc9D6, $acbcf940,
  $32D86ce3, $45df5c75, $dcd60dcf, $abd13D59,
  $26D930ac, $51de003a, $c8D75180, $bfd06116,
  $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
  $2802b89e, $5f058808, $c60cd9b2, $b10be924,
  $2f6f7c87, $58684c11, $c1611dab, $b6662D3D,
  $76dc4190, $01db7106, $98D220bc, $efd5102a,
  $71b18589, $06b6b51f, $9fbfe4a5, $e8b8D433,
  $7807c9a2, $0f00f934, $9609a88e, $e10e9818,
  $7f6a0dbb, $086D3D2D, $91646c97, $e6635c01,
  $6b6b51f4, $1c6c6162, $856530D8, $f262004e,
  $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
  $65b0D9c6, $12b7e950, $8bbeb8ea, $fcb9887c,
  $62dd1ddf, $15da2D49, $8cd37cf3, $fbd44c65,
  $4db26158, $3ab551ce, $a3bc0074, $D4bb30e2,
  $4adfa541, $3dd895D7, $a4D1c46D, $D3D6f4fb,
  $4369e96a, $346ed9fc, $ad678846, $da60b8D0,
  $44042D73, $33031de5, $aa0a4c5f, $dd0D7cc9,
  $5005713c, $270241aa, $be0b1010, $c90c2086,
  $5768b525, $206f85b3, $b966D409, $ce61e49f,
  $5edef90e, $29D9c998, $b0D09822, $c7D7a8b4,
  $59b33D17, $2eb40D81, $b7bd5c3b, $c0ba6cad,
  $edb88320, $9abfb3b6, $03b6e20c, $74b1D29a,
  $ead54739, $9dd277af, $04db2615, $73dc1683,
  $e3630b12, $94643b84, $0D6D6a3e, $7a6a5aa8,
  $e40ecf0b, $9309ff9D, $0a00ae27, $7D079eb1,
  $f00f9344, $8708a3D2, $1e01f268, $6906c2fe,
  $f762575D, $806567cb, $196c3671, $6e6b06e7,
  $fed41b76, $89D32be0, $10da7a5a, $67dd4acc,
  $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
  $D6D6a3e8, $a1D1937e, $38D8c2c4, $4fdff252,
  $D1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
  $D80D2bda, $af0a1b4c, $36034af6, $41047a60,
  $df60efc3, $a867df55, $316e8eef, $4669be79,
  $cb61b38c, $bc66831a, $256fd2a0, $5268e236,
  $cc0c7795, $bb0b4703, $220216b9, $5505262f,
  $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04,
  $c2D7ffa7, $b5D0cf31, $2cd99e8b, $5bdeae1D,
  $9b64c2b0, $ec63f226, $756aa39c, $026D930a,
  $9c0906a9, $eb0e363f, $72076785, $05005713,
  $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38,
  $92D28e9b, $e5D5be0D, $7cdcefb7, $0bdbdf21,
  $86D3D2D4, $f1D4e242, $68ddb3f8, $1fda836e,
  $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
  $88085ae6, $ff0f6a70, $66063bca, $11010b5c,
  $8f659eff, $f862ae69, $616bffd3, $166ccf45,
  $a00ae278, $D70dd2ee, $4e048354, $3903b3c2,
  $a7672661, $D06016f7, $4969474D, $3e6e77db,
  $aed16a4a, $D9D65adc, $40df0b66, $37D83bf0,
  $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
  $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6,
  $bad03605, $cdd70693, $54de5729, $23D967bf,
  $b3667a2e, $c4614ab8, $5D681b02, $2a6f2b94,
  $b40bbe37, $c30c8ea1, $5a05df1b, $2D02ef8D );
  
  id: String [9] = 'THANKS þ ';
  
  verstr: String [4] = '0180';
  
{$IFDEF MSDOS}
  cfgfile: String [12] = 'TAC.CFG';
{$ELSE}
  cfgfile: String [12] = 'TAC2.CFG';
{$ENDIF}
  
  
  ignorecase: Boolean = False;
  movefiles: Boolean = False;
  recursion: Boolean = True;
  logging: Boolean = False;
  
Var
  
{$IFDEF OS2}
  tdb: ^superdb;
  sss: ^String;
{$ENDIF}
  
  pointers: Array [1..5] Of Boolean;
  
  THANKS_base: String;
  THANKS_list: String;
  THANKS_klst: String;
  THANKS_parm: String;
  THANKS_sdir: String;
  
  numbase: Word;
  numlist: Word;
  numklst: Word;
  
  logfile,
  echostr: String;
  
  crc32db: ^lintdb;
  fsizedb: ^worddb;
  klistdb: ^klstdb;
  nlistdb: Array [1..2] Of ^listdb;
  
  copybuf: ^cpybuf;
  
  dbheader: String [12];
  
  srcpath: String;
  
  buf: ^Buffer;
  
  bufaddr: Array [1..2] Of Word;
  
  dirinfo: SearchRec;
  
  inf2: SearchRec;
  
  inf: Array [1..16] Of SearchRec;
  
  level: Byte;
  
  errcode: Byte;
  
  dirsleft: LongInt;
  
  dbid: Array [1..8] Of Char;
  
  tb: File;
  
  i: File;
  
  ci: Text;

  lfile: Text;
  
  s1,
  s2: String;
  
  st1,
  st2,
  st3: String;
  
  b1,
  b2: Byte;
  
  w1,
  w2: Word;
  
  l1,
  l2: LongInt;
  
  i1,
  i2: Integer;
  
  nuked,
  valid: Boolean;
  
  (***** MISCELLANEOUS ROUTINES *****)
  
Procedure header;
Begin
  WriteLn;
{$IFDEF MSDOS}
  WriteLn ('Therion''s Hellspawn Advert Nuker & Killer System þ Version 1.80SE');
{$ELSE}
  WriteLn ('Therion''s Hellspawn Advert Nuker & Killer System for OS/2 þ Version 1.80SE');
{$ENDIF}
  WriteLn ('         (c) 1994-1996 Nocturnal Productions. All Rights Reserved.');
  WriteLn;
End;

Procedure usage;
Begin
  WriteLn ('Usage: THANKS [-|/]<switches> [filemask]');
  WriteLn;
  WriteLn ('Switches:');
  WriteLn;
  WriteLn ('  a to add files fitting [filemask] to the CRC-32 database and exit.');
  WriteLn ('  c to cleanup mode.');
  WriteLn ('  d to inhibit use of the CRC-32 database when checking files.');
  WriteLn ('  h or ? for this help screen.');
  WriteLn ('  i to ignore case during keyword check.');
  WriteLn ('  k to inhibit use of the keyword list when checking files.');
  WriteLn ('  l to inhibit use of the filename list when checking files.');
  WriteLn ('  m to move files to storage dir instead of deleting them.');
  WriteLn ('  r to inhibit recursion.');
  WriteLn ('  w to enable logging.');
  WriteLn;
  WriteLn ('Example: THANKS -dk only kills files found in the filename list.');
  WriteLn;
  WriteLn ('NOTE: The filemask can only be specified in add mode.');
  WriteLn;
End;

Procedure lwrite (s: String);
Begin
  If IOResult = 0 Then;
  WriteLn (s);
  If logging Then WriteLn (lfile, s);
End;

Procedure err (X: Byte);
Begin
  Case X Of
    1: lwrite (id + cfgfile + ' not found!');
    2: lwrite (id + 'Invalid version of ' + cfgfile + ' - Please upgrade!');
    3: lwrite (id + 'Invalid parameters encountered!');
    4: lwrite (id + 'Invalid number of parameters!');
    5: lwrite (id + 'Filemask can only be used in add mode!');
    10: lwrite (id + 'Just updated database - please run again!');
    11: lwrite (id + 'Broken database!');
    20: lwrite (id + 'Panic! Database too large!');
    255: usage;
  End;
  Halt (X);
{$I-}
  If logging Then Close (lfile);
End;

(*

     Blazingly fast Upper Case routine.

*)

Procedure upperbuf; Assembler;
Asm
  push  DS
  push  ES
{$IFDEF MSDOS}
  mov   BX, Offset bufaddr
  mov   ES, Word Ptr [BX]
  Inc   BX
  Inc   BX
  mov   SI, Word Ptr [BX]
{$ELSE}
  mov   ebx, Offset bufaddr
  mov   ES, Word Ptr [ebx]
  Inc   ebx
  Inc   ebx
  mov   SI, Word Ptr [ebx]
{$ENDIF}
  push  SI
  pop   DI
  push  ES
  pop   DS
  cld
  mov   CX, 65520
  @loop1:
  lodsb
  cmp   AL, 'a'
  jl    @next
  cmp   AL, 'z'
  jg    @next
  XOr   AL, 32
  @next:
  stosb
  loop  @loop1
  pop   ES
  pop   DS
End;

{$IFDEF OS2}

(*

     Pathetic attempt at an OS/2 BlockPos(), but it's really not that slow
     as i had feared. It is way faster than the DOS Boyer/Moore routine i
     used in the older versions of THANKS.

*)

Function BlockPos: Word;
Var
  wp1,
  wp2,
  wc1,
  wc2: Word;
  ts1: String;
  gotcha: Boolean;
Begin
  wp1 := 1;
  wp2 := 1;
  gotcha := False;
  Repeat
    While (sss^ [1] <> buf^ [wp1] ) And (wp1 < l2) Do Inc (wp1);
    If (wp1 + Length (sss^) ) < l2 Then Begin
      Move (buf^ [wp1], ts1 [1], Length (sss^) );
      ts1 [0] := sss^ [0];
      If ts1 = sss^ Then gotcha := True;
    End;
    Inc (wp1);
  Until (gotcha) Or (wp1 >= l2);
  If wp1 >= l2 Then blockpos := 0 Else blockpos := wp1 - 1;
End;

{$ENDIF}

Function UpdC32 (octet: Byte; crc: LongInt) : LongInt;
Begin
  UpdC32 := crc_32_tab [Byte (crc XOr LongInt (octet) ) ] XOr ( (crc ShR 8) And $00FFFFFF)
End;

Function calccrc (count: Word): LongInt;
Begin
  l1 := - 1;
  For w1 := 1 To count Do l1 := UpdC32 (Ord (buf^ [w1] ), l1);
  calccrc := l1 XOr $ffffffff;
End;

Function exist (filename: String): Boolean;
Var inf: SearchRec;
Begin
  FindFirst (filename, AnyFile, inf);
  exist := (DosError = 0);
{$IFDEF OS2}
  findclose (inf);
{$ENDIF}
End;

Procedure movefile (mfname: String);
Var
  mi,
  mo: File;
  ms1,
  ms2: String;
{$IFDEF MSDOS}
  mw1: Word;
{$ELSE}
  mw1: LongInt;
{$ENDIF}
Begin
  ms1 := mfname;
  While Pos (':', ms1) <> 0 Do Delete (ms1, 1, 1);
  While Pos ('\', ms1) <> 0 Do Delete (ms1, 1, 1);
  New (copybuf);
  If THANKS_sdir [Length (THANKS_sdir) ] <> '\' Then THANKS_sdir := THANKS_sdir + '\';
  ms1 := THANKS_sdir + ms1;
  Assign (mi, mfname);
  Reset (mi, 1);
  Assign (mo, ms1);
  Rewrite (mo, 1);
  Repeat
    BlockRead (mi, copybuf^, 4096, mw1);
    BlockWrite (mo, copybuf^, mw1);
  Until EoF (mi);
  Close (mo);
  Close (mi);
End;

Procedure ShellSort (Start, Stop: Word);

Var
  gap, i, j, k : Integer;
  tl: LongInt;
  tw: Word;
  
Begin
  gap := (Stop - Start) Div 2;
  Stop := Stop - Start + 1;
  While (gap > 0) Do
  Begin
    For i := (gap + 1) To Stop Do
    Begin
      j := i - gap;
      While (j > 0) Do
      Begin
        k := j + gap;
        If (crc32db^ [j + Start - 1] <= crc32db^ [k + Start - 1] ) Then j := 0
        Else Begin
          tl := crc32db^ [j + start - 1];
          tw := fsizedb^ [j + start - 1];
          crc32db^ [j + start - 1] := crc32db^ [k + start - 1];
          fsizedb^ [j + start - 1] := fsizedb^ [k + start - 1];
          crc32db^ [k + start - 1] := tl;
          fsizedb^ [k + start - 1] := tw;
          j := j - gap;
        End;
      End;
    End;
    gap := gap Div 2;
  End;
End;

(***** RECURSION ROUTINES *****)

Procedure scanlevel;
Begin
  FindFirst ('*.*', $10, inf [level] );
End;

(*

     !NOTE!:

     This routine thrashes the program if the subdir structure is deeper
     than the size of the inf array! Increase if necessary!

*)

Function downlevel: Boolean;
Begin
  While ( (inf [level].Name [1] = '.') Or (inf [level].Attr <> $10) ) And (DosError = 0) Do FindNext (inf [level] );
  If DosError = 0 Then Begin
    ChDir (inf [level].Name);
    Inc (level);
    scanlevel;
    downlevel := True;
  End
  Else downlevel := False;
End;

Procedure nextdir;
Begin
{$IFDEF OS2}
  findclose (inf [level] );
{$ENDIF}
  ChDir ('..');
  Dec (level);
  FindNext (inf [level] );
  While ( (inf [level].Name [1] = '.') Or (inf [level].Attr <> $10) ) And (DosError = 0) Do FindNext (inf [level] );
  If DosError = 0 Then Begin
    If (inf [level].Name [1] <> '.') And (inf [level].Attr = $10) Then Begin
      ChDir (inf [level].Name);
      Inc (level);
      scanlevel;
    End;
  End;
End;

(***** KILLER ROUTINES *****)

Procedure listkill2;
Begin
  For w1 := 1 To numlist Do Begin
    If inf2.Name = nlistdb [ ( (w1 - 1) Div 5000) + 1]^ [ ( (w1 - 1) Mod 5000) + 1] Then Begin
{$I-}
      SetFAttr (i, $20);
      If IOResult = 0 Then;
      If movefiles Then movefile (inf2.Name);
      Close (i);
      Erase (i);
{$I+}
      If IOResult = 0 Then Begin
        lwrite (id + 'L þ Found and removed ' + inf2.Name);
        nuked := True;
        Break;
      End;
    End;
  End;
End;

Procedure listkill;
Begin
  FindFirst ('*.*', $27, inf2);
  While DosError = 0 Do Begin
    For w1 := 1 To numlist Do Begin
      If inf2.Name = nlistdb [ ( (w1 - 1) Div 5000) + 1]^ [ ( (w1 - 1) Mod 5000) + 1] Then Begin
        Assign (i, inf2.Name);
{$I-}
        SetFAttr (i, $20);
        If IOResult = 0 Then;
        If movefiles Then movefile (inf2.Name);
        Erase (i);
{$I+}
        If IOResult = 0 Then Begin
          lwrite (id + 'L þ Found and removed ' + inf2.Name);
          nuked := True;
          Break;
        End;
      End;
      FindNext (inf2);
    End;
  End;
{$IFDEF OS2}
  findclose (inf2);
{$ENDIF}
End;

Procedure klstkill2;

{$IFDEF OS2}
Var su: LongInt;
{$ENDIF}

Begin
  If ignorecase Then Begin
    bufaddr [1] := Seg (buf^);
    bufaddr [2] := Ofs (buf^);
    upperbuf;
  End;
  For w1 := 1 To numklst Do Begin
    valid := True;
    For w2 := 1 To 10 Do Begin
{$IFDEF MSDOS}
      If (klistdb^ [w1] [w2] <> '') And (blockpos (buf^, l2, klistdb^ [w1] [w2] ) = 0) Then Begin
        valid := False;
        Break;
      End;
{$ELSE}
      If (klistdb^ [w1] [w2] <> '') Then Begin
        New (sss);
        sss^ := klistdb^ [w1] [w2];
        If blockpos = 0 Then Begin
          valid := False;
          Break;
        End;
        Dispose (sss);
      End;
{$ENDIF}
    End;
    If valid Then Begin
      Assign (i, inf2.Name);
      SetFAttr (i, $20);
      If movefiles Then movefile (inf2.Name);
{$I-}
      Erase (i);
{$I+}
      If IOResult = 0 Then Begin
        lwrite (id + 'K þ Found and removed ' + inf2.Name);
        Exit;
      End;
    End;
  End;
End;

Procedure klstkill;
Begin
  New (buf);
  FindFirst ('*.*', $21, inf2);
  While DosError = 0 Do Begin
    Assign (i, inf2.Name);
{$I-}
    Reset (i, 1);
{$I+}
    If IOResult = 0 Then Begin
      nuked := False;
      If (THANKS_list <> '') Then listkill2;
      If Not nuked Then Begin
        l2 := FileSize (i);
        Close (i);
        If l2 <= 65520 Then Begin
          Reset (i, 1);
          BlockRead (i, buf^, l2);
          Close (i);
          klstkill2;
        End;
      End;
      FindNext (inf2);
    End;
  End;
  Dispose (buf);
{$IFDEF OS2}
  findclose (inf2);
{$ENDIF}
End;

Procedure basekill;
Begin
  New (buf);
  FindFirst ('*.*', $21, inf2);
  While DosError = 0 Do Begin
    Assign (i, inf2.Name);
{$I-}
    Reset (i, 1);
{$I+}
    If IOResult = 0 Then Begin
      nuked := False;
      If (THANKS_list <> '') Then listkill2;
      If Not nuked Then Begin
        l2 := FileSize (i);
        Close (i);
        If l2 <= 65520 Then Begin
          Reset (i, 1);
          BlockRead (i, buf^, l2);
          Close (i);
          l1 := calccrc (l2);
          For w1 := 1 To numbase Do Begin
            If l1 < crc32db^ [w1] Then Break;
            If (l1 = crc32db^ [w1] ) And (l2 = fsizedb^ [w1] ) Then Begin
              Assign (i, inf2.Name);
              SetFAttr (i, $20);
              If movefiles Then movefile (inf2.Name);
{$I-}
              Erase (i);
{$I+}
              If IOResult = 0 Then lwrite (id + 'B þ Found and removed ' + inf2.Name);
              nuked := True;
            End;
          End;
        End;
        If (THANKS_klst <> '') And (nuked = False) And (l2 <= 65520) Then klstkill2;
      End;
      FindNext (inf2);
    End;
  End;
  Dispose (buf);
{$IFDEF OS2}
  findclose (inf2);
{$ENDIF}
End;

(***** INITIALIZATION ROUTINES *****)

Procedure initbase;
Begin
  Assign (tb, THANKS_base);
  Reset (tb, 1);
  If IOResult <> 0 Then Begin
    errcode := 1;
    Exit;
  End;
  If (FileSize (tb) < 12) And (FileSize (tb) Mod 6 <> 0) Then err (11);
  BlockRead (tb, dbid, 8);
  If dbid <> 'THANKS!'#$1a Then Begin
    errcode := 11;
    Exit;
  End;
  BlockRead (tb, numbase, 2);
  If (Hi (numbase) <> 1) Or (Lo (numbase) <> 50) Then Begin
    errcode := 12;
    Exit;
  End;
  BlockRead (tb, numbase, 2);
  l2 := FileSize (tb);
  If (l2 Div 6) - 2 <> numbase Then err (11);
  If (numbase > 0) And (numbase < 16381) Then Begin
    New (crc32db);
    New (fsizedb);
    For w1 := 1 To numbase Do Begin
      BlockRead (tb, crc32db^ [w1], 4);
      BlockRead (tb, fsizedb^ [w1], 2);
    End;
    Close (tb);
    pointers [1] := True;
    pointers [2] := True;
  End
  Else THANKS_base := '';
  If numbase > 16380 Then err (20);
End;

Procedure initlist;
Begin
  Assign (ci, THANKS_list);
  Reset (ci);
  numlist := 0;
  If Not EoF (ci) Then Begin
    New (nlistdb [1] );
    New (nlistdb [2] );
    Repeat
      ReadLn (ci, s1);
      nlistdb [ (numlist Div 5000) + 1]^ [numlist + 1] := s1;
      Inc (numlist);
    Until (numlist = 10000) Or (EoF (ci) );
    Close (ci);
    pointers [3] := True;
    If numlist < 5001 Then Dispose (nlistdb [2] ) Else pointers [4] := True;
  End
  Else THANKS_list := '';
End;

Procedure initklst;
Begin
  Assign (ci, THANKS_klst);
  Reset (ci);
  numklst := 0;
  If Not EoF (ci) Then Begin
    New (klistdb);
    Repeat
      ReadLn (ci, s1);
      If (Length (s1) > 0) And (s1 [1] <> ';') Then Begin
        If ignorecase Then For b2 := 1 To Length (s1) Do s1 [b2] := UpCase (s1 [b2] );
        b2 := 1;
        Repeat
          s2 := s1;
          b1 := Pos (',', s2);
          If b1 > 0 Then Delete (s2, b1, Length (s2) - b1 + 1);
          klistdb^ [numklst + 1] [b2] := s2;
          If b1 > 0 Then Delete (s1, 1, b1);
          Inc (b2);
        Until (s1 = s2) Or (b2 = 10);
        If b2 <> 10 Then For b1 := b2 To 10 Do klistdb^ [numklst + 1] [b1] := '';
        Inc (numklst);
      End;
    Until (numklst = 256) Or (EoF (ci) );
    Close (ci);
    pointers [5] := True;
  End
  Else THANKS_klst := '';
End;

(***** DATABASE MAINTENANCE ROUTINES *****)

{$IFDEF MSDOS}

Procedure cleanbase (errcode: Byte);
Begin
  If errcode = 31 Then Begin
    initbase;
    errcode := 0;
  End;
  If numbase > 0 Then Begin
    shellsort (1, numbase);
    dbheader := 'THANKS!'#$1a#50#1;
    Move (numbase, dbheader [11], 2);
    Assign (tb, THANKS_base);
    Rewrite (tb, 1);
    BlockWrite (tb, dbheader [1], 12);
    BlockWrite (tb, crc32db^ [1], 4);
    BlockWrite (tb, fsizedb^ [1], 2);
    w2 := numbase;
    If numbase > 1 Then For w1 := 2 To w2 Do Begin
      If (crc32db^ [w1] <> crc32db^ [w1 - 1] ) And (fsizedb^ [w1] <> fsizedb^ [w1 - 1] ) Then Begin
        BlockWrite (tb, crc32db^ [w1], 4);
        BlockWrite (tb, fsizedb^ [w1], 2);
      End
      Else If (crc32db^ [w1] = crc32db^ [w1 - 1] ) And (fsizedb^ [w1] = fsizedb^ [w1 - 1] ) Then Dec (numbase);
    End;
    Seek (tb, 10);
    BlockWrite (tb, numbase, 2);
    Close (tb);
  End;
  lwrite (id + 'Successful cleanup of database');
  err (errcode);
End;

{$ELSE}

Procedure cleanbase (errcode: Byte);
Var w3: Word;
Begin
  If errcode = 31 Then Begin
    initbase;
    errcode := 0;
  End;
  If numbase > 0 Then Begin
    shellsort (1, numbase);
    dbheader := 'THANKS!'#$1a#50#1;
    New (tdb);
    w3 := 1;
    Move (crc32db^ [1], tdb^ [1], 4);
    Move (fsizedb^ [1], tdb^ [5], 2);
    Inc (w3);
    w2 := numbase;
    If numbase > 1 Then For w1 := 2 To w2 Do Begin
      If (crc32db^ [w1] <> crc32db^ [w1 - 1] ) And (fsizedb^ [w1] <> fsizedb^ [w1 - 1] ) Then Begin
        Move (crc32db^ [w1], tdb^ [ (w3 * 6) - 5], 4);
        Move (fsizedb^ [w1], tdb^ [ (w3 * 6) - 1], 2);
        Inc (w3);
      End
      Else If (crc32db^ [w1] = crc32db^ [w1 - 1] ) And (fsizedb^ [w1] = fsizedb^ [w1 - 1] ) Then Dec (numbase);
    End;
    Assign (tb, THANKS_base);
    Rewrite (tb, 1);
    Move (numbase, dbheader [11], 2);
    BlockWrite (tb, dbheader [1], 12);
    BlockWrite (tb, tdb^ [1], numbase * 6);
    Close (tb);
    Dispose (tdb);
  End;
  lwrite (id + 'Successful cleanup of database');
  err (errcode);
End;

{$ENDIF}

Procedure addbase;
Begin
  If THANKS_base <> '' Then initbase;
  New (buf);
  FindFirst (ParamStr (2), $21, inf2);
  While DosError = 0 Do Begin
    Assign (i, inf2.Name);
{$I-}
    Reset (i, 1);
{$I+}
    If IOResult = 0 Then Begin
      l2 := FileSize (i);
      Close (i);
      If l2 <= 65520 Then Begin
        Reset (i, 1);
        BlockRead (i, buf^, l2);
        Close (i);
        l1 := calccrc (l2);
        If numbase < 16380 Then Begin
          Inc (numbase);
          crc32db^ [numbase] := l1;
          fsizedb^ [numbase] := l2;
          lwrite (id + 'Successful addition of ' + inf2.Name + ' to database');
        End;
      End;
    End;
    FindNext (inf2);
  End;
  Dispose (buf);
{$IFDEF OS2}
  findclose (inf2);
{$ENDIF}
  cleanbase (0);
End;

(***** MAIN ROUTINES *****)

Procedure parsecmdline;
Begin
  s1 := '';
  If THANKS_parm <> '' Then Begin
    If Pos (THANKS_parm [1], '-/') <> 0 Then Delete (THANKS_parm, 1, 1);
    s1 := THANKS_parm;
  End;
  If ParamCount > 0 Then s1 := s1 + ParamStr (1);
  For b1 := 1 To Length (s1) Do s1 [b1] := UpCase (s1 [b1] );
  If Pos (s1 [1], '-/') > 0 Then Delete (s1, 1, 1);
  For b1 := 1 To Length (s1) Do Begin
    Case s1 [b1] Of
      'A': addbase;
      'C': cleanbase (31);
      'D': THANKS_base := '';
      'H',
      '?': err (255);
      'I': ignorecase := True;
      'K': THANKS_klst := '';
      'L': THANKS_list := '';
      'M': If (THANKS_sdir <> '') And (exist (THANKS_sdir) ) Then movefiles := True;
      'R': recursion := False;
      'W': logging := True;
      Else err (3);
    End;
  End;
  If logfile = '' Then logging := False;
End;

Procedure init_THANKS;

Begin
  s1 := ParamStr (0);
  While Pos (s1 [Length (s1) ], ':\') = 0 Do Delete (s1, Length (s1), 1);
  srcpath := s1;
  s1 := s1 + cfgfile;
  Assign (ci, s1);
{$I-}
  Reset (ci);
{$I+}
  If IOResult <> 0 Then err (1);
  ReadLn (ci, s1);
  If s1 <> verstr Then err (2);
  For b1 := 1 To 178 Do ReadLn (ci, s1);
  ReadLn (ci, THANKS_base);
  ReadLn (ci, THANKS_list);
  ReadLn (ci, THANKS_klst);
  ReadLn (ci, THANKS_parm);
  ReadLn (ci, THANKS_sdir);
  For b1 := 1 To 9 Do ReadLn (ci, logfile);
  THANKS_base := srcpath + THANKS_base;
  THANKS_list := srcpath + THANKS_list;
  THANKS_klst := srcpath + THANKS_klst;
  Close (ci);
  If (ParamCount = 2) And (Pos ('a', ParamStr (1) ) = 0) And (Pos ('A', ParamStr (1) ) = 0) Then err (5);
  If (ParamCount > 0) Or (THANKS_parm <> '') Then parsecmdline;
  logfile := srcpath + logfile;
  For b1 := 1 To 5 Do pointers [b1] := False;
  errcode := 0;
  If exist (THANKS_base) Then initbase Else THANKS_base := '';
  If errcode <> 0 Then Begin
    Case errcode Of
      1: lwrite (id + 'Cannot open database file!');
      11: lwrite (id + 'Invalid database file!');
      12: cleanbase (10);
      Else lwrite (id + 'Undefined database error!');
    End;
    errcode := 0;
    THANKS_base := '';
  End;
  If exist (THANKS_list) Then initlist Else THANKS_list := '';
  If exist (THANKS_klst) Then initklst Else THANKS_klst := '';
  If logging Then Begin
    Assign (lfile, logfile);
{$I-}
    Append (lfile);
    If IOResult <> 0 Then Rewrite (lfile);
{$I+}
  End;
End;

Procedure done_THANKS;
Begin
  If pointers [5] Then Dispose (klistdb);
  If pointers [4] Then Dispose (nlistdb [2] );
  If pointers [3] Then Dispose (nlistdb [1] );
  If pointers [2] Then Dispose (fsizedb);
  If pointers [1] Then Dispose (crc32db);
  lwrite (id + 'Done!');
  lwrite ('');
  If logging Then Close (lfile);
End;

Procedure do_THANKS;
Begin
  level := 1;
  lwrite (id + 'Checking base directory');
  scanlevel;
  errcode := 0;
  If THANKS_base <> '' Then basekill;
  DosError := 0;
  If (THANKS_klst <> '') And (THANKS_base = '') Then klstkill;
  DosError := 0;
  If (THANKS_list <> '') And (THANKS_base = '') And (THANKS_klst = '') Then listkill;
  DosError := 0;
  If recursion Then Repeat
    Repeat Until Not downlevel;
    If level <> 1 Then Begin
      echostr := id + 'Checking ';
      For b1 := 2 To level Do echostr := echostr + inf [b1 - 1].Name + '\';
      lwrite (echostr);
      If THANKS_base <> '' Then basekill;
      DosError := 0;
      If (THANKS_klst <> '') And (THANKS_base = '') Then klstkill;
      DosError := 0;
      If (THANKS_list <> '') And (THANKS_base = '') And (THANKS_klst = '') Then listkill;
      DosError := 0;
      nextdir;
    End;
  Until level = 1;
End;

Begin
  header;
  If ParamCount > 2 Then err (4);
  init_THANKS;
  do_THANKS;
  done_THANKS;
End.

