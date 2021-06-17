Program bored_stiff;


(*

     Welcome to Therion's Archive Converter 1.80 Special Edition.

     Legal stuff first:

     Everything belonging to TAC, be it executables or source code, is
     (C)opyright 1992-1996 Nocturnal Productions. You are allowed to
     distribute the included archives as You wish, but You are NOT
     allowed to distribute recompiled or modified versions of ANY of
     the sources or programs related to TAC. You are allowed to modify
     the source for Your own purposes as long as You keep the modified
     sources and executables for Yourself.

 ** In other words: Spread hacks of my program and I'll get /really/ mad! **

     Nevertheless, this program is the result of four years of
     programming and testing, of which the last two has given the best
     results. Many people have helped me testing it along those years,
     so i hope that this release will spread TAC to a wider audience
     than before.

     My style is messy, so i have tried to clarify it by commenting
     this and that here and there. Hope You'll understand whatever
     went on in my head when i wrote /that/ particular routine.

     Enjoy!

*)


{$IFDEF MSDOS}
  {$M 8192,0,8192}
{$ELSE}
  {$M 16384}
{$ENDIF}

Uses
  {$IFDEF OS2}
  use32, os2base,
  {$ELSE}
  crt,
  {$ENDIF}
  DOS;

(*

     I certainly will not try to explain what all these constants and
     variables are good for, although i'm certain that i use them all =) ..

*)

Const

  arctypes: String [24] = 'ARCHPKARJLZHPAKTARUC2ZIP';
  arctyps: String [8] = 'AHJLPTUZ';

  dearctypes: String [144] = 'ARCHPKARJLZHPAKTARUC2ZIP' +
                             'GZ SITDWCMD LBRSQ SQZSH ' +
                             'CP HYPCHZLIMHAPHA GASZOO' +
                             'AR+RARAAFEX UUEXXEAR BIT' +
                             'HQXCPZCRUCPTPITSPLPP AIN' +
                             'MIMBLUCMTQ  SHPSHKQIPBIN';
  
  tempdir: String [13] = '$$TEMP$$.TAC';
  tempfil: String [13] = '$$TEMP$$.FIL';
  tempimg: String [13] = '$$TEMP$$.IMG';
  
  redir: String [6] = ' >nul';
  
  in_4DOS: Boolean = False;
  in_OS2: Boolean = False;
  
  batches: Boolean = False;
  clobber: Boolean = False;
  logging: Boolean = False;
  delafter: Boolean = False;
  striparc: Boolean = True;
  inteldet: Boolean = False;
  killfile: Boolean = False;
  lowercase: Boolean = False;
  quietmode: Boolean = False;
  virusscan: Boolean = False;
  recursion: Boolean = True;
  timestamp: Boolean = True;
  ExcludeEQ: Boolean = False;
  floppymode: Boolean = False;
  renameflag: Boolean = False;
  keepsmallest: Boolean = False;

  THANKSexe: String [10] = 'THANKS.EXE';

  TACcfg: String [7] = 'TAC.CFG';

  {$IFDEF MSDOS}

  id: String [6] = 'TAC þ ';

  bat1: String [8] = 'TAC1.BAT';
  bat2: String [8] = 'TAC2.BAT';
  bat3: String [8] = 'TAC3.BAT';

  {$ELSE}

  id: String [8] = 'TAC/2 þ ';

  bat1: String [8] = 'TAC1.CMD';
  bat2: String [8] = 'TAC2.CMD';
  bat3: String [8] = 'TAC3.CMD';
  
  {$ENDIF}
  
  verstr: String [4] = '0180';
  
Type
  archiver = Record
               filename,
               switches,
               rswitch,
               sswitch:
               {$IFDEF OS2}
               String;
               {$ELSE}
               String [32];
               {$ENDIF}
             End;
  
  dearchiver = Record
                 filename,
                 switches,
                 rswitch:
                 {$IFDEF OS2}
                 String;
                 {$ELSE}
                 String [32];
                 {$ENDIF}
               End;

  virscan = Record
              filename,
              switches:
              {$IFDEF OS2}
              String;
              {$ELSE}
              String [32];
              {$ENDIF}
            End;
  
Label skip;

Var
  Arc: Array [1..8] Of archiver;
  dearc: Array [1..48] Of dearchiver;
  vscan: virscan;
  
  IMG_ext: dearchiver;
  IMG_fil: dearchiver;
  
  stripzip: String;
  striparj: String;
  
  logfile,
  echostr: String;
  
  THANKS_parm: String;
  THANKS_sdir: String;
  
  i,
  o: File;
  
  ci: Text;
  
  lfile: Text;
  
  b1,
  b2: Byte;
  
  {$IFDEF OS2}
  OKey: KbdKeyInfo;
  
  w1,
  w2: LongInt;
  {$ELSE}
  w1,
  w2: Word;
  {$ENDIF}
  
  s1,
  s2,
  s3,
  ss1,
  ss2,
  ss3,
  sss,
  st2,
  st3,
  execstr: String;
  
  src: String;
  
  sourcetype: Byte;
  
  desttype: Char;
  
  seekptr: LongInt;
  
  c1,
  key: Char;
  
  p: Pointer;
  
  dirinfo: SearchRec;
  
  buf: Array [1..4] Of Char;
  bbuf: Array [1..4] Of Byte;
  
{$IFDEF MSDOS}

(*

     Say "Cheese!" =) ..

*)

Procedure headermsg;
Begin
  WriteLn;
  WriteLn ('Therion''s Archive Converter 1.80SE, (c) 1992-1996 Nocturnal Productions.');
  WriteLn;
End;

Procedure showsyntax;
Begin
  WriteLn ('Usage: TAC [-|/]<To-format>[switches] <filemask>');
  WriteLn;
  WriteLn ('Type TAC ? ? for format and switch descriptions');
  WriteLn;
End;

{$ELSE}

(*

     Say "Cheese!" in 32 Bit =) ..

*)

Procedure headermsg;
Begin
  WriteLn;
  WriteLn ('Therion''s Archive Converter /2 1.80SE, (c) 1992-1996 Nocturnal Productions.');
  WriteLn;
End;

Procedure showsyntax;
Begin
  WriteLn ('Usage: TAC2 [-|/]<To-format>[switches] <filemask>');
  WriteLn;
  WriteLn ('Type TAC2 ? ? for format and switch descriptions');
  WriteLn;
End;

{$ENDIF}

Procedure usage;
Begin
  {$IFDEF MSDOS}
  WriteLn ('Usage: TAC [-|/]<To-format>[switches] <filemask>');
  {$ELSE}
  WriteLn ('Usage: TAC2 [-|/]<To-format>[switches] <filemask>');
  {$ENDIF}
  WriteLn;
  WriteLn ('To-format is one of the following (preceded with a / or -):');
  WriteLn ('   A (Arc), H (Hpack), J (Arj), L (Lha), P (Pak), T (Tar), U (UC2) or Z (Zip).');
  WriteLn;
  WriteLn ('Switches:');
  WriteLn ('   b to execute ' + bat1 + ' before, ' + bat2 + ' during and ' + bat3 + ' after repacking.');
  WriteLn ('   c to delete file with same filename as archive to be created.');
  WriteLn ('   d to delete old files after conversion.');
  WriteLn ('   e to inhibit use of StripAV and StripARJ.');
  WriteLn ('   f to treat all input files as Floppy Disk images.');
  WriteLn ('   i to use intelligent detection instead of file extensions.');
  WriteLn ('   k to invoke THANKS before re-archiving.');
  {$IFDEF OS2}
  WriteLn ('   o to lowercase file extensions (relevant for HPFS only)');
  {$ENDIF}
  WriteLn ('   q to keep (almost) quiet.');
  WriteLn ('   r to inhibit recursion.');
  WriteLn ('   s to inhibit timestamps being set to latest file in archive.');
  WriteLn ('   v to scan for viruses before creating new archive.');
  WriteLn ('   w to enable logging.');
  WriteLn ('   x to skip those archives already in the specified toformat.');
End;

(*

     Routines to shut up if the user specified q, and to talk if the user
     didn't specify c.

*)

Procedure nuloutput;
Begin
  Assign (Output, 'NUL');
  Rewrite (Output);
End;

Procedure stdoutput;
Begin
  Assign (Output, '');
  Rewrite (Output);
End;

(*

     Checks for 4DOS to see if we can use redirection to STDERR and for OS/2
     to disallow use of the AIN dearchiver, which thrashes the good 'ole WARP.

*)

{$IFDEF MSDOS}
Procedure Test4DOS;
Begin
  Asm
    MOV    AX, 0D44DH
    MOV    BX, 0
    Int    2Fh
    CMP    AX, 44DDh
    JNE    @No_fdos
    mov    BX, CX
    mov    Word Ptr [w1], BX
    mov    AH, 51h
    mov    AL, 0
    Int    21h
    mov    Word Ptr [w2], BX
    push   DS
    mov    DS, Word Ptr [w2]
    mov    SI, 16h
    lodsw
    pop    DS
    mov    BX, AX
    mov    Word Ptr [w2], BX
    jmp    @getout
    @no_fdos:
    mov    Word Ptr [w1], 0
    mov    Word Ptr [w2], $ffff
    @getout:
  End;
  If w1 = w2 Then in_4DOS := True Else in_4DOS := False;
End;

Function OS2_GetVersion: Word; Assembler;
Asm
  MOV    AH, 30h
  Int    21h
  MOV    BH, AH
  XOr    AH, AH
  MOV    CL, 10
  Div    CL
  MOV    AH, BH
  XCHG   AH, AL
End;
{$ENDIF}

(*

     Simple WriteLn() replacement that writes a line to both the screen and
     optionally to the log file.

*)

Procedure lwrite (s: String);
Begin
  WriteLn (s);
  If logging Then WriteLn (lfile, s);
End;

(*

     The error handler.

*)

Procedure help (X: Byte);
Begin
  If quietmode Then stdoutput;
  Case X Of
    0: showsyntax;
    1: lwrite ('Invalid parameter(s) in command line!');
    2: lwrite ('Error reading from input file!');
    3: lwrite ('Please set Your COMSPEC environment variable!');
    5: lwrite ('Error changing directory!');
    7: lwrite ('Parameter error: More than one format specified!');
    8: lwrite ('Format not specified and virusscanner not defined!');
    10: lwrite ('Error unpacking archive, exiting!');
    11: lwrite ('Error packing archive, exiting!');
    13: lwrite ('Error deleting old archive, exiting!');
    15: lwrite ('Cannot find dearchiver, exiting!');
    16: lwrite ('Cannot find archiver, exiting!');
    23: lwrite ('Cannot find configuration file ' + s1 + ', exiting!');
    24: lwrite ('Invalid configuration file - please run TACSETUP!');
    30: lwrite ('Virus found inside archive! Take necessary precautions!');
    40: lwrite ('Cannot find Image dearchiver!');
    41: lwrite ('Cannot find Image filter!');
    43: lwrite ('Error filtering image, exiting!');
    44: lwrite ('Error unpacking image, exiting!');
    211: usage;
  End;
  {$I-}
  Close (lfile);
  Halt (X);
End;

(*

     KeyPressed() for OS/2 to avoid using CRT, which slows down screen output
     drastically. Well, at least it did back then. Don't need CRT anyway.

*)

{$IFDEF OS2}
Function KeyPressed: Boolean;
Begin
  KbdCharIn (OKey, io_NoWait, 0);
  KeyPressed := (OKey. fbStatus And kbdtrf_Final_Char_In) <> 0;
End;
{$ENDIF}

(*

     Simple batch-alike function.

*)

Function exist (filename: String): Boolean;
Var inf: SearchRec;
Begin
  FindFirst (filename, AnyFile, inf);
  exist := (DosError = 0);
  {$IFDEF OS2}
  FindClose (inf);
  {$ENDIF}
End;

(*

     Like FSearch(), but checks current directory as well. Could be smarter, i
     guess.

*)

Function findfile (filename: String): Boolean;
Begin
  If Not exist (filename) Then Begin
    sss := FSearch (filename, GetEnv ('PATH') );
    If sss = '' Then findfile := False Else findfile := True;
  End
  Else findfile := True;
End;

(*

     VERY primitive KILLDIR / DELTREE-alike routine, but it works, and it's
     fast enough for me.

*)

Procedure killdir (st1: String);
Var inf2: SearchRec;
Begin
  st1 := FExpand (st1);
  If exist (st1) Then Begin
    GetDir (0, st3);
    ChDir (st1);
    Repeat
      FindFirst ('*.*', $10, inf2);
      Repeat
        If (DosError = 0) And
           (inf2.Name [1] <> '.') And
           (inf2.Attr = $10)
        Then Begin
          ChDir (inf2.Name);
          FindFirst ('*.*', $10, inf2);
        End
        Else FindNext (inf2);
      Until DosError <> 0;
      FindFirst ('*.*', $27, inf2);
      While DosError = 0 Do Begin
        Assign (o, inf2.Name);
        SetFAttr (o, $20);
        Erase (o);
        FindNext (inf2);
      End;
      GetDir (0, st2);
      ChDir ('..');
      RmDir (st2);
      GetDir (0, st2);
    Until Length (st2) = Length (st3);
  End;
End;

(*

     Slave work; Read the configuration file.

*)

Procedure getcfg;
Begin
  s1 := ParamStr (0);
  While s1 [Length (s1) ] <> '\' Do Delete (s1, Length (s1), 1);
  s1 := s1 + TACcfg;
  Assign (ci, s1);
  {$I-}
  Reset (ci);
  {$I+}
  If IOResult <> 0 Then help (23)
  Else Begin
    ReadLn (ci, s2);
    If s2 <> verstr Then help (24);
    For b1 := 1 To 8 Do Begin
      ReadLn (ci, Arc [b1].filename);
      ReadLn (ci, Arc [b1].switches);
      ReadLn (ci, Arc [b1].rswitch);
      ReadLn (ci, Arc [b1].sswitch);
      ReadLn (ci, dearc [b1].filename);
      ReadLn (ci, dearc [b1].switches);
      ReadLn (ci, dearc [b1].rswitch);
    End;
    For b1 := 9 To 48 Do Begin
      ReadLn (ci, dearc [b1].filename);
      ReadLn (ci, dearc [b1].switches);
      ReadLn (ci, dearc [b1].rswitch);
    End;
    ReadLn (ci, vscan. filename);
    ReadLn (ci, vscan. switches);
    For b1 := 1 To 3 Do ReadLn (ci, stripzip);
    ReadLn (ci, THANKS_parm);
    ReadLn (ci, THANKS_sdir);
    ReadLn (ci, stripzip);
    ReadLn (ci, striparj);
    ReadLn (ci, IMG_ext. filename);
    ReadLn (ci, IMG_ext. switches);
    ReadLn (ci, IMG_ext. rswitch);
    ReadLn (ci, IMG_fil. filename);
    ReadLn (ci, IMG_fil. rswitch);
    ReadLn (ci, IMG_fil. switches);
    ReadLn (ci, logfile);
    Close (ci);
  End;
End;

Procedure cleanup;
Begin
  ChDir ('..');
  killdir (tempdir);
End;

(*

     This is where we execute the programs to remove AV information in ZIP
     archives and Security Envelopes in ARJ archives.

*)

Procedure callstrip;
Begin
  Case sourcetype Of
    2: If findfile (striparj) Then Begin
      lwrite (id + 'Stripping ARJ file ' + s2);
      SwapVectors;
      Exec (GetEnv ('COMSPEC'), '/C ' + striparj + ' ' + s2 + redir);
      SwapVectors;
    End;
    8: If findfile (stripzip) Then Begin
      lwrite (id + 'Stripping ZIP file ' + s2);
      SwapVectors;
      Exec (GetEnv ('COMSPEC'), '/C ' + stripzip + ' ' + s2 + redir);
      SwapVectors;
    End;
  End;
End;

(*

     Execute the Virus Scanner.

*)

Function scan: Byte;
Begin
  lwrite (id + 'Scanning for viruses');
  SwapVectors;
  Exec (GetEnv ('COMSPEC'), '/C ' + vscan. filename + ' ' + vscan. switches + redir);
  SwapVectors;
  scan := DosExitCode;
End;

(*

     Now the fun starts. This is where Floppy Disk Images are extracted. Both
     the converter and the extracter are needed, otherwise we bum out.

     TIFC is always executed on the images before extraction. If it cannot
     recognize the image format, it does no harm.

*)

Function unimage (fn: String): Byte;
Var ifn: String;
Begin
  If Not findfile (IMG_ext. filename) Then Begin
    unimage := 40;
    Exit;
  End;
  If Not findfile (IMG_fil. filename) Then Begin
    unimage := 41;
    Exit;
  End;
  If recursion Then ss2 := IMG_fil. rswitch
  Else ss2 := IMG_fil. switches;
  ss2 := ss2 + ' ' + fn + ' ' + tempimg;
  lwrite (id + 'Filtering ' + s2);
  execstr := '/c ' + IMG_fil. filename + ' ' + ss2 + redir;
  SwapVectors;
  Exec (GetEnv ('COMSPEC'), execstr);
  SwapVectors;
  If DosExitCode = 0 Then ifn := tempimg Else ifn := fn;
  MkDir (tempdir);
  If IOResult <> 0 Then;
  {$I-}
  ChDir (tempdir);
  {$I+}
  If IOResult <> 0 Then help (5);
  If recursion Then ss2 := IMG_ext. rswitch
  Else ss2 := IMG_ext. switches;
  ss2 := ss2 + ' ' + '..\' + ifn;
  lwrite (id + 'Extracting ' + s2);
  execstr := '/c ' + IMG_ext. filename + ' ' + ss2 + redir;
  SwapVectors;
  Exec (GetEnv ('COMSPEC'), execstr);
  SwapVectors;
  If DosExitCode <> 0 Then Begin
    cleanup;
    help (44);
  End;
  If ifn = tempimg Then Begin
    Assign (i, '..\' + tempimg);
    {$I-}
    Erase (i);
    {$I+}
  End;
  sourcetype := 251;
End;

(*

     Hey, wow!, now we're actually getting somewhere.

     This is the extraction routine. It could have been so easy to write, but
     i have added special handling to those formats who need it.

     - HPack, DWC and HAP all requires the files to be called .HPK, .DWC and
       .HAP, respectively.

     - .Z and .gz are both deleted by the dearchiver after extraction, and
       will be extracted to where the source archive is, no matter what.

     - cpio requires the archive to be piped into it.

     - PowerPacker requires a destination filename.

     - HAP requires a filemask to extract from the archive.

     - GZip, DWC, Squeeze and Comt all require the input file to be in the
       current directory.

     - The thing that is commented out is a leftover from my old StuffIt
       extractor that required human intervention.

     typ is obtained from Determine() (see later on) and fn is the filename to
     extract from.

     If Intelligent Detection is used, we tell the user what format we think
     the file is.

     Go go Gadget dearchiver! =) ..

*)

Function unpack (typ: Byte; fn: String): Byte;
Begin
  Case sourcetype Of
    2,
    11,
    21: renameflag := True;
  End;
  If typ <> 0 Then Begin
    If (typ <> 9) And (typ <> 11) And (typ <> 14) Then Begin
      If (Pos (':', fn) = 0) And (Pos ('\', fn) = 0) Then fn := '..\' + fn;
    End;
    ss1 := dearc [typ].filename;
    If Not findfile (ss1) Then Begin
      unpack := 255;
      w2 := 55552;
      Exit;
    End;
    If recursion Then ss2 := dearc [typ].rswitch
    Else ss2 := dearc [typ].switches;
    If sourcetype = 17 Then ss2 := ss2 + ' <';
    ss2 := ss2 + ' ' + fn;
    If sourcetype = 39 Then Begin
      sss := fn;
      While Pos (':', sss) > 0 Do Delete (sss, 1, 1);
      While Pos ('\', sss) > 0 Do Delete (sss, 1, 1);
      If Pos ('.', sss) > 0 Then Delete (sss, Pos ('.', sss), 3);
      ss2 := ss2 + ' ' + sss;
    End;
    {$I-}
    MkDir (tempdir);
    If IOResult <> 0 Then;
    ChDir (tempdir);
    {$I+}
    If IOResult <> 0 Then help (5);
    echostr := id + 'Uncompressing ' + s2;
    If inteldet Then Begin
      echostr := echostr + ' (';
      For b2 := 1 To 3 Do echostr := echostr + dearctypes [ ( (typ - 1) * 3) + b2];
      echostr := echostr + ')';
    End;
    lwrite (echostr);
    If (typ = 9) Or (typ = 11) Or (typ = 14) Or (typ = 43) Then Begin
      Assign (i, '..\' + fn);
      Reset (i, 1);
      Assign (o, fn);
      Rewrite (o, 1);
      GetMem (p, 4096);
      Repeat
        BlockRead (i, p^, 4096, w1);
        BlockWrite (o, p^, w1, w2);
      Until (w1 = 0) Or (w2 <> w1);
      FreeMem (p, 4096);
      Close (o);
      Close (i);
    End;
    If renameflag Then Begin
      sss := fn;
      Assign (i, fn);
      If typ <> 11 Then Delete (fn, 1, 3);
      {$IFDEF OS2}
      For w2 := Length (fn) Downto 1 Do If fn [w2] = '.' Then Break;
      Delete (fn, w2 + 1, Length (fn) - w2);
      {$ELSE}
      If Pos ('.', fn) > 1 Then Delete (fn, Pos ('.', fn) + 1, 3);
      {$ENDIF}
      w1 := ( (sourcetype - 1) * 3) + 1;
      For w2 := w1 To w1 + 2 Do fn := fn + dearctypes [w2];
      If typ <> 11 Then fn := '..\' + fn;

      If sss <> fn Then Rename (i, fn);

      Delete (ss2, Length (ss2) - Length (fn) + 1, Length (fn) + 1);
      ss2 := ss2 + fn;
    End;
    If typ = 21 Then ss2 := ss2 + ' *.* ';
    execstr := '/c ' + ss1 + ' ' + ss2;
    (*
    if typ<>10 then
    *)
    execstr := execstr + redir;
    SwapVectors;
    Exec (GetEnv ('COMSPEC'), execstr);
    SwapVectors;
    If (typ = 11) Or (typ = 14) Then Begin
      Assign (i, fn);
      Erase (i);
    End;
    unpack := DosExitCode;
  End
  Else unpack := 0;
End;

(*

     Here we call our archiver of choice.

     typ is the destination type, fn is the filename to create and the rest is
     just slave work.

*)

Function pack (typ: Char; fn: String): Byte;
Begin
  If sourcetype <> 0 Then Begin
    While Pos (':', fn) > 0 Do Delete (fn, 1, 1);
    While Pos ('\', fn) > 0 Do Delete (fn, 1, 1);
    If Pos ('.', fn) > 1 Then While fn [Length (fn) ] <> '.' Do Delete (fn, Length (fn), 1);
    If fn [Length (fn) ] <> '.' Then fn := fn + '.';
    b1 := Pos (typ, arctyps);
    For b2 := 1 To 3 Do fn := fn + arctypes [ ( (b1 - 1) * 3) + b2];
    {$IFDEF OS2}
    If Not lowercase Then For b2 := Length (fn) - 2 To Length (fn) Do fn [b2] := UpCase (fn [b2] )
    Else For b2 := Length (fn) - 2 To Length (fn) Do Begin
      If (fn [b2] >= 'A') And (fn [b2] <= 'Z') Then fn [b2] := Chr (Ord (fn [b2] ) + 32);
    End;
    While fn [Length (fn) ] = ' ' Do Delete (fn, Length (fn), 1);
    {$ENDIF}
    s3 := fn;
    fn := '..\' + fn;
    If exist (fn) Then Begin
      If Not clobber Then Begin
        ss1 := fn;
        Delete (ss1, 1, 3);
        If quietmode Then stdoutput;
        Write (id + ss1 + ' already exists. Overwrite? ');
        {$IFDEF MSDOS}
        Repeat key := ReadKey Until Pos (UpCase (key), 'YN') > 0;
        {$ELSE}
        Repeat Until (KeyPressed) And (Pos (UpCase (OKey. chchar), 'YN') > 0);
        key := OKey. chchar;
        {$ENDIF}
        WriteLn (key);
        If quietmode Then nuloutput;
        If UpCase (key) = 'Y' Then Begin
          Assign (o, fn);
          Rename (o, '..\' + tempfil);
        End;
      End
      Else Begin
        Assign (o, fn);
        Rename (o, '..\' + tempfil);
      End;
    End;
    If Not exist (fn) Then Begin
      ss1 := Arc [b1].filename;
      If Not findfile (ss1) Then Begin
        pack := 255;
        w2 := 55552;
        Exit;
      End;
      If recursion Then ss2 := Arc [b1].rswitch Else ss2 := Arc [b1].switches;
      If timestamp Then ss2 := ss2 + ' ' + Arc [b1].sswitch;
      ss2 := ss2 + ' ' + fn + ' *';
      {$IFDEF MSDOS}
      If desttype <> 'H' Then ss2 := ss2 + '.*';
      {$ENDIF}
      echostr := id + 'Creating ';
      For b1 := 4 To Length (fn) Do echostr := echostr + fn [b1];
      lwrite (echostr);
      SwapVectors;
      Exec (GetEnv ('COMSPEC'), '/c ' + ss1 + ' ' + ss2 + redir);
      SwapVectors;
      pack := DosExitCode;
    End
    Else Begin
      pack := 255;
      w2 := 61613;
    End;
  End
  Else pack := 0;
End;

(*

     Whoa! Here's the routine to determine the archive formats.

*)

Procedure determine (fn: String);

(*

     This routine scans for all the ASCii-based formats (well, some of them),
     all in one go.

     Stony Brook Pascal+ makes TAC die right here, as it does not like binary
     files opened and read as text files. Don't ask me why, but it does.

*)

Procedure scantxt;
Var
  sbuf,
  swork: String;
  bwork: Byte;
Begin
  Assign (ci, fn);
  Reset (ci);
  While (Not EoF (ci) ) And (sourcetype = 0) Do Begin
    ReadLn (ci, sbuf);
    swork := sbuf;
    While (swork [1] = ' ') And (Length (swork) > 0) Do Delete (swork, 1, 1);
    While swork [Length (swork) ] = ' ' Do Delete (swork, Length (swork), 1);
    If swork = 'FiLeStArTfIlEsTaRt' Then sourcetype := 48 Else Begin
      For bwork := 1 To Length (swork) Do swork [bwork] := UpCase (swork [bwork] );
      sbuf := swork;
      Delete (sbuf, 6, Length (sbuf) - 5);
      If swork = 'CONTENT-TRANSFER-ENCODING: BASE64' Then sourcetype := 41
      Else If sbuf = 'SHIP ' Then sourcetype := 45
      Else Begin
        Delete (swork, 7, Length (swork) - 6);
        If swork = 'BEGIN ' Then Begin
          ReadLn (ci, sbuf);
          If sbuf [1] = 'M' Then sourcetype := 29;
          If sbuf [1] = 'h' Then sourcetype := 30;
        End;
      End;
    End;
  End;
  Close (ci);
End;

(*

     .. And here we go. First we just check for the file extensions. This
     could probably be optimized, but .. erhm .. i had other things to do that
     day =) ..

*)

Begin
  renameflag := False;
  ss1 := fn;
  sourcetype := 0;
  If Not inteldet Then Begin
    While Pos ('.', ss1) <> 0 Do Begin
      While (ss1 [1] <> '.') And
            (Length (ss1) > 0)
      Do Delete (ss1, 1, 1);
      Delete (ss1, 1, 1);
    End;
    For b2 := 1 To Length (ss1) Do ss1 [b2] := UpCase (ss1 [b2] );
    If ss1 = 'ARC' Then sourcetype := 1;
    If ss1 = 'HPK' Then sourcetype := 2;
    If ss1 = 'ARJ' Then sourcetype := 3;
    If (ss1 = 'LHA') Or
       (ss1 = 'LZH') Or
       (ss1 = 'LZS') Or
       (ss1 = 'ICE') 
    Then sourcetype := 4;
    If ss1 = 'PAK' Then sourcetype := 5;
    If ss1 = 'TAR' Then sourcetype := 6;
    If ss1 = 'UC2' Then sourcetype := 7;
    If ss1 = 'ZIP' Then sourcetype := 8;
    If ss1 [Length (ss1) ] = 'Z' Then sourcetype := 9;
    If ss1 = 'SIT' Then sourcetype := 10;
    If ss1 = 'DWC' Then sourcetype := 11;
    If (ss1 = 'MD') Or
       (ss1 = 'CD') 
    Then sourcetype := 12;
    If ss1 = 'LBR' Then sourcetype := 13;
    If ss1 = 'SQZ' Then sourcetype := 15
    Else If ss1 [2] = 'Q' Then sourcetype := 14;
    If (ss1 [1] = 'S') And (ss1 [2] = 'H') Then sourcetype := 16;
    If ss1 = 'CP' Then sourcetype := 17;
    If ss1 = 'HYP' Then sourcetype := 18;
    If ss1 = 'CHZ' Then sourcetype := 19;
    If ss1 = 'LIM' Then sourcetype := 20;
    If ss1 = 'HAP' Then sourcetype := 21;
    If ss1 = 'HA' Then sourcetype := 22;
    If ss1 = 'GAS' Then sourcetype := 23;
    If ss1 = 'ZOO' Then sourcetype := 24;
    If ss1 = 'ARP' Then sourcetype := 25;
    If ss1 = 'RAR' Then sourcetype := 26;
    If ss1 = 'AAF' Then sourcetype := 27;
    If ss1 = 'EX' Then sourcetype := 28;
    If (ss1 = 'UUE') Or
       (ss1 = 'UU') 
    Then sourcetype := 29;
    If (ss1 = 'XXE') Or
       (ss1 = 'XX') 
    Then sourcetype := 30;
    If ss1 = 'AR' Then sourcetype := 31;
    If ss1 = 'BIT' Then sourcetype := 32;
    If (ss1 = 'HQX') Or
       (ss1 = 'HEX') Or
       (ss1 = 'HCX') 
    Then sourcetype := 33;
    If ss1 = 'CPZ' Then sourcetype := 34;
    If ss1 = 'CRU' Then sourcetype := 35;
    If ss1 = 'CPT' Then sourcetype := 36;
    If ss1 = 'PIT' Then sourcetype := 37;
    If ss1 = 'SPL' Then sourcetype := 38;
    If ss1 = 'PP' Then sourcetype := 39;
    If ss1 = 'AIN' Then sourcetype := 40;
    If ss1 = 'MIM' Then sourcetype := 41;
    If ss1 = 'BLU' Then sourcetype := 42;
    If ss1 = 'CMT' Then sourcetype := 43;
    If ss1 = 'Q' Then sourcetype := 44;
    If ss1 = 'SHP' Then sourcetype := 45;
    If ss1 = 'SHK' Then sourcetype := 46;
    If ss1 = 'QIP' Then sourcetype := 47;
    If ss1 = 'BIN' Then sourcetype := 48;
  End

(*

     And here is where we "taste" the files and try to match one of the
     fingerprints known to us with whatever can be found inside the archive.

*)

  Else Begin
    FileMode := 0;
    Assign (i, fn);
    {$I-}
    Reset (i, 1);
    If IOResult <> 0 Then help (2);
    BlockRead (i, buf, 4);
    
    If buf = 'HPAK' Then sourcetype := 2;
    
    If buf = 'UC2'#$1a Then sourcetype := 7;
    
    If (buf [1] = #$60) And (buf [2] = #$ea) Then sourcetype := 3;
    
    If (buf [3] = '-') And (buf [4] = 'l') Then sourcetype := 4;

    If (buf [1] = #$1a) Then Begin
      If buf [2] <= #$0b Then sourcetype := 5;
      If buf [2] = #8 Then sourcetype := 1;
      If buf [2] = #$14 Then sourcetype := 25;
      If buf [2] = #$48 Then sourcetype := 18;
    End;
    
    If buf = 'PK'#3#4 Then sourcetype := 8;
    
    If (buf [1] = #$1f) And (buf [2] = #$9D) Then sourcetype := 9;
    
    If (buf [1] = #$1f) And (buf [2] = #$8b) Then sourcetype := 9;
    
    If buf = 'MDmd' Then sourcetype := 12;
    
    If (buf [1] = #$76) And (buf [2] = #$ff) Then sourcetype := 14;
    
    If buf = 'HLSQ' Then sourcetype := 15;
    
    If (buf [1] = #$c7) And (buf [2] = #$71) Then sourcetype := 17;
    
    If (buf [1] = #$4c) And (buf [2] = #$4D) And (buf [3] = #$1a) Then sourcetype := 20;
    
    If buf = #$91'3HF' Then sourcetype := 21;
    
    If buf = #$52#$45#$7e#$5e Then sourcetype := 26;

    If buf = 'Rar!' Then sourcetype := 26;
    
    If buf = '!<ar' Then sourcetype := 31;
    
    If buf = #1#0#0#0 Then sourcetype := 34;
    
    If buf = 'PIT ' Then sourcetype := 36;
    
    If (buf [1] = #$93) And (buf [2] = #$b9) Then sourcetype := 37;
    
    If buf = #1#1#$84#$5D Then sourcetype := 38;
    
    If buf = 'PP20' Then sourcetype := 39;
    
    If (buf [1] = #$21) And (buf [2] = #$12) Then sourcetype := 40;
    
    If (buf [1] = #10) And (buf [2] = #71) And (buf [3] = #76) Then sourcetype := 42;
    
    If buf = #$44#$53#$00#$5D Then sourcetype := 44;
    
    If buf = #$4e#$f5#$46#$e9 Then sourcetype := 46;
    
    If (buf [1] = 'Q') And (buf [2] = 'P') Then sourcetype := 47;
    
    If buf = 'ENC.' Then Begin
      BlockRead (i, buf, 4);
      If buf = 'COM.' Then Begin
        BlockRead (i, buf, 4);
        If buf = 'B&F=' Then sourcetype := 43;
      End;
    End;
    
    Seek (i, 0);
    BlockRead (i, buf, 4);
    
    If (buf [1] = '#') And (buf [2] = '!') Then Begin
      Seek (i, 2);
      BlockRead (i, buf, 4);
      If Pos ('/bi', buf) > 0 Then sourcetype := 16;
    End;
    
    Seek (i, 0);
    BlockRead (i, buf, 4);
    
    If buf = '(Thi' Then Begin
      Seek (i, 46);
      BlockRead (i, buf, 2);
      If (buf [1] = ':') Or (buf [2] = ':') Then sourcetype := 33;
    End;
    
    Seek (i, 0);
    BlockRead (i, buf, 4);
    
    If (buf [1] = 'H') And (buf [2] = 'A') Then Begin
      BlockRead (i, buf, 1);
      If (Ord (buf [1] ) And $fc = $20) Then sourcetype := 22;
    End;
    
    Seek (i, 0);
    BlockRead (i, buf, 4);
    
    If buf = 'CRUS' Then Begin
      BlockRead (i, buf, 3);
      If buf = 'H vS' Then Begin
        Seek (i, 10);
        BlockRead (i, buf, 2);
        If (buf [1] = #$0a) And (buf [2] = #$1a) Then sourcetype := 35;
      End;
    End;
    
    If sourcetype = 0 Then Begin
      
      If sourcetype = 0 Then Begin
        Seek (i, FileSize (i) - 3);
        If IOResult = 0 Then Begin
          ss1 [0] := #3;
          BlockRead (i, ss1 [1], 3);
          If (IOResult = 0) And (ss1 = 'DWC') Then Begin
            sourcetype := 11;
            ss1 := fn;
            While (ss1 [1] <> '.') And (Length (ss1) > 0) Do Delete (ss1, 1, 1);
            Delete (ss1, 1, 1);
          End;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 20);
        If IOResult = 0 Then Begin
          ss1 [0] := #4;
          BlockRead (i, ss1 [1], 4);
          If (IOResult = 0) And (ss1 = #$dc#$a7#$c4#$fd) Then sourcetype := 24;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 65);
        If IOResult = 0 Then Begin
          ss1 [0] := #8;
          BlockRead (i, ss1 [1], 8);
          If (IOResult = 0) And ( (ss1 = 'SIT!SIT!') Or (ss1 = 'SITDSIT!') ) Then sourcetype := 10;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 36);
        If IOResult = 0 Then Begin
          ss1 [0] := #9;
          BlockRead (i, ss1 [1], 9);
          If (IOResult = 0) And (buf = 'LHA''s SFX') Then sourcetype := 4;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 32);
        If IOResult = 0 Then Begin
          ss1 [0] := #12;
          BlockRead (i, ss1 [1], 12);
          If (IOResult = 0) And (ss1 = 'SFX by LARC ') Then sourcetype := 4;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 526);
        If IOResult = 0 Then Begin
          ss1 [0] := #12;
          BlockRead (i, ss1 [1], 12);
          If (IOResult = 0) And (ss1 = 'SFX by LARC ') Then sourcetype := 4;
        End;
      End;

(*

     This is the "new and improved" SFX handler. Usually, an SFX is nothing
     but an EXE file with the archive itself appended as an overlay. The only
     case where this is not true are LARC (both EXE and COM files) and old
     LHArc COM files.

*)

      If sourcetype = 0 Then Begin
        Seek (i, 2);
        BlockRead (i, bbuf, 4);
        seekptr := ( (bbuf [4] * 256) + bbuf [3] );
        seekptr := (seekptr - 1) * 512;
        seekptr := seekptr + (bbuf [2] * 256) + bbuf [1];
        Seek (i, seekptr);
        If IOResult = 0 Then Begin
          BlockRead (i, buf, 4);
          If IOResult = 0 Then Begin
            If (buf [1] = #$60) And (buf [2] <= #$ea) Then sourcetype := 3;
            If (buf [1] = #$1a) And (buf [2] <= #$0b) Then sourcetype := 5;
            If buf = 'PK'#3#4 Then sourcetype := 8;
            If buf = 'HLSQ' Then sourcetype := 15;
            If buf = 'Rar!' Then sourcetype := 26;
          End;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 0);
        ss1 [0] := #16;
        BlockRead (i, ss1 [1], 16);
        If IOResult = 0 Then Begin
          If (ss1 [15] <> #0) Or (ss1 [16] <> #0) Then Begin
            Delete (ss1, 15, 2);
            If ss1 = #0'           '#0#0 Then sourcetype := 13;
          End;
        End;
      End;
      
      If sourcetype = 0 Then Begin
        Seek (i, 257);
        If IOResult = 0 Then Begin
          ss1 [0] := #5;
          BlockRead (i, ss1 [1], 5);
          If (IOResult = 0) And (ss1 = 'ustar') Then sourcetype := 6;
        End;
      End;
      
    End;
    Close (i);
    {$I+}
  End;
  If sourcetype = 0 Then lwrite (id + fn + ' - cannot determine archive format. Skipping.');
  FileMode := 2;
End;

(*

     This is where we call THANKS. Errorlevels are logged but not reacted
     upon.

*)

Procedure killfiles;
Begin
  If findfile (THANKSexe) Then Begin
    If logging Then THANKS_parm := THANKS_parm + 'W';
    st3 := THANKSexe + ' ' + THANKS_parm + redir;
    lwrite (id + 'Invoking THANKS');
    If logging Then Close (lfile);
    SwapVectors;
    Exec (GetEnv ('COMSPEC'), '/C ' + st3);
    SwapVectors;
    If logging Then Append (lfile);
    lwrite (id + 'Returned from THANKS');
    w2 := DosExitCode;
    Str (w2, st3);
    If w2 <> 0 Then lwrite (id + 'THANKS reported error ' + st3 + '!'#13#10);
  End;
End;

(*

     Command-line parser. Sets a whole bunch of flags.

*)

Procedure parsecmdline;
Begin
  If ParamCount <> 2 Then help (0);
  desttype := ' ';
  s1 := ParamStr (1);
  If Pos (s1 [1], '/-') > 0 Then Delete (s1, 1, 1);
  If Length (s1) > 15 Then help (1);
  If Length (s1) = 0 Then help (1);
  For b1 := 1 To Length (s1) Do Begin
    c1 := UpCase (s1 [b1] );
    {$IFDEF MSDOS}
    If Pos (c1, 'ABCDEFHIJKLMPQRSTUVWXZ?') = 0 Then help (1);
    {$ELSE}
    If Pos (c1, 'ABCDEFHIJKLMOPQRSTUVWXZ?') = 0 Then help (1);
    {$ENDIF}
    If c1 = 'B' Then batches := True
    Else If c1 = 'C' Then clobber := True
    Else If c1 = 'D' Then delafter := True
    Else If c1 = 'E' Then striparc := False
    Else If c1 = 'F' Then floppymode := True
    Else If c1 = 'I' Then inteldet := True
    Else If c1 = 'K' Then killfile := True
    Else If c1 = 'M' Then keepsmallest := True
    {$IFDEF OS2}
    Else If c1 = 'O' Then lowercase := True
    {$ENDIF}
    Else If c1 = 'Q' Then quietmode := True
    Else If c1 = 'R' Then recursion := False
    Else If c1 = 'S' Then timestamp := False
    Else If c1 = 'V' Then virusscan := True
    Else If c1 = 'W' Then logging := True
    Else If c1 = 'X' Then ExcludeEQ := True
    Else If c1 = '?' Then help (211)
    Else Begin
      If desttype = ' ' Then desttype := c1 Else help (7);
    End;
  End;
  If keepsmallest Then delafter := True;
  If vscan. filename = '' Then virusscan := False;
  If logfile = '' Then logging := False;
End;

(*

     Main thingie.

*)

Begin
  If GetEnv ('COMSPEC') = '' Then help (3);

(*

     Fix the redirection string.

*)

  {$IFDEF MSDOS}
  Test4DOS;
  If in_4DOS Then redir := ' >&nul';
  If OS2_GetVersion >= $0100 Then in_OS2 := True;
  {$ELSE}
  redir := ' >&nul';
  {$ENDIF}
  headermsg;
  getcfg;
  parsecmdline;

(*

     Open the log file if necessary.

*)

  If logging Then Begin
    s1 := ParamStr (0);
    While s1 [Length (s1) ] <> '\' Do Delete (s1, Length (s1), 1);
    s1 := s1 + logfile;
    Assign (lfile, s1);
    {$I-}
    Append (lfile);
    If IOResult <> 0 Then Rewrite (lfile);
    src := #13#10 + id + '°±²Û Began session with parameters ' + ParamStr (1) + ' and ' + ParamStr (2) + ' Û²±°'#13#10;
    WriteLn (lfile, src);
    {$I+}
  End;
  s1 := ParamStr (2);
  src := s1;
  While (Pos (src [Length (src) ], ':\') = 0) And (Length (src) > 0) Do Delete (src, Length (src), 1);
  FindFirst (s1, $21, dirinfo);

(*

     This is sadly necessary, as DOS cannot handle if You convert *.ZIP to
     ZIP. It will convert the first archive twice. I got an explanation once,
     but cannot remember it. Something about DOS's DTA.

     Theory:

     Any archiver leaves the archive bit set after creating an archive.

     So we simply turn off all the archive bits and only handle those archives
     without archive bits.

     Yeah, well, a dirty hack, but blame DOS for it!

*)

  If DosError = 0 Then Begin
    lwrite (id + 'Touching file attributes');
    lwrite ('');
    While DosError = 0 Do Begin
      Assign (i, src + dirinfo. Name);
      GetFAttr (i, w2);
      If w2 And $20 = $20 Then w2 := w2 - $20;
      SetFAttr (i, w2);
      FindNext (dirinfo);
    End;
  End;
  {$IFDEF OS2}
  FindClose (dirinfo);
  {$ENDIF}
  FindFirst (s1, $21, dirinfo);

(*

     If no destination type, then it's either virusscanning or nothing.

*)

  c1 := desttype;
  If c1 = ' ' Then Begin
    c1 := '!';
    If vscan. filename = '' Then help (8);
    virusscan := True;
  End;
  While DosError = 0 Do Begin
    If dirinfo. Attr And $20 = 0 Then Begin

(*

     Call TAC1.BAT / TAC1.CMD if found

*)

      If (batches) And (FSearch (bat1, '.;' + GetEnv ('PATH') ) <> '') Then Begin
        lwrite (id + 'Executing ' + bat1);
        SwapVectors;
        Exec (GetEnv ('COMSPEC'), '/C ' + bat1 + ' ' + src + dirinfo. Name);
        SwapVectors;
      End;
      If quietmode Then nuloutput;
      s2 := src + dirinfo. Name;
      {$IFDEF MSDOS}
      For w2 := 1 To Length (s2) Do s2 [w2] := UpCase (s2 [w2] );
      {$ENDIF}
      w2 := 0;

(*

     If not handling Floppy Disk Images, we've gotta find out what format the
     source archive is in.

*)

      If Not floppymode Then Begin
        determine (s2);
        If (ExcludeEQ) And (desttype = arctyps [sourcetype] ) Then Begin
          lwrite (id + 'Skipped file ' + s2);
          Goto skip;
        End;
        If (in_OS2) And (sourcetype = 40) Then Begin
          lwrite (id + 'Will not call AIN under OS/2, skipping!');
          Goto skip;
        End;
      End;

(*

     Call StripZIP or StripARJ if told to.

*)

      If striparc Then callstrip;

(*

     And extract whatever we're handling.

*)

      Case floppymode Of
        True:
              Begin
                Case unimage (s2) Of
                  40: help (40);
                  41: help (41);
                End;
              End;
        False:
               Begin
                 If unpack (sourcetype, s2) <> 0 Then Begin
                   If w2 = 55552 Then help (15) Else Begin
                     cleanup;
                     help (10);
                   End;
                 End;
               End;
      End;

(*

     Call THANKS.

*)

      If (killfile) And (c1 <> '!') And (sourcetype <> 0) Then killfiles;

(*

     Call the virusscanner.

*)

      If (virusscan) And (sourcetype <> 0) And (scan <> 0) Then Begin
        cleanup;
        help (30);
      End;

(*

     Call TAC2.BAT / TAC2.CMD if found.

*)

      If (batches) And (sourcetype <> 0) And (FSearch (bat2, '..;' + GetEnv ('PATH') ) <> '') Then Begin
        lwrite (id + 'Executing ' + bat2);
        SwapVectors;
        If exist ('..\' + bat2) Then Exec (GetEnv ('COMSPEC'), '/C ..\' + bat2)
        Else Exec (GetEnv ('COMSPEC'), '/C ' + bat2);
        SwapVectors;
      End;

(*

     If not virusscanning only, pack whatever we've unpacked.

*)

      If c1 <> '!' Then Begin
        w2 := 0;
        If pack (desttype, s2) <> 0 Then Begin
          cleanup;
          If w2 <> 61613 Then Begin
            If exist (tempfil) Then Begin
              Assign (i, tempfil);
              Rename (i, s2);
            End;
            If w2 = 55552 Then help (16) Else help (11);
          End;
        End;
      End;
      If (sourcetype <> 0) And (w2 <> 61613) Then Begin
        cleanup;
        If exist (tempfil) Then Begin
          Assign (i, tempfil);
          SetFAttr (i, 0);
          Erase (i);
        End;
        echostr := id + 'Finished ';
        If c1 = '!' Then echostr := echostr + 'scanning' Else echostr := echostr + 'conversion';
        echostr := echostr + ' of ' + s2;
        lwrite (echostr);

(*

     Delete the smallest file if told to. Otherwise delete the old archive.

*)

        If (delafter) And (desttype <> '!') And (s2 <> s3) Then Begin
          Assign (i, s3);
          Reset (i, 1);
          If (keepsmallest) And (FileSize (i) > dirinfo. Size) Then Begin
            Close (i);
            Erase (i);
            lwrite (id + 'New file larger than old. Deleted!');
          End
          Else Begin
            Close (i);
            {$I-}
            Assign (i, src + dirinfo. Name);
            SetFAttr (i, 0);
            Erase (i);
            {$I+}
            If IOResult <> 0 Then help (13)
            Else lwrite (id + 'Deleted old file ' + s2);
          End;
        End;

(*

     Execute TAC3.BAT / TAC3.CMD if found.

*)

        If (batches) And (sourcetype <> 0) And (exist (s3) ) And (FSearch (bat3, '.;' + GetEnv ('PATH') ) <> '') Then Begin
          lwrite (id + 'Executing ' + bat3);
          SwapVectors;
          Exec (GetEnv ('COMSPEC'), '/C ' + bat3 + ' ' + s3);
          SwapVectors;
        End;
      End;
      lwrite ('');
    End;
    skip:
    FindNext (dirinfo);
  End;
  {$IFDEF OS2}
  FindClose (dirinfo);
  {$ENDIF}

(*

     We've done our job, Thank You and Good Night!

*)

  If logging Then Close (lfile);
End.

