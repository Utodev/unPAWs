PROGRAM UnPAWs;
uses
  SysUtils, Strings, Dos, Z80Load;

(***************************************************************************)
(*                                                                         *)
(*  Professional Adventure Writing System extractor.                       *)
(*  Original vesion: Jose Luis Cebrian Pague                               *)
(*  Pascal version : Carlos Sanchez                                        *)
(*  128K and graphics support: Alexander Katz (sasha@kats.pp.kiev.ua)      *)
(*                                                                         *)
(***************************************************************************)
(*                                                                         *)
(*  Z80 files support and other minor changes added by Carlos Sanchez      *)
(*  when translating the code from C to pascal                             *)
(*                                                                         *)
(***************************************************************************)
(*  For future changes see WHATSNEW.TXT                                    *)
(***************************************************************************)

{$I-,S-,R-}

CONST Version = '2.1';
      Copyright='(c) 2002,2003 Alexander Katz';
      Product = 'UNPAWS';
      MAXTVOC=6;


VAR Spectrum : ^SpectrumMemory;
    Pages: SpectrumPages;
    Locs: Array [0..7] of Integer;
    Mess: Array [0..7] of Integer;
    PagesUsed:Byte;
    FOut : Text;
    F, Tape : File;
    FSize : Longint;
    VocPtr, ConPtr, ProPtr, ResPtr : Word;
    i, j, n, p : Integer;
    InputFileName, OutputFileName, OutputTapeName : String;
    InputName, InputExt, SnapType: string;
    OffAbreviations: Word;
    OffVoc, offcon, OffMsg, OffSys, OffLoc, OffObj, OffWObj,
    OffLObj, OffXObj, OffPro, OffResp, OffFont, OffGraph, OffGraphAttr: Word;
    MainTop, MainAttr: Word;
    tOffs: Word;
    NumLoc, NumMsg, NumSys, NumObj, NumPro, NumFonts : Integer ;
    NumCurr: Integer;
    AbrevOn, FontsOn, GraphOn:Boolean;
    ExpandTokens:boolean;
    compressed:boolean;
    M128K,Basic128: Boolean;
    snappage,tmpb, ColorCodes: Byte;
    LastPrintedChar:Byte;
    S:String;
    QuillVersion: Word;
    Patched: Boolean;
    opcodetype: Boolean;
    opcode:Integer;
    GraphCount: Integer;
    tempint: Integer;
    XPos: Word;

TYPE COND= Record
            Condact:String[20];
            Params:Integer
           End;


CONST Condacts:ARRAY [0..107] OF COND=(
(Condact:'AT';Params:1),
(Condact:'NOTAT';Params:1),
(Condact:'ATGT';Params:1),
(Condact:'ATLT';Params:1),
(Condact:'PRESENT';Params:1),
(Condact:'ABSENT';Params:1),
(Condact:'WORN';Params:1),
(Condact:'NOTWORN';Params:1),
(Condact:'CARRIED';Params:1),
(Condact:'NOTCARR';Params:1),
(Condact:'CHANCE';Params:1),
(Condact:'ZERO';Params:1),
(Condact:'NOTZERO';Params:1),
(Condact:'EQ';Params:2),
(Condact:'GT';Params:2),
(Condact:'LT';Params:2),
(Condact:'ADJECT1';Params:1),
(Condact:'ADVERB';Params:1),
(Condact:'INVEN';Params:0),
(Condact:'DESC';Params:0),
(Condact:'QUIT';Params:0),
(Condact:'END';Params:0),
(Condact:'DONE';Params:0),
(Condact:'OK';Params:0),
(Condact:'ANYKEY';Params:0),
(Condact:'SAVE';Params:0),
(Condact:'LOAD';Params:0),
(Condact:'TURNS';Params:0),
(Condact:'SCORE';Params:0),
(Condact:'CLS';Params:0),
(Condact:'DROPALL';Params:0),
(Condact:'AUTOG';Params:0),
(Condact:'AUTOD';Params:0),
(Condact:'AUTOW';Params:0),
(Condact:'AUTOR';Params:0),
(Condact:'PAUSE';Params:1),
(Condact:'TIMEOUT';Params:0),
(Condact:'GOTO';Params:1),
(Condact:'MESSAGE';Params:1),
(Condact:'REMOVE';Params:1),
(Condact:'GET';Params:1),
(Condact:'DROP';Params:1),
(Condact:'WEAR';Params:1),
(Condact:'DESTROY';Params:1),
(Condact:'CREATE';Params:1),
(Condact:'SWAP';Params:2),
(Condact:'PLACE';Params:2),
(Condact:'SET';Params:1),
(Condact:'CLEAR';Params:1),
(Condact:'PLUS';Params:2),
(Condact:'MINUS';Params:2),
(Condact:'LET';Params:2),
(Condact:'NEWLINE';Params:0),
(Condact:'PRINT';Params:1),
(Condact:'SYSMESS';Params:1),
(Condact:'ISAT';Params:2),
(Condact:'COPYOF';Params:2),
(Condact:'COPYOO';Params:2),
(Condact:'COPYFO';Params:2),
(Condact:'COPYFF';Params:2),
(Condact:'LISTOBJ';Params:0),
(Condact:'EXTERN';Params:1),
(Condact:'RAMSAVE';Params:0),
(Condact:'RAMLOAD';Params:1),
(Condact:'BEEP';Params:2),
(Condact:'PAPER';Params:1),
(Condact:'INK';Params:1),
(Condact:'BORDER';Params:1),
(Condact:'PREP';Params:1),
(Condact:'NOUN2';Params:1),
(Condact:'ADJECT2';Params:1),
(Condact:'ADD';Params:2),
(Condact:'SUB';Params:2),
(Condact:'PARSE';Params:0),
(Condact:'LISTAT';Params:1),
(Condact:'PROCESS';Params:1),
(Condact:'SAME';Params:2),
(Condact:'MES';Params:1),
(Condact:'CHARSET';Params:1),
(Condact:'NOTEQ';Params:2),
(Condact:'NOTSAME';Params:2),
(Condact:'MODE';Params:2),
(Condact:'LINE';Params:1),
(Condact:'TIME';Params:2),
(Condact:'PICTURE';Params:1),
(Condact:'DOALL';Params:1),
(Condact:'PROMPT';Params:1),
(Condact:'GRAPHIC';Params:1),
(Condact:'ISNOTAT';Params:2),
(Condact:'WEIGH';Params:2),
(Condact:'PUTIN';Params:2),
(Condact:'TAKEOUT';Params:2),
(Condact:'NEWTEXT';Params:0),
(Condact:'ABILITY';Params:2),
(Condact:'WEIGHT';Params:1),
(Condact:'RANDOM';Params:1),
(Condact:'INPUT';Params:1),
(Condact:'SAVEAT';Params:0),
(Condact:'BACKAT';Params:0),
(Condact:'PRINTAT';Params:2),
(Condact:'WHATO';Params:0),
(Condact:'RESET';Params:1),
(Condact:'PUTO';Params:1),
(Condact:'NOTDONE';Params:0),
(Condact:'AUTOP';Params:1),
(Condact:'AUTOT';Params:1),
(Condact:'MOVE';Params:1),
(Condact:'PROTECT';Params:0));

CONST tVocs:ARRAY [0..6] OF String=(
'Verb',
'Adverb',
'Noun',
'Adjective',
'Preposition',
'Conjunction',
'Pronoun');

CONST Tokens:ARRAY [163..255] of String=(
' SPECTRUM ',
' PLAY ',
'RND',
'INKEY$',
'PI',
'FN ',
'POINT ',
'SCREEN$ ',
'ATTR ',
'AT ',
'TAB ',
'VAL$ ',
'CODE ',
'VAL ',
'LEN ',
'SIN ',
'COS ',
'TAN ',
'ASN ',
'ACS ',
'ATN ',
'LN ',
'EXP ',
'INT ',
'SQR ',
'SGN ',
'ABS ',
'PEEK ',
'IN ',
'USR ',
'STR$ ',
'CHR$ ',
'NOT ',
'BIN ',
' OR ',
' AND ',
'<=',
'>=',
'<>',
' LINE ',
' THEN ',
' TO ',
' STEP ',
' DEF FN ',
' CAT ',
' FORMAT ',
' MOVE ',
' ERASE ',
' OPEN #',
' CLOSE #',
' MERGE ',
' VERIFY ',
' BEEP ',
' CIRCLE ',
' INK ',
' PAPER ',
' FLASH ',
' BRIGHT ',
' INVERSE ',
' OVER ',
' OUT ',
' LPRINT ',
' LLIST ',
' STOP ',
' READ ',
' DATA ',
' RESTORE ',
' NEW ',
' BORDER ',
' CONTINUE ',
' DIM ',
' REM ',
' FOR ',
' GO TO ',
' GO SUB ',
' INPUT ',
' LOAD ',
' LIST ',
' LET ',
' PAUSE ',
' NEXT ',
' POKE ',
' PRINT ',
' PLOT ',
' RUN ',
' SAVE ',
' RANDOMIZE ',
' IF ',
' CLS ',
' DRAW ',
' CLEAR ',
' RETURN ',
' COPY ');

CONST plot_moves: array[0..7] of String = (
' 001  000',
' 001  001',
' 000  001',
'-001  001',
'-001  000',
'-001 -001',
' 000 -001',
' 001 -001' );

PROCEDURE Error(n:Byte);
Var i:integer;
BEGIN
 IF N<>0 THEN Write('Error: ');
 CASE N OF
  1:WriteLn('File not found.');
  2:WriteLn('Must be a .SP, .SNA or .Z80 file.');
  3:WriteLn('It doesn''t seem to be a Quill/PAW game.');
  4:WriteLn('Parameter missing.');
  5:WriteLn('The .Z80 format version of this file is not valid.');
  6:WriteLn('The .Z80 file is not a ZX 48K/128K snapshot.');
  7:Writeln('Invalid address was referred.');
 END;
 if m128k then for i:=0 to 7 do if Pages[i]<>nil then FreeMem(Pages[i],16384);
 if Spectrum<>nil then FreeMem(Spectrum,49152);
 Halt(n)
END;

FUNCTION DPeek(C:Word):Word;
BEGIN
 if (c<16384) or (c>65534)Then Error(7);
 DPeek := spectrum^[c] OR (spectrum^[c+1] SHL 8);
END;

FUNCTION Peek(C:Word):Byte;
BEGIN
 if c<16384 Then Error(7);
 Peek := Spectrum^[c];
END;

FUNCTION PeekNeg(C:Word):Byte;
BEGIN
 PeekNeg := Spectrum^[c] XOR 255;
END;

Procedure SetPage(num: byte);
begin
 move(Pages[num]^,Spectrum^[49152],16384);
 snappage:=num;
end;

Procedure SetNextPage;
begin
 Inc(snappage);
 if snappage in [2,5] then inc(snappage);
 if snappage=8 then snappage:=0;
 SetPage(snappage);
end;

FUNCTION Select(Option:Boolean;S1,S2:String):String;
(* Emulates ? operator (C language) *)
BEGIN
 IF Option THEN Select:=S1
           ELSE Select:=S2;
END;

FUNCTION HEX(value:Byte):String;
var
 S:string;
 v:byte;
BEGIN
 S:='0x  ';
 v:=value div 16;
 if v<10 then v:=v+ord('0') else v:=v-10+ord('a');
 S[3]:=chr(v);
 v:=value mod 16;
 if v<10 then v:=v+ord('0') else v:=v-10+ord('a');
 S[4]:=chr(v);
 Hex:=S;
END;

FUNCTION Justify(S : String; N: Byte) : String;
BEGIN
 WHILE Length(S)<N DO S:=S+' ';
 JustiFy := S;
END;

FUNCTION IntToStr(L:Longint):String;
VAR S : String;
BEGIN
 Str(L,S);
 IntToStr := S
END;

FUNCTION IntToStr2(L:Longint; width: integer; zeros: boolean):String;
VAR S : String;
    i: integer;
BEGIN
 Str(L:width,S);
 if zeros then
 begin
 for i:=length(S) downto 1 do
  if (s[i]=' ') or (s[i]='-') then s[i]:='0';
 end;
 if l<0 then s[1]:='-';
 IntToStr2 := S
END;


FUNCTION Abreviation(c:Integer) : String;
VAR Offs: Word;
    S:String;
    oldpage: byte;
BEGIN
 oldpage:=snappage;
 if m128k then SetPage(0);
 S:='';
 Offs:=OffAbreviations;
 Dec(c,164);
 WHILE c<>0 DO
  BEGIN
   WHILE (Spectrum^[Offs] AND 128) = 0 DO Inc(Offs);
   Inc(Offs);
   Dec(c);
  END;
  WHILE (Spectrum^[Offs] AND 128) = 0 DO BEGIN
                                         S := S + Char(Spectrum^[Offs]);
                                         Inc(Offs);
                               END;
  S := S + Char(Spectrum^[Offs] AND 127);
  Abreviation:= S;
  if m128k then  SetPage(oldpage);
END;

FUNCTION Type_Voc(C:Integer):String;
BEGIN
 IF (C>=0) AND (C<=MAXTVOC) then Type_Voc:=TVocs[c]
                            else Type_Voc:='RESERVED';
END;

FUNCTION Vocabula(Num, v_type : Integer) : String;
VAR VocPtr: Word;
    Voc:String;
    oldpage:byte;
BEGIN
 oldpage:=snappage;
 if m128k then SetPage(0);
 VocPtr:=OffVoc;
 if v_type>=0 then { PAW & Quill.C}
 begin
    SetLength(Voc,5);
    //Voc[0]:=#5;
   WHILE Peek(VocPtr)<>0 do
    BEGIN
     IF (Peek(vocptr+5) = Num) AND (Peek(vocptr+6) = v_type) THEN
      BEGIN
       Voc[1]:=char(PeekNeg(VocPtr));
       Voc[2]:=char(PeekNeg(VocPtr+1));
       Voc[3]:=char(PeekNeg(VocPtr+2));
       Voc[4]:=char(PeekNeg(VocPtr+3));
       Voc[5]:=char(PeekNeg(VocPtr+4));
       Vocabula:=Voc;
       if m128k then  SetPage(oldpage);
       Exit;
      END;
     Inc(VocPtr,7);
    END;
 end
 else
 begin { Quill.A}
    SetLength(Voc,4);
//    Voc[0]:=#4;
   WHILE Peek(VocPtr)<>0 do
    BEGIN
     IF Peek(vocptr+4) = Num THEN
      BEGIN
       Voc[1]:=char(PeekNeg(VocPtr));
       Voc[2]:=char(PeekNeg(VocPtr+1));
       Voc[3]:=char(PeekNeg(VocPtr+2));
       Voc[4]:=char(PeekNeg(VocPtr+3));
       Vocabula:=Voc;
       if m128k then  SetPage(oldpage);
       Exit;
      END;
     Inc(VocPtr,5);
    END;
 end;
 Vocabula:='';
 if m128k then  SetPage(oldpage);
END;

PROCEDURE Put_String(S : String);forward;

PROCEDURE Put_Token(c:Byte);
BEGIN
{ after TAB (6 and 22) if real space is printed, then no preceding space}
 If (LastPrintedChar in [32,6,7,9,13,22,24]) and (Tokens[c][1]=' ')
 Then Put_String(Copy(Tokens[c],2,255))
 Else Put_String(Tokens[c]);
END;

PROCEDURE Put_CharCode(c:Byte);
BEGIN
 IF ColorCodes>0 THEN
    BEGIN
         Write(FOut,'{',c,'}');
         Dec(ColorCodes);
         exit;
    END;
   IF (c=7)and (QuillVersion=0) or (c=13) THEN begin Write(FOut,'^');if(c=13) then XPos:=0;End;
   IF (c>15) and (c<22) THEN ColorCodes:=1
   ELSE IF (c=22) or (c=23) THEN ColorCodes:=2
   ELSE  ColorCodes:=0;
   IF(c=6) and (QuillVersion<>0) THEN
   BEGIN
    if (XPos mod 16)=0 then Inc(XPos);
    while (XPos mod 16)<>0 Do Inc(XPos);
    if XPos=32 Then XPos:=0;
    Write(FOut,'{6}');
   END
   ELSE IF(c=22) or (c=23) then Put_CharCode(32)
   ELSE IF (c>31) AND (c<>Byte('^')) AND (c<127) AND (c<>96)
      THEN begin
            Write(FOut,Chr(c)); LastPrintedChar:=c;
            if (ColorCodes=0) and (XPos=31) and (QuillVersion<>0) then Begin Write(FOut,' ');XPos:=0; End
            else Inc(XPos);
           end
   ELSE IF (C>164) AND compressed THEN Put_String(Abreviation(c))
   ELSE IF ExpandTokens and ((c>164) or basic128 and (c>162)) THEN Put_Token(c)
   ELSE begin Write(FOut,'{',c,'}');
          if c=8 then begin LastPrintedChar:=c; Dec(Xpos);end
          else if (ColorCodes=0) and (XPos=31) and (QuillVersion<>0) then Begin Write(FOut,' ');XPos:=0; End
          else if not (c in[6,13,16,17,18,19,20,21,22,23]) then inc(XPos);
        end

END;


PROCEDURE Put_String(S : String);
VAR I:Byte;
BEGIN
 FOR I:=1 TO Length(S) DO
   Put_CharCode(Byte(S[I]));
END;

PROCEDURE Put_Messages(Title:String; Offtab:Word; Start:Integer; Finish:Integer);
VAR I:Integer;
    C: Byte;
    Offs:Word;
    plain:boolean;
BEGIN
 plain:= (Start<0) and (QuillVersion=1);
 if Start<0 then Start:=0;
 I:=Start;
 if OffTab=0 then exit;
 Offs:=OffTab;
 XPos:=0;
 WHILE (I<=Finish) DO
  BEGIN
   if not plain then Offs:=DPeek(OffTab+2*(i-start));
   WriteLn(FOut,Title,' ',i:3,'  ');
   Inc(I);
   ColorCodes:=0;
   LastPrintedChar:=32;
   if plain then WHILE Spectrum^[Offs]=0 DO Inc(Offs);
   WHILE (Spectrum^[Offs] <> (31 XOR $FF)) AND ((Offs<OffTab) or plain) DO
    BEGIN
     C:=PeekNeg(Offs);
     Inc(Offs);
     Put_CharCode(c);
    END;
   Inc(Offs);
   WriteLn(FOut);
   WriteLn(FOut);
   XPos:=0;
   if not plain and (Offs>=OffTab) then break;
  END;
END;

PROCEDURE SetPageDistribution(NumLocs, NumMess :Integer);
VAR
  i, val, last :integer;
BEGIN
 for i:=0 to 7 do
  begin
   Locs[i]:=0;
   Mess[i]:=0;
  end;
  if QuillVersion<>0 then begin
                           Locs[0]:=NumLocs;
                           Mess[0]:=NumMess;
                           exit;
                          end;
  last:=0;
  i:=0;
  PagesUsed:=0;
  while Peek(MainTop+297+i)<>255 do
  begin
   val:=Peek(MainTop+298+i);
   if val<>255 then begin Locs[Peek(MainTop+297+i)]:=val; Inc(PagesUsed); end
   else if last<>255 then begin Locs[Peek(MainTop+297+i)]:=NumLocs; Inc(PagesUsed); end;
   last:=val;
   inc(i,2);
  end;
  last:=0;
  i:=0;
  while Peek(MainTop+283+i)<>255 do
  begin
   val:=Peek(MainTop+284+i);
   if val<>255 then Mess[Peek(MainTop+283+i)]:=val
   else if last<>255 then Mess[Peek(MainTop+283+i)]:=NumMess;
   last:=val;
   inc(i,2);
  end;
 if not m128k then
  for i:=1 to 7 do
   begin
    Locs[i]:=0;
    Mess[i]:=0;
   end;
END;

FUNCTION UpSt(S:String):String;
VAR I:Byte;
BEGIN
 FOR I:=1 TO Length(S) DO
  S[I]:=UpCase(S[I]);
 UpSt:=S;
END;


Procedure Put_Graph(OffGraph:Word; OffAttr:Word; FirstNum:Integer; LastNum:Integer);
VAR
 n,m,i, Offs: word;
 gflag, nargs, value: byte;
 inv, ovr: char;
 opcode: string;
 neg: array [0..7] of word;
BEGIN

     if QuillVersion<>0 then lastnum:=GraphCount-1;
     FOR n:=firstnum to lastnum do
     BEGIN
       Write(FOut,'Location ',n:3, ' graphics flags: ');
       gflag := Peek(OffAttr+n-firstnum);
       Offs  := DPeek(OffGraph + 2*(n-firstnum));

       if gflag and $80 <>0 then
           Write(FOut,'Picture.    ')
       else
           Write(FOut,'Subroutine. ');

       WriteLn(FOut, 'Ink=',gflag mod 8 ,' Paper=',
               (gflag and $3f) div 8, ' Bit6=', (gflag and 64) div  64);
       if(Peek(Offs) and 7 <> 7) then
       repeat
       begin
               for i:=0 to 7 do neg[i]:=0;
	       gflag := Peek(Offs );
               Write(Fout,'     ');
	       inv := ' '; ovr := ' ';
	       if gflag and 8 <> 0 then ovr := 'o';
	       if gflag and 16<>0 then inv := 'i';
	       value := gflag div 8;
               nargs:=0;
               case (gflag and 7) of
	         0: begin
                     nargs := 2;
		     if (ovr='o') and (inv='i') THEN
                       opcode := 'ABS MOVE   '
                     else
                       opcode := 'PLOT    '+ ovr+inv+' ';
                    end;
	         1: begin
                     nargs := 2;
                     if gflag and $40 <>0 then neg[0] := 1;
                     if gflag and $80 <>0 then neg[1] := 1;

		     if (ovr='o') and (inv='i') THEN
                       opcode := 'REL MOVE   '
		     else
                       opcode := 'LINE    '+ ovr+inv+' ';
		    end;
	         2: begin
                     if (gflag and $10 <>0) and (gflag and $20 <>0) then
		      begin
                            if gflag and $40 <>0 then neg[0] := 1;
                            if gflag and $80 <>0 then neg[1] := 1;
			    nargs := 3;
                            if QuillVersion=0 then
                             opcode := 'SHADE   '+ovr+inv+' '
                            else
                             opcode := 'BSHADE     ';
		      end
		     else
                     if gflag and $10 <>0 then
                      begin
		       nargs := 4;
                       opcode := 'BLOCK      ';
                      end
		     else
                     if gflag and $20 <>0 then
		      begin
                            if gflag and $40 <>0 then neg[0] := 1;
                            if gflag and $80 <>0 then neg[1] := 1;
			    nargs := 3;
                            opcode := 'SHADE   '+ovr+inv+' ';
		      end
		     else
		      begin
                            if gflag and $40 <>0 then neg[0] := 1;
                            if gflag and $80 <>0 then neg[1] := 1;
			    nargs := 2;
                            opcode := 'FILL       ';
		      end;
		    end;
	         3: begin
                     nargs := 1;
                     opcode :='GOSUB    sc='+ IntToStr2(value and 7,3, true)+' ';
		    end;
	         4: begin
                     if QuillVersion=0 then
                     begin
                       nargs := 3;
                       opcode := 'TEXT    '+ovr+inv+'  '+IntToStr2(value div 4,3, true)+' ';
                     end
                     else
                     begin
                       nargs:=0;
                       opcode := 'RPLOT   '+ovr+inv+'  '+plot_moves[value div 4]+' ';
                     end;
		    end;
	         5: begin
                     nargs := 0;
		     if gflag and $80 <>0 then
                      opcode :=  'BRIGHT      '+IntToStr2(value and 15, 3, true)
                     else
                      opcode := 'PAPER       '+IntToStr2(value and 15, 3, true);
		    end;
                 6: begin
                     nargs := 0;
		     if gflag and $80 <>0 then
                      opcode := 'FLASH       '+IntToStr2(value and 15, 3, true)
                     else
                      opcode := 'INK         '+IntToStr2(value and 15, 3, true);
		    end;
	         7: begin
                     nargs := 0;
                     opcode := 'END';
                    end;
	       end; {case}
	       Write(FOut, opcode);
               if nargs>0 then
	       for m := 0 to nargs-1 do
                Write(FOut, Select(neg[m]<>0, '-',' '), IntToStr2(Peek(Offs+1+m),3,true),' ');
	       WriteLn(Fout);
	       Inc(Offs,nargs + 1);
         end
         until ( (gflag and 7) = 7);
         WriteLn(Fout);
	END
END;

Function MapQuillCondActToPAW(num:integer; is_condition:boolean):integer;
BEGIN
 MapQuillCondActToPAW:=num;
 if (QuillVersion=0) or is_condition then exit;
 if QuillVersion=1 then
   if num>20 then num:=num+10
   else if num>11 then num:=num+9
   else if num>10 then num:=num+6;
 if (num>20) and (num<36) then num:=num-2
 else if num=36 then num:=num+10
 else if (num>17) and (num<21) then num:=num+29;
 MapQuillCondActToPAW:=num+18;
END;

Procedure SaveBlockToTape(var F: File; Name: string; first, last: word);
Var
 header: array[0..16] of Byte;
 Length: word;
 tmpb:Byte;
 tmpw:Word;
 i:word;
BEGIN
 header[0]:=3;
 Name:=Justify(Name,10);
 Move(Name[1],header[1],10);
 Length:=last-first+1;
 Move(Length,header[11],2);
 Move(first, header[13],2);
 header[15]:=0;
 header[16]:=128;
 tmpw:=19;tmpb:=0;
 BlockWrite(F,tmpw,2);
 BlockWrite(F,tmpb,1);
 BlockWrite(F,header,17);
 for i:=0 to 16 do tmpb:= tmpb xor header[i];
 BlockWrite(F,tmpb,1);
 tmpw:=Length+2;tmpb:=255;
 BlockWrite(F,tmpw,2);
 BlockWrite(F,tmpb,1);
 BlockWrite(F,Spectrum^[first], Length);
 for i:=first to last do tmpb:= tmpb xor Spectrum^[i];
 BlockWrite(F,tmpb,1);
END;

PROCEDURE Syntax;
BEGIN
 WriteLn;
 WriteLn('SYNTAX:  ',Product,' [-I]<filename> [[-O]<filename>|-T<filename>]');
 WriteLn;
 WriteLn('-I must be a .SP, .SNA or .Z80 file');
 WriteLn('-O Text file where the output is written. If you don''t include');
 WriteLn('   this parameter results will be sent to standard output.');
 WriteLn('-T Tape image file where the output is exported. You may load this ''tape''');
 WriteLn('   into the Quill/Illustrator/PAW editor. This parameter suppresses');
 WriteLn('   text output.');
 Writeln('Other options:');
 WriteLn('-A Include compress (database compression) information.');
 WriteLn('-C Include charsets information.');
 WriteLn('-G Include graphics information.');
 Writeln('-S snapshot type. Must be one of strings: SNA, SP, Z80');
 WriteLn('-E Expand Spectrum keyword tokens');
 WriteLn;
 WriteLn('Example :  ',Product,' RIPPER1.ZX RIPPER1.DB -GAC -SZ80');
 WriteLn;
 Halt;
END;


PROCEDURE CheckParameters;
VAR I,J:Byte;
    opt,parm:String;
    badoption:boolean;
//    Path: array[0..fsPathName] of Char;
//    Dir: array[0..fsDirectory] of Char;
//    Name: array[0..fsFileName] of Char;
//    Ext: array[0..fsExtension] of Char;
BEGIN
 IF ParamCount=0 THEN Syntax;

 InputFileName:='';
 OutPutFileName:='';
 OutputTapeName:='';
 SnapType:='';
 AbrevOn:=False;
 GraphOn:=false;
 FontsOn:=false;
 ExpandTokens:=false;
 badoption:=false;
 FOR I:=1 TO ParamCount DO
  IF (Copy(ParamStr(I),1,1)='-') AND (Length(ParamStr(I))>1) THEN
   FOR J:=2 TO Length(ParamStr(I)) DO
   BEGIN
   opt :=Copy(ParamStr(I),J,1);
   opt[1]:=UpCase(opt[1]);
   parm:=Copy(ParamStr(I),J+1,255);
   IF (opt='I') THEN BEGIN IF InputFileName='' THEN InputFileName:=parm ELSE badoption:=true; break; END
   ELSE
   IF (opt='T') THEN BEGIN IF OutputTapeName='' THEN OutputTapeName:=parm ELSE badoption:=true; break; END
   ELSE
   IF (opt='O') THEN BEGIN IF OutputFileName=''  THEN OutputFileName:=parm ELSE badoption:=true; break; END
   ELSE
   IF (opt='S') THEN BEGIN IF SnapType=''  THEN SnapType:=UpSt(parm) ELSE badoption:=true; break; END
   ELSE
   IF opt='A' THEN BEGIN IF not AbrevOn THEN AbrevOn:=True ELSE badoption:=true;END
   ELSE
   IF opt='C' THEN BEGIN IF not FontsOn THEN FontsOn:=True ELSE badoption:=true;END
   ELSE
   IF opt='G' THEN BEGIN IF not GraphOn THEN GraphOn:=True ELSE badoption:=true;END
   ELSE
   IF opt='E' THEN BEGIN IF not ExpandTokens THEN ExpandTokens:=True ELSE badoption:=true;END
   ELSE
    badoption:=true;
  END
  ELSE
  BEGIN
   IF InputFileName='' THEN InputFileName:=ParamStr(i)
   ELSE IF OutputFileName='' THEN OutputFileName:=ParamStr(i)
   ELSE badoption:=true;
  END;
 IF (SnapType<>'') and (SnapType<>'SNA') and (SnapType<>'SP') and (SnapType<>'Z80') THEN badoption:=true;
 IF badoption THEN InputFileName:='';
 IF InputFileName='' THEN Syntax;
 InputName:=ChangeFileExt(ExtractFileName(InputFileName),'');
 InputExt:=UpSt(ExtractFileExt(InputFileName));
// StrPCopy(Path,InputFileName);
 //FileSplit(Path,Dir, Name,Ext);
 //InputName:=StrPas(Name);
 //InputExt:=UpSt(StrPas(Ext));
END;


BEGIN (* main *)
 WriteLn(Product,' v',Version,' ',Copyright);
 for i:=0 to 7 do Pages[i]:=nil;
 Spectrum:=nil;

 (* Load parameters *)
 CheckParameters;

 (* Load File *)
 Assign(F,InputFileName);
 Reset(F,1);
 IF IOResult<>0 THEN Error(1);
 FSize:=FileSize(F);
 M128K:=false;
 IF SnapType='' THEN SnapType:=Copy(InputExt,2,255);
 GetMem(Spectrum,49152);
 IF (SnapType='SNA') AND (FSize=49179) THEN {SNA 48 }
         BEGIN
          Seek(F,27);
          BlockRead(F,Spectrum^[16384],49152);
         END ELSE
 IF (SnapType='SNA') AND ((FSize=131103) or (Fsize=147487)) THEN  {SNA 128}
         BEGIN
          M128K:=true;
          for i:=0 to 7 do getmem(Pages[i],16384);
          Seek(F,27);
          BlockRead(F,Spectrum^[16384],49152);
          BlockRead(F,tmpb,1);
          BlockRead(F,tmpb,1);
          BlockRead(F,snappage,1);
          BlockRead(F,tmpb,1);
          if snappage<>((snappage and $07)+16) then Error(2);
	  snappage:=snappage and $07;
          if snappage<>0 then blockread(F,Pages[0]^,16384);
          if snappage<>1 then blockread(F,Pages[1]^,16384);
          if snappage<>3 then blockread(F,Pages[3]^,16384);
          if snappage<>4 then blockread(F,Pages[4]^,16384);
          if snappage<>6 then blockread(F,Pages[6]^,16384);
          if snappage<>7 then blockread(F,Pages[7]^,16384);
          move(Spectrum^[16384],Pages[5]^,16384);
          move(Spectrum^[32768],Pages[2]^,16384);
          move(Spectrum^[49152],Pages[snappage]^,16384);
          SetPage(0);
         END ELSE
 IF (SnapType='SP') AND (FSize=49190) THEN { SP 48 }
         BEGIN
          Seek(F,38);
          BlockRead(F,Spectrum^[16384],49152);
          SnapType:='SP';
         END
  ELSE IF SnapType='Z80' THEN CASE LoadZ80(InputFileName,Spectrum^, Pages) OF
                                             1: begin
                                                 m128k:=true;
                                                 move(Pages[5]^,Spectrum^[16384],16384);
                                                 move(Pages[2]^,Spectrum^[32768],16384);
                                                 SetPage(0);
                                                end;
                                             2:Error(5);
                                             3:Error(6);
                                            END
  ELSE Error(2);
 Close(F);

 (* Analyze *)

 QuillVersion:=0;

 MainTop:=DPeek(65533);
 MainAttr:=MainTop+311;
 IF   (MainTop<=(65535-321))
   and (MainTop>=(16384-311))
   and (Peek(MainAttr) = 16)
   and (Peek(MainAttr+2) = 17)
   and (Peek(MainAttr+4) = 18)
   and (Peek(MainAttr+6) = 19)
   and (Peek(MainAttr+8) = 20)
   and (Peek(MainAttr+10) = 21)
 THEN
     Writeln('PAW signature found.')
 ELSE BEGIN
      MainTop:=26931;
      MainAttr:=MainTop+977;
      IF    (Peek(MainAttr) = 16)
        and (Peek(MainAttr+2) = 17)
        and (Peek(MainAttr+4) = 18)
        and (Peek(MainAttr+6) = 19)
        and (Peek(MainAttr+8) = 20)
        and (Peek(MainAttr+10) = 21)
      THEN BEGIN
          Writeln('Quill.A signature found.');
          QuillVersion:=1;
      END
      ELSE BEGIN
           MainTop:=27356;
           MainAttr:=MainTop+169;
           IF    (Peek(MainAttr) = 16)
             and (Peek(MainAttr+2) = 17)
             and (Peek(MainAttr+4) = 18)
             and (Peek(MainAttr+6) = 19)
             and (Peek(MainAttr+8) = 20)
             and (Peek(MainAttr+10) = 21)
           THEN BEGIN
               Writeln('Quill.C signature found.');
               QuillVersion:=3;
           END
           ELSE
               Error(3);
      END
 END;


 (* Global data *)
 IF QuillVersion=0 THEN BEGIN
   NumMsg:=Peek(MainTop+326);
   OffSys:=DPeek(65505);
   NumSys:=Peek(MainTop+327);
   NumLoc:=Peek(MainTop+325);
   OffObj := DPeek(65499);
   NumObj := Peek(MainTop+324);
   NumPro := Peek(MainTop+328) ;
   OffPro := DPeek(65497) ;
   OffAbreviations := DPeek(MainTop+332) ;
   NumFonts := Peek(MainTop+329);
   OffFont := DPeek(MainTop+330);
   OffGraph := DPeek(65521);
   OffGraphAttr:=DPeek(65523);
   compressed:=Peek(OffAbreviations)=0;
 END
 ELSE
 BEGIN
   tOffs:=MainAttr+13;
   NumMsg:=Peek(tOffs+3);
   if QuillVersion=1 THEN
   BEGIN
    OffSys:=MainTop+168;
    NumSys:=32;
   END
   ELSE
   BEGIN
    OffSys:=DPeek(tOffs+15);
    NumSys:=Peek(tOffs+4);
   END;
   NumLoc:=Peek(tOffs+2);
   IF QuillVersion=1 THEN OffObj := DPeek(tOffs+8)
   ELSE OffObj := DPeek(tOffs+9);
   NumObj := Peek(tOffs+1);

   IF QuillVersion=1 THEN OffPro := DPeek(tOffs+6)
   ELSE OffPro := DPeek(tOffs+7);
   IF QuillVersion=1 THEN OffResp := DPeek(tOffs+4)
   ELSE OffResp := DPeek(tOffs+5);

   IF QuillVersion=1 THEN OffAbreviations := tOffs+24
   ELSE OffAbreviations := tOffs+29;
   compressed:=(QuillVersion=3) and (Peek(OffAbreviations)<128) and (Peek(OffAbreviations+1)=128);

   Patched:= (DPeek(24791)=DPeek(23606)) or (DPeek(24802)=DPeek(23606));
   if(not Patched) then begin
                         if DPeek(23606)<16384 then NumFonts:=0
                         else NumFonts:=1;
                        end
   else if (DPeek(24791)<16384) and (DPeek(24802)<16384)  then NumFonts:=0
   else if (DPeek(24791)<16384) or (DPeek(24802)<16384)  then NumFonts:=1
   else NumFonts:=2;
   if (DPeek(64182)<=64182) and (DPeek(64188) = 64181) then
   begin
      OffGraph := DPeek(64184);
      OffGraphAttr:=DPeek(64186);
      GraphCount:=Peek(64190);
   end
   else
    OffGraph:=0;
 END;

 Basic128:= Peek(23611)and 16<>0;
 SetPageDistribution(NumLoc,NumMsg);

 WriteLn;
 WriteLn('Analyzing [',InputName,']');

 IF OutPutTapeName='' THEN
 BEGIN
  (* Open output file *)
  Assign(FOut,OutputFileName);
  Rewrite(FOut);
  WriteLn(FOut);

  (* System colors *)
  if QuillVersion=0 then
       WriteLn(FOut,'PAW Database : ',InputFileName)
  else
      WriteLn(FOut,'Quill Database : ',InputFileName);
  WriteLn(FOut);
  WriteLn(FOut,'Extracted by ',Product,' v'+ version);
  WriteLn(FOut);
  WriteLn('Checking general data...');
  WriteLn(FOut,'General data');
  WriteLn(FOut,'------------');
  WriteLn(FOut);
  WriteLn(FOut,'Locations                   ', NumLoc:10);
  WriteLn(FOut,'Objects                     ', NumObj:10);
  if QuillVersion<>0 then
       WriteLn(FOut,'Conveyable objects          ', Peek(MainAttr+13):10);
  WriteLn(FOut,'Messages                    ', NumMsg:10);
  WriteLn(FOut,'System messages             ', NumSys:10);
  if QuillVersion=0 then
       WriteLn(FOut,'Processes                   ', NumPro:10);
  Writeln(FOut,'Character sets              ', NumFonts:10);
  if QuillVersion=0 then
       Writeln(FOut,'Default character set       ', Peek(MainTop+281):10);
  if (QuillVersion<>0) and (OffGraph<>0) then
       Writeln(FOut,'Graphics Count              ', GraphCount:10);
  WriteLn(FOut,'Default ink color           ', Peek(MainAttr+1):10) ;
  WriteLn(FOut,'Default paper color         ', Peek(MainAttr+3):10) ;
  WriteLn(FOut,'Default flash state         ', Peek(MainAttr+5):10) ;
  WriteLn(FOut,'Default bright state        ', Peek(MainAttr+7):10) ;
  WriteLn(FOut,'Default inverse state       ', Peek(MainAttr+9):10) ;
  WriteLn(FOut,'Default over state          ', Peek(MainAttr+11):10) ;
  WriteLn(FOut,'Default border color        ', Peek(MainAttr+12):10) ;
  if (QuillVersion<>0) and (OffGraph<>0) then
        Writeln(FOut,'The Illustrator used');
  if (QuillVersion<>0) and patched then
        Writeln(FOut,'The Patch used');
  if QuillVersion<>0 then
        Writeln(FOut,'Database version            ', Select(QuillVersion=1,'A','C'):10)
  else if Peek(65527)>31 then
        Writeln(FOut,'Database version            ', Chr(Peek(65527)):10)
  else
        Writeln(FOut,'Database version            ', Peek(65527):10);
  Writeln(FOut, 'Database ', Select(compressed,'','not '),'compressed');
  WriteLn(FOut,'Snapshot type     ',
  (SnapType+Select(m128k,' 128K',' 48K')+' Basic'+Select(Basic128, '128','48')):20) ;
  if (QuillVersion=0) and (PagesUsed>1) and not m128k then
     Writeln(FOut,'Pages missing               ', (PagesUsed-1):10);
  WriteLn(FOut);

  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);


  WriteLn('Extracting vocabulary');
  (* Vocabulary *)
  WriteLn(FOut,'Vocabulary');
  WriteLn(FOut,'----------');

  if QuillVersion=0 then
  begin
     vocptr := DPeek(65509);
     OffVoc := DPeek(65509);

     WHILE (vocptr < 65509) AND  (Peek(vocptr)<>0) DO
     BEGIN
      WriteLn(FOut,char(PeekNeg(vocptr)),char(PeekNeg(vocptr+1)),
             char(PeekNeg(vocptr+2)),char(PeekNeg(vocptr+3)),char(PeekNeg(vocptr+4)),
             Peek(vocptr+5):5,' ',type_voc(Peek(vocptr+6)));
      VocPtr := VocPtr + 7 ;
     END;
  end
  else { Quill Vocabulary}
  begin
     if QuillVersion=1 then vocptr := DPeek(MainAttr+29)
     else vocptr := DPeek(MainAttr+32);
     OffVoc := vocptr;

     WHILE (vocptr < 65530) AND  (Peek(vocptr)<>0) DO
     BEGIN
      WriteLn(FOut,char(PeekNeg(vocptr)),char(PeekNeg(vocptr+1)),
             char(PeekNeg(vocptr+2)),char(PeekNeg(vocptr+3)),Peek(vocptr+4):5);
      VocPtr := VocPtr + 5 ;
     END;
  end;
  WriteLn(FOut);

  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);
  (* Messages *)
  WriteLn(FOut,'MESSAGES');
  WriteLn(FOut,'--------');
  WriteLn('Extracting ',NumMsg, ' message(s)');
  numcurr:=0;
  for i:=0 to 7 do
  if Mess[i]<>0 then
   begin
    if m128k then SetPage(i);
    if QuillVersion=0 then OffMsg:=DPeek(65503)
    else if QuillVersion=1 then OffMsg:=DPeek(MainAttr+25)
    else OffMsg:=DPeek(MainAttr+26);
    Put_Messages ('Message', OffMsg, numcurr, Mess[i]-1);
    numcurr:=Mess[i];
   end;
  if m128k then SetPage(0);
  {OffMsg:=DPeek(65503);}
  WriteLn(FOut);


  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);
  (* System messages *)
  WriteLn(FOut,'SYSTEM MESSAGES');
  WriteLn(FOut,'---------------') ;
  WriteLn('Extracting ',NumSys,' system message(s)');
  Put_Messages ('System Message', OffSys, -1, numsys-1);
  WriteLn(FOut);

  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);
  (* Location descriptions *)
  WriteLn(FOut,'LOCATIONS');
  WriteLn(FOut,'---------');
  WriteLn('Extracting ',NumLoc,' location(s)');
  numcurr:=0;
  for i:=0 to 7 do
  if Locs[i]<>0 then
   begin
    if m128k then SetPage(i);
    if QuillVersion=0 then OffLoc:=DPeek(65501)
    else if QuillVersion=1 then OffLoc:=DPeek(MainAttr+23)
    else OffLoc:=DPeek(MainAttr+24);
    Put_Messages ('Location', OffLoc, numcurr, Locs[i]-1);
    numcurr:=Locs[i];
   end;
  if m128k then SetPage(0);
  {OffLoc:=DPeek(65501);}
  WriteLn(FOut);

  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);
  (* Conections *)
  WriteLn(FOut,'CONNECTIONS');
  WriteLn(FOut,'-----------') ;
  WriteLn('Extracting connections');
  numcurr:=0;
  for j:=0 to 7 do if Locs[j]>0 then
  BEGIN
   if m128k then SetPage(j);
   if QuillVersion=0 then offcon := DPeek(65507)
   else if QuillVersion=1 then offcon:=DPeek(MainAttr+27)
   else offcon:=DPeek(MainAttr+30);
   FOR i:=numcurr TO Locs[j]-1 DO
    BEGIN
     conptr := DPeek(offcon+2 * (i-numcurr)) ;
     n := 0 ;
     WHILE Peek(conptr) <> 255 DO
       BEGIN
        if QuillVersion<>0 then S:=Vocabula(Peek(conptr),-1)
        else begin
         S := Vocabula(Peek(conptr), 0);
         if S='' then S := Vocabula (Peek(conptr), 2) ;
        end;
        if n=0 THEN Write(FOut,'Location ',i:3,':  ')
               ELSE Write(FOut,'               ') ;
        n := n+ 1;
        if S<>'' then WriteLn(FOut,S,' ',Peek(conptr+1))
                 else WriteLn(FOut,Peek(conptr),' ',Peek(conptr+1));
        conptr :=conptr+ 2 ;
       END;
       IF n<>0 THEN WriteLn(FOut);
    END;
    numcurr:=Locs[j];
  END;
  if m128k then SetPage(0);
  {OffCon := DPeek(65507) ;}
  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);
  (* Object names *)
  WriteLn(FOut,'OBJECT NAMES');
  WriteLn(FOut,'------------');
  WriteLn('Extracting ',NumObj,' object(s)');
  Put_Messages ('Object',OffObj , 0, NumObj-1);
  WriteLn(FOut);

  if QuillVersion<>1 then
  begin
    WriteLn(FOut,'--------------------------------------------------------------------------');
    WriteLn(FOut);
    (* Object words *)
    WriteLn('Extracting object words');
    WriteLn(FOut,'OBJECT WORDS');
    WriteLn(FOut,'------------') ;
    if QuillVersion=0 then offwobj := DPeek(65513)
    else offwobj:=DPeek(MainAttr+36);
    FOR I:=0 TO NumObj-1 DO
    BEGIN
      if QuillVersion=0 then
      begin
        S := Vocabula(Peek(offwobj+2*i), 2) ;
        if S<>'' THEN S:=S + ' ' + IntToStr(Peek(offwobj+2*i));
        if (Peek(offwobj+2*i) = 255) THEN S:=S+'_    ' ;
        Write(FOut,'Object ',i:3,'    ',S,' ') ;
        S := Vocabula(Peek(offwobj+2*i+1), 3) ;
        if S<>'' THEN S := S + ' '+IntToStr( Peek(offwobj+2*i+1));
        if (Peek(offwobj+2*i+1) = 255) then S := S+'_    ';
        WriteLn(FOut,S) ;
      end
      else
      begin
        S := Vocabula(Peek(offwobj+i), -1) ;
        if (S<>'') and (Peek(offwobj+i)<>255) THEN S:=S + ' ' + IntToStr(Peek(offwobj+i));
        if (Peek(offwobj+i) = 255) and (S='') THEN S:=S+'_' ;
        WriteLn(FOut,'Object ',i:3,'    ',S) ;
      end
    END;
    WriteLn(FOut);
  end;

  WriteLn(FOut,'--------------------------------------------------------------------------');
  WriteLn(FOut);
  (* Objects "Initially at" *)
  WriteLn('Extracting Initially At');
  WriteLn(FOut,'INITIALLY AT');
  WriteLn(FOut,'------------');
  if QuillVersion=0 then OffLObj := DPeek(65511)
  else if QuillVersion=1 then OffLObj:=DPeek(MainAttr+31)
  else OffLObj:=DPeek(MainAttr+34);
  for i:=0 TO NumObj-1 DO
    CASE (Peek(offlobj+i)) OF
     252:WriteLn(FOut,'Object ',i:3,'    NC');
     253:WriteLn(FOut,'Object ',i:3,'    W');
     254:WriteLn(FOut,'Object ',i:3,'    C')
     ELSE WriteLn(FOut,'Object ',i:3,'    ',Peek(offlobj+i));
    END;
  WriteLn(FOut);

  if QuillVersion=0 then
  begin
    WriteLn(FOut,'--------------------------------------------------------------------------');
    WriteLn(FOut);
    (* Objects weight and type *)
    WriteLn('Extracting objects weight and type');
    WriteLn(FOut,'OBJECT WEIGHT AND TYPE');
    WriteLn(FOut,'----------------------') ;
    offxobj := DPeek(65515) ;
    for I:=0 TO NumObj-1 DO
       WriteLn(FOut,'Object ',i:3,':  weights ',
         Peek(offxobj+i) AND 63:3,'    ',
          Select((Peek(offxobj+i) AND 64)<>0,'C','          '),' ',
           Select((Peek(offxobj+i) AND 128)<>0,'W','      '));
    WriteLn(FOut);
  end;

  (* Processes *)
  WriteLn('Extracting Process/Response tables');
  if QuillVersion<>0 Then NumPro:=2;
  FOR P:=0 TO NumPro-1 DO
  BEGIN
   WriteLn(FOut,'--------------------------------------------------------------------------');
   WriteLn(FOut);
   IF (p=0) THEN BEGIN
                  WriteLn(FOut,'RESPONSE TABLE');
                  WriteLn(FOut,'--------------')
                 END
            ELSE BEGIN
                  if QuillVersion=0 then
                  begin
                   WriteLn(FOut,'PROCESS ',p:3);
                   WriteLn(FOut,'-----------');
                  end
                  else
                  begin
                   WriteLn(FOut,'PROCESS TABLE');
                   WriteLn(FOut,'-------------');
                  end
                 END;

    if QuillVersion=0 then
      proptr := DPeek(OffPro+p*2)
    else if p=0 then
     proptr:=OffResp
   else
    proptr:=OffPro;
    while (Peek(proptr)<>0) DO
      BEGIN
          if QuillVersion<>0 then
          BEGIN
            S:=Vocabula(Peek(proptr),-1);
            if S = '' THEN S := IntToStr(Peek(proptr)) ;
          END
          else
          begin
            S := Vocabula (Peek(proptr), 0) ;
            if (Peek(proptr) = 1) then S := S + '*    '
            else
            if (Peek(proptr) = 255) then S := S + '_    ';
            if S='' THEN
              BEGIN
                if (Peek(proptr) < 20) then
                    S := Vocabula(Peek(proptr), 2);
                if S = '' THEN S := IntToStr(Peek(proptr)) ;
              END;
          end;
          Write(FOut,S,Select(QuillVersion=0,' ','  ')) ;
          proptr := proptr + 1 ;
          if QuillVersion<>0 then
          BEGIN
            S:=Vocabula(Peek(proptr),-1);
            if S = '' THEN S := IntToStr(Peek(proptr)) ;
          END
          else
          begin
            S := Vocabula (Peek(proptr), 2) ;
            if (Peek(proptr) = 1) then S := S + '*    '
            else
            if (Peek(proptr) = 255) then S := S + '_    ';
            if S = '' THEN
             BEGIN
                if (Peek(proptr) < 20) then
                    S := Vocabula(Peek(proptr), 2);
                if S='' THEN S:=IntToStr(Peek(proptr)) ;
             END;
          end;
          Write(FOut,S,'      ',Select(QuillVersion=0,'',' ')) ;
          proptr := proptr +1 ;

          resptr := DPeek(proptr) ;
          proptr := proptr + 2 ;

          n := 0 ;
          opcodetype:=true;
          if (Peek(resptr) = 255) and (QuillVersion<>0) then
          begin
            resptr:=resptr+1;
            opcodetype:=false;
          end;
          while Peek(resptr) <> 255 do
          BEGIN
            if (n<>0) then Write(FOut,'                 ') ;
            n := n + 1;
            opcode:=MapQuillCondActToPAW(Peek(resptr),opcodetype);
            if (opcode <= 107) THEN
              BEGIN
                  Write(FOut,Justify(condacts[opcode].condact,12));
                  if (condacts[opcode].params > 0) THEN
                      Write(FOut,' ',Peek(resptr+1):3);
                  if (condacts[opcode].params > 1) THEN
                      Write(FOut,' ',Peek(resptr+2):3);
                  resptr :=resptr + condacts[opcode].params;
              END;
              resptr := resptr + 1 ;
              WriteLn(FOut);
              if (Peek(resptr) = 255) and (QuillVersion<>0) and opcodetype then
              begin
                resptr:=resptr+1;
                opcodetype:=false;
              end;
          END;
          WriteLn(FOut);
      END;
      WriteLn(FOut);
  END;


   IF AbrevOn and compressed THEN
   BEGIN
    WriteLn(FOut,'--------------------------------------------------------------------------');
    WriteLn(FOut);
    (* Compression data *)
    WriteLn('Extracting compression data');
    WriteLn(FOut,'Compression data');
    WriteLn(FOut,'----------------');
    FOR I:=164 TO 254 DO
        WriteLn(FOut,'Compression ',I:2,': ',Abreviation(I));
    WriteLn(FOut);
   END;
  if m128k then SetPage(0);

  if GraphOn and (OffGraph<>0) then
  BEGIN
    WriteLn(FOut,'--------------------------------------------------------------------------');
    WriteLn(FOut);
    (* Graphics *)
    WriteLn(FOut,'GRAPHICS DATA');
    WriteLn(FOut,'-------------') ;
    WriteLn('Extracting location graphics');
    numcurr:=0;
    for j:=0 to 7 do if Locs[j]>0 then
    BEGIN
      if m128k then SetPage(j);
      if QuillVersion=0 then
      begin
        OffGraph := DPeek(65521);
        OffGraphAttr:=DPeek(65523);
      end;
      Put_Graph(OffGraph, OffGraphAttr, numcurr, Locs[j]-1);
      numcurr:=Locs[j];
    END;
    WriteLn(FOut);
    if m128k then SetPage(0);
    {OffCon := DPeek(65507) ;}
  END;
  if m128k then SetPage(0);


  IF FontsOn THEN
   BEGIN
    WriteLn(FOut,'--------------------------------------------------------------------------');
    WriteLn(FOut);
    (* Character Sets data *)
    WriteLn('Extracting Character Sets data');
    WriteLn(FOut,'Charset 0');
    WriteLn(FOut,'---------');
    WriteLn(FOut,'UDG data');
    WriteLn(FOut,'--------');
    Writeln(FOut, 'static char udg_bits[] = {');
    if QuillVersion=0 then tempint:=18 else tempint:=20;
    FOR I:=0 TO tempint DO
     begin
     for j:=0 To 7 do
      Write(FOut,' ',Hex(Peek(MainTop+i*8+j)), select((i=tempint)and(j=7),'};',','));
     WriteLn(Fout);
     end;
    WriteLn(FOut);

    if QuillVersion=0 then
    begin
      WriteLn(FOut,'Shade data');
      WriteLn(FOut,'----------');
      Writeln(FOut, 'static char shade_bits[] = {');
      FOR I:=0 TO 15 DO
       begin
       for j:=0 To 7 do
        Write(FOut,' ',Hex(Peek(MainTop+152+i*8+j)), select((i=15)and(j=7),'};',','));
       WriteLn(Fout);
       end;
      WriteLn(FOut);
    end;

    if NumFonts>0 Then
    Begin
      for n:=0 to NumFonts-1 do
      begin
      tempint:=n+1;
      if QuillVersion<>0 then
      begin
       if not patched then begin OffFont:=DPeek(23606); tempint:=0; end
       else if n=0 then
        begin
         OffFont:=DPeek(24791);
         tempint:=0;
         if OffFont<16384 then begin OffFont:=DPeek(24802); tempint:=1; end;
        end
       else begin OffFont:=DPeek(24802); tempint:=1; end;
       OffFont:=OffFont+256;
      end;

      if (QuillVersion=0) or (tempint>0) then
      begin
        WriteLn(FOut,'CharSet ',tempint);
        WriteLn(FOut,'----------');
      end
      else
      begin
        WriteLn(FOut,'CharSet data');
        WriteLn(FOut,'------------');
      end;
        Writeln(FOut, 'static char font_bits_',tempint,'[] = {');
        FOR I:=0 TO 95 DO
         begin
         for j:=0 To 7 do
          if QuillVersion=0 then
            Write(FOut,' ',Hex(Peek(OffFont+ 768*n+i*8+j)), select((i=95)and(j=7),'};',','))
          else
            Write(FOut,' ',Hex(Peek(OffFont+i*8+j)), select((i=95)and(j=7),'};',','));
         WriteLn(Fout);
         end;
        WriteLn(FOut);
      end;
    End;

    if(patched) then
    begin
      WriteLn(FOut,'Patch data');
      WriteLn(FOut,'----------');
      WriteLn(FOut,'Default printing line:    ',Peek(64414));
      Writeln(FOut,'Cursor character:         "',Char(PeekNeg(27192)),'"');
      Writeln(FOut,'Prompt character:         "',Char(PeekNeg(27187)),'"');
      Writeln(FOut,'Non-inverse prompt:       ',Select(Peek(27186)=255,'yes','no'));
      Writeln(FOut,'Non-flashing cursor:      ',Select(Peek(27179)=255,'yes','no'));
    end;

   END;
 END
 ELSE { TapeExport }
 BEGIN
  Writeln('Exporting To Tape Image: ',OutputTapeName);
  Assign(Tape, OutputTapeName);
  Rewrite(Tape,1);
  if QuillVersion=0 then
  begin
   j:=0;
   for i:=0 to 7 do
   if Locs[i]<>0 then
      begin
       if m128k then SetPage(i);
       S:=Justify(InputName,9);
       SaveBlockToTape(Tape, S+Chr(65+j), DPeek(65533), DPeek(65517)-1);
       SaveBlockToTape(Tape, S+Chr(66+j), DPeek(65519), 65535);
       inc(j,2);
      end;
  end
  else
  begin
       if m128k then SetPage(0);
       S:=Justify(InputName,10);
       if QuillVersion=1 then tempint:=MainAttr+33
       else tempint:=MainAttr+38;
       SaveBlockToTape(Tape, S, MainTop, DPeek(tempint)-1);
       if OffGraph<>0 then
        SaveBlockToTape(Tape, 'GRAPHICS', DPeek(64182), 64182+11);
       if NumFonts>0 then
        for n:=0 to NumFonts-1 do
        begin
         if not patched then OffFont:=DPeek(23606)
         else if n=0 then
           begin
            OffFont:=DPeek(24791);
            if OffFont<16384 then OffFont:=DPeek(24802);
           end
         else OffFont:=DPeek(24802);
         SaveBlockToTape(Tape, 'FONT'+chr(48+n), OffFont+256, OffFont+1023);
        End;
  end;
  if m128k then SetPage(0);
  SaveBlockToTape(Tape, 'SCREEN', 16384,23295);
  Close(Tape);
 END;

 if m128k then for i:=0 to 7 do if Pages[i]<>nil then FreeMem(Pages[i],16384);
 if Spectrum<>nil then FreeMem(Spectrum,49152);
 WriteLn('Ok. Finished.');
 Close(FOut);
END.


