

UNIT Z80Load;

(***************************************************************************)
(*                                                                         *)
(*  Professional Adventure Writing System extractor.                       *)
(*  Original vesion: Jose Luis Cebri†n PagÅe                               *)
(*  Pascal version : Carlos Sanchez (csg@iberpoint.net)                    *)
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

INTERFACE

Type
 SpectrumMemory = Array [16384..65535] of Byte;
 SnapshotPage = Array [49152..65535] of Byte;
 SpectrumPages = Array [0..7] of ^Snapshotpage;

FUNCTION LoadZ80(FileName: String; VAR Z80RAM:SpectrumMemory; VAR Pages:SpectrumPages):Integer;
(*
Loads a .Z80 file in a memory zone. The memory zone is supposed to be
the beginning of Spectrum's RAM, at address 4000h (16384),
128k pages store in additional  block
*)

IMPLEMENTATION

TYPE PByte=^Byte;

FUNCTION GetByte(VAR P:PByte; VAR Counter:Word):Byte;
begin
 GetByte:=P^;
 Inc(P);
 Dec(Counter);
END;

{ Decodes copmressed block. Returns false om bad memory reference}
FUNCTION Decode(VAR SRC,DST; BlockLen,OutputLen:Word):Boolean;
VAR P0,PF:PByte;
    A,B:Byte;
BEGIN
 P0:=@Src;
 PF:=@Dst;
 Decode:=true;
 WHILE (BlockLen<>0) AND (OutputLen<>0) DO
 BEGIN
  A:=GetByte(P0,BlockLen);
  IF A<>237 THEN BEGIN
                  PF^:=A;
                  Inc(Pf);
                  Dec(OutputLen);
                 END
            ELSE BEGIN
                  IF BlockLen=0 Then BEGIN Decode:=false; break; END;
                  B:=GetByte(P0,BlockLen);
                  IF B<>237 THEN BEGIN
                                 IF OutputLen<2 Then BEGIN Decode:=false; break; END;
                                  PF^:=237;
                                  Inc(Pf);
                                  PF^:=B;
                                  Inc(Pf);
                                  Dec(OutputLen,2);
                                 END
                            ELSE BEGIN
                                  IF BlockLen<2 Then BEGIN Decode:=false; break; END;
                                  A:=GetByte(P0,BlockLen);
                                  B:=GetByte(P0,BlockLen);
                                  IF OutputLen<A Then BEGIN Decode:=false; break; END;
                                  FillChar(PF^,A,B);
                                  Inc(PF,A);
                                  Dec(OutputLen,A);
                                 END;
                 END;
 END;
 IF (BlockLen<>0) OR (OutputLen<>0) THEN Decode:=false;
END;

FUNCTION LoadZ80(FileName: String; VAR Z80RAM:SpectrumMemory; VAR Pages: SpectrumPages):Integer;
VAR F:FILE;
    P:Pointer;
    AddHeaderSize:Word;
    BlockLen: Word;
    Page, SnapPage: Byte;
    HWMode:Byte;
    Offset:Word;
    m128k:Boolean;
    i: word;
    oldversion, compressed: Boolean;
    SnapLength:LongInt;
    PC_TEST: WORD;
    tmp: Byte;
BEGIN
 LoadZ80:=0; (* no error *)
 Assign(F,FileName);
 Reset(F,1);
 SnapLength:=FIleSize(F);
 if SnapLength<38 THEN BEGIN
                        LoadZ80:=2; (* Invalid file *)
                        Exit
                      END;
 Seek(F,6);
 BlockRead(F,PC_TEST,2);
 oldversion:=PC_TEST<>0;

 if oldversion then
 BEGIN
  Seek(F,12);
  BlockRead(F,tmp,1);
  if tmp=255 then tmp:=0;
  compressed:=tmp and 32 <> 0;
  Seek(F,30);
  if not compressed then
   begin
    if SnapLength<>(49152+30) THEN BEGIN
                        LoadZ80:=2; (* Invalid file *)
                        Exit
                      END
    else
     BlockRead(F,Z80RAM,SnapLength-30);
   end
  else
   begin
    GetMem(P,SnapLength-30);
    BlockRead(F,P^,SnapLength-30-4);
    IF not Decode(P^,Z80RAM,SnapLength-34,49152) THEN LoadZ80:=2;
    FreeMem(P,SnapLength-30);
   end;
  Exit;
 END;
 Seek(F,30);
 BlockRead(F,AddHeaderSize,2);
 IF (AddHeaderSize<>23) AND (AddHeaderSize<>54) AND (AddHeaderSize<>55)
                        THEN BEGIN
                             LoadZ80:=2; (* Invalid file *)
                             Exit
                        END;
 IF SnapLength<34+AddHeaderSize
                        THEN BEGIN
                             LoadZ80:=2; (* Invalid file *)
                             Exit
                        END;
 Seek(F,34);
 BlockRead(F,HWMode,1);
 IF ((AddHeaderSize=23) AND (HWMode>4)) THEN BEGIN
                                              LoadZ80:=3; (* No a 48K/128K file*)
                                              Exit
                                             END;
 IF ((AddHeaderSize=23) AND (HWMode>2)) OR
    ((AddHeaderSize=54) AND (HWMode>3)) OR
    ((AddHeaderSize=55) AND (HWMode>3)) THEN m128k:=true
 ELSE
     m128k:=false;
 if m128k then
    for i:=0 to 7 do getmem(Pages[i],16384);

 Seek(F,32+AddHeaderSize);
 WHILE NOT EOF(F) DO
  BEGIN
   IF SnapLength-FilePos(F)<4 THEN BEGIN LoadZ80:=2; Exit; END;
   BlockRead(F,BlockLen,2);
   BlockRead(F,Page,1);
   if blocklen=65535 then
    begin
     IF SnapLength-FilePos(F)<16384 THEN BEGIN LoadZ80:=2; Exit; END;
     GetMem(P,16384);
     BlockRead(F,P^,16384);
    end
   else
    begin
     IF SnapLength-FilePos(F)<BlockLen THEN BEGIN LoadZ80:=2; Exit; END;
     GetMem(P,BlockLen);
     BlockRead(F,P^,BlockLen);
    end;
   OffSet:=0;
   SnapPage:=255;
   if not m128k then
     CASE Page OF
           4:Offset:=32768;
           5:Offset:=49152;
           8:Offset:=16384;
           6,7:BEGIN
                Offset:=0;
               END
           ELSE BEGIN
                 LoadZ80:=3;
                 Exit
                END;
     END
   ELSE
     CASE Page OF
           3,4,5,6,7,8,9,10: SnapPage:=Page-3;
           ELSE BEGIN
                 LoadZ80:=3;
                 Exit
                END;
     END;

   IF not m128k and (Offset<>0) THEN  BEGIN
                       if BlockLen=65535 then
                           Move(P^,Z80RAM[Offset],16384)
                       else
                           if not Decode(P^,Z80RAM[Offset],BlockLen,16384) then
                            begin
                                 FreeMem(P,BlockLen);
                                 LoadZ80:=2;
                                 Exit;
                            end;
                      END;
   IF m128k and (SnapPage<>255) THEN  BEGIN
                       if BlockLen=65535 then
                          Move(P^,Pages[SnapPage]^,16384)
                       else
                          if not Decode(P^,Pages[SnapPage]^,BlockLen,16384) then
                          begin
                               FreeMem(P,BlockLen);
                               LoadZ80:=2;
                               Exit;
                          end;
                      END;

   IF BlockLen=65535
   THEN FreeMem(P,16384)
   ELSE FreeMem(P,BlockLen);
 END;
Close(F);
if m128k then LoadZ80:=1;
END;

END.