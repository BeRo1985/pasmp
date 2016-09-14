program parallelfor;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$APPTYPE CONSOLE}

uses
{$ifdef unix}
  cthreads,
{$endif}
  SysUtils,
  PasMP in '..\..\src\PasMP.pas';

{$if defined(win32) or defined(win64) or defined(windows)}
procedure Sleep(ms:longword); stdcall; external 'kernel32.dll' name 'Sleep';
{$ifend}

const N=65536;

type PCells=^TCells;
     TCells=array[1..N] of boolean;

var FakeAtomicOperationMutex:TPasMPMutex;
    Index,Sum:longint;
    Cells:TCells;

{$ifndef fpc}
{$if not ((CompilerVersion>=20) and not defined(fpc))}
procedure ParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:longint;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:longint;
begin
 FakeAtomicOperationMutex.Acquire;
 try
  writeln(FromIndex,'..',ToIndex,' from thread #',ThreadIndex);
  inc(Sum,(ToIndex-FromIndex)+1);
 finally
  FakeAtomicOperationMutex.Release;
 end;
 for Index:=FromIndex to ToIndex do begin
  PCells(Data)^[Index]:=true;
 end;
 Sleep(100); // simulate some extra work load
end;
{$ifend}
{$else}
procedure ParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:longint;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt);
var Index:longint;
begin
 FakeAtomicOperationMutex.Acquire;
 try
  writeln(FromIndex,'..',ToIndex,' from thread #',ThreadIndex);
  inc(Sum,(ToIndex-FromIndex)+1);
 finally
  FakeAtomicOperationMutex.Release;
 end;
 for Index:=FromIndex to ToIndex do begin
  PCells(Data)^[Index]:=true;
 end;
 Sleep(100); // simulate some extra work load
end;
{$endif}

begin

 TPasMP.CreateGlobalInstance;

 FillChar(Cells,SizeOf(TCells),#0);

 Sum:=0;

 FakeAtomicOperationMutex:=TPasMPMutex.Create;
 try
{$ifndef fpc}
{$if (CompilerVersion>=20) and not defined(fpc)}
  GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(@Cells,
                                             1,
                                             N,
                                             procedure(const Job:PPasMPJob;const ThreadIndex:longint;const Data:pointer;const FromIndex,ToIndex:TPasMPNativeInt)
                                             var Index:longint;
                                             begin
                                              FakeAtomicOperationMutex.Acquire;
                                              try
                                               writeln(FromIndex,'..',ToIndex,' from thread #',ThreadIndex);
                                               inc(Sum,(ToIndex-FromIndex)+1);
                                              finally
                                               FakeAtomicOperationMutex.Release;
                                              end;
                                              for Index:=FromIndex to ToIndex do begin
                                               PCells(Data)^[Index]:=true;
                                              end;
                                              Sleep(100); // simulate some extra work load
                                             end,
                                             16,
                                             8)); // <= Invoke = Run+Wait+Release into a single call
{$else}
  GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(@Cells,1,N,ParallelForJobFunction,16,8)); // <= Invoke = Run+Wait+Release into a single call
{$ifend}
{$else}
  GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(@Cells,1,N,ParallelForJobFunction,16,8)); // <= Invoke = Run+Wait+Release into a single call
{$endif}
 finally
  FakeAtomicOperationMutex.Free;
 end;

 writeln(Sum,' should be ',N);

 Sum:=0;
 for Index:=1 to N do begin
  if Cells[Index] then begin
   inc(Sum);
  end;
 end;
 writeln(Sum,' should be also ',N);

 readln;

end.
