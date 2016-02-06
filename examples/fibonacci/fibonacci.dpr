program fibonacci;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  PasMP in '..\..\src\PasMP.pas';

var PasMPInstance:TPasMP;

function fibI(n:longint):longint;
var Last,Temporary:longint;
begin
 Last:=0;
 result:=1;
 dec(n);
 while n>0 do begin
  dec(n);
  Temporary:=result;
  inc(result,Last);
  Last:=Temporary;
 end;
end;

function fibR(n:longint):longint;
begin
 if n<2 then begin
  result:=n;
 end else begin
  result:=fibR(n-2)+fibR(n-1);
 end;
end;

type PfibRPJobData=^TfibRPJobData;
     TfibRPJobData=record
      Current:longint;
      Depth:longint;
      ReturnValue:longint;
     end;

procedure fibRPJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var JobData,NewJobData:PfibRPJobData;
    Jobs:array[0..1] of PPasMPJob;
begin
 JobData:=PfibRPJobData(pointer(@Job^.Data));
 if JobData^.Current<2 then begin
  JobData^.ReturnValue:=JobData^.Current;
 end else if (JobData^.Depth>8) or PasMPInstance.IsJobStackDepthLimitReached then begin
  JobData^.ReturnValue:=fibR(JobData^.Current);
 end else begin

  Jobs[0]:=PasMPInstance.Acquire(fibRPJobFunction,nil);
  NewJobData:=PfibRPJobData(pointer(@Jobs[0]^.Data));
  NewJobData^.Current:=JobData^.Current-2;
  NewJobData^.Depth:=JobData^.Depth+1;

  Jobs[1]:=PasMPInstance.Acquire(fibRPJobFunction,nil);
  NewJobData:=PfibRPJobData(pointer(@Jobs[1]^.Data));
  NewJobData^.Current:=JobData^.Current-1;
  NewJobData^.Depth:=JobData^.Depth+1;

  PasMPInstance.Invoke(Jobs); // Invoke combines Run, Wait and Release into a call

  JobData^.ReturnValue:=PfibRPJobData(pointer(@Jobs[0].Data))^.ReturnValue+PfibRPJobData(pointer(@Jobs[1].Data))^.ReturnValue;

 end;
end;

function fibRP(n:longint):longint;
var Job:PPasMPJob;
    JobData:PfibRPJobData;
begin
 Job:=PasMPInstance.Acquire(fibRPJobFunction);
 JobData:=PfibRPJobData(pointer(@Job^.Data));
 JobData^.Current:=n;
 JobData^.Depth:=0;
 PasMPInstance.Invoke(Job);
 result:=JobData^.ReturnValue;
end;

const N=45;

var Frequency,StartTime,EndTime:int64;
    i:longint;
begin
 PasMPInstance:=TPasMP.Create(-1,0,true,8);

 QueryPerformanceFrequency(Frequency);

 for i:=1 to 1 do begin
  write('                fibI (iterate): ');
  QueryPerformanceCounter(StartTime);
  write(fibI(N));
  QueryPerformanceCounter(EndTime);
  writeln(' in ',(EndTime-StartTime)/Frequency:1:8,'s');
 end;

 for i:=1 to 1 do begin
  write('              fibR (recursive): ');
  QueryPerformanceCounter(StartTime);
  write(fibR(N));
  QueryPerformanceCounter(EndTime);
  writeln(' in ',(EndTime-StartTime)/Frequency:1:8,'s');
 end;

 for i:=1 to 9 do begin
  PasMPInstance.Reset; // <= optional, triggers amongst other things the job queue memory pool garbage collector
  write('fibRP (parallelized recursive): ');
  QueryPerformanceCounter(StartTime);
  write(fibRP(N));
  QueryPerformanceCounter(EndTime);
  writeln(' in ',(EndTime-StartTime)/Frequency:1:8,'s');
 end;

 readln;

 PasMPInstance.Free;

end.
