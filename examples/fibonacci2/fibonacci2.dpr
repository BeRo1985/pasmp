program fibonacci2;
{$ifdef fpc}
 {$mode delphi}
{$endif}
{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  PasMP in '..\..\src\PasMP.pas';

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

type TfibRPJobTask=class(TPasMPJobTask)
      public
       fCurrent:longint;
       fDepth:longint;
       fReturnValue:longint;
       constructor Create(const Current,Depth:longint);
       procedure Run; override;
     end;

constructor TfibRPJobTask.Create(const Current,Depth:longint);
begin
 inherited Create;
 FreeOnRelease:=false; // don't free on release, because we do need the return value yet before freeing
 fCurrent:=Current;
 fDepth:=Depth;
end;

procedure TfibRPJobTask.Run;
var Jobs:array[0..1] of PPasMPJob;
    JobTasks:array[0..1] of TfibRPJobTask;
begin
 if fCurrent<2 then begin
  fReturnValue:=fCurrent;
 end else if fDepth>8 then begin
  fReturnValue:=fibR(fCurrent);
 end else begin
  JobTasks[0]:=TfibRPJobTask.Create(fCurrent-2,fDepth+1);
  try
   JobTasks[1]:=TfibRPJobTask.Create(fCurrent-1,fDepth+1);
   try
    GlobalPasMP.Invoke([GlobalPasMP.Acquire(JobTasks[0]),
                        GlobalPasMP.Acquire(JobTasks[1])]); // Invoke combines Run, Wait and Release into a single call, but
    // or just GlobalPasMP.Invoke([JobTasks[0],JobTasks[1]]);
    fReturnValue:=JobTasks[0].fReturnValue+JobTasks[1].fReturnValue;
   finally
    JobTasks[1].Free;
   end;
  finally
   JobTasks[0].Free;
  end;
 end;
end;

function fibRP(n:longint):longint;
var JobTask:TfibRPJobTask;
begin
 JobTask:=TfibRPJobTask.Create(n,0);
 try
  GlobalPasMP.Invoke(GlobalPasMP.Acquire(JobTask)); // Invoke combines Run, Wait and Release into a single call, but
  // or just GlobalPasMP.Invoke(JobTask);
  result:=JobTask.fReturnValue;
 finally
  JobTask.Free;
 end;
end;

const N=45;

var Frequency,StartTime,EndTime:int64;
    i:longint;
begin

 TPasMP.CreateGlobalInstance;

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
  GlobalPasMP.Reset; // <= optional per workload-frame, triggers amongst other things the job queue memory pool garbage collector
  write('fibRP (parallelized recursive): ');
  QueryPerformanceCounter(StartTime);
  write(fibRP(N));
  QueryPerformanceCounter(EndTime);
  writeln(' in ',(EndTime-StartTime)/Frequency:1:8,'s');
 end;

 readln;

end.
