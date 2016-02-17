(******************************************************************************
 *                                   PasMP                                    *
 ******************************************************************************
 *                        Version 2016-02-17-20-01-0000                       *
 ******************************************************************************
 *                                zlib license                                *
 *============================================================================*
 *                                                                            *
 * Copyright (C) 2016, Benjamin Rosseaux (benjamin@rosseaux.de)               *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 *                                                                            *
 ******************************************************************************
 *                  General guidelines for code contributors                  *
 *============================================================================*
 *                                                                            *
 * 1. Make sure you are legally allowed to make a contribution under the zlib *
 *    license.                                                                *
 * 2. The zlib license header goes at the top of each source file, with       *
 *    appropriate copyright notice.                                           *
 * 3. After a pull request, check the status of your pull request on          *
      http://github.com/BeRo1985/pasmp                                        *
 * 4. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 2.6 *
 *    so don't use generics/templates, operator overloading and another newer *
 *    syntax features than Delphi 7 has support for that, but if needed, make *
 *    it out-ifdef-able.                                                      *
 * 5. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, *
 *    but if needed, make it out-ifdef-able.                                  *
 * 6. No use of third-party libraries/units as possible, but if needed, make  *
 *    it out-ifdef-able.                                                      *
 * 7. Try to use const when possible.                                         *
 * 8. Make sure to comment out writeln, used while debugging.                 *
 * 9. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32,     *
 *    x86-64, ARM, ARM64, etc.).                                              *
 * 10. Make sure the code runs on platforms with weak and strong memory       *
 *     models without any issues.                                             *
 *                                                                            *
 ******************************************************************************)
unit PasMP;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef CPUi386}
  {$define CPU386}
 {$endif}
 {$ifdef CPUamd64}
  {$define CPUx86_64}
 {$endif}
 {$ifdef CPU386}
  {$define CPUx86}
  {$define CPU32}
  {$asmmode intel}
 {$endif}
 {$ifdef CPUx86_64}
  {$define CPUx64}
  {$define CPU64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define CAN_INLINE}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
 {$if declared(RawByteString)}
  {$define HAS_TYPE_RAWBYTESTRING}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
 {$ifend}
 {$if declared(UTF8String)}
  {$define HAS_TYPE_UTF8STRING}
 {$else}
  {$undef HAS_TYPE_UTF8STRING}
 {$ifend}
 {$define HAS_GENERICS}
 {$define HAS_STATIC}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef CPU64}
  {$define CPU32}
 {$endif}
 {$ifdef CPUx64}
  {$define CPUx86_64}
  {$define CPU64}
 {$else}
  {$ifdef CPU386}
   {$define CPUx86}
   {$define CPU32}
  {$endif}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$ifdef conditionalexpressions}
  {$if declared(RawByteString)}
   {$define HAS_TYPE_RAWBYTESTRING}
  {$else}
   {$undef HAS_TYPE_RAWBYTESTRING}
  {$ifend}
  {$if declared(UTF8String)}
   {$define HAS_TYPE_UTF8STRING}
  {$else}
   {$undef HAS_TYPE_UTF8STRING}
  {$ifend}
 {$else}
  {$undef HAS_TYPE_RAWBYTESTRING}
  {$undef HAS_TYPE_UTF8STRING}
 {$endif}
 {$if CompilerVersion>=24}
  {$legacyifend on}
  {$define HAS_ATOMICS}
 {$ifend}
 {$if CompilerVersion>=20}
  {$define CAN_INLINE}
  {$define HAS_ANONYMOUS_METHODS}
  {$define HAS_GENERICS}
  {$define HAS_STATIC}
 {$ifend}
 {$if CompilerVersion>=25}
  {$define HAS_WEAK}
  {$define HAS_VOLATILE}
  {$define HAS_REF}
 {$ifend}
{$endif}
{$ifdef CPU386}
 {$define HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
{$endif}
{$ifdef CPUx86_64}
 {$define HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
{$endif}
{$ifdef CPUARM}
 {$define HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
{$endif}
{$ifdef Win32}
 {$define Windows}
{$endif}
{$ifdef Win64}
 {$define Windows}
{$endif}
{$ifdef WinCE}
 {$define Windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

{$undef UseXorShift128}

{$undef UseThreadLocalStorage}
{$undef UseThreadLocalStorageX8632}

{$ifdef PasMPUseAsStrictSingletonInstance}
{$if defined(Windows) and (defined(CPU386) or defined(CPUx86_64)) and not (defined(FPC) or defined(UseMultiplePasMPInstanceInstances))}
 // Delphi (under x86 Windows) has fast thread local storage handling (per nearly direct TEB access by reading fs:[0x18])
 {$define UseThreadLocalStorage}
 {$ifdef cpu386}
  {$define UseThreadLocalStorageX8632}
 {$endif}
 {$ifdef cpux86_64}
  {$define UseThreadLocalStorageX8664}
 {$endif}
{$else}
 // FreePascal has portable but unfortunately slow thread local storage handling (for example under Windows, over TLSGetIndex
 // calls etc. in FPC_THREADVAR_RELOCATE), so use here the bit faster thread ID hash table approach with less total CPU-cycle
 // count and less OS-API calls than with the FPC_THREADVAR_RELOCATE variant
{$ifend}
{$endif}

{$define PasMPUseWakeUpConditionVariable}

interface

uses {$ifdef Windows}
      Windows,MMSystem,
     {$else}
      {$ifdef fpc}
       {$ifdef Unix}
        {$ifdef usecthreads}
         cthreads,
        {$endif}
        BaseUnix,Unix,UnixType,PThreads,
        {$ifdef Linux}
         Linux,
        {$else}
         ctypes,sysctl,
        {$endif}
       {$endif}
      {$endif}
     {$endif}
     SysUtils,Classes,Math,SyncObjs;

const PasMPAllocatorPoolBucketBits=12;
      PasMPAllocatorPoolBucketSize=1 shl PasMPAllocatorPoolBucketBits;
      PasMPAllocatorPoolBucketMask=PasMPAllocatorPoolBucketSize-1;

      PasMPJobQueueStartSize=4096; // must be power of two

      PasMPJobWorkerThreadHashTableSize=4096;
      PasMPJobWorkerThreadHashTableMask=PasMPJobWorkerThreadHashTableSize-1;

      PasMPDefaultDepth=16;

      PasMPJobThreadIndexBits=16;
      PasMPJobThreadIndexSize=longword(longword(1) shl PasMPJobThreadIndexBits);
      PasMPJobThreadIndexMask=PasMPJobThreadIndexSize-1;

      PasMPJobFlagHasOwnerWorkerThread=longword(longword(1) shl (PasMPJobThreadIndexBits+1));
      PasMPJobFlagReleaseOnFinish=longword(longword(1) shl (PasMPJobThreadIndexBits+2));

      PasMPCPUCacheLineSize=64;

      PasMPOnceInit={$ifdef Linux}PTHREAD_ONCE_INIT{$else}0{$endif};

      PasMPVersionMajor=1000000;
      PasMPVersionMinor=1000;
      PasMPVersionRelease=1;

{$ifndef FPC}
      // Delphi evaluates every $IF-directive even if it is disabled by a surrounding, so it's then a error in Delphi, and for to avoid it, we define dummys here.
      FPC_VERSION=0;
      FPC_RELEASE=0;
      FPC_PATCH=0;
      FPC_FULLVERSION=(FPC_VERSION*10000)+(FPC_RELEASE*100)+(FPC_PATCH*1);
{$endif}

//    FPC_VERSION_PASMP=(FPC_VERSION*PasMPVersionMajor)+(FPC_RELEASE*PasMPVersionMinor)+(FPC_PATCH*PasMPVersionRelease);

{$ifndef Windows}
{$ifndef fpc}
      INFINITE=longword(-1);
{$endif}
{$endif}

type TPasMPAvailableCPUCores=array of longint;

     PPasMPPtrUInt=^TPasMPPtrUInt;
     PPasMPPtrInt=^TPasMPPtrInt;

{$ifdef fpc}
 {$undef OldDelphi}
     TPasMPUInt64=uint64;
     TPasMPPtrUInt=PtrUInt;
     TPasMPPtrInt=PtrInt;
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     TPasMPUInt64=uint64;
     TPasMPPtrUInt=NativeUInt;
     TPasMPPtrInt=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
  {$if CompilerVersion>=15.0}
     TPasMPUInt64=uint64;
  {$else}
     TPasMPUInt64=int64;
  {$ifend}
  {$ifdef CPU64}
     TPasMPPtrUInt=qword;
     TPasMPPtrInt=int64;
  {$else}
     TPasMPPtrUInt=longword;
     TPasMPPtrInt=longint;
  {$endif}
{$endif}

     PPasMPInt128=^TPasMPInt128;
     TPasMPInt128=record
{$ifdef BIG_ENDIAN}
      Hi,Lo:TPasMPUInt64;
{$else}
      Lo,Hi:TPasMPUInt64;
{$endif}
     end;

     PPasMPInt64=^TPasMPInt64;
     TPasMPInt64=record
      case boolean of
       false:(
{$ifdef BIG_ENDIAN}
        Hi,Lo:longword;
{$else}
        Lo,Hi:longword;
{$endif}
       );
       true:(
        Value:int64;
       );
     end;

     PPasMPTaggedPointer=^TPasMPTaggedPointer;
     TPasMPTaggedPointer=record
      case longint of
       0:(
        PointerValue:pointer;
        TagValue:TPasMPPtrUInt;
       );
       1:(
        Value:{$ifdef CPU64}TPasMPInt128{$else}TPasMPInt64{$endif};
       );
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPInterlocked=class
      public
       class function Increment(var Destination:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$ifdef CPU64}
       class function Increment(var Destination:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$endif}
       class function Decrement(var Destination:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$ifdef CPU64}
       class function Decrement(var Destination:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$endif}
       class function Add(var Destination:longint;const Value:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$ifdef CPU64}
       class function Add(var Destination:int64;const Value:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$endif}
       class function Sub(var Destination:longint;const Value:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$ifdef CPU64}
       class function Sub(var Destination:int64;const Value:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$endif}
       class function Exchange(var Destination:longint;const Source:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function Exchange(var Destination:longword;const Source:longword):longword; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$ifdef CPU64}
       class function Exchange(var Destination:int64;const Source:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function Exchange(var Destination:TPasMPUInt64;const Source:TPasMPUInt64):TPasMPUInt64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$endif}
       class function Exchange(var Destination:pointer;const Source:pointer):pointer; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function Exchange(var Destination:TObject;const Source:TObject):TObject; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function CompareExchange(var Destination:longint;const NewValue,Comperand:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function CompareExchange(var Destination:longword;const NewValue,Comperand:longword):longword; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$if defined(CPU64) or defined(CPU386) or defined(CPUARM)}
       class function CompareExchange(var Destination:int64;const NewValue,Comperand:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function CompareExchange(var Destination:TPasMPInt64;const NewValue,Comperand:TPasMPInt64):TPasMPInt64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function CompareExchange(var Destination:TPasMPUInt64;const NewValue,Comperand:TPasMPUInt64):TPasMPUInt64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$ifend}
{$if defined(CPU64) and defined(HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE)}
       class function CompareExchange(var Destination:TPasMPInt128;const NewValue,Comperand:TPasMPInt128):TPasMPInt128; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(fpc)}inline;{$ifend}
{$ifend}
       class function CompareExchange(var Destination:pointer;const NewValue,Comperand:pointer):pointer; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function CompareExchange(var Destination:TObject;const NewValue,Comperand:TObject):TObject; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function Read(var Source:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$if defined(CPU64) or defined(CPU386) or defined(CPUARM)}
       class function Read(var Source:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function Read(var Source:TPasMPInt64):TPasMPInt64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$if defined(CPU64) and defined(HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE)}
       class function Read(var Source:TPasMPInt128):TPasMPInt128; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(fpc)}inline;{$ifend}
{$ifend}
{$ifend}
       class function Write(var Destination:longint;const Source:longint):longint; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$if defined(CPU64) or defined(CPU386) or defined(CPUARM)}
       class function Write(var Destination:int64;const Source:int64):int64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
       class function Write(var Destination:TPasMPInt64;const Source:TPasMPInt64):TPasMPInt64; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(HAS_ATOMICS) or defined(fpc)}inline;{$ifend}
{$if defined(CPU64) and defined(HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE)}
       class function Write(var Destination:TPasMPInt128;const Source:TPasMPInt128):TPasMPInt128; overload; {$ifdef HAS_STATIC}static;{$endif}{$if defined(fpc)}inline;{$ifend}
{$ifend}
{$ifend}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPAtomic=class(TPasMPInterlocked);
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPMemoryBarrier=class
      public
       class procedure Read; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       class procedure ReadDependency; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       class procedure ReadWrite; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       class procedure Write; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       class procedure Sync; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPMemory=class
      public
       class procedure AllocateAlignedMemory(var p;Size:longint;Align:longint=PasMPCPUCacheLineSize); {$ifdef HAS_STATIC}static;{$endif}
       class procedure FreeAlignedMemory(const p); {$ifdef HAS_STATIC}static;{$endif}
       class procedure Barrier; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     TPasMP=class;

     PPasMPOnce=^TPasMPOnce;
     TPasMPOnce={$ifdef Linux}pthread_once_t{$else}longint{$endif};

     TPasMPOnceInitRoutine={$ifdef fpc}TProcedure{$else}procedure{$endif};

     TPasMPEvent=class(TEvent);

     TPasMPSimpleEvent=class(TPasMPEvent)
      public
       constructor Create;
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPCriticalSection=class(TCriticalSection)
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TRTLCriticalSection))-1] of byte;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPMutex=class(TSynchroObject)
{$ifdef Windows}
      private
       fMutex:THandle;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TRTLCriticalSection))-1] of byte;
{$else}
{$ifdef Unix}
      private
       fMutex:pthread_mutex_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_mutex_t))-1] of byte;
{$else}
      private
       fCriticalSection:TPasMPCriticalSection;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TPasMPCriticalSection))-1] of byte;
{$endif}
{$endif}
      public
       constructor Create; overload;
{$ifdef Unix}
       constructor Create(const lpMutexAttributes:pointer); overload;
{$endif}
{$ifdef Windows}
       constructor Create(const lpMutexAttributes:pointer;const bInitialOwner:boolean;const lpName:string); overload;
       constructor Create(const DesiredAccess:longword;const bInitialOwner:boolean;const lpName:string); overload;
{$endif}
       destructor Destroy; override;
       procedure Acquire; override;
       procedure Release; override;
{$ifdef Windows}
       property Mutex:THandle read fMutex;
{$else}
{$ifdef Unix}
       property Mutex:pthread_mutex_t read fMutex;
{$else}
       property CriticalSection:TPasMPCriticalSection read fCriticalSection;
{$endif}
{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPConditionVariableLock=class(TSynchroObject)
{$ifdef Windows}
      private
       fCriticalSection:TRTLCriticalSection;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TRTLCriticalSection))-1] of byte;
{$else}
{$ifdef Unix}
      private
       fMutex:pthread_mutex_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_mutex_t))-1] of byte;
{$else}
      private
       fCriticalSection:TPasMPCriticalSection;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TPasMPCriticalSection))-1] of byte;
{$endif}
{$endif}
      public
       constructor Create;
       destructor Destroy; override;
       procedure Acquire; override;
       procedure Release; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef Windows}
     PPasMPConditionVariableData=^TPasMPConditionVariableData;
     TPasMPConditionVariableData=pointer;
{$endif}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPConditionVariable=class
{$ifdef Windows}
      private
       fConditionVariable:TPasMPConditionVariableData;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TPasMPConditionVariableData))-1] of byte;
{$else}
{$ifdef unix}
      private
       fConditionVariable:pthread_cond_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_cond_t))-1] of byte;
{$else}
      private
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fWaitCounter:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fReleaseCounter:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fGenerationCounter:longint;
       fCriticalSection:TPasMPCriticalSection;
       fEvent:TPasMPEvent;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*3)+SizeOf(TPasMPCriticalSection)+SizeOf(TPasMPEvent)))-1] of byte;
{$endif}
{$endif}
      public
       constructor Create;
       destructor Destroy; override;
       procedure Signal; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure Broadcast; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       function Wait(const Lock:TPasMPConditionVariableLock;const dwMilliSeconds:longword=INFINITE):TWaitResult; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPSemaphore=class(TSynchroObject)
      private
       fInitialCount:longint;
       fMaximumCount:longint;
{$ifdef Windows}
       fHandle:THandle;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*2)+SizeOf(THandle)))-1] of byte;
{$else}
{$ifdef unix}
       fHandle:longint;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-(SizeOf(longint)*3))-1] of byte;
{$else}
{$define PasMPSemaphoreUseConditionVariable}
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fCurrentCount:longint;
{$ifdef PasMPSemaphoreUseConditionVariable}
       fConditionVariableLock:TPasMPConditionVariableLock;
       fConditionVariable:TPasMPConditionVariable;
{$else}
       fCriticalSection:TPasMPCriticalSection;
       fEvent:TPasMPEvent;
{$endif}
      protected
{$ifdef PasMPSemaphoreUseConditionVariable}
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*3)+SizeOf(TPasMPConditionVariableLock)+SizeOf(TPasMPConditionVariable)))-1] of byte;
{$else}
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*3)+SizeOf(TPasMPCriticalSection)+SizeOf(TPasMPEvent)))-1] of byte;
{$endif}
{$endif}
{$endif}
      public
       constructor Create(const InitialCount,MaximumCount:longint);
       destructor Destroy; override;
       procedure Acquire; overload; override;
       procedure Release; overload; override;
       function Acquire(const AcquireCount:longint):TWaitResult; reintroduce; overload;
       function Release(const ReleaseCount:longint):longint; reintroduce; overload;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPInvertedSemaphore=class(TSynchroObject)
      private
       fInitialCount:longint;
       fMaximumCount:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fCurrentCount:longint;
       fConditionVariableLock:TPasMPConditionVariableLock;
       fConditionVariable:TPasMPConditionVariable;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*3)+SizeOf(TPasMPConditionVariableLock)+SizeOf(TPasMPConditionVariable)))-1] of byte;
      public
       constructor Create(const InitialCount,MaximumCount:longint);
       destructor Destroy; override;
       procedure Acquire; overload; override; // Acquire a number of resource elements. It never blocks.
       procedure Release; overload; override; // Release a number of resource elements. It never blocks, but it may wake up waiting threads.
       function Acquire(const AcquireCount:longint;out Count:longint):longint; reintroduce; overload;
       function Release(const ReleaseCount:longint;out Count:longint):longint; reintroduce; overload;
       function Wait(const dwMilliSeconds:longword=INFINITE):TWaitResult; // Block until the inverted semaphore reaches zero
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef Windows}
     PPasMPSRWLock=^TPasMPSRWLock;
     TPasMPSRWLock=pointer;
{$endif}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPMultipleReaderSingleWriterLock=class
{$ifdef Windows}
      private
       fSRWLock:TPasMPSRWLock;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TPasMPSRWLock))-1] of byte;
{$else}
{$ifdef unix}
      private
       fReadWriteLock:pthread_rwlock_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_rwlock_t))-1] of byte;
{$else}
      private
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fReaders:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fWriters:longint;
       fConditionVariableLock:TPasMPConditionVariableLock;
       fConditionVariable:TPasMPConditionVariable;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*2)+SizeOf(TPasMPConditionVariableLock)+SizeOf(TPasMPConditionVariable)))-1] of byte;
{$endif}
{$endif}
      public
       constructor Create;
       destructor Destroy; override;
       procedure AcquireRead; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       function TryAcquireRead:boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure ReleaseRead; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure AcquireWrite; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       function TryAcquireWrite:boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure ReleaseWrite; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure ReadToWrite; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure WriteToRead; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPSlimReaderWriterLock=class(TSynchroObject)
{$ifdef Windows}
      private
       fSRWLock:TPasMPSRWLock;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(TPasMPSRWLock))-1] of byte;
{$else}
{$ifdef unix}
      private
       fReadWriteLock:pthread_rwlock_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_rwlock_t))-1] of byte;
{$else}
      private
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fCount:longint;
       fConditionVariableLock:TPasMPConditionVariableLock;
       fConditionVariable:TPasMPConditionVariable;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-(SizeOf(longint)+SizeOf(TPasMPConditionVariableLock)+SizeOf(TPasMPConditionVariable)))-1] of byte;
{$endif}
{$endif}
      public
       constructor Create;
       destructor Destroy; override;
       procedure Acquire; override;
       function TryAcquire:boolean;
       procedure Release; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPSpinLock=class(TSynchroObject)
{$ifdef unix}
      private
       fSpinLock:pthread_spinlock_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_spinlock_t))-1] of byte;
{$else}
      private
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fState:longint;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte;
{$endif}
      public
       constructor Create;
       destructor Destroy; override;
       procedure Acquire; override;
       function TryAcquire:longbool; {$if not defined(Unix)}{$if defined(cpu386) or defined(cpux86_64)}register;{$else}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}{$ifend}{$ifend}
       procedure Release; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPBarrier=class
{$ifdef unix}
      private
       fBarrier:pthread_barrier_t;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-SizeOf(pthread_barrier_t))-1] of byte;
{$else}
      private
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fCount:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fTotal:longint;
       fConditionVariableLock:TPasMPConditionVariableLock;
       fConditionVariable:TPasMPConditionVariable;
      protected
       fCacheLineFillUp:array[0..(PasMPCPUCacheLineSize-((SizeOf(longint)*2)+SizeOf(TPasMPConditionVariableLock)+SizeOf(TPasMPConditionVariable)))-1] of byte;
{$endif}
      public
       constructor Create(const Count:longint);
       destructor Destroy; override;
       function Wait:boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     PPasMPInterlockedStackEntry=^TPasMPInterlockedStackEntry;
     TPasMPInterlockedStackEntry=pointer;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     // The lock-free variant is based on the idea behind the concept of the internal workings of the "Interlocked Singly Linked Lists" Windows API, just stripped by the Depth stuff
     // The lock-based variant is based of my head
     TPasMPInterlockedStack=class // only for PasMP internal usage
      private
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
       fHead:PPasMPTaggedPointer;
{$else}
       fCriticalSection:TPasMPCriticalSection;
       fHead:pointer;
{$endif}
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear; {$ifdef CAN_INLINE}inline;{$endif}
       function IsEmpty:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       function Push(const Item:pointer):pointer; {$ifdef CAN_INLINE}inline;{$endif}
       function Pop:pointer; {$ifdef CAN_INLINE}inline;{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     PPasMPInterlockedQueueNode=^TPasMPInterlockedQueueNode;
     TPasMPInterlockedQueueNode=record
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
      Previous:TPasMPTaggedPointer;
      Next:TPasMPTaggedPointer;
{$else}
      Next:pointer;
{$endif}
      Data:record
       // Empty
      end;
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     // The lock-free variant is based on http://people.csail.mit.edu/edya/publications/OptimisticFIFOQueue-journal.pdf
     // The lock-based variant is based on the two-lock concurrent queue
     TPasMPInterlockedQueue=class // only for PasMP internal usage
      private
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
       fHead:PPasMPTaggedPointer;
       fTail:PPasMPTaggedPointer;
{$else}
       fHeadCriticalSection:TPasMPCriticalSection;
       fTailCriticalSection:TPasMPCriticalSection;
       fHead:PPasMPInterlockedQueueNode;
       fTail:PPasMPInterlockedQueueNode;
{$endif}
       fItemSize:longint;
       fInternalNodeSize:longint;
      public
       constructor Create(ItemSize:longint);
       destructor Destroy; override;
       procedure Clear; {$ifdef CAN_INLINE}inline;{$endif}
       function IsEmpty:boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure Enqueue(const Item); {$ifdef CAN_INLINE}inline;{$endif}
       function Dequeue(out Item):boolean; {$ifdef CAN_INLINE}inline;{$endif}
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     TPasMPInterlockedHashTableHash=longword;

     PPasMPInterlockedHashTableItem=^TPasMPInterlockedHashTableItem;
     TPasMPInterlockedHashTableItem=record
      case longint of
       0:(
        Lock:longint;
        State:longint;
        Hash:TPasMPInterlockedHashTableHash;
        Data:record
         // Empty
        end;
       );
       1:(
        LockState:TPasMPInt64;
       );
     end;

     PPasMPInterlockedHashTableState=^TPasMPInterlockedHashTableState;
     TPasMPInterlockedHashTableState=record
      case longint of
       0:(
        Previous:PPasMPInterlockedHashTableState;
        Next:PPasMPInterlockedHashTableState;
        ReferenceCounter:longint;
        Version:longint;
        Size:longint;
        Mask:longint;
        LogSize:longint;
        Count:longint;
        Lock:longint;
        Items:pointer;
       );
       1:(
        FillUp:array[0..(PasMPCPUCacheLineSize*(SizeOf(TPasMPPtrUInt) div SizeOf(longword)))-1] of byte;
       );
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     // A thread-safe hash table with open addressing and double hashing
     // The read operation is almost lock-free until the read-acquisition of the multiple-reader-single-writer-lock of a hash item,
     // since a item value can larger than one and two native maschine words
     // The write operations are almost multiple-reader-single-writer-lock-based
     // Why not complete lock-free? => Because TPasMPInterlockedHashTable should be universal usable independently by the key and
     // value data types and also key-and-value-object-reference-counting-free as much as possble.
     TPasMPInterlockedHashTable=class // only for PasMP internal usage
      private
       fCriticalSection:TPasMPCriticalSection;
       fLock:longint;
       fItemSize:longint;
       fInternalItemSize:longint;
       fFirstState:PPasMPInterlockedHashTableState;
       fLastState:PPasMPInterlockedHashTableState;
       function CreateState:PPasMPInterlockedHashTableState;
       procedure FreeState(const State:PPasMPInterlockedHashTableState);
       function AcquireState:PPasMPInterlockedHashTableState; {$ifdef CAN_INLINE}inline;{$endif}
       procedure ReleaseState(const State:PPasMPInterlockedHashTableState); {$ifdef CAN_INLINE}inline;{$endif}
       procedure Clear;
       function GetKeyValue(const Key,Value:pointer):boolean;
       function SetKeyValueOnState(const CurrentState:PPasMPInterlockedHashTableState;const Key,Value:pointer):boolean;
       procedure Grow;
       function SetKeyValue(const Key,Value:pointer):boolean;
       function DeleteKey(const Key:pointer):boolean;
      protected
       procedure InitializeItem(const Data:pointer); virtual;
       procedure FinalizeItem(const Data:pointer); virtual;
       procedure CopyItem(const Source,Destination:pointer); virtual;
       procedure GetKey(const Data,Key:pointer); virtual;
       procedure SetKey(const Data,Key:pointer); virtual;
       procedure GetValue(const Data,Value:pointer); virtual;
       procedure SetValue(const Data,Value:pointer); virtual;
       function HashKey(const Key:pointer):TPasMPInterlockedHashTableHash; virtual;
       function CompareKey(const Data,Key:pointer):boolean; virtual;
      public
       constructor Create(const ItemSize:longint);
       destructor Destroy; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPSingleProducerSingleConsumerRingBuffer=class
      protected
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fReadIndex:longint;
       fCacheLineFillUp0:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte; // for to force fReadIndex and fWriteIndex to different CPU cache lines
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fWriteIndex:longint;
       fCacheLineFillUp1:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte; // for to force fWriteIndex and fData to different CPU cache lines
       fData:array of byte;
       fSize:longint;
       fCacheLineFillUp2:array[0..(PasMPCPUCacheLineSize-(SizeOf(pointer)+SizeOf(longint)))-1] of byte; // as CPU cache line alignment
      public
       constructor Create(const Size:longint);
       destructor Destroy; override;
       function Read(const Buffer:pointer;Bytes:longint):longint;
       function TryRead(const Buffer:pointer;Bytes:longint):longint;
       function ReadAsMuchAsPossible(const Buffer:pointer;Bytes:longint):longint;
       function Write(const Buffer:pointer;Bytes:longint):longint;
       function TryWrite(const Buffer:pointer;Bytes:longint):longint;
       function WriteAsMuchAsPossible(const Buffer:pointer;Bytes:longint):longint;
       function AvailableForRead:longint;
       function AvailableForWrite:longint;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPSingleProducerSingleConsumerBoundedQueue=class
      protected
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fReadIndex:longint;
       fCacheLineFillUp0:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte; // for to force fReadIndex and fWriteIndex to different CPU cache lines
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fWriteIndex:longint;
       fCacheLineFillUp1:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte; // for to force fWriteIndex and fData to different CPU cache lines
       fData:array of byte;
       fMaximalCount:longint;
       fItemSize:longint;
       fCacheLineFillUp2:array[0..(PasMPCPUCacheLineSize-(SizeOf(pointer)+SizeOf(longint)))-1] of byte; // as CPU cache line alignment
      public
       constructor Create(const MaximalCount,ItemSize:longint);
       destructor Destroy; override;
       function Enqueue(const Item):boolean;
       function Dequeue(out Item):boolean;
       function AvailableForEnqueue:longint;
       function AvailableForDequeue:longint;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef HAS_GENERICS}
{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>=class
      protected
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fReadIndex:longint;
       fCacheLineFillUp0:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte; // for to force fReadIndex and fWriteIndex to different CPU cache lines
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fWriteIndex:longint;
       fCacheLineFillUp1:array[0..(PasMPCPUCacheLineSize-SizeOf(longint))-1] of byte; // for to force fWriteIndex and fData to different CPU cache lines
       fData:array of T;
       fMaximalCount:longint;
       fCacheLineFillUp2:array[0..(PasMPCPUCacheLineSize-(SizeOf(pointer)+SizeOf(longint)))-1] of byte; // as CPU cache line alignment
      public
       constructor Create(const MaximalCount:longint);
       destructor Destroy; override;
       function Enqueue(const Item:T):boolean;
       function Dequeue(out Item:T):boolean;
       function AvailableForEnqueue:longint;
       function AvailableForDequeue:longint;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}
{$endif}

     PPasMPBoundedStackItem=^TPasMPBoundedStackItem;
     TPasMPBoundedStackItem=record
      Next:TPasMPInterlockedStackEntry;
      Data:record
       // Empty
      end;
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPBoundedStack=class
      private
       fStack:TPasMPInterlockedStack;
       fFree:TPasMPInterlockedStack;
       fData:pointer;
       fMaximalCount:longint;
       fItemSize:longint;
       fInternalItemSize:longint;
      public
       constructor Create(const MaximalCount,ItemSize:longint);
       destructor Destroy; override;
       function IsEmpty:boolean;
       function IsFull:boolean;
       function Push(const Item):boolean;
       function Pop(out Item):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef HAS_GENERICS}
{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPBoundedTypedStack<T>=class
      private
       type PPasMPBoundedTypedStackItem=^TPasMPBoundedTypedStackItem;
            TPasMPBoundedTypedStackItem=record
             Next:TPasMPInterlockedStackEntry;
             Data:T;
            end;
      private
       fStack:TPasMPInterlockedStack;
       fFree:TPasMPInterlockedStack;
       fData:pointer;
       fMaximalCount:longint;
       fInternalItemSize:longint;
      public
       constructor Create(const MaximalCount:longint);
       destructor Destroy; override;
       function IsEmpty:boolean;
       function IsFull:boolean;
       function Push(const Item:T):boolean;
       function Pop(out Item:T):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}
{$endif}

     PPasMPUnboundedStackItem=^TPasMPUnboundedStackItem;
     TPasMPUnboundedStackItem=record
      Next:TPasMPInterlockedStackEntry;
      Data:record
       // Empty
      end;
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPUnboundedStack=class
      private
       fStack:TPasMPInterlockedStack;
       fItemSize:longint;
      public
       constructor Create(const ItemSize:longint);
       destructor Destroy; override;
       function IsEmpty:boolean;
       function Push(const Item):boolean;
       function Pop(out Item):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef HAS_GENERICS}
{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPUnboundedTypedStack<T>=class
      private
       type PPasMPUnboundedTypedStackItem=^TPasMPUnboundedTypedStackItem;
            TPasMPUnboundedTypedStackItem=record
             Next:TPasMPInterlockedStackEntry;
             Data:T;
            end;
      private
       fStack:TPasMPInterlockedStack;
       fItemSize:longint;
      public
       constructor Create;
       destructor Destroy; override;
       function IsEmpty:boolean;
       function Push(const Item:T):boolean;
       function Pop(out Item:T):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}
{$endif}

     PPasMPBoundedQueueItem=^TPasMPBoundedQueueItem;
     TPasMPBoundedQueueItem=record
      Next:TPasMPInterlockedStackEntry;
      Data:record
       // Empty
      end;
     end;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPBoundedQueue=class
      private
       fQueue:TPasMPInterlockedQueue;
       fFree:TPasMPInterlockedStack;
       fData:pointer;
       fMaximalCount:longint;
       fItemSize:longint;
       fInternalItemSize:longint;
      public
       constructor Create(const MaximalCount,ItemSize:longint);
       destructor Destroy; override;
       function IsEmpty:boolean;
       function IsFull:boolean;
       function Enqueue(const Item):boolean;
       function Dequeue(out Item):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef HAS_GENERICS}
{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPBoundedTypedQueue<T>=class
      private
       type PPasMPBoundedTypedQueueItem=^TPasMPBoundedTypedQueueItem;
            TPasMPBoundedTypedQueueItem=record
             Next:TPasMPInterlockedStackEntry;
             Data:T;
            end;
      private
       fQueue:TPasMPInterlockedQueue;
       fFree:TPasMPInterlockedStack;
       fData:pointer;
       fMaximalCount:longint;
       fInternalItemSize:longint;
      public
       constructor Create(const MaximalCount:longint);
       destructor Destroy; override;
       function IsEmpty:boolean;
       function IsFull:boolean;
       function Enqueue(const Item:T):boolean;
       function Dequeue(out Item:T):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}
{$endif}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPUnboundedQueue=class
      private
       fQueue:TPasMPInterlockedQueue;
       fItemSize:longint;
      public
       constructor Create(const ItemSize:longint);
       destructor Destroy; override;
       function IsEmpty:boolean;
       procedure Enqueue(const Item);
       function Dequeue(out Item):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$ifdef HAS_GENERICS}
{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPUnboundedTypedQueue<T>=class
      private
       type PPasMPUnboundedTypedQueueItem=^TPasMPUnboundedTypedQueueItem;
            TPasMPUnboundedTypedQueueItem=record
             Data:T;
            end;
      private
       fQueue:TPasMPInterlockedQueue;
      public
       constructor Create;
       destructor Destroy; override;
       function IsEmpty:boolean;
       procedure Enqueue(const Item:T);
       function Dequeue(out Item:T):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}
{$endif}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPHashTable=class(TPasMPInterlockedHashTable)
      private
       fKeySize:longint;
       fValueSize:longint;
       fItemSize:longint;
      protected
       procedure InitializeItem(const Data:pointer); override;
       procedure FinalizeItem(const Data:pointer); override;
       procedure CopyItem(const Source,Destination:pointer); override;
       procedure GetKey(const Data,Key:pointer); override;
       procedure SetKey(const Data,Key:pointer); override;
       procedure GetValue(const Data,Value:pointer); override;
       procedure SetValue(const Data,Value:pointer); override;
       function HashKey(const Key:pointer):TPasMPInterlockedHashTableHash; override;
       function CompareKey(const Data,Key:pointer):boolean; override;
      public
       constructor Create(const KeySize,ValueSize:longint);
       destructor Destroy; override;
       function GetKeyValue(const Key;out Value):boolean;
       function SetKeyValue(const Key,Value):boolean;
       function DeleteKey(const Key):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPStringHashTable=class(TPasMPInterlockedHashTable)
      private
       fKeySize:longint;
       fValueSize:longint;
       fItemSize:longint;
      protected
       procedure InitializeItem(const Data:pointer); override;
       procedure FinalizeItem(const Data:pointer); override;
       procedure CopyItem(const Source,Destination:pointer); override;
       procedure GetKey(const Data,Key:pointer); override;
       procedure SetKey(const Data,Key:pointer); override;
       procedure GetValue(const Data,Value:pointer); override;
       procedure SetValue(const Data,Value:pointer); override;
       function HashKey(const Key:pointer):TPasMPInterlockedHashTableHash; override;
       function CompareKey(const Data,Key:pointer):boolean; override;
      public
       constructor Create(const ValueSize:longint);
       destructor Destroy; override;
       function GetKeyValue(const Key:string;out Value):boolean;
       function SetKeyValue(const Key:string;const Value):boolean;
       function DeleteKey(const Key:string):boolean;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPThread=class(TThread);
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     PPasMPJob=^TPasMPJob;

{$ifdef HAS_ANONYMOUS_METHODS}
     TPasMPJobReferenceProcedure=reference to procedure(const Job:PPasMPJob;const ThreadIndex:longint);
{$endif}

     TPasMPJobProcedure=procedure(const Job:PPasMPJob;const ThreadIndex:longint);

     TPasMPJobMethod=procedure(const Job:PPasMPJob;const ThreadIndex:longint) of object;

{$ifdef HAS_ANONYMOUS_METHODS}
     TPasMPParallelForReferenceProcedure=reference to procedure(const Job:PPasMPJob;const ThreadIndex:longint;const Data:pointer;const FromIndex,ToIndex:longint);
{$endif}

     TPasMPParallelForProcedure=procedure(const Job:PPasMPJob;const ThreadIndex:longint;const Data:pointer;const FromIndex,ToIndex:longint);

     TPasMPParallelForMethod=procedure(const Job:PPasMPJob;const ThreadIndex:longint;const Data:pointer;const FromIndex,ToIndex:longint) of object;

     TPasMPParallelSortCompareFunction=function(const a,b:pointer):longint;

     TPasMPJobWorkerThread=class;

     TPasMPJob=record
      case longint of
       0:(                                          // 32 / 64 bit
        Method:TMethod;                             //  8 / 16 => 2x pointers
        ParentJob:PPasMPJob;                        //  4 /  8 => 1x pointer
        InternalData:longword;                      //  4 /  4 => 1x 32-bit unsigned integer (lower bits => owner worker thread index, higher bits => flags)
        State:longint;                              //  4 /  4 => 1x 32-bit signed integer (if it's below 0, then the job is completed, otherwise it's a 0-based unfinished job counter)
        Data:pointer;                               // ------- => just a dummy variable as struct field offset anchor
       );                                           // 20 / 32
       1:(
        Next:TPasMPInterlockedStackEntry;
       );
       2:(
        // for 32-bit Destinations: use one whole cache line (1x 64 bytes = 16x 32-bit pointers/integers) to avoid false sharing (1 cache line => 64 bytes on the most CPUs) and also to have some free place for meta data
        // for 64-bit Destinations: use two whole cache lines (2x 64 bytes = 16x 64-bit pointers/integers) to avoid false sharing (1 cache line => 64 bytes on the most CPUs) and also to have some free place for meta data
        // and so on . . .
        FillUp:array[0..(PasMPCPUCacheLineSize*(SizeOf(TPasMPPtrUInt) div SizeOf(longword)))-1] of byte;
       );
     end;

     TPPasMPJobs=array of PPasMPJob;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPJobTask=class
      private
       fFreeOnRelease:boolean;
       fJob:PPasMPJob;
       fThreadIndex:longint;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Run; virtual;
       function Split:TPasMPJobTask; virtual;
       function PartialPop:TPasMPJobTask; virtual;
       function Spread:boolean; virtual;
       property FreeOnRelease:boolean read fFreeOnRelease write fFreeOnRelease;
       property Job:PPasMPJob read fJob;
       property ThreadIndex:longint read fThreadIndex;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     PPasMPJobAllocatorMemoryPoolBucket=^TPasMPJobAllocatorMemoryPoolBucket;
     TPasMPJobAllocatorMemoryPoolBucket=array[0..PasMPAllocatorPoolBucketSize-1] of TPasMPJob;

     PPPasMPJobAllocatorMemoryPoolBuckets=^TPPasMPJobAllocatorMemoryPoolBuckets;
     TPPasMPJobAllocatorMemoryPoolBuckets=array of PPasMPJobAllocatorMemoryPoolBucket;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPJobAllocator=class
      private
       fJobWorkerThread:TPasMPJobWorkerThread;
       fFreeJobs:TPasMPInterlockedStack;
       fMemoryPoolBuckets:TPPasMPJobAllocatorMemoryPoolBuckets;
       fCountMemoryPoolBuckets:longint;
       fCountAllocatedJobs:longint;
       procedure AllocateNewBuckets(const NewCountMemoryPoolBuckets:longint);
       function AllocateJob:PPasMPJob; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure FreeJobs; {$ifdef CAN_INLINE}inline;{$endif}
       procedure FreeJob(const Job:PPasMPJob);
      public
       constructor Create(const AJobWorkerThread:TPasMPJobWorkerThread);
       destructor Destroy; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPWorkerSystemThread=class(TPasMPThread)
      private
       fJobWorkerThread:TPasMPJobWorkerThread;
      protected
       procedure Execute; override;
      public
       constructor Create(const AJobWorkerThread:TPasMPJobWorkerThread);
       destructor Destroy; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     TPasMPJobQueueJobs=array of PPasMPJob;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPJobQueue=class
      private
       fPasMPInstance:TPasMP;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fQueueLockState:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fQueueSize:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fQueueMask:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fQueueBottom:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fQueueTop:longint;
       {$ifdef HAS_VOLATILE}[volatile]{$endif}fQueueJobs:TPasMPJobQueueJobs;
       function HasJobs:boolean;
       procedure Resize(const QueueBottom,QueueTop:longint);
       procedure PushJob(const AJob:PPasMPJob);
       function PopJob:PPasMPJob;
       function StealJob:PPasMPJob;
      public
       constructor Create(const APasMPInstance:TPasMP);
       destructor Destroy; override;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPJobWorkerThread=class
      private
       fPasMPInstance:TPasMP;
       fNext:TPasMPJobWorkerThread;
       fThreadIndex:longint;
{$ifndef UseThreadLocalStorage}
       fThreadID:{$ifdef fpc}TThreadID{$else}longword{$endif};
{$endif}
       fSystemThread:TPasMPWorkerSystemThread;
       fIsReadyEvent:TPasMPEvent;
       fJobAllocator:TPasMPJobAllocator;
       fJobQueue:TPasMPJobQueue;
{$ifdef UseXorShift128}
       fXorShift128x:longword;
       fXorShift128y:longword;
       fXorShift128z:longword;
       fXorShift128w:longword;
{$else}
{$ifdef CPU64}
       fXorShift64:TPasMPUInt64;
{$else}
       fXorShift32:longword;
{$endif}
{$endif}
       procedure ThreadInitialization;
       function GetJob:PPasMPJob;
       procedure ThreadProc;
      public
       constructor Create(const APasMPInstance:TPasMP;const AThreadIndex:longint);
       destructor Destroy; override;
       property ThreadIndex:longint read fThreadIndex;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

     TPasMPJobWorkerThreadHashTable=array[0..PasMPJobWorkerThreadHashTableSize-1] of TPasMPJobWorkerThread;

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMPScope=class
      private
       fPasMPInstance:TPasMP;
       fWaitCalled:longbool;
       fJobs:TPPasMPJobs;
       fCountJobs:longint;
      public
       constructor Create(const APasMPInstance:TPasMP);
       destructor Destroy; override;
       procedure Run(const Job:PPasMPJob); overload;
       procedure Run(const Jobs:array of PPasMPJob); overload;
       procedure Run(const JobTask:TPasMPJobTask); overload;
       procedure Run(const JobTasks:array of TPasMPJobTask); overload;
       procedure Wait;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

{$if defined(fpc) and (fpc_version>=3)}{$push}{$optimization noorderfields}{$ifend}
     TPasMP=class
      private
       fAvailableCPUCores:TPasMPAvailableCPUCores;
       fDoCPUCorePinning:longbool;
       fSleepingOnIdle:longbool;
       fFPUExceptionMask:TFPUExceptionMask;
       fFPUPrecisionMode:TFPUPrecisionMode;
       fFPURoundingMode:TFPURoundingMode;
       fJobWorkerThreads:array of TPasMPJobWorkerThread;
       fCountJobWorkerThreads:longint;
       fSleepingJobWorkerThreads:longint;
       fWorkingJobWorkerThreads:longint;
       fSystemIsReadyEvent:TPasMPEvent;
{$ifdef PasMPUseWakeUpConditionVariable}
       fWakeUpCounter:longint;
       fWakeUpConditionVariableLock:TPasMPConditionVariableLock;
       fWakeUpConditionVariable:TPasMPConditionVariable;
{$else}
       fWakeUpEvent:TPasMPEvent;
{$endif}
       fCriticalSection:TPasMPCriticalSection;
       fJobAllocatorCriticalSection:TPasMPCriticalSection;
       fJobAllocator:TPasMPJobAllocator;
       fJobQueue:TPasMPJobQueue;
{$ifndef UseThreadLocalStorage}
       fJobWorkerThreadHashTableCriticalSection:TPasMPCriticalSection;
       fJobWorkerThreadHashTable:TPasMPJobWorkerThreadHashTable;
{$endif}
       class procedure DestroyGlobalInstance;
       class function GetThreadIDHash(ThreadID:{$ifdef fpc}TThreadID{$else}longword{$endif}):longword; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       class function RoundUpToPowerOfTwo(x:TPasMPPtrUInt):TPasMPPtrUInt; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       class function RoundUpToMask(x,m:TPasMPPtrUInt):TPasMPPtrUInt; {$ifdef HAS_STATIC}static;{$endif}{$ifdef CAN_INLINE}inline;{$endif}
       function GetJobWorkerThread:TPasMPJobWorkerThread; {$ifndef UseThreadLocalStorage}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}{$endif}
       procedure WaitForWakeUp;
       procedure WakeUpAll;
       function CanSpread:boolean;
       function GlobalAllocateJob:PPasMPJob;
       procedure GlobalFreeJob(const Job:PPasMPJob);
       function AllocateJob(const MethodCode,MethodData,Data:pointer;const ParentJob:PPasMPJob;const Flags:longword):PPasMPJob; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure FinishJobRelease(Job:PPasMPJob); {$if defined(cpu386) or defined(cpux86_64)}register;{$ifend}
       procedure FinishJob(Job:PPasMPJob); {$if defined(cpu386) or defined(cpux86_64)}register;{$ifend}
       procedure ExecuteJobTask(const Job:PPasMPJob;const JobWorkerThread:TPasMPJobWorkerThread;const ThreadIndex:longint); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure ExecuteJob(const Job:PPasMPJob;const JobWorkerThread:TPasMPJobWorkerThread); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef HAS_ANONYMOUS_METHODS}
       procedure JobReferenceProcedureJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelForJobReferenceProcedureProcess(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelForJobReferenceProcedureFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelForStartJobReferenceProcedureFunction(const Job:PPasMPJob;const ThreadIndex:longint);
{$endif}
       procedure ParallelForJobFunctionProcess(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelForStartJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelDirectIntroSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelIndirectIntroSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelDirectMergeSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelDirectMergeSortRootJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelIndirectMergeSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
       procedure ParallelIndirectMergeSortRootJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
      public
       constructor Create(const MaxThreads:longint=-1;const ThreadHeadRoomForForeignTasks:longint=0;const DoCPUCorePinning:boolean=true;const SleepingOnIdle:boolean=true);
       destructor Destroy; override;
       class function CreateGlobalInstance:TPasMP;
       class function GetGlobalInstance:TPasMP; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       class function GetCountOfHardwareThreads(var AvailableCPUCores:TPasMPAvailableCPUCores):longint; {$ifdef HAS_STATIC}static;{$endif}
       class procedure Yield; {$ifdef HAS_STATIC}static;{$endif}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       class function Once(var OnceControl:TPasMPOnce;const InitRoutine:TPasMPOnceInitRoutine):boolean; {$ifdef Linux}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}{$endif}
       class function IsJobCompleted(const Job:PPasMPJob):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       class function IsJobValid(const Job:PPasMPJob):boolean; {$ifdef CAN_INLINE}inline;{$endif}
       procedure Reset;
       function CreateScope:TPasMPScope; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef HAS_ANONYMOUS_METHODS}
       function Acquire(const JobReferenceProcedure:TPasMPJobReferenceProcedure;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob; overload;
{$endif}
       function Acquire(const JobProcedure:TPasMPJobProcedure;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob; overload;
       function Acquire(const JobMethod:TPasMPJobMethod;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob; overload;
       function Acquire(const JobTask:TPasMPJobTask;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob; overload;
       procedure Release(const Job:PPasMPJob); overload; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure Release(const Jobs:array of PPasMPJob); overload;
       procedure Run(const Job:PPasMPJob); overload; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
       procedure Run(const Jobs:array of PPasMPJob); overload;
       procedure Wait(const Job:PPasMPJob); overload;
       procedure Wait(const Jobs:array of PPasMPJob); overload;
       procedure RunWait(const Job:PPasMPJob); overload; {$ifdef CAN_INLINE}inline;{$endif}
       procedure RunWait(const Jobs:array of PPasMPJob); overload;
       procedure WaitRelease(const Job:PPasMPJob); overload; {$ifdef CAN_INLINE}inline;{$endif}
       procedure WaitRelease(const Jobs:array of PPasMPJob); overload;
       procedure Invoke(const Job:PPasMPJob); overload; {$ifdef CAN_INLINE}inline;{$endif}
       procedure Invoke(const Jobs:array of PPasMPJob); overload;
       procedure Invoke(const JobTask:TPasMPJobTask); overload; {$ifdef CAN_INLINE}inline;{$endif}
       procedure Invoke(const JobTasks:array of TPasMPJobTask); overload;
{$ifdef HAS_ANONYMOUS_METHODS}
       function ParallelFor(const Data:pointer;const FirstIndex,LastIndex:longint;const ParallelForReferenceProcedure:TPasMPParallelForReferenceProcedure;const Granularity:longint=1;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob; overload;
{$endif}
       function ParallelFor(const Data:pointer;const FirstIndex,LastIndex:longint;const ParallelForProcedure:TPasMPParallelForProcedure;const Granularity:longint=1;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob; overload;
       function ParallelFor(const Data:pointer;const FirstIndex,LastIndex:longint;const ParallelForMethod:TPasMPParallelForMethod;const Granularity:longint=1;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob; overload;
       function ParallelDirectIntroSort(const Items:pointer;const Left,Right,ElementSize:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
       function ParallelIndirectIntroSort(const Items:pointer;const Left,Right:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
       function ParallelDirectMergeSort(const Items:pointer;const Left,Right,ElementSize:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
       function ParallelIndirectMergeSort(const Items:pointer;const Left,Right:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
       property CountJobWorkerThreads:longint read fCountJobWorkerThreads;
     end;
{$if defined(fpc) and (fpc_version>=3)}{$pop}{$ifend}

var GlobalPasMP:TPasMP=nil; // "Optional" singleton-like global PasMP instance

    GlobalPasMPMaximalThreads:longint=-1;
    GlobalPasMPThreadHeadRoomForForeignTasks:longint=0;
    GlobalPasMPDoCPUCorePinning:boolean=true;
    GlobalPasMPSleepingOnIdle:boolean=true;

    GPasMP:TPasMP absolute GlobalPasMP; // A shorter name for lazy peoples

{$ifdef fpc}
{$else}
{$if CompilerVersion>=25}
{$else}
{$ifdef fpc}
procedure FallbackMemoryBarrier; {$ifdef CAN_INLINE}inline;{$endif}
{$else}
{$if CompilerVersion>=25}
procedure FallbackReadBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackReadWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
{$else}
{$ifdef CPU386}
procedure FallbackReadBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackReadWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
procedure FallbackWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$else}
{$ifdef CPUx64}
procedure FallbackReadBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackReadWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
procedure FallbackWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$else}
procedure FallbackReadBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackReadWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
procedure FallbackWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
{$endif}
{$endif}
procedure FallbackMemoryBarrier; {$ifdef CAN_INLINE}inline;{$endif}
{$ifend}
{$endif}
{$ifend}
{$endif}

implementation

const PasMPBarrierFlag=longint(1) shl 30;

{$ifdef UseThreadLocalStorage}
{$if defined(UseThreadLocalStorageX8632) or defined(UseThreadLocalStorageX8664)}
var CurrentJobWorkerThreadTLSIndex,CurrentJobWorkerThreadTLSOffset:longint;
{$else}
threadvar CurrentJobWorkerThread:TPasMPJobWorkerThread;
{$ifend}
{$endif}

var GlobalPasMPCriticalSection:TPasMPCriticalSection=nil;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
type qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
type qword=int64;
{$ifdef CPU64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

{$ifdef Windows}

function SwitchToThread:BOOL; external 'kernel32.dll' name 'SwitchToThread';

function SetThreadIdealProcessor(hThread:THANDLE;dwIdealProcessor:longword):longword; stdcall; external 'kernel32.dll' name 'SetThreadIdealProcessor';

procedure InitializeConditionVariable(ConditionVariable:PPasMPConditionVariableData); stdcall; external 'kernel32.dll' name 'InitializeConditionVariable';
function SleepConditionVariableCS(ConditionVariable:PPasMPConditionVariableData;CriticalSection:PRTLCriticalSection;dwMilliSeconds:longword):bool; stdcall; external 'kernel32.dll' name 'SleepConditionVariableCS';
procedure WakeConditionVariable(ConditionVariable:PPasMPConditionVariableData); stdcall; external 'kernel32.dll' name 'WakeConditionVariable';
procedure WakeAllConditionVariable(ConditionVariable:PPasMPConditionVariableData); stdcall; external 'kernel32.dll' name 'WakeAllConditionVariable';

procedure InitializeSRWLock(SRWLock:PPasMPSRWLock); stdcall; external 'kernel32.dll' name 'AcquireSRWLockShared';
procedure AcquireSRWLockShared(SRWLock:PPasMPSRWLock); stdcall; external 'kernel32.dll' name 'AcquireSRWLockShared';
function TryAcquireSRWLockShared(SRWLock:PPasMPSRWLock):bool; stdcall; external 'kernel32.dll' name 'TryAcquireSRWLockShared';
procedure ReleaseSRWLockShared(SRWLock:PPasMPSRWLock); stdcall; external 'kernel32.dll' name 'ReleaseSRWLockShared';
procedure AcquireSRWLockExclusive(SRWLock:PPasMPSRWLock); stdcall; external 'kernel32.dll' name 'AcquireSRWLockExclusive';
function TryAcquireSRWLockExclusive(SRWLock:PPasMPSRWLock):bool; stdcall; external 'kernel32.dll' name 'TryAcquireSRWLockExclusive';
procedure ReleaseSRWLockExclusive(SRWLock:PPasMPSRWLock); stdcall; external 'kernel32.dll' name 'ReleaseSRWLockExclusive';

{$else}
{$ifdef Linux}
const _SC_UIO_MAXIOV=60;
      _SC_NPROCESSORS_CONF=(_SC_UIO_MAXIOV)+23;

type cpu_set_p=^cpu_set_t;
     cpu_set_t=int64;

{$ifdef fpc}
{$linklib c}
{$else}
type ppthread_mutex_t=^pthread_mutex_t;
     ppthread_mutexattr_t=^pthread_mutexattr_t;

     ppthread_cond_t=^pthread_cond_t;
     ppthread_condattr_t=^pthread_condattr_t;

     Ppthread_rwlock_t=^tpthread_rwlock_t;
     Ppthread_rwlockattr_t=^pthread_rwlockattr_t;
{$endif}

function sysconf(__name:longint):longint; cdecl; external 'c' name 'sysconf';

function sched_getaffinity(pid:ptruint;cpusetsize:longint;cpuset:pointer):longint; cdecl; external 'c' name 'sched_getaffinity';
function sched_setaffinity(pid:ptruint;cpusetsize:longint;cpuset:pointer):longint; cdecl; external 'c' name 'sched_setaffinity';

function pthread_setaffinity_np(pid:ptruint;cpusetsize:longint;cpuset:pointer):longint; cdecl; external 'c' name 'pthread_setaffinity_np';
function pthread_getaffinity_np(pid:ptruint;cpusetsize:longint;cpuset:pointer):longint; cdecl; external 'c' name 'pthread_getaffinity_np';

{$ifndef fpc}
function pthread_mutex_init(__mutex:ppthread_mutex_t;__mutex_attr:ppthread_mutexattr_t):longint; cdecl; external 'c' name 'pthread_mutex_init';
function pthread_mutex_destroy(__mutex:ppthread_mutex_t):longint; cdecl; external 'c' name 'pthread_mutex_destroy';
function pthread_mutex_trylock(__mutex:ppthread_mutex_t):longint; cdecl; external 'c' name 'pthread_mutex_trylock';
function pthread_mutex_lock(__mutex:ppthread_mutex_t):longint; cdecl; external 'c' name 'pthread_mutex_lock';
function pthread_mutex_unlock(__mutex:ppthread_mutex_t):longint; cdecl; external 'c' name 'pthread_mutex_unlock';

function pthread_cond_init(__cond:ppthread_cond_t;__cond_attr:ppthread_condattr_t):longint; cdecl; external 'c' name 'pthread_cond_init';
function pthread_cond_destroy(__cond:ppthread_cond_t):longint; cdecl; external 'c' name 'pthread_cond_destroy';
function pthread_cond_signal(__cond:ppthread_cond_t):longint; cdecl; external 'c' name 'pthread_cond_signal';
function pthread_cond_broadcast(__cond:ppthread_cond_t):longint; cdecl; external 'c' name 'pthread_cond_broadcast';
function pthread_cond_wait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):longint; cdecl; external 'c' name 'pthread_cond_wait';
function pthread_cond_timedwait(__cond:ppthread_cond_t;__mutex:ppthread_mutex_t;__abstime:PTimeSpec):longint; cdecl; external 'c' name 'pthread_cond_timedwait';

function pthread_rwlock_init(__rwlock:Ppthread_rwlock_t;__attr:Ppthread_rwlockattr_t):longint; cdecl; external 'c' name 'pthread_rwlock_init';
function pthread_rwlock_destroy(__rwlock:Ppthread_rwlock_t):longint; cdecl; external 'c' name 'pthread_rwlock_destroy';
function pthread_rwlock_rdlock(__rwlock:Ppthread_rwlock_t):longint; cdecl; external 'c' name 'pthread_rwlock_rdlock';
function pthread_rwlock_tryrdlock(__rwlock:Ppthread_rwlock_t):longint; cdecl; external 'c' name 'pthread_rwlock_tryrdlock';
function pthread_rwlock_timedrdlock(__rwlock:Ppthread_rwlock_t;__abstime:Ptimespec):longint; cdecl; external 'c' name 'pthread_rwlock_timedrdlock';
function pthread_rwlock_wrlock(__rwlock:Ppthread_rwlock_t):longint; cdecl; external 'c' name 'pthread_rwlock_wrlock';
function pthread_rwlock_trywrlock(__rwlock:Ppthread_rwlock_t):longint; cdecl; external 'c' name 'pthread_rwlock_trywrlock';
function pthread_rwlock_timedwrlock(__rwlock:Ppthread_rwlock_t;__abstime:Ptimespec):longint; cdecl; external 'c' name 'pthread_rwlock_timedwrlock';
function pthread_rwlock_unlock(__rwlock:Ppthread_rwlock_t):longint; cdecl; external 'c' name 'pthread_rwlock_unlock';
{$endif}

{$endif}
{$endif}

{$ifdef CPUARM}
function InterlockedCompareExchange64(var Destination:int64;NewValue:int64;Comperand:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 // LDREXD and STREXD were introduced in ARM 11, so the LDREXD and STREXD instructions in ARM all v7 variants or above. In v6, only some variants support it.
 // Input:
 // r0 = pointer to Destination
 // r1 = NewValue.Lo
 // r2 = NewValue.Hi
 // r3 = Comperand.Lo
 // [sp] = Comperand.Hi
{$define UseSTREXDEQ}
{$ifdef UseSTREXDEQ}
 stmfd sp!,{r2,r4,r5,r6,r7}
 // the strex instruction demands that Rm be an even numbered register, so move r1 and r2 into r4 and r5
 mov r4,r1 // r4 = NewValue.Lo
 mov r5,r2 // r5 = NewValue.Hi
 ldrd	r2,[sp,#20] // r2 = Comperand.Hi
 dmb sy
.Loop:
 ldrexd	r6,[r0] // loads R6 and R7, so r6 = Destination.Lo, r7 = Destination.Hi
 cmp r6,r3 // if Destination.Lo = Comperand.Lo
//it eq
 cmpeq r7,r2 // if Destination.Hi = Comperand.Hi
 strexdeq r1,r4,[r0]  // [r0]=r4 and [r0+4]=r5
 bne .Fail
 cmp r1,#1 // 1 for failure and 0 for success
 beq .Loop
 bne .Done
.Fail:
 clrex
.Done:
 dmb sy
 mov r0,r6
 mov r1,r7
 ldmfd sp!,{r2,r4,r5,r6,r7}
{$else}
 stmfd sp!,{r2,r4,r5,r6,r7}
 // the strex instruction demands that Rm be an even numbered register, so move r1 and r2 into r4 and r5
 mov r4,r1 // r4 = NewValue.Lo
 mov r5,r2 // r5 = NewValue.Hi
 ldrd	r2,[sp,#20] // r2 = Comperand.Hi
 dmb sy
.Loop:
 ldrexd	r6,[r0] // loads R6 and R7, so r6 = Destination.Lo, r7 = Destination.Hi
 cmp r6,r3 // if Destination.Lo = Comperand.Lo
 it	eq
 cmpeq r7,r2 // if Destination.Hi = Comperand.Hi
 bne .Fail
 strexd r1,r4,[r0]  // [r0]=r4 and [r0+4]=r5
 cmp r1,#1 // 1 for failure and 0 for success
 beq .Loop
 bne .Done
.Fail:
 clrex
.Done:
 dmb sy
 mov r0,r6
 mov r1,r7
 ldmfd sp!,{r2,r4,r5,r6,r7}
{$endif}
end;
{$endif}

{$ifdef CPU386}
function InterlockedCompareExchange64(var Destination:int64;NewValue:int64;Comperand:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 push ebx
 push edi
 mov edi,eax
 mov edx,dword ptr [Comperand+4]
 mov eax,dword ptr [Comperand+0]
 mov ecx,dword ptr [NewValue+4]
 mov ebx,dword ptr [NewValue+0]
 lock cmpxchg8b [edi]
 pop edi
 pop ebx
end;
{$endif}

{$ifdef CPUx86_64}
function InterlockedCompareExchange128(var Destination:TPasMPInt128;const NewValue,Comperand:TPasMPInt128):TPasMPInt128; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 push rbx
{$ifdef Windows}
 push rcx
 mov rbx,qword ptr [r8]
 mov rcx,qword ptr [r8+8]
 mov r8,rdx
 mov rax,qword ptr [r9]
 mov rdx,qword ptr [r9+8]
 lock cmpxchg16b [r8]
 pop rcx
 mov qword ptr [rcx],rax
 mov qword ptr [rcx+8],rdx
{$else}
 mov rbx,rsi
 mov rax,rcx
 mov rcx,rdx
 mov rdx,r8
 lock cmpxchg16b [rdi]
{$endif}
 pop rbx
end;
{$endif}

{$ifndef fpc}
{$ifdef CPU386}
function InterlockedDecrement(var Destination:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 mov edx,$ffffffff
 xchg eax,edx
 lock xadd dword ptr [edx],eax
 dec eax
end;

function InterlockedIncrement(var Destination:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 mov edx,1
 xchg eax,edx
 lock xadd dword ptr [edx],eax
 inc eax
end;

function InterlockedExchange(var Destination:longint;Source:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 lock xchg dword ptr [eax],edx
 mov eax,edx
end;

function InterlockedExchangePointer(var Destination:pointer;Source:pointer):pointer; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 lock xchg dword ptr [eax],edx
 mov eax,edx
end;

function InterlockedExchangeAdd(var Destination:longint;Source:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 xchg edx,eax
 lock xadd dword ptr [edx],eax
end;

function InterlockedCompareExchange(var Destination:longint;NewValue,Comperand:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 xchg ecx,eax
 lock cmpxchg dword ptr [ecx],edx
end;
{$else}
{$ifdef CPUx86_64}
function InterlockedDecrement(var Destination:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov rax,rcx
{$else}
 mov rax,rdi
{$endif}
 mov edx,$ffffffff
 xchg rdx,rax
 lock xadd dword ptr [rdx],eax
 dec eax
end;

function InterlockedDecrement64(var Destination:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov rax,rcx
{$else}
 mov rax,rdi
{$endif}
 mov rdx,$ffffffffffffffff
 xchg rdx,rax
 lock xadd qword ptr [rdx],rax
 dec rax
end;

function InterlockedIncrement(var Destination:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov rax,rcx
{$else}
 mov rax,rdi
{$endif}
 mov edx,1
 xchg rdx,rax
 lock xadd dword ptr [rdx],eax
 inc eax
end;

function InterlockedIncrement64(var Destination:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov rax,rcx
{$else}
 mov rax,rdi
{$endif}
 mov rdx,1
 xchg rdx,rax
 lock xadd qword ptr [rdx],rax
 inc rax
end;

function InterlockedExchange(var Destination:longint;Source:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 lock xchg dword ptr [rcx],edx
 mov eax,edx
{$else}
 lock xchg dword ptr [rdi],esi
 mov eax,esi
{$endif}
end;

function InterlockedExchange64(var Destination:int64;NewValue:int64;Comperand:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 lock xchg rdx,qword ptr [rcx]
 mov rax,rdx
{$else}
 lock xchg rsi,qword ptr [rdi]
 mov rax,rsi
{$endif}
end;

function InterlockedExchangePointer(var Destination:pointer;Source:pointer):pointer; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 lock xchg rdx,qword ptr [rcx]
 mov rax,rdx
{$else}
 lock xchg rsi,qword ptr [rdi]
 mov rax,rsi
{$endif}
end;

function InterlockedExchangeAdd(var Destination:longint;Source:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 xchg rdx,rcx
 lock xadd dword ptr [rdx],ecx
 mov eax,ecx
{$else}
 xchg rsi,rdi
 lock xadd dword ptr [rsi],edi
 mov eax,edi
{$endif}
end;

function InterlockedExchangeAdd64(var Destination:int64;NewValue:int64;Comperand:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 xchg rdx,rcx
 lock xadd qword ptr [rdx],rcx
 mov rax,rcx
{$else}
 xchg rsi,rdi
 lock xadd qword ptr [rsi],rdi
 mov rax,rdi
{$endif}
end;

function InterlockedCompareExchange(var Destination:longint;NewValue,Comperand:longint):longint; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov eax,r8d
 lock cmpxchg dword ptr [rcx],edx
{$else}
 mov eax,edx
 lock cmpxchg dword ptr [rdi],esi
{$endif}
end;

function InterlockedCompareExchange64(var Destination:int64;NewValue,Comperand:int64):int64; assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov rax,r8
 lock cmpxchg qword ptr [rcx],rdx
{$else}
 mov rax,rdx
 lock cmpxchg qword ptr [rdi],rsi
{$endif}
end;
{$else}
function InterlockedDecrement(var Destination:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedDecrement(Destination);
end;

function InterlockedIncrement(var Destination:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedIncrement(Destination);
end;

function InterlockedExchange(var Destination:longint;Source:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedExchange(Destination,Source);
end;

function InterlockedExchangePointer(var Destination:pointer;Source:pointer):pointer; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedExchangePointer(Destination,Source);
end;

function InterlockedExchangeAdd(var Destination:longint;Source:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedExchangeAdd(Destination,Source);
end;

function InterlockedCompareExchange(var Destination:longint;NewValue,Comperand:longint):longint; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedCompareExchange(Destination,NewValue,Comperand);
end;

function InterlockedCompareExchange64(var Destination:int64;NewValue,Comperand:int64):int64; {$ifdef CAN_INLINE}inline;{$endif}
begin
 result:=Windows.InterlockedCompareExchange64(Destination,NewValue,Comperand);
end;
{$endif}
{$endif}
{$endif}

{$ifdef fpc}
procedure FallbackMemoryBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 ReadWriteBarrier;
end;
{$else}
{$if CompilerVersion>=25}
procedure FallbackReadBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 MemoryBarrier;
end;
procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 // reads imply barrier on earlier reads depended on
end;
procedure FallbackReadWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 MemoryBarrier;
end;
procedure FallbackWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 MemoryBarrier;
end;
{$else}
{$ifdef CPU386}
procedure FallbackReadBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
asm
 lfence
end;

procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 // reads imply barrier on earlier reads depended on
end;

procedure FallbackReadWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
asm
 mfence
end;

procedure FallbackWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
asm
 sfence
end;
{$else}
{$ifdef CPUx64}
procedure FallbackReadBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
asm
 lfence
end;

procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 // reads imply barrier on earlier reads depended on
end;

procedure FallbackReadWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
asm
 mfence
end;

procedure FallbackWriteBarrier; assembler; {$ifdef fpc}nostackframe; {$ifdef CAN_INLINE}inline;{$endif}{$endif}
asm
 sfence
end;
{$else}
procedure FallbackReadBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
end;

procedure FallbackReadDependencyBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
 // reads imply barrier on earlier reads depended on
end;

procedure FallbackReadWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
end;

procedure FallbackWriteBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
end;
{$endif}
{$endif}

procedure FallbackMemoryBarrier; {$ifdef CAN_INLINE}inline;{$endif}
begin
{$ifdef fpc}
 ReadWriteBarrier;
{$else}
 FallBackReadWriteBarrier;
{$endif}
end;
{$ifend}
{$endif}

function IntLog2(x:longword):longword; {$ifdef CPU386}assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
 test eax,eax
 jz @Done
 bsr eax,eax
 @Done:
end;{$else}{$ifdef CPUx86_64}assembler; {$ifdef fpc}nostackframe;{$else}register;{$endif}
asm
{$ifdef Windows}
 mov eax,ecx
{$else}
 mov eax,edi
{$endif}
 test eax,eax
 jz @Done
 bsr eax,eax
 @Done:
end;
{$else}
begin
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
 x:=x shr 1;
 x:=x-((x shr 1) and $55555555);
 x:=((x shr 2) and $33333333)+(x and $33333333);
 x:=((x shr 4)+x) and $0f0f0f0f;
 x:=x+(x shr 8);
 x:=x+(x shr 16);
 result:=x and $3f;
end;
{$endif}
{$endif}

procedure MemorySwap(a,b:pointer;Size:longint);
var Temp:longword;
begin
 while Size>=SizeOf(longword) do begin
  Temp:=longword(a^);
  longword(a^):=longword(b^);
  longword(b^):=Temp;
  inc(TPasMPPtrUInt(a),SizeOf(longword));
  inc(TPasMPPtrUInt(b),SizeOf(longword));
  dec(Size,SizeOf(longword));
 end;
 while Size>=SizeOf(byte) do begin
  Temp:=byte(a^);
  byte(a^):=byte(b^);
  byte(b^):=Temp;
  inc(TPasMPPtrUInt(a),SizeOf(byte));
  inc(TPasMPPtrUInt(b),SizeOf(byte));
  dec(Size,SizeOf(byte));
 end;
end;

class function TPasMP.GetThreadIDHash(ThreadID:{$ifdef fpc}TThreadID{$else}longword{$endif}):longword;
begin
 result:=(ThreadID*83492791) xor ((ThreadID shr 24)*19349669) xor ((ThreadID shr 16)*73856093) xor ((ThreadID shr 8)*50331653);
end;

class function TPasMP.RoundUpToPowerOfTwo(x:TPasMPPtrUInt):TPasMPPtrUInt;
begin
 dec(x);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
{$ifdef CPU64}
 x:=x or (x shr 32);
{$endif}
 result:=x+1;
end;

class function TPasMP.RoundUpToMask(x,m:TPasMPPtrUInt):TPasMPPtrUInt;
begin
 if (x and (m-1))<>0 then begin
  result:=(x+m) and not (m-1);
 end else begin
  result:=x;
 end;
end;

class procedure TPasMP.Yield; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 SwitchToThread;
end;
{$else}
{$ifdef Unix}
{$ifdef usecthreads}
begin
 sched_yield;
end;
{$else}
var timeout:timeval;
begin
 timeout.tv_sec:=0;
 timeout.tv_usec:=0;
 fpselect(0,nil,nil,nil,@timeout);
end;
{$endif}
{$else}
{$ifdef fpc}
begin
 ThreadSwitch;
end;
{$endif}
{$endif}
{$endif}

class function TPasMPInterlocked.Increment(var Destination:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicIncrement(Destination);
{$else}
 result:=InterlockedIncrement(Destination);
{$endif}
end;

{$ifdef CPU64}
class function TPasMPInterlocked.Increment(var Destination:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicIncrement(Destination);
{$else}
 result:=InterlockedIncrement64(Destination);
{$endif}
end;
{$endif}

class function TPasMPInterlocked.Decrement(var Destination:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicDecrement(Destination);
{$else}
 result:=InterlockedDecrement(Destination);
{$endif}
end;

{$ifdef CPU64}
class function TPasMPInterlocked.Decrement(var Destination:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicDecrement(Destination);
{$else}
 result:=InterlockedDecrement64(Destination);
{$endif}
end;
{$endif}

class function TPasMPInterlocked.Add(var Destination:longint;const Value:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicIncrement(Destination,Value);
{$else}
 result:=InterlockedExchangeAdd(Destination,Value);
{$endif}
end;

{$ifdef CPU64}
class function TPasMPInterlocked.Add(var Destination:int64;const Value:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicIncrement(Destination,Value);
{$else}
 result:=InterlockedExchangeAdd64(Destination,Value);
{$endif}
end;
{$endif}

class function TPasMPInterlocked.Sub(var Destination:longint;const Value:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicIncrement(Destination,-Value);
{$else}
 result:=InterlockedExchangeAdd(Destination,-Value);
{$endif}
end;

{$ifdef CPU64}
class function TPasMPInterlocked.Sub(var Destination:int64;const Value:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicIncrement(Destination,-Value);
{$else}
 result:=InterlockedExchangeAdd64(Destination,-Value);
{$endif}
end;
{$endif}

class function TPasMPInterlocked.Exchange(var Destination:longint;const Source:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(Destination,Source);
{$else}
 result:=InterlockedExchange(Destination,Source);
{$endif}
end;

class function TPasMPInterlocked.Exchange(var Destination:longword;const Source:longword):longword;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(Destination,Source);
{$else}
 result:=longword(InterlockedExchange(longint(Destination),longint(Source)));
{$endif}
end;

{$ifdef CPU64}
class function TPasMPInterlocked.Exchange(var Destination:int64;const Source:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(Destination,Source);
{$else}
 result:=InterlockedExchange64(Destination,Source);
{$endif}
end;

class function TPasMPInterlocked.Exchange(var Destination:TPasMPUInt64;const Source:TPasMPUInt64):TPasMPUInt64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(Destination,Source);
{$else}
 result:=TPasMPUInt64(InterlockedExchange64(int64(Destination),int64(Source)));
{$endif}
end;
{$endif}

class function TPasMPInterlocked.Exchange(var Destination:pointer;const Source:pointer):pointer;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(Destination,Source);
{$else}
{$ifdef CPU64}
 result:=pointer(TPasMPPtrInt(InterlockedExchange64(int64(TPasMPPtrInt(Destination)),int64(TPasMPPtrInt(Source)))));
{$else}
 result:=pointer(TPasMPPtrInt(InterlockedExchange(longint(TPasMPPtrInt(Destination)),longint(TPasMPPtrInt(Source)))));
{$endif}
{$endif}
end;

class function TPasMPInterlocked.Exchange(var Destination:TObject;const Source:TObject):TObject;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(pointer(Destination),pointer(Source));
{$else}
{$ifdef CPU64}
 result:=pointer(TPasMPPtrInt(InterlockedExchange64(int64(TPasMPPtrInt(Destination)),int64(TPasMPPtrInt(Source)))));
{$else}
 result:=pointer(TPasMPPtrInt(InterlockedExchange(longint(TPasMPPtrInt(Destination)),longint(TPasMPPtrInt(Source)))));
{$endif}
{$endif}
end;

class function TPasMPInterlocked.CompareExchange(var Destination:longint;const NewValue,Comperand:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Destination,NewValue,Comperand);
{$else}
 result:=InterlockedCompareExchange(Destination,NewValue,Comperand);
{$endif}
end;

class function TPasMPInterlocked.CompareExchange(var Destination:longword;const NewValue,Comperand:longword):longword;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Destination,NewValue,Comperand);
{$else}
 result:=longword(InterlockedCompareExchange(longint(Destination),longint(NewValue),longint(Comperand)));
{$endif}
end;

{$if defined(CPU64) or defined(CPU386) or defined(CPUARM)}
class function TPasMPInterlocked.CompareExchange(var Destination:int64;const NewValue,Comperand:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Destination,NewValue,Comperand);
{$else}
 result:=InterlockedCompareExchange64(Destination,NewValue,Comperand);
{$endif}
end;

class function TPasMPInterlocked.CompareExchange(var Destination:TPasMPInt64;const NewValue,Comperand:TPasMPInt64):TPasMPInt64;
begin
{$ifdef HAS_ATOMICS}
 result.Value:=AtomicCmpExchange(Destination.Value,NewValue.Value,Comperand.Value);
{$else}
 result.Value:=InterlockedCompareExchange64(Destination.Value,NewValue.Value,Comperand.Value);
{$endif}
end;

class function TPasMPInterlocked.CompareExchange(var Destination:TPasMPUInt64;const NewValue,Comperand:TPasMPUInt64):TPasMPUInt64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Destination,NewValue,Comperand);
{$else}
 result:=TPasMPUInt64(InterlockedCompareExchange64(int64(Destination),int64(NewValue),int64(Comperand)));
{$endif}
end;
{$ifend}

{$if defined(CPU64) and defined(HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE)}
class function TPasMPInterlocked.CompareExchange(var Destination:TPasMPInt128;const NewValue,Comperand:TPasMPInt128):TPasMPInt128;
begin
 result:=InterlockedCompareExchange128(Destination,NewValue,Comperand);
end;
{$ifend}

class function TPasMPInterlocked.CompareExchange(var Destination:pointer;const NewValue,Comperand:pointer):pointer;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Destination,NewValue,Comperand);
{$else}
{$ifdef CPU64}
 result:=pointer(TPasMPPtrInt(InterlockedCompareExchange64(int64(TPasMPPtrInt(Destination)),int64(TPasMPPtrInt(NewValue)),int64(TPasMPPtrInt(Comperand)))));
{$else}
 result:=pointer(TPasMPPtrInt(InterlockedCompareExchange(longint(TPasMPPtrInt(Destination)),longint(TPasMPPtrInt(NewValue)),longint(TPasMPPtrInt(Comperand)))));
{$endif}
{$endif}
end;

class function TPasMPInterlocked.CompareExchange(var Destination:TObject;const NewValue,Comperand:TObject):TObject;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(pointer(Destination),pointer(NewValue),pointer(Comperand));
{$else}
{$ifdef CPU64}
 result:=pointer(TPasMPPtrInt(InterlockedCompareExchange64(int64(TPasMPPtrInt(Destination)),int64(TPasMPPtrInt(NewValue)),int64(TPasMPPtrInt(Comperand)))));
{$else}
 result:=pointer(TPasMPPtrInt(InterlockedCompareExchange(longint(TPasMPPtrInt(Destination)),longint(TPasMPPtrInt(NewValue)),longint(TPasMPPtrInt(Comperand)))));
{$endif}
{$endif}
end;

class function TPasMPInterlocked.Read(var Source:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Source,0,0);
{$else}
 result:=InterlockedCompareExchange(Source,0,0);
{$endif}
end;

{$if defined(CPU64) or defined(CPU386) or defined(CPUARM)}
class function TPasMPInterlocked.Read(var Source:int64):int64;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicCmpExchange(Source,0,0);
{$else}
 result:=InterlockedCompareExchange64(Source,0,0);
{$endif}
end;

class function TPasMPInterlocked.Read(var Source:TPasMPInt64):TPasMPInt64;
begin
{$ifdef HAS_ATOMICS}
 result.Value:=AtomicCmpExchange(Source.Value,0,0);
{$else}
 result.Value:=InterlockedCompareExchange64(Source.Value,0,0);
{$endif}
end;

{$if defined(CPU64) and defined(HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE)}
class function TPasMPInterlocked.Read(var Source:TPasMPInt128):TPasMPInt128;
var Temp:TPasMPInt128;
begin
 Temp.Lo:=0;
 Temp.Hi:=0;
 result:=InterlockedCompareExchange128(Source,Temp,Temp);
end;
{$ifend}
{$ifend}

class function TPasMPInterlocked.Write(var Destination:longint;const Source:longint):longint;
begin
{$ifdef HAS_ATOMICS}
 result:=AtomicExchange(Destination,Source);
{$else}
 result:=InterlockedExchange(Destination,Source);
{$endif}
end;

{$if defined(CPU64) or defined(CPU386) or defined(CPUARM)}
class function TPasMPInterlocked.Write(var Destination:int64;const Source:int64):int64;
{$ifdef CPU64}
{$ifdef HAS_ATOMICS}
begin
 result:=AtomicExchange(Destination,Source);
end;
{$else}
begin
 result:=InterlockedExchange64(Destination,Source);
end;
{$endif}
{$else}
{$ifdef HAS_ATOMICS}
var Old:int64;
begin
 repeat
  Old:=Destination;
  result:=AtomicCmpExchange(Destination,Source,Old);
 until result=Old;
end;
{$else}
var Old:int64;
begin
 repeat
  Old:=Destination;
  result:=InterlockedCompareExchange64(Destination,Source,Old);
 until result=Old;
end;
{$endif}
{$endif}

class function TPasMPInterlocked.Write(var Destination:TPasMPInt64;const Source:TPasMPInt64):TPasMPInt64;
{$ifdef CPU64}
{$ifdef HAS_ATOMICS}
begin
 result.Value:=AtomicExchange(Destination.Value,Source.Value);
end;
{$else}
begin
 result.Value:=InterlockedExchange64(Destination.Value,Source.Value);
end;
{$endif}
{$else}
{$ifdef HAS_ATOMICS}
var Old:int64;
begin
 repeat
  Old:=Destination.Value;
  result.Value:=AtomicCmpExchange(Destination.Value,Source.Value,Old);
 until result.Value=Old;
end;
{$else}
var Old:int64;
begin
 repeat
  Old:=Destination.Value;
  result.Value:=InterlockedCompareExchange64(Destination.Value,Source.Value,Old);
 until result.Value=Old;
end;
{$endif}
{$endif}

{$if defined(CPU64) and defined(HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE)}
class function TPasMPInterlocked.Write(var Destination:TPasMPInt128;const Source:TPasMPInt128):TPasMPInt128;
var Old:TPasMPInt128;
begin
 repeat
  Old:=Destination;
  result:=InterlockedCompareExchange128(Destination,Source,Old);
 until (result.Lo=Old.Lo) and (result.Hi=Old.Hi);
end;
{$ifend}
{$ifend}

class procedure TPasMPMemoryBarrier.Read;
begin
{$ifdef fpc}
 ReadBarrier;
{$else}
{$if CompilerVersion>=25}
 MemoryBarrier;
{$else}
 FallbackReadBarrier;
{$ifend}
{$endif}
end;

class procedure TPasMPMemoryBarrier.ReadDependency;
begin
 // reads imply barrier on earlier reads depended on
end;

class procedure TPasMPMemoryBarrier.ReadWrite;
begin
{$ifdef fpc}
 ReadWriteBarrier;
{$else}
{$if CompilerVersion>=25}
 MemoryBarrier;
{$else}
 FallbackReadWriteBarrier;
{$ifend}
{$endif}
end;

class procedure TPasMPMemoryBarrier.Write;
begin
{$ifdef fpc}
 WriteBarrier;
{$else}
{$if CompilerVersion>=25}
 MemoryBarrier;
{$else}
 FallbackWriteBarrier;
{$ifend}
{$endif}
end;

class procedure TPasMPMemoryBarrier.Sync;
begin
{$ifdef fpc}
 ReadWriteBarrier;
{$else}
{$if CompilerVersion>=25}
 MemoryBarrier;
{$else}
 FallbackReadWriteBarrier;
{$ifend}
{$endif}
end;

class procedure TPasMPMemory.AllocateAlignedMemory(var p;Size:longint;Align:longint=PasMPCPUCacheLineSize);
var Original,Aligned:pointer;
    Mask:ptruint;
begin
 if (Align and (Align-1))<>0 then begin
  Align:=TPasMP.RoundUpToPowerOfTwo(Align);
 end;
 Mask:=Align-1;
 inc(Size,((Align shl 1)+SizeOf(pointer)));
 GetMem(Original,Size);
 FillChar(Original^,Size,#0);
 Aligned:=pointer(ptruint(ptruint(Original)+SizeOf(pointer)));
 if (Align>1) and ((ptruint(Aligned) and Mask)<>0) then begin
  inc(ptruint(Aligned),ptruint(ptruint(Align)-(ptruint(Aligned) and Mask)));
 end;
 pointer(pointer(ptruint(ptruint(Aligned)-SizeOf(pointer)))^):=Original;
 pointer(pointer(@p)^):=Aligned;
end;

class procedure TPasMPMemory.FreeAlignedMemory(const p);
var pp:pointer;
begin
 pp:=pointer(pointer(@p)^);
 if assigned(pp) then begin
  pp:=pointer(pointer(ptruint(ptruint(pp)-SizeOf(pointer)))^);
  FreeMem(pp);
 end;
end;

class procedure TPasMPMemory.Barrier;
begin
{$ifdef fpc}
 ReadWriteBarrier;
{$else}
{$if CompilerVersion>=25}
 MemoryBarrier;
{$else}
 FallbackReadWriteBarrier;
{$ifend}
{$endif}
end;

constructor TPasMPSimpleEvent.Create;
begin
 inherited Create(nil,false,false,'');
end;

constructor TPasMPMutex.Create;
begin
 inherited Create;
{$ifdef Windows}
 fMutex:=CreateMutex(nil,false,nil);
 if fMutex=0 then begin
  RaiseLastOSError;
 end;
{$else}
{$ifdef Unix}
 pthread_mutex_init(@fMutex,nil);
{$else}
 fCriticalSection:=TCriticalSection.Create;
{$endif}
{$endif}
end;

{$ifdef Unix}
constructor TPasMPMutex.Create(const lpMutexAttributes:pointer);
begin
 inherited Create;
{$ifdef Windows}
 fMutex:=CreateMutex(lpMutexAttributes,false,'');
 if fMutex=0 then begin
  RaiseLastOSError;
 end;
{$else}
{$ifdef Unix}
 pthread_mutex_init(@fMutex,lpMutexAttributes);
{$else}
 fCriticalSection:=TCriticalSection.Create;
{$endif}
{$endif}
end;
{$endif}

{$ifdef Windows}
constructor TPasMPMutex.Create(const lpMutexAttributes:pointer;const bInitialOwner:boolean;const lpName:string);
begin
 inherited Create;
{$ifdef Windows}
 fMutex:=CreateMutex(lpMutexAttributes,bInitialOwner,PChar(lpName));
 if fMutex=0 then begin
  RaiseLastOSError;
 end;
{$else}
{$ifdef Unix}
 pthread_mutex_init(@fMutex,lpMutexAttributes);
{$else}
 fCriticalSection:=TCriticalSection.Create;
{$endif}
{$endif}
end;

constructor TPasMPMutex.Create(const DesiredAccess:longword;const bInitialOwner:boolean;const lpName:string);
begin
 inherited Create;
{$ifdef Windows}
 fMutex:=OpenMutex(DesiredAccess,bInitialOwner,PChar(lpName));
 if fMutex=0 then begin
  RaiseLastOSError;
 end;
{$else}
{$ifdef Unix}
 pthread_mutex_init(@fMutex,nil);
{$else}
 fCriticalSection:=TCriticalSection.Create;
{$endif}
{$endif}
end;
{$endif}

destructor TPasMPMutex.Destroy;
begin
{$ifdef Windows}
 CloseHandle(fMutex);
{$else}
{$ifdef Unix}
 pthread_mutex_destroy(@fMutex);
{$else}
 fCriticalSection.Free;
{$endif}
{$endif}
 inherited Destroy;
end;

procedure TPasMPMutex.Acquire;
begin
{$ifdef Windows}
 case WaitForSingleObject(fMutex,INFINITE) of
  WAIT_OBJECT_0:begin
  end;
  WAIT_TIMEOUT:begin
  end;
  WAIT_ABANDONED:begin
  end;
  else begin
   RaiseLastOSError;
  end;
 end;
{$else}
{$ifdef Unix}
 pthread_mutex_lock(@fMutex);
{$else}
 fCriticalSection.Acquire;
{$endif}
{$endif}
end;

procedure TPasMPMutex.Release;
begin
{$ifdef Windows}
 if not ReleaseMutex(fMutex) then begin
  RaiseLastOSError;
 end;
{$else}
{$ifdef Unix}
 pthread_mutex_unlock(@fMutex);
{$else}
 fCriticalSection.Release;
{$endif}
{$endif}
end;

constructor TPasMPConditionVariableLock.Create;
begin
 inherited Create;
{$ifdef Windows}
 InitializeCriticalSection(fCriticalSection);
{$else}
{$ifdef Unix}
 pthread_mutex_init(@fMutex,nil);
{$else}
 fCriticalSection:=TPasMPCriticalSection.Create;
{$endif}
{$endif}
end;

destructor TPasMPConditionVariableLock.Destroy;
begin
{$ifdef Windows}
 DeleteCriticalSection(fCriticalSection);
{$else}
{$ifdef Unix}
 pthread_mutex_destroy(@fMutex);
{$else}
 fCriticalSection.Free;
{$endif}
{$endif}
 inherited Destroy;
end;

procedure TPasMPConditionVariableLock.Acquire;
begin
{$ifdef Windows}
 EnterCriticalSection(fCriticalSection);
{$else}
{$ifdef Unix}
 pthread_mutex_lock(@fMutex);
{$else}
 fCriticalSection.Acquire;
{$endif}
{$endif}
end;

procedure TPasMPConditionVariableLock.Release; 
begin
{$ifdef Windows}
 LeaveCriticalSection(fCriticalSection);
{$else}
{$ifdef Unix}
 pthread_mutex_unlock(@fMutex);
{$else}
 fCriticalSection.Release;
{$endif}
{$endif}
end;

constructor TPasMPConditionVariable.Create;
begin
 inherited Create;
{$ifdef Windows}
 InitializeConditionVariable(@fConditionVariable);
{$else}
{$ifdef Unix}
 pthread_cond_init(@fConditionVariable,nil);
{$else}
 fWaitCounter:=0;
 fCriticalSection:=TPasMPCriticalSection.Create;
 fReleaseCounter:=0;
 fGenerationCounter:=0;
 fEvent:=TPasMPEvent.Create(nil,true,false,'');
{$endif}
{$endif}
end;

destructor TPasMPConditionVariable.Destroy;
begin
{$ifdef Windows}
{$else}
{$ifdef Unix}
 pthread_cond_destroy(@fConditionVariable);
{$else}
 fCriticalSection.Free;
 fEvent.Free;
{$endif}
{$endif}
 inherited Destroy;
end;

function TPasMPConditionVariable.Wait(const Lock:TPasMPConditionVariableLock;const dwMilliSeconds:longword=INFINITE):TWaitResult; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 if SleepConditionVariableCS(@fConditionVariable,@Lock.fCriticalSection,dwMilliSeconds) then begin
  result:=wrSignaled;
 end else begin
  case GetLastError of
   ERROR_TIMEOUT:begin
    result:=wrTimeOut;
   end;      
   else begin
    result:=wrError;
   end;
  end;
 end;
end;
{$else}
{$ifdef Unix}
var TimeSpec_:TTimeSpec;
begin
 if dwMilliSeconds=INFINITE then begin
  case pthread_cond_wait(@fConditionVariable,@Lock.fMutex) of
   0:begin
    result:=wrSignaled;
   end;
   ESysETIMEDOUT:begin
    result:=wrTimeOut;
   end;
   ESysEINVAL:begin
    result:=wrAbandoned;
   end;
   else begin
    result:=wrError;
   end;
  end;
 end else begin
  TimeSpec_.tv_sec:=dwMilliSeconds div 1000;
  TimeSpec_.tv_nsec:=(dwMilliSeconds mod 1000)*1000000000;
  case pthread_cond_timedwait(@fConditionVariable,@Lock.fMutex,@TimeSpec_) of
   0:begin
    result:=wrSignaled;
   end;
   ESysETIMEDOUT:begin
    result:=wrTimeOut;
   end;
   ESysEINVAL:begin
    result:=wrAbandoned;
   end;
   else begin
    result:=wrError;
   end;
  end;
 end;
end;
{$else}
var SavedGenerationCounter:longint;
    WaitDone,WasLastWaiter:boolean;
begin

 result:=wrError;

 fCriticalSection.Acquire;
 try
  inc(fWaitCounter);
  SavedGenerationCounter:=fGenerationCounter;
 finally
  fCriticalSection.Release;
 end;

 Lock.Release;
 try
  repeat
   case fEvent.WaitFor(dwMilliSeconds) of
    wrSignaled:begin
     try
      WaitDone:=(fReleaseCounter>0) and (SavedGenerationCounter<>fGenerationCounter);
     finally
      fCriticalSection.Release;
     end;
     if WaitDone then begin
      result:=wrSignaled;
     end;
    end;
    wrTimeOut:begin
     WaitDone:=true;
     result:=wrTimeOut;
    end;
    wrAbandoned:begin
     WaitDone:=true;
     result:=wrAbandoned;
    end;
    else begin
     WaitDone:=true;
     result:=wrError;
    end;
   end;
  until WaitDone;
 finally
  Lock.Acquire;
 end;

 fCriticalSection.Acquire;
 try
  dec(fWaitCounter);
  dec(fReleaseCounter);
  WasLastWaiter:=fReleaseCounter=0;
 finally
  fCriticalSection.Release;
 end;

 if WasLastWaiter then begin
  fEvent.ResetEvent;
 end;

end;
{$endif}
{$endif}

procedure TPasMPConditionVariable.Signal; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 WakeConditionVariable(@fConditionVariable);
end;
{$else}
{$ifdef Unix}
begin
 pthread_cond_signal(@fConditionVariable);
end;
{$else}
begin
 fCriticalSection.Acquire;
 try
  if fWaitCounter>fReleaseCounter then begin
   inc(fReleaseCounter);
   inc(fGenerationCounter);
   fEvent.SetEvent;
  end;
 finally
  fCriticalSection.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPConditionVariable.Broadcast; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 WakeAllConditionVariable(@fConditionVariable);
end;
{$else}
{$ifdef Unix}
begin
 pthread_cond_broadcast(@fConditionVariable);
end;
{$else}
begin
 fCriticalSection.Acquire;
 try
  if fWaitCounter>0 then begin
   fReleaseCounter:=fWaitCounter;
   inc(fGenerationCounter);
   fEvent.SetEvent;
  end;
 finally
  fCriticalSection.Release;
 end;
end;
{$endif}
{$endif}
         
constructor TPasMPSemaphore.Create(const InitialCount,MaximumCount:longint);
begin
 inherited Create;
 fInitialCount:=InitialCount;
 fMaximumCount:=MaximumCount;
{$ifdef Windows}
 fHandle:=CreateSemaphore(nil,InitialCount,MaximumCount,nil);
{$else}
{$ifdef unix}
 sem_init(@fHandle,0,InitialCount);
{$else}
 fCurrentCount:=fInitialCount;
{$ifdef PasMPSemaphoreUseConditionVariable}
 fConditionVariableLock:=TPasMPConditionVariableLock.Create;
 fConditionVariable:=TPasMPConditionVariable.Create;
{$else}
 fCriticalSection:=TPasMPCriticalSection.Create;
 fEvent:=TPasMPEvent.Create(nil,false,false,'');
{$endif}
{$endif}
{$endif}
end;

destructor TPasMPSemaphore.Destroy;
begin
{$ifdef Windows}
 CloseHandle(fHandle);
{$else}
{$ifdef unix}
 sem_destroy(@fHandle);
{$else}
{$ifdef PasMPSemaphoreUseConditionVariable}
 fConditionVariable.Free;
 fConditionVariableLock.Free;
{$else}
 fEvent.Free;
 fCriticalSection.Free;
{$endif}
{$endif}
{$endif}
 inherited Destroy;
end;

procedure TPasMPSemaphore.Acquire;
begin
 Acquire(1);
end;

procedure TPasMPSemaphore.Release;
begin
 Release(1);
end;

function TPasMPSemaphore.Acquire(const AcquireCount:longint):TWaitResult;
{$ifdef Windows}
var Counter:longint;
begin
 result:=wrError;
 for Counter:=1 to AcquireCount do begin
  case WaitForSingleObject(fHandle,INFINITE) of
   WAIT_OBJECT_0:begin
    result:=wrSignaled;
   end;
   WAIT_TIMEOUT:begin
    result:=wrTimeOut;
    exit;
   end;
   WAIT_ABANDONED:begin
    result:=wrAbandoned;
    exit;
   end;
   else begin
    result:=wrError;
    exit;
   end;
  end;
 end;
end;
{$else}
{$ifdef unix}
var Counter:longint;
begin
 result:=wrError;
 for Counter:=1 to AcquireCount do begin
  case sem_wait(@fHandle) of
   0:begin
    result:=wrSignaled;
   end;
   ESysETIMEDOUT:begin
    result:=wrTimeOut;
    exit;
   end;
   ESysEINVAL:begin
    result:=wrAbandoned;
    exit;
   end;
   else begin
    result:=wrError;
    exit;
   end;
  end;
 end;
end;
{$else}
{$ifdef PasMPSemaphoreUseConditionVariable}
var Counter:longint;
begin
 result:=wrError;
 fConditionVariableLock.Acquire;
 try
  for Counter:=1 to AcquireCount do begin
   result:=wrSignaled;
   while fCurrentCount=0 do begin
    result:=fConditionVariable.Wait(fConditionVariableLock,INFINITE);
    if result<>wrSignaled then begin
     break;
    end;
   end;
   if result<>wrSignaled then begin
    break;
   end;
   if fCurrentCount<>0 then begin
    dec(fCurrentCount);
   end;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$else}
var Counter:longint;
    Done:boolean;
begin
 result:=wrError;
 for Counter:=1 to AcquireCount do begin
  result:=wrSignaled;
  repeat
   fCriticalSection.Acquire;
   try
    Done:=fCurrentCount<>0;
    if Done then begin
     dec(fCurrentCount);
    end;
   finally
    fCriticalSection.Release;
   end;
   if Done then begin
    break;
   end;
   result:=fEvent.WaitFor(INFINITE);
  until result<>wrSignaled;
  if result<>wrSignaled then begin
   exit;
  end;
 end;
end;
{$endif}
{$endif}
{$endif}

function TPasMPSemaphore.Release(const ReleaseCount:longint):longint;
{$ifdef Windows}
begin
 ReleaseSemaphore(fHandle,ReleaseCount,@result);
end;
{$else}
{$ifdef unix}
begin
 result:=0;
 while result<ReleaseCount do begin
  case sem_post(@fHandle) of
   0:begin
    inc(result);
   end;
   else begin
    break;
   end;
  end;
 end;
end;
{$else}
{$ifdef PasMPSemaphoreUseConditionVariable}
begin
 fConditionVariableLock.Acquire;
 try
  if ((fCurrentCount+ReleaseCount)<fCurrentCount) or
     ((fCurrentCount+ReleaseCount)>fMaximumCount) then begin
   // Invalid release count
   result:=0;
  end else begin
   if fCurrentCount<>0 then begin
    // There can't be any thread to wake up if the value of fCurrentCount isn't zero
    inc(fCurrentCount,ReleaseCount);
   end else begin
    fCurrentCount:=ReleaseCount;
    fConditionVariable.Broadcast;
   end;
   result:=fCurrentCount;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$else}
var WakeUp:boolean;
begin
 WakeUp:=false;
 fCriticalSection.Acquire;
 try
  if ((fCurrentCount+ReleaseCount)<fCurrentCount) or
     ((fCurrentCount+ReleaseCount)>fMaximumCount) then begin
   // Invalid release count
   result:=0;
  end else begin
   if fCurrentCount<>0 then begin
    // There can't be any thread to wake up if the value of fCurrentCount isn't zero
    inc(fCurrentCount,ReleaseCount);
   end else begin
    fCurrentCount:=ReleaseCount;
    WakeUp:=true;
   end;
   result:=fCurrentCount;
  end;
 finally
  fCriticalSection.Release;
 end;
 if WakeUp then begin
  fEvent.SetEvent;
 end;
end;
{$endif}
{$endif}
{$endif}

constructor TPasMPInvertedSemaphore.Create(const InitialCount,MaximumCount:longint);
begin
 inherited Create;
 fInitialCount:=InitialCount;
 fMaximumCount:=MaximumCount;
 fCurrentCount:=InitialCount;
 fConditionVariableLock:=TPasMPConditionVariableLock.Create;
 fConditionVariable:=TPasMPConditionVariable.Create;
end;

destructor TPasMPInvertedSemaphore.Destroy;
begin
 fConditionVariable.Free;
 fConditionVariableLock.Free;
 inherited Destroy;
end;

procedure TPasMPInvertedSemaphore.Acquire;
var Temp:longint;
begin
 Acquire(1,Temp);
end;

procedure TPasMPInvertedSemaphore.Release;
var Temp:longint;
begin
 Release(1,Temp);
end;

function TPasMPInvertedSemaphore.Acquire(const AcquireCount:longint;out Count:longint):longint;
begin
 fConditionVariableLock.Acquire;
 try                          
  if AcquireCount<=0 then begin
   result:=0;
  end else if (fCurrentCount+AcquireCount)<fMaximumCount then begin
   result:=AcquireCount;
  end else begin
   result:=fMaximumCount-fCurrentCount;
  end;
  inc(fCurrentCount,result);
  Count:=fCurrentCount;
 finally
  fConditionVariableLock.Release;
 end;
end;

function TPasMPInvertedSemaphore.Release(const ReleaseCount:longint;out Count:longint):longint;
begin
 fConditionVariableLock.Acquire;
 try
  if ReleaseCount<=0 then begin
   result:=0;
  end else if fCurrentCount<ReleaseCount then begin
   result:=fCurrentCount;
  end else begin
   result:=ReleaseCount;
  end;
  dec(fCurrentCount,result);
  if fCurrentCount=0 then begin
   fConditionVariable.Broadcast;
  end;
  Count:=fCurrentCount;
 finally
  fConditionVariableLock.Release;
 end;
end;

function TPasMPInvertedSemaphore.Wait(const dwMilliSeconds:longword=INFINITE):TWaitResult;
begin
 result:=wrSignaled;
 fConditionVariableLock.Acquire;
 try
  while fCurrentCount<>0 do begin
   result:=fConditionVariable.Wait(fConditionVariableLock,dwMilliSeconds);
   if dwMilliSeconds=INFINITE then begin
    // special case due to spurious wakeups of condition variables
    if not (result in [wrSignaled,wrTimeOut]) then begin
     break;
    end;
   end else begin
    if result<>wrSignaled then begin
     break;
    end;
   end;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;

constructor TPasMPMultipleReaderSingleWriterLock.Create;
begin
 inherited Create;
{$ifdef Windows}
 InitializeSRWLock(@fSRWLock);
{$else}
{$ifdef unix}
 pthread_rwlock_init(@fReadWriteLock,nil);
{$else}
 fReaders:=0;
 fWriters:=0;
 fConditionVariableLock:=TPasMPConditionVariableLock.Create;
 fConditionVariable:=TPasMPConditionVariable.Create;
{$endif}
{$endif}
end;

destructor TPasMPMultipleReaderSingleWriterLock.Destroy;
begin
{$ifdef Windows}
{$else}
{$ifdef unix}
 pthread_rwlock_destroy(@fReadWriteLock);
{$else}
 fConditionVariable.Free;
 fConditionVariableLock.Free;
{$endif}
{$endif}
 inherited Destroy;
end;

procedure TPasMPMultipleReaderSingleWriterLock.AcquireRead; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 AcquireSRWLockShared(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_rdlock(@fReadWriteLock);
end;
{$else}
var State:longint;
begin
 fConditionVariableLock.Acquire;
 try
  while fWriters<>0 do begin
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
  inc(fReaders);
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

function TPasMPMultipleReaderSingleWriterLock.TryAcquireRead:boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 result:=TryAcquireSRWLockShared(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 result:=pthread_rwlock_tryrdlock(@fReadWriteLock)=0;
end;
{$else}
var State:longint;
begin
 fConditionVariableLock.Acquire;
 try
  result:=fWriters=0;
  if result then begin
   inc(fReaders);
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPMultipleReaderSingleWriterLock.ReleaseRead; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 ReleaseSRWLockShared(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_unlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  dec(fReaders);
  if fReaders=0 then begin
   fConditionVariable.Broadcast;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPMultipleReaderSingleWriterLock.AcquireWrite; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 AcquireSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_wrlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  while (fReaders<>0) or (fWriters<>0) do begin
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
  inc(fWriters);
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

function TPasMPMultipleReaderSingleWriterLock.TryAcquireWrite:boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 result:=TryAcquireSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 result:=pthread_rwlock_trywrlock(@fReadWriteLock)=0;
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  result:=(fReaders=0) and (fWriters=0);
  if result then begin
   inc(fWriters);
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPMultipleReaderSingleWriterLock.ReleaseWrite; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 ReleaseSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_unlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  dec(fWriters);
  if fWriters=0 then begin
   fConditionVariable.Broadcast;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPMultipleReaderSingleWriterLock.ReadToWrite; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 ReleaseSRWLockShared(@fSRWLock);
 AcquireSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_unlock(@fReadWriteLock);
 pthread_rwlock_wrlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  dec(fReaders);
  while (fWriters<>0) and (fReaders<>0) do begin
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
  inc(fWriters);
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPMultipleReaderSingleWriterLock.WriteToRead; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef Windows}
begin
 ReleaseSRWLockExclusive(@fSRWLock);
 AcquireSRWLockShared(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_unlock(@fReadWriteLock);
 pthread_rwlock_rdlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  dec(fWriters);
  while fWriters<>0 do begin
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
  inc(fReaders);
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

constructor TPasMPSlimReaderWriterLock.Create;
begin
 inherited Create;
{$ifdef Windows}
 InitializeSRWLock(@fSRWLock);
{$else}
{$ifdef unix}
 pthread_rwlock_init(@fReadWriteLock,nil);
{$else}
 fCount:=0;
 fConditionVariableLock:=TPasMPConditionVariableLock.Create;
 fConditionVariable:=TPasMPConditionVariable.Create;
{$endif}
{$endif}
end;

destructor TPasMPSlimReaderWriterLock.Destroy;
begin
{$ifdef Windows}
{$else}
{$ifdef unix}
 pthread_rwlock_destroy(@fReadWriteLock);
{$else}
 fConditionVariable.Free;
 fConditionVariableLock.Free;
{$endif}
{$endif}
 inherited Destroy;
end;

procedure TPasMPSlimReaderWriterLock.Acquire;
{$ifdef Windows}
begin
 AcquireSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_wrlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  while fCount<>0 do begin
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
  inc(fCount);
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

function TPasMPSlimReaderWriterLock.TryAcquire:boolean;
{$ifdef Windows}
begin
 result:=TryAcquireSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 result:=pthread_rwlock_trywrlock(@fReadWriteLock)=0;
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  result:=fCount=0;
  if result then begin
   inc(fCount);
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

procedure TPasMPSlimReaderWriterLock.Release;
{$ifdef Windows}
begin
 ReleaseSRWLockExclusive(@fSRWLock);
end;
{$else}
{$ifdef unix}
begin
 pthread_rwlock_unlock(@fReadWriteLock);
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  dec(fCount);
  if fCount=0 then begin
   fConditionVariable.Broadcast;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}
{$endif}

constructor TPasMPSpinLock.Create;
begin
 inherited Create;
{$ifdef Unix}
 pthread_spin_init(@fSpinLock,0);
{$else}
 fState:=0;
{$endif}
end;

destructor TPasMPSpinLock.Destroy;
begin
{$ifdef Unix}
 pthread_spin_destroy(@fSpinLock);
{$endif}
 inherited Destroy;
end;

procedure TPasMPSpinLock.Acquire; {$if defined(Unix)}
begin
 pthread_spin_lock(@fSpinLock);
end;
{$else}{$ifdef cpu386}assembler; register;
asm
 test dword ptr [eax+TPasMPSpinLock.fState],1
 jnz @SpinLoop
@TryAgain:
 lock bts dword ptr [eax+TPasMPSpinLock.fState],0
 jnc @TryDone
@SpinLoop:
 db $f3,$90 // pause (rep nop)
 test dword ptr [eax+TPasMPSpinLock.fState],1
 jnz @SpinLoop
 jmp @TryAgain
@TryDone:
end;
{$else}{$ifdef cpux86_64}assembler; register;
{$ifdef Windows}
asm
 // Win64 ABI
 // rcx = self
 test dword ptr [rcx+TPasMPSpinLock.fState],1
 jnz @SpinLoop
@TryAgain:
 lock bts dword ptr [rcx+TPasMPSpinLock.fState],0
 jnc @TryDone
@SpinLoop:
 pause
 test dword ptr [rcx+TPasMPSpinLock.fState],1
 jnz @SpinLoop
 jmp @TryAgain
@TryDone:
end;
{$else}
asm
 // System V ABI
 // rdi = self
 test dword ptr [edi+TPasMPSpinLock.fState],1
 jnz @SpinLoop
@TryAgain:
 lock bts dword ptr [rdi+PasMPSpinLock.fState],0
 jnc @TryDone
@SpinLoop:
 pause
 test dword ptr [rdi+TPasMPSpinLock.fState],1
 jnz @SpinLoop
 jmp @TryAgain
@TryDone:
end;
{$endif}
{$else}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 while TPasMPInterlocked.CompareExchange(fState,-1,0)<>0 do begin
  TPasMP.Yield;
 end;
end;
{$endif}
{$endif}
{$ifend}

function TPasMPSpinLock.TryAcquire:longbool; {$if defined(Unix)}
begin
 result:=pthread_spin_trylock(@fSpinLock)=0;
end;
{$else}{$ifdef cpu386}assembler; register;
asm
 xor eax,eax
 lock bts dword ptr [eax+TPasMPSpinLock.fState],0
 jc @Failed
  not eax
 @Failed:
end;
{$else}{$ifdef cpux86_64}assembler; register;
{$ifdef Windows}
asm
 // Win64 ABI
 // rcx = self
 xor rax,rax
 lock bts dword ptr [rcx+TPasMPSpinLock.fState],0
 jc @Failed
  not rax
 @Failed:
end;
{$else}
asm
 // System V ABI
 // rdi = self
 xor rax,rax
 lock bts dword ptr [rdi+TPasMPSpinLock.fState],0
 jc @Failed
  not rax
 @Failed:
end;
{$endif}
{$else}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 result:=TPasMPInterlocked.CompareExchange(fState,-1,0)=0;
end;
{$endif}
{$endif}
{$ifend}

procedure TPasMPSpinLock.Release; {$if defined(Unix)}
begin
 pthread_spin_unlock(@fSpinLock);
end;
{$else}{$ifdef cpu386}assembler; register;
asm
 mov dword ptr [eax+TPasMPSpinLock.fState],0
end;
{$else}{$ifdef cpux86_64}assembler; register;
{$ifdef Windows}
asm
 // Win64 ABI
 // rcx = self
 mov dword ptr [rcx+TPasMPSpinLock.fState],0
end;
{$else}
asm
 // System V ABI
 // rdi = self
 mov dword ptr [rdi+TPasMPSpinLock.fState],0
end;
{$endif}
{$else}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 TPasMPInterlocked.Exchange(fState,0);
end;
{$endif}
{$endif}
{$ifend}

constructor TPasMPBarrier.Create(const Count:longint);
begin
 inherited Create;
{$ifdef unix}
 pthread_barrier_init(@fBarrier,nil,Count);
{$else}
 fCount:=Count;
 fTotal:=0;
 fConditionVariableLock:=TPasMPConditionVariableLock.Create;
 fConditionVariable:=TPasMPConditionVariable.Create;
{$endif}
end;

destructor TPasMPBarrier.Destroy;
begin
{$ifdef unix}
 pthread_barrier_destroy(@fBarrier);
{$else}
 fConditionVariableLock.Acquire;
 try
  while fTotal>PasMPBarrierFlag do begin
   // Wait until everyone exits the barrier
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
 finally
  fConditionVariableLock.Release;
 end;
 fConditionVariable.Free;
 fConditionVariableLock.Free;
{$endif}
 inherited Destroy;
end;

function TPasMPBarrier.Wait:boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
{$ifdef unix}
begin
 result:=pthread_barrier_wait(@fBarrier)=PTHREAD_BARRIER_SERIAL_THREAD;
end;
{$else}
begin
 fConditionVariableLock.Acquire;
 try
  while fTotal>PasMPBarrierFlag do begin
   // Wait until everyone exits the barrier
   fConditionVariable.Wait(fConditionVariableLock,INFINITE);
  end;
  if fTotal=PasMPBarrierFlag then begin
   // Are we the first to enter?
   fTotal:=0;
  end;
  inc(fTotal);
  if fTotal=fCount then begin
   inc(fTotal,PasMPBarrierFlag-1);
   fConditionVariable.Broadcast;
   result:=true;
  end else begin
   while fTotal<PasMPBarrierFlag do begin
    // Wait until enough threads enter the barrier
    fConditionVariable.Wait(fConditionVariableLock,INFINITE);
   end;
   dec(fTotal);
   if ftotal=PasMPBarrierFlag then begin
    // Get entering threads to wake up
    fConditionVariable.Broadcast;
   end;
   result:=false;
  end;
 finally
  fConditionVariableLock.Release;
 end;
end;
{$endif}

constructor TPasMPInterlockedStack.Create;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
begin
 inherited Create;
 TPasMPMemory.AllocateAlignedMemory(fHead,SizeOf(TPasMPTaggedPointer),PasMPCPUCacheLineSize);
 fHead^.PointerValue:=nil;
 fHead^.TagValue:=0;
end;
{$else}
begin
 inherited Create;
 fCriticalSection:=TPasMPCriticalSection.Create;
 fHead:=nil;
end;
{$endif}

destructor TPasMPInterlockedStack.Destroy;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
begin
 TPasMPMemory.FreeAlignedMemory(fHead);
 inherited Destroy;
end;
{$else}
begin
 fCriticalSection.Free;
 inherited Destroy;
end;
{$endif}

procedure TPasMPInterlockedStack.Clear;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
begin
 fHead^.PointerValue:=nil;
 fHead^.TagValue:=0;
end;
{$else}
begin
 fHead:=nil;
end;
{$endif}

function TPasMPInterlockedStack.IsEmpty:boolean;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
begin
 result:=not assigned(fHead^.PointerValue);
end;
{$else}
begin
 result:=true;
 if assigned(fHead) then begin
  fCriticalSection.Acquire;
  try
   if assigned(fHead) then begin
    result:=false;
   end;
  finally
   fCriticalSection.Leave;
  end;
 end;
end;
{$endif}

function TPasMPInterlockedStack.Push(const Item:pointer):pointer;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
var OldHead,NewHead,ComparsionHead:TPasMPTaggedPointer;
begin
 OldHead:=fHead^;
 repeat
  pointer(Item^):=OldHead.PointerValue;
  NewHead.PointerValue:=Item;
  NewHead.TagValue:=OldHead.TagValue+1;
  ComparsionHead:=OldHead;
  OldHead.Value:=TPasMPInterlocked.CompareExchange(fHead^.Value,NewHead.Value,ComparsionHead.Value);
 until {$ifdef cpu64}(OldHead.PointerValue=ComparsionHead.PointerValue) and (OldHead.TagValue=ComparsionHead.TagValue){$else}OldHead.Value.Value=ComparsionHead.Value.Value{$endif};
 result:=OldHead.PointerValue;
end;
{$else}
begin
 fCriticalSection.Acquire;
 try
  result:=fHead;
  pointer(Item^):=fHead;
  fHead:=Item;
 finally
  fCriticalSection.Leave;
 end;
end;
{$endif}

function TPasMPInterlockedStack.Pop:pointer;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
var OldHead,NewHead,ComparsionHead:TPasMPTaggedPointer;
begin
 if assigned(fHead^.PointerValue) then begin
  OldHead:=fHead^;
  while assigned(OldHead.PointerValue) do begin
   NewHead.PointerValue:=pointer(OldHead.PointerValue^);
   NewHead.TagValue:=NewHead.TagValue+1;
   ComparsionHead:=OldHead;
   OldHead.Value:=TPasMPInterlocked.CompareExchange(fHead^.Value,NewHead.Value,ComparsionHead.Value);
   if {$ifdef cpu64}(OldHead.PointerValue=ComparsionHead.PointerValue) and (OldHead.TagValue=ComparsionHead.TagValue){$else}OldHead.Value.Value=ComparsionHead.Value.Value{$endif} then begin
    break;
   end;
  end;
  result:=OldHead.PointerValue;
 end else begin
  result:=nil;
 end;
end;
{$else}
begin
 result:=nil;
 if assigned(fHead) then begin
  fCriticalSection.Acquire;
  try
   if assigned(fHead) then begin
    result:=fHead;
    fHead:=pointer(result^);
   end;
  finally
   fCriticalSection.Leave;
  end;
 end;
end;
{$endif}

constructor TPasMPInterlockedQueue.Create;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
var Node:PPasMPInterlockedQueueNode;
begin
 inherited Create;
 fItemSize:=ItemSize;
 fInternalNodeSize:=TPasMP.RoundUpToPowerOfTwo(Max(SizeOf(TPasMPInterlockedQueueNode)+fItemSize,PasMPCPUCacheLineSize));
 fHead:=nil;
 fTail:=nil;
 TPasMPMemory.AllocateAlignedMemory(fHead,SizeOf(TPasMPTaggedPointer),PasMPCPUCacheLineSize);
 TPasMPMemory.AllocateAlignedMemory(fTail,SizeOf(TPasMPTaggedPointer),PasMPCPUCacheLineSize);
 TPasMPMemory.AllocateAlignedMemory(Node,SizeOf(TPasMPInterlockedQueueNode),PasMPCPUCacheLineSize);
 Node^.Previous.PointerValue:=nil;
 Node^.Previous.TagValue:=0;
 Node^.Next.PointerValue:=nil;
 Node^.Next.TagValue:=0;
 fHead^.PointerValue:=Node;
 fHead^.TagValue:=0;
 fTail^.PointerValue:=Node;
 fTail^.TagValue:=0;
end;
{$else}
begin
 inherited Create;
 fItemSize:=ItemSize;
 fInternalNodeSize:=TPasMP.RoundUpToPowerOfTwo(Max(SizeOf(TPasMPInterlockedQueueNode)+fItemSize,PasMPCPUCacheLineSize));
 fHeadCriticalSection:=TPasMPCriticalSection.Create;
 fTailCriticalSection:=TPasMPCriticalSection.Create;
 fHead:=nil;
 TPasMPMemory.AllocateAlignedMemory(fHead,fInternalNodeSize,PasMPCPUCacheLineSize);
 fHead^.Next:=nil;
 fTail:=fHead;
end;
{$endif}

destructor TPasMPInterlockedQueue.Destroy;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
var CurrentNode,NextNode:PPasMPInterlockedQueueNode;
    Item:pointer;
begin
 GetMem(Item,fItemSize);
 try
  repeat
  until not Dequeue(Item^);
 finally
  FreeMem(Item);
 end;
 if assigned(PPasMPInterlockedQueueNode(fTail)^.Previous.PointerValue) then begin
  TPasMPMemory.FreeAlignedMemory(PPasMPInterlockedQueueNode(fTail)^.Previous.PointerValue);
 end;
 TPasMPMemory.FreeAlignedMemory(fTail);
 TPasMPMemory.FreeAlignedMemory(fHead);
 inherited Destroy;
end;
{$else}
var CurrentNode,NextNode:PPasMPInterlockedQueueNode;
begin
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  NextNode:=CurrentNode^.Next;
  TPasMPMemory.FreeAlignedMemory(CurrentNode);
  CurrentNode:=NextNode;
 end;
 fTailCriticalSection.Free;
 fHeadCriticalSection.Free;
 inherited Destroy;
end;
{$endif}

procedure TPasMPInterlockedQueue.Clear;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
var Node:PPasMPInterlockedQueueNode;
    Item:pointer;
begin
 GetMem(Item,fItemSize);
 try
  repeat
  until not Dequeue(Item^);
 finally
  FreeMem(Item);
 end;
 if assigned(PPasMPInterlockedQueueNode(fTail)^.Previous.PointerValue) then begin
  TPasMPMemory.FreeAlignedMemory(PPasMPInterlockedQueueNode(fTail)^.Previous.PointerValue);
 end;
 TPasMPMemory.AllocateAlignedMemory(Node,fInternalNodeSize,PasMPCPUCacheLineSize);
 Node^.Previous.PointerValue:=nil;
 Node^.Previous.TagValue:=0;
 Node^.Next.PointerValue:=nil;
 Node^.Next.TagValue:=0;
 fHead^.PointerValue:=Node;
 fHead^.TagValue:=0;
 fTail^.PointerValue:=Node;
 fTail^.TagValue:=0;
end;
{$else}
var CurrentNode,NextNode:PPasMPInterlockedQueueNode;
begin
 CurrentNode:=fHead;
 while assigned(CurrentNode) do begin
  NextNode:=CurrentNode^.Next;
  TPasMPMemory.FreeAlignedMemory(CurrentNode);
  CurrentNode:=NextNode;
 end;
 fHead:=nil;
 TPasMPMemory.AllocateAlignedMemory(fHead,fInternalNodeSize,PasMPCPUCacheLineSize);
 fHead^.Next:=nil;
 fTail:=fHead;
end;
{$endif}

function TPasMPInterlockedQueue.IsEmpty:boolean;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
begin
 result:=fHead^.PointerValue=fTail^.PointerValue;
end;
{$else}
begin
 result:=fHead=fTail;
end;
{$endif}

procedure TPasMPInterlockedQueue.Enqueue(const Item);
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
// Based on http://people.csail.mit.edu/edya/publications/OptimisticFIFOQueue-journal.pdf
var Node:PPasMPInterlockedQueueNode;
    Tail,OldTail,NewTail:TPasMPTaggedPointer;
begin
 TPasMPMemory.AllocateAlignedMemory(Node,fInternalNodeSize,PasMPCPUCacheLineSize);
 Node^.Previous.PointerValue:=nil;
 Node^.Previous.TagValue:=0;
 Move(Item,Node^.Data,fItemSize);
 OldTail:=fTail^;
 repeat
  Tail:=OldTail;
  Node^.Next.PointerValue:=Tail.PointerValue;
  Node^.Next.TagValue:=Tail.TagValue+1;
  NewTail.PointerValue:=Node;
  NewTail.TagValue:=Tail.TagValue+1;
  OldTail.Value:=TPasMPInterlocked.CompareExchange(fTail^.Value,NewTail.Value,Tail.Value);
 until {$ifdef CPU64}(OldTail.PointerValue=Tail.PointerValue) and (OldTail.TagValue=Tail.TagValue){$else}OldTail.Value.Value=Tail.Value.Value{$endif};
 PPasMPInterlockedQueueNode(Tail.PointerValue)^.Previous.PointerValue:=Node;
 PPasMPInterlockedQueueNode(Tail.PointerValue)^.Previous.TagValue:=Tail.TagValue;
end;
{$else}
var Node:PPasMPInterlockedQueueNode;
begin
 TPasMPMemory.AllocateAlignedMemory(Node,fInternalNodeSize,PasMPCPUCacheLineSize);
 Node^.Next:=nil;
 Move(Item,Node^.Data,fItemSize);
 fTailCriticalSection.Acquire;
 try
  fTail^.Next:=Node;
  fTail:=Node;
 finally
  fTailCriticalSection.Release;
 end;
end;
{$endif}

function TPasMPInterlockedQueue.Dequeue(out Item):boolean;
{$ifdef HAS_DOUBLE_NATIVE_MACHINE_WORD_ATOMIC_COMPARE_EXCHANGE}
// Based on http://people.csail.mit.edu/edya/publications/OptimisticFIFOQueue-journal.pdf
var Tail,Head,CheckHead,FirstNodePrevious,NewHead,OldHead,CurrentNode,NextNode,NewNode:TPasMPTaggedPointer;
begin
 result:=false;
 repeat
  Head.Value:=fHead^.Value;
  Tail.Value:=fTail^.Value;
  FirstNodePrevious:=PPasMPInterlockedQueueNode(Head.PointerValue)^.Previous;
  CheckHead.Value:=fHead^.Value;
  if {$ifdef cpu64}(Head.PointerValue=CheckHead.PointerValue) and (Head.TagValue=CheckHead.TagValue){$else}Head.Value.Value=CheckHead.Value.Value{$endif} then begin
   if {$ifdef cpu64}(Head.PointerValue<>Tail.PointerValue) or (Head.TagValue<>Tail.TagValue){$else}Head.Value.Value<>Tail.Value.Value{$endif} then begin
    // Not in the original paper, but there is a race condition where push adds a node, but leaves Node^.Next^.Previous uninitialized for a short time.
    // This only manifests too when FirstNodePrevious.TagValue = Head.TagValue, which is also very rare. If they aren't equal, FixList fixes the issue
		// (or at least it takes long enough, so that things settle). So here ensure time is not wasted getting to the end-game only to try to dereference
    // nil.
    if assigned(FirstNodePrevious.PointerValue) then begin
     if FirstNodePrevious.TagValue<>Head.TagValue then begin
      // Fix list
      CurrentNode:=Tail;
      repeat
       CheckHead.Value:=fHead^.Value;
{$ifdef cpu64}
       if ((Head.PointerValue=CheckHead.PointerValue) and (Head.TagValue=CheckHead.TagValue)) and
          ((CurrentNode.PointerValue<>Head.PointerValue) or (CurrentNode.TagValue<>Head.TagValue)) then begin
{$else}
       if (Head.Value.Value=CheckHead.Value.Value) and (CurrentNode.Value.Value<>Head.Value.Value) then begin
{$endif}
        NextNode:=PPasMPInterlockedQueueNode(CurrentNode.PointerValue)^.Next;
        NewNode.PointerValue:=CurrentNode.PointerValue;
        NewNode.TagValue:=CurrentNode.TagValue-1;
        PPasMPInterlockedQueueNode(NextNode.PointerValue)^.Previous.Value:=NewNode.Value;
        CurrentNode.PointerValue:=NextNode.PointerValue;
        CurrentNode.TagValue:=NextNode.TagValue-1;
       end else begin
        break;
       end;
      until false;
     end else begin
      NewHead.PointerValue:=FirstNodePrevious.PointerValue;
      NewHead.TagValue:=Head.TagValue+1;
      OldHead.Value:=TPasMPInterlocked.CompareExchange(fHead^.Value,NewHead.Value,Head.Value);
      if {$ifdef CPU64}(OldHead.PointerValue=Head.PointerValue) and (OldHead.TagValue=Head.TagValue){$else}OldHead.Value.Value=Head.Value.Value{$endif} then begin
       Move(PPasMPInterlockedQueueNode(FirstNodePrevious.PointerValue)^.Data,Item,fItemSize);
       TPasMPMemory.FreeAlignedMemory(Head.PointerValue);
       result:=true;
       exit;
      end;
     end;
    end;
   end else begin
    break;
   end;
  end;
 until false;
end;
{$else}
var Node,NewHead:PPasMPInterlockedQueueNode;
begin
 result:=false;
 if assigned(fHead) and (fHead<>fTail) then begin
  fHeadCriticalSection.Acquire;
  try
   Node:=fHead;
   NewHead:=fHead^.Next;
   if assigned(NewHead) then begin
    Move(NewHead^.Data,Item,fItemSize);
    fHead:=NewHead;
    TPasMPMemory.FreeAlignedMemory(Node);
    result:=true;
   end;
  finally
   fHeadCriticalSection.Release;
  end;
 end;
end;
{$endif}

const PasMPInterlockedHashTableItemStateDeleted=-1;
      PasMPInterlockedHashTableItemStateEmpty=0;
      PasMPInterlockedHashTableItemStateUsed=1;

constructor TPasMPInterlockedHashTable.Create(const ItemSize:longint);
begin
 inherited Create;
 fCriticalSection:=TPasMPCriticalSection.Create;
 fLock:=0;
 fItemSize:=ItemSize;
 fInternalItemSize:=TPasMP.RoundUpToPowerOfTwo(Max(SizeOf(TPasMPInterlockedHashTableItem)+fItemSize,PasMPCPUCacheLineSize));
 TPasMPMemory.AllocateAlignedMemory(fFirstState,SizeOf(TPasMPInterlockedHashTableState),PasMPCPUCacheLineSize);
 FillChar(fFirstState^,SizeOf(TPasMPInterlockedHashTableState),#0);
 Initialize(fFirstState^);
 fFirstState^.Previous:=nil;
 fFirstState^.Next:=nil;
 fFirstState^.ReferenceCounter:=1;
 fFirstState^.Version:=0;
 fFirstState^.Size:=TPasMP.RoundUpToPowerOfTwo(Max(16,4096 div fInternalItemSize));
 fFirstState^.Mask:=fFirstState^.Size-1;
 fFirstState^.LogSize:=IntLog2(fFirstState^.Size);
 fFirstState^.Count:=0;
 TPasMPMemory.AllocateAlignedMemory(fFirstState^.Items,fFirstState^.Size*fInternalItemSize,PasMPCPUCacheLineSize);
 FillChar(fFirstState^.Items^,fFirstState^.Size*fInternalItemSize,#0);
 fLastState:=fFirstState;
end;

destructor TPasMPInterlockedHashTable.Destroy;
begin
 while assigned(fFirstState) do begin
  FreeState(fFirstState);
 end;
 fCriticalSection.Free;
 inherited Destroy;
end;

function TPasMPInterlockedHashTable.CreateState:PPasMPInterlockedHashTableState;
begin
 TPasMPMemory.AllocateAlignedMemory(result,SizeOf(TPasMPInterlockedHashTableState),PasMPCPUCacheLineSize);
 FillChar(result^,SizeOf(TPasMPInterlockedHashTableState),#0);
 Initialize(result^);
 result^.Previous:=nil;
 result^.Next:=nil;
 result^.ReferenceCounter:=1;
end;

procedure TPasMPInterlockedHashTable.FreeState(const State:PPasMPInterlockedHashTableState);
var Index:longint;
    Item:PPasMPInterlockedHashTableItem;
begin
 if assigned(State^.Previous) then begin
  State^.Previous^.Next:=State^.Next;
 end else if fFirstState=State then begin
  fFirstState:=State^.Next;
 end;
 if assigned(State^.Next) then begin
  State^.Next^.Previous:=State^.Previous;
 end else if fLastState=State then begin
  fLastState:=State^.Previous;
 end;
 Item:=State^.Items;
 for Index:=0 to State^.Size-1 do begin
  FinalizeItem(@Item^.Data);
  Finalize(Item^);
  inc(TPasMPPtrUInt(Item),fInternalItemSize);
 end;
 TPasMPMemory.FreeAlignedMemory(State^.Items);
 Finalize(State^);
 TPasMPMemory.FreeAlignedMemory(State);
end;

function TPasMPInterlockedHashTable.AcquireState:PPasMPInterlockedHashTableState;
var Lock:longint;
begin
 repeat
  Lock:=fLock and longint(longword($fffffffe));
 until TPasMPInterlocked.CompareExchange(fLock,Lock+2,Lock)=Lock;
 result:=fLastState;
 TPasMPInterlocked.Increment(result^.ReferenceCounter);
 TPasMPInterlocked.Sub(fLock,2);
end;

procedure TPasMPInterlockedHashTable.ReleaseState(const State:PPasMPInterlockedHashTableState);
var Lock:longint;
begin
 if TPasMPInterlocked.Decrement(State^.ReferenceCounter)=0 then begin
  repeat
   Lock:=fLock and longint(longword($fffffffe));
  until TPasMPInterlocked.CompareExchange(fLock,Lock or 1,Lock)=Lock;
  repeat
  until fLock=1;
  try
   if State^.ReferenceCounter=0 then begin
    FreeState(State);
   end;
  finally
   TPasMPInterlocked.Write(fLock,0);
  end;
 end;
end;

procedure TPasMPInterlockedHashTable.InitializeItem(const Data:pointer);
begin
end;

procedure TPasMPInterlockedHashTable.FinalizeItem(const Data:pointer);
begin
end;

procedure TPasMPInterlockedHashTable.CopyItem(const Source,Destination:pointer);
begin
end;

procedure TPasMPInterlockedHashTable.GetKey(const Data,Key:pointer);
begin
end;

procedure TPasMPInterlockedHashTable.SetKey(const Data,Key:pointer);
begin
end;

procedure TPasMPInterlockedHashTable.GetValue(const Data,Value:pointer);
begin
end;

procedure TPasMPInterlockedHashTable.SetValue(const Data,Value:pointer);
begin
end;

function TPasMPInterlockedHashTable.HashKey(const Key:pointer):TPasMPInterlockedHashTableHash;
begin
 result:=0;
end;

function TPasMPInterlockedHashTable.CompareKey(const Data,Key:pointer):boolean;
begin
 result:=false;
end;

procedure TPasMPInterlockedHashTable.Clear;
begin
end;

function TPasMPInterlockedHashTable.GetKeyValue(const Key,Value:pointer):boolean;
var CurrentState:PPasMPInterlockedHashTableState;
    Hash:TPasMPInterlockedHashTableHash;
    StartIndex,Index,Step,ItemLock:longint;
    Item:PPasMPInterlockedHashTableItem;
begin
 CurrentState:=AcquireState;
 Hash:=HashKey(Key);
 StartIndex:=(Hash shr (32-CurrentState^.LogSize)) and CurrentState^.Mask;
 Step:=((Hash shl 1) or 1) and CurrentState^.Mask;
 Index:=StartIndex;
 repeat
  Item:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(CurrentState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(Index)*TPasMPPtrUInt(fInternalItemSize))));
  case Item^.State of
   PasMPInterlockedHashTableItemStateDeleted:begin
    // Found deleted item slot => ignore it
   end;
   PasMPInterlockedHashTableItemStateEmpty:begin
    // Found empty item slot => abort search
    break;
   end;
   PasMPInterlockedHashTableItemStateUsed:begin
    // Found used item slot => try to read it
    if Item^.Hash=Hash then begin
     repeat
      ItemLock:=Item^.Lock and longint(longword($fffffffe));
     until TPasMPInterlocked.CompareExchange(Item^.Lock,ItemLock+2,ItemLock)=ItemLock;
     if (Item^.State=PasMPInterlockedHashTableItemStateUsed) and (Item^.Hash=Hash) and CompareKey(@Item^.Data,Key) then begin
      GetValue(@Item^.Data,Value);
      TPasMPInterlocked.Sub(Item^.Lock,2);
      ReleaseState(CurrentState);
      result:=true;
      exit;
     end;
     TPasMPInterlocked.Sub(Item^.Lock,2);
    end;
   end;
  end;
  Index:=(Index+Step) and CurrentState^.Mask;
 until Index=StartIndex;
 ReleaseState(CurrentState);
 result:=false;
end;

function TPasMPInterlockedHashTable.SetKeyValueOnState(const CurrentState:PPasMPInterlockedHashTableState;const Key,Value:pointer):boolean;
var Hash:TPasMPInterlockedHashTableHash;
    StartIndex,Index,Step,FoundDeletedItemSlotIndex,ItemLock:longint;
    Item:PPasMPInterlockedHashTableItem;
    FoundDeletedItemSlot:boolean;
begin

 Hash:=HashKey(Key);

 StartIndex:=(Hash shr (32-CurrentState^.LogSize)) and CurrentState^.Mask;
 Step:=((Hash shl 1) or 1) and CurrentState^.Mask;

 FoundDeletedItemSlotIndex:=-1;

 // First try to set a existent or empty slot item
 Index:=StartIndex;
 repeat
  Item:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(CurrentState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(Index)*TPasMPPtrUInt(fInternalItemSize))));
  case Item^.State of
   PasMPInterlockedHashTableItemStateDeleted:begin
    // Found deleted item slot => remember it for the next try iteration
    FoundDeletedItemSlotIndex:=Index;
   end;
   PasMPInterlockedHashTableItemStateEmpty:begin
    // Found empty item slot => try to use it
    repeat
     ItemLock:=Item^.Lock and longint(longword($fffffffe));
    until TPasMPInterlocked.CompareExchange(Item^.Lock,ItemLock+2,ItemLock)=ItemLock;
    if Item^.State=PasMPInterlockedHashTableItemStateEmpty then begin
     repeat
      ItemLock:=Item^.Lock and longint(longword($fffffffe));
     until TPasMPInterlocked.CompareExchange(Item^.Lock,(ItemLock-2) or 1,ItemLock)=ItemLock;
     repeat
     until Item^.Lock=1;
     Item^.Hash:=Hash;
     InitializeItem(@Item^.Data);
     SetKey(@Item^.Data,Key);
     SetValue(@Item^.Data,Value);
     TPasMPInterlocked.Write(Item^.State,PasMPInterlockedHashTableItemStateUsed);
     TPasMPInterlocked.Write(Item^.Lock,0);
     TPasMPInterlocked.Increment(CurrentState^.Count);
     result:=true;
     exit;
    end;
    TPasMPInterlocked.Sub(Item^.Lock,2);
   end;
   PasMPInterlockedHashTableItemStateUsed:begin
    // Found used item slot => try to overwrite it
    if Item^.Hash=Hash then begin
     repeat
      ItemLock:=Item^.Lock and longint(longword($fffffffe));
     until TPasMPInterlocked.CompareExchange(Item^.Lock,ItemLock+2,ItemLock)=ItemLock;
     if (Item^.State=PasMPInterlockedHashTableItemStateUsed) and (Item^.Hash=Hash) and CompareKey(@Item^.Data,Key) then begin
      repeat
       ItemLock:=Item^.Lock and longint(longword($fffffffe));
      until TPasMPInterlocked.CompareExchange(Item^.Lock,(ItemLock-2) or 1,ItemLock)=ItemLock;
      repeat
      until Item^.Lock=1;
      SetValue(@Item^.Data,Value);
      TPasMPInterlocked.Write(Item^.Lock,0);
      result:=true;
      exit;
     end;
     TPasMPInterlocked.Sub(Item^.Lock,2);
    end;
   end;
  end;
  Index:=(Index+Step) and CurrentState^.Mask;
 until Index=StartIndex;

 // Otherwise try to set the last found deleted slot item
 if FoundDeletedItemSlotIndex>=0 then begin
  Index:=FoundDeletedItemSlotIndex;
  Item:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(CurrentState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(Index)*TPasMPPtrUInt(fInternalItemSize))));
  repeat
   ItemLock:=Item^.Lock and longint(longword($fffffffe));
  until TPasMPInterlocked.CompareExchange(Item^.Lock,ItemLock+2,ItemLock)=ItemLock;
  if Item^.State=PasMPInterlockedHashTableItemStateDeleted then begin
   repeat
    ItemLock:=Item^.Lock and longint(longword($fffffffe));
   until TPasMPInterlocked.CompareExchange(Item^.Lock,(ItemLock-2) or 1,ItemLock)=ItemLock;
   repeat
   until Item^.Lock=1;
   InitializeItem(@Item^.Data);
   Item^.Hash:=Hash;
   SetKey(@Item^.Data,Key);
   SetValue(@Item^.Data,Value);
   TPasMPInterlocked.Write(Item^.State,PasMPInterlockedHashTableItemStateUsed);
   TPasMPInterlocked.Write(Item^.Lock,0);
   TPasMPInterlocked.Increment(CurrentState^.Count);
   result:=true;
   exit;
  end;
  TPasMPInterlocked.Sub(Item^.Lock,2);
 end;

 // Otherwise try to find and set a deleted slot item
 Index:=StartIndex;
 repeat
  Item:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(CurrentState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(Index)*TPasMPPtrUInt(fInternalItemSize))));
  case Item^.State of
   PasMPInterlockedHashTableItemStateDeleted:begin
    repeat
     ItemLock:=Item^.Lock and longint(longword($fffffffe));
    until TPasMPInterlocked.CompareExchange(Item^.Lock,ItemLock+2,ItemLock)=ItemLock;
    if Item^.State=PasMPInterlockedHashTableItemStateDeleted then begin
     repeat
      ItemLock:=Item^.Lock and longint(longword($fffffffe));
     until TPasMPInterlocked.CompareExchange(Item^.Lock,(ItemLock-2) or 1,ItemLock)=ItemLock;
     repeat
     until Item^.Lock=1;
     Item^.Hash:=Hash;
     InitializeItem(@Item^.Data);
     SetKey(@Item^.Data,Key);
     SetValue(@Item^.Data,Value);
     TPasMPInterlocked.Write(Item^.State,PasMPInterlockedHashTableItemStateUsed);
     TPasMPInterlocked.Write(Item^.Lock,0);
     TPasMPInterlocked.Increment(CurrentState^.Count);
     result:=true;
     exit;
    end;
    TPasMPInterlocked.Sub(Item^.Lock,2);
   end;
  end;
  Index:=(Index+Step) and CurrentState^.Mask;
 until Index=StartIndex;

 result:=false;

end;

procedure TPasMPInterlockedHashTable.Grow;
var CurrentState,NewState:PPasMPInterlockedHashTableState;
    StartIndex,Index,Step,OtherIndex,OtherStep:longint;
    Item,OtherItem:PPasMPInterlockedHashTableItem;
    GlobalLock,ItemLock:longint;
    FoundDeletedItemSlot:boolean;
begin
 CurrentState:=fLastState;
 if CurrentState^.Count>=CurrentState^.Size then begin
  NewState:=CreateState;
  NewState^.Version:=CurrentState^.Version+1;
  NewState^.Size:=CurrentState^.Size shl 1;
  NewState^.Mask:=NewState^.Size-1;
  NewState^.LogSize:=CurrentState^.LogSize+1;
  NewState^.Count:=CurrentState^.Count;
  TPasMPMemory.AllocateAlignedMemory(NewState^.Items,NewState^.Size*fInternalItemSize,PasMPCPUCacheLineSize);
  FillChar(NewState^.Items^,NewState^.Size*fInternalItemSize,#0);
  OtherIndex:=0;
  while OtherIndex<CurrentState^.Size do begin
   OtherItem:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(CurrentState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(OtherIndex)*TPasMPPtrUInt(fInternalItemSize))));
   if OtherItem^.State=PasMPInterlockedHashTableItemStateUsed then begin
    repeat
     ItemLock:=OtherItem^.Lock and longint(longword($fffffffe));
    until TPasMPInterlocked.CompareExchange(OtherItem^.Lock,ItemLock+2,ItemLock)=ItemLock;
    if OtherItem^.State=PasMPInterlockedHashTableItemStateUsed then begin
     StartIndex:=(OtherItem^.Hash shr (32-NewState^.LogSize)) and NewState^.Mask;
     Step:=((OtherItem^.Hash shl 1) or 1) and NewState^.Mask;
     Index:=StartIndex;
     repeat
      Item:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(NewState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(Index)*TPasMPPtrUInt(fInternalItemSize))));
      if Item^.State=PasMPInterlockedHashTableItemStateEmpty then begin
       Item^.Hash:=OtherItem^.Hash;
       InitializeItem(@Item^.Data);
       CopyItem(@OtherItem^.Data,@Item^.Data);
       Item^.State:=PasMPInterlockedHashTableItemStateUsed;
       break;
      end;
     until Index=StartIndex;
    end;
    TPasMPInterlocked.Sub(OtherItem^.Lock,2);
   end;
   inc(Index);
  end;
  if assigned(fLastState) then begin
   fLastState^.Next:=NewState;
   NewState^.Previous:=fLastState;
  end else begin
   NewState^.Previous:=nil;
   fFirstState:=NewState;
  end;
  NewState^.Next:=nil;
  fLastState:=NewState;
  TPasMPInterlocked.Decrement(CurrentState^.ReferenceCounter);
 end;
end;

function TPasMPInterlockedHashTable.SetKeyValue(const Key,Value:pointer):boolean;
var CurrentState:PPasMPInterlockedHashTableState;
    Version,StateLock,GlobalLock:longint;
begin
 repeat
  result:=false;
  CurrentState:=AcquireState;
  Version:=CurrentState^.Version;
  try
   repeat
    StateLock:=CurrentState^.Lock and longint(longword($fffffffe));
   until TPasMPInterlocked.CompareExchange(CurrentState^.Lock,StateLock+2,StateLock)=StateLock;
   if SetKeyValueOnState(CurrentState,Key,Value) then begin
    TPasMPInterlocked.Sub(CurrentState^.Lock,2);
    result:=Version=fLastState^.Version;
   end else begin
    // Otherwise as last solution, grow the hash table
    repeat
     GlobalLock:=fLock and longint(longword($fffffffe));
    until TPasMPInterlocked.CompareExchange(fLock,GlobalLock or 1,GlobalLock)=GlobalLock;
    repeat
    until fLock=1;
    try
     repeat
      StateLock:=CurrentState^.Lock and longint(longword($fffffffe));
     until TPasMPInterlocked.CompareExchange(CurrentState^.Lock,(StateLock-2) or 1,StateLock)=StateLock;
     repeat
     until CurrentState^.Lock=1;
     try
      Grow;
     finally
      TPasMPInterlocked.Write(CurrentState^.Lock,0);
     end;
    finally
     TPasMPInterlocked.Write(fLock,0);
    end;
   end;
  finally
   ReleaseState(CurrentState);
  end;
 until result and (Version=fLastState^.Version); // Check if we're done or when we do need to restart after a grow race condition case
end;

function TPasMPInterlockedHashTable.DeleteKey(const Key:pointer):boolean;
var CurrentState:PPasMPInterlockedHashTableState;
    Hash:TPasMPInterlockedHashTableHash;
    StartIndex,Index,Step,ItemLock,Version:longint;
    Item:PPasMPInterlockedHashTableItem;
begin
 repeat
  CurrentState:=AcquireState;
  Version:=CurrentState^.Version;
  Hash:=HashKey(Key);
  StartIndex:=(Hash shr (32-CurrentState^.LogSize)) and CurrentState^.Mask;
  Step:=((Hash shl 1) or 1) and CurrentState^.Mask;
  Index:=StartIndex;
  repeat
   Item:=pointer(TPasMPPtrUInt(TPasMPPtrUInt(CurrentState^.Items)+TPasMPPtrUInt(TPasMPPtrUInt(Index)*TPasMPPtrUInt(fInternalItemSize))));
   case Item^.State of
    PasMPInterlockedHashTableItemStateDeleted:begin
     // Found deleted item slot => ignore it
    end;
    PasMPInterlockedHashTableItemStateEmpty:begin
     // Found empty item slot => abort search
     break;
    end;
    PasMPInterlockedHashTableItemStateUsed:begin
     // Found used item slot => try to read it
     if Item^.Hash=Hash then begin
      repeat
       ItemLock:=Item^.Lock and longint(longword($fffffffe));
      until TPasMPInterlocked.CompareExchange(Item^.Lock,ItemLock+2,ItemLock)=ItemLock;
      if (Item^.State=PasMPInterlockedHashTableItemStateUsed) and (Item^.Hash=Hash) and CompareKey(@Item^.Data,Key) then begin
       repeat
        ItemLock:=Item^.Lock and longint(longword($fffffffe));
       until TPasMPInterlocked.CompareExchange(Item^.Lock,(ItemLock-2) or 1,ItemLock)=ItemLock;
       FinalizeItem(@Item^.Data);
       TPasMPInterlocked.Write(Item^.State,PasMPInterlockedHashTableItemStateDeleted);
       TPasMPInterlocked.Write(Item^.Lock,0);
       TPasMPInterlocked.Decrement(CurrentState^.Count);
       ReleaseState(CurrentState);
       if Version=fLastState^.Version then begin
        result:=true;
        exit;
       end else begin
        continue;
       end;
      end;
      TPasMPInterlocked.Sub(Item^.Lock,2);
     end;
    end;
   end;
   Index:=(Index+Step) and CurrentState^.Mask;
  until Index=StartIndex;
  ReleaseState(CurrentState);
 until Version=fLastState^.Version; // Check when we do need to restart after a grow race condition case
 result:=false;
end;

constructor TPasMPSingleProducerSingleConsumerRingBuffer.Create(const Size:longint);
begin
 inherited Create;
 fSize:=Size;
 fReadIndex:=0;
 fWriteIndex:=0;
 fData:=nil;
 SetLength(fData,fSize);
end;

destructor TPasMPSingleProducerSingleConsumerRingBuffer.Destroy;
begin
 SetLength(fData,0);
 inherited Destroy;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.Read(const Buffer:pointer;Bytes:longint):longint;
var LocalReadIndex,LocalWriteIndex,ToRead:longint;
    p:PByte;
begin
 if (Bytes=0) or (Bytes>fSize) then begin
  result:=0;
 end else begin
  repeat
{$if not (defined(CPU386) or defined(CPUx86_64))}
   TPasMPMemoryBarrier.ReadWrite;
{$ifend}
   LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
   TPasMPMemoryBarrier.ReadDependency;
{$else}
   TPasMPMemoryBarrier.Read;
{$ifend}
   LocalWriteIndex:=fWriteIndex;
   if LocalWriteIndex>=LocalReadIndex then begin
    result:=LocalWriteIndex-LocalReadIndex;
   end else begin
    result:=(fSize-LocalReadIndex)+LocalWriteIndex;
   end;
   if Bytes<=result then begin
    break;
   end else begin
    TPasMP.Yield;
   end;
  until false;
  p:=pointer(Buffer);
  if (LocalReadIndex+Bytes)>fSize then begin
   ToRead:=fSize-LocalReadIndex;
   Move(fData[LocalReadIndex],p^,ToRead);
   inc(p,ToRead);
   dec(Bytes,ToRead);
   LocalReadIndex:=0;
  end;
  if Bytes>0 then begin
   Move(fData[LocalReadIndex],p^,Bytes);
   inc(LocalReadIndex,Bytes);
   if LocalReadIndex>=fSize then begin
    dec(LocalReadIndex,fSize);
   end;
  end;
{$ifdef CPU386}
  asm
   mfence
  end;
{$else}
  TPasMPMemoryBarrier.ReadWrite;
{$endif}
  fReadIndex:=LocalReadIndex;
  result:=Bytes;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.TryRead(const Buffer:pointer;Bytes:longint):longint;
var LocalReadIndex,LocalWriteIndex,ToRead:longint;
    p:PByte;
begin
 if (Bytes=0) or (Bytes>fSize) then begin
  result:=0;
 end else begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
  TPasMPMemoryBarrier.ReadWrite;
{$ifend}
  LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  LocalWriteIndex:=fWriteIndex;
  if LocalWriteIndex>=LocalReadIndex then begin
   result:=LocalWriteIndex-LocalReadIndex;
  end else begin
   result:=(fSize-LocalReadIndex)+LocalWriteIndex;
  end;
  if Bytes>result then begin
   result:=0;
  end else begin
   p:=pointer(Buffer);
   if (LocalReadIndex+Bytes)>fSize then begin
    ToRead:=fSize-LocalReadIndex;
    Move(fData[LocalReadIndex],p^,ToRead);
    inc(p,ToRead);
    dec(Bytes,ToRead);
    LocalReadIndex:=0;
   end;
   if Bytes>0 then begin
    Move(fData[LocalReadIndex],p^,Bytes);
    inc(LocalReadIndex,Bytes);
    if LocalReadIndex>=fSize then begin
     dec(LocalReadIndex,fSize);
    end;
   end;
{$ifdef CPU386}
   asm
    mfence
   end;
{$else}
   TPasMPMemoryBarrier.ReadWrite;
{$endif}
   fReadIndex:=LocalReadIndex;
   result:=Bytes;
  end;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.ReadAsMuchAsPossible(const Buffer:pointer;Bytes:longint):longint;
var LocalReadIndex,LocalWriteIndex,ToRead:longint;
    p:PByte;
begin
 if (Bytes=0) or (Bytes>fSize) then begin
  result:=0;
 end else begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
  TPasMPMemoryBarrier.ReadWrite;
{$ifend}
  LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  LocalWriteIndex:=fWriteIndex;
  if LocalWriteIndex>=LocalReadIndex then begin
   result:=LocalWriteIndex-LocalReadIndex;
  end else begin
   result:=(fSize-LocalReadIndex)+LocalWriteIndex;
  end;
  if Bytes>result then begin
   Bytes:=result;
  end;
  if Bytes>0 then begin
   p:=pointer(Buffer);
   if (LocalReadIndex+Bytes)>fSize then begin
    ToRead:=fSize-LocalReadIndex;
    Move(fData[LocalReadIndex],p^,ToRead);
    inc(p,ToRead);
    dec(Bytes,ToRead);
    LocalReadIndex:=0;
   end;
   if Bytes>0 then begin
    Move(fData[LocalReadIndex],p^,Bytes);
    inc(LocalReadIndex,Bytes);
    if LocalReadIndex>=fSize then begin
     dec(LocalReadIndex,fSize);
    end;
   end;
{$ifdef CPU386}
   asm
    mfence
   end;
{$else}
   TPasMPMemoryBarrier.ReadWrite;
{$endif}
   fReadIndex:=LocalReadIndex;
  end;
  result:=Bytes;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.Write(const Buffer:pointer;Bytes:longint):longint;
var LocalReadIndex,LocalWriteIndex,ToWrite:longint;
    p:PByte;
begin
 if (Bytes=0) or (Bytes>fSize) then begin
  result:=0;
 end else begin
  repeat
{$if not (defined(CPU386) or defined(CPUx86_64))}
   TPasMPMemoryBarrier.ReadWrite;
{$ifend}
   LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
   TPasMPMemoryBarrier.ReadDependency;
{$else}
   TPasMPMemoryBarrier.Read;
{$ifend}
   LocalWriteIndex:=fWriteIndex;
   if LocalWriteIndex>=LocalReadIndex then begin
    result:=((fSize+LocalReadIndex)-LocalWriteIndex)-1;
   end else begin
    result:=(LocalReadIndex-LocalWriteIndex)-1;
   end;
   if Bytes<=result then begin
    break;
   end else begin
    TPasMP.Yield;
   end;
  until false;
  p:=pointer(Buffer);
  if (LocalWriteIndex+Bytes)>fSize then begin
   ToWrite:=fSize-LocalWriteIndex;
   Move(p^,fData[LocalWriteIndex],ToWrite);
   inc(p,ToWrite);
   dec(Bytes,ToWrite);
   LocalWriteIndex:=0;
  end;
  if Bytes>0 then begin
   Move(p^,fData[LocalWriteIndex],Bytes);
   inc(LocalWriteIndex,Bytes);
   if LocalWriteIndex>=fSize then begin
    dec(LocalWriteIndex,fSize);
   end;
  end;
{$ifdef CPU386}
  asm
   mfence
  end;
{$else}
  TPasMPMemoryBarrier.ReadWrite;
{$endif}
  fWriteIndex:=LocalWriteIndex;
  result:=Bytes;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.TryWrite(const Buffer:pointer;Bytes:longint):longint;
var LocalReadIndex,LocalWriteIndex,ToWrite:longint;
    p:PByte;
begin
 if (Bytes=0) or (Bytes>fSize) then begin
  result:=0;
 end else begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
  TPasMPMemoryBarrier.ReadWrite;
{$ifend}
  LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  LocalWriteIndex:=fWriteIndex;
  if LocalWriteIndex>=LocalReadIndex then begin
   result:=((fSize+LocalReadIndex)-LocalWriteIndex)-1;
  end else begin
   result:=(LocalReadIndex-LocalWriteIndex)-1;
  end;
  if Bytes>result then begin
   result:=0;
  end else begin
   p:=pointer(Buffer);
   if (LocalWriteIndex+Bytes)>fSize then begin
    ToWrite:=fSize-LocalWriteIndex;
    Move(p^,fData[LocalWriteIndex],ToWrite);
    inc(p,ToWrite);
    dec(Bytes,ToWrite);
    LocalWriteIndex:=0;
   end;
   if Bytes>0 then begin
    Move(p^,fData[LocalWriteIndex],Bytes);
    inc(LocalWriteIndex,Bytes);
    if LocalWriteIndex>=fSize then begin
     dec(LocalWriteIndex,fSize);
    end;
   end;
{$ifdef CPU386}
   asm
    mfence
   end;
{$else}
   TPasMPMemoryBarrier.ReadWrite;
{$endif}
   fWriteIndex:=LocalWriteIndex;
   result:=Bytes;
  end;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.WriteAsMuchAsPossible(const Buffer:pointer;Bytes:longint):longint;
var LocalReadIndex,LocalWriteIndex,ToWrite:longint;
    p:PByte;
begin
 if (Bytes=0) or (Bytes>fSize) then begin
  result:=0;
 end else begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
  TPasMPMemoryBarrier.ReadWrite;
{$ifend}
  LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  LocalWriteIndex:=fWriteIndex;
  if LocalWriteIndex>=LocalReadIndex then begin
   result:=((fSize+LocalReadIndex)-LocalWriteIndex)-1;
  end else begin
   result:=(LocalReadIndex-LocalWriteIndex)-1;
  end;
  if Bytes>result then begin
   Bytes:=result;
  end;
  if Bytes>0 then begin
   p:=pointer(Buffer);
   if (LocalWriteIndex+Bytes)>fSize then begin
    ToWrite:=fSize-LocalWriteIndex;
    Move(p^,fData[LocalWriteIndex],ToWrite);
    inc(p,ToWrite);
    dec(Bytes,ToWrite);
    LocalWriteIndex:=0;
   end;
   if Bytes>0 then begin
    Move(p^,fData[LocalWriteIndex],Bytes);
    inc(LocalWriteIndex,Bytes);
    if LocalWriteIndex>=fSize then begin
     dec(LocalWriteIndex,fSize);
    end;
   end;
{$ifdef CPU386}
   asm
    mfence
   end;
{$else}
   TPasMPMemoryBarrier.ReadWrite;
{$endif}
   fWriteIndex:=LocalWriteIndex;
  end;
  result:=Bytes;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.AvailableForRead:longint;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=LocalWriteIndex-LocalReadIndex;
 end else begin
  result:=(fSize-LocalReadIndex)+LocalWriteIndex;
 end;
end;

function TPasMPSingleProducerSingleConsumerRingBuffer.AvailableForWrite:longint;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=((fSize+LocalReadIndex)-LocalWriteIndex)-1;
 end else begin
  result:=(LocalReadIndex-LocalWriteIndex)-1;
 end;
end;

constructor TPasMPSingleProducerSingleConsumerBoundedQueue.Create(const MaximalCount,ItemSize:longint);
begin
 inherited Create;
 fMaximalCount:=MaximalCount;
 fItemSize:=ItemSize;
 fReadIndex:=0;
 fWriteIndex:=0;
 fData:=nil;
 SetLength(fData,fMaximalCount*fItemSize);
end;

destructor TPasMPSingleProducerSingleConsumerBoundedQueue.Destroy;
begin
 SetLength(fData,0);
 inherited Destroy;
end;

function TPasMPSingleProducerSingleConsumerBoundedQueue.Enqueue(const Item):boolean;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=(((fMaximalCount+LocalReadIndex)-LocalWriteIndex)-1)>0;
 end else begin
  result:=((LocalReadIndex-LocalWriteIndex)-1)>0;
 end;
 if result then begin
  LocalWriteIndex:=fWriteIndex;
  Move(Item,fData[LocalWriteIndex*fItemSize],fItemSize);
  inc(LocalWriteIndex);
  if LocalWriteIndex>=fMaximalCount then begin
   LocalWriteIndex:=0;
  end;
  TPasMPMemoryBarrier.ReadWrite;
  fWriteIndex:=LocalWriteIndex;
 end;
end;

function TPasMPSingleProducerSingleConsumerBoundedQueue.Dequeue(out Item):boolean;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=(LocalWriteIndex-LocalReadIndex)>0;
 end else begin
  result:=((fMaximalCount-LocalReadIndex)+LocalWriteIndex)>0;
 end;
 if result then begin
  LocalReadIndex:=fReadIndex;
  Move(fData[LocalReadIndex*fItemSize],Item,fItemSize);
  inc(LocalReadIndex);
  if LocalReadIndex>=fMaximalCount then begin
   LocalReadIndex:=0;
  end;
  TPasMPMemoryBarrier.ReadWrite;
  fReadIndex:=LocalReadIndex;
 end;
end;

function TPasMPSingleProducerSingleConsumerBoundedQueue.AvailableForEnqueue:longint;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=((fMaximalCount+LocalReadIndex)-LocalWriteIndex)-1;
 end else begin
  result:=(LocalReadIndex-LocalWriteIndex)-1;
 end;
end;

function TPasMPSingleProducerSingleConsumerBoundedQueue.AvailableForDequeue:longint;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=LocalWriteIndex-LocalReadIndex;
 end else begin
  result:=(fMaximalCount-LocalReadIndex)+LocalWriteIndex;
 end;
end;

{$ifdef HAS_GENERICS}
constructor TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>.Create(const MaximalCount:longint);
begin
 inherited Create;
 fMaximalCount:=MaximalCount;
 fReadIndex:=0;
 fWriteIndex:=0;
 fData:=nil;
 SetLength(fData,fMaximalCount);
end;

destructor TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>.Destroy;
begin
 SetLength(fData,0);
 inherited Destroy;
end;

function TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>.Enqueue(const Item:T):boolean;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=(((fMaximalCount+LocalReadIndex)-LocalWriteIndex)-1)>0;
 end else begin
  result:=((LocalReadIndex-LocalWriteIndex)-1)>0;
 end;
 if result then begin
  LocalWriteIndex:=fWriteIndex;
  fData[LocalWriteIndex]:=Item;
  inc(LocalWriteIndex);
  if LocalWriteIndex>=fMaximalCount then begin
   LocalWriteIndex:=0;
  end;
  TPasMPMemoryBarrier.ReadWrite;
  fWriteIndex:=LocalWriteIndex;
 end;
end;

function TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>.Dequeue(out Item:T):boolean;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=(LocalWriteIndex-LocalReadIndex)>0;
 end else begin
  result:=((fMaximalCount-LocalReadIndex)+LocalWriteIndex)>0;
 end;
 if result then begin
  LocalReadIndex:=fReadIndex;
  Item:=fData[LocalReadIndex];
  inc(LocalReadIndex);
  if LocalReadIndex>=fMaximalCount then begin
   LocalReadIndex:=0;
  end;
  TPasMPMemoryBarrier.ReadWrite;
  fReadIndex:=LocalReadIndex;
 end;
end;

function TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>.AvailableForEnqueue:longint;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=((fMaximalCount+LocalReadIndex)-LocalWriteIndex)-1;
 end else begin
  result:=(LocalReadIndex-LocalWriteIndex)-1;
 end;
end;

function TPasMPSingleProducerSingleConsumerBoundedTypedQueue<T>.AvailableForDequeue:longint;
var LocalReadIndex,LocalWriteIndex:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.ReadWrite;
{$ifend}
 LocalReadIndex:=fReadIndex;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 LocalWriteIndex:=fWriteIndex;
 if LocalWriteIndex>=LocalReadIndex then begin
  result:=LocalWriteIndex-LocalReadIndex;
 end else begin
  result:=(fMaximalCount-LocalReadIndex)+LocalWriteIndex;
 end;
end;
{$endif}

constructor TPasMPBoundedStack.Create(const MaximalCount,ItemSize:longint);
var i:longint;
    p:PByte;
    StackItem:PPasMPBoundedStackItem;
begin
 inherited Create;
 fStack:=TPasMPInterlockedStack.Create;
 fFree:=TPasMPInterlockedStack.Create;
 fMaximalCount:=MaximalCount;
 fItemSize:=ItemSize;
 fInternalItemSize:=TPasMP.RoundUpToPowerOfTwo(Max(SizeOf(TPasMPBoundedStackItem)+fItemSize,PasMPCPUCacheLineSize));
 TPasMPMemory.AllocateAlignedMemory(fData,fInternalItemSize*fMaximalCount,PasMPCPUCacheLineSize);
 p:=fData;
 for i:=0 to fMaximalCount-1 do begin
  StackItem:=pointer(p);
  inc(p,fInternalItemSize);
  fFree.Push(StackItem);
 end;
end;

destructor TPasMPBoundedStack.Destroy;
begin
 TPasMPMemory.FreeAlignedMemory(fData);
 fFree.Free;
 fStack.Free;
 inherited Destroy;
end;

function TPasMPBoundedStack.IsEmpty:boolean;
begin
 result:=fStack.IsEmpty;
end;

function TPasMPBoundedStack.IsFull:boolean;
begin
 result:=fFree.IsEmpty;
end;

function TPasMPBoundedStack.Push(const Item):boolean;
var StackItem:PPasMPBoundedStackItem;
begin
 StackItem:=fFree.Pop;
 if assigned(StackItem) then begin
  Move(Item,StackItem^.Data,fItemSize);
  fStack.Push(StackItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TPasMPBoundedStack.Pop(out Item):boolean;
var StackItem:PPasMPBoundedStackItem;
begin
 StackItem:=fStack.Pop;
 if assigned(StackItem) then begin
  Move(StackItem^.Data,Item,fItemSize);
  fFree.Push(StackItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;

{$ifdef HAS_GENERICS}
constructor TPasMPBoundedTypedStack<T>.Create(const MaximalCount:longint);
var i:longint;
    p:PByte;
    StackItem:PPasMPBoundedTypedStackItem;
begin
 inherited Create;
 fStack:=TPasMPInterlockedStack.Create;
 fFree:=TPasMPInterlockedStack.Create;
 fMaximalCount:=MaximalCount;
 fInternalItemSize:=Max(TPasMP.RoundUpToPowerOfTwo(SizeOf(TPasMPBoundedTypedStackItem)),PasMPCPUCacheLineSize);
 TPasMPMemory.AllocateAlignedMemory(fData,fInternalItemSize*fMaximalCount,PasMPCPUCacheLineSize);
 p:=fData;
 for i:=0 to fMaximalCount-1 do begin
  StackItem:=pointer(p);
  Initialize(StackItem^);
  inc(p,fInternalItemSize);
  fFree.Push(StackItem);
 end;
end;

destructor TPasMPBoundedTypedStack<T>.Destroy;
var i:longint;
    p:PByte;
    StackItem:PPasMPBoundedTypedStackItem;
begin
 p:=fData;
 for i:=0 to fMaximalCount-1 do begin
  StackItem:=pointer(p);
  Finalize(StackItem^);
  inc(p,fInternalItemSize);
 end;
 TPasMPMemory.FreeAlignedMemory(fData);
 fFree.Free;
 fStack.Free;
 inherited Destroy;
end;

function TPasMPBoundedTypedStack<T>.IsEmpty:boolean;
begin
 result:=fStack.IsEmpty;
end;

function TPasMPBoundedTypedStack<T>.IsFull:boolean;
begin
 result:=fFree.IsEmpty;
end;

function TPasMPBoundedTypedStack<T>.Push(const Item:T):boolean;
var StackItem:PPasMPBoundedTypedStackItem;
begin
 StackItem:=fFree.Pop;
 if assigned(StackItem) then begin
  StackItem^.Data:=Item;
  fStack.Push(StackItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TPasMPBoundedTypedStack<T>.Pop(out Item:T):boolean;
var StackItem:PPasMPBoundedTypedStackItem;
begin
 StackItem:=fStack.Pop;
 if assigned(StackItem) then begin
  Item:=StackItem^.Data;
  Finalize(StackItem^);
  fFree.Push(StackItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;
{$endif}

constructor TPasMPUnboundedStack.Create(const ItemSize:longint);
begin
 inherited Create;
 fStack:=TPasMPInterlockedStack.Create;
 fItemSize:=ItemSize;
end;

destructor TPasMPUnboundedStack.Destroy;
var StackItem:PPasMPUnboundedStackItem;
begin
 repeat
  StackItem:=fStack.Pop;
  if assigned(StackItem) then begin
   TPasMPMemory.FreeAlignedMemory(StackItem);
  end else begin
   break;
  end;
 until false;
 fStack.Free;
 inherited Destroy;
end;

function TPasMPUnboundedStack.IsEmpty:boolean;
begin
 result:=fStack.IsEmpty;
end;

function TPasMPUnboundedStack.Push(const Item):boolean;
var StackItem:PPasMPUnboundedStackItem;
begin
 TPasMPMemory.AllocateAlignedMemory(StackItem,SizeOf(TPasMPUnboundedStackItem)+fItemSize,PasMPCPUCacheLineSize);
 Move(Item,StackItem^.Data,fItemSize);
 fStack.Push(StackItem);
 result:=true;
end;

function TPasMPUnboundedStack.Pop(out Item):boolean;
var StackItem:PPasMPUnboundedStackItem;
begin
 StackItem:=fStack.Pop;
 if assigned(StackItem) then begin
  Move(StackItem^.Data,Item,fItemSize);
  TPasMPMemory.FreeAlignedMemory(StackItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;

{$ifdef HAS_GENERICS}
constructor TPasMPUnboundedTypedStack<T>.Create;
begin
 inherited Create;
 fStack:=TPasMPInterlockedStack.Create;
 fItemSize:=SizeOf(T);
end;

destructor TPasMPUnboundedTypedStack<T>.Destroy;
var StackItem:PPasMPUnboundedTypedStackItem;
begin
 repeat
  StackItem:=fStack.Pop;
  if assigned(StackItem) then begin
   Finalize(StackItem^);
   TPasMPMemory.FreeAlignedMemory(StackItem);
  end else begin
   break;
  end;
 until false;
 fStack.Free;
 inherited Destroy;
end;

function TPasMPUnboundedTypedStack<T>.IsEmpty:boolean;
begin
 result:=fStack.IsEmpty;
end;

function TPasMPUnboundedTypedStack<T>.Push(const Item:T):boolean;
var StackItem:PPasMPUnboundedTypedStackItem;
begin
 TPasMPMemory.AllocateAlignedMemory(StackItem,SizeOf(TPasMPUnboundedTypedStackItem)+fItemSize,PasMPCPUCacheLineSize);
 Initialize(StackItem^);
 StackItem^.Data:=Item;
 fStack.Push(StackItem);
 result:=true;
end;

function TPasMPUnboundedTypedStack<T>.Pop(out Item:T):boolean;
var StackItem:PPasMPUnboundedTypedStackItem;
begin
 StackItem:=fStack.Pop;
 if assigned(StackItem) then begin
  Item:=StackItem^.Data;
  Finalize(StackItem^);
  TPasMPMemory.FreeAlignedMemory(StackItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;
{$endif}

constructor TPasMPBoundedQueue.Create(const MaximalCount,ItemSize:longint);
var i:longint;
    p:PByte;
    QueueItem:PPasMPBoundedQueueItem;
begin
 inherited Create;
 fQueue:=TPasMPInterlockedQueue.Create(SizeOf(PPasMPBoundedQueueItem));
 fFree:=TPasMPInterlockedStack.Create;
 fMaximalCount:=MaximalCount;
 fItemSize:=ItemSize;
 fInternalItemSize:=TPasMP.RoundUpToPowerOfTwo(Max(SizeOf(TPasMPBoundedQueueItem)+fItemSize,PasMPCPUCacheLineSize));
 TPasMPMemory.AllocateAlignedMemory(fData,fInternalItemSize*fMaximalCount,PasMPCPUCacheLineSize);
 p:=fData;
 for i:=0 to fMaximalCount-1 do begin
  QueueItem:=pointer(p);
  inc(p,fInternalItemSize);
  fFree.Push(QueueItem);
 end;
end;

destructor TPasMPBoundedQueue.Destroy;
begin
 TPasMPMemory.FreeAlignedMemory(fData);
 fFree.Free;
 fQueue.Free;
 inherited Destroy;
end;

function TPasMPBoundedQueue.IsEmpty:boolean;
begin
 result:=fQueue.IsEmpty;
end;

function TPasMPBoundedQueue.IsFull:boolean;
begin
 result:=fFree.IsEmpty;
end;

function TPasMPBoundedQueue.Enqueue(const Item):boolean;
var QueueItem:PPasMPBoundedQueueItem;
begin
 QueueItem:=fFree.Pop;
 if assigned(QueueItem) then begin
  Move(Item,QueueItem^.Data,fItemSize);
  fQueue.Enqueue(QueueItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TPasMPBoundedQueue.Dequeue(out Item):boolean;
var StackItem:PPasMPBoundedQueueItem;
begin
 result:=fQueue.Dequeue(StackItem);
 if result then begin
  Move(StackItem^.Data,Item,fItemSize);
  fFree.Push(StackItem);
 end;
end;

{$ifdef HAS_GENERICS}
constructor TPasMPBoundedTypedQueue<T>.Create(const MaximalCount:longint);
var i:longint;
    p:PByte;
    QueueItem:PPasMPBoundedTypedQueueItem;
begin
 inherited Create;
 fQueue:=TPasMPInterlockedQueue.Create(SizeOf(PPasMPBoundedQueueItem));
 fFree:=TPasMPInterlockedStack.Create;
 fMaximalCount:=MaximalCount;
 fInternalItemSize:=Max(TPasMP.RoundUpToPowerOfTwo(SizeOf(TPasMPBoundedTypedQueueItem)),PasMPCPUCacheLineSize);
 TPasMPMemory.AllocateAlignedMemory(fData,fInternalItemSize*fMaximalCount,PasMPCPUCacheLineSize);
 p:=fData;
 for i:=0 to fMaximalCount-1 do begin
  QueueItem:=pointer(p);
  Initialize(QueueItem^);
  inc(p,fInternalItemSize);
  fFree.Push(QueueItem);
 end;
end;

destructor TPasMPBoundedTypedQueue<T>.Destroy;
var i:longint;
    p:PByte;
    QueueItem:PPasMPBoundedTypedQueueItem;
begin
 p:=fData;
 for i:=0 to fMaximalCount-1 do begin
  QueueItem:=pointer(p);
  Finalize(QueueItem^);
  inc(p,fInternalItemSize);
 end;
 TPasMPMemory.FreeAlignedMemory(fData);
 fFree.Free;
 fQueue.Free;
 inherited Destroy;
end;

function TPasMPBoundedTypedQueue<T>.IsEmpty:boolean;
begin
 result:=fQueue.IsEmpty;
end;

function TPasMPBoundedTypedQueue<T>.IsFull:boolean;
begin
 result:=fFree.IsEmpty;
end;

function TPasMPBoundedTypedQueue<T>.Enqueue(const Item:T):boolean;
var QueueItem:PPasMPBoundedTypedQueueItem;
begin
 QueueItem:=fFree.Pop;
 if assigned(QueueItem) then begin
  Initialize(QueueItem^);
  QueueItem^.Data:=Item;
  fQueue.Enqueue(QueueItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TPasMPBoundedTypedQueue<T>.Dequeue(out Item:T):boolean;
var QueueItem:PPasMPBoundedTypedQueueItem;
begin
 result:=fQueue.Dequeue(QueueItem);
 if result then begin
  Item:=QueueItem^.Data;
  Finalize(QueueItem^);
  fFree.Push(QueueItem);
  result:=true;
 end else begin
  result:=false;
 end;
end;
{$endif}

constructor TPasMPUnboundedQueue.Create(const ItemSize:longint);
begin
 inherited Create;
 fQueue:=TPasMPInterlockedQueue.Create(ItemSize);
 fItemSize:=ItemSize;
end;

destructor TPasMPUnboundedQueue.Destroy;
begin
 fQueue.Free;
 inherited Destroy;
end;

function TPasMPUnboundedQueue.IsEmpty:boolean;
begin
 result:=fQueue.IsEmpty;
end;

procedure TPasMPUnboundedQueue.Enqueue(const Item);
begin
 fQueue.Enqueue(Item);
end;

function TPasMPUnboundedQueue.Dequeue(out Item):boolean;
begin
 result:=fQueue.Dequeue(Item);
end;

{$ifdef HAS_GENERICS}
constructor TPasMPUnboundedTypedQueue<T>.Create;
begin
 inherited Create;
 fQueue:=TPasMPInterlockedQueue.Create(SizeOf(PPasMPUnboundedTypedQueueItem));
end;

destructor TPasMPUnboundedTypedQueue<T>.Destroy;
var QueueItem:PPasMPUnboundedTypedQueueItem;
begin
 while fQueue.Dequeue(QueueItem) do begin
  if assigned(QueueItem) then begin
   Finalize(QueueItem^);
   TPasMPMemory.FreeAlignedMemory(QueueItem);
  end;
 end;
 fQueue.Free;
 inherited Destroy;
end;

function TPasMPUnboundedTypedQueue<T>.IsEmpty:boolean;
begin
 result:=fQueue.IsEmpty;
end;

procedure TPasMPUnboundedTypedQueue<T>.Enqueue(const Item:T);
var QueueItem:PPasMPUnboundedTypedQueueItem;
begin
 TPasMPMemory.AllocateAlignedMemory(QueueItem,SizeOf(TPasMPUnboundedTypedQueueItem),PasMPCPUCacheLineSize);
 Initialize(QueueItem^);
 QueueItem^.Data:=Item;
 fQueue.Enqueue(QueueItem);
end;

function TPasMPUnboundedTypedQueue<T>.Dequeue(out Item:T):boolean;
var QueueItem:PPasMPUnboundedTypedQueueItem;
begin
 result:=fQueue.Dequeue(QueueItem);
 if result then begin
  result:=assigned(QueueItem);
  if result then begin
   Item:=QueueItem^.Data;
   Finalize(QueueItem^);
   TPasMPMemory.FreeAlignedMemory(QueueItem);
  end;
 end;
end;
{$endif}

constructor TPasMPHashTable.Create(const KeySize,ValueSize:longint);
begin
 fKeySize:=KeySize;
 fValueSize:=ValueSize;
 fItemSize:=fKeySize+fValueSize;
 inherited Create(fItemSize);
end;

destructor TPasMPHashTable.Destroy;
begin
 inherited Destroy;
end;

procedure TPasMPHashTable.InitializeItem(const Data:pointer);
begin
 FillChar(Data^,fItemSize,#0);
end;

procedure TPasMPHashTable.FinalizeItem(const Data:pointer);
begin
end;

procedure TPasMPHashTable.CopyItem(const Source,Destination:pointer);
begin
 Move(Source^,Destination^,fItemSize);
end;

procedure TPasMPHashTable.GetKey(const Data,Key:pointer);
begin
 Move(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)))^,Key^,fKeySize);
end;

procedure TPasMPHashTable.SetKey(const Data,Key:pointer);
begin
 Move(Key^,pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)))^,fKeySize);
end;

procedure TPasMPHashTable.GetValue(const Data,Value:pointer);
begin
 Move(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)+TPasMPPtrUInt(fKeySize)))^,Value^,fValueSize);
end;

procedure TPasMPHashTable.SetValue(const Data,Value:pointer);
begin
 Move(Value^,pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)+TPasMPPtrUInt(fKeySize)))^,fValueSize);
end;

function TPasMPHashTable.HashKey(const Key:pointer):TPasMPInterlockedHashTableHash;
{$ifdef CPUARM}
var b:pbyte;
    len,h,i:longword;
begin
 result:=2166136261;
 len:=fKeySize;
 h:=len;
 if len>0 then begin
  b:=Key;
  while len>3 do begin
   i:=longword(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(len,4);
  end;
  if len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(len,2);
  end;
  if len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$else}
const m=longword($57559429);
      n=longword($5052acdb);
var b:pbyte;
    h,k,len:longword;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 len:=fKeySize;
 h:=len;
 k:=h+n+1;
 if len>0 then begin
  b:=Key;
  while len>7 do begin
   begin
    p:=longword(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(len,8);
  end;
  if len>3 then begin
   p:=longword(pointer(b)^)*{$ifdef fpc}qword{$else}int64{$endif}(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(len,4);
  end;
  if len>0 then begin
   if len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(len,2);
   end else begin
    p:=0;
   end;
   if len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*{$ifdef fpc}qword{$else}int64{$endif}(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*{$ifdef fpc}qword{$else}int64{$endif}(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$endif}

function TPasMPHashTable.CompareKey(const Data,Key:pointer):boolean;
{$ifdef OldDelphi}
type PLongwords=^TLongwords;
     TLongwords=array[0..$ffff] of longword;
     PBytes=^TBytes;
     TBytes=array[0..$ffff] of byte;
var Index:longint;
begin
 for Index:=0 to (fKeySize div SizeOf(longword))-1 do begin
  if PLongwords(pointer(Data))^[Index]<>PLongwords(pointer(Key))^[Index] then begin
   result:=false;
   exit;
  end;
 end;
 for Index:=(fKeySize and not (SizeOf(longword)-1)) to fKeySize-1 do begin
  if PBytes(pointer(Data))^[Index]<>PBytes(pointer(Key))^[Index] then begin
   result:=false;
   exit;
  end;
 end;
 result:=true;
end;
{$else}
begin
 result:=CompareMem(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data))),Key,fKeySize);
end;
{$endif}

function TPasMPHashTable.GetKeyValue(const Key;out Value):boolean;
begin
 result:=inherited GetKeyValue(@Key,@Value);
end;

function TPasMPHashTable.SetKeyValue(const Key,Value):boolean;
begin
 result:=inherited SetKeyValue(@Key,@Value);
end;

function TPasMPHashTable.DeleteKey(const Key):boolean;
begin
 result:=inherited DeleteKey(@Key);
end;

constructor TPasMPStringHashTable.Create(const ValueSize:longint);
begin
 fKeySize:=SizeOf(string);
 fValueSize:=ValueSize;
 fItemSize:=fKeySize+fValueSize;
 inherited Create(fItemSize);
end;

destructor TPasMPStringHashTable.Destroy;
begin
 inherited Destroy;
end;

procedure TPasMPStringHashTable.InitializeItem(const Data:pointer);
begin
 FillChar(Data^,fItemSize,#0);
end;

procedure TPasMPStringHashTable.FinalizeItem(const Data:pointer);
begin
 string(Data^):='';
end;

procedure TPasMPStringHashTable.CopyItem(const Source,Destination:pointer);
begin
 string(Destination^):=string(Source^);
 Move(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Source)+TPasMPPtrUInt(fKeySize)))^,pointer(TPasMPPtrUInt(TPasMPPtrUInt(Destination)+TPasMPPtrUInt(fKeySize)))^,fValueSize);
end;

procedure TPasMPStringHashTable.GetKey(const Data,Key:pointer);
begin
 string(Key^):=string(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)))^);
end;

procedure TPasMPStringHashTable.SetKey(const Data,Key:pointer);
begin
 string(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)))^):=string(Key^);
end;

procedure TPasMPStringHashTable.GetValue(const Data,Value:pointer);
begin
 Move(pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)+TPasMPPtrUInt(fKeySize)))^,Value^,fValueSize);
end;

procedure TPasMPStringHashTable.SetValue(const Data,Value:pointer);
begin
 Move(Value^,pointer(TPasMPPtrUInt(TPasMPPtrUInt(Data)+TPasMPPtrUInt(fKeySize)))^,fValueSize);
end;

function TPasMPStringHashTable.HashKey(const Key:pointer):TPasMPInterlockedHashTableHash;
var Index:longint;
begin
 result:=length(string(Key^));
 for Index:=1 to length(string(Key^)) do begin
  result:=((result shl 27) or (result shl 5))+ord(string(Key^)[Index]);
 end;
end;

function TPasMPStringHashTable.CompareKey(const Data,Key:pointer):boolean;
begin
 result:=string(Data^)=string(Key^);
end;

function TPasMPStringHashTable.GetKeyValue(const Key:string;out Value):boolean;
begin
 result:=inherited GetKeyValue(@Key,@Value);
end;

function TPasMPStringHashTable.SetKeyValue(const Key:string;const Value):boolean;
begin
 result:=inherited SetKeyValue(@Key,@Value);
end;

function TPasMPStringHashTable.DeleteKey(const Key:string):boolean;
begin
 result:=inherited DeleteKey(@Key);
end;

constructor TPasMPJobTask.Create;
begin
 inherited Create;
 fFreeOnRelease:=false;
 fJob:=nil;
 fThreadIndex:=-1;
end;

destructor TPasMPJobTask.Destroy;
begin
 inherited Destroy;
end;

procedure TPasMPJobTask.Run;
begin
end;

function TPasMPJobTask.Split:TPasMPJobTask;
begin
 result:=nil;
end;

function TPasMPJobTask.PartialPop:TPasMPJobTask;
begin
 result:=nil;
end;

function TPasMPJobTask.Spread:boolean;
begin
 result:=false;
end;

constructor TPasMPJobAllocator.Create(const AJobWorkerThread:TPasMPJobWorkerThread);
begin
 inherited Create;
 fJobWorkerThread:=AJobWorkerThread;
 fMemoryPoolBuckets:=nil;
 fCountMemoryPoolBuckets:=1;
 SetLength(fMemoryPoolBuckets,fCountMemoryPoolBuckets);
 TPasMPMemory.AllocateAlignedMemory(fMemoryPoolBuckets[0],SizeOf(TPasMPJobAllocatorMemoryPoolBucket),SizeOf(TPasMPJob));
 fCountAllocatedJobs:=0;
 fFreeJobs:=TPasMPInterlockedStack.Create;
end;

destructor TPasMPJobAllocator.Destroy;
var MemoryPoolBucketIndex:longint;
begin
 for MemoryPoolBucketIndex:=0 to fCountMemoryPoolBuckets-1 do begin
  TPasMPMemory.FreeAlignedMemory(fMemoryPoolBuckets[MemoryPoolBucketIndex]);
 end;
 SetLength(fMemoryPoolBuckets,0);
 fFreeJobs.Free;
 inherited Destroy;
end;

procedure TPasMPJobAllocator.AllocateNewBuckets(const NewCountMemoryPoolBuckets:longint);
var OldCountMemoryPoolBuckets,MemoryPoolBucketIndex:longint;
begin
 OldCountMemoryPoolBuckets:=fCountMemoryPoolBuckets;
 fCountMemoryPoolBuckets:=TPasMP.RoundUpToPowerOfTwo(NewCountMemoryPoolBuckets);
 if OldCountMemoryPoolBuckets<fCountMemoryPoolBuckets then begin
  SetLength(fMemoryPoolBuckets,fCountMemoryPoolBuckets);
  for MemoryPoolBucketIndex:=OldCountMemoryPoolBuckets to fCountMemoryPoolBuckets-1 do begin
   TPasMPMemory.AllocateAlignedMemory(fMemoryPoolBuckets[MemoryPoolBucketIndex],SizeOf(TPasMPJobAllocatorMemoryPoolBucket),SizeOf(TPasMPJob));
  end;
 end else begin
  fCountMemoryPoolBuckets:=OldCountMemoryPoolBuckets;
 end;
end;

function TPasMPJobAllocator.AllocateJob:PPasMPJob; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
var JobIndex,MemoryPoolBucketIndex:longint;
begin
 result:=fFreeJobs.Pop;
 if not assigned(result) then begin
  JobIndex:=fCountAllocatedJobs;
  inc(fCountAllocatedJobs);
  MemoryPoolBucketIndex:=JobIndex shr PasMPAllocatorPoolBucketBits;
  if fCountMemoryPoolBuckets<=MemoryPoolBucketIndex then begin
   AllocateNewBuckets(MemoryPoolBucketIndex+1);
  end;
  result:=@fMemoryPoolBuckets[MemoryPoolBucketIndex]^[JobIndex and PasMPAllocatorPoolBucketMask];
 end;
end;

procedure TPasMPJobAllocator.FreeJobs; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 fCountAllocatedJobs:=0;
 fFreeJobs.Clear;
end;

procedure TPasMPJobAllocator.FreeJob(const Job:PPasMPJob);
begin
 fFreeJobs.Push(Job);
 Job^.InternalData:=0;
end;

constructor TPasMPWorkerSystemThread.Create(const AJobWorkerThread:TPasMPJobWorkerThread);
begin
 fJobWorkerThread:=AJobWorkerThread;
 inherited Create(false);
end;

destructor TPasMPWorkerSystemThread.Destroy;
begin
 inherited Destroy;
end;

procedure TPasMPWorkerSystemThread.Execute;
begin
 ReturnValue:=0;
 fJobWorkerThread.ThreadProc;
 ReturnValue:=1;
end;

constructor TPasMPJobQueue.Create(const APasMPInstance:TPasMP);
begin
 inherited Create;
 fPasMPInstance:=APasMPInstance;
 fQueueLockState:=0;
 fQueueSize:=TPasMP.RoundUpToPowerOfTwo(PasMPJobQueueStartSize);
 fQueueMask:=fQueueSize-1;
 SetLength(fQueueJobs,fQueueSize);
 fQueueBottom:=0;
 fQueueTop:=0;
end;

destructor TPasMPJobQueue.Destroy;
begin
 SetLength(fQueueJobs,0);
 inherited Destroy;
end;

function TPasMPJobQueue.HasJobs:boolean;
begin
 result:=fQueueBottom>fQueueTop;
end;

procedure TPasMPJobQueue.Resize(const QueueBottom,QueueTop:longint);
var QueueLockState,OldMask,Index:longint;
    NewJobs:TPasMPJobQueueJobs;
begin
 NewJobs:=nil;
 begin
  // Acquire single-writer-side of lock
  repeat
{$if defined(CPU386) or defined(CPUx86_64)}
   TPasMPMemoryBarrier.ReadDependency;
{$else}
   TPasMPMemoryBarrier.Read;
{$ifend}
   QueueLockState:=fQueueLockState and longint(longword($fffffffe));
   if TPasMPInterlocked.CompareExchange(fQueueLockState,QueueLockState or 1,QueueLockState)=QueueLockState then begin
    break;
   end else begin
    TPasMP.Yield;
   end;
  until false;
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  while fQueueLockState<>1 do begin
   TPasMP.Yield;
{$if defined(CPU386) or defined(CPUx86_64)}
   TPasMPMemoryBarrier.ReadDependency;
{$else}
   TPasMPMemoryBarrier.Read;
{$ifend}
  end;
 end;
 try
  OldMask:=fQueueMask;
  inc(fQueueSize,fQueueSize);
  fQueueMask:=fQueueSize-1;
  SetLength(NewJobs,fQueueSize);
  for Index:=QueueTop to QueueBottom do begin
   NewJobs[Index and fQueueMask]:=fQueueJobs[Index and OldMask];
  end;
  SetLength(fQueueJobs,0);
  fQueueJobs:=NewJobs;
  NewJobs:=nil;
{$ifdef CPU386}
  asm
   mfence
  end;
{$else}
  TPasMPMemoryBarrier.ReadWrite;
{$endif}
 finally
  // Release single-writer-side of lock
  TPasMPInterlocked.Exchange(fQueueLockState,0);
 end;
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.Write;
{$ifend}
end;

procedure TPasMPJobQueue.PushJob(const AJob:PPasMPJob);
var QueueBottom,QueueTop:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.Read;
{$ifend}
 QueueBottom:=fQueueBottom;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 QueueTop:=fQueueTop;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 if (QueueBottom-QueueTop)>(fQueueSize-1) then begin
  // Full queue => non-lock-free resize
  Resize(QueueBottom,QueueTop);
 end;
 fQueueJobs[QueueBottom and fQueueMask]:=AJob;
{$ifdef CPU386}
 asm
  mfence
 end;
{$else}
{$ifdef CPUx86_64}
 TPasMPMemoryBarrier.ReadWrite;
{$endif}
{$endif}
{$if defined(CPU386) or defined(CPUx86_64)}
 fQueueBottom:=QueueBottom+1;
{$else}
 TPasMPInterlocked.Exchange(fQueueBottom,QueueBottom+1);
{$ifend}
end;

function TPasMPJobQueue.PopJob:PPasMPJob;
var QueueBottom,QueueTop:longint;
begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.Read;
{$ifend}
 QueueBottom:=fQueueBottom-1;
{$if defined(CPU386) or defined(CPUx86_64)}
 TPasMPMemoryBarrier.ReadDependency;
{$else}
 TPasMPMemoryBarrier.Read;
{$ifend}
 TPasMPInterlocked.Exchange(fQueueBottom,QueueBottom);
{$ifdef CPU386}
 asm
  mfence
 end;
{$else}
 TPasMPMemoryBarrier.ReadWrite;
{$endif}
 QueueTop:=fQueueTop;
 if QueueTop<=QueueBottom then begin
{$if defined(CPU386) or defined(CPUx86_64)}
  TPasMPMemoryBarrier.ReadDependency;
{$else}
  TPasMPMemoryBarrier.Read;
{$ifend}
  result:=pointer(fQueueJobs[QueueBottom and fQueueMask]);
  if QueueTop=QueueBottom then begin
   if TPasMPInterlocked.CompareExchange(fQueueTop,QueueTop+1,QueueTop)<>QueueTop then begin
    // Failed race against steal operation
    result:=nil;
   end;
{$if defined(CPU386) or defined(CPUx86_64)}
   fQueueBottom:=QueueTop+1;
{$else}
   TPasMPInterlocked.Exchange(fQueueBottom,QueueTop+1);
{$ifend}
  end else begin
   // There's still more than one item left in the queue
  end;
 end else begin
  // Deque was already empty
{$if defined(CPU386) or defined(CPUx86_64)}
  fQueueBottom:=QueueTop;
{$else}
  TPasMPInterlocked.Exchange(fQueueBottom,QueueTop);
{$ifend}
  result:=nil;
 end;
end;

function TPasMPJobQueue.StealJob:PPasMPJob;
var QueueTop,QueueBottom,QueueLockState:longint;
begin
 result:=nil;

 // Try to acquire multiple-reader-side of lock
{$if not (defined(CPU386) or defined(CPUx86_64))}
 TPasMPMemoryBarrier.Read;
{$ifend}

 QueueLockState:=fQueueLockState and longint(longword($fffffffe));
 if TPasMPInterlocked.CompareExchange(fQueueLockState,QueueLockState+2,QueueLockState)=QueueLockState then begin

  begin
{$if not (defined(CPU386) or defined(CPUx86_64))}
   TPasMPMemoryBarrier.Read;
{$ifend}
   QueueTop:=fQueueTop;
{$if defined(CPU386) or defined(CPUx86_64)}
   TPasMPMemoryBarrier.ReadDependency;
{$else}
   TPasMPMemoryBarrier.Read;
{$ifend}
   QueueBottom:=fQueueBottom;
   if QueueTop<QueueBottom then begin
    // Non-empty queue.
{$if defined(CPU386) or defined(CPUx86_64)}
    TPasMPMemoryBarrier.ReadDependency;
{$else}
    TPasMPMemoryBarrier.Read;
{$ifend}
    result:=fQueueJobs[QueueTop and fQueueMask];
    if TPasMPInterlocked.CompareExchange(fQueueTop,QueueTop+1,QueueTop)<>QueueTop then begin
     // Failed race against steal operation
     result:=nil;
    end;
   end;
  end;

  begin
   // Release multiple-reader-side of lock
   TPasMPInterlocked.Add(fQueueLockState,-2);
  end;

 end;

end;

constructor TPasMPJobWorkerThread.Create(const APasMPInstance:TPasMP;const AThreadIndex:longint);
{$ifdef CPU64}
const XorShift64Mul:TPasMPUInt64=TPasMPUInt64(10116239910488455739);
{$endif}
begin
 inherited Create;
 fPasMPInstance:=APasMPInstance;
 fJobAllocator:=TPasMPJobAllocator.Create(self);
 fJobQueue:=TPasMPJobQueue.Create(fPasMPInstance);
 fIsReadyEvent:=TPasMPEvent.Create(nil,false,false,'');
 fThreadIndex:=AThreadIndex;
{$ifdef UseXorShift128}
 fXorShift128x:=(longword(AThreadIndex+1)*83492791) or 1;
 fXorShift128y:=(longword(AThreadIndex+1)*19349669) or 1;
 fXorShift128z:=(longword(AThreadIndex+1)*50331653) or 1;
 fXorShift128w:=(longword(AThreadIndex+1)*73856093) or 1;
{$else}
{$ifdef CPU64}
 fXorShift64:=(longword(AThreadIndex+1)*XorShift64Mul) or 1;
//fXorShift64:=(longword(AThreadIndex+1)*TPasMPUInt64(10116239910488455739)) or 1;
{$else}
 fXorShift32:=(longword(AThreadIndex+1)*83492791) or 1;
{$endif}
{$endif}
 if fThreadIndex>0 then begin
  fSystemThread:=TPasMPWorkerSystemThread.Create(self);
 end else begin
  fSystemThread:=nil;
  ThreadInitialization;
 end;
end;

destructor TPasMPJobWorkerThread.Destroy;
begin
 if assigned(fSystemThread) then begin
  fSystemThread.Terminate;
  fPasMPInstance.WakeUpAll;
  fSystemThread.WaitFor;
  fSystemThread.Free;
 end;
 fIsReadyEvent.Free;
 fJobQueue.Free;
 fJobAllocator.Free;
 inherited Destroy;
end;

procedure TPasMPJobWorkerThread.ThreadInitialization;
var ThreadIDHash:longword;
    HashJobWorkerThread:TPasMPJobWorkerThread;
{$ifdef Windows}
    CurrentThreadHandle:THANDLE;
{$else}
{$ifdef Linux}
    CPUSet:int64;
{$endif}
{$endif}
begin

 SetExceptionMask(fPasMPInstance.fFPUExceptionMask);
 SetPrecisionMode(fPasMPInstance.fFPUPrecisionMode);
 SetRoundMode(fPasMPInstance.fFPURoundingMode);

 if (length(fPasMPInstance.fAvailableCPUCores)>1) and
    (fThreadIndex<length(fPasMPInstance.fAvailableCPUCores)) then begin
{$ifdef Windows}
  CurrentThreadHandle:=GetCurrentThread;
  if fPasMPInstance.fDoCPUCorePinning then begin
 //SetThreadIdealProcessor(CurrentThreadHandle,fPasMPInstance.fAvailableCPUCores[fThreadIndex]);
   SetThreadAffinityMask(CurrentThreadHandle,longword(1) shl fPasMPInstance.fAvailableCPUCores[fThreadIndex]);
  end;
{$else}
{$ifdef Linux}
  if fPasMPInstance.fDoCPUCorePinning then begin
   CPUSet:=int64(1) shl fPasMPInstance.fAvailableCPUCores[fThreadIndex];
   sched_setaffinity(GetThreadID,SizeOf(CPUSet),@CPUSet);
  end;
{$endif}
{$endif}
 end;

{$ifdef UseThreadLocalStorage}
{$ifdef UseThreadLocalStorageX8632}
 TLSSetValue(CurrentJobWorkerThreadTLSIndex,self);
{$else}
{$ifdef UseThreadLocalStorageX8664}
 TLSSetValue(CurrentJobWorkerThreadTLSIndex,self);
{$else}
 CurrentJobWorkerThread:=self;
{$endif}
{$endif}
{$else}
 fThreadID:=GetCurrentThreadID;
 ThreadIDHash:=TPasMP.GetThreadIDHash(fThreadID);

 fPasMPInstance.fJobWorkerThreadHashTableCriticalSection.Acquire;
 try
  HashJobWorkerThread:=fPasMPInstance.fJobWorkerThreadHashTable[ThreadIDHash and PasMPJobWorkerThreadHashTableMask];
  if assigned(HashJobWorkerThread) then begin
   HashJobWorkerThread.fNext:=self;
  end;
  fNext:=nil;
  fPasMPInstance.fJobWorkerThreadHashTable[ThreadIDHash and PasMPJobWorkerThreadHashTableMask]:=self;
 finally
  fPasMPInstance.fJobWorkerThreadHashTableCriticalSection.Release;
 end;
{$endif}

 fIsReadyEvent.SetEvent;

end;

function TPasMPJobWorkerThread.GetJob:PPasMPJob;
const XorShiftBitShift={$ifdef UseXorShift128}16{$else}{$ifdef CPU64}48{$else}16{$endif}{$endif};
var XorShiftTemp:{$ifdef UseXorShift128}longword{$else}{$ifdef CPU64}TPasMPUInt64{$else}longword{$endif}{$endif};
    OtherJobWorkerThreadIndex:longword;
    OtherJobWorkerThread:TPasMPJobWorkerThread;
begin

 result:=fJobQueue.PopJob;
 if (not assigned(result)) or (result^.State<0) then begin

  // This is not a valid job because our own queue is empty, so try stealing from some other queue

{$ifdef UseXorShift128}
  XorShiftTemp:=fXorShift128x xor (fXorShift128x shl 11);
  fXorShift128x:=fXorShift128y;
  fXorShift128y:=fXorShift128z;
  fXorShift128z:=fXorShift128w;
  fXorShift128w:=((fXorShift128w xor (fXorShift128w shr 19)) xor XorShiftTemp) xor (XorShiftTemp shr 8);
  XorShiftTemp:=XorShiftTemp;
{$else}
{$ifdef CPU64}
  XorShiftTemp:=fXorShift64;
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shl 21);
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shr 35);
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shl 4);
  fXorShift64:=XorShiftTemp;
{ XorShiftTemp:=fXorShift64;
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shl 13);
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shr 7);
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shl 17);
  fXorShift64:=XorShiftTemp;{}
{$else}
  XorShiftTemp:=fXorShift32;
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shl 13);
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shr 17);
  XorShiftTemp:=XorShiftTemp xor (XorShiftTemp shl 5);
  fXorShift32:=XorShiftTemp;
{$endif}
{$endif}

  OtherJobWorkerThreadIndex:=((XorShiftTemp shr XorShiftBitShift)*longword(fPasMPInstance.fCountJobWorkerThreads)) shr 16;
  OtherJobWorkerThread:=fPasMPInstance.fJobWorkerThreads[OtherJobWorkerThreadIndex];
  if OtherJobWorkerThread=self then begin
   // Don't try to steal from ourselves
   result:=nil;
  end else begin
   result:=OtherJobWorkerThread.fJobQueue.StealJob;
   if (not assigned(result)) or (result^.State<0) then begin
    // We couldn't steal a job from the other queue either
    result:=nil;
   end;
  end;

  if not assigned(result) then begin

   result:=fPasMPInstance.fJobQueue.StealJob;
   if (not assigned(result)) or (result^.State<0) then begin
    // We couldn't steal a job from the global queue
    result:=nil;
   end;

   if not assigned(result) then begin
    // If we couldn't steal a job from any queue either, so we just yield our time slice for now
    TPasMP.Yield;
   end;

  end;

 end;

end;

procedure TPasMPJobWorkerThread.ThreadProc;
var SpinCount,CountMaxSpinCount:longint;
    Job:PPasMPJob;
begin
 ThreadInitialization;
 fPasMPInstance.fSystemIsReadyEvent.WaitFor(INFINITE);
 fPasMPInstance.WaitForWakeUp;
 SpinCount:=0;
 CountMaxSpinCount:=128;
 while not fSystemThread.Terminated do begin
  Job:=GetJob;
  if assigned(Job) then begin
   TPasMPInterlocked.Increment(fPasMPInstance.fWorkingJobWorkerThreads);
   fPasMPInstance.ExecuteJob(Job,self);
   TPasMPInterlocked.Decrement(fPasMPInstance.fWorkingJobWorkerThreads);
   SpinCount:=0;
  end else begin
   if SpinCount<CountMaxSpinCount then begin
    inc(SpinCount);
   end else begin
    fPasMPInstance.WaitForWakeUp;
    SpinCount:=0;
   end;
  end;
 end;
end;

constructor TPasMPScope.Create(const APasMPInstance:TPasMP);
begin
 inherited Create;
 fPasMPInstance:=APasMPInstance;
 fWaitCalled:=false;
 fJobs:=nil;
 fCountJobs:=0;
end;

destructor TPasMPScope.Destroy;
begin
 if not fWaitCalled then begin
  Wait;
 end;
 fPasMPInstance.Release(fJobs);
 SetLength(fJobs,0);
 inherited Destroy;
end;

procedure TPasMPScope.Run(const Job:PPasMPJob);
begin
 fPasMPInstance.Run(Job);
 if length(fJobs)<=(fCountJobs+1) then begin
  SetLength(fJobs,(fCountJobs+1)*2);
 end;
 fJobs[fCountJobs]:=Job;
 inc(fCountJobs);
end;

procedure TPasMPScope.Run(const Jobs:array of PPasMPJob);
var Count:longint;
begin
 fPasMPInstance.Run(Jobs);
 Count:=length(Jobs);
 if Count>0 then begin
  if length(fJobs)<=(fCountJobs+Count) then begin
   SetLength(fJobs,(fCountJobs+Count)*2);
  end;
  Move(Jobs[0],fJobs[fCountJobs],Count*SizeOf(PPasMPJob));
  inc(fCountJobs,Count);
 end;
end;

procedure TPasMPScope.Run(const JobTask:TPasMPJobTask);
begin
 Run(fPasMPInstance.Acquire(JobTask));
end;

procedure TPasMPScope.Run(const JobTasks:array of TPasMPJobTask);
var Index:longint;
begin
 for Index:=0 to length(JobTasks)-1 do begin
  Run(fPasMPInstance.Acquire(JobTasks[Index]));
 end;
end;

procedure TPasMPScope.Wait;
begin
 fWaitCalled:=true;
 if fCountJobs>0 then begin
  SetLength(fJobs,fCountJobs);
  fPasMPInstance.Wait(fJobs);
 end;
end;

constructor TPasMP.Create(const MaxThreads:longint=-1;const ThreadHeadRoomForForeignTasks:longint=0;const DoCPUCorePinning:boolean=true;const SleepingOnIdle:boolean=true);
var Index:longint;
begin

 inherited Create;

 fFPUExceptionMask:=GetExceptionMask;
 fFPUPrecisionMode:=GetPrecisionMode;
 fFPURoundingMode:=GetRoundMode;

 fAvailableCPUCores:=nil;

 fDoCPUCorePinning:=DoCPUCorePinning;
 
 fSleepingOnIdle:=SleepingOnIdle;

 fCountJobWorkerThreads:=TPasMP.GetCountOfHardwareThreads(fAvailableCPUCores)-ThreadHeadRoomForForeignTasks;
 if fCountJobWorkerThreads<1 then begin
  fCountJobWorkerThreads:=1;
 end;
 if (MaxThreads>0) and (fCountJobWorkerThreads>MaxThreads) then begin
  fCountJobWorkerThreads:=MaxThreads;
 end;
 if fCountJobWorkerThreads>=longint(PasMPJobThreadIndexSize) then begin
  fCountJobWorkerThreads:=longint(PasMPJobThreadIndexSize-1);
 end;

 fSleepingJobWorkerThreads:=0;

 fSystemIsReadyEvent:=TPasMPEvent.Create(nil,true,false,'');

{$ifdef PasMPUseWakeUpConditionVariable}
 fWakeUpCounter:=0;
 fWakeUpConditionVariableLock:=TPasMPConditionVariableLock.Create;
 fWakeUpConditionVariable:=TPasMPConditionVariable.Create;
{$else}
 fWakeUpEvent:=TPasMPEvent.Create(nil,true,false,'');
{$endif}

 fJobWorkerThreads:=nil;
 SetLength(fJobWorkerThreads,fCountJobWorkerThreads);

 fCriticalSection:=TPasMPCriticalSection.Create;

 fJobAllocatorCriticalSection:=TPasMPCriticalSection.Create;

 fJobAllocator:=TPasMPJobAllocator.Create(nil);

 fJobQueue:=TPasMPJobQueue.Create(self);

{$ifndef UseThreadLocalStorage}
 fJobWorkerThreadHashTableCriticalSection:=TPasMPCriticalSection.Create;

 FillChar(fJobWorkerThreadHashTable,SizeOf(TPasMPJobWorkerThreadHashTable),AnsiChar(#0));
{$endif}

 for Index:=0 to fCountJobWorkerThreads-1 do begin
  fJobWorkerThreads[Index]:=TPasMPJobWorkerThread.Create(self,Index);
 end;
 for Index:=0 to fCountJobWorkerThreads-1 do begin
  fJobWorkerThreads[Index].fIsReadyEvent.WaitFor(INFINITE);
  FreeAndNil(fJobWorkerThreads[Index].fIsReadyEvent);
 end;
 fSystemIsReadyEvent.SetEvent;

end;

destructor TPasMP.Destroy;
var Index:longint;
    JobWorkerThread:TPasMPJobWorkerThread;
begin
 for Index:=0 to fCountJobWorkerThreads-1 do begin
  JobWorkerThread:=fJobWorkerThreads[Index];
  if assigned(JobWorkerThread.fSystemThread) then begin
   JobWorkerThread.fSystemThread.Terminate;
  end;
 end;
 WakeUpAll;
 for Index:=0 to fCountJobWorkerThreads-1 do begin
  JobWorkerThread:=fJobWorkerThreads[Index];
  if assigned(JobWorkerThread.fSystemThread) then begin
   while JobWorkerThread.fSystemThread.ReturnValue=0 do begin
    WakeUpAll;
    TPasMP.Yield;
   end;
   JobWorkerThread.fSystemThread.WaitFor;
  end;
 end;
 for Index:=0 to fCountJobWorkerThreads-1 do begin
  JobWorkerThread:=fJobWorkerThreads[Index];
  if assigned(JobWorkerThread.fSystemThread) then begin
   FreeAndNil(JobWorkerThread.fSystemThread);
  end;
  JobWorkerThread.Free;
 end;
 SetLength(fJobWorkerThreads,0);
 SetLength(fAvailableCPUCores,0);
 fJobQueue.Free;
 fJobAllocator.Free;
 fJobAllocatorCriticalSection.Free;
 fSystemIsReadyEvent.Free;
{$ifdef PasMPUseWakeUpConditionVariable}
 fWakeUpConditionVariable.Free;
 fWakeUpConditionVariableLock.Free;
{$else}
 fWakeUpEvent.Free;
{$endif}
{$ifndef UseThreadLocalStorage}
 fJobWorkerThreadHashTableCriticalSection.Free;
{$endif}
 fCriticalSection.Free;
 inherited Destroy;
end;

class function TPasMP.CreateGlobalInstance:TPasMP;
begin
 TPasMPMemoryBarrier.Sync;
 if not assigned(GlobalPasMP) then begin
  GlobalPasMPCriticalSection.Acquire;
  try
   if not assigned(GlobalPasMP) then begin
    GlobalPasMP:=TPasMP.Create(GlobalPasMPMaximalThreads,
                               GlobalPasMPThreadHeadRoomForForeignTasks,
                               GlobalPasMPDoCPUCorePinning,
                               GlobalPasMPSleepingOnIdle);
    TPasMPMemoryBarrier.Sync;
   end;
  finally
   GlobalPasMPCriticalSection.Release;
  end;
 end;
 result:=GlobalPasMP;
end;

class procedure TPasMP.DestroyGlobalInstance;
begin
 GlobalPasMPCriticalSection.Acquire;
 try
  FreeAndNil(GlobalPasMP);
 finally
  GlobalPasMPCriticalSection.Release;
 end;
end;

class function TPasMP.GetGlobalInstance:TPasMP; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 if not assigned(GlobalPasMP) then begin
  CreateGlobalInstance;
 end;
 result:=GlobalPasMP;
end;

class function TPasMP.GetCountOfHardwareThreads(var AvailableCPUCores:TPasMPAvailableCPUCores):longint;
{$ifdef Windows}
var PhysicalCores,LogicalCores,i,j:longint;
    sinfo:SYSTEM_INFO;
    dwProcessAffinityMask,dwSystemAffinityMask:TPasMPPtrUInt;
 procedure GetCPUInfo(var PhysicalCores,LogicalCores:longint);
 const RelationProcessorCore=0;
       RelationNumaNode=1;
       RelationCache=2;
       RelationProcessorPackage=3;
       RelationGroup=4;
       RelationAll=$ffff;
       CacheUnified=0;
       CacheInstruction=1;
       CacheData=2;
       CacheTrace=3;
 type TLogicalProcessorRelationship=dword;
      TProcessorCacheType=dword;
      TCacheDescriptor=packed record
       Level:byte;
       Associativity:byte;
       LineSize:word;
       Size:dword;
       pcType:TProcessorCacheType;
      end;
      PSystemLogicalProcessorInformation=^TSystemLogicalProcessorInformation;
      TSystemLogicalProcessorInformation=packed record
       ProcessorMask:TPasMPPtrUInt;
       case Relationship:TLogicalProcessorRelationship of
        0:(
         Flags:byte;
        );
        1:(
         NodeNumber:dword;
        );
        2:(
         Cache:TCacheDescriptor;
        );
        3:(
         Reserved:array[0..1] of int64;
        );
      end;
      TGetLogicalProcessorInformation=function(Buffer:PSystemLogicalProcessorInformation;out ReturnLength:DWORD):BOOL; stdcall;
  function CountSetBits(Value:TPasMPPtrUInt):longint;
  begin
   result:=0;
   while Value<>0 do begin
    inc(result);
    Value:=Value and (Value-1);
   end;
  end;
 var GetLogicalProcessorInformation:TGetLogicalProcessorInformation;
     Buffer:array of TSystemLogicalProcessorInformation;
     ReturnLength:dword;
     Index,Count:longint;
 begin
  Buffer:=nil;
  PhysicalCores:=0;
  LogicalCores:=0;
  try
   GetLogicalProcessorInformation:=GetProcAddress(GetModuleHandle('kernel32'),'GetLogicalProcessorInformation');
   if assigned(GetLogicalProcessorInformation) then begin
    SetLength(Buffer,16);
    Count:=0;
    repeat
     ReturnLength:=length(Buffer)*SizeOf(TSystemLogicalProcessorInformation);
     if GetLogicalProcessorInformation(@Buffer[0],ReturnLength) then begin
      Count:=ReturnLength div SizeOf(TSystemLogicalProcessorInformation);
     end else begin
      if GetLastError=ERROR_INSUFFICIENT_BUFFER then begin
       SetLength(Buffer,(ReturnLength div SizeOf(TSystemLogicalProcessorInformation))+1);
       continue;
      end;
     end;
     break;
    until false;
    if Count>0 then begin
     PhysicalCores:=0;
     for Index:=0 to Count-1 do begin
      if Buffer[Index].Relationship=RelationProcessorCore then begin
       inc(PhysicalCores);
       inc(LogicalCores,CountSetBits(Buffer[Index].ProcessorMask));
      end;
     end;
    end;
   end;
  finally
   SetLength(Buffer,0);
  end;
 end;
begin
 GetCPUInfo(PhysicalCores,LogicalCores);
 result:=LogicalCores;
 if result=0 then begin
  result:=PhysicalCores;
 end;
 GetSystemInfo(sinfo);
 GetProcessAffinityMask(GetCurrentProcess,dwProcessAffinityMask,dwSystemAffinityMask);
 SetLength(AvailableCPUCores,result);
 j:=0;
 for i:=0 to sinfo.dwNumberOfProcessors-1 do begin
  if (dwProcessAffinityMask and (1 shl i))<>0 then begin
   AvailableCPUCores[j]:=i;
   inc(j);
   if j>=result then begin
    break;
   end;
  end;
 end;
 if result>j then begin
  result:=j;
  SetLength(AvailableCPUCores,result);
 end;
end;
{$else}
{$ifdef Linux}
var i,j:longint;
    CPUSet:int64;
begin
 result:=sysconf(_SC_NPROCESSORS_CONF);
 SetLength(AvailableCPUCores,result);
 sched_getaffinity(GetProcessID,SizeOf(CPUSet),@CPUSet);
 j:=0;
 for i:=0 to 127 do begin
  if (CPUSet and (int64(1) and i))<>0 then begin
   AvailableCPUCores[j]:=i;
   inc(j);
   if j>=result then begin
    break;
   end;
  end;
 end;
 if result>j then begin
  result:=j;
  SetLength(AvailableCPUCores,result);
 end;
end;
{$else}
{$ifdef Solaris}
var i:longint;
begin
 result:=sysconf(_SC_NPROC_ONLN);
 SetLength(AvailableCPUCores,result);
 for i:=0 to result-1 do begin
  AvailableCPUCores[i]:=i;
 end;
end;
{$else}
{$ifdef Unix}
var mib:array[0..1] of cint;
    len:cint;
    t:cint;
    i:longint;
begin
 mib[0]:=CTL_HW;
 mib[1]:=HW_AVAILCPU;
 len:=SizeOf(t);
 fpsysctl(PAnsiChar(@mib),2,@t,@len,nil,0);
 if t<1 then begin
  mib[1]:=HW_NCPU;
  fpsysctl(PAnsiChar(@mib),2,@t,@len,nil,0);
  if t<1 then begin
   t:=1;
  end;
 end;
 result:=t;
 SetLength(AvailableCPUCores,result);
 for i:=0 to result-1 do begin
  AvailableCPUCores[i]:=i;
 end;
end;
{$else}
var i:longint;
begin
 result:=1;
 SetLength(AvailableCPUCores,result);
 for i:=0 to result-1 do begin
  AvailableCPUCores[i]:=i;
 end;
end;
{$endif}
{$endif}
{$endif}
{$endif}

class function TPasMP.Once(var OnceControl:TPasMPOnce;const InitRoutine:TPasMPOnceInitRoutine):boolean; {$ifdef Linux}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}{$endif}
{$ifdef Linux}
begin
 result:=pthread_once(@OnceControl,InitRoutine)=0;
end;
{$else}
var SavedOnceControl:TPasMPOnce;
begin
 result:=false;
 SavedOnceControl:=OnceControl;
{$ifdef CPU386}
 asm
  mfence
 end;
{$else}
 TPasMPMemoryBarrier.ReadWrite;
{$endif}
 while SavedOnceControl<>1 do begin
  if SavedOnceControl=0 then begin
   if TPasMPInterlocked.CompareExchange(OnceControl,2,0)=0 then begin
    try
     InitRoutine;
    finally
     OnceControl:=1;
    end;
    result:=true;
    exit;
   end;
  end;
{$ifdef cpu386}
  asm
   db $f3,$90 // pause (rep nop)
  end;
{$else}
  TPasMP.Yield;
{$endif}
{$ifdef CPU386}
  asm
   mfence
  end;
{$else}
  TPasMPMemoryBarrier.ReadWrite;
{$endif}
  SavedOnceControl:=OnceControl;
 end;
end;
{$endif}

procedure TPasMP.Reset;
var Index:longint;
begin
 fJobAllocator.FreeJobs;
 for Index:=0 to fCountJobWorkerThreads-1 do begin
  fJobWorkerThreads[Index].fJobAllocator.FreeJobs;
 end;
end;

function TPasMP.CreateScope:TPasMPScope; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 result:=TPasMPScope.Create(self);
end;

class function TPasMP.IsJobCompleted(const Job:PPasMPJob):boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 result:=assigned(Job) and (Job^.State<0);
end;

class function TPasMP.IsJobValid(const Job:PPasMPJob):boolean; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 result:=assigned(Job) and (Job^.State>=0);
end;

function TPasMP.GetJobWorkerThread:TPasMPJobWorkerThread; {$ifdef UseThreadLocalStorage}{$if defined(UseThreadLocalStorageX8632) or defined(UseThreadLocalStorageX8664)}assembler;{$ifend}{$else}{$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}{$endif}
{$ifdef UseThreadLocalStorage}
{$ifdef UseThreadLocalStorageX8632}
asm
 mov eax,dword ptr fs:[$00000018]
 mov ecx,dword ptr CurrentJobWorkerThreadTLSOffset
 mov eax,dword ptr [eax+ecx]
end;
{$else}
{$ifdef UseThreadLocalStorageX8664}
asm
 mov rax,qword ptr gs:[$00000058]
 mov ecx,dword ptr CurrentJobWorkerThreadTLSOffset
 mov rax,qword ptr [rax+rcx]
end;
{$else}
begin
 result:=CurrentJobWorkerThread;
end;
{$endif}
{$endif}
{$else}
var ThreadID:{$ifdef fpc}TThreadID{$else}longword{$endif};
    ThreadIDHash:longword;
begin
 ThreadID:=GetCurrentThreadID;
 ThreadIDHash:=TPasMP.GetThreadIDHash(ThreadID);
 result:=fJobWorkerThreadHashTable[ThreadIDHash and PasMPJobWorkerThreadHashTableMask];
 while assigned(result) and (result.fThreadID<>ThreadID) do begin
  result:=result.fNext;
 end;
end;
{$endif}

procedure TPasMP.WaitForWakeUp;
{$ifdef PasMPUseWakeUpConditionVariable}
var SavedWakeUpCounter:longint;
begin
 if fSleepingOnIdle then begin
  fWakeUpConditionVariableLock.Acquire;
  try
   TPasMPInterlocked.Increment(fSleepingJobWorkerThreads);
   SavedWakeUpCounter:=fWakeUpCounter;
   repeat
    fWakeUpConditionVariable.Wait(fWakeUpConditionVariableLock);
   until SavedWakeUpCounter<>fWakeUpCounter;
   TPasMPInterlocked.Decrement(fSleepingJobWorkerThreads);
  finally
   fWakeUpConditionVariableLock.Release;
  end;
 end else begin
  TPasMP.Yield;
 end;
end;
{$else}
begin
 if fSleepingOnIdle then begin
  fWakeUpEvent.ResetEvent;
  TPasMPInterlocked.Increment(fSleepingJobWorkerThreads);
  fWakeUpEvent.WaitFor(INFINITE);
  TPasMPInterlocked.Decrement(fSleepingJobWorkerThreads);
 end else begin
  TPasMP.Yield;
 end;
end;
{$endif}

procedure TPasMP.WakeUpAll;
{$ifdef PasMPUseWakeUpConditionVariable}
begin
 if fSleepingOnIdle and (fSleepingJobWorkerThreads>0) then begin
  fWakeUpConditionVariableLock.Acquire;
  try
   inc(fWakeUpCounter);
   fWakeUpConditionVariable.Broadcast;
  finally
   fWakeUpConditionVariableLock.Release;
  end;
 end;
end;
{$else}
begin
 if fSleepingOnIdle and (fSleepingJobWorkerThreads>0) then begin
  fWakeUpEvent.SetEvent;
 end;
end;
{$endif}

function TPasMP.CanSpread:boolean;
var CurrentJobWorkerThread,JobWorkerThread:TPasMPJobWorkerThread;
    ThreadIndex,Index:longint;
begin
 result:=false;
 CurrentJobWorkerThread:=GetJobWorkerThread;
 if assigned(CurrentJobWorkerThread) then begin
  ThreadIndex:=CurrentJobWorkerThread.fThreadIndex;
  if ((ThreadIndex=0) and (fWorkingJobWorkerThreads=0)) or ((ThreadIndex<>0) and (fWorkingJobWorkerThreads=1)) then begin
   for Index:=0 to fCountJobWorkerThreads-1 do begin
    JobWorkerThread:=fJobWorkerThreads[Index];
    if (JobWorkerThread<>CurrentJobWorkerThread) and JobWorkerThread.fJobQueue.HasJobs then begin
     // We are not alone with queued work.
     exit;
    end;
   end;
   // We are alone with queued work.
   result:=true;
  end;
 end;
end;

function TPasMP.GlobalAllocateJob:PPasMPJob;
begin
 fJobAllocatorCriticalSection.Acquire;
 try
  result:=fJobAllocator.AllocateJob;
 finally
  fJobAllocatorCriticalSection.Release;
 end;
end;

procedure TPasMP.GlobalFreeJob(const Job:PPasMPJob);
begin
 fJobAllocatorCriticalSection.Acquire;
 try
  fJobAllocator.FreeJob(Job);
 finally
  fJobAllocatorCriticalSection.Release;
 end;
end;

function TPasMP.AllocateJob(const MethodCode,MethodData,Data:pointer;const ParentJob:PPasMPJob;const Flags:longword):PPasMPJob; {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
var JobWorkerThread:TPasMPJobWorkerThread;
begin
 if assigned(ParentJob) and (ParentJob^.State>=0) then begin
  TPasMPInterlocked.Increment(ParentJob^.State);
 end;
 JobWorkerThread:=GetJobWorkerThread;
 if assigned(JobWorkerThread) then begin
  result:=JobWorkerThread.fJobAllocator.AllocateJob;
 end else begin
  result:=GlobalAllocateJob;
 end;
 result^.Method.Code:=MethodCode;
 result^.Method.Data:=MethodData;
 result^.ParentJob:=ParentJob;
 if assigned(JobWorkerThread) then begin
  result^.InternalData:=longword(JobWorkerThread.fThreadIndex) or (PasMPJobFlagHasOwnerWorkerThread or Flags);
 end else begin
  result^.InternalData:=Flags;
 end;
 result^.State:=0;
 result^.Data:=Data;
end;

{$ifdef HAS_ANONYMOUS_METHODS}
type PPasMPJobReferenceProcedureJobData=^TPasMPJobReferenceProcedureJobData;
     TPasMPJobReferenceProcedureJobData=record
      JobReferenceProcedure:TPasMPJobReferenceProcedure;
      Data:pointer;
     end;

procedure TPasMP.JobReferenceProcedureJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var JobReferenceProcedureJobData:PPasMPJobReferenceProcedureJobData;
begin
 JobReferenceProcedureJobData:=PPasMPJobReferenceProcedureJobData(pointer(@Job^.Data));
 try
  JobReferenceProcedureJobData^.JobReferenceProcedure(JobReferenceProcedureJobData^.Data,ThreadIndex);
 finally
  Finalize(JobReferenceProcedureJobData^);
 end;
end;

function TPasMP.Acquire(const JobReferenceProcedure:TPasMPJobReferenceProcedure;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob;
var JobMethod:TPasMPJobMethod;
    JobReferenceProcedureJobData:PPasMPJobReferenceProcedureJobData;
begin
 JobMethod:=JobReferenceProcedureJobFunction;
 result:=AllocateJob(TMethod(JobMethod).Code,TMethod(JobMethod).Data,nil,ParentJob,Flags);
 if assigned(result) then begin
  JobReferenceProcedureJobData:=PPasMPJobReferenceProcedureJobData(pointer(@result^.Data));
  Initialize(JobReferenceProcedureJobData^);
  JobReferenceProcedureJobData^.JobReferenceProcedure:=JobReferenceProcedure;
  JobReferenceProcedureJobData^.Data:=Data;
 end;
end;
{$endif}

function TPasMP.Acquire(const JobProcedure:TPasMPJobProcedure;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob;
begin
 result:=AllocateJob(Addr(JobProcedure),nil,Data,ParentJob,Flags);
end;

function TPasMP.Acquire(const JobMethod:TPasMPJobMethod;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob;
begin
 result:=AllocateJob(TMethod(JobMethod).Code,TMethod(JobMethod).Data,Data,ParentJob,Flags);
end;

function TPasMP.Acquire(const JobTask:TPasMPJobTask;const Data:pointer=nil;const ParentJob:PPasMPJob=nil;const Flags:longword=0):PPasMPJob;
begin
 result:=AllocateJob(nil,pointer(JobTask),Data,ParentJob,Flags);
 JobTask.fJob:=result;
 JobTask.fThreadIndex:=-1;
end;

procedure TPasMP.Release(const Job:PPasMPJob); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 if assigned(Job) then begin
  if (assigned(Job^.Method.Data) and not assigned(Job^.Method.Code)) and TPasMPJobTask(pointer(Job^.Method.Data)).fFreeOnRelease then begin
   TPasMPJobTask(pointer(Job^.Method.Data)).Free;
  end;
  if (Job^.InternalData and PasMPJobFlagHasOwnerWorkerThread)<>0 then begin
   fJobWorkerThreads[Job^.InternalData and PasMPJobThreadIndexMask].fJobAllocator.FreeJob(Job);
  end else begin
   GlobalFreeJob(Job);
  end;
 end;
end;

procedure TPasMP.Release(const Jobs:array of PPasMPJob);
var JobIndex:longint;
begin
 for JobIndex:=0 to length(Jobs)-1 do begin
  Release(Jobs[JobIndex]);
 end;
end;

procedure TPasMP.FinishJobRelease(Job:PPasMPJob); {$if defined(cpu386) or defined(cpux86_64)}register;{$ifend}
begin
 Release(Job);
end;

procedure TPasMP.FinishJob(Job:PPasMPJob); {$if defined(cpu386) or defined(cpux86_64)}assembler; register;{$ifend}
{$if defined(cpu386)}
asm
 // Object Pascal i386 register fastcall call convention
 // eax = self
 // edx = Job
 // ecx = Temporary
@Loop:
 cmp dword ptr [edx+TPasMPJob.State],0
 jl @Done
 lock dec dword ptr [edx+TPasMPJob.State]
 jns @Done
 xor ecx,ecx
 lock xchg dword ptr [edx+TPasMPJob.ParentJob],ecx
 test dword ptr [edx+TPasMPJob.InternalData],PasMPJobFlagReleaseOnFinish
 jz @NoFreeOnRelease
 push ecx
 push edx
 call TPasMP.FinishJobRelease
 pop edx
 pop ecx
 @NoFreeOnRelease:
 mov edx,ecx
 test edx,edx
 jnz @Loop
@Done:
end;
{$elseif defined(cpux86_64)}
{$ifdef Windows}
asm
 // Win64 ABI
 // rcx = self
 // rdx = Job
 // r8 = Temporary
 push rbp
@Loop:
 cmp dword ptr [rdx+TPasMPJob.State],0
 jl @Done
 lock dec dword ptr [rdx+TPasMPJob.State]
 jns @Done
 xor r8,r8
 lock xchg qword ptr [rdx+TPasMPJob.ParentJob],r8
 test dword ptr [rdx+TPasMPJob.InternalData],PasMPJobFlagReleaseOnFinish
 jz @NoFreeOnRelease
 push r8
 push rdx
 // 2x 64-bit pushs => Stack stays 16 bytes aligned
 sub rsp,32 // Allocate shadow space
 call TPasMP.FinishJobRelease
 add rsp,32 // Deallocate shadow space
 pop rdx
 pop r8
@NoFreeOnRelease:
 mov rdx,r8
 test rdx,rdx
 jnz @Loop
@Done:
 pop rbp
end;
{$else}
asm
 // System V ABI
 // rdi = self
 // rsi = Job
 // rdx = Temporary
 push rbp
@Loop:
 cmp dword ptr [rsi+TPasMPJob.State],0
 jl @Done
 lock dec dword ptr [rsi+TPasMPJob.State]
 jns @Done
 xor edx,edx
 lock xchg qword ptr [rsi+TPasMPJob.ParentJob],rdx
 test dword ptr [rsi+TPasMPJob.InternalData],PasMPJobFlagReleaseOnFinish
 jz @NoFreeOnRelease
 push rdx
 push rsi
 // 2x 64-bit pushs => Stack stays 16 bytes aligned
 call TPasMP.FinishJobRelease
 pop rsi
 pop rdx
@NoFreeOnRelease:
 mov rsi,rdx
 test rsi,rsi
 jnz @Loop
@Done:
 pop rbp
end;
{$endif}
{$else}
var LastJob:PPasMPJob;
begin
 while assigned(Job) and
       (Job^.State>=0) and
       (TPasMPInterlocked.Decrement(Job^.State)<0) do begin
  LastJob:=Job;
  Job:=TPasMPInterlocked.Exchange(pointer(Job^.ParentJob),nil);
  if (LastJob^.InternalData and PasMPJobFlagReleaseOnFinish)<>0 then begin
   FinishJobRelease(LastJob);
  end;
 end;
end;
{$ifend}

procedure TPasMP.ExecuteJobTask(const Job:PPasMPJob;const JobWorkerThread:TPasMPJobWorkerThread;const ThreadIndex:longint); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
var JobTask,NewJobTask:TPasMPJobTask;
    NewJob:PPasMPJob;
begin

 JobTask:=TPasMPJobTask(pointer(Job^.Method.Data));
 JobTask.fThreadIndex:=ThreadIndex;

 if CanSpread then begin
  // First try to spread, when all worker threads (except us) are jobless
  JobTask.Spread;
 end;

 if ((Job^.InternalData and PasMPJobFlagHasOwnerWorkerThread)<>0) and
    (longint(Job^.InternalData and PasMPJobThreadIndexMask)<>ThreadIndex) then begin
  // It's a stolen job => try Split
  NewJobTask:=JobTask.Split;
  if not assigned(NewJobTask) then begin
   // if Split of a stolen job has failed => try PartialPop
   NewJobTask:=JobTask.PartialPop;
  end;
 end else begin
  // It's a non-stolen job => try PartialPop
  NewJobTask:=JobTask.PartialPop;
 end;

 if assigned(NewJobTask) then begin
  // Run our both halfed jobs
  NewJob:=Acquire(NewJobTask);
  Run(NewJob);
  JobTask.Run;
  Wait(NewJob);
  Release(NewJob);
 end else begin
  // if PartialPop has also failed => just execute the job as whole
  JobTask.Run;
 end;

end;

procedure TPasMP.ExecuteJob(const Job:PPasMPJob;const JobWorkerThread:TPasMPJobWorkerThread); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 if JobWorkerThread.fJobQueue.HasJobs then begin
  WakeUpAll;
 end;
 if assigned(Job^.Method.Data) then begin
  if assigned(Job^.Method.Code) then begin
   TPasMPJobMethod(Job^.Method)(Job,JobWorkerThread.ThreadIndex);
  end else begin
   ExecuteJobTask(Job,JobWorkerThread,JobWorkerThread.ThreadIndex);
  end;
 end else begin
  if assigned(Job^.Method.Code) then begin
   TPasMPJobProcedure(pointer(Job^.Method.Code))(Job,JobWorkerThread.ThreadIndex);
  end;
 end;
 FinishJob(Job);
end;

procedure TPasMP.Run(const Job:PPasMPJob); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
var JobWorkerThread:TPasMPJobWorkerThread;
begin
 if assigned(Job) then begin
  JobWorkerThread:=GetJobWorkerThread;
  if assigned(JobWorkerThread) then begin
   JobWorkerThread.fJobQueue.PushJob(Job);
  end else begin
   fJobQueue.PushJob(Job);
  end;
  WakeUpAll;
 end;
end;

procedure TPasMP.Run(const Jobs:array of PPasMPJob);
var JobWorkerThread:TPasMPJobWorkerThread;
    JobIndex:longint;
    Job:PPasMPJob;
begin
 JobWorkerThread:=GetJobWorkerThread;
 for JobIndex:=0 to length(Jobs)-1 do begin
  Job:=Jobs[JobIndex];
  if assigned(Job) then begin
   if assigned(JobWorkerThread) then begin
    JobWorkerThread.fJobQueue.PushJob(Job);
   end else begin
    fJobQueue.PushJob(Job);
   end;
  end;
 end;
 WakeUpAll;
end;

procedure TPasMP.Wait(const Job:PPasMPJob);
var SpinCount,CountMaxSpinCount:longint;
    NextJob:PPasMPJob;
    JobWorkerThread:TPasMPJobWorkerThread;
begin
 if assigned(Job) then begin
  JobWorkerThread:=GetJobWorkerThread;
  SpinCount:=0;
  CountMaxSpinCount:=128;
  while Job^.State>=0 do begin
   if assigned(JobWorkerThread) then begin
    NextJob:=JobWorkerThread.GetJob;
    if assigned(NextJob) then begin
     ExecuteJob(NextJob,JobWorkerThread);
     SpinCount:=0;
    end else begin
     if SpinCount<CountMaxSpinCount then begin
      inc(SpinCount);
     end else begin
      TPasMP.Yield;
     end;
    end;
   end else begin
    TPasMP.Yield;
   end;
  end;
 end;
end;

procedure TPasMP.Wait(const Jobs:array of PPasMPJob);
var JobIndex,CountJobs,SpinCount,CountMaxSpinCount:longint;
    Job,NextJob:PPasMPJob;
    Done:boolean;
    JobWorkerThread:TPasMPJobWorkerThread;
begin
 CountJobs:=length(Jobs);
 if CountJobs>0 then begin
  JobWorkerThread:=GetJobWorkerThread;
  SpinCount:=0;
  CountMaxSpinCount:=128;
  repeat
   Done:=true;
   for JobIndex:=0 to CountJobs-1 do begin
    Job:=Jobs[JobIndex];
    if assigned(Job) and (Job^.State>=0) then begin
     Done:=false;
     break;
    end;
   end;
   if Done then begin
    break;
   end else begin
    if assigned(JobWorkerThread) then begin
     NextJob:=JobWorkerThread.GetJob;
     if assigned(NextJob) then begin
      ExecuteJob(NextJob,JobWorkerThread);
      SpinCount:=0;
     end else begin
      if SpinCount<CountMaxSpinCount then begin
       inc(SpinCount);
      end else begin
       TPasMP.Yield;
      end;
     end;
    end else begin
     TPasMP.Yield;
    end;
   end;
  until false;
 end;
end;

procedure TPasMP.RunWait(const Job:PPasMPJob); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 if assigned(Job) then begin
  Run(Job);
  Wait(Job);
 end;
end;

procedure TPasMP.RunWait(const Jobs:array of PPasMPJob);
begin
 Run(Jobs);
 Wait(Jobs);
end;

procedure TPasMP.WaitRelease(const Job:PPasMPJob); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 if assigned(Job) then begin
  Wait(Job);
  Release(Job);
 end;
end;

procedure TPasMP.WaitRelease(const Jobs:array of PPasMPJob);
begin
 Wait(Jobs);
 Release(Jobs);
end;

procedure TPasMP.Invoke(const Job:PPasMPJob); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 if assigned(Job) then begin
  Run(Job);
  Wait(Job);
  Release(Job);
 end;
end;

procedure TPasMP.Invoke(const Jobs:array of PPasMPJob);
begin
 Run(Jobs);
 Wait(Jobs);
 Release(Jobs);
end;

procedure TPasMP.Invoke(const JobTask:TPasMPJobTask); {$ifdef fpc}{$ifdef CAN_INLINE}inline;{$endif}{$endif}
begin
 Invoke(Acquire(JobTask));
end;

procedure TPasMP.Invoke(const JobTasks:array of TPasMPJobTask);
var CountJobTasks,Index:longint;
    Jobs:array of PPasMPJob;
begin
 Jobs:=nil;
 CountJobTasks:=length(JobTasks);
 SetLength(Jobs,CountJobTasks);
 try
  for Index:=0 to CountJobTasks-1 do begin
   Jobs[Index]:=Acquire(JobTasks[Index]);
  end;
  Invoke(Jobs);
 finally
  SetLength(Jobs,0);
 end;
end;

{$ifdef HAS_ANONYMOUS_METHODS}
type PPasMPParallelForReferenceProcedureStartJobData=^TPasMPParallelForReferenceProcedureStartJobData;
     TPasMPParallelForReferenceProcedureStartJobData=record
      ParallelForReferenceProcedure:TPasMPParallelForReferenceProcedure;
      Data:pointer;
      FirstIndex:longint;
      LastIndex:longint;
      Granularity:longint;
      Depth:longint;
      CanSpread:longbool;
     end;

     PPasMPParallelForReferenceProcedureJobData=^TPasMPParallelForReferenceProcedureJobData;
     TPasMPParallelForReferenceProcedureJobData=record
      StartJobData:PPasMPParallelForReferenceProcedureStartJobData;
      FirstIndex:longint;
      LastIndex:longint;
      RemainDepth:longint;
     end;

procedure TPasMP.ParallelForJobReferenceProcedureProcess(const Job:PPasMPJob;const ThreadIndex:longint);
var JobData:PPasMPParallelForReferenceProcedureJobData;
    StartJobData:PPasMPParallelForReferenceProcedureStartJobData;
begin
 JobData:=PPasMPParallelForReferenceProcedureJobData(pointer(@Job^.Data));
 StartJobData:=JobData^.StartJobData;
 if assigned(StartJobData^.ParallelForReferenceProcedure) then begin
  StartJobData^.ParallelForReferenceProcedure(Job,ThreadIndex,StartJobData^.Data,JobData^.FirstIndex,JobData^.LastIndex);
 end;
end;

procedure TPasMP.ParallelForJobReferenceProcedureFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var NewJobs:array[0..1] of PPasMPJob;
    StartJobData:PPasMPParallelForReferenceProcedureStartJobData;
    JobData,NewJobData:PPasMPParallelForReferenceProcedureJobData;
begin
 JobData:=PPasMPParallelForReferenceProcedureJobData(pointer(@Job^.Data));
 if JobData^.FirstIndex<=JobData^.LastIndex then begin
  StartJobData:=JobData^.StartJobData;
  if (((JobData^.LastIndex-JobData^.FirstIndex)+1)<=StartJobData^.Granularity) or (JobData^.RemainDepth=0) then begin
   ParallelForJobReferenceProcedureProcess(Job,ThreadIndex);
  end else begin
   if ((Job^.InternalData and PasMPJobFlagHasOwnerWorkerThread)<>0) and
      (longint(Job^.InternalData and PasMPJobThreadIndexMask)<>ThreadIndex) then begin
    // It is a stolen job => split in two halfs
    begin
     NewJobs[0]:=Acquire(ParallelForJobReferenceProcedureFunction,nil);
     NewJobData:=PPasMPParallelForReferenceProcedureJobData(pointer(@NewJobs[0]^.Data));
     NewJobData^.StartJobData:=StartJobData;
     NewJobData^.FirstIndex:=JobData^.FirstIndex;
     NewJobData^.LastIndex:=(JobData^.FirstIndex+((JobData^.LastIndex-JobData^.FirstIndex) div 2))-1;
     NewJobData^.RemainDepth:=JobData^.RemainDepth-1;
    end;
    begin
     NewJobs[1]:=Acquire(ParallelForJobReferenceProcedureFunction,nil);
     NewJobData:=PPasMPParallelForReferenceProcedureJobData(pointer(@NewJobs[1]^.Data));
     NewJobData^.StartJobData:=StartJobData;
     NewJobData^.FirstIndex:=PPasMPParallelForReferenceProcedureJobData(pointer(@NewJobs[0]^.Data))^.LastIndex+1;
     NewJobData^.LastIndex:=JobData^.LastIndex;
     NewJobData^.RemainDepth:=JobData^.RemainDepth-1;
    end;
    Invoke(NewJobs);
   end else begin
    // It is a non-stolen job => split and increment by granularity count
    begin
     NewJobs[0]:=Acquire(ParallelForJobReferenceProcedureFunction,nil);
     NewJobData:=PPasMPParallelForReferenceProcedureJobData(pointer(@NewJobs[0]^.Data));
     NewJobData^.StartJobData:=StartJobData;
     NewJobData^.FirstIndex:=JobData^.FirstIndex+StartJobData^.Granularity;
     NewJobData^.LastIndex:=JobData^.LastIndex;
     NewJobData^.RemainDepth:=JobData^.RemainDepth-1;
     JobData^.LastIndex:=NewJobData^.FirstIndex-1;
    end;
    Run(NewJobs[0]);
    ParallelForJobReferenceProcedureProcess(Job,ThreadIndex);
    WaitRelease(NewJobs[0]);
   end;
  end;
 end;
end;

procedure TPasMP.ParallelForStartJobReferenceProcedureFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var NewJobs:array[0..31] of PPasMPJob;
    JobData:PPasMPParallelForReferenceProcedureStartJobData;
    NewJobData:PPasMPParallelForReferenceProcedureJobData;
    Index,EndIndex,Granularity,Count,CountJobs,PartSize,Rest,Size,JobIndex:longint;
begin
 JobData:=PPasMPParallelForReferenceProcedureStartJobData(pointer(@Job^.Data));
 try
  Index:=JobData^.FirstIndex;
  EndIndex:=JobData^.LastIndex+1;
  if JobData^.FirstIndex<EndIndex then begin
   Granularity:=JobData^.Granularity;
   Count:=EndIndex-Index;
   if Count<=Granularity then begin
    ParallelForJobFunctionProcess(Job,ThreadIndex);
   end else begin
    if JobData^.CanSpread then begin
     // Only try to spread, when all worker threads (except us) are jobless
     CountJobs:=Count div Granularity;
    end else begin
     CountJobs:=1;
    end;
    if CountJobs<1 then begin
     CountJobs:=1;
    end else if CountJobs>length(NewJobs) then begin
     CountJobs:=length(NewJobs);
    end;
    PartSize:=Count div CountJobs;
    Rest:=Count-(CountJobs*PartSize);
    for JobIndex:=0 to CountJobs-1 do begin
     Size:=PartSize;
     if Rest>JobIndex then begin
      inc(Size);
     end;
     NewJobs[JobIndex]:=Acquire(ParallelForJobReferenceProcedureFunction,nil);
     NewJobData:=PPasMPParallelForReferenceProcedureJobData(pointer(@NewJobs[JobIndex]^.Data));
     NewJobData^.StartJobData:=JobData;
     NewJobData^.FirstIndex:=Index;
     NewJobData^.LastIndex:=(Index+Size)-1;
     if NewJobData^.LastIndex>JobData^.LastIndex then begin
      NewJobData^.LastIndex:=JobData^.LastIndex;
     end;
     NewJobData^.RemainDepth:=JobData^.Depth;
     Run(NewJobs[JobIndex]);
     inc(Index,Size);
    end;
    for JobIndex:=0 to CountJobs-1 do begin
     WaitRelease(NewJobs[JobIndex]);
    end;
   end;
  end;
 finally
  Finalize(JobData^);
 end;
end;

function TPasMP.ParallelFor(const Data:pointer;const FirstIndex,LastIndex:longint;const ParallelForReferenceProcedure:TPasMPParallelForReferenceProcedure;const Granularity:longint=1;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelForReferenceProcedureStartJobData;
begin
 result:=Acquire(ParallelForStartJobReferenceProcedureFunction,nil,ParentJob);
 JobData:=PPasMPParallelForReferenceProcedureStartJobData(pointer(@result^.Data));
 Initialize(JobData^);
 JobData^.ParallelForReferenceProcedure:=ParallelForReferenceProcedure;
 JobData^.Data:=Data;
 JobData^.FirstIndex:=FirstIndex;
 JobData^.LastIndex:=LastIndex;
 JobData^.Granularity:=Granularity;
 JobData^.Depth:=Depth;
 JobData^.CanSpread:=CanSpread;
end;
{$endif}

type PPasMPParallelForStartJobData=^TPasMPParallelForStartJobData;
     TPasMPParallelForStartJobData=record
      Method:TMethod;
      Data:pointer;
      FirstIndex:longint;
      LastIndex:longint;
      Granularity:longint;
      Depth:longint;
      CanSpread:longbool;
     end;

     PPasMPParallelForJobData=^TPasMPParallelForJobData;
     TPasMPParallelForJobData=record
      StartJobData:PPasMPParallelForStartJobData;
      FirstIndex:longint;
      LastIndex:longint;
      RemainDepth:longint;
     end;

procedure TPasMP.ParallelForJobFunctionProcess(const Job:PPasMPJob;const ThreadIndex:longint);
var JobData:PPasMPParallelForJobData;
    StartJobData:PPasMPParallelForStartJobData;
begin
 JobData:=PPasMPParallelForJobData(pointer(@Job^.Data));
 StartJobData:=JobData^.StartJobData;
 if assigned(StartJobData^.Method.Data) then begin
  TPasMPParallelForMethod(StartJobData^.Method)(Job,ThreadIndex,StartJobData^.Data,JobData^.FirstIndex,JobData^.LastIndex);
 end else begin
  TPasMPParallelForProcedure(StartJobData^.Method.Code)(Job,ThreadIndex,StartJobData^.Data,JobData^.FirstIndex,JobData^.LastIndex);
 end;
end;

procedure TPasMP.ParallelForJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var NewJobs:array[0..1] of PPasMPJob;
    JobData,NewJobData:PPasMPParallelForJobData;
    StartJobData:PPasMPParallelForStartJobData;
begin
 JobData:=PPasMPParallelForJobData(pointer(@Job^.Data));
 if JobData^.FirstIndex<=JobData^.LastIndex then begin
  StartJobData:=JobData^.StartJobData;
  if (((JobData^.LastIndex-JobData^.FirstIndex)+1)<=StartJobData^.Granularity) or (JobData^.RemainDepth<=0) then begin
   ParallelForJobFunctionProcess(Job,ThreadIndex);
  end else begin
   if ((Job^.InternalData and PasMPJobFlagHasOwnerWorkerThread)<>0) and
      (longint(Job^.InternalData and PasMPJobThreadIndexMask)<>ThreadIndex) then begin
    // It is a stolen job => split in two halfs
    begin
     NewJobs[0]:=Acquire(ParallelForJobFunction,nil);
     NewJobData:=PPasMPParallelForJobData(pointer(@NewJobs[0]^.Data));
     NewJobData^.StartJobData:=JobData^.StartJobData;
     NewJobData^.FirstIndex:=JobData^.FirstIndex;
     NewJobData^.LastIndex:=(JobData^.FirstIndex+((JobData^.LastIndex-JobData^.FirstIndex) div 2))-1;
     NewJobData^.RemainDepth:=JobData^.RemainDepth-1;
    end;
    begin
     NewJobs[1]:=Acquire(ParallelForJobFunction,nil);
     NewJobData:=PPasMPParallelForJobData(pointer(@NewJobs[1]^.Data));
     NewJobData^.StartJobData:=JobData^.StartJobData;
     NewJobData^.FirstIndex:=PPasMPParallelForJobData(pointer(@NewJobs[0]^.Data))^.LastIndex+1;
     NewJobData^.LastIndex:=JobData^.LastIndex;
     NewJobData^.RemainDepth:=JobData^.RemainDepth-1;
    end;
    Invoke(NewJobs);
   end else begin
    // It is a non-stolen job => split and increment by granularity count
    begin
     NewJobs[0]:=Acquire(ParallelForJobFunction,nil);
     NewJobData:=PPasMPParallelForJobData(pointer(@NewJobs[0]^.Data));
     NewJobData^.StartJobData:=JobData^.StartJobData;
     NewJobData^.FirstIndex:=JobData^.FirstIndex+StartJobData^.Granularity;
     NewJobData^.LastIndex:=JobData^.LastIndex;
     NewJobData^.RemainDepth:=JobData^.RemainDepth-1;
     JobData^.LastIndex:=NewJobData^.FirstIndex-1;
    end;
    Run(NewJobs[0]);
    ParallelForJobFunctionProcess(Job,ThreadIndex);
    WaitRelease(NewJobs[0]);
   end;
  end;
 end;
end;

procedure TPasMP.ParallelForStartJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var NewJobs:array[0..31] of PPasMPJob;
    JobData:PPasMPParallelForStartJobData;
    NewJobData:PPasMPParallelForJobData;
    Index,EndIndex,Granularity,Count,CountJobs,PartSize,Rest,Size,JobIndex:longint;
begin
 JobData:=PPasMPParallelForStartJobData(pointer(@Job^.Data));
 Index:=JobData^.FirstIndex;
 EndIndex:=JobData^.LastIndex+1;
 if JobData^.FirstIndex<EndIndex then begin
  Granularity:=JobData^.Granularity;
  Count:=EndIndex-Index;
  if Count<=Granularity then begin
   ParallelForJobFunctionProcess(Job,ThreadIndex);
  end else begin
   if JobData^.CanSpread then begin
    // Only try to spread, when all worker threads (except us) are jobless
    CountJobs:=Count div Granularity;
   end else begin
    CountJobs:=1;
   end;
   if CountJobs<1 then begin
    CountJobs:=1;
   end else if CountJobs>length(NewJobs) then begin
    CountJobs:=length(NewJobs);
   end;
   PartSize:=Count div CountJobs;
   Rest:=Count-(CountJobs*PartSize);
   for JobIndex:=0 to CountJobs-1 do begin
    Size:=PartSize;
    if Rest>JobIndex then begin
     inc(Size);
    end;
    NewJobs[JobIndex]:=Acquire(ParallelForJobFunction,nil);
    NewJobData:=PPasMPParallelForJobData(pointer(@NewJobs[JobIndex]^.Data));
    NewJobData^.StartJobData:=JobData;
    NewJobData^.FirstIndex:=Index;
    NewJobData^.LastIndex:=(Index+Size)-1;
    if NewJobData^.LastIndex>JobData^.LastIndex then begin
     NewJobData^.LastIndex:=JobData^.LastIndex;
    end;
    NewJobData^.RemainDepth:=JobData^.Depth;
    Run(NewJobs[JobIndex]);
    inc(Index,Size);
   end;
   for JobIndex:=0 to CountJobs-1 do begin
    WaitRelease(NewJobs[JobIndex]);
   end;
  end;
 end;
end;

function TPasMP.ParallelFor(const Data:pointer;const FirstIndex,LastIndex:longint;const ParallelForProcedure:TPasMPParallelForProcedure;const Granularity:longint=1;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelForStartJobData;
begin
 result:=Acquire(ParallelForStartJobFunction,nil,ParentJob);
 JobData:=PPasMPParallelForStartJobData(pointer(@result^.Data));
 JobData^.Method.Code:=Addr(ParallelForProcedure);
 JobData^.Method.Data:=nil;
 JobData^.Data:=Data;
 JobData^.FirstIndex:=FirstIndex;
 JobData^.LastIndex:=LastIndex;
 if Granularity<1 then begin
  JobData^.Granularity:=1;
 end else begin
  JobData^.Granularity:=Granularity;
 end;
 JobData^.Depth:=Depth;
 JobData^.CanSpread:=CanSpread;
end;

function TPasMP.ParallelFor(const Data:pointer;const FirstIndex,LastIndex:longint;const ParallelForMethod:TPasMPParallelForMethod;const Granularity:longint=1;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelForStartJobData;
begin
 result:=Acquire(ParallelForStartJobFunction,nil,ParentJob);
 JobData:=PPasMPParallelForStartJobData(pointer(@result^.Data));
 JobData^.Method:=TMethod(ParallelForMethod);
 JobData^.Data:=Data;
 JobData^.FirstIndex:=FirstIndex;
 JobData^.LastIndex:=LastIndex;
 if Granularity<1 then begin
  JobData^.Granularity:=1;
 end else begin
  JobData^.Granularity:=Granularity;
 end;
 JobData^.Depth:=Depth;
end;

type PPasMPParallelDirectIntroSortJobData=^TPasMPParallelDirectIntroSortJobData;
     TPasMPParallelDirectIntroSortJobData=record
      Items:pointer;
      Left:longint;
      Right:longint;
      Depth:longint;
      ElementSize:longint;
      Granularity:longint;
      CompareFunc:TPasMPParallelSortCompareFunction;
     end;

procedure TPasMP.ParallelDirectIntroSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
type PByteArray=^TByteArray;
     TByteArray=array[0..$3fffffff] of byte;
var NewJobs:array[0..1] of PPasMPJob;
    JobData,NewJobData:PPasMPParallelDirectIntroSortJobData;
    Left,Right,ElementSize,Size,Parent,Child,Middle,Pivot,i,j,iA,iB,iC:longint;
    CompareFunc:TPasMPParallelSortCompareFunction;
    Items,Temp:pointer;
begin
 JobData:=PPasMPParallelDirectIntroSortJobData(pointer(@Job^.Data));
 Left:=JobData^.Left;
 Right:=JobData^.Right;
 if Left<Right then begin
  Items:=JobData^.Items;
  ElementSize:=JobData^.ElementSize;
  CompareFunc:=JobData^.CompareFunc;
  Size:=(Right-Left)+1;
  if Size<16 then begin
   // Insertion sort
   iA:=Left;
   iB:=iA+1;
   while iB<=Right do begin
    iC:=iB;
    while (iA>=Left) and
          (iC>=Left) and
          (CompareFunc(pointer(@PByteArray(Items)^[iA*ElementSize]),pointer(@PByteArray(Items)^[iC*ElementSize]))>0) do begin
     MemorySwap(@PByteArray(Items)^[iA*ElementSize],@PByteArray(Items)^[iC*ElementSize],ElementSize);
     dec(iA);
     dec(iC);
    end;
    iA:=iB;
    inc(iB);
   end;
  end else begin
   if (JobData^.Depth=0) or (Size<=JobData^.Granularity) then begin
    // Heap sort
    GetMem(Temp,JobData^.ElementSize);
    try
     i:=Size div 2;
     repeat
      if i>Left then begin
       dec(i);
       Move(PByteArray(Items)^[(Left+i)*ElementSize],Temp^,ElementSize);
      end else begin
       if Size=0 then begin
        break;
       end else begin
        dec(Size);
        Move(PByteArray(Items)^[(Left+Size)*ElementSize],Temp^,ElementSize);
        Move(PByteArray(Items)^[Left*ElementSize],PByteArray(Items)^[(Left+Size)*ElementSize],ElementSize);
       end;
      end;
      Parent:=i;
      Child:=(i*2)+1;
      while Child<Size do begin
       if ((Child+1)<Size) and (CompareFunc(pointer(@PByteArray(Items)^[((Left+Child)+1)*ElementSize]),pointer(@PByteArray(Items)^[(Left+Child)*ElementSize]))>0) then begin
        inc(Child);
       end;
       if CompareFunc(pointer(@PByteArray(Items)^[(Left+Child)*ElementSize]),Temp)>0 then begin
        Move(PByteArray(Items)^[(Left+Child)*ElementSize],PByteArray(Items)^[(Left+Parent)*ElementSize],ElementSize);
        Parent:=Child;
        Child:=(Parent*2)+1;
       end else begin
        break;
       end;
      end;
      Move(Temp^,PByteArray(Items)^[(Left+Parent)*ElementSize],ElementSize);
     until false;
    finally
     FreeMem(Temp);
    end;
   end else begin
    // Quick sort width median-of-three optimization
    Middle:=Left+((Right-Left) shr 1);
    if (Right-Left)>3 then begin
     if CompareFunc(pointer(@PByteArray(Items)^[Left*ElementSize]),pointer(@PByteArray(Items)^[Middle*ElementSize]))>0 then begin
      MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Middle*ElementSize],ElementSize);
     end;
     if CompareFunc(pointer(@PByteArray(Items)^[Left*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))>0 then begin
      MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
     end;
     if CompareFunc(pointer(@PByteArray(Items)^[Middle*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))>0 then begin
      MemorySwap(@PByteArray(Items)^[Middle*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
     end;
    end;
    Pivot:=Middle;
    i:=Left;
    j:=Right;
    repeat
     while (i<Right) and (CompareFunc(pointer(@PByteArray(Items)^[i*ElementSize]),pointer(@PByteArray(Items)^[Pivot*ElementSize]))<0) do begin
      inc(i);
     end;
     while (j>=i) and (CompareFunc(pointer(@PByteArray(Items)^[j*ElementSize]),pointer(@PByteArray(Items)^[Pivot*ElementSize]))>0) do begin
      dec(j);
     end;
     if i>j then begin
      break;
     end else begin
      if i<>j then begin
       MemorySwap(@PByteArray(Items)^[i*ElementSize],@PByteArray(Items)^[j*ElementSize],ElementSize);
       if Pivot=i then begin
        Pivot:=j;
       end else if Pivot=j then begin
        Pivot:=i;
       end;
      end;
      inc(i);
      dec(j);
     end;
    until false;
    if Left<j then begin
     NewJobs[0]:=Acquire(ParallelDirectIntroSortJobFunction,nil);
     NewJobData:=PPasMPParallelDirectIntroSortJobData(pointer(@NewJobs[0]^.Data));
     NewJobData^.Items:=JobData^.Items;
     NewJobData^.Left:=Left;
     NewJobData^.Right:=j;
     NewJobData^.Depth:=JobData^.Depth-1;
     NewJobData^.ElementSize:=JobData^.ElementSize;
     NewJobData^.Granularity:=JobData^.Granularity;
     NewJobData^.CompareFunc:=CompareFunc;
    end else begin
     NewJobs[0]:=nil;
    end;
    if i<Right then begin
     NewJobs[1]:=Acquire(ParallelDirectIntroSortJobFunction,nil);
     NewJobData:=PPasMPParallelDirectIntroSortJobData(pointer(@NewJobs[1]^.Data));
     NewJobData^.Items:=JobData^.Items;
     NewJobData^.Left:=i;
     NewJobData^.Right:=Right;
     NewJobData^.Depth:=JobData^.Depth-1;
     NewJobData^.ElementSize:=JobData^.ElementSize;
     NewJobData^.Granularity:=JobData^.Granularity;
     NewJobData^.CompareFunc:=CompareFunc;
    end else begin
     NewJobs[1]:=nil;
    end;
    Invoke(NewJobs);
   end;
  end;
 end;
end;

function TPasMP.ParallelDirectIntroSort(const Items:pointer;const Left,Right,ElementSize:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelDirectIntroSortJobData;
begin
 result:=Acquire(ParallelDirectIntroSortJobFunction,nil,ParentJob);
 JobData:=PPasMPParallelDirectIntroSortJobData(pointer(@result^.Data));
 JobData^.Items:=Items;
 JobData^.Left:=Left;
 JobData^.Right:=Right;
 if Left<Right then begin
  JobData^.Depth:=IntLog2((Right-Left)+1) shl 1;
  if JobData^.Depth>Depth then begin
   JobData^.Depth:=Depth;
  end;
 end else begin
  JobData^.Depth:=0;
 end;
 JobData^.ElementSize:=ElementSize;
 JobData^.Granularity:=Granularity;
 JobData^.CompareFunc:=CompareFunc;
end;

type PPasMPParallelIndirectIntroSortJobData=^TPasMPParallelIndirectIntroSortJobData;
     TPasMPParallelIndirectIntroSortJobData=record
      Items:pointer;
      Left:longint;
      Right:longint;
      Depth:longint;
      Granularity:longint;
      CompareFunc:TPasMPParallelSortCompareFunction;
     end;

procedure TPasMP.ParallelIndirectIntroSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
type PPointers=^TPointers;
     TPointers=array[0..($7fffffff div SizeOf(pointer))-1] of pointer;
var NewJobs:array[0..1] of PPasMPJob;
    JobData,NewJobData:PPasMPParallelIndirectIntroSortJobData;
    Left,Right,Size,Parent,Child,Middle,i,j:longint;
    CompareFunc:TPasMPParallelSortCompareFunction;
    Items,Temp,Pivot:pointer;
begin
 JobData:=PPasMPParallelIndirectIntroSortJobData(pointer(@Job^.Data));
 Left:=JobData^.Left;
 Right:=JobData^.Right;
 if Left<Right then begin
  Items:=JobData^.Items;
  CompareFunc:=JobData^.CompareFunc;
  Size:=(Right-Left)+1;
  if Size<16 then begin
   // Insertion sort
   for i:=Left+1 to Right do begin
    Temp:=PPointers(Items)^[i];
    j:=i-1;
    if (j>=Left) and (CompareFunc(PPointers(Items)^[j],Temp)>0) then begin
     repeat
      PPointers(Items)^[j+1]:=PPointers(Items)^[j];
      dec(j);
     until not ((j>=Left) and (CompareFunc(PPointers(Items)^[j],Temp)>0));
     PPointers(Items)^[j+1]:=Temp;
    end;
   end;
  end else begin
   if (JobData^.Depth=0) or (Size<=JobData^.Granularity) then begin
    // Heap sort
    i:=Size div 2;
    Temp:=nil;
    repeat
     if i>Left then begin
      dec(i);
      Temp:=PPointers(Items)^[Left+i];
     end else begin
      if Size=0 then begin
       break;
      end else begin
       dec(Size);
       Temp:=PPointers(Items)^[Left+Size];
       PPointers(Items)^[Left+Size]:=PPointers(Items)^[Left];
      end;
     end;
     Parent:=i;
     Child:=(i*2)+1;
     while Child<Size do begin
      if ((Child+1)<Size) and (CompareFunc(PPointers(Items)^[Left+Child+1],PPointers(Items)^[Left+Child])>0) then begin
       inc(Child);
      end;
      if CompareFunc(PPointers(Items)^[Left+Child],Temp)>0 then begin
       PPointers(Items)^[Left+Parent]:=PPointers(Items)^[Left+Child];
       Parent:=Child;
       Child:=(Parent*2)+1;
      end else begin
       break;
      end;
     end;
     PPointers(Items)^[Left+Parent]:=Temp;
    until false;
   end else begin
    // Quick sort width median-of-three optimization
    Middle:=Left+((Right-Left) shr 1);
    if (Right-Left)>3 then begin
     if CompareFunc(PPointers(Items)^[Left],PPointers(Items)^[Middle])>0 then begin
      Temp:=PPointers(Items)^[Left];
      PPointers(Items)^[Left]:=PPointers(Items)^[Middle];
      PPointers(Items)^[Middle]:=Temp;
     end;
     if CompareFunc(PPointers(Items)^[Left],PPointers(Items)^[Right])>0 then begin
      Temp:=PPointers(Items)^[Left];
      PPointers(Items)^[Left]:=PPointers(Items)^[Right];
      PPointers(Items)^[Right]:=Temp;
     end;
     if CompareFunc(PPointers(Items)^[Middle],PPointers(Items)^[Right])>0 then begin
      Temp:=PPointers(Items)^[Middle];
      PPointers(Items)^[Middle]:=PPointers(Items)^[Right];
      PPointers(Items)^[Right]:=Temp;
     end;
    end;
    Pivot:=PPointers(Items)^[Middle];
    i:=Left;
    j:=Right;
    repeat
     while (i<Right) and (CompareFunc(PPointers(Items)^[i],Pivot)<0) do begin
      inc(i);
     end;
     while (j>=i) and (CompareFunc(PPointers(Items)^[j],Pivot)>0) do begin
      dec(j);
     end;
     if i>j then begin
      break;
     end else begin
      if i<>j then begin
       Temp:=PPointers(Items)^[i];
       PPointers(Items)^[i]:=PPointers(Items)^[j];
       PPointers(Items)^[j]:=Temp;
      end;
      inc(i);
      dec(j);
     end;
    until false;
    if Left<j then begin
     NewJobs[0]:=Acquire(ParallelIndirectIntroSortJobFunction,nil);
     NewJobData:=PPasMPParallelIndirectIntroSortJobData(pointer(@NewJobs[0]^.Data));
     NewJobData^.Items:=JobData^.Items;
     NewJobData^.Left:=Left;
     NewJobData^.Right:=j;
     NewJobData^.Depth:=JobData^.Depth-1;
     NewJobData^.Granularity:=JobData^.Granularity;
     NewJobData^.CompareFunc:=CompareFunc;
    end else begin
     NewJobs[0]:=nil;
    end;
    if i<Right then begin
     NewJobs[1]:=Acquire(ParallelIndirectIntroSortJobFunction,nil);
     NewJobData:=PPasMPParallelIndirectIntroSortJobData(pointer(@NewJobs[1]^.Data));
     NewJobData^.Items:=JobData^.Items;
     NewJobData^.Left:=i;
     NewJobData^.Right:=Right;
     NewJobData^.Depth:=JobData^.Depth-1;
     NewJobData^.Granularity:=JobData^.Granularity;
     NewJobData^.CompareFunc:=CompareFunc;
    end else begin
     NewJobs[1]:=nil;
    end;
    Invoke(NewJobs);
   end;
  end;
 end;
end;

function TPasMP.ParallelIndirectIntroSort(const Items:pointer;const Left,Right:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelIndirectIntroSortJobData;
begin
 result:=Acquire(ParallelIndirectIntroSortJobFunction,nil,ParentJob);
 JobData:=PPasMPParallelIndirectIntroSortJobData(pointer(@result^.Data));
 JobData^.Items:=Items;
 JobData^.Left:=Left;
 JobData^.Right:=Right;
 if Left<Right then begin
  JobData^.Depth:=IntLog2((Right-Left)+1) shl 1;
  if JobData^.Depth>Depth then begin
   JobData^.Depth:=Depth;
  end;
 end else begin
  JobData^.Depth:=0;
 end;
 JobData^.Granularity:=Granularity;
 JobData^.CompareFunc:=CompareFunc;
end;

type PPasMPParallelDirectMergeSortData=^TPasMPParallelDirectMergeSortData;
     TPasMPParallelDirectMergeSortData=record
      Items:pointer;
      Temp:pointer;
      ElementSize:longint;
      Granularity:longint;
      CompareFunc:TPasMPParallelSortCompareFunction;
     end;

     PPasMPParallelDirectMergeSortJobData=^TPasMPParallelDirectMergeSortJobData;
     TPasMPParallelDirectMergeSortJobData=record
      Data:PPasMPParallelDirectMergeSortData;
      Left:longint;
      Right:longint;
      Depth:longint;
     end;

procedure TPasMP.ParallelDirectMergeSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
type PByteArray=^TByteArray;
     TByteArray=array[0..$3fffffff] of byte;
var NewJobs:array[0..1] of PPasMPJob;
    JobData,NewJobData:PPasMPParallelDirectMergeSortJobData;
    Left,Right,ElementSize,Size,Middle,iA,iB,iC,Count:longint;
    CompareFunc:TPasMPParallelSortCompareFunction;
    Items,Temp:pointer;
    Data:PPasMPParallelDirectMergeSortData;
begin
 JobData:=PPasMPParallelDirectMergeSortJobData(pointer(@Job^.Data));
 Left:=JobData^.Left;
 Right:=JobData^.Right;
 if Left<Right then begin
  Data:=JobData^.Data;
  Items:=Data^.Items;
  ElementSize:=Data^.ElementSize;
  CompareFunc:=Data^.CompareFunc;
  Size:=(Right-Left)+1;
  case Size of
   2:begin
    if CompareFunc(pointer(@PByteArray(Items)^[Left*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))>0 then begin
     MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
    end;
   end;
   3:begin
    Middle:=Left+1;
    if CompareFunc(pointer(@PByteArray(Items)^[Left*ElementSize]),pointer(@PByteArray(Items)^[Middle*ElementSize]))<=0 then begin
     if CompareFunc(pointer(@PByteArray(Items)^[Middle*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))<=0 then begin
      // 0 <= 1 <= 2
     end else if CompareFunc(pointer(@PByteArray(Items)^[Left*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))<=0 then begin
      // 0 <= 2 < 1
      MemorySwap(@PByteArray(Items)^[Middle*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
     end else begin
      // 2 < 0 <= 1
      MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
      MemorySwap(@PByteArray(Items)^[Middle*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
     end;
    end else begin
     if CompareFunc(pointer(@PByteArray(Items)^[Left*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))<=0 then begin
      // 1 < 0 <= 2
      MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Middle*ElementSize],ElementSize);
     end else if CompareFunc(pointer(@PByteArray(Items)^[Middle*ElementSize]),pointer(@PByteArray(Items)^[Right*ElementSize]))<=0 then begin
      // 1 <= 2 < 0
      MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Middle*ElementSize],ElementSize);
      MemorySwap(@PByteArray(Items)^[Middle*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
     end else begin
      // 2 < 1 < 0
      MemorySwap(@PByteArray(Items)^[Left*ElementSize],@PByteArray(Items)^[Right*ElementSize],ElementSize);
     end;
    end;
   end;
   else begin
    if (JobData^.Depth=0) or (Size<=JobData^.Data.Granularity) then begin
{    // Insertion sort (with temporary memory)
     GetMem(Temp,ElementSize);
     try
      for iA:=Left+1 to Right do begin
       iB:=iA-1;
       if (iB>=Left) and (CompareFunc(pointer(@PByteArray(Items)^[iB*ElementSize]),pointer(@PByteArray(Items)^[iA*ElementSize]))>0) then begin
        Move(PByteArray(Items)^[iA*ElementSize],Temp^,ElementSize);
        repeat
         Move(PByteArray(Items)^[iB*ElementSize],PByteArray(Items)^[(iB+1)*ElementSize],ElementSize);
         dec(iB);
        until not ((iB>=Left) and (CompareFunc(pointer(@PByteArray(Items)^[iB*ElementSize]),Temp)>0));
        Move(Temp^,PByteArray(Items)^[(iB+1)*ElementSize],ElementSize);
       end;
      end;
     finally
      FreeMem(Temp);
     end;}
     // Insertion sort (in-place)
     iA:=Left;
     iB:=iA+1;
     while iB<=Right do begin
      iC:=iB;
      while (iA>=Left) and
            (iC>=Left) and
            (CompareFunc(pointer(@PByteArray(Items)^[iA*ElementSize]),pointer(@PByteArray(Items)^[iC*ElementSize]))>0) do begin
       MemorySwap(@PByteArray(Items)^[iA*ElementSize],@PByteArray(Items)^[iC*ElementSize],ElementSize);
       dec(iA);
       dec(iC);
      end;
      iA:=iB;
      inc(iB);
     end;
    end else begin
     Middle:=Left+((Right-Left) shr 1);
     if Left<Middle then begin
      NewJobs[0]:=Acquire(ParallelDirectMergeSortJobFunction,nil);
      NewJobData:=PPasMPParallelDirectMergeSortJobData(pointer(@NewJobs[0]^.Data));
      NewJobData^.Data:=Data;
      NewJobData^.Left:=Left;
      NewJobData^.Right:=Middle-1;
      NewJobData^.Depth:=JobData^.Depth-1;
     end else begin
      NewJobs[0]:=nil;
     end;
     if Middle<=Right then begin
      NewJobs[1]:=Acquire(ParallelDirectMergeSortJobFunction,nil);
      NewJobData:=PPasMPParallelDirectMergeSortJobData(pointer(@NewJobs[1]^.Data));
      NewJobData^.Data:=JobData^.Data;
      NewJobData^.Left:=Middle;
      NewJobData^.Right:=Right;
      NewJobData^.Depth:=JobData^.Depth-1;
     end else begin
      NewJobs[1]:=nil;
     end;
     Invoke(NewJobs);
     begin
      // Merge
      Temp:=Data^.Temp;
      iA:=Left;
      iB:=Middle;
      iC:=Left;
      while (iA<Middle) and
            (CompareFunc(pointer(@PByteArray(Items)^[iA*ElementSize]),pointer(@PByteArray(Items)^[iB*ElementSize]))<=0) do begin
       inc(iA);
      end;
      if iA<Middle then begin
       Left:=iA;
       iC:=iA;
       Move(PByteArray(Items)^[iB*ElementSize],PByteArray(Temp)^[iC*ElementSize],ElementSize);
       inc(iB);
       inc(iC);
       while (iA<Middle) and (iB<=Right) do begin
        if CompareFunc(pointer(@PByteArray(Items)^[iA*ElementSize]),pointer(@PByteArray(Items)^[iB*ElementSize]))>0 then begin
         Move(PByteArray(Items)^[iB*ElementSize],PByteArray(Temp)^[iC*ElementSize],ElementSize);
         inc(iB);
        end else begin
         Move(PByteArray(Items)^[iA*ElementSize],PByteArray(Temp)^[iC*ElementSize],ElementSize);
         inc(iA);
        end;
        inc(iC);
       end;
       if iA<Middle then begin
        Count:=Middle-iA;
        Move(PByteArray(Items)^[iA*ElementSize],PByteArray(Temp)^[iC*ElementSize],Count*ElementSize);
        inc(iC,Count);
       end;
       if iB<=Right then begin
        Count:=(Right-iB)+1;
        Move(PByteArray(Items)^[iB*ElementSize],PByteArray(Temp)^[iC*ElementSize],Count*ElementSize);
       end;
       Move(PByteArray(Temp)^[Left*ElementSize],PByteArray(Items)^[Left*ElementSize],((Right-Left)+1)*ElementSize);
      end;
     end;
    end;
   end;
  end;
 end;
end;

type PPasMPParallelDirectMergeSortRootJobData=^TPasMPParallelDirectMergeSortRootJobData;
     TPasMPParallelDirectMergeSortRootJobData=record
      Items:pointer;
      Left:longint;
      Right:longint;
      Depth:longint;
      ElementSize:longint;
      Granularity:longint;
      CompareFunc:TPasMPParallelSortCompareFunction;
     end;

procedure TPasMP.ParallelDirectMergeSortRootJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var Data:TPasMPParallelDirectMergeSortData;
    JobData:PPasMPParallelDirectMergeSortRootJobData;
    ChildJobData:PPasMPParallelDirectMergeSortJobData;
    ChildJob:PPasMPJob;
begin
 JobData:=PPasMPParallelDirectMergeSortRootJobData(pointer(@Job^.Data));
 GetMem(Data.Temp,((JobData^.Right-JobData^.Left)+1)*JobData^.ElementSize);
 try
  Data.Items:=JobData^.Items;
  Data.ElementSize:=JobData^.ElementSize;
  Data.Granularity:=JobData^.Granularity;
  Data.CompareFunc:=JobData^.CompareFunc;
  ChildJob:=Acquire(ParallelDirectMergeSortJobFunction,nil);
  ChildJobData:=PPasMPParallelDirectMergeSortJobData(pointer(@ChildJob^.Data));
  ChildJobData^.Data:=@Data;
  ChildJobData^.Left:=JobData^.Left;
  ChildJobData^.Right:=JobData^.Right;
  ChildJobData^.Depth:=JobData^.Depth;
  Invoke(ChildJob);
 finally
  FreeMem(Data.Temp);
 end;
end;

function TPasMP.ParallelDirectMergeSort(const Items:pointer;const Left,Right,ElementSize:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelDirectMergeSortRootJobData;
begin
 if ((Left+1)<Right) and (ElementSize>0) then begin
  result:=Acquire(ParallelDirectMergeSortRootJobFunction,nil,ParentJob);
  JobData:=PPasMPParallelDirectMergeSortRootJobData(pointer(@result^.Data));
  JobData^.Items:=Items;
  JobData^.Left:=Left;
  JobData^.Right:=Right;
  JobData^.ElementSize:=ElementSize;
  JobData^.Granularity:=Granularity;
  JobData^.CompareFunc:=CompareFunc;
  if Left<Right then begin
   JobData^.Depth:=IntLog2((Right-Left)+1);
   if JobData^.Depth>Depth then begin
    JobData^.Depth:=Depth;
   end;
  end else begin
   JobData^.Depth:=0;
  end;
 end else begin
  result:=nil;
 end;
end;

type PPasMPParallelIndirectMergeSortData=^TPasMPParallelIndirectMergeSortData;
     TPasMPParallelIndirectMergeSortData=record
      Items:pointer;
      Temp:pointer;
      Granularity:longint;
      CompareFunc:TPasMPParallelSortCompareFunction;
     end;

     PPasMPParallelIndirectMergeSortJobData=^TPasMPParallelIndirectMergeSortJobData;
     TPasMPParallelIndirectMergeSortJobData=record
      Data:PPasMPParallelIndirectMergeSortData;
      Left:longint;
      Right:longint;
      Depth:longint;
     end;

procedure TPasMP.ParallelIndirectMergeSortJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
type PPointers=^TPointers;
     TPointers=array[0..($7fffffff div SizeOf(pointer))-1] of pointer;
var ChildJobs:array[0..1] of PPasMPJob;
    JobData,ChildJobData:PPasMPParallelIndirectMergeSortJobData;
    Left,Right,Size,Middle,i,j,iA,iB,iC,Count:longint;
    CompareFunc:TPasMPParallelSortCompareFunction;
    Items,Temp:pointer;
    Data:PPasMPParallelIndirectMergeSortData;
begin
 JobData:=PPasMPParallelIndirectMergeSortJobData(pointer(@Job^.Data));
 Left:=JobData^.Left;
 Right:=JobData^.Right;
 if Left<Right then begin
  Data:=JobData^.Data;
  Items:=Data^.Items;
  CompareFunc:=Data^.CompareFunc;
  Size:=(Right-Left)+1;
  case Size of
   2:begin
    if CompareFunc(PPointers(Items)^[Left],PPointers(Items)^[Right])>0 then begin
     Temp:=PPointers(Items)^[Left];
     PPointers(Items)^[Left]:=PPointers(Items)^[Right];
     PPointers(Items)^[Right]:=Temp;
    end;
   end;
   3:begin
    if CompareFunc(PPointers(Items)^[Left+0],PPointers(Items)^[Left+1])<=0 then begin
     if CompareFunc(PPointers(Items)^[Left+1],PPointers(Items)^[Left+2])<=0 then begin
      // 0 <= 1 <= 2
     end else if CompareFunc(PPointers(Items)^[Left+0],PPointers(Items)^[Left+2])<=0 then begin
      // 0 <= 2 < 1
      Temp:=PPointers(Items)^[Left+1];
      PPointers(Items)^[Left+1]:=PPointers(Items)^[Left+2];
      PPointers(Items)^[Left+2]:=Temp;
     end else begin
      // 2 < 0 <= 1
      Temp:=PPointers(Items)^[Left+0];
      PPointers(Items)^[Left+0]:=PPointers(Items)^[Left+2];
      PPointers(Items)^[Left+2]:=PPointers(Items)^[Left+1];
      PPointers(Items)^[Left+1]:=Temp;
     end;
    end else begin
     if CompareFunc(PPointers(Items)^[Left+0],PPointers(Items)^[Left+2])<=0 then begin
      // 1 < 0 <= 2
      Temp:=PPointers(Items)^[Left+0];
      PPointers(Items)^[Left+0]:=PPointers(Items)^[Left+1];
      PPointers(Items)^[Left+1]:=Temp;
     end else if CompareFunc(PPointers(Items)^[Left+1],PPointers(Items)^[Left+2])<=0 then begin
      // 1 <= 2 < 0
      Temp:=PPointers(Items)^[Left+0];
      PPointers(Items)^[Left+0]:=PPointers(Items)^[Left+1];
      PPointers(Items)^[Left+1]:=PPointers(Items)^[Left+2];
      PPointers(Items)^[Left+2]:=Temp;
     end else begin
      // 2 < 1 < 0
      Temp:=PPointers(Items)^[Left+0];
      PPointers(Items)^[Left+0]:=PPointers(Items)^[Left+2];
      PPointers(Items)^[Left+2]:=Temp;
     end;
    end;
   end;
   else begin
    if (JobData^.Depth=0) or (Size<=JobData^.Data.Granularity) then begin
     // Insertion sort
     for i:=Left+1 to Right do begin
      j:=i-1;
      if (j>=Left) and (CompareFunc(PPointers(Items)^[j],PPointers(Items)^[i])>0) then begin
       Temp:=PPointers(Items)^[i];
       repeat
        PPointers(Items)^[j+1]:=PPointers(Items)^[j];
        dec(j);
       until not ((j>=Left) and (CompareFunc(PPointers(Items)^[j],Temp)>0));
       PPointers(Items)^[j+1]:=Temp;
      end;
     end;
    end else begin
     Middle:=Left+((Right-Left) shr 1);
     if Left<Middle then begin
      ChildJobs[0]:=Acquire(ParallelIndirectMergeSortJobFunction,nil);
      ChildJobData:=PPasMPParallelIndirectMergeSortJobData(pointer(@ChildJobs[0]^.Data));
      ChildJobData^.Data:=Data;
      ChildJobData^.Left:=Left;
      ChildJobData^.Right:=Middle-1;
      ChildJobData^.Depth:=JobData^.Depth-1;
     end else begin
      ChildJobs[0]:=nil;
     end;
     if Middle<=Right then begin
      ChildJobs[1]:=Acquire(ParallelIndirectMergeSortJobFunction,nil);
      ChildJobData:=PPasMPParallelIndirectMergeSortJobData(pointer(@ChildJobs[1]^.Data));
      ChildJobData^.Data:=JobData^.Data;
      ChildJobData^.Left:=Middle;
      ChildJobData^.Right:=Right;
      ChildJobData^.Depth:=JobData^.Depth-1;
     end else begin
      ChildJobs[1]:=nil;
     end;
     Invoke(ChildJobs);
     begin
      // Merge
      Temp:=Data^.Temp;
      iA:=Left;
      iB:=Middle;
      iC:=Left;
      while (iA<Middle) and
            (CompareFunc(PPointers(Items)^[iA],PPointers(Items)^[iB])<=0) do begin
       inc(iA);
      end;
      if iA<Middle then begin
       Left:=iA;
       iC:=iA;
       PPointers(Temp)^[iC]:=PPointers(Items)^[iB];
       inc(iB);
       inc(iC);
       while (iA<Middle) and (iB<=Right) do begin
        if CompareFunc(PPointers(Items)^[iA],PPointers(Items)^[iB])>0 then begin
         PPointers(Temp)^[iC]:=PPointers(Items)^[iB];
         inc(iB);
        end else begin
         PPointers(Temp)^[iC]:=PPointers(Items)^[iA];
         inc(iA);
        end;
        inc(iC);
       end;
       if iA<Middle then begin
        Count:=Middle-iA;
        Move(PPointers(Items)^[iA],PPointers(Temp)^[iC],Count*SizeOf(pointer));
        inc(iC,Count);
       end;
       if iB<=Right then begin
        Count:=(Right-iB)+1;
        Move(PPointers(Items)^[iB],PPointers(Temp)^[iC],Count*SizeOf(pointer));
       end;
       Move(PPointers(Temp)^[Left],PPointers(Items)^[Left],((Right-Left)+1)*SizeOf(pointer));
      end;
     end;
    end;
   end;
  end;
 end;
end;

type PPasMPParallelIndirectMergeSortRootJobData=^TPasMPParallelIndirectMergeSortRootJobData;
     TPasMPParallelIndirectMergeSortRootJobData=record
      Items:pointer;
      Left:longint;
      Right:longint;
      Depth:longint;
      Granularity:longint;
      CompareFunc:TPasMPParallelSortCompareFunction;
     end;

procedure TPasMP.ParallelIndirectMergeSortRootJobFunction(const Job:PPasMPJob;const ThreadIndex:longint);
var Data:TPasMPParallelIndirectMergeSortData;
    JobData:PPasMPParallelIndirectMergeSortRootJobData;
    ChildJobData:PPasMPParallelIndirectMergeSortJobData;
    ChildJob:PPasMPJob;
begin
 JobData:=PPasMPParallelIndirectMergeSortRootJobData(pointer(@Job^.Data));
 GetMem(Data.Temp,((JobData^.Right-JobData^.Left)+1)*SizeOf(pointer));
 try
  Data.Items:=JobData^.Items;
  Data.Granularity:=JobData^.Granularity;
  Data.CompareFunc:=JobData^.CompareFunc;
  ChildJob:=Acquire(ParallelIndirectMergeSortJobFunction,nil);
  ChildJobData:=PPasMPParallelIndirectMergeSortJobData(pointer(@ChildJob^.Data));
  ChildJobData^.Data:=@Data;
  ChildJobData^.Left:=JobData^.Left;
  ChildJobData^.Right:=JobData^.Right;
  ChildJobData^.Depth:=JobData^.Depth;
  Invoke(ChildJob);
 finally
  FreeMem(Data.Temp);
 end;
end;

function TPasMP.ParallelIndirectMergeSort(const Items:pointer;const Left,Right:longint;const CompareFunc:TPasMPParallelSortCompareFunction;const Granularity:longint=16;const Depth:longint=PasMPDefaultDepth;const ParentJob:PPasMPJob=nil):PPasMPJob;
var JobData:PPasMPParallelIndirectMergeSortRootJobData;
begin
 if (Left+1)<Right then begin
  result:=Acquire(ParallelIndirectMergeSortRootJobFunction,nil,ParentJob);
  JobData:=PPasMPParallelIndirectMergeSortRootJobData(pointer(@result^.Data));
  JobData^.Items:=Items;
  JobData^.Left:=Left;
  JobData^.Right:=Right;
  JobData^.Granularity:=Granularity;
  JobData^.CompareFunc:=CompareFunc;
  if Left<Right then begin
   JobData^.Depth:=IntLog2((Right-Left)+1);
   if JobData^.Depth>Depth then begin
    JobData^.Depth:=Depth;
   end;
  end else begin
   JobData^.Depth:=0;
  end;
 end else begin
  result:=nil;
 end;
end;

initialization
{$ifdef UseThreadLocalStorage}
{$if defined(UseThreadLocalStorageX8632) or defined(UseThreadLocalStorageX8664)}
 CurrentJobWorkerThreadTLSIndex:=TLSAlloc;
 CurrentJobWorkerThreadTLSOffset:={$if defined(UseThreadLocalStorageX8632)}$e10+(CurrentJobWorkerThreadTLSIndex*4){$else}$1480+(CurrentJobWorkerThreadTLSIndex*8){$ifend};
{$ifend}
{$endif}
 GlobalPasMP:=nil;
 GlobalPasMPCriticalSection:=TPasMPCriticalSection.Create;
finalization
 if assigned(GlobalPasMP) then begin
  TPasMP.DestroyGlobalInstance;
 end;
 GlobalPasMPCriticalSection.Free;
end.
