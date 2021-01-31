#!/bin/bash

BLACK=`echo -en '\e[2;30m'`
RED=`echo -en '\e[2;31m'`
GREEN=`echo -en '\e[2;32m'`
YELLOW=`echo -en '\e[2;33m'`
BLUE=`echo -en '\e[2;34m'`
MAGENTA=`echo -en '\e[2;35m'`
CYAN=`echo -en '\e[2;36m'`
WHITE=`echo -en '\e[2;37m'`
DEFAULT=`echo -en '\e[0;39m'`

# Some cleanup
rm -rf blob/*.bin *.7z *.bin *.h *.lst *.p

# First of all, we assemble the binary
if [[ "$OS" -eq "Windows_NT" ]]; then
	AS=./asw.exe
	P2BIN=./p2bin.exe
	EXEEXT=.exe
else
	AS=asl
	P2BIN=p2bin
	EXEEXT=
fi
$AS -xx -c -E -q -L -r 2 -A -U -i src blob.asm

if [[ -f blob.log ]]; then
	# There were errors or warnings when building the ROM. Print message and
	# print a filtered version of the log file.
	echo '
*****************************************************
*                                                   *
*   There were errors/warnings when building ROM.   *
*                                                   *
*****************************************************' | cat - blob.log	\
	  | sed -r "s/^(\*.*\*)$/${RED}\1${DEFAULT}/g;
		        s/> > >([A-Za-z0-9_ '\"]+\.asm)\(([0-9]+)\)(.*):/${GREEN}>>>${MAGENTA}\1${CYAN}:${GREEN}\2${CYAN}\3:${DEFAULT}/g;
		        :repeat;
		            s/\x1B\[2;36m\s+([A-Za-z0-9_ '\"]+)\(([0-9]+)\)(.*):\x1B\[0;39m/${CYAN}:${MAGENTA}\1${CYAN}:${GREEN}\2${CYAN}\3:${DEFAULT}/g;
		        t repeat;
		        s/> > >/${GREEN}>>>${DEFAULT}/g;
		        s/\berrors?\b:/${RED}Error${CYAN}:${DEFAULT}/gI;
		        s/\bwarnings?\b:/${YELLOW}Warning${CYAN}:${DEFAULT}/gI"	\
	  | less -R
	exit 1
fi

$P2BIN blob.p &> /dev/null

# There are 12 bytes in jmp instructions at the start that we need to skip
JumpSkip=12

# Several symbols we must look for:
HackerName="0x$(egrep -o '\bHackerName\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
BusErrorMsg="0x$(egrep -o '\bBusErrorMsg\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
BusError="0x$(egrep -o '\bBusError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
AddressError="0x$(egrep -o '\bAddressError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
TraceError="0x$(egrep -o '\bTraceError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
SpuriousException="0x$(egrep -o '\bSpuriousException\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
ZeroDivideError="0x$(egrep -o '\bZeroDivideError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
CHKExceptionError="0x$(egrep -o '\bCHKExceptionError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
TRAPVError="0x$(egrep -o '\bTRAPVError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
IllegalInstrError="0x$(egrep -o '\bIllegalInstrError\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
PrivilegeViolation="0x$(egrep -o '\bPrivilegeViolation\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
LineAEmulation="0x$(egrep -o '\bLineAEmulation\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
LineFEmulation="0x$(egrep -o '\bLineFEmulation\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"
TrapVector="0x$(egrep -o '\bTrapVector\s+:\s*[A-Fa-f0-9]+' blob.lst | egrep -o '\b[A-Fa-f0-9]+\b')"

# Address of first instruction after "move.w	#Revision,d0"
post_revision="0x$(egrep -o '\b[A-Fa-f0-9]+\s+:\s*[A-Fa-f0-9]+\s*[A-Fa-f0-9]+\s+move\.w\s+#Revision,d0' blob.lst | egrep -o '^[0-9A-Fa-f]+')"

# Last address in listing:
padding_start="0x$(egrep -o '/\s+[A-Fa-f0-9]+\s*:' blob.lst | tail -n 1 | egrep -o '[A-Fa-f0-9]+')"

# Now that we have every address we need, it is time to start slicing the built
# binary into several chunks.
dd ibs=1 skip=$((JumpSkip          )) count=$((HackerName-JumpSkip                 )) if=blob.bin of=blob/Part1.bin status=none
dd ibs=1 skip=$((BusErrorMsg       )) count=$((BusError-BusErrorMsg                )) if=blob.bin of=blob/Part2.bin status=none
dd ibs=1 skip=$((BusError          )) count=$((AddressError-BusError               )) if=blob.bin of=blob/Part3.bin status=none
dd ibs=1 skip=$((AddressError      )) count=$((TraceError-AddressError             )) if=blob.bin of=blob/Part4.bin status=none
dd ibs=1 skip=$((TraceError        )) count=$((SpuriousException-TraceError        )) if=blob.bin of=blob/Part5.bin status=none
dd ibs=1 skip=$((SpuriousException )) count=$((ZeroDivideError-SpuriousException   )) if=blob.bin of=blob/Part6.bin status=none
dd ibs=1 skip=$((ZeroDivideError   )) count=$((CHKExceptionError-ZeroDivideError   )) if=blob.bin of=blob/Part7.bin status=none
dd ibs=1 skip=$((CHKExceptionError )) count=$((TRAPVError-CHKExceptionError        )) if=blob.bin of=blob/Part8.bin status=none
dd ibs=1 skip=$((TRAPVError        )) count=$((IllegalInstrError-TRAPVError        )) if=blob.bin of=blob/Part9.bin status=none
dd ibs=1 skip=$((IllegalInstrError )) count=$((PrivilegeViolation-IllegalInstrError)) if=blob.bin of=blob/PartA.bin status=none
dd ibs=1 skip=$((PrivilegeViolation)) count=$((LineAEmulation-PrivilegeViolation   )) if=blob.bin of=blob/PartB.bin status=none
dd ibs=1 skip=$((LineAEmulation    )) count=$((LineFEmulation-LineAEmulation       )) if=blob.bin of=blob/PartC.bin status=none
dd ibs=1 skip=$((LineFEmulation    )) count=$((TrapVector-LineFEmulation           )) if=blob.bin of=blob/PartD.bin status=none
dd ibs=1 skip=$((TrapVector        )) count=$((post_revision+2-TrapVector          )) if=blob.bin of=blob/PartE.bin status=none
dd ibs=1 skip=$((post_revision+4   )) count=$((padding_start-(post_revision+4)     )) if=blob.bin of=blob/PartF.bin status=none

find . -iname '*~' -delete
zipname="`pwd`/Debugger.7z"
distfiles="Debugger.asm DebuggerMap.bin Disassembler.asm skcompat.asm Terminal.asm TerminalFont.bin TerminalPal.bin"
( cd src  ; echo -n "Generating debugger assembly archive... " && (7z u -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on $zipname $distfiles 1> /dev/null && echo "done.") || echo "failed" )
distfiles="DebuggerBlob.asm `ls blob/*.bin | sed 's%blob/%%g'`"
zipname="`pwd`/DebuggerBlob.7z"
( cd blob ; echo -n "Generating debugger assembly archive... " && (7z u -t7z -m0=lzma -mx=9 -mfb=64 -md=32m -ms=on $zipname $distfiles 1> /dev/null && echo "done.") || echo "failed" )

