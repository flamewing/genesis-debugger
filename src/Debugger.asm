; ===========================================================================
; Copyright (C) 2011-2018 by flamewing
;
; Permission to use, copy, modify, and/or distribute this software for any
; purpose with or without fee is hereby granted.
;
; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT
; OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
; ===========================================================================
; Use this to enable/disable the debugger:
EnableDebugger = 1						; 1 = enabled, 0 = disabled
; ===========================================================================
; Use this to identify the revision of your hack:
	ifndef Revision
		equ Revision,1
	endif
; ===========================================================================
; This should be the path to "Disassembler.asm":
  if EnableDebugger
	include "Disassembler.asm"
; ===========================================================================
; Convenience macros, for increased maintainability of the code.
	ifndef intMacros_defined
intMacros_defined = 1
enableInts macro
	move	#$2300,sr
	endm

disableInts macro
	move	#$2700,sr
	endm
	endif
; ===========================================================================
; Make sure this points to the correct location.
; ---------------------------------------------------------------------------
; Enigma compressed mappings
MapEng_Debugger:	BINCLUDE	"DebuggerMap.bin"
	even
; ===========================================================================
; Edit these to your name and e-mail. If the e-mail is empty (""), then nothing
; will be printed on the line. You can use all characters in the ASCII character
; set (see https://en.wikipedia.org/wiki/ASCII#ASCII_printable_characters).
HackerName:	vtstring WHITE,"Your name  "
EMailmsg:	vtstring BLUE ,"your.email@server.domain         "
; ===========================================================================
; NO USER SERVICEABLE PARTS BELOW THIS POINT
; ===========================================================================
; All 16 registers are saved on stack, so we need to skip 16*4 bytes to reach
; the exception data.
except_off = 16*4
; Size of stack frame of group 0 exceptions except for reset.
group0_sz  = 14
; Offset for saved SR in group 0 stack frame.
group0_sr  = 8
; Size of stack frame for group 1 and 2 exceptions.
group12_sz = 6
; ===========================================================================
; System strings, do not edit.
BusErrorMsg:			vtstring BLUE ,"   A bus error has occurred  "
AddressErrorMsg:		vtstring BLUE ,"An address error has occurred"
IllegalInstrMsg:		vtstring BLUE ,"Illegal instruction was found"
ZeroDivideMsg:			vtstring BLUE ," A division by zero happened "
CHKExceptionMsg:		vtstring BLUE ,"   Unhandled chk exception   "
TRAPVErrMsg:			vtstring BLUE ,"  Unhandled integer overflow "
PrivilegeViolationMsg:	vtstring BLUE ," Privilege violation happened"
TraceMsg:				vtstring BLUE ,"   Trace exception occurred  "
LineAEmulationMsg:		vtstring BLUE ,"Line 1010 emulation triggered"
LineFEmulationMsg:		vtstring BLUE ,"Line 1111 emulation triggered"
SpuriousExceptionMsg:	vtstring BLUE ,"A spurious exception happened"
TrapVectorMsg:			vtstring BLUE ," Unhandled trap was triggered"
AtAddressmsg:			vtstring BLUE ," executing address "
Atmsg:					vtstring WHITE,"at "
ReadFrommsg:			vtstring BLUE ,"   on a read from "
WriteTomsg:				vtstring BLUE ,"    on a write to "
UnrecovDatamsg:			vtstring RED  ,"\x7F Unavailable information for disasm \x7F"
UnrecovAddrmsg:			vtstring RED  ,"\x7F  Unrecoverable address for disasm  \x7F"
UnknownAddrmsg:			vtstring RED  ,"\x7F  Unknown address; candidates are:  \x7F"
UnknownAddr:			vtstring RED  ,"$????????:"
ErrorDataMsg:			vtstring BLUE ,"\x7F\x7F\x7F Error data \x7F\x7F\x7F"
ErrorFlags:				vtstring GREEN,"Flags: "
AccessAddress:			vtstring GREEN,"Access address:"
IRMessage:				vtstring GREEN,"ir:"
; ===========================================================================
; Macro responsible for managing the 'header' common to all error handling
; routines: disables interrupts, saves all registers, loads the message given
; as parameter, calls the common initialization code and sets (if needed) the
; instruction maks for finding the instruction.
InitErrorHandler macro framesz,errmsg,rewindflag,rewindfun
	disableInts							; Disable interrupts
	pea	framesz(sp)						; Save SP before exception
	movem.l	d0-a6,-(sp)					; Save all other registers; we will need them
	lea	errmsg(pc),a1
	bsr.w	CommonErrorInit
	if "rewindflag"<>""
		moveq	#rewindflag,d1
	endif
	if "rewindfun"<>""
		lea	rewindfun(pc),a4
	endif
	move.l	sp,usp						; Save current stack pointer to usp
	endm
; ===========================================================================
BusError:
	InitErrorHandler group0_sz,BusErrorMsg
	bra.w	BusAddressError_Handler
; ===========================================================================
AddressError:
	InitErrorHandler group0_sz,AddressErrorMsg
	bra.w	BusAddressError_Handler
; ===========================================================================
TraceError:
	; This tecnically should rewind to the previously executed instruction for
	; the tracing; but without the IR, there is no hope for that. I have no idea
	; how to properly handle this case.
	InitErrorHandler group12_sz,TraceMsg,0
	bra.w	Exception_Handler
; ===========================================================================
SpuriousException:
	; I have absolutely no idea how to handle this case.
	InitErrorHandler group12_sz,SpuriousExceptionMsg,0
	bra.w	Exception_Handler
; ===========================================================================
ZeroDivideError:
	InitErrorHandler group12_sz,ZeroDivideMsg,-1,ChkDiv
	bra.w	Exception_Handler
; ===========================================================================
CHKExceptionError:
	InitErrorHandler group12_sz,CHKExceptionMsg,-1,ChkChk
	bra.w	Exception_Handler
; ===========================================================================
TRAPVError:
	InitErrorHandler group12_sz,TRAPVErrMsg,-1,ChkTrapV
	bra.w	Exception_Handler
; ===========================================================================
IllegalInstrError:
	InitErrorHandler group12_sz,IllegalInstrMsg,0
	bra.w	Exception_Handler
; ===========================================================================
PrivilegeViolation:
	InitErrorHandler group12_sz,PrivilegeViolationMsg,0
	bra.w	Exception_Handler
; ===========================================================================
LineAEmulation:
	InitErrorHandler group12_sz,LineAEmulationMsg,0
	bra.w	Exception_Handler
; ===========================================================================
LineFEmulation:
	InitErrorHandler group12_sz,LineFEmulationMsg,0
	bra.w	Exception_Handler
; ===========================================================================
TrapVector:
	InitErrorHandler group12_sz,TrapVectorMsg,-1,ChkTrap
	bra.w	Exception_Handler
; ===========================================================================
; Do initial setup of screen so that the system is in a known state.
; Input:
; 	a1	Error message to print
CommonErrorInit:
	move.l	a1,-(sp)
	bsr.w	InitTerminal

	; Decode enigma-compressed plane mappings to RAM buffer.
	lea	MapEng_Debugger(pc),a0
	lea	(Chunk_Table).l,a1
	move.w	#make_art_tile(ArtTile_VRAM_Start,0,0),d0
	ifdef debugger_blob
	jsr	EniDec(pc)
	else
	jsr	(EniDec).w
	endif

	; Print the type of error.
	movea.l	(sp)+,a1
	setcursor	5, 0
	bsr.w	Print_Message

	; Print out hacker name.
	lea	HackerName(pc),a1
	setcursor	28, 3
	bsr.w	Print_Message

	; Print e-mail and "at" text, if required.
	move.w	EMailmsg(pc),d0				; Is e-mail string empty?
	beq.s	.skip_email					; Branch if yes
	; Print the "at" message, then the e-mail.
	setcursor	1, 4
	lea Atmsg(pc),a1
	bsr.w	Print_Message
	lea	EMailmsg(pc),a1
	bsr.w	Print_Message

.skip_email:
	; Print hack revision.
	setcursor	9, 15
	move.w	#Revision,d0
	bsr.w	Print_Word

	; Print the stack pointer -- but adjust it to skip all registers we added.
	setcursor	30, 15
	move.l	sp,d0
	addi.l	#15*4+4,d0					; 15 registers + return address
	bsr.w	Print_Long

	; Print the user stack pointer.
	setcursor	30, 14
	move.l	usp,a0						; Get usp
	move.l	a0,d0
	bsr.w	Print_Long

	; Print the registers as they were at the start of the exception handler.
	lea	4(sp),a0						; Skip return address
	setcursor	4, 17,a2
	moveq	#4,d3						; 5 lines of registers

	; Dump all registers.
.reg_row_loop:
	moveq	#2,d4						; 3 columns of registers per line
	seekset	a2							; Simplifies line wrapping

.reg_col_loop:
	move.l	(a0)+,d0
	bsr.w	Print_Long
	seekp.w	4							; Advance to next column
	dbra	d4,.reg_col_loop
	seekp.w	nCols,a2					; Advance to next line
	dbra	d3,.reg_row_loop

	rts
; ===========================================================================
BusAddressError_Handler:
	; a0 = pointer to exception data.
	move.l	usp,a0
	adda.w	#except_off,a0				; 16 registers

	; First, the interrupt flags.
	move.w	(a0)+,d0					; Get interrupt flags
	; Print the "Flags: " text
	lea	ErrorFlags(pc),a1
	setcursor	1, 13
	bsr.w	Print_Message
	tellp.l	a2							; Destination for the flags
	; Print information on whether or not the exception happened during a read
	; or during a write. An exception can happen on a read if executing non-
	; aligned code.
	move.w	#'W'|RED, d1
	lea	WriteTomsg(pc),a1
	btst	#4,d0						; Did the error happen during a read?
	beq.s	.print_rw					; Branch if not
	; On a read, must also display the 'read from' bit.
	move.w	#'R'|RED, d1
	lea	ReadFrommsg(pc),a1

.print_rw:
	setcursor	5, 1
	bsr.w	Print_Message
	seekset.l	a2
	vtput.w	d1

	; Whether or not the exception was on a valid instruction.
	move.w	#'I'|RED, d1
	btst	#3,d0						; Is it a valid instruction?
	beq.s	.print_in					; Branch if yes
	move.w	#'N'|RED, d1

.print_in:
	vtput.w	d1

	moveq	#2,d2

	; Function codes: FC? low/high. See user manual.
.fc_code_loop:
	move.w	#'L'|RED, d1
	btst	d2,d0						; Is FC low?
	beq.s	.print_hl					; Branch if yes
	move.w	#'H'|RED, d1

.print_hl:
	vtput.w	d1
	dbra	d2,.fc_code_loop

	setcursor	11, 12
	lea		ErrorDataMsg(pc),a1
	bsr.w	Print_Message

	; Print the misaligned address that caused the exception.
	move.l	(a0)+,d0					; Read access address
	setcursor	23, 1
	bsr.w	Print_Address
	vtputc	"!"
	; Print the "Access address:" text
	lea	AccessAddress(pc),a1
	setcursor	17, 13
	bsr.w	Print_Message
	bsr.w	Print_Address

	; Print the instruction register (IR)
	move.w	(a0)+,d0					; Read IR to d0
	move.w	d0,d5						; Save IR for later
	; Print the "IR:" text
	lea	IRMessage(pc),a1
	setcursor	15, 15
	bsr.w	Print_Message
	; Instruction Register.
	bsr.w	Print_Word

	; Print the status register (SR).
	move.w	(a0)+,d0					; Read SR to d0
	; Status Register.
	setcursor	4, 14
	bsr.w	Print_Word

	; Print location of the next instruction to be executed -- Program Counter (PC).
	move.l	(a0)+,d0					; Read PC
	movea.l	d0,a3						; Save it for later
	setcursor	18, 14
	bsr.w	Print_Address

	; Instruction disassembly. Needs the raw instruction in d5 and PC as given
	; by the exception data: either for the next opcode to execute or, for move
	; instructions, it may be right before the destination extension word if the
	; error happened when reading the source.
	setcursor	DisassemblyAlign, 9
	lea	RewindBusAddressError(pc),a4
	pea	.chk_disasm(pc)					; Makes SetFullRecovery return to this.
	pea	SetFullRecovery(pc)				; Makes Disassemble_Opcode return to this
	bra.w	Disassemble_Opcode			; Will eventually return to the following line

.chk_disasm:
	bne.s	DisassemblyFailed			; Return is nonzero if information was lost
	; Disassembly was complete.
	; Print the address of the instruction.
	move.l	a3,d0						; Copy PC; this points to the word after what caused the error
	subq.l	#2,d0						; All instruction opcodes are 2 bytes
	setcursor	1, 8
	bsr.w	Print_Address
	vtputc	":"
	bra.s	PrepareStackTrace
; ---------------------------------------------------------------------------
DisassemblyFailed:
	; Missing information led to incomplete disassembly.
	; This happens for Bcc (except BSR), JMP (but not JSR) and DBcc when cc is
	; false or when cc true, but the register was not -1 (i.e., loop not done).
	; The Bcc and DBcc cases can't trigger address errors, and the Genesis does
	; not ever generate bus errors; so this will usually only happen for JMP.
	setcursor	1, 11
	lea	UnrecovDatamsg(pc),a1
	bmi.s	.chk_v_flag					; lea does not affect condition codes
	; We could recover all but instruction address.
	lea	UnrecovAddrmsg(pc),a1

.chk_v_flag:
	bvc.s	.print_miss_msg				; lea does not affect condition codes
	setcursor	1, 10
	lea	UnknownAddrmsg(pc),a1

.print_miss_msg:
	bsr.w	Print_Message
	lea	UnknownAddr(pc),a1
	setcursor	1, 8
	bsr.w	Print_Message

PrepareStackTrace:
	; Print stack dump -- starting AFTER all exception data.
	moveq	#group0_sz,d1				; Size of address/bus error stack frame
	bra.w	FinishErrorDump
; ===========================================================================
Exception_Handler:
	; a0 = pointer to exception data.
	move.l	usp,a0
	adda.w	#except_off,a0				; 16 registers

	lea	AtAddressmsg(pc),a1
	setcursor	5, 1
	bsr.w	Print_Message

	setcursor	11, 13
	lea		ErrorDataMsg(pc),a1
	bsr.w	Print_Message

	; Print the status register (SR).
	move.w	(a0)+,d0					; Read SR to d0
	; Status Register.
	setcursor	4, 14
	bsr.w	Print_Word

	; Get location of the instruction -- Program Counter (PC).
	movea.l	(a0)+,a3					; Read PC

	tst.w	d1							; Do we need to scan for the instruction?
	beq.s	.found_instr				; Branch if not
	moveq	#0,d2						; Number of bytes found in extension words

.prev_word:
	move.w	-(a3),d5					; Get previous word
	jsr	(a4)							; Is this the instruction?
	beq.s	.found_instr				; Branch if yes
	addq.w	#2,d2						; Two more bytes
	bra.s	.prev_word					; Loop

.found_instr:
	move.l	a3,d0						; Print PC
	setcursor	17, 14
	bsr.w	Print_Address
	setcursor	24, 1
	bsr.w	Print_Address
	vtputc	"!"
	setcursor	1, 8
	bsr.w	Print_Address
	vtputc	":"

	; Instruction disassembly. Needs the raw instruction in d5 and PC after the
	; opcode in a3.
	move.w	(a3)+,d5					; Read instruction
	setcursor	DisassemblyAlign, 9
	lea	RewindStub(pc),a4
	bsr.w	Disassemble_Opcode
	moveq	#group12_sz,d1				; Size of group 1 & 2 exception frame
	; Fall through.
	;bra.w	FinishErrorDump
; ===========================================================================
; SUBROUTINE
; Prints stack dump, maps the buffer onto VRAM and locksthe machine.
; Input:
;	d1	How many bytes to skip after the registers on the stack.
FinishErrorDump:
	move.l	usp,a0
	lea except_off(a0,d1.w),a0			; Pointer to data to dump.
	setcursor	0, 23,a2
	move.w	a0,d3
	subi.w	#System_Stack,d3			; d3 = -1 * number of bytes to print
	bpl.s	NoStackTrace				; Branch if positive or zero (stack underflow or empty)
	neg.w	d3
	cmpi.w	#$50,d3						; Limit to $50 bytes, as this is all space we have.
	blo.s	.have_byte_count
	moveq	#$50,d3

.have_byte_count:
	lsr.w	#1,d3						; Convert to words.
	move.w	d3,d5
	addq.w	#7,d3						; Prepare to round up number of lines.
	lsr.w	#3,d3						; Convert to lines of stack dump.
	subq.w	#1,d3						; Turn into loop index.
	andi.w	#7,d5						; Number of words we have to print on last line.
	bne.s	.have_last_row_cnt			; Branch if nonzero
	moveq	#8,d5						; Otherwise, we need to print an entire line

.have_last_row_cnt:
	swap	d1							; Save exception size
	lsr.w	#1,d5						; Convert to number of longs...
	scs.b	d1							; ... but set d1 to true if we have a last word.
	swap	d1							; Save "last word" flag and restore exception size
	subq.w	#1,d5						; Convert this to a loop index too

	; Stack dump loop.
.stack_row_loop
	moveq	#3,d4						; 4 columns of stack dump
	tst.w	d3							; Do we have more than one line left?
	bne.s	.row_not_empty				; Branch if yes
	move.w	d5,d4						; Number of longwords for last line
	bmi.s	StackDump_Done				; If this line is empty, we are done

.row_not_empty:
	seekset	a2							; Simplifies line wrapping
	vtputc	"+"
	exg.l	d0,d1
	bsr.w	HexDump_Byte
	exg.l	d0,d1
	addi.w	#$10,d1
	vtputc	" "

.stack_col_loop:
	move.l	(a0)+,d0
	bsr.w	HexDump_Long
	seekp.w	1							; Advance to next column
	dbra	d4,.stack_col_loop
	seekp.w	nCols,a2					; Advance to next line
	dbra	d3,.stack_row_loop

StackDump_Done:
	; If we have a last word (as opposed to last long), we need to delete the
	; last two characters printed.
	swap	d1							; Get "last word" flag.
	tst.b	d1
	beq.s	.no_clr_word
	seekp.w	-5							; Roll back 4 hex digits and one space
	vtputs	"    "						; Clear these digits.
.no_clr_word:

NoStackTrace:
	bsr.w	DrawTerminal

	startZ80

	movem.l	(sp)+,d0-a6					; Restore all registers
	addq.w	#4,sp						; Pop off the saved SP as well

	; SP is now back to the same point it was at the start of the exception.

	enableInts							; Enable interrupts

	; Exception loop.
.error_trap:
	nop
	nop
	bra.s	.error_trap
; ===========================================================================
RewindStub:
	rts
; ===========================================================================
; Adjusts a3 to be just after the instruction in d5 if possible. This is not
; possible for some dbCC cases, for jmp and for bCC other than bsr; in these
; cases, finishes the disassembly and the sets condition codes as appropriate
; and DOES NOT RETURN TO CALLER in this case.
; Input:
; 	usp	Pointer to saved registers (d0-a6,sr)
; 	a3	For normal, forward disassembly, a3 is PC after the opcode, but before
; 	  	any the extension words, if any; in this case, the function at (a4) must
; 	  	be a no-nop.
; 	  	For rewinding disassembly (address and bus errors), a3 is PC for either
; 	  	the next instruction to execute or, for move instructions only, it may
; 	  	be right before the destination extension word if the error happened
; 	  	when reading from the source. In this case, the function at (a4) should
; 	  	handle rewinding to just after the opcode for all instructions, that is,
; 	  	to the same location that would be in a3 in a forward disassembly.
; 	a5	Where the output text goes
; 	d1	Effective address bits
; 	d2	For bCC and dbCC instructions, 4 * the condition code from IR. It is not
; 	  	used for other instructions.
; 	d3	Instruction size (%00 = byte, %01 = word, %10 = long, all others = invalid)
; 	d5	Instruction register
; Output:
; 	a3	Program counter of instruction disassembled + 2 (before extension words)
; 	a5	Cell just after last printed text
; 	If the disassembly was successful and recovered all information, then zero
; 	flag is set; if information was lost, zero flag is cleared.
; 	If the only information lost was the PC for the opcode, then negative flag
; 	if cleared; if more information than that was lost than that, this flag is
; 	set instead.
; 	The overflow flag is set if and only if the zero flag is clear AND if it was
; 	possible to infer the address of the instruction, but there were multiple
; 	possible sources.
RewindBusAddressError:
	move.w	d5,d0
	andi.w	#$FFC0,d0
	cmpi.w	#$4E80,d0					; Is it a jsr?
	beq.s	.got_jsr					; Branch if yes
	cmpi.w	#$4EC0,d0					; Is it a jmp?
	beq.w	.got_jmp					; Branch if yes
	move.w	d5,d0
	andi.w	#$F0F8,d0
	cmpi.w	#$50C8,d0					; Was this a dbCC?
	beq.s	.got_dbCC					; Branch if yes
	andi.w	#$F000,d0
	cmpi.w	#$6000,d0					; Was this a bCC?
	beq.s	.got_bCC					; Branch if yes
	; Filter out move instructions, as they need a bit more care.
	rol.w	#4,d0
	beq.s	.not_move					; Not a move
	subq.b	#4,d0
	bmi.s	.got_move					; Branch if a move

.not_move:
	suba.w	d4,a3

.end_normal:
	bsr.w	Calc_effective_address_size
	suba.w	d4,a3
	rts
; ---------------------------------------------------------------------------
.got_jsr:
	move.l	usp,a0
	movea.l	except_off+group0_sz(a0),a3	; a3 = pointer to jsr return target.
	moveq	#2,d3						; Pretend long immediate, even though invalid
	bsr.w	Calc_effective_address_size
	suba.w	d4,a3						; Adjust address for first parameter
	rts
; ---------------------------------------------------------------------------
.got_move:
	exg.l	d5,d1
	bsr.w	Calc_effective_address_size
	suba.w	d4,a3						; Adjust address for first parameter
	exg.l	d5,d1
	move.l	usp,a0
	move.w	except_off(a0),d0			; Address error flags
	btst	#4,d0						; Did the error happen during a read?
	beq.s	.end_normal					; Branch if not
	rts
; ---------------------------------------------------------------------------
.got_dbCC:
	move.l	usp,a0
	move.w	except_off+group0_sr(a0),sr	; Fetch value of sr at the time of the exception.
	; If the condition was true, we can recover everything; however, this case
	; never happens for address errors. It is handled here for completness, as
	; other systems than the Genesis may generate bus errors and it will, thus,
	; become relevant.
	bsr.w	Check_Condition				; Was the condition true?
	bne.s	.dbCC_complete				; Branch if yes (can recover everything).
	; Get the counter value; if it is -1, then we still can recover everything,
	; with the same caveats for address errors as above.
	move.w	d5,d0
	andi.w	#7,d0
	lsl.w	#4,d0
	move.l	(a0,d0.w),d0
	; So was the loop counter -1?
	cmpi.w	#-1,d0
	beq.s	.dbCC_complete				; Branch if yes.
	bra.s	.bCC_dbCC_target_only
; ---------------------------------------------------------------------------
.got_bCC:
	move.b	d5,d3						; Get displacement
	beq.s	.not_short_branch			; Branch if not a short branch
	; We can always recover everything for a short branch.
	ext.w	d3
	neg.w	d3
	lea	(a3,d3.w),a3
	rts
; ---------------------------------------------------------------------------
.not_short_branch:
	tst.w	d2
	beq.s	.bCC_dbCC_target_only		; This is a bra, unfortunatelly.
	cmpi.b	#4,d2						; Is this a bsr?
	beq.s	.got_bsr					; Branch if yes.
	; This is a generic bCC.
	move.l	usp,a0
	move.w	except_off+group0_sr(a0),sr	; Fetch value of sr at the time of the exception.
	; If the condition was false, we can recover everything; however, this case
	; never happens for address errors. It is handled here for completness, as
	; other systems than the Genesis may generate bus errors and it will, thus,
	; become relevant.
	bsr.w	Check_Condition				; Was the condition true?
	beq.s	.bCC_complete				; Branch if not (can recover everything).

.bCC_dbCC_target_only:
	; Last ditch effort: try searching for the branch.
	bsr.w	WordBranch_ScanSource
	beq.s	.not_found					; Branch if not found (this should be impossible)
	bvc.s	.done						; Branch if only one match was found

	addq.w	#8,sp						; Don't return to caller, nor to its caller
	pea	SetInferredRecovery(pc)			; Replace "new" return with this
	bra.s	.print_branch_addr
; ---------------------------------------------------------------------------
.not_found:
	; Flag as intruction's address being lost
	addq.w	#8,sp						; Don't return to caller, nor to its caller
	pea	SetLostAddress(pc)				; Replace "new" return with this

.print_branch_addr:
	; a3 points to the target of the branch, so print it
	move.l	a3,d0
	lea	(a3),a2
	bra.w	Print_Address
; ---------------------------------------------------------------------------
.got_bsr:
	move.l	usp,a0
	movea.l	except_off+group0_sz(a0),a3	; a3 = pointer to bsr return target.

.bCC_complete:
	tst.b	d5
	bne.s	.done

.dbCC_complete:
	subq.l	#2,a3

.done:
	rts
; ---------------------------------------------------------------------------
.got_jmp:
	addq.w	#8,sp						; Don't return to caller
	pea	SetLostAddress(pc)				; Replace "new" return with this
	lea	(a3),a2
	move.w	d5,d0
	move.w	d5,d1
	andi.w	#$38,d0
	lsr.w	#2,d0
	andi.w	#$7,d1
	move.w	jmp_EA_CodeMap(pc,d0.w),d0
	jmp	jmp_EA_CodeMap(pc,d0.w)
; ===========================================================================
jmp_EA_CodeMap:	offsetTable
	offsetTableEntry.w	InvalidAddrMode            	; %000
	offsetTableEntry.w	InvalidAddrMode            	; %001
	offsetTableEntry.w	Reg_Addr_Indirect          	; %010
	offsetTableEntry.w	InvalidAddrMode            	; %011
	offsetTableEntry.w	InvalidAddrMode            	; %100
	offsetTableEntry.w	jmp_Reg_Addr_Disp_Indirect 	; %101
	offsetTableEntry.w	jmp_Reg_Addr_Index_Indirect	; %110
	offsetTableEntry.w	jmp_Misc_EA_Modes          	; %111
; ===========================================================================
jmp_Reg_Addr_Disp_Indirect:
	;d16(An)
	; We can recover everything but instruction address.
	move.w	d1,d0						; d0 = register # (0-7) of An
	lsl.w	#2,d0						; d0 = number of bytes before register in its register group
	move.l	usp,a0
	move.l	$20(a0,d0.w),d0				; $20 = number of bytes before first address register; d0 = An
	sub.l	a3,d0						; d0 = -d16
	neg.l	d0							; d0 = d16
	beq.w	Reg_Addr_Indirect
	bsr.w	Print_Word_Signed
	bra.w	Reg_Addr_Indirect
; ===========================================================================
jmp_Reg_Addr_Index_Indirect:
	;d8(An,Xn.S)
	; We can recover nothing.
	addq.w	#4,sp
	pea	SetLostData(pc)					; Replace "new" return with this
	vtputs	"$??(",RED
	bsr.w	Reg_Addr_Direct
	vtputs	",Xn) \x3B ",RED			; \x3B = ';' -- this works around a bug in AS parser
	move.l	a3,d0
	bra.w	Print_Address
; ===========================================================================
jmp_Misc_EA_Modes:
	lsl.w	#1,d1
	move.w	jmp_Misc_EACodeMap(pc,d1.w),d1
	jmp	jmp_Misc_EACodeMap(pc,d1.w)
; ===========================================================================
jmp_Misc_EACodeMap:	offsetTable
	offsetTableEntry.w	jmp_Abs_Short        	; %000
	offsetTableEntry.w	jmp_Abs_Long         	; %001
	offsetTableEntry.w	jmp_PC_Disp_Indirect 	; %010
	offsetTableEntry.w	jmp_PC_Index_Indirect	; %011
	offsetTableEntry.w	InvalidAddrMode      	; %100
	offsetTableEntry.w	InvalidAddrMode      	; %101
	offsetTableEntry.w	InvalidAddrMode      	; %110
	offsetTableEntry.w	InvalidAddrMode      	; %111
; ===========================================================================
jmp_Abs_Short:
	; We can recover everything but instruction address.
	vtputc	"("
	move.l	a3,d0
	ext.l	d0
	bsr.w	Print_Address
	vtputs	").w",BLUE
	rts
; ===========================================================================
jmp_Abs_Long:
	; We can recover everything but instruction address.
	vtputc	"("
	move.l	a3,d0
	bsr.w	Print_Address
	vtputs	").l",BLUE
	rts
; ===========================================================================
jmp_PC_Disp_Indirect:
	;d16(PC)
	; We may be able to recover everything.
	bsr.w	WordBranch_ScanSource		; Try to find the source branch
	beq.s	.not_found					; Branch if not found (this should be impossible)
	bvs.s	.found_many					; Branch if found more than one
	; Victory snatched from the jaws of defeat! We have recovered
	; everything here.
	addq.w	#4,sp
	lea	(a2),a3							; Store found location to a2
	pea	SetFullRecovery(pc)				; Replace "new" return with this
	bra.w	PC_Disp_Indirect
; ---------------------------------------------------------------------------
.found_many:
	addq.w	#4,sp
	pea	SetInferredRecovery(pc)			; Replace "new" return with this

.not_found:
	move.l	a3,d0
	bsr.w	Print_Address
	vtputs	"(pc)",GREEN
	rts
; ===========================================================================
jmp_PC_Index_Indirect:
	;d8(PC,Xn.S)
	; We may be able to recover everything.
	addq.w	#4,sp
	pea	SetLostData(pc)					; Replace "new" return with this
	;move.l	a3,d0
	;bsr.w	Print_Address
	vtputs	"$??",RED
	vtputs	"(pc,",GREEN
	vtputs	",Xn) \x3B ",RED			; \x3B = ';' -- this works around a bug in AS parser
	move.l	a3,d0
	bra.w	Print_Address
; ===========================================================================
; SUBROUTINE
; Checks if the condition of a bCC or a dbCC was true of false.
; Input:
; 	d2	Condition code * 4 to test.
; Output:
; 	Zero flag set if condition was false, unset if it was true.
Check_Condition:
	lea	Check_CC(pc,d2.w),a1
	jsr	(a1)
	tst.b	d0
	rts
; ---------------------------------------------------------------------------
ChkCC macro cond
	cond	d0
	rts
	endm
; Handlers for condition checks.
Check_CC:
	ChkCC	st.b
	ChkCC	sf.b
	ChkCC	shi.b
	ChkCC	sls.b
	ChkCC	scc.b
	ChkCC	scs.b
	ChkCC	sne.b
	ChkCC	seq.b
	ChkCC	svc.b
	ChkCC	svs.b
	ChkCC	spl.b
	ChkCC	smi.b
	ChkCC	sge.b
	ChkCC	slt.b
	ChkCC	sgt.b
	ChkCC	sle.b
; ===========================================================================
; SUBROUTINE
; Validates an opcode as a divs or divl instruction.
; Input:
; 	d2	Number of bytes of extension words found so far.
; 	d5	Instruction to validate as divs/divu.
; 	Zero flag set if this is a valid divs or divl, clear otherwise.
ChkDiv:
	move.w	d5,d0
	andi.w	#$F0C0,d0					; Get the mask bits
	cmpi.w	#$80C0,d0					; Is this equal to the bits for the instruction?
	bne.s	.done						; Branch if not
	moveq	#1,d3						; Word-sized
	move.w	d5,d1
	bsr.w	Calc_effective_address_size
	cmp.w	d2,d4						; Check if the ea size matches what was found.

.done:
	rts
; ===========================================================================
; SUBROUTINE
; Validates an opcode as a chk instruction.
; Input:
; 	d2	Number of bytes of extension words found so far.
; 	d5	Instruction to validate as chk.
; 	Zero flag set if this is a valid chk, clear otherwise.
ChkChk:
	move.w	d5,d0
	andi.w	#$F040,d0					; Get the mask bits
	cmpi.w	#$4000,d0					; Is this equal to the bits for the instruction?
	bne.s	.done						; Branch if not
	moveq	#1,d3						; Word-sized
	move.w	d5,d1
	bsr.w	Calc_effective_address_size
	cmp.w	d2,d4						; Check if the ea size matches what was found.

.done:
	rts
; ===========================================================================
; SUBROUTINE
; Validates an opcode as a trapv instruction.
; Input:
; 	d2	Number of bytes of extension words found so far.
; 	d5	Instruction to validate as trapv.
; 	Zero flag set if this is a valid trapv, clear otherwise.
ChkTrapV:
	move.w	d5,d0
	andi.w	#$4E76,d0					; Get the mask bits
	cmpi.w	#$4E76,d0					; Is this equal to the bits for the instruction?
	rts
; ===========================================================================
; SUBROUTINE
; Validates an opcode as a trap instruction.
; Input:
; 	d2	Number of bytes of extension words found so far.
; 	d5	Instruction to validate as trap.
; 	Zero flag set if this is a valid trap, clear otherwise.
ChkTrap:
	move.w	d5,d0
	andi.w	#$FFF0,d0					; Get the mask bits
	cmpi.w	#$4E40,d0					; Is this equal to the bits for the instruction?
	rts
; ===========================================================================
; Computes size of affective address data
; Input:
; 	d1	Effective address bits
; 	d3	Instruction size (%00 = byte, %01 = word, %10 = long, all others = invalid)
; Output:
; 	d4	Size (in bytes) of data for effective address
Calc_effective_address_size:
	moveq	#0,d4						; Start with zero bytes
	move.w	d1,d0
	andi.w	#$38,d0
	cmpi.b	#$20,d0						; Reg (data|addr) direct, reg addr indirect, reg addr post-incr indirect, reg addr pre-decr indirect?
	ble.s	.done						; rts if so
	addq.w	#2,d4						; All others have at least one word
	cmpi.b	#$38,d0						; Misc EA modes?
	bne.s	.done						; rts if not
	move.w	d1,d0
	andi.w	#$7,d0
	cmpi.b	#$1,d0						; Abs long
	beq.s	.is_long					; Need 2 more bytes
	cmpi.b	#$4,d0						; Abs short, PC disp indirect, PC index indirect?
	blt.s	.done						; rts if so
	cmpi.b	#2,d3						; Long immed?
	blo.s	.done						; rts if not

.is_long:
	addq.w	#2,d4						; Two words

.done:
	rts
; ===========================================================================
SetFullRecovery:
	move.b	#%00100,ccr					; Zero flag
	rts
; ===========================================================================
SetInferredRecovery:
	move.b	#%00010,ccr					; Carry flag
	rts
; ===========================================================================
SetLostAddress:
	move.b	#%00000,ccr
	rts
; ===========================================================================
SetLostData:
	move.b	#%01000,ccr					; Negative flag
	rts
; ===========================================================================
; Tries to search for a word-sized branch to a given destination. If multiple
; such branches exist, up to 4 of them are printed to DisplayCell(1, 11).
; Input:
; 	a3	Target of branch
; 	a5	Write destination (in RAM)
; 	d5	Instruction register
; Output:
; 	a2	Program counter before the extension word of the branch instruction if
; 	  	only a single match was found, or to the first match found for multiple
; 	  	matches; unchanged otherwise.
; 	a3	For single matches, equal to a2; otherwise, unchanged.
; 	d5	High word changed to the extension word of the last match found.
; 	If the zero flag is set, no matches were found in the search area.
; 	If the overflow flag is set, several matches were found in the search area.
; 	If neither is set, a single match was found.
WordBranch_ScanSource:
	move.l	a3,d0						; Copy destination address
	bclr	#0,d0						; Strip off lowest bit so ge have a valid address...
	st.b	d2							; ... but make sure we know if it was 1
	move.l	#-$8000,d1					; Offset back for the search
	add.l	d1,d0						; d0 is the first address of the search
	movea.l	d0,a0						; Copy it to a0
	cmpa.l	(ROMEndLoc).w,a0			; Is start address past the end of ROM?
	bls.s	.start_in_rom_ram			; Branch if not
	cmpa.l	#$FFFF0000,a0				; Is start address in RAM?
	bhs.s	.start_in_rom_ram			; Branch if yes
	movea.l	#$FFFF0000,a0				; Set to start at the start of RAM

.start_in_rom_ram:
	cmp.l	(ROMEndLoc).w,d0			; Is it before the end of ROM?
	addi.l	#$FFFE,d0					; Generate the final address in the searh
	cmp.l	(ROMEndLoc).w,d0			; Is it before the end of ROM?
	bls.s	.not_end_of_rom				; Branch if yes
	move.l	(ROMEndLoc).w,d0			; Otherwise cap it to the right value

.not_end_of_rom:
	sub.l	a0,d0						; Prepare to make it a counter
	addq.l	#1,d0						; Correct number of bytes
	lsr.l	#1,d0						; Turn into number of words
	subq.l	#1,d0						; Make it into a loop counter
	bmi.s	.done						; Abort the search if we ended up with zero words to search
	move.w	d0,d1						; Should be a word by now, or something is very wrong

	swap	d5							; Move instruction to high word
	move.w	#$7FFE,d5					; d5 is now instruction + extension word for start address
	ext.w	d2
	sub.w	d2,d5						; And we now compensated for the low bit of the address
	moveq	#0,d2						; d2 will be a match counter

.loop:
	cmp.l	(a0),d5						; Is this current location the right one?
	bne.s	.next_word					; Branch if not
	tst.w	d2							; Have we found any previous matches?
	beq.s	.found_first				; Branch if not
	cmpi.w	#4,d2						; Have we found too many matches?
	blt.s	.keep_searching				; Branch if not
	addq.w	#1,d2						; Flag as so...
	bra.s	.search_done				; ... and stop the search
; ---------------------------------------------------------------------------
.keep_searching:
	pea	(a0)							; Store found location in stack
	bra.s	.chknext					; And go to the next iteration
; ---------------------------------------------------------------------------
.found_first:
	lea	2(a0),a2						; Store found location to a2

.chknext:
	addq.w	#1,d2						; Flag that we found yet one more

.next_word:
	subq.w	#2,d5						; Change extension word
	addq.l	#2,a0						; Change source address
	dbra	d1,.loop

.search_done:
	swap	d5							; Put instruction back in low word
	tst.w	d2							; Have we found any matched
	beq.s	.done						; No (this should be impossible?)
	cmpi.w	#1,d2						; Have we found a single match?
	bne.s	.found_many					; Branch if not
	; Victory snatched from the jaws of defeat! We have recovered
	; everything here.
	lea	(a2),a3							; Store found location to a2
	move.b	#%00000,ccr					; All zero = "found the one"

.done:
	rts									; Preserves condition codes
; ---------------------------------------------------------------------------
.found_many:
	tellp.l	a1							; Save output pointer
	setcursor	1, 11
	cmpi.b	#4,d2						; Did we have too many matches?
	sgt.b	d1							; Set d1 if so
	blt.s	.make_index					; Branch if not
	moveq	#4,d2						; Cap counter

.make_index:
	subq.w	#2,d2						; Discard one as it is in a2 and make it into a loop counter

.print_candidates:
	move.l	(sp)+,d0					; Fetch a stored address
	move.w	d2,-(sp)
	bsr.w	Print_Address
	vtputc	","
	move.w	(sp)+,d2
	dbra	d2,.print_candidates

	move.l	a2,d0
	subq.l	#2,d0						; Was offset by 2
	bsr.w	Print_Address

	tst.b	d1							; Were there too many addresses?
	beq.s	.skip_dots					; Branch if not
	vtputs	",..."

.skip_dots:
	seekset.l	a1						; Restore output pointer
	move.b	#%00010,ccr					; Overflow flag = "found too many"
	rts
; ===========================================================================
  else
BusError:
AddressError:
TraceError:
SpuriousException:
ZeroDivideError:
CHKExceptionError:
TRAPVError:
IllegalInstrError:
PrivilegeViolation:
LineAEmulation:
LineFEmulation:
TrapVector:
.error_trap:
	nop
	nop
	bra.s	.error_trap
  endif

