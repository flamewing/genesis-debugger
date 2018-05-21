; ===========================================================================
; Copyright 2011-2014 Flamewing <flamewing.sonic@gmail.com>
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
;     http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS,
; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
; See the License for the specific language governing permissions and
; limitations under the License.
; ===========================================================================
; ===========================================================================
; Alignment of disassembled instructions.
DisassemblyAlign = 3
; ===========================================================================
; This should be the path to "Terminal.asm":
	include "Terminal.asm"
; ===========================================================================
; NO USER SERVICEABLE PARTS BELOW THIS POINT
; ===========================================================================
; SUBROUTINE
; Disassembles an opcode.
; This is the "bare bones" input needed by the disassembler; the function passed
; in (a4) may require more parameters than this.
; Also, the function in (a4) may affect the return guarantees of this function,
; even though it should strive to not do this.
; Input:
; 	a3	For normal, forward disassembly, a3 is PC after the opcode, but before
; 	  	any the extension words, if any; in this case, the function at (a4) must
; 	  	be a no-nop.
; 	  	For rewinding disassembly (address and bus errors), a3 is PC for either
; 	  	the next instruction to execute or, for move instructions only, it may
; 	  	be right before the destination extension word if the error happened
; 	  	when reading from the source. In this case, the function at (a4) should
; 	  	handle rewinding to just after the opcode for all instructions, that is,
; 	  	to the same location that would be in a3 in a forward disassembly.
; 	a4	Pointer to function that gets called once the size and effective address
; 	  	bits for the opcode are known. Its purpose is to rewind the PC to just
; 	  	before the first extension word if needed.
; 	a5	Where the output text goes
; 	d5	Instruction register
; Output:
; 	a2	PC after the instruction and all extension words
; 	a3	Program counter of instruction disassembled + 2 (before extension words)
; 	a5	Cell just after last printed text
; 	d4	Number of bytes taken by the instruction's extension words
Disassemble_Opcode:
	moveq	#0,d4
	; Do a switch jump based on highest nibble of instruction.
	; This matches broad categories laid out in programmer's reference.
	move.w	d5,d0
	andi.w	#$F000,d0
	rol.w	#5,d0
	move.w	OpCodeMap(pc,d0.w),d0
	jmp	OpCodeMap(pc,d0.w)
; ===========================================================================
OpCodeMap:	offsetTable
	offsetTableEntry.w	Dis_BitMan_MoveP_Immed      	; %0000
	offsetTableEntry.w	Dis_MoveByte                	; %0001
	offsetTableEntry.w	Dis_MoveLong                	; %0010
	offsetTableEntry.w	Dis_MoveWord                	; %0011
	offsetTableEntry.w	Dis_Miscellaneous           	; %0100
	offsetTableEntry.w	Dis_AddQ_SubQ_Scc_DBcc      	; %0101
	offsetTableEntry.w	Dis_Bcc_Bsr_Bra             	; %0110
	offsetTableEntry.w	Dis_MoveQ                   	; %0111
	offsetTableEntry.w	Dis_Or_Div_Sbcd             	; %1000
	offsetTableEntry.w	Dis_Sub_SubX                	; %1001
	offsetTableEntry.w	Line1010Emulator            	; %1010
	offsetTableEntry.w	Dis_Cmp_Eor                 	; %1011
	offsetTableEntry.w	Dis_And_Mul_Abcd_Exg        	; %1100
	offsetTableEntry.w	Dis_Add_AddX                	; %1101
	offsetTableEntry.w	Dis_Shift_Rotate            	; %1110
	offsetTableEntry.w	Line1111Emulator            	; %1111
; ===========================================================================
Dis_BitMan_MoveP_Immed:					; %0000
	; Check to see if this is an immediate to CCR/SR instruction.
	move.w	d5,d0
	andi.w	#$5BF,d0
	cmpi.w	#$3C,d0
	beq.w	Handle_Immed_to_CCR_SR		; Branch if it is.
	; Check to see if this is a bit manipulation instruction with an immediate
	; operand.
	move.w	d5,d0
	andi.w	#$F00,d0
	cmpi.w	#$800,d0
	beq.s	Handle_BitMan_immed			; Branch if it is.
	; Also check for MOVES, which does not exist on MC68000.
	cmpi.w	#$E00,d0
	beq.w	IllegalInstruction			; Branch if it is.
	; Check to see if this is a reserved instruction (not on MC68000).
	move.w	d5,d0
	andi.w	#$1C0,d0
	cmpi.w	#$C0,d0
	beq.w	IllegalInstruction			; Branch if it is.
	; Check to see if this is an instruction with immediate operands.
	btst	#8,d5
	beq.w	Handle_Immed				; Branch if it is.
	; Check to see if this is a movep instruction.
	move.w	d5,d0
	andi.w	#$38,d0
	cmpi.w	#$8,d0
	beq.w	Handle_MoveP				; Branch if it is.
	; Bit manipulation with bit number in register.
	move.w	d5,d2
	andi.w	#$C0,d2
	lsr.w	#5,d2
	lea	Bitman_Msgs(pc),a1
	adda.w	(a1,d2.w),a1
	bsr.w	Print_Message
	lea	LongSize(pc),a1
	moveq	#2,d3						; Size of operation: long
	move.b	d5,d1
	andi.b	#$38,d1
	beq.s	.got_size
	lea	ByteSize(pc),a1
	moveq	#0,d3						; Size of operation: byte

.got_size:
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	bsr.w	Reg_Data_Direct
	vtputc	","
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_BitMan_immed:
	; Bit manipulation with bit number in immediate value.
	move.w	d5,d2
	andi.w	#$C0,d2
	lsr.w	#5,d2
	lea	Bitman_Msgs(pc),a1
	adda.w	(a1,d2.w),a1
	bsr.w	Print_Message
	lea	LongSize(pc),a1
	moveq	#2,d3						; Size of operation: long
	move.b	d5,d1
	andi.b	#$38,d1
	beq.s	.got_size
	lea	ByteSize(pc),a1
	moveq	#0,d3						; Size of operation: byte

.got_size:
	bsr.w	Print_Message

	move.w	d5,d1						; Effective address bits
	moveq	#2,d4						; Bit number is an immediate parameter
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	move.w	(a2)+,d0					; Bit number in immediate field
	vtputc	"#"
	bsr.w	Print_Byte
	vtputc	","
	bra.w	Handle_effective_address
; ===========================================================================
Handle_MoveP:
	; movep.
	lea	MovePmsg(pc),a1
	bsr.w	Print_Message
	lea	WordSize(pc),a1
	moveq	#1,d3						; Size of operation: word
	btst	#6,d5
	beq.s	.got_size
	add.b	d3,d3
	lea	LongSize(pc),a1
	moveq	#2,d3						; Size of operation: long

.got_size:
	bsr.w	Print_Message
	; Bit 7 indicates direction of operation.
	btst	#7,d5
	beq.s	.skip_reg
	; Moving from register.
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	bsr.w	Reg_Data_Direct
	vtputc	","

.skip_reg:
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	; Print displacement (if nonzero).
	lea	(a3),a2
	move.w	(a2)+,d0
	beq.s	.skip_null
	bsr.w	Print_Word_Signed

.skip_null:
	move.w	d5,d1
	andi.w	#7,d1
	moveq	#2,d4
	btst	#7,d5
	bne.w	Reg_Addr_Indirect
	bsr.w	Reg_Addr_Indirect
	; If we got here, a register is the destination.
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	bra.w	Reg_Data_Direct
; ===========================================================================
Handle_Immed:
	; Instructions with immediate operands.
	move.w	d5,d1
	andi.w	#$E00,d1
	lsr.w	#8,d1
	cmpi.b	#8,d1						; Invalid instruction?
	beq.w	IllegalInstruction
	lea	Immed_Msgs(pc),a1
	adda.w	(a1,d1.w),a1
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#2,d4						; Immediate value is an additional parameter
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bsr.w	Immediate
	vtputc	","
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Immed_to_CCR_SR:
	; Instructions affecting CCR/SR with immediate operands.
	move.w	d5,d1
	andi.w	#$E00,d1
	lsr.w	#8,d1
	cmpi.b	#8,d1						; Invalid instruction?
	beq.w	IllegalInstruction
	lea	Immed_Msgs(pc),a1
	adda.w	(a1,d1.w),a1
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	eori.w	#2,d3						; This is here because table is reused.
	lea	Immed_to_CCR_SR_Msgs(pc),a1
	adda.w	(a1,d3.w),a1
	lsr.w	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#2,d4						; Immediate is an additional parameter
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bsr.w	Immediate
	vtputc	","
	bra.w	Print_Message
; ===========================================================================
Bitman_Msgs:	offsetTable
		offsetTableEntry.w Btstmsg
		offsetTableEntry.w Bchgmsg
		offsetTableEntry.w Bclrmsg
		offsetTableEntry.w Bsetmsg
Immed_Msgs:	offsetTable
		offsetTableEntry.w Orimsg
		offsetTableEntry.w Andimsg
		offsetTableEntry.w Subimsg
		offsetTableEntry.w Addimsg
		offsetTableEntry.w IllegalInstructionMsg
		offsetTableEntry.w Eorimsg
		offsetTableEntry.w Cmpimsg
		offsetTableEntry.w IllegalInstructionMsg
Immed_to_CCR_SR_Msgs:	offsetTable
		offsetTableEntry.w SRegmsg
		offsetTableEntry.w CCRmsg
		offsetTableEntry.w CCRmsg
		offsetTableEntry.w SRegmsg
Btstmsg:	vtstring BLUE,"btst"
Bchgmsg:	vtstring BLUE,"bchg"
Bclrmsg:	vtstring BLUE,"bclr"
Bsetmsg:	vtstring BLUE,"bset"

Orimsg:		vtstring BLUE,"ori"
Andimsg:	vtstring BLUE,"andi"
Subimsg:	vtstring BLUE,"subi"
Addimsg:	vtstring BLUE,"addi"
Eorimsg:	vtstring BLUE,"eori"
Cmpimsg:	vtstring BLUE,"cmpi"

SRegmsg:	vtstring GREEN,"sr"
CCRmsg:		vtstring GREEN,"ccr"

MovePmsg:	vtstring BLUE,"movep"
; ===========================================================================
Dis_MoveByte:							; %0001
Dis_MoveWord:							; %0011
Dis_MoveLong:							; %0010
	lea	Movemsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d1
	lsr.w	#3,d1
	move.w	d1,d2
	andi.w	#$38,d2
	lsr.w	#6,d1
	andi.w	#7,d1
	or.w	d2,d1						; Effective address bits for destination
	; Is this a movea?
	cmpi.b	#8,d2
	bne.s	.not_addr					; Branch if not.
	vtputc	"a",BLUE

.not_addr:
	move.w	d5,d3
	andi.w	#$3000,d3
	rol.w	#4,d3
	; Remap move sizes to other sizes, so we can use the same table.
	move.b	MoveSizeMsgs(pc,d3.w),d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	move.w	d1,-(sp)					; Save address mode of last parameter
	lsr.w	#1,d3
	moveq	#0,d4						; No additional immediate parameters
	; Note: for moves, the function should get the source effective address bits
	; from the IR at d5.
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	move.w	d5,d1
	bsr.w	Handle_effective_address
	vtputc	","
	move.w	(sp)+,d0
	move.w	d0,d1
	bra.w	Handle_effective_address_Part2
; ===========================================================================
Movemsg:	vtstring BLUE,"move"
MoveSizeMsgs:
		dc.b 6,0,4,2
; ===========================================================================
Dis_Miscellaneous:						; %0100
	; Chock full of unrelated instructions.
	; Is this a lea or a chk instruction?
	btst	#8,d5
	bne.w	Handle_Lea_Chk				; Branch if yes.
	; Is this a move to/from SR/CCR, negx, clr, neg or not instructions?
	btst	#11,d5
	beq.w	Handle_MoveSRCCR_NegX_Clr_Neg_Not	; Branch if yes.
	; Otherwise, do a switch based on ramaining bits of second-to-highest nibble.
	move.w	d5,d0
	andi.w	#$600,d0
	lsr.w	#8,d0
	move.w	MiscOpCodeMap(pc,d0.w),d0
	jmp	MiscOpCodeMap(pc,d0.w)
; ===========================================================================
MiscOpCodeMap:	offsetTable
	offsetTableEntry.w	Handle_Ext_Nbcd_Swap_Pea_MoveMToMem                 	; %00
	offsetTableEntry.w	Handle_Illegal_Tas_Tst                              	; %01
	offsetTableEntry.w	Handle_MoveMToReg                                   	; %10
	offsetTableEntry.w	Handle_Trap_LinkW_Unlk_MoveUSP_Reset_Nop_Stop_Rte_Rts_TrapV_Rtr_Jmp_Jsr	; %11
; ===========================================================================
Handle_Trap_LinkW_Unlk_MoveUSP_Reset_Nop_Stop_Rte_Rts_TrapV_Rtr_Jmp_Jsr:
	; Select instruction based on second-to-lowest nibble.
	; Is this a jmp or a jsr?
	btst	#7,d5
	bne.w	Handle_Jmp_Jsr				; Branch if yes.
	;Is this a trap?
	move.w	d5,d1
	andi.w	#$30,d1
	beq.s	Handle_Trap					; Branch if yes.
	; Is this a link or unlk?
	cmpi.w	#$10,d1
	beq.s	Handle_Link_Unlk			; Branch if yes.
	; Is this a move to/from USP?
	cmpi.w	#$20,d1
	beq.w	Handle_MoveUSP				; Branch if yes.
	; Finally down to the last nibble!
	btst	#3,d1
	bne.w	IllegalInstruction
	; Is this a stop?
	move.w	d5,d1
	andi.w	#7,d1
	cmpi.w	#2,d1
	beq.s	Handle_Stop					; Branch if yes.
	lsl.w	#1,d1
	; This is one of: reset, stop, rte, rts, trapv or rtr.
	; None of them have operands.
	cmpi.b	#8,d1						; Invalid instruction?
	beq.w	IllegalInstruction
	lea	Reset_Nop_Stop_Rte_Rts_TrapV_Rtrmsgs(pc),a1
	adda.w	(a1,d1.w),a1
	lea	(a3),a2
	bra.w	Print_Message
; ===========================================================================
Handle_Stop:
	; A stop instruction.
	lea	Stopmsg(pc),a1
	bsr.w	Print_Message
	moveq	#1,d3
	lea	(a3),a2
	bra.w	Immediate
; ===========================================================================
Handle_Trap:
	; Watch out! A trap!
	lea	Trapmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d0
	andi.w	#$F,d0
	lea	(a3),a2
	bra.w	Print_Byte
; ===========================================================================
Handle_Link_Unlk:
	; link or unlk.
	; bit 3 says whether this is a link or unlk.
	btst	#3,d5
	beq.s	Handle_Link
	lea	Unlkmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#7,d1
	lea	(a3),a2
	bra.w	Reg_Addr_Direct
; ===========================================================================
Handle_Link:
	lea	Linkmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#7,d1
	bsr.w	Reg_Addr_Direct
	vtputc	","
	moveq	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#2,d4						; Number of bytes to allocate on stack is immediate
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Immediate
; ===========================================================================
Handle_MoveUSP:
	; Move to/from USP.
	lea	Movemsg(pc),a1
	bsr.w	Print_Message
	lea	LongSize(pc),a1
	bsr.w	Print_Message
	lea	(a3),a2
	move.w	d5,d1
	andi.w	#7,d1
	; Bit 3 indicated direction of move.
	btst	#3,d5
	beq.s	.to_usp
	; Moving from USP.
	lea	USPmsg(pc),a1
	bsr.w	Print_Message
	vtputc	","
	bra.w	Reg_Addr_Direct
; ===========================================================================
.to_usp:
	bsr.w	Reg_Addr_Direct
	; Moving to USP.
	vtputc	","
	lea	USPmsg(pc),a1
	bra.w	Print_Message
; ===========================================================================
Handle_Jmp_Jsr:
	; jmp or jsr.
	; Is this a jmp?
	lea	Jmpmsg(pc),a1
	btst	#6,d5
	bne.s	.got_type					; Branch if so.
	lea	Jsrmsg(pc),a1

.got_type:
	bsr.w	Print_Message
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	; jsr (a4) will not return here if this was a jmp and we are in a rewinding
	; disassembly that starts at the target of the jump.
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Ext_Nbcd_Swap_Pea_MoveMToMem:
	; ext, nbcd, swap, pea, movem to memory.
	; If bit 7 is set, this can be an ext or movem.
	btst	#7,d5
	beq.s	.not_movem
	move.w	d5,d1
	andi.w	#$38,d1
	beq.s	Handle_Ext					; This is an ext.
	bra.w	Handle_MoveM				; We have a movem.
; ===========================================================================
.not_movem:
	; Filter out invalid instructions.
	move.w	d5,d1
	andi.w	#$38,d1
	cmpi.w	#8,d1
	beq.w	IllegalInstruction
	; Is this an nbcd?
	btst	#6,d5
	beq.s	Handle_Nbcd					; Branch if yes.
	; Check if this is a swap.
	tst.b	d1
	beq.s	Handle_Swap					; Branch if yes.
	; We have a pea. Make soup?
	lea	Peamsg(pc),a1
	bsr.w	Print_Message
	moveq	#2,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Swap:
	; Swap.
	lea	Swapmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#7,d1
	lea	(a3),a2
	bra.w	Reg_Data_Direct
; ===========================================================================
Handle_Nbcd:
	; Nbcd.
	lea	Nbcdmsg(pc),a1
	bsr.w	Print_Message
	moveq	#0,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Ext:
	; Ext.
	lea	Extmsg(pc),a1
	bsr.w	Print_Message
	lea	WordSize(pc),a1
	btst	#6,d5
	beq.s	.got_size
	lea	LongSize(pc),a1

.got_size:
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#7,d1
	lea	(a3),a2
	bra.w	Reg_Data_Direct
; ===========================================================================
Handle_MoveMToReg:
	; Bit 7 is another instruction that doesn't exist for MC68000.
	btst	#7,d5
	beq.w	IllegalInstruction

Handle_MoveM:
	; Movem.
	; This is for an end-of-line marker, as movem instructions can give quite
	; long lines when disassembled.
	tellp.w	d0
	seekp.w	nCols-DisassemblyAlign-1,d0
	move.w	d0,-(sp)
	lea	MoveMmsg(pc),a1
	bsr.w	Print_Message
	lea	WordSize(pc),a1
	moveq	#1,d3
	btst	#6,d5
	beq.s	.got_size
	lea	LongSize(pc),a1
	add.w	d3,d3

.got_size:
	bsr.w	Print_Message

	move.w	d5,d1						; Effective address bits
	moveq	#2,d4						; Register list is an additional immediate parameter
	jsr	(a4)							; Rewind a3 to instruction+2 if needed

	; a0 = end-of-line address.
	movea.w	(sp)+,a0
	lea	(a3),a2
	; Fetch list.
	move.w	(a2)+,d0
	; Assuming normal register list.
	lea	Normal_Next_Reg(pc),a4
	; If bit 10 is set, we are moving from register list to memory.
	btst	#10,d5
	bne.s	.reg_to_mem
	move.w	d5,d1
	andi.w	#$38,d1
	; Is the addressing move -(An)?
	cmpi.w	#$20,d1
	bne.s	.not_predecr
	; It is; use reversed register list.
	lea	Predecr_Next_Reg(pc),a4

.not_predecr:
	; Print register list.
	bsr.s	Handle_MoveM_Reg_List
	vtputc	","
	bsr.s	Do_Line_Break
	bra.w	Handle_effective_address
; ===========================================================================
.reg_to_mem:
	move.w	d0,-(sp)
	bsr.w	Handle_effective_address
	move.w	(sp)+,d0
	; Moving to register list; print it.
	vtputc	","
	bra.s	Handle_MoveM_Reg_List
; ===========================================================================
Normal_Next_Reg:
	lsr.w	#1,d0
	rts
; ---------------------------------------------------------------------------
Normal_Put_Back:
	roxl.w	#1,d0
	rts
; ===========================================================================
Predecr_Next_Reg:
	lsl.w	#1,d0
	rts
; ---------------------------------------------------------------------------
	if *-Predecr_Next_Reg > Normal_Put_Back-Normal_Next_Reg
	fatal "The code in Predecr_Next_Reg must not be larger than the code in Normal_Next_Reg"
	endif
	org Predecr_Next_Reg+Normal_Put_Back-Normal_Next_Reg
Predecr_Put_Back:
	roxr.w	#1,d0
	rts
; ===========================================================================
Handle_MoveM_Reg_List:
	; Prints a register list for -(An), adding line breaks if needed.
	; First, do Dn.
	moveq	#15,d3						; 16 registers

	; Loop until a register is specified.
.find_reg_loop:
	jsr	(a4)
	dblo.w	d3,.find_reg_loop
	; Save the register.
	move.w	d3,d2
	bmi.s	.done						; We finished the loop, so exit.

	; Loop through all contiguous registers.
.find_last_reg:
	jsr	(a4)
	dbhs.w	d3,.find_last_reg

	; Add back last bit shifted out.
	jsr	Normal_Put_Back-Normal_Next_Reg(a4)
	; Have we finished going through the list?
	tst.w	d3
	bpl.s	.list_not_done
	; Yes; make d3 sane.
	addq.w	#1,d3

.list_not_done:
	; Do we need to print one or more registers?
	cmp.w	d3,d2
	beq.s	.do_single_reg
	; More than one. Print first register.
	moveq	#15,d1
	sub.w	d2,d1
	bsr.s	Print_Reg
	vtputc	"-"

.do_single_reg:
	; Print register.
	moveq	#15,d1
	sub.w	d3,d1
	bsr.s	Print_Reg
	; Have we finished going through the list?
	tst.w	d0
	beq.s	.done
	; No. Print a slash and loop.
	vtputc	"/"
	bsr.s	Do_Line_Break
	dbra	d3,.find_reg_loop

.done:
	rts
; ===========================================================================
Print_Reg:
	cmpi.b	#8,d1
	blo.w	Reg_Data_Direct
	subq.w	#8,d1
	bra.w	Reg_Addr_Direct
; ===========================================================================
Do_Line_Break:
	; Prints a register list for all but -(An), adding line breaks if needed.
	move.w	a0,d1
	getdst.w	d1
	cmpi.w	#8,d1
	bhs.s	.done

	moveq	#' ',d2
	subq.w	#2,d1						; Make it into a loop counter and remove an additional character
	bmi.s	.buffer_underrun			; This should never happen

	fillr	d2,d1
	vtputs	"\\ "						; Put a line continuation character and a blank

.buffer_underrun:
	moveq	#9+DisassemblyAlign,d1
	fillr	d2,d1
	adda.w	#DisplayCell(0, 1),a0

.done:
	rts
; ===========================================================================
Handle_Illegal_Tas_Tst:
	; illegal, tas, tst.
	; Is this a tst?
	move.w	d5,d1
	andi.w	#$C0,d1
	cmpi.w	#$C0,d1
	bne.s	Handle_Tst					; Branch if yes.
	; Is this an invalid instruction?
	move.w	d5,d1
	andi.w	#$3F,d1
	cmpi.w	#$3A,d1
	beq.w	IllegalInstruction			; If yes, branch.
	; Is this a illegal instruction?
	cmpi.w	#$3C,d1
	beq.s	Handle_Illegal				; Branch if yes.
	; We have a tas.
	lea	Tasmsg(pc),a1
	bsr.w	Print_Message
	moveq	#0,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Illegal:
	; illegal.
	lea	Illegalmsg(pc),a1
	lea	(a3),a2
	bra.w	Print_Message
; ===========================================================================
Handle_Tst:
	; tst.
	lea	Tstmsg(pc),a1
	bra.s	Handle_NegX_Clr_Neg_Not_Tst
; ===========================================================================
Handle_MoveSRCCR_NegX_Clr_Neg_Not:
	; Is this a move to/from SR/CCR?
	move.w	d5,d1
	andi.w	#$C0,d1
	cmpi.w	#$C0,d1
	beq.s	Handle_MoveSRCCR			; Branch if yes.
	; negx, clr, neg, not.
	move.w	d5,d1
	andi.w	#$600,d1
	lsr.w	#8,d1
	lea	NegX_Clr_Neg_Notmsgs(pc),a1
	adda.w	(a1,d1.w),a1

Handle_NegX_Clr_Neg_Not_Tst:
	bsr.w	Print_Message

	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_MoveSRCCR:
	; Move to/from SR/CCR.
	lea	Movemsg(pc),a1
	bsr.w	Print_Message
	lea	WordSize(pc),a1
	bsr.w	Print_Message
	moveq	#1,d3

	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	btst	#10,d5
	beq.s	.from_ccr_sr
	; Move is to SR/CCR.
	bsr.w	Handle_effective_address
	vtputc	","

.from_ccr_sr:
	move.w	d5,d1
	andi.w	#$600,d1
	lsr.w	#8,d1
	lea	Immed_to_CCR_SR_Msgs(pc),a1
	adda.w	(a1,d1.w),a1
	btst	#10,d5
	bne.w	Print_Message
	bsr.w	Print_Message
	; Move is from SR/CCR.
	vtputc	","
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Lea_Chk:
	; lea, chk.
	; Is this an invalid instruction on MC68000?
	move.w	d5,d1
	andi.w	#$1C0,d1
	cmpi.w	#$1C0,d1
	bne.s	.not_invalid				; Branch if not.
	move.w	d5,d1
	andi.w	#$38,d1
	beq.w	IllegalInstruction			; Branch if yes.

.not_invalid:
	; Is this a chk?
	btst	#6,d5
	beq.s	.chk						; Branch if yes.
	; lea.
	lea	Leamsg(pc),a1
	bsr.w	Print_Message
	moveq	#2,d3
	bra.s	.lea_chk
; ===========================================================================
.chk:
	; chk.
	btst	#7,d5
	beq.w	IllegalInstruction
	lea	Chkmsg(pc),a1
	bsr.w	Print_Message
	moveq	#1,d3

.lea_chk:
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bsr.w	Handle_effective_address
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	btst	#6,d5
	beq.w	Reg_Data_Direct
	bra.w	Reg_Addr_Direct
; ===========================================================================
NegX_Clr_Neg_Notmsgs:	offsetTable
		offsetTableEntry.w NegXmsg
		offsetTableEntry.w Clrmsg
		offsetTableEntry.w Negmsg
		offsetTableEntry.w Notmsg
NegXmsg:	vtstring BLUE,"negx"
Clrmsg:		vtstring BLUE,"clr"
Negmsg:		vtstring BLUE,"neg"
Notmsg:		vtstring BLUE,"not"

Reset_Nop_Stop_Rte_Rts_TrapV_Rtrmsgs:	offsetTable
		offsetTableEntry.w Resetmsg
		offsetTableEntry.w Nopmsg
		offsetTableEntry.w Stopmsg
		offsetTableEntry.w Rtemsg
		offsetTableEntry.w IllegalInstructionMsg
		offsetTableEntry.w Rtsmsg
		offsetTableEntry.w TrapVmsg
		offsetTableEntry.w Rtrmsg
Resetmsg:	vtstring BLUE,"reset"
Nopmsg:		vtstring BLUE,"nop"
Rtemsg:		vtstring BLUE,"rte"
Rtsmsg:		vtstring BLUE,"rts"
TrapVmsg:	vtstring BLUE,"trapv"
Rtrmsg:		vtstring BLUE,"rtr"

Nbcdmsg:	vtstring BLUE,"nbcd.b  "
Extmsg:		vtstring BLUE,"ext"
Swapmsg:	vtstring BLUE,"swap  "
MoveMmsg:	vtstring BLUE,"movem"

Stopmsg:	vtstring BLUE,"stop  "
USPmsg:		vtstring GREEN,"usp"
Linkmsg:	vtstring BLUE,"link  "
Unlkmsg:	vtstring BLUE,"unlk  "
Trapmsg:	vtstring BLUE,"trap  #"
Jmpmsg:		vtstring BLUE,"jmp  "
Jsrmsg:		vtstring BLUE,"jsr  "
Leamsg:		vtstring BLUE,"lea  "
Peamsg:		vtstring BLUE,"pea  "
Chkmsg:		vtstring BLUE,"chk.w  "
Tstmsg:		vtstring BLUE,"tst"
Tasmsg:		vtstring BLUE,"tas.b  "
Illegalmsg:	vtstring BLUE,"illegal"
; ===========================================================================
Dis_AddQ_SubQ_Scc_DBcc:	; %0101
	; Is this addq or subq?
	move.w	d5,d1
	andi.w	#$C0,d1
	cmpi.w	#$C0,d1
	bne.w	Handle_AddQ_SubQ			; Branch if yes.
	; Is this a loop instruction (DBcc)?
	move.w	d5,d1
	andi.w	#$38,d1
	cmpi.w	#8,d1
	beq.s	Handle_Dbcc					; Branch if yes.
	; Is this Scc?
	cmpi.w	#$38,d1
	bne.w	Handle_Scc					; Branch if yes.
	move.w	d5,d1
	andi.w	#6,d1
	beq.w	Handle_Scc					; Branch if yes.
	bra.w	IllegalInstruction
; ===========================================================================
Handle_Dbcc:
	; DBcc.
	lea	DbCCmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d2
	andi.w	#$F00,d2
	lsr.w	#7,d2
	lea	Db_Trap_S_CC(pc),a1
	adda.w	(a1,d2.w),a1
	bsr.w	Print_Message
	lea	WordSize(pc),a1
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#7,d1
	bsr.w	Reg_Data_Direct
	vtputc	","
	moveq	#1,d3						; Size = word; irrelevant for this opcode
	move.w	d5,d1						; Effective address bits; irrelevant here
	moveq	#2,d4						; Branch distance is an additional immediate parameter
	; Note: rewinding disassemblies do not return from jsr (a4) if they started
	; at the target of the dbCC.
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	move.w	(a3),d0
	ext.l	d0
	add.l	a3,d0
	lea	2(a3),a2
	bra.w	Print_Address
; ===========================================================================
Handle_Scc:
	; Scc.
	vtputc	"s",BLUE
	move.w	d5,d2
	andi.w	#$F00,d2
	lsr.w	#7,d2
	lea	Db_Trap_S_CC(pc),a1
	adda.w	(a1,d2.w),a1
	bsr.w	Print_Message
	lea	ByteSize(pc),a1
	bsr.w	Print_Message
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_AddQ_SubQ:
	; addq/subq.
	lea	AddQmsg(pc),a1
	btst	#8,d5
	beq.s	.got_msg
	lea	SubQmsg(pc),a1

.got_msg:
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	vtputc	"#"
	move.w	d5,d2
	andi.w	#$E00,d2
	rol.w	#7,d2
	bne.s	.got_immed
	addq.w	#8,d2

.got_immed:
	itoa.w	d2,RED
	vtput.w	d2
	vtputc	","
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
AddQmsg:	vtstring BLUE,"addq"
SubQmsg:	vtstring BLUE,"subq"
DbCCmsg:	vtstring BLUE,"db"
; ===========================================================================
Db_Trap_S_CC:	offsetTable
		offsetTableEntry.w Tcmsg
		offsetTableEntry.w Fcmsg
		offsetTableEntry.w HImsg
		offsetTableEntry.w LSmsg
		offsetTableEntry.w CCmsg
		offsetTableEntry.w CSmsg
		offsetTableEntry.w NEmsg
		offsetTableEntry.w EQmsg
		offsetTableEntry.w VCmsg
		offsetTableEntry.w VSmsg
		offsetTableEntry.w PLmsg
		offsetTableEntry.w MImsg
		offsetTableEntry.w GEmsg
		offsetTableEntry.w LTmsg
		offsetTableEntry.w GTmsg
		offsetTableEntry.w LEmsg
; ===========================================================================
Dis_Bcc_Bsr_Bra:						; %0110
	; Bcc, BSR, BRA.
	vtputc	"b",BLUE
	move.w	d5,d2
	andi.w	#$F00,d2
	lsr.w	#7,d2
	lea	Branch_CC(pc),a1
	adda.w	(a1,d2.w),a1
	bsr.w	Print_Message
	tst.b	d5							; Is this a short branch?
	sne.b	d3							; d3 = -1 if yes
	lea	ShortSize(pc),a1
	bne.s	.got_size					; Branch if short jump.
	lea	WordSize(pc),a1

.got_size:
	bsr.w	Print_Message
	addq.b	#1,d3						; d3 = 0 for short branch, 1 for word branch
	move.w	d5,d1						; Effective address bits; irrelevant for bCC
	moveq	#0,d4						; No additional immediate parameters
	; Note: rewinding disassemblies may not return from jsr (a4) if they started
	; at the target of the bCC; the exception would be for a bsr, for which the
	; return address is available, hence the instruction's PC is recoverable.
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	tst.b	d5							; Is this a short branch?
	bne.s	Handle_Bcc_Short			; Branch if yes.
	move.w	(a3),d0
	ext.l	d0
	add.l	a3,d0
	moveq	#2,d4
	lea	2(a3),a2
	bra.w	Print_Address
; ===========================================================================
Handle_Bcc_Short:
	move.b	d5,d0
	ext.w	d0
	ext.l	d0
	add.l	a3,d0
	lea	(a3),a2
	bra.w	Print_Address
; ===========================================================================
Branch_CC:	offsetTable
		offsetTableEntry.w RAmsg
		offsetTableEntry.w SRmsg
		offsetTableEntry.w HImsg
		offsetTableEntry.w LSmsg
		offsetTableEntry.w CCmsg
		offsetTableEntry.w CSmsg
		offsetTableEntry.w NEmsg
		offsetTableEntry.w EQmsg
		offsetTableEntry.w VCmsg
		offsetTableEntry.w VSmsg
		offsetTableEntry.w PLmsg
		offsetTableEntry.w MImsg
		offsetTableEntry.w GEmsg
		offsetTableEntry.w LTmsg
		offsetTableEntry.w GTmsg
		offsetTableEntry.w LEmsg
Tcmsg:	vtstring BLUE,"t"
Fcmsg:	vtstring BLUE,"f"
RAmsg:	vtstring BLUE,"ra"
SRmsg:	vtstring BLUE,"sr"
HImsg:	vtstring BLUE,"hi"
LSmsg:	vtstring BLUE,"ls"
CCmsg:	vtstring BLUE,"cc"
CSmsg:	vtstring BLUE,"cs"
NEmsg:	vtstring BLUE,"ne"
EQmsg:	vtstring BLUE,"eq"
VCmsg:	vtstring BLUE,"vc"
VSmsg:	vtstring BLUE,"vs"
PLmsg:	vtstring BLUE,"pl"
MImsg:	vtstring BLUE,"mi"
GEmsg:	vtstring BLUE,"ge"
LTmsg:	vtstring BLUE,"lt"
GTmsg:	vtstring BLUE,"gt"
LEmsg:	vtstring BLUE,"le"
; ===========================================================================
Dis_MoveQ:								; %0111
	; moveq.
	lea	Moveqmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d0
	bsr.w	Print_Byte_Signed
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	lea	(a3),a2
	bra.w	Reg_Data_Direct
; ===========================================================================
Moveqmsg:	vtstring BLUE,"moveq  #"
; ===========================================================================
Dis_Or_Div_Sbcd:						; %1000
	; or, div, sbcd.
	; Is this a div?
	move.w	d5,d1
	andi.w	#$C0,d1
	cmpi.w	#$C0,d1
	beq.w	Handle_Div					; Branch if yes.
	; Filter out invalid instructions.
	move.w	d5,d1
	andi.w	#$1F0,d1
	cmpi.w	#$180,d1
	beq.w	IllegalInstruction
	cmpi.w	#$140,d1
	beq.w	IllegalInstruction
	; Is this a sbcd?
	andi.w	#$1F0,d1
	cmpi.w	#$100,d1
	beq.s	Handle_Sbcd					; Branch if yes.
	lea	Ormsg(pc),a1
	bsr.w	Print_Message
	; or.
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	bra.w	Handle_Add_And_Or_Sub
; ===========================================================================
Handle_Sbcd:
	; sbcd.
	lea	Sbcdmsg(pc),a1
	bsr.w	Print_Message
	moveq	#0,d3
	bra.w	Handle_AddX_Abcd_SubX_Sbcd_Common
; ===========================================================================
Handle_Div:
	; div.
	lea	Divmsg(pc),a1
	bra.w	Handle_Div_Mul_Common
; ===========================================================================
Sbcdmsg:	vtstring BLUE,"sbcd.b  "
Ormsg:		vtstring BLUE,"or"
Divmsg:		vtstring BLUE,"div"
; ===========================================================================
Dis_Cmp_Eor:							; %1011
	; cmp, eor.
	; Is this a cmpm?
	move.w	d5,d1
	andi.w	#$138,d1
	cmpi.w	#$108,d1
	beq.w	Handle_CmpM					; Branch if yes.
	; Is this a cmpa?
	move.w	d5,d1
	andi.w	#$C0,d1
	cmpi.w	#$C0,d1
	beq.s	Handle_CmpA					; Branch if yes.
	; Is this a normal cmp?
	btst	#8,d5
	beq.s	Handle_Cmp					; Branch if yes.
	; eor.
	lea	Eormsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	bsr.w	Reg_Data_Direct
	vtputc	","
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
Handle_Cmp:
	; cmp.
	lea	Cmpmsg(pc),a1
	pea	Reg_Data_Direct(pc)				; An rts will branch to this now
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	bra.s	Handle_Cmp_CmpA
; ===========================================================================
Handle_CmpA:
	; cmpa.
	lea	CmpAmsg(pc),a1
	pea	Reg_Addr_Direct(pc)				; An rts will branch to this now
	bsr.w	Print_Message
	moveq	#2,d3
	btst	#8,d5
	beq.s	.got_size
	add.b	d3,d3

.got_size:
Handle_Cmp_CmpA:
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bsr.w	Handle_effective_address
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	rts									; Branches to either Reg_Data_Direct or Reg_Addr_Direct
; ===========================================================================
Handle_CmpM:
	; cmpm.
	lea	CmpMmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#7,d1
	bsr.w	Reg_Addr_Post_Indirect
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	lea	(a3),a2
	bra.w	Reg_Addr_Post_Indirect
; ===========================================================================
Eormsg:		vtstring BLUE,"eor"
Cmpmsg:		vtstring BLUE,"cmp"
CmpAmsg:	vtstring BLUE,"cmpa"
CmpMmsg:	vtstring BLUE,"cmpm"
; ===========================================================================
Dis_And_Mul_Abcd_Exg:					; %1100
	; and, mul, abcd, exg.
	; Is this a mul?
	move.w	d5,d1
	andi.w	#$C0,d1
	cmpi.w	#$C0,d1
	beq.w	Handle_Mul					; Branch if so.
	; Is this an exg?
	move.w	d5,d1
	andi.w	#$1F8,d1
	cmpi.w	#$188,d1
	beq.s	Handle_Exg_Mixed			; Branch if yes.
	cmpi.w	#$148,d1
	beq.s	Handle_Exg_Both_Addr		; Branch if yes.
	cmpi.w	#$140,d1
	beq.s	Handle_Exg_Both_Data		; Branch if yes.
	; Is this an abcd?
	andi.w	#$1F0,d1
	cmpi.w	#$100,d1
	beq.s	Handle_Abcd					; Branch if yes.
	; and.
	lea	Andmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	bra.w	Handle_Add_And_Or_Sub
; ===========================================================================
Handle_Exg_Both_Data:
	; exg dx,dy.
	lea	Reg_Data_Direct(pc),a2
	pea	Reg_Data_Direct(pc)				; An rts will branch to this now
	bra.s	Handle_Exg
; ===========================================================================
Handle_Exg_Both_Addr:
	; exg ax,ay.
	lea	Reg_Addr_Indirect(pc),a2
	pea	Reg_Addr_Indirect(pc)			; An rts will branch to this now
	bra.s	Handle_Exg
; ===========================================================================
Handle_Exg_Mixed:
	; exg dx,ay.
	lea	Reg_Data_Direct(pc),a2
	pea	Reg_Addr_Indirect(pc)			; An rts will branch to this now

Handle_Exg:
	; exg.
	lea	Exgmsg(pc),a1
	bsr.w	Print_Message
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	jsr	(a2)
	vtputc	","
	move.w	d5,d1
	andi.w	#7,d1
	lea	(a3),a2
	rts									; Branches to Reg_(Data_D|Addr_Ind)irect
; ===========================================================================
Handle_Abcd:
	; The alphabet.
	lea	Abcdmsg(pc),a1
	bsr.w	Print_Message
	moveq	#0,d3
	bra.w	Handle_AddX_Abcd_SubX_Sbcd_Common
; ===========================================================================
Handle_Mul:
	; A mul? Is this Dark Sun?
	lea	Mulmsg(pc),a1

Handle_Div_Mul_Common:
	; Div/mul.
	; Print u or s as needed.
	bsr.w	Print_Message

	btst	#8,d5
	beq.s	.is_unsigned
	vtputc	"s",BLUE
	bra.s	.got_signedness
; ---------------------------------------------------------------------------
.is_unsigned:
	vtputc	"u",BLUE

.got_signedness:
	lea	WordSize(pc),a1
	bsr.w	Print_Message
	moveq	#1,d3						; Word sized
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bsr.w	Handle_effective_address
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	bra.w	Reg_Data_Direct
; ===========================================================================
Abcdmsg:	vtstring BLUE,"abcd.b  "
Andmsg:		vtstring BLUE,"and"
Exgmsg:		vtstring BLUE,"exg.l   "
Mulmsg:		vtstring BLUE,"mul"
; ===========================================================================
Dis_Sub_SubX:							; %1001
	; sub, subx.
	lea	Submsg(pc),a1
	bra.s	Handle_Add_AddX_Sub_SubX
; ===========================================================================
Dis_Add_AddX:							; %1101
	; add, addx.
	lea	Addmsg(pc),a1

Handle_Add_AddX_Sub_SubX:
	bsr.w	Print_Message
	; Is this addx/subx?
	move.w	d5,d1
	andi.w	#$130,d1
	cmpi.w	#$100,d1
	beq.w	Handle_AddX_SubX			; Branch if yes.
	; Is this add/sub?
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	cmpi.b	#6,d3
	bne.s	Handle_Add_And_Or_Sub		; Branch if yes.
	; adda/suba.
	vtputc	"a",BLUE
	moveq	#2,d3
	btst	#8,d5
	beq.s	.got_size
	add.b	d3,d3

.got_size:
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bsr.w	Handle_effective_address
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	bra.w	Reg_Addr_Direct
; ===========================================================================
Handle_Add_And_Or_Sub:
	; add, and, or, sub.
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message
	lsr.w	#1,d3
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	btst	#8,d5
	bne.s	.ea_second
	; First parameter is effective address.
	bsr.w	Handle_effective_address
	vtputc	","

.ea_second:
	; Print register.
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	btst	#8,d5
	beq.w	Reg_Data_Direct
	bsr.w	Reg_Data_Direct
	; Second parameter is effective address.
	vtputc	","
	bra.w	Handle_effective_address
; ===========================================================================
Handle_AddX_SubX:
	; addx/subx.
	vtputc	"x",BLUE
	move.w	d5,d3
	andi.w	#$C0,d3
	lsr.w	#5,d3
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d3.w),a1
	bsr.w	Print_Message

Handle_AddX_Abcd_SubX_Sbcd_Common:
	; Common to lots of instructions.
	lea	Reg_Data_Direct(pc),a4
	; Bit 3 determines if parameters are Dn or -(An).
	btst	#3,d5
	beq.s	.got_type
	lea	Reg_Addr_Pre_Indirect(pc),a4

.got_type:
	move.w	d5,d1
	andi.w	#7,d1
	jsr	(a4)
	vtputc	","
	move.w	d5,d1
	andi.w	#$E00,d1
	rol.w	#7,d1
	lea	(a3),a2
	jmp	(a4)
; ===========================================================================
Addmsg:	vtstring BLUE,"add"
Submsg:	vtstring BLUE,"sub"
; ===========================================================================
Dis_Shift_Rotate:						; %1110
	; Shift and rotate instructions.
	; Filter out invalid instructions on the MC68000.
	move.w	d5,d1
	andi.w	#$8C0,d1
	cmpi.w	#$8C0,d1
	beq.w	IllegalInstruction

	andi.w	#$C0,d1
	move.w	d5,d2
	cmpi.w	#$C0,d1						; Are we doing a register or memory shift?
	beq.s	.mem_shift					; Branch if memory shift
	; Register shift
	andi.w	#$18,d2
	lsr.w	#2,d2
	lsr.w	#5,d1
	bra.s	.get_msg
; ===========================================================================
.mem_shift:
	; Memory shift
	andi.w	#$600,d2
	lsr.w	#8,d2
	moveq	#-1,d1

.get_msg:
	lea	ShiftMessages(pc),a1
	adda.w	(a1,d2.w),a1
	bsr.w	Print_Message
	; Bit 8 indicates direction of shift.
	btst	#8,d5
	beq.s	.shiftr
	vtputc	"l",BLUE
	bra.s	.got_dir
; ---------------------------------------------------------------------------
.shiftr:
	vtputc	"r",BLUE

.got_dir:
	lea	WordSize(pc),a1
	tst.b	d1
	bmi.s	.got_size
	lea	SizeMsgs(pc),a1
	adda.w	(a1,d1.w),a1

.got_size:
	bsr.w	Print_Message

	tst.b	d1
	bmi.s	.MemoryShift

	move.w	d5,d2
	andi.w	#$E00,d2
	rol.w	#7,d2
	; Register shift
	btst	#5,d5
	bne.s	.reg_cnt
	; Static count
	vtputc	"#"
	tst.b	d2
	bne.s	.got_cnts
	addq.w	#8,d2

.got_cnts:
	itoa.w	d2,RED
	bra.s	.RegShift_Common

; ---------------------------------------------------------------------------
.reg_cnt:
	vtputc	"d",GREEN
	itoa.w	d2,GREEN

.RegShift_Common:
	vtput.w	d2
	vtputc	","
	move.w	d5,d1
	andi.w	#7,d1
	lea	(a3),a2
	bra.w	Reg_Data_Direct
; ===========================================================================
.MemoryShift:
	moveq	#1,d3						; Word operation
	move.w	d5,d1						; Effective address bits
	moveq	#0,d4						; No additional immediate parameters
	jsr	(a4)							; Rewind a3 to instruction+2 if needed
	lea	(a3),a2
	bra.w	Handle_effective_address
; ===========================================================================
ShiftMessages:	offsetTable
		offsetTableEntry.w ASdmsg
		offsetTableEntry.w LSdmsg
		offsetTableEntry.w ROXdmsg
		offsetTableEntry.w ROdmsg
ASdmsg:		vtstring BLUE,"as"
LSdmsg:		vtstring BLUE,"ls"
ROXdmsg:	vtstring BLUE,"rox"
ROdmsg:		vtstring BLUE,"ro"
; ===========================================================================
Line1010Emulator:						; %1010
Line1111Emulator:						; %1111
; Neither of these can cause address errors, so don't bother with the rewinding
; portion. In systems where bus errors are possible, this could be relevant.
	lea	(a3),a2
	lea	DeclareWord(pc),a1
	bsr.w	Print_Message
	move.w	d5,d0
	bra.w	Print_Word
; ===========================================================================
IllegalInstruction:
; This can't cause address errors, so don't bother with the rewinding portion.
; In systems where bus errors are possible, this could be relevant.
	lea	(a3),a2
	lea	DeclareWord(pc),a1
	bsr.w	Print_Message
	move.w	d5,d0
	bsr.w	Print_Word
	lea	IllegalInstructionMsg(pc),a1
	bra.w	Print_Message
; ===========================================================================
IllegalInstructionMsg:	vtstring RED ," ; Illegal instruction!"
DeclareWord:			vtstring BLUE,"dc.w     "

SizeMsgs:	offsetTable
		offsetTableEntry.w ByteSize
		offsetTableEntry.w WordSize
		offsetTableEntry.w LongSize
		offsetTableEntry.w NoSize
ShortSize:	vtstring BLUE,".s  "
ByteSize:	vtstring BLUE,".b  "
WordSize:	vtstring BLUE,".w  "
LongSize:	vtstring BLUE,".l  "
NoSize:		vtstring BLUE,"    "
; ===========================================================================
; Prints effective address for opcode
; Input:
; 	a2	Pointer to address just before extension word (if any)
; 	a5	Write destination (in RAM)
; 	d3	Instruction size (%00 = byte, %01 = word, %10 = long, all others = invalid)
; 	d5	Instruction register
; Output:
; 	a2	Pointer to address just after extension word (if any)
Handle_effective_address:
	move.w	d5,d0
	move.w	d5,d1

Handle_effective_address_Part2:
	andi.w	#$38,d0
	lsr.w	#2,d0
	andi.w	#$7,d1
	move.w	EACodeMap(pc,d0.w),d0
	jmp	EACodeMap(pc,d0.w)
; ===========================================================================
EACodeMap:	offsetTable
	offsetTableEntry.w	Reg_Data_Direct       	; %000
	offsetTableEntry.w	Reg_Addr_Direct       	; %001
	offsetTableEntry.w	Reg_Addr_Indirect     	; %010
	offsetTableEntry.w	Reg_Addr_Post_Indirect 	; %011
	offsetTableEntry.w	Reg_Addr_Pre_Indirect  	; %100
	offsetTableEntry.w	Reg_Addr_Disp_Indirect 	; %101
	offsetTableEntry.w	Reg_Addr_Index_Indirect	; %110
	offsetTableEntry.w	Misc_EA_Modes          	; %111
; ===========================================================================
Reg_Data_Direct:
	;Dn
	vtputc	"d",GREEN
	itoa.w	d1,GREEN
	vtput.w	d1
	rts
; ===========================================================================
Reg_Addr_Direct:
	cmpi.b	#7,d1
	beq.s	.print_sp
	;An
	vtputc	"a",GREEN
	itoa.w	d1,GREEN
	vtput.w	d1
	rts
; ---------------------------------------------------------------------------
.print_sp:
	vtputs	"sp",GREEN
	rts
; ===========================================================================
Reg_Addr_Indirect:
	;(An)
	vtputc	"("
	bsr.s	Reg_Addr_Direct
	vtputc	")"
	rts
; ===========================================================================
Reg_Addr_Post_Indirect:
	;(An)+
	bsr.s	Reg_Addr_Indirect
	vtputc	"+"
	rts
; ===========================================================================
Reg_Addr_Pre_Indirect:
	;-(An)
	vtputc	"-"
	bra.s	Reg_Addr_Indirect
; ===========================================================================
Reg_Addr_Disp_Indirect:
	;d16(An)
	move.w	(a2)+,d0
	beq.s	Reg_Addr_Indirect
	bsr.w	Print_Word_Signed
	bra.s	Reg_Addr_Indirect
; ===========================================================================
Reg_Addr_Index_Indirect:
	;d8(An,Xn.S)
	move.w	(a2)+,d0
	tst.b	d0
	beq.s	.skip_null
	bsr.w	Print_Byte_Signed

.skip_null:
	vtputc	"("
	bsr.s	Reg_Addr_Direct
	vtputc	","

Common_Index_Indirect:
	move.w	d0,d1
	andi.w	#$7000,d1
	rol.w	#4,d1
	lea	Reg_Data_Direct(pc),a0
	btst	#15,d0
	beq.s	.not_addr
	lea	Reg_Addr_Direct(pc),a0

.not_addr:
	jsr	(a0)
	vtputc	"."
	btst	#11,d0
	beq.s	.size_w
	vtputc	"l",GREEN
	bra.s	.got_size
; ---------------------------------------------------------------------------
.size_w:
	vtputc	"w",GREEN

.got_size:
	vtputc	")"
	rts
; ===========================================================================
Misc_EA_Modes:
	lsl.w	#1,d1
	move.w	MiscEACodeMap(pc,d1.w),d1
	jmp	MiscEACodeMap(pc,d1.w)
; ===========================================================================
MiscEACodeMap:	offsetTable
	offsetTableEntry.w	Abs_Short         	; %000
	offsetTableEntry.w	Abs_Long          	; %001
	offsetTableEntry.w	PC_Disp_Indirect  	; %010
	offsetTableEntry.w	PC_Index_Indirect 	; %011
	offsetTableEntry.w	Immediate         	; %100
	offsetTableEntry.w	InvalidAddrMode   	; %101
	offsetTableEntry.w	InvalidAddrMode   	; %110
	offsetTableEntry.w	InvalidAddrMode   	; %111
; ===========================================================================
Abs_Short:
	vtputc	"("
	move.w	(a2)+,d0
	ext.l	d0
	bsr.w	Print_Address
	vtputs	").w",BLUE
	rts
; ===========================================================================
Abs_Long:
	vtputc	"("
	move.l	(a2)+,d0
	bsr.w	Print_Address
	vtputs	").l",BLUE
	rts
; ===========================================================================
PC_Disp_Indirect:
	;d16(PC)
	move.w	(a2)+,d0
	ext.l	d0
	add.l	a2,d0
	subq.l	#2,d0
	bsr.w	Print_Address
	vtputs	"(pc)",GREEN
	rts
; ===========================================================================
PC_Index_Indirect:
	;d8(PC,Xn.S)
	move.w	(a2),d0
	ext.w	d0
	ext.l	d0
	add.l	a2,d0
	bsr.w	Print_Address
	move.w	(a2)+,d0
	vtputs	"(pc,",GREEN
	bra.w	Common_Index_Indirect
; ===========================================================================
Immediate:
	;#value
	vtputc	"#"
	cmpi.b	#2,d3
	bhs.s	LongImmed
	move.w	(a2)+,d0
	tst.b	d3
	beq.w	Print_Byte
	bra.w	Print_Word
; ===========================================================================
LongImmed:
	move.l	(a2)+,d0
	bra.w	Print_Long
; ===========================================================================
InvalidAddrMode:						; %101,%110,%111
	lea	InvalidAddrModeMsg(pc),a1
	bra.w	Print_Message
; ===========================================================================
InvalidAddrModeMsg:	vtstring BLUE,"\x7FInvalid address mode\x7F"
; ===========================================================================

