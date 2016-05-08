; Quikman ROM cartridge for Commodore VIC20
; written by Robert Hurst <robert@hurst-ri.us>
; originally used Commodore VICMON (SYS 45056)
; ported using the CA65 assembler (cc65.org)
; updated version: 13-Dec-2008
;
; to assemble this source using cc65.org project:
;	ca65.exe --cpu 6502 --listing quikman-rom.s
;	ld65.exe -C doc/vic20-cartA.cfg -o quikman-rom.a0 quikman-rom.o
;
; to run the binary using viceteam.org project:
;	xvic -memory none -ntsc -sound -joydev1 2 -cartA quikman-rom.a0
; to run the binary using mess.org project:
;	mess -view "Pixel Aspect (25:31)" vic20 -cart1 quikman-rom.a0
;
; pertinent VIC20 symbols
JIFFYH		= $A0		; jiffy-clock hi byte value
JIFFYM		= $A1		; jiffy-clock mid byte value
JIFFYL		= $A2		; jiffy-clock low byte value
CLRPAGE		= $F4		; color memory page (unexpanded = $97)
SCRNPAGE	= $0288		; screen memory page (unexpanded = $1E)
CTRLCSHIFT	= $028D		; keyboard flag: control/commodore/shift
CASSBUFF	= $033C		; cassette buffer
VIC			= $9000		; start of video interface chip registers
RESET		= $FD22		; warm startup
CHROUT		= $FFD2		; print character with cursor translation
GETIN		= $FFE4		; get a character from keyboard queue
;
; my symbol / memory map
;			= $00		; base index (monster, sprite)
;			= $01		; index x2
;			= $02		; index x8
PPILLTIMER	= $10		; powerpill effectiveness timer
FRUITTIMER	= $39		; 0 - 250
FRUITFLAG	= $3A		; zero or non-zero, if fruit has been activated
PPILLFLAG	= $3B		; just ate a powerpill this turn (0=no)
CHOMP		= $3C		; pointer into sound effect for fruit and fleeing monsters
CHEWING		= $3D		; flag whether quikman just ate a dot or not
DIGIT		= $3E		; award points at this digit place
POINTS		= $3F		; how many points scored
OLDDIR		= $40		; direction sprite was last moving in
NEWDIR		= $41		; direction sprite wants to take, if valid by MAZEMOVE
JOYVAL		= $42		; last joystick read value
QMANDIR		= $43		; quikman's current direction (0=right,1=down,2=left,3=up)
FRAME		= $44		; frame number
LIVES		= $45		; 0 - 3
FLASHPILL	= $46		; powerpill blink counter (0-30)
EXTRAQMAN	= $47		; bonus quickman flag (0=unused)
DEMOQMAN	= $48		; spirit of quickman index (0-3)
FRUITLEVEL	= $49		; 0 - 12
DOTS		= $4A		; 0 - 170
PENALTY		= $4B		; $4B-$4E monsters are free-to-roam flag
;			= $4F		; $4F-$52 monsters current direction (0=right,1=down,2=left,3=up)
;			= $53		; $53,$55,$57,$59 monster's knowledge of quikman's "X" coord was
;			= $54		; $54,$56,$58,$5A monster's knowledge of quikman's "Y" coord was
MONMOVE		= $61		; $61-$64 monster array for its next best move
;			= $69		; temporary var
FLEEINGSCORE= $70		; fleeing monster score: 2, 4, 8, 16
;			= $F7		; $F7/$F8,$F9,$FA are screen cell pointers for sprite's position
;			= $FE		; $FC/$FD,$FE/$FF are color cell pointers for same
;
; program indirects (my sprite registers)
SPRITE		= $02A1		; bitmask 0-7 controls sprite on/off
SPRITEX		= $02A2		; $02A2,A4,A6,A8,AA,AC,AE,B0 each "X" coordinate
SPRITEY		= $02A3		; $02A3,A5,A7,A9,AB,AD,AF,B1 each "Y" coordinate
SPRITECLR	= $02B2		; $02B2-$02B9 color
SPRITEIMG1	= $02BA		; $02BA-$02C1 low-byte of SPRITE image
SPRITEIMG2	= $02C2		; $02C2-$02C9 hi-byte of SPRITE image
SPRITELAST	= $02CC		; $02CC-$02DC keep last state of SPRITE registers
SAVEBACK	= $02DD		; $02DD-$02E6 keep what's under the sprite's 2-cell
;
; other constants
FRUITCELL	= $1F1B		; screen cell address of fruit
FRUITCELLCLR= $971B		; color cell address of fruit
;
; uses standard VIC20 (unexpanded)
		.org $A000
		.segment "STARTUP"
;
;*********************************************************************
; Commodore Cartridge boot sequence
; load address of startup & restore key:
		.word	MAIN
		.word	NMI
;				power-up signature: A0CBM
		.byte	$41, $30, $C3, $C2, $CD
;
;*********************************************************************
; RESTORE key was pressed by a frustrated player ...
NMI:
		CLD
		PLA				; Y
		TAY
		PLA				; X
		TAX
		PLA				; A
		JMP RESTART
;
;*********************************************************************
; Starting entry point for this program
MAIN:
		; initialize VIC into usable state
		JSR $FD8D	; ramtas	Initialise System Constants (memory pointers)
		JSR $FDF9	; ioinit	Initialise I/O (timers are enabled)
		JSR $E518	; cint1		Initialize I/O (VIC reset, must follow ramtas)
		JSR $FF8A	; restor	Restore Vectors (required)
		;JSR $FF8D	; vector	Change Vectors For User (not required)
		;LDA #$7F
		;STA $911E	; disable NMI, so no reset key wanted?
		; my VIC chipset init
		LDA VIC+$01
		SBC #$04		; adjust top scan line to accomodate extra row
		STA VIC+$01
		LDA #$80+$15	; set for videoram @ $1E00 with 21-columns
		STA VIC+$02		; video matrix address + columns
		LDA #$B0		; $B0 = 10110000 = 24 rows + 8x8 height
		STA VIC+$03		; rows / character height
		LDX #$FF		; set for $1C00
		STX VIC+$05		; use programmable char set
;
;*********************************************************************
; SOFT reset entry point
RESTART:
		; my interrupt vector init
		SEI
		LDX #<BACKGROUND
		LDY #>BACKGROUND
		STX $0314
		STY $0315
		CLI
		; seed BASIC RAM with game data
		LDA #<MAZEDATA
		STA $FC
		LDA #>MAZEDATA
		STA $FD
		LDA #$00
		STA $FE
		LDA #$1A
		STA $FF
		LDY #$00
@copy:	LDA ($FC),Y
		STA ($FE),Y
		INY
		BNE @copy
		INC $FD
		INC $FF
		LDA $FF
		CMP #$1E
		BNE @copy
		TYA
@snd:	STA VIC+$0A,Y	; reset sound channels
		INY
		CPY #$04
		BNE @snd
		LDA #$8F		; brown & highest
		STA VIC+$0E		; auxiliary color & volume
		JSR RESTORE
		LDX #$15
@msg:	LDA BANNERMSG,X
		STA $1FE3,X
		LDA #$03
		STA $97E3,X
		DEX
		BPL @msg
		LDA #$09		; brown button, white font, just like the F-key
		STA $97E3+$0E
		STA $97E3+$0F
;
DEMO:
		JSR INITVARS
		LDY #$00
		STY LIVES
		JSR GAMEOVER
		LDY #$0A
		STY SPRITE
@loop:	LDA FRAME
		AND #$7F
		BNE @slow
		LDX VIC+$0F
		INX
		TXA
		AND #$07
		TAX
		ORA #$08
		STA VIC+$0F
		JSR MAZEPAINT
@skip:	INC FRUITLEVEL
		LDX FRUITLEVEL
		CPX #$0D
		BCC @fruit
		STX PPILLFLAG	; demo powerpill
		LDX #$00
		STX FRUITLEVEL
@fruit:	LDA FRUIT,X
		STA FRUITCELL
		LDA FRUITCLR,X
		STA FRUITCELLCLR
		LDA #$40
		STA FRUITTIMER
		STA FRUITFLAG
@slow:	LDX JIFFYL
		INX
		INX
@wait:	CPX JIFFYL
		BNE @wait
		LDA SPRITE
		EOR #$1E
		STA SPRITE
		JSR NPC			; demo mode
@scan:	JSR SPRITES
		JSR GETIN		; get keyboard
		CMP #$85		; got F1 ?
		BEQ RESETGAME	; try again ...
		LDA #$FF
		STA $9122
		LDA $9111
		AND #$20		; FIRE
		BNE @loop
;
;*********************************************************************
RESETGAME:
		LDA #$09
		STA CHOMP
		LDA #$03		; start with 3-lives
		STA LIVES
		LDY #$FF		; -1 will become 0 at start of "next" level
		STY FRUITLEVEL
		JSR GAMEOVER	; clear status
		LDX #$00		; reset score
		STX EXTRAQMAN	; reset bonus
@loop1:	LDA #$B0		; each digit to "0"
		STA SCORE-$9200,X ; into savebuffer
		INX				; do next digit
		CPX #$06		; all 6 of them
		BNE @loop1
;
STARTLVL:
		JSR RESTORE		; initialize new level
;
RESETCHR:
		JSR INITVARS
		LDA #$1C		; 1st page where quikman is on
		STA SPRITEIMG2
		LDA #$A8		; start quikman off with a smug smile
		STA SPRITEIMG1
		LDA #$1F		; turn on sprites 0-4
		STA SPRITE
		LDX #$0C
@smug:	LDA #$08
		JSR PAUSE		; then he sees there are monsters ...
		DEX
		BPL @smug
		LDX #$05
@loop2:	LDA $C377,X		; print READY from ROM
		AND #$BF
		ORA #$80
		STA $1F18,X
		LDA #$07		; make it yellow
		STA $9718,X
		LDA #$98		; restore monster caged image (heh)
		STA SPRITEIMG1,X
		DEX
		BNE @loop2		; how geeky is that?
		LDA #$C8		; quikman gets ready
		STA SPRITEIMG1
		LDX #$18
@ready:	LDA #$08
		JSR PAUSE		; wait 2+ seconds
		DEX
		BPL @ready
		LDX #$00
		LDA #$20		; erase READY
@loop4:	STA $1F19,X
		INX
		CPX #$05
		BNE @loop4
;
; zero $39 - $43
ZEROVARS:
		LDX #$00
		STX FRUITFLAG
		STX PPILLFLAG
		STX CHEWING
		LDX #$02
		STX QMANDIR		; start off going LEFT
		STX JOYVAL		; preload last joystick value as going LEFT
;
;*********************************************************************
PLAYLOOP:
		LDA CTRLCSHIFT	; is the player holding down any
		BNE PLAYLOOP	; control, commodore, shift key(s)?
		LDA FRUITCELL
		CMP #$21		; is there fruit on display?
		BCC @warp
		LDX FRUITLEVEL
		LDA FRUITCLR,X	; restore fruit color
		STA FRUITCELLCLR
@warp:	LDA #$0C		; 10-levels to warp
		SEC
		SBC FRUITLEVEL	; progressive speed
		BCC @cruz
		CMP #$03
		BCS @pace
@cruz:	LDA #$03
@pace:	TAY
@sleep:	INX
		BNE @sleep
		DEY
		BNE @sleep		; one 1000, two 1000, three 1000, ...
		STY $00			; quikman is sprite #0
		STY $01
		LDA QMANDIR
		STA OLDDIR		; save last direction quikman was going in
		LDX JOYVAL		; recall last joystick value
		STY $9113
		LDA #$7F
		STA $9122
		LDA $9120
		AND #$80		; JOY 3
		BNE @joy0
		LDX #$00
@joy0:	LDA #$FF
		STA $9122
		LDY $9111
		TYA
		AND #$08
		BNE @joy1
		LDX #$01
@joy1:	TYA
		AND #$10
		BNE @joy2
		LDX #$02
@joy2:	TYA
		AND #$04
		BNE @joy3
		LDX #$03
@joy3:	STX JOYVAL		; save
		TXA
		STA NEWDIR		; do the same for the joystick
		JSR MAZEMOVE
		BCS @skip1		; is the direction valid?
		LDA JOYVAL		; yes,
		STA QMANDIR		; request quikman to move in direction of joystick
		CLC
		BCC @skip2
@skip1:	LDA QMANDIR
		STA NEWDIR
		JSR MAZEMOVE	; keep the current direction going?
@skip2:	LDA SPRITEX
		BNE @skip3		; is quikman at end of tunnel left?
		LDA #$9E
		STA SPRITEX		; put quikman at beginning of tunnel right
@skip3:	CMP #$A0		; is quikman at end of tunnel right?
		BNE @skip4
		LDA #$00
		STA SPRITEX		; put quikman at beginning of tunnel left
@skip4:
		LDX #$B0		; closed mouth
		LDA SPRITEX
		ORA SPRITEY
		AND #$02
		BNE @anim
		LDA QMANDIR		; take 0=right, 1=down, 2=left, 3=up value
		ASL				; multiply by 8 to get address
		ASL
		ASL
		CLC
		ADC #$B8		; add base offset
		TAX
@anim:	STX SPRITEIMG1
;
		LDX #$00		; use X as a flag
		LDA SPRITEX
		AND #$07
		CMP #$04
		BNE @skip5		; is quikman in the middle of a left/right cell?
		INX
		LDA QMANDIR		; yes, 0=right, 2=left
		EOR #$02
		TAY
		BEQ @skip6		; going left, use 1st saveback cell
		DEY
		BNE @skip6		; going right, use overflow saveback cell
@skip5:	LDA SPRITEY
		AND #$07
		CMP #$03
		BNE @skip6		; is quikman in the middle of an up/down cell?
		INX
		LDY QMANDIR
		CPY #$01		; going down, use overflow saveback cell
		BEQ @skip6
		LDY #$00		; going up, use 1st saveback cell
@skip6:	CPX #$00
		BNE @skip7		; does quikman have something in its mouth?
		JMP NPCNEXT		; no, continue play
@skip7:	LDA SAVEBACK,Y	; retrieve the character from quikman's saveback buffer
		CMP #$20
		BNE @skip8
		JMP PLAYLOOP	; nothing to eat here, so move a bit faster
		; check what was just eaten ...
@skip8:	TAX				; save that something in X
		LDA #$20
		STA SAVEBACK,Y	; replace the cell quikman is on with an empty space
		CPX #$1E		; is it a dot?
		BNE POWERUP
		LDA #$01
		STA POINTS		; score 1
		STA CHEWING		; quikman has to chew this dot, monsters keep movin'
		LDA #$0A		; score it @ 10-point digit
		STA DIGIT
;
POWERUP:
		CPX #$21
		BCC @skip1		; is X < 33 ?
		CPX #$29		; no, is X >= 41 ?
		BCS NPCNEXT		; ate a piece of fruit?
		TXA				; YUMMY!
		SEC
		SBC #$21		; strip off char code for score index
		TAX
		LDA FRUITSCORE,X
		STA POINTS		; award points
		LDA #$09
		STA DIGIT		; in hundreds
		STA CHOMP
		CLC
		BCC NPCNEXT
@skip1:	CPX #$1F		; ate a powerpill?
		BNE EATING		; no, but I did eat a dot
		LDA #$05
		STA POINTS		; award 5-points
		LDA #$0A
		STA DIGIT		; score @ 10-digit
		STA PPILLFLAG
		BEQ NPCNEXT		; powerpills are dots on steroids, account for it
;
EATING:	INC DOTS		; ate a dot, account for it
		LDA DOTS
		CMP #$AA		; are all dots eaten?
		BNE NPCNEXT
;===	achieved end of level	===
WONLEVEL:
		LDA #$A8		; quikman ends with a smug smile
		STA SPRITEIMG1
		LDA #$80
		JSR PAUSE
		LDA #$00
		STA FRAME
@loop:	DEC FRAME
		LDA FRAME
		AND #$07
		TAX
		JSR MAZEPAINT
		LDA FRAME
		AND #$1F
		BNE @next
		LDA SPRITE
		EOR #$1E
		STA SPRITE		; blink monsters
@next:	LDX #$04
		LDA #$70		; ghost ends small
@tiny:	STA SPRITEIMG1,X
		DEX
		BNE @tiny
		JSR SPRITES
		LDA FRAME
		CMP #$06
		BNE @loop		; continue until true blue
		LDA #$40
		JSR PAUSE
		LDX #$03
@loop1:	LDA PENALTY,X
		LDY FRUITLEVEL
		INY
@loop2:	LSR
		DEY
		BNE @loop2
		STA PENALTY,X		; after each level, the monsters dispatch quicker
		DEX
		BPL @loop1
		JMP STARTLVL
;
NPCNEXT:
		JSR GETIN		; get keyboard
		CMP	#$03		; got STOP ?
		BEQ WONLEVEL	; CHEATER!!
		JSR NPC
		JSR SPRITES
		JMP PLAYLOOP
;
;*********************************************************************
; non-player characters & events
NPC:
		LDA FRUITFLAG
		BNE @skip3		; is fruit already on display?
		LDA DOTS
		CMP #$4B		; has the 75th dot been eaten?
		BEQ @skip1
		CMP #$7D		; has the 125th dot been eaten?
		BNE @skip3
@skip1:	LDX FRUITLEVEL	; prepare thy bonus
		CPX #$0C		; reach the last level?
		BCC @skip2
		LDX #$0C		; only the key is left, and it leaves a bad metallic after-taste
@skip2:	LDA FRUIT,X
		STA FRUITCELL	; display fruit
		LDA FRUITCLR,X
		STA FRUITCELLCLR
		LDA #$FA		; 250-moves and counting
		STA FRUITTIMER	; reset fruit timer
		STA FRUITFLAG
@skip3:	LDA FRUITTIMER	; fruit is on display
		BEQ @skip4		; nothing to do
		DEC FRUITTIMER	; remove a tick
		BNE @skip4		; there is still time left
		LDA #$20		; time's up!
		STA FRUITCELL	; no more fruit
@skip4:	LDA DOTS
		CMP #$4C		; has the 76th dot been eaten?
		BEQ @skip5
		CMP #$7E		; has the 126th dot been eaten?
		BNE @skip6
@skip5:	LDA #$00
		STA FRUITFLAG	; more fruit potential on this level
;
@skip6:	LDA PPILLFLAG
		BEQ KISSING		; just swallowed a powerpill?
		LDA #$00		; account for that action
		STA PPILLFLAG
		LDA #$02		; start scoring @ 200-points
		STA FLEEINGSCORE
		LDX FRUITLEVEL
		TXA
		AND #$07
		BEQ @break		; every 8-levels, keep timer up
		CPX #$10
		BCS @timer		; 16-levels of powerpill timing
		TXA
@break:	ASL				; x2
		ASL				; x2
		AND #$3F
		EOR #$3F		; invert A
		CPX #$05
		BCC @timer		; timer good to the 1st apple
		LSR				; 1/2
@timer:	STA PPILLTIMER	; set powerpill timer
		LDY #$04
@loop1:	LDX PENALTY-1,Y
		BNE @skip7		; is monster waiting in cage already?
		LDA #$06		; no, make monster blue
		STA SPRITECLR,Y
		LDA #$A0		; make monster fleeing (0)
		STA SPRITEIMG1,Y
		LDA $4E,Y
		EOR #$02		; and reverse its direction
		STA $4E,Y
@skip7:	DEY
		BNE @loop1
;
; check all monsters if any are in contact with quikman
KISSING:
		LDY #$08
KISSME:
		LDA LIVES
		BEQ NEXTKISS
		LDA SPRITEX
		CMP SPRITEX,Y
		BNE @skip3
		LDA SPRITEY
		SEC
		SBC SPRITEY,Y
		BCS @skip2
		EOR #$FF
@skip2:	CMP #$05
		BCS @skip3
		LDX #$FF
		BNE ENGAGED		; is quikman engaged with a monster?
@skip3:	LDA SPRITEY
		CMP SPRITEY,Y
		BNE NEXTKISS
		LDA SPRITEX
		SEC
		SBC SPRITEX,Y
		BCS @skip4
		EOR #$FF
@skip4:	CMP #$05
		BCC ENGAGED		; is quikman engaged with a monster?
;
NEXTKISS:
		DEY				; next monster
		DEY				; X,Y coord pair check
		BNE KISSME
		JMP MONSTERS	; quikman is still freely running!
;
ENGAGED:
		TYA
		LSR
		TAX
		LDA SPRITEIMG1,X
		CMP #$70		; is monster returning to cage?
		BEQ NEXTKISS
		CMP #$A0		; is monster fleeing?
		BNE DEAD		; no, quikman bites the dust
		LDA #$09		; ahah!  caught a little sickly one!
		STA DIGIT		; in hundreds
		LDA FLEEINGSCORE
		STA POINTS		; fleeing monster score
		ASL				; next is worth x2 bonus
		STA FLEEINGSCORE
		TYA
		PHA
		TXA
		PHA
		LDA #$D0
		STA VIC+$0A
		LDA SPRITE
		EOR SPRITEMASK,X
		STA SPRITE
		LDX PPILLTIMER
		LDA #$01
		JSR PAUSE
		LDA #$A8		; quikman smiles
		STA SPRITEIMG1
		LDA #$01
		JSR PAUSE
		LDA JIFFYL
		CLC
		ADC #$20
@loop:	LDY JIFFYL
		INY
@loop1:	CPY JIFFYL
		BNE @loop1
		STX PPILLTIMER
		INC VIC+$0A
		CMP JIFFYL
		BNE @loop		; wait up to a jiffy
		LDA #$00
		STA VIC+$0A
		PLA
		TAX
		PLA
		TAY
		LDA #$70		; monster runs back to cage
		STA SPRITEIMG1,X
		LDA SPRITE
		EOR SPRITEMASK,X
		STA SPRITE
		DEX
		LDA MONSTERCLR,X
		STA SPRITECLR+1,X
		JMP NEXTKISS	; is there another monster here?
;
DEAD:
		PLA				; remove quikman's call to NPC from stack
		PLA				; because he just died . . .
		LDX #$06
@oops:	LDA #$10
		JSR PAUSE
		DEX
		BPL @oops
		; death sequence
		LDA #$01		; only feature quikman dying
		STA SPRITE
		LDA #$B0		; low-order byte of 1st quikman image
		STA SPRITEIMG1
		LDX #$01
@loop:	LDA QUIKMANCLR,X ; reset monsters starting colors
		STA SPRITECLR,X ; into their sprite color registers
		LDA #$80		; reset monsters as looking down
		STA SPRITEIMG1,X
		INX
		CPX #$05
		BNE @loop
		LDA #$F0
		STA VIC+$0C
		LDA #$20
		STA FRAME		; rotate quikman 8 times
@loop1:	LDA SPRITEIMG1
		CMP #$D0		; are we at the 4th quikman image?
		BCC @skip
		INC VIC+$0C
		LDA #$B0		; reset to 1st quikman image
@skip:	CLC
		ADC #$08		; advance to next image
		STA SPRITEIMG1
		DEC VIC+$0C		; wooosh!
		DEC VIC+$0C
		DEC VIC+$0C
		LDA #$04
		JSR PAUSE
		DEC FRAME
		BNE @loop1		; repeat next sequence
		LDX #$D8
		STX SPRITEIMG1	; explode!
		LDA #$0A
		JSR PAUSE
		LDX #$E0
		STX SPRITEIMG1	; smoke!
		LDA #$08
		JSR PAUSE
		LDX #$E8
		STX SPRITEIMG1	; dust!
		LDA #$06
		JSR PAUSE
		LDA #$00
		STA SPRITE
		STA VIC+$0C
		LDA #$64
		JSR PAUSE
		DEC LIVES
		BEQ FINALITY	; any lives remaining?
		LDX LIVES
		LDA #$1B
		STA $1FE4,X		; avatar explodes
		LDA #$0A
		JSR PAUSE
		LDA #$1C
		STA $1FE4,X		; avatar smoking
		LDA #$08
		JSR PAUSE
		LDA #$1D
		STA $1FE4,X		; avatar dusted
		LDA #$01
		STA SPRITE
		LDA #$D8
		STA SPRITEIMG1
		JSR INITVARS
		LDA #$06
		JSR PAUSE
		LDX LIVES
		LDA #$20
		STA $1FE4,X		; erase the avatar
		JMP RESETCHR	; quikman still has life -- try again!
;
FINALITY:
		LDY #00
		STY SPRITE
		JSR GAMEOVER
		LDA #$F0		; 4-second pause
		JSR PAUSE
		JMP DEMO		; this game is really over now
;
;*********************************************************************
MONSTERS:
		LDA #$04
		STA $00			; start with monster #4
;
DOMONSTER:
		LDA $00
		TAY
		ASL				; x2
		STA $01
		LDX PENALTY-1,Y
		BEQ ITMOVES		; is this monster free to roam?
		LDA FRAME
		AND #$03
		BEQ @anim
		DEX				; no, countdown to freedom
		STX PENALTY-1,Y
@anim:	LDA #$98		; reset monster as caged, but coming out
		CPX #$10
		BCC @eyes
		TXA
		AND #$08
		BNE @skip
		LDA #$78		; reset monster as caged looking right
		BNE @eyes
@skip:	LDA #$88		; reset monster as caged looking left
@eyes:	STA SPRITEIMG1,Y
		LDX $01
		LDA SPRITEX,X
		AND #$F8
		STA SPRITEX,X
;
NEXTMONSTER:
		DEC $00			; process next monster
		BNE DOMONSTER
		INC FRAME
		INC $00
		LDA SPRITEY+$02
		CMP #$58		; no bonus at cage row
		BEQ @fini			
		AND #$07
		ORA SPRITEX+$02
		AND #$0F
		BEQ DOMONSTER
		LDA CHEWING
		BNE @skip1		; is quikman eating a dot?
@fini:	RTS				; no, we're done
@skip1:	LDA DOTS
		AND #$01
		BNE @fini		; quikman can swallow every other dot faster
		JSR SPRITES		; yes, chasing monsters get another turn
		JMP MONSTERS
;
ITMOVES:
		LDA SPRITEIMG1,Y
		CMP #$70
		BNE @go			; small ghost going home?
		LDX $01			; yes, get pairing index
		LDA SPRITEX,X
		CMP #$50
		BNE @caged
		LDA SPRITEY,X
		CMP #$48
		BNE @caged		; is monster above cage ($50,$48 coord) doorway ?
		INC SPRITEY,X	; move into doorway
		LDX #$01
		STX $4E,Y		; make direction DOWN to go into cage
@caged:	LDA SPRITEY,X
		CMP #$58
		BNE @skip1		; is monster at cage row level?
		TYA
		ASL
		ASL
		ASL
		CLC
		ADC #$40
		CMP SPRITEX,X	; is monster inside cage?
		BNE @skip1
		LDA #$98		; restore monster caged image
		STA SPRITEIMG1,Y
		LDA CAGEDATA-1,Y
		EOR #$FF
		LSR
		ADC #$20		; compute waiting room time
		STA PENALTY-1,Y	; monster is waiting
		JMP NEXTMONSTER	; done moving
@go:	LDA FRAME
		AND #$01
		BEQ @cont		; check for powerpill active?
		LDA SPRITEIMG1,Y
		CMP #$A0		; this monster IS fleeing
		BEQ NEXTMONSTER	; skip its turn
@cont:	LDX $01			; no, get pairing index
		LDA SPRITEX,X
		CMP #$50
		BNE @skip1
		LDA SPRITEY,X
		CMP #$58
		BNE @skip1		; is monster in cage ($50,$58 coord) doorway ?
		DEC SPRITEY,X	; move it a pixel UP to force it through the closed door
		LDX #$03
		STX $4E,Y		; make direction UP to get out of cage
		BNE @skip3
@skip1:	LDA SPRITEX,X
		BNE @skip2		; is monster against the left-side of the tunnel?
		LDX $00
		STA $4E,X		; force a change of direction to the right
@skip2:	CMP #$9F		; is monster against the right-side of the tunnel?
		BNE @skip3
		LDX $00
		LDA #$02
		STA $4E,X		; force a change of direction to the left
@skip3:	LDY #$00
		LDX #$04
@loop1:	STX MONMOVE-1,Y	; preset move priority as 0=right,1=down,2=left,3=up
		INY
		DEX
		BNE @loop1
		LDY $01			; start of monster's calculated move
		LDA SPRITEX,Y
		AND #$07
		BEQ @skip4		; is monster horizontally aligned with a screen cell?
		LDA SPRITEY,Y
		AND #$07
		BNE @skip5		; is monster vertically aligned with a screen cell?
@skip4:	JSR AI			; yes, check to see if a direction change is in its future
		CLC
		BCC @skip6
@skip5:	LDX $4E,Y		; not in a position to make a direction change,
		STX $61			; so just keep monster going in its current direction
@skip6:	LDY #$00
		STY $04
@loop2:	LDX $61,Y
		TXA
		LDX $00
		EOR $4E,X
		CMP #$02
		BEQ @skip7		; don't allow monsters to reverse direction on their own
		LDX $61,Y
		STX NEWDIR
		LDY $00
		LDX $4E,Y
		STX OLDDIR
		JSR MAZEMOVE	; validate
		BCC MAKEMOVE	; is this a good move?
@skip7:	INC $04
		LDY $04
		CPY #$04
		BNE @loop2
		LDY $00			; reverse direction
		LDA OLDDIR
		EOR #$02
		STA $4E,Y
		JMP NEXTMONSTER
;
MAKEMOVE:
		LDY $00			; commit to this move
		LDX NEWDIR
		STX $4E,Y		; save as monster's current direction
		LDA SPRITEIMG1,Y
		CMP #$70		; running back to cage
		BEQ @mommy		; to give birth again
		CMP #$A0		; fleeing from quikman
		BEQ @anim
		TXA
		ASL				; multiply by 8 to get address
		ASL
		ASL
		CLC
		ADC #$78		; add base offset
@anim:	STA SPRITEIMG1,Y
@jmp:	JMP NEXTMONSTER
@mommy:
		LDY $01			; start of monster's calculated move
		LDA SPRITEX,Y
		ORA SPRITEY,Y
		AND #$02
		BEQ @jmp
		JMP ITMOVES
;
; monster's artificial intelligence
AI:
; first, preload $61-$64 with "best" moves this monster can make
; to give quikman the kiss of death
		LDX $01
		LDA $51,X		; retrieve this monster's "X" knowledge where quikman was
		CMP SPRITEX,X	; aligned?
		BNE @math1		; nope, compute intersect
		LDA JIFFYL
		AND #$01
		BEQ @skip1		; flip a coin
@math1:	SEC
		SBC SPRITEX,X
		BCS @skip1
		LDY #$02
		STY MONMOVE		; LEFT is better
		LDY #$00
		STY $64			; RIGHT is worse
		BEQ @skip2
@skip1:	LDY #$00
		STY MONMOVE		; RIGHT is better
		LDY #$02
		STY $64			; LEFT is worse
@skip2:	LDA $52,X		; retrieve this monster's "Y" knowledge where quikman was
		CMP SPRITEY,X	; aligned?
		BNE @math2		; nope, compute intersect
		LDA JIFFYL
		AND #$01
		BEQ @skip3		; flip a coin
@math2:	SEC
		SBC SPRITEY,X
		BCS @skip3
		LDY #$03
		STY $62			; UP is 2nd best
		LDY #$01
		STY $63			; DOWN is 3rd best
		BNE AI2
@skip3:	LDY #$01		; DOWN is 2nd best
		STY $62
		LDY #$03		; UP is 3rd best
		STY $63
;
; next, prioritize monster move, based upon its current location in respect to
; its knowledge where quikman was considered last.
AI2:	LDX $01
		TXA
		ASL
		ASL
		ASL				; x8
		CMP JIFFYL
		BCS @skip3		; ignore priority during this time window
		LDA $51,X
		SEC
		SBC SPRITEX,X
		BCS @skip1
		EOR #$FF
@skip1:	STA $69
		LDA $52,X
		SEC
		SBC SPRITEY,X
		BCS @skip2
		EOR #$FF
@skip2:	CMP $69
		BCC @skip3		; can monster improve upon order of choices?
		LDX MONMOVE		; swap 1st & 2nd choices
		LDY $62
		STX $62
		STY MONMOVE
		LDY $63			; swap 3rd & 4th choices
		LDX $64
		STY $64
		STX $63
@skip3:	LDY $00
		LDA SPRITEIMG1,Y
		CMP #$A0		; is this monster fleeing?
		BNE @fini		; no, chase!
		LDX MONMOVE		; swap 1st & 3rd choices
		LDY $63
		STX $63
		STY MONMOVE
@fini:	RTS
;
; reset monsters to default starting location
INITVARS:
		LDX #$00
@loop1:	LDA STARTPOS,X	; reset each sprite starting position
		STA SPRITEX,X
		INX
		CPX #$0A		; 5 sprites per X,Y coordinate pair
		BNE @loop1
		LDY #$00
@loop2:	LDX CAGEDATA,Y
		STX PENALTY,Y
		INY
		CPY #$10
		BNE @loop2
		RTS
;
; restore sound/screen
RESTORE:
		INC FRUITLEVEL
		LDA #$00
		STA SPRITE		; turn off all sprites
		LDA #$0E		; black / blue
		STA VIC+$0F		; background / border color
		LDA #$93		; Shift-HOME is clearscreen
		JSR	CHROUT		; print it
		LDX #$06		; blue
		JSR MAZEPAINT
		LDX #$15		; skip 1st & last row
@draw:	LDA MAZEDATA,X
		STA $1E00,X
		LDA MAZEDATA+$E3,X
		STA $1EE3,X
		INX
		BNE @draw
		STX DOTS		; and no dots are eaten (yet)
@loop2:	LDA QUIKMANCLR,X ; reset monsters starting colors
		STA SPRITECLR,X ; into their sprite color registers
		LDA #$80		; reset monsters as looking down @ quikman
		STA SPRITEIMG1,X
		LDA #$1C
		STA SPRITEIMG2,X
		INX
		CPX #$05
		BNE @loop2
		LDY LIVES		; paint lives remaining
		JSR GAMEOVER
		DEY
		BEQ @next
@loop4:	LDA #$19		; quikman character
		STA $1FE4,Y		; bottom-left of screen
		LDA #$07		; use yellow
		STA $97E4,Y		; and paint it
		DEY
		BNE @loop4
@next:	LDY FRUITLEVEL
		LDX #$00
@loop5:	CPY #$0C		; are we at the last level (key)?
		BCC @skip1
		LDY #$0C		; only keys remain
@skip1:	LDA FRUIT,Y		; fruit character
		STA $1FF1,X		; bottom right of screen
		LDA FRUITCLR,Y	; get its color
		STA $97F1,X		; and paint it
		CPY #$00		; did we paint the cherry yet?
		BEQ BEGIN		; if so, we're done
		INX
		STX $FF
		LDA FRUITLEVEL
		SEC
		SBC $FF
		TAY
		CPX #$07		; no more than 7 fruits to display
		BNE @loop5
BEGIN:	;LDA LIVES		; allow new screen to be processed
;		BEQ @cont
;		LDA #$40
;@cont:	JSR PAUSE		; by player
		RTS
;
; recolor maze with some new paint in X
MAZEPAINT:
		LDY #$15
@loop:	LDA MAZEDATA,Y
		CMP #$29
		BCC @page2
		CMP #$3F
		BCS @page2
		LDA #$01
		TXA
		STA $9600,Y
@page2:	LDA MAZEDATA+$E3,Y
		CMP #$29
		BCC @skip
		CMP #$3F
		BCS @skip
		TXA
		STA $96E3,Y
@skip:	INY
		BNE @loop
		RTS
;
; if move is valid, carry flag will be clear on return
MAZEMOVE:
		LDY $01			; get X,Y coord index
		LDA OLDDIR		; get the last direction moving
		AND #$01		; mask UP/DOWN
		BEQ @skip1		; is direction LEFT/RIGHT?
		INY				; no, then fetch the "Y" coordinate
@skip1:	LDA SPRITEX,Y	; get one of sprite's coord
		AND #$07
		BEQ MAZEANY		; at a crossroad?  check move in any 4-directions
		LDA NEWDIR
		CMP OLDDIR
		BEQ MYMOVE		; still want to move in the same direction?
		EOR OLDDIR
		CMP #$02
		BEQ MYMOVE		; is this a reverse direction request?
		SEC				; no new move made
		RTS
;
MAZEANY:
		JSR SPRITEPREP
		LDA $F8			; reset screen hi-byte back into saved maze data
		SEC
		SBC #$04
		STA $F8
		LDX NEWDIR
		CPX #$02
		BCS @skip2		; is X (2=left) or (3=up)?
		LDA $F7			; no
		CLC
		ADC PEEKAHEAD,X	; look (0=right) or (1=down)
		BCC @skip1
		INC $F8
@skip1:	STA $F7
		CLC
		BCC @skip4		; go validate
@skip2:	LDA $F7
		SEC
		SBC PEEKAHEAD-2,X
		BCS @skip3		; look (2=left) or (3=up)
		DEC $F8
@skip3:	STA $F7
@skip4:	LDY #$00		; validate
		LDA ($F7),Y
		CMP #$29		; is this direction into a maze wall?
		BCC MYMOVE		; good move?
		RTS
;
; continue this sprite's move in whatever is loaded in NEWDIR
MYMOVE:
		LDA NEWDIR
		ASL				; 0=0, 1=2, 2=4, 3=6, 4=8
		TAX
		LDY $01
		LDA INERTIA,X
		CLC
		ADC SPRITEX,Y
		STA SPRITEX,Y
		LDA INERTIA+1,X
		CLC
		ADC SPRITEY,Y
		STA SPRITEY,Y
		CLC
		RTS
;
;*********************************************************************
; my very own sprite routines
; major custom hack for this maze game implementation
SPRITES:
		LDA #$00		; start with sprite #0
		STA $00			; current sprite # to render
@loop1:	ASL
		STA $01			; current sprite (x2) pairing index
		ASL
		ASL
		STA $02			; current sprite (x8) image index
		LDX $00
		LDA SPRITE
		AND SPRITEMASK,X
		BEQ @skip2		; nothing to do?
		LDA SPRITELAST	; what state was this sprite before?
		AND SPRITEMASK,X
		BEQ @skip1		; it was "off"
		JSR ERASESPRITE	; was "on" before, and we still want it "on"
@skip1:	JSR SPRITEPREP	; new sprite, go turn it "on"
		JSR PREPMATRIX
		JSR RENDER
		JSR PLACEMATRIX
		JMP @next
@skip2:	LDA SPRITELAST
		AND SPRITEMASK,X
		BEQ @next		; still nothing to do?  Then do nothing ...
		JSR ERASESPRITE	; make this sprite disappear
@next:	INC $00
		LDA $00
		CMP #$05		; only 5-sprites needed in this game
		BNE @loop1
		LDX #$00
@loop2:	LDA SPRITE,X	; save copy of current sprite registers
		STA SPRITELAST,X
		INX
		CPX #$11		; all 17 values, not including colors
		BNE @loop2
		RTS				; fini
;
; remove sprite from screen
ERASESPRITE:
		JSR LASTSPRITEPREP
		JSR PREPMATRIX
		JSR RESTOREMATRIX
		RTS
;
LASTSPRITEPREP:
		LDA $01			; 0, 2, 4, 6, 8
		CLC
		ADC #<SPRITELAST
		BNE SPRITEPREP2
;
; prepares the following registers:
; $F7/$F8,$F9/$FA		screen cell pointers for sprite position
; $FC/$FD,$FE/$FF		color cell pointers for same
SPRITEPREP:
		LDA $01			; 0, 2, 4, 6, 8 index
		CLC
		ADC #<SPRITE
SPRITEPREP2:
		TAX				; save this register index
		LDA $0201,X		; get "X" coordinate
		CMP #$A0
		BCC @skip1		; is "X" at or beyond last column?
		SBC #$A0		; yes, subtract 160-pixels wide
@skip1:	LSR				; and divide by 8-pixel width
		LSR
		LSR
		STA $F7			; save column offset from left screen
		STA $FC			; save column offset from left color
		LDA SCRNPAGE	; get high order byte of screen memory page
		STA $F8
		LDA CLRPAGE		; get high order byte of screen color page
		AND #$FE		; make it and "even" number
		STA $FD			; save high order
		LDA $0202,X		; get "Y" coordinate
		CMP #$B8
		BCC @skip2		; is "Y" at or beyond last row?
		SBC #$B8		; yes, subtract 184-pixels high
@skip2:	LSR				; and divide by 8-pixel height
		LSR
		LSR
		TAY
		BEQ @fini		; if on top row, no math required
		LDA $F7			; get column offset
@loop1:	CLC
		ADC #$15		; add 21 for next row
		BCC @skip3		; overflow to next page?
		INC $F8			; yes, increment high order bytes
		INC $FD
@skip3:	STA $F7			; save column offset
		STA $FC
		DEY
		BNE @loop1		; do for each "row"
@fini:	LDA $F8			; copy high-order bytes
		STA $FA			; for overflow sprite character
		LDA $FD			; do the same
		STA $FF			; for color
; determine whether overflow character is to the right or down
		LDA $0201,X		; get "X" coordinate
		AND #$07
		BEQ @vert		; 0 assumes moving up/down?
		LDA $F7			; ok, moving left/right then ...
		CLC
		ADC #$01		; make overflow character to the right
		BCC @savel
@saveh:	INC $FA
		INC $FF
@savel:	STA $F9			; save character offset
		STA $FE			; save color offset
		RTS
@vert:	LDA $F7
		CLC
		ADC #$15		; make overflow character below
		BCC @savel
		BCS @saveh
;
; prepares saveback buffers for restoring, should a larger-numbered sprite be
; overlapping any part of a smaller-numbered sprite
PREPMATRIX:
		LDY #$00
		LDA ($F7),Y		; retrieve screen cell
		PHA
		LDY $01			; 0, 2, 4, 6, 8 index
		LDA $0201,X
		AND #$07
		BNE @2cell
		LDA $0202,X
		AND #$07
		BEQ @start
@2cell:	LDY #$00
		LDA ($F9),Y		; retrieve overflow cell
		PHA
		LDY $01			; 0, 2, 4, 6, 8 index
		INY
@start:	TYA
		TAX
@loop:	PLA
@retry:	CMP $01
		BCC @skip		; is A < ME ?
		CMP #$0A
		BCS @skip		; is A >= MAX ?
; there is a sprite # greater than us on top ...
		TAY
		LDA SAVEBACK,Y	; get > sprite# saveback info
		CLC
		BCC @retry
@skip:	STA SAVEBACK,X
		DEX
		CPX $01
		BEQ @loop
		RTS
;
; restores the sprite's saveback buffer to the screen squares it occupies
; erasure part 3
RESTOREMATRIX:
		LDX $01
		LDA SAVEBACK,X	; recover character
		LDY #$00
		STA ($F7),Y		; restore to screen
		LDA #$01
		STA ($FC),Y		; just leave "white" behind
		LDA SPRITELAST+1,X
		AND #$07
		BNE @2cell
		LDA SPRITELAST+2,X
		AND #$07
		BEQ @fini
@2cell:	LDA SAVEBACK+1,X
		STA ($F9),Y		; restore to screen
		LDA #$01
		STA ($FE),Y		; only color overflow if X or Y are offset
@fini:	RTS
;
; render sprite within its character matrix by merging its image over its saveback
; $05/$06 points to graphic character
RENDER:
		LDX $00			; 0-4
		LDA SPRITEIMG1,X
		STA $05
		LDA SPRITEIMG2,X
		STA $06
		LDX $01			; 0,2,4,6,8
		LDA SPRITEY,X
		AND #$07
		STA $03
		TAX				; X will hold the sprite's Y coord
		LDY #$00		; erase temp image matrix area
		TYA
@loop1:	STA CASSBUFF+$20,Y
		INY
		CPY #$10		; customized from 4 to 2 character cells
		BNE @loop1
		TAY				; copy 8x8 character image into temp matrix
@loop2:	LDA ($05),Y		; $05/$06 points to character matrix to draw
		STA CASSBUFF+$20,X
		INX
		INY
		CPY #$08
		BNE @loop2
		LDX $01
		LDA SPRITEX,X
		AND #$07		; get modulos on X coordinate
		TAY
		BEQ @skip1		; if its zero, no shifting required
@loop3:	LDX #$00
@loop4:	CLC
		ROR CASSBUFF+$20,X
		ROR CASSBUFF+$28,X
		INX
		CPX #$08
		BNE @loop4
		DEY
		BNE @loop3
@skip1:	STY $FB			; Y is always zero here
@loop5:	LDA $01			; index x2
		CLC
		ADC $FB
		TAX				; X is sprite custom character
		LDA #$1C		; 1st page is where sprites are stored
		STA $06
		LDA SAVEBACK,X
		CMP #$80
		BCC @skip2		; is character reversed?
		LDY #$80		; yes, use start of ROM character set
		STY $06
@skip2:	AND #$1F		; get modulos of first 32-characters
		ASL				; and multiply by 8-pixel height
		ASL
		ASL
		STA $05			; save as low-order byte index
		LDA SAVEBACK,X
		AND #$60		; mask 01100000
		LSR				; divide by 16
		LSR
		LSR
		LSR
		LSR
		CLC
		ADC $06			; add result to high-order page index
		STA $06
		LDY #$00
		LDA $FB
		ASL
		ASL
		ASL
		TAX
@loop6:	LDA ($05),Y		; copy 8x8 character image into behind matrix
		STA CASSBUFF+$30,X
		INX
		INY
		CPY #$08
		BNE @loop6
		INC $FB
		LDA $FB
		CMP #$02		; only a 2-cell sprite now
		BNE @loop5
		LDY #$00
		LDA $02			; 0, 8, 16, 24, 32
		ASL				; 0, 16, 32, 48, 64
		TAX
@loop7:	LDA	CASSBUFF+$20,Y
		ORA CASSBUFF+$30,Y
		STA $1C00,X
		INX
		INY
		CPY #$10		; customized from 4 to 2 character cells
		BNE @loop7
		RTS
;
; puts the sprite character matrix on the screen
PLACEMATRIX:
		LDA $01			; 0, 2, 4, 6, 8
		LDY #$00
		STA ($F7),Y
		LDX $00
		LDA SPRITECLR,X
		STA ($FC),Y
		LDX $01
		LDA SPRITEX,X
		AND #$07
		BNE @color
		LDA SPRITEY,X
		AND #$07
		BEQ @fini
@color:	LDA ($FC),Y
		STA ($FE),Y		; only color overflow if X & Y are offset
		INX
		TXA
		STA ($F9),Y
@fini:	RTS
;
;*********************************************************************
; This section is dedicated to background processing, accomplished
; via the keyboard IRQ service, called 60-times per second (jiffy).
BACKGROUND:
		CLD
		LDA $1EDC
		CMP #$C5
		BNE @esc
		LDA #$09
		STA $96DC		; paint the cage door
@esc:	LDA JIFFYL
		AND #$07
		BNE FLASH
		LDA PPILLTIMER	; drain powerpill
		BEQ FLASH		; is there still power left?
		CMP #$1F		; yes ... but are they
		BCS DRAIN		; getting confidence back?
		AND #$03		; yes, let's warn quikman
		BNE DRAIN
		LDY #$04
@pp1:	LDA SPRITEIMG1,Y
		CMP #$A0		; is monster fleeing?
		BNE @pp2
		LDA SPRITECLR,Y
		EOR #$07		; flash white / blue
		STA SPRITECLR,Y
@pp2:	DEY
		BNE @pp1
DRAIN:	DEC PPILLTIMER
		BNE FLASH
		LDY #$04
@loop:	LDA MONSTERCLR-1,Y
		STA SPRITECLR,Y	; restore all monsters to their default colors
		LDA SPRITEIMG1,Y
		CMP #$70		; is monster already going home?
		BEQ @next
		LDA #$98		; restore monster chasing image
		STA SPRITEIMG1,Y
@next:	DEY
		BNE @loop
FLASH:	LDA FLASHPILL	; powerpill flash
		CMP #$1E		; 30-jiffies?
		BNE @skip1
		LDX #$00		; reset counter
		STX FLASHPILL
@loop1:	LDA $1CF8,X		; custom graphic char
		EOR $8288,X		; rom graphic char
		STA $1CF8,X		; redraw 8x8 char cell
		INX
		CPX #$08
		BNE @loop1
		LDA #$FE		; render monster feet
		EOR $1C9F		; custom graphic char
		STA $1C9F		; redraw caged monster
		STA $1CA7		; redraw fleeing monster
		STA $1C8F		; looking left
		STA $1C97		; looking up
		LDA #$7F		; render monster feet
		EOR $1C7F		; custom graphic char
		STA $1C7F		; looking right
		STA $1C87		; looking down
@skip1:	INC FLASHPILL
		LDA LIVES
		BNE @hi			; playing?
		LDA JIFFYL		; manufacture a moving quikman 'spirit'
		BEQ @hi
		LDA JIFFYM
		AND #$03
		BNE @hi
		INC DEMOQMAN
		LDA DEMOQMAN
		AND #$03
		ASL				; x2 for pair
		TAY				; for the monsters to 'chase'
		LDA CAGEDATA+$08,Y
		STA SPRITEX
		LDA CAGEDATA+$09,Y
		STA SPRITEY
@hi:	LDX #$00		; yes
@loop3:	LDA SCORE-$9200,X ; check current score against high score
		CMP MAZEDATA-$9200+$0F,X
		BCC @top		; is quikman beating the high score?
		BNE @skip4		; yes!
		INX
		CPX #$06
		BNE @loop3
@skip4:	LDX #$00
@loop4:	LDA SCORE-$9200,X		; woot!
		STA MAZEDATA-$9200+$0F,X
		INX
		CPX #$06
		BNE @loop4
@top:	LDX #$14		; refresh top line
@loop5:	LDA MAZEDATA-$9200,X
		STA $1E00,X
		DEX
		BPL @loop5
ADDPTS:	LDY POINTS		; award points to score on screen
		BEQ CLRPTS
@loop1:	LDX DIGIT
@loop2:	LDA MAZEDATA-$9200,X
		CMP #$B9		; reach "9" ?
		BEQ @skip2
		INC MAZEDATA-$9200,X	; ding!
		DEY
		BNE @loop1
		BEQ CLRPTS
@skip2:	LDA #$B0
		STA MAZEDATA-$9200,X	; wrap to "0"
		DEX				; and increment next order
		BNE @loop2
CLRPTS:	STY POINTS
		LDX #$00
		LDY #$00
@loop6:	LDA PENALTY,X
		BNE @next		; not aware while caged
		LDA SPRITEIMG1+1,X
		CMP #$70
		BNE @go			; small ghost going home?
		LDA #$50
		STA $53,Y
		LDA #$48
		STA $54,Y
		BNE @next
@go:	LDA DOTS
		CMP #$A6		; make them all "smart" with 5-dots or less
		BCS @skip5
		LDA CAGEDATA,X
		BEQ @skip5		; is monster "smart"?  Red one is ...
		CMP JIFFYL		; no, so check as often as it waits
		BNE @next		; is its wait time equal to the jiffy clock?
@skip5:	LDA SPRITEX		; update this monster's awareness to where quikman is
		STA $53,Y
		LDA SPRITEY
		STA $54,Y
@next:	INY
		INY
		INX
		CPX #$04
		BNE @loop6
;
wahka:	LDA CHEWING
		BEQ @skip1
		LDA #$91		; start with an odd frequency
		STA VIC+$0C		; ignite a voice
@skip1:	LDA #$00		; dot is swallowed
		STA CHEWING
		LDA VIC+$0C
		BEQ @next1		; is this voice mute?
		LDA VIC+$0C
		AND #$01
		BEQ @skip3		; is it even?
		LDA VIC+$0C
		CLC
		ADC #$10		; increase tone
		CMP #$F1
		BCC @skip2		; is voice too high?
		SEC
		SBC #$01		; make it even
@skip2:	STA VIC+$0C
		CLC
		BCC @next1		; goto next effect
@skip3:	LDA VIC+$0C
		SEC
		SBC #$10		; drain tone
		STA VIC+$0C
@next1:	LDX CHOMP
		BEQ @skip4
		LDA JIFFYL
		AND #$01
		BNE @skip4
		LDA SNDBIT,X	; load tone data
		STA VIC+$0B
		DEC CHOMP
@skip4:	LDA EXTRAQMAN
		BNE @fini		; already got bonus life
		LDA SCORE+1-$9200
		CMP #$B1		; did quikman just score 10,000-points?
		BNE @fini
		STA EXTRAQMAN
		LDX LIVES
		LDA #$19
		STA $1FE4,X
		LDA #$07
		STA $97E4,X
		INC LIVES		; reward
@fini:	JMP $EABF		; jump to hardware IRQ
;
; Pass A for number of jiffies to wait, while preserving X
PAUSE:	PHA
		TXA
		PHA
		JSR SPRITES		; redraw sprites
		PLA
		TAX
		PLA
		CLC
		ADC JIFFYL
@loop:	CMP JIFFYL
		BNE @loop
		RTS
;
; Y > 0 erase; Y = 0 display
GAMEOVER:
		LDX #$08
@loop:	LDA GOTEXT,X	; GAME OVER
		CPY #$00
		BEQ @dead
		LDA #$20		; space
@dead:	STA $1F17,X		; print character
		STA $1B17,X
		LDA #$02		; red
		STA $9717,X
		DEX
		BPL @loop
		RTS
;
;*********************************************************************
; READ-ONLY DATA
CAGEDATA:		; knowledge cycle time
		.byte	$00, $33, $76, $F9
				; right, up, left, right
		.byte	$00, $03, $02, $00
				; coordinate to consider going to upon release or demo
		.byte	$A0, $28, $00, $28, $A0, $88, $00, $88
GOTEXT:			; GAME OVER
		.byte	$87, $81, $8D, $85, $A0, $8F, $96, $85, $92
INERTIA:		; maintain direction
		.byte	$01, $00, $00, $01, $FF, $00, $00, $FF
PEEKAHEAD:		;
		.byte	$01, $15
SNDBIT:			; yummy sound effect
		.byte	$00, $00, $C0, $B8, $B0, $A8, $B0, $B8, $C0, $C8
STARTPOS:		;
		.byte	$50, $88, $50, $48, $50, $58, $60, $58, $40, $58
SPRITEMASK:		; really?
		.byte	$01, $02, $04, $08, $10
;
;*********************************************************************
; Maze data (copied to RAM: $1A00 - $1BFF)
; Screen size: 24-rows by 21-columns
		.res $AC00 - *
MAZEDATA:
SCORE	= * + $06
		.byte	$93, $83, $8F, $92, $85, $BA, $B0, $B0, $B0, $B0, $B0, $B0, $A0, $A0, $A0, $B0, $B2, $B0, $B0, $B0, $B0
		.byte	$37, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3D, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $38
		.byte	$39, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $39
		.byte	$39, $1E, $2F, $30, $1E, $2F, $2B, $2B, $30, $1E, $39, $1E, $2F, $2B, $2B, $30, $1E, $2F, $30, $1E, $39
		.byte	$39, $1F, $2D, $2E, $1E, $2D, $2C, $2C, $2E, $1E, $32, $1E, $2D, $2C, $2C, $2E, $1E, $2D, $2E, $1F, $39
		.byte	$39, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $39
		.byte	$39, $1E, $33, $34, $1E, $31, $1E, $33, $3A, $3A, $3D, $3A, $3A, $34, $1E, $31, $1E, $33, $34, $1E, $39
		.byte	$39, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $39
		.byte	$35, $3A, $3A, $38, $1E, $3B, $3A, $3A, $34, $20, $32, $20, $33, $3A, $3A, $3C, $1E, $37, $3A, $3A, $36
		.byte	$20, $20, $20, $39, $1E, $39, $20, $20, $20, $20, $20, $20, $20, $20, $20, $39, $1E, $39, $20, $20, $20
		.byte	$3A, $3A, $3A, $36, $1E, $32, $20, $37, $3A, $29, $C5, $2A, $3A, $38, $20, $32, $1E, $35, $3A, $3A, $3A
		.byte	$20, $20, $20, $20, $1E, $20, $20, $39, $20, $20, $20, $20, $20, $39, $20, $20, $1E, $20, $20, $20, $20
		.byte	$3A, $3A, $3A, $38, $1E, $31, $20, $35, $3A, $3A, $3A, $3A, $3A, $36, $20, $31, $1E, $37, $3A, $3A, $3A
		.byte	$20, $20, $20, $39, $1E, $39, $20, $20, $20, $20, $20, $20, $20, $20, $20, $39, $1E, $39, $20, $20, $20
		.byte	$37, $3A, $3A, $36, $1E, $32, $20, $33, $3A, $3A, $3D, $3A, $3A, $34, $20, $32, $1E, $35, $3A, $3A, $38
		.byte	$39, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $39
		.byte	$39, $1E, $33, $38, $1E, $33, $3A, $3A, $34, $1E, $32, $1E, $33, $3A, $3A, $34, $1E, $37, $34, $1E, $39
		.byte	$39, $1F, $1E, $39, $1E, $1E, $1E, $1E, $1E, $1E, $20, $1E, $1E, $1E, $1E, $1E, $1E, $39, $1E, $1F, $39
		.byte	$3B, $34, $1E, $32, $1E, $31, $1E, $33, $3A, $3A, $3D, $3A, $3A, $34, $1E, $31, $1E, $32, $1E, $33, $3C
		.byte	$39, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $39, $1E, $1E, $1E, $1E, $39
		.byte	$39, $1E, $33, $3A, $3A, $3E, $3A, $3A, $34, $1E, $32, $1E, $33, $3A, $3A, $3E, $3A, $3A, $34, $1E, $39
		.byte	$39, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $1E, $39
		.byte	$35, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $3A, $36
BANNERMSG:		; ©2008 RHURST  F1START
		.byte	$3F, $B2, $B0, $B0, $B8, $A0, $92, $88, $95, $92, $93, $94, $A0, $A0, $00, $01, $93, $94, $81, $92, $94
FRUITSCORE:
		.byte	$01, $03, $05, $07, $0A, $14, $1E, $32
;
;*********************************************************************
; Custom character data (copied to RAM: $1C00 - $1DFF)
		.assert * = $AE00, error, "Graphics not at $AE00"
		.byte	$FF, $EA, $EF, $EF, $EB, $EF, $EF, $FF	; @ [F - then sprite #0
		.byte	$FC, $EC, $EC, $EC, $EC, $EC, $EC, $FC	; A  1] - then sprite #0
		.byte	$38, $7C, $FE, $92, $FE, $FE, $FE, $AA	; B sprite #1 - red
		.byte	$00, $00, $00, $00, $00, $00, $00, $00	; C
		.byte	$38, $7C, $FE, $92, $FE, $FE, $FE, $AA	; D sprite #2 - green
		.byte	$00, $00, $00, $00, $00, $00, $00, $00	; E
		.byte	$38, $7C, $FE, $92, $FE, $FE, $FE, $AA	; F sprite #3 - cyan
		.byte	$00, $00, $00, $00, $00, $00, $00, $00	; G
		.byte	$38, $7C, $FE, $92, $FE, $FE, $FE, $AA	; H sprite #4 - yellow
		.byte	$00, $00, $00, $00, $00, $00, $00, $00	; I
;
QUIKMANCLR:		; yellow
		.byte	$07
MONSTERCLR:		; red, green, cyan, yellow
		.byte	$02, $05, $03, $07
FRUIT:			; cherry, strawberry, 2-peach, 2-apple, 2-pineapple, 2-tbird, 2-bell, key
		.byte	$21, $22, $23, $23, $24, $24, $25, $25, $26, $26, $27, $27, $28
FRUITCLR:		; red, red, 2-yellow, 2-red, 2-green, 2-magenta, 2-yellow, cyan
		.byte	$02, $02, $07, $07, $02, $02, $05, $05, $04, $04, $07, $07, $03
;
; resume graphic character data
		.res $AE70 - *
		.byte	$00, $38, $7C, $54, $7C, $7C, $54, $00	; N small ghost
		.byte	$1C, $3E, $7F, $64, $7F, $7F, $7F, $55	; O ghost right
		.byte	$1C, $3E, $7F, $7F, $49, $7F, $7F, $55	; P ghost down
		.byte	$38, $7C, $FE, $26, $FE, $FE, $FE, $AA	; Q ghost left
		.byte	$38, $7C, $FE, $FE, $FE, $FE, $FE, $AA	; R ghost up
		.byte	$38, $7C, $FE, $92, $FE, $82, $FE, $AA	; S ghost caged
		.byte	$38, $7C, $FE, $92, $FE, $82, $FE, $54	; T ghost fleeing
		.byte	$3C, $7E, $BD, $FF, $BD, $C3, $7E, $3C	; U smiley
		.byte	$3C, $7E, $FF, $FF, $FF, $FF, $7E, $3C	; V pacman closed
		.byte	$3E, $7C, $F8, $F0, $F0, $F8, $7C, $3E	; W pacman right
		.byte	$3C, $7E, $FF, $FF, $E7, $C3, $81, $00	; X pacman down
		.byte	$7C, $3E, $1F, $0F, $0F, $1F, $3E, $7C	; Y pacman left
		.byte	$00, $81, $C3, $E7, $FF, $FF, $7E, $3C	; Z pacman up
		.byte	$00, $10, $10, $6C, $10, $10, $00, $00	; [ explosion
		.byte	$10, $44, $28, $C6, $28, $44, $10, $00	; # smoke
		.byte	$92, $44, $00, $82, $00, $44, $92, $00	; ] dust
		.byte	$00, $00, $00, $18, $18, $00, $00, $00	; ^ dot
		.byte	$00, $3C, $7E, $7E, $7E, $7E, $3C, $00	; <- powerpill (animated)
		.byte	$00, $00, $00, $00, $00, $00, $00, $00	;$20 empty space
		.byte	$04, $08, $18, $24, $62, $F7, $F2, $60	; ! cherry
		.byte	$10, $7C, $FE, $AA, $D6, $AA, $54, $28	; " strawberry
		.byte	$20, $10, $7C, $FE, $FE, $FE, $7C, $38	; # peach
		.byte	$08, $10, $7C, $FE, $FE, $FE, $7C, $28	; $ apple
		.byte	$08, $10, $38, $38, $7C, $FE, $FE, $6C	; % pear
		.byte	$10, $30, $92, $FE, $7C, $38, $10, $28	; & tbird
		.byte	$10, $38, $7C, $7C, $7C, $7C, $FE, $10	; ' bell
		.byte	$18, $24, $18, $08, $08, $18, $08, $18	; ( key
		.byte	$00, $FF, $01, $01, $01, $01, $FE, $00	; ) doorway east
		.byte	$00, $FF, $80, $80, $80, $80, $7F, $00	; * doorway west
		.byte	$00, $FF, $00, $00, $00, $00, $00, $00	; + maze wall h-top
		.byte	$00, $00, $00, $00, $00, $00, $FF, $00	; , maze wall h-bottom
		.byte	$40, $40, $40, $40, $40, $20, $1F, $00	; - maze wall s-w corner
		.byte	$02, $02, $02, $02, $02, $04, $F8, $00	; . maze wall s-e corner
		.byte	$00, $1F, $20, $40, $40, $40, $40, $40	; / maze wall n-w corner
		.byte	$00, $F8, $04, $02, $02, $02, $02, $02	; 0 maze wall n-e corner
		.byte	$00, $18, $24, $42, $42, $42, $42, $42	; 1 maze wall north
		.byte	$42, $42, $42, $42, $42, $24, $18, $00	; 2 maze wall south
		.byte	$00, $1F, $20, $40, $40, $20, $1F, $00	; 3 maze wall west
		.byte	$00, $F8, $04, $02, $02, $04, $F8, $00	; 4 maze wall east
		.byte	$42, $41, $40, $40, $40, $20, $1F, $00	; 5 maze wall s-w elbow
		.byte	$42, $82, $02, $02, $02, $04, $F8, $00	; 6 maze wall s-e elbow
		.byte	$00, $1F, $20, $40, $40, $40, $41, $42	; 7 maze wall n-w elbow
		.byte	$00, $F8, $04, $02, $02, $02, $82, $42	; 8 maze wall n-e elbow
		.byte	$42, $42, $42, $42, $42, $42, $42, $42	; 9 maze wall vertical
		.byte	$00, $FF, $00, $00, $00, $00, $FF, $00	; : maze wall horizontal
		.byte	$42, $41, $40, $40, $40, $40, $41, $42	; ; maze wall west tee
		.byte	$42, $82, $02, $02, $02, $02, $82, $42	; < maze wall east tee
		.byte	$00, $FF, $00, $00, $00, $00, $81, $42	; = maze wall north tee
		.byte	$42, $81, $00, $00, $00, $00, $FF, $00	; > maze wall south tee
		.byte	$3C, $42, $99, $A1, $A1, $99, $42, $3C	; ? (C)copyright symbol
