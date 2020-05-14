; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P4.5 is pressed.
$NOLIST
$MODLP51
$LIST

; There is a couple of typos in MODLP51 in the definition of the timer 0/1 reload
; special function registers (SFRs), so:

TIMER0_RELOAD_L DATA 0xf2
TIMER1_RELOAD_L DATA 0xf3
TIMER0_RELOAD_H DATA 0xf4
TIMER1_RELOAD_H DATA 0xf5

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

MODEBUTTON   equ P4.5
SOUND_OUT     equ P3.7
RIGHTBUTTON      equ P0.0
LEFTBUTTON     equ P0.1
TABUTTON      equ P0.2

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
sec:		  ds 1 
min:		  ds 1
hr: 		  ds 1
day:		  ds 1
minA:		  ds 1
hrA: 		  ds 1
dayA:		  ds 1


; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
seconds_flag: dbit 1 ; Set to one in the ISR every time 1000 ms had passed
m_flag: 	  dbit 1
m_flagA:	  dbit 1
alarmFlag:	  dbit 1
modeFlag:	  dbit 1



cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P1.1
LCD_RW equ P1.2
LCD_E  equ P1.3
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST


time_string:  db '--- --:--:-- -M', 0
sun:		  db 'SUN', 0
mon:		  db 'MON', 0
tue:		  db 'TUE', 0
wed:	      db 'WED', 0
thu:		  db 'THU', 0
fri:		  db 'FRI', 0
sat:	  	  db 'SAT', 0
alarm_string: db 'ALM --:-- -M ---', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Set autoreload value
	mov TIMER0_RELOAD_H, #high(TIMER0_RELOAD)
	mov TIMER0_RELOAD_L, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    clr TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti
 
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;

; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if one second has passed
	mov a, Count1ms+0
	cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1000), Timer2_ISR_done
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb seconds_flag ; Let the main program know one second had passed
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	
count_sec:
	; Increment the BCD counter
	mov a, BCD_counter
	cjne a, #0x59, inc_sec
	mov a, #0
	mov BCD_counter, a
	
count_min:
	; Minute counter
	mov a, min
	add a, #1
	da a
	mov min, a
	cjne a, #0x60, reset
	mov a, #0
	mov min, a
	
count_hr:
	; Hour counter
	mov a, hr
	add a, #1
	da a
	mov hr, a
	cjne a, #0x13, inc_hr
	mov a, #1
	mov hr, a
	
inc_hr:
	mov a, hr
	cjne a, #0x12, reset
	mov a, #1
	CPL m_flag
	jnb m_flag, inc_day
	sjmp reset
	
inc_day:
	mov a, day
	add a, #1
	da a
	mov day, a
	cjne a, #8, reset
	mov a, #1
	mov day, a
	clr a
	
inc_sec:
	mov a, BCD_counter
	add a, #1
	da a
	mov BCD_counter, a
	
reset:
	nop


Timer2_ISR_done:
	pop psw
	pop acc
	reti
	
	
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    lcall Timer0_Init
    lcall Timer2_Init
    ; In case you decide to use the pins of P0 configure the port in bidirectional mode:
    mov P0M0, #0
    mov P0M1, #0
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    Set_Cursor(1, 1)
    Send_Constant_String(#time_string)
    setb seconds_flag
    setb m_flag
    clr m_flagA
    setb alarmFlag
    setb modeFlag
    Set_Cursor(2, 1)
    Send_Constant_String(#alarm_string)
    
	mov BCD_counter, #0x50
	mov min, #0x9
	mov hr, #0x7
	mov day, #1
	
	mov minA, #0x45
	mov hrA, #0x6
	mov dayA, #5

	
	; After initialization the program stays in this 'forever' loop
	
loop:
	jb TABUTTON, loop_a  ; if the 'toggle' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb TABUTTON, loop_a  ; if the 'toggle' button is not pressed skip
	jnb TABUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
	setb TR2                ; Start timer 2
	ljmp toggleAlarm
	ljmp loop_b             ; Display the new value
	
loop_a:
	jb MODEBUTTON, modeCheck  ; if the 'mode' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MODEBUTTON, modeCheck  ; if the 'mode' button is not pressed skip
	jnb MODEBUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
	setb TR2                ; Start timer 2
	CPL modeFlag
	ljmp loop_b             ; Display the new value
	
aTimeRight:
	jb RIGHTBUTTON, aTimeLeft  ; if the 'mode' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb RIGHTBUTTON, aTimeLeft  ; if the 'mode' button is not pressed skip
	jnb RIGHTBUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
	setb TR2                ; Start timer 2
	mov a, min
	add a, #0x01
	da a
	mov min, a
	ljmp loop_b             ; Display the new value
	
aTimeLeft:
	jb LEFTBUTTON, continue  ; if the 'mode' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb LEFTBUTTON, continue  ; if the 'mode' button is not pressed skip
	jnb LEFTBUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
	setb TR2                ; Start timer 2
	mov a, min
	add a, #0x99
	da a
	mov min, a
	sjmp continue             ; Display the new value
	
modeCheck:
	jnb modeFlag, aTimeRight
	jb modeFlag, ljALRMR
	
ljALRMR:
	ljmp aAlrmRight
	
continue:
	mov a, min
	cjne a, #0x60, c1
	mov a, #0
	mov min, a
	ljmp loop_b

c1:
	mov min, a
	mov a, minA
	cjne a, #0x60, c2
	mov a, #0
	mov minA, a
	ljmp loop_b
	
c2:
	mov minA, a
	mov a, hr
	cjne a, #0x13, c3
	mov a, #1
	mov hr, a
	ljmp loop_b
	
c3:
	mov hr, a
	mov a, hrA
	cjne a, #0x13, c4
	mov a, #1
	mov hrA, a
	mov a, #1
	mov day, a
	ljmp loop_b
	
c4:
	mov hrA, a
	mov a, day
	cjne a, #8, c5
	mov a, #1
	mov day, a
	ljmp loop_b
	
c5:
	mov day, a
	mov a, dayA
	cjne a, #8, c6
	mov a, #1
	mov dayA, a
	ljmp loop_b
	
c6: mov dayA, a
	mov a, min
	cjne a, #0x99, c7
	mov a, #0x59
	mov min, a
	ljmp loop_b

c7:
	mov min, a
	mov a, minA
	cjne a, #0x99, c8
	mov a, #0x59
	mov minA, a
	ljmp loop_b
	
c8:
	mov minA, a
	mov a, hr
	cjne a, #0, c9
	mov a, #0x12
	mov hr, a
	sjmp loop_b
	
c9:
	mov hr, a
	mov a, hrA
	cjne a, #0, c10
	mov a, #0x12
	mov hrA, a
	sjmp loop_b
	
c10:
	mov hrA, a
	mov a, day
	cjne a, #0, c11
	mov a, #7
	mov day, a
	sjmp loop_b
	
c11:
	mov day, a
	mov a, dayA
	cjne a, #0, c12
	mov a, #7
	mov dayA, a
	sjmp loop_b
	
c12: mov dayA, a
	sjmp loop_b
	

	
aAlrmRight:
	jb RIGHTBUTTON, aAlrmLeft  ; if the 'mode' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb RIGHTBUTTON, aAlrmLeft  ; if the 'mode' button is not pressed skip
	jnb RIGHTBUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
	setb TR2                ; Start timer 2
	mov a, minA
	add a, #0x01
	da a
	mov minA, a
	sjmp loop_b             ; Display the new value
	
aAlrmLeft:
	jb LEFTBUTTON, contJump  ; if the 'mode' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb LEFTBUTTON, contJump  ; if the 'mode' button is not pressed skip
	jnb LEFTBUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	
	setb TR2                ; Start timer 2
	mov a, minA
	add a, #0x99
	da a
	mov minA, a
	ljmp continue             ; Display the new value

contJump:
	ljmp continue
	
	
reset_day:
	mov day, a
	clr a
	ljmp loop_b
	
toggleAlarm:
	CPL alarmFlag
	sjmp loop_b
	
	
loop_b:
    clr seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
    ;Set_Cursor(1,1)
    ;Send_Constant_String(#time_string)
    
    Set_Cursor(1, 11)
    Display_BCD(BCD_counter)
    
    Set_Cursor(1, 8)
    Display_BCD(min)
    
    Set_Cursor(1, 5)
    Display_BCD(hr)
    
    Set_Cursor(2, 8)
    Display_BCD(minA)
    
    Set_Cursor(2, 5)
    Display_BCD(hrA)
    
    mov a, dayA
    cjne a, #1, is_monA
    Set_Cursor(2, 14)
    Send_Constant_String(#sun)
    sjmp loop_cont
    
loop_cont:
    mov a, day
    cjne a, #1, is_mon
    Set_Cursor(1, 1)
    Send_Constant_String(#sun)
    ljmp day_done
    
;==========================================
    
is_monA:
	mov a, dayA
	cjne a, #2, is_tueA
    Set_Cursor(2, 14)
    Send_Constant_String(#mon)
    ljmp todA
    
is_mon:
	mov a, day
	cjne a, #2, is_tue
    Set_Cursor(1, 1)
    Send_Constant_String(#mon)
    ljmp day_done
    
is_tueA:
	mov a, dayA
	cjne a, #3, is_wedA
    Set_Cursor(2, 14)
    Send_Constant_String(#tue)
    ljmp todA
    
is_tue:
	mov a, day
	cjne a, #3, is_wed
    Set_Cursor(1, 1)
    Send_Constant_String(#tue)
    ljmp day_done
    
is_wedA:
	mov a, dayA
	cjne a, #4, is_thuA
    Set_Cursor(2, 14)
    Send_Constant_String(#wed)
    ljmp todA
    
is_wed:
	mov a, day
	cjne a, #4, is_thu
    Set_Cursor(1, 1)
    Send_Constant_String(#wed)
    ljmp day_done
    
is_thuA:
	mov a, dayA
	cjne a, #5, is_friA
    Set_Cursor(2, 14)
    Send_Constant_String(#thu)
    ljmp todA
    
is_thu:
	mov a, day
	cjne a, #5, is_fri
    Set_Cursor(1, 1)
    Send_Constant_String(#thu)
    ljmp day_done
    
is_friA:
	mov a, dayA
	cjne a, #6, is_satA
    Set_Cursor(2, 14)
    Send_Constant_String(#fri)
    ljmp todA
    
is_fri:
	mov a, day
	cjne a, #6, is_sat
    Set_Cursor(1, 1)
    Send_Constant_String(#fri)
    ljmp day_done
    
is_satA:
	mov a, dayA
    Set_Cursor(2, 14)
    Send_Constant_String(#sat)
    ljmp todA
    
is_sat:
	mov a, day
	cjne a, #7, day_done
    Set_Cursor(1, 1)
    Send_Constant_String(#sat)
    ljmp day_done
    
;==========================================

todA:
	jb m_flagA, show_PMA
	Set_Cursor(2, 11)
	Display_char(#'A')
	clr m_flagA
	ljmp loop_cont   
    
day_done:
	jb m_flag, show_PM
	Set_Cursor(1, 14)
	Display_char(#'A')
	clr m_flag
	ljmp check_alarmAM
	
show_PMA:
	Set_Cursor(2, 11)
	Display_char(#'P')
	setb m_flagA
	ljmp loop_cont
    
show_PM:
	Set_Cursor(1, 14)
	Display_char(#'P')
	setb m_flag
	ljmp check_alarmPM
	
check_alarmAM:
	mov a, m_flagA
	mov m_flagA, a
	jb m_flagA, offback
	mov a, day
	cjne a, dayA, offback
	mov a, hr
	cjne a, hrA, offback
	mov a, min
	cjne a, minA, offback
	jnb alarmFlag, offback
	sjmp beep

check_alarmPM:
	mov a, m_flagA
	mov m_flagA, a
	jnb m_flagA, offback
	mov a, day
	cjne a, dayA, offback
	mov a, hr
	cjne a, hrA, offback
	mov a, min
	cjne a, minA, offback
	jnb alarmFlag, offback
	sjmp beep
	
beep:
	setb TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	ljmp loop
	
offback:
	clr TR0
	ljmp loop

END
